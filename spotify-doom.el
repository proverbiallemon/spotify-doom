;;; spotify-doom.el --- Control Spotify -*- lexical-binding: t -*-

;; Author: Lemon Newby
;; URL: https://github.com/proverbiallemon/spotify-doom
;; Version: 0.1.1
;; Keywords: multimedia, spotify, doom
;; Package-Requires: ((emacs "26.1") (request "0.3.3") (oauth2 "0.16") (simple-httpd "1.5.1") (exec-path-from-shell "1.12"))

(eval-when-compile (require 'use-package))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; spotify-doom is a package that allows controlling Spotify from within Doom Emacs.
;; It uses the Spotify API and implements the OAuth2 PKCE flow for authorization.

;;; Code:

(require 'request)
(require 'oauth2)
(require 'cl-lib)
(require 'simple-httpd)
(require 'exec-path-from-shell)
(require 'request)
(require 'json)
(require 'url-util)

(defvar spotify-doom-code-verifier nil)

(defvar spotify-doom-access-token nil
  "The current Spotify access token.")

(defvar spotify-doom-refresh-token nil
  "The current Spotify refresh token.")



;; Load environment variables from .env file
(defun spotify-doom-read-env (key)
  "Read the value for KEY from the .env file."
  (let ((default-directory (file-name-directory load-file-name)))
    (with-temp-buffer
      (insert-file-contents "spotify-doom.env")
      (goto-char (point-min))
      (if (re-search-forward (format "^%s=\\(.*\\)$" key) nil t)
          (let ((value (match-string 1)))
            (message "Value of %s is %s" key value)
            value)
        (error "Could not find %s in .env file" key)))))

(defvar spotify-doom-client-id (spotify-doom-read-env "SPOTIFY_CLIENT_ID")
  "Your Spotify Client ID.")

(defvar spotify-doom-redirect-uri (spotify-doom-read-env "SPOTIFY_REDIRECT_URI")
 "Your Spotify Redirect URI.")

(defun spotify-doom--base64url-encode (str)
  (replace-regexp-in-string "=" "" (replace-regexp-in-string "/" "_" (replace-regexp-in-string "+" "-" (base64-encode-string str)))))

(defun spotify-doom--generate-code-verifier ()
  (let ((verifier-bytes (make-string 32 0)))
    (dotimes (i 32)
      (aset verifier-bytes i (random 256)))
    (spotify-doom--base64url-encode verifier-bytes)))


(defun spotify-doom-generate-code-challenge (code-verifier)
  (require 'sha1)
  (let* ((verifier-bytes (encode-coding-string code-verifier 'utf-8))
         (hashed (secure-hash 'sha256 verifier-bytes nil nil t)))
    (spotify-doom--base64url-encode hashed)))

(defun spotify-doom-authorization-url (code-challenge)
  (let ((url (concat "https://accounts.spotify.com/authorize"
                     "?client_id=" (url-hexify-string (substring-no-properties spotify-doom-client-id))
                     "&response_type=code"
                     "&redirect_uri=" (url-hexify-string spotify-doom-redirect-uri)
                     "&code_challenge_method=S256"
                     "&code_challenge=" code-challenge
                     "&scope=user-read-private%20user-read-email")))
    url))


(defun spotify-doom-request-token (code code-verifier)
  (print code)
  (print (format "spotify-doom-client-id: %S" spotify-doom-client-id))
  (let ((data (format "grant_type=authorization_code&client_id=%s&code=%s&redirect_uri=%s&code_verifier=%s"
                      (url-hexify-string (substring-no-properties spotify-doom-client-id))
                      (url-hexify-string code)
                      (url-hexify-string spotify-doom-redirect-uri)
                      (url-hexify-string code-verifier))))
    (request
     "https://accounts.spotify.com/api/token"
     :type "POST"
     :headers `(("Content-Type" . "application/x-www-form-urlencoded")
                ("Content-Length" . ,(number-to-string (length data))))
     :data data
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (setq spotify-doom-access-token (alist-get 'access_token data))
                 (setq spotify-doom-refresh-token (alist-get 'refresh_token data))
                 (message "Full response: %S" data)))
     :error (cl-function
             (lambda (&rest args &key error-thrown data &allow-other-keys)
               (message "Got error: %S" error-thrown)
               (message "Error response: %S" data))))))


(defun spotify-doom-refresh-token (refresh-token)
  (let* ((data-alist `((client_id . ,(substring-no-properties spotify-doom-client-id))
                       (grant_type . "refresh_token")
                       (refresh_token . ,refresh-token)))
         (data (url-build-query-string data-alist))
         (content-length (number-to-string (length data))))
    (request
     "https://accounts.spotify.com/api/token"
     :type "POST"
     :headers `(("Content-Type" . "application/x-www-form-urlencoded")
                ("Content-Length" . ,content-length))
     :data data
     :parser 'json-read
     :success (cl-function
          (lambda (&key data &allow-other-keys)
            (setq spotify-doom-access-token (alist-get 'access_token data))
            (message "Full response: %S" data)))
     :error (cl-function
             (lambda (&rest args &key error-thrown data &allow-other-keys)
               (message "Got error: %S" error-thrown)
               (message "Error response: %S" data))))))


(defun spotify-doom-get-user-info ()
  (request
   "https://api.spotify.com/v1/me"
   :type "GET"
   :headers (list (cons "Authorization" (concat "Bearer " spotify-doom-access-token)))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "User Info: %s" data)))
   :error (cl-function
           (lambda (&rest args &key error-thrown &allow-other-keys)
             (message "Got error: %S" error-thrown)))))



(defun spotify-doom-authenticate ()
  (interactive)
  (let ((code-verifier (spotify-doom--generate-code-verifier))
        (httpd-port 8080))
    (setq spotify-doom-code-verifier code-verifier)
    (httpd-start)
    (browse-url (spotify-doom-authorization-url (spotify-doom-generate-code-challenge code-verifier)))
    (run-at-time "60 sec" nil (lambda ()
                                (call-interactively #'spotify-doom-callback)))))


(defun spotify-doom-callback ()
  (interactive)
  (let ((remaining-time 60)
        (timeout nil)
        (code nil))
    (while (and (not timeout) (not code))
      (setq code (read-string (format "Enter the code (time remaining: %ds): " remaining-time)))
      (if (string-empty-p code)
          (progn
            (setq remaining-time (- remaining-time 1))
            (setq timeout (= remaining-time 0))
            (sit-for 1))
        (spotify-doom-request-token code spotify-doom-code-verifier)))))


(add-to-list 'load-path "/path/to/spotify-doom-extension")
(require 'spotify-doom-extension)


(provide 'spotify-doom)

;;; spotify-doom.el ends here

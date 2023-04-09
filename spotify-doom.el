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


;; Load environment variables from .env file
(defun spotify-doom-read-env (key)
  "Read the value for KEY from the .env file."
  (let ((default-directory (file-name-directory load-file-name)))
    (with-temp-buffer
      (insert-file-contents "spotify-doom.env")
      (goto-char (point-min))
      (if (re-search-forward (format "^%s=\\(.*\\)$" key) nil t)
          (match-string 1)
        (error "Could not find %s in .env file" key)))))



(defvar spotify-doom-client-id (spotify-doom-read-env "SPOTIFY_CLIENT_ID")
  "Your Spotify Client ID.")

(defvar spotify-doom-redirect-uri (spotify-doom-read-env "SPOTIFY_REDIRECT_URI")
  "Your Spotify Redirect URI.")

(defun spotify-doom-generate-random-string (length)
  (let* ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_~.")
         (chars-length (length chars))
         (random-string (make-string length 0)))
    (dotimes (i length random-string)
      (aset random-string i (aref chars (random chars-length))))))

(defun spotify-doom-generate-code-challenge (code-verifier)
  (require 'base64)
  (require 'sha1)
  (let* ((verifier-bytes (encode-coding-string code-verifier 'utf-8))
         (hashed (secure-hash 'sha256 verifier-bytes nil nil t))
         (base64url (base64-encode-string hashed t)))
    (replace-regexp-in-string "=" "" base64url)))

(defun spotify-doom-authorization-url (code-challenge)
  (let ((url (concat "https://accounts.spotify.com/authorize"
                     "?client_id=" spotify-doom-client-id
                     "&response_type=code"
                     "&redirect_uri=" (url-hexify-string spotify-doom-redirect-uri)
                     "&code_challenge_method=S256"
                     "&code_challenge=" code-challenge
                     "&scope=user-read-private%20user-read-email")))
    url))

(defun spotify-doom-request-token (code code-verifier)
  (request
   "https://accounts.spotify.com/api/token"
   :type "POST"
   :data (list (cons "client_id" spotify-doom-client-id)
               (cons "grant_type" "authorization_code")
               (cons "code" code)
               (cons "redirect_uri" spotify-doom-redirect-uri)
               (cons "code_verifier" code-verifier))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Access Token: %s" (alist-get 'access_token data))
               (message "Refresh Token: %s" (alist-get 'refresh_token data))))
   :error (cl-function
           (lambda (&rest args &key error-thrown &allow-other-keys)
             (message "Got error: %S" error-thrown)))))

(defun spotify-doom-refresh-token (refresh-token)
  (request
   "https://accounts.spotify.com/api/token"
   :type "POST"
   :data (list (cons "client_id" spotify-doom-client-id)
               (cons "grant_type" "refresh_token")
               (cons "refresh_token" refresh-token))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "New Access Token: %s" (alist-get 'access_token data))))
   :error (cl-function
           (lambda (&rest args &key error-thrown &allow-other-keys)
             (message "Got error: %S" error-thrown)))))

(defun spotify-doom-get-user-info (access-token)
  (request
   "https://api.spotify.com/v1/me"
   :type "GET"
   :headers (list (cons "Authorization" (concat "Bearer " access-token)))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "User Info: %s" data)))
   :error (cl-function
           (lambda (&rest args &key error-thrown &allow-other-keys)
 (message "Got error: %S" error-thrown)))))
(defun spotify-doom-authorize ()
  (let* ((code-verifier (spotify-doom-generate-random-string 128))
         (code-challenge (spotify-doom-generate-code-challenge code-verifier)))
    (setq spotify-doom-code-verifier code-verifier)
    (browse-url (spotify-doom-authorization-url code-challenge))))

(defun spotify-doom-callback (code)
  (spotify-doom-request-token code spotify-doom-code-verifier))

(defun spotify-doom-test ()
  (interactive)
  (spotify-doom-authorize))

;; Use the spotify-doom-test function to start the authorization process.
;; After authorizing the app and receiving a code, call spotify-doom-callback with the code as its argument.
;;; spotify-doom.el ends here

;;; spotify-doom.el --- Control Spotify from Doom Emacs -*- lexical-binding: t -*-

;; Author: Lemon Newby
;; URL: https://github.com/proverbiallemon/spotify-doom
;; Version: 0.1.1
;; Keywords: multimedia, spotify, doom
;; Package-Requires: ((emacs "26.1") (request "0.3.3") (oauth2 "0.10") (simple-httpd "1.5.1") (exec-path-from-shell "1.12"))

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
(require 'simple-httpd)
(require 'cl-lib)
(require 'exec-path-from-shell)

;; Load environment variables from .env file
(exec-path-from-shell-copy-env "CLIENT_ID")
(exec-path-from-shell-copy-env "CLIENT_SECRET")

(defconst spotify-doom-client-id "your-client-id")
(defconst spotify-doom-redirect-uri "http://localhost:8080/spotify-doom-callback")
(defconst spotify-doom-httpd-port 8080)

(defun spotify-doom-generate-code-verifier ()
  "Generate a random code verifier."
  (cl-loop for i below 43
           collect (cl-random 62) into chars
           finally return (concat (mapcar (lambda (n) (aref "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" n)) chars))))

(defun spotify-doom-generate-code-challenge (code-verifier)
  "Generate a code challenge from the code verifier CODE-VERIFIER."
  (base64-url-encode (secure-hash 'sha256 code-verifier nil nil t)))

(defun spotify-doom-start-web-server ()
  "Start a local web server to handle the OAuth2 callback."
 ;; Set the buffer-local variable to the current code-verifier
  (setq-local spotify-doom-code-verifier code-verifier)
  (httpd-stop) ; Ensure any existing server is stopped before starting a new one
  (setq httpd-port spotify-doom-httpd-port)
  (httpd-start)
  (httpd-define-handler spotify-doom-callback proc request
    "Handle the callback from the Spotify authorization server."
    (spotify-doom-handle-callback request)
    (with-httpd-buffer proc "text/html"
      (insert "<!DOCTYPE html><html><body><h1>Authorization Successful</h1><p>You can close this window.</p></body></<html>"))))
  (defun spotify-doom-authorization-url (code-challenge)
"Generate the authorization URL with the provided code challenge CODE-CHALLENGE."
(format "https://accounts.spotify.com/authorize?client_id=%s&response_type=code&redirect_uri=%s&code_challenge_method=S256&code_challenge=%s&scope=user-read-private%%20user-read-playback-state%%20user-modify-playback-state"
spotify-doom-client-id (url-hexify-string spotify-doom-redirect-uri) code-challenge))

(defun spotify-doom-handle-callback (request)
"Handle the callback from the Spotify authorization server by REQUEST."
(let ((query (url-parse-query-string (cadr (assoc "QUERY_STRING" request)))))
(spotify-doom-exchange-auth-code (cdr (assoc "code" query)))))

(defun spotify-doom-exchange-auth-code (auth-code)
"Exchange the authorization code AUTH-CODE for an access token."
;; TODO: Implement the token exchange here
)

(defun spotify-doom-authorize ()
  "Start the Spotify authorization process using PKCE flow."
  (interactive)
  (let ((code-verifier (spotify-doom-generate-code-verifier))
        (code-challenge (spotify-doom-generate-code-challenge code-verifier)))
    (spotify-doom-start-web-server code-verifier)
    (browse-url (spotify-doom-authorization-url code-challenge))))

(provide 'spotify-doom)

;;; spotify-doom.el ends here

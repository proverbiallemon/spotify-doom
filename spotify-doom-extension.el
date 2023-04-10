;;; spotify-doom-extension.el --- Extend Spotify-Doom Functionality -*- lexical-binding: t -*-

;; Author: Lemon Newyby
;; URL: https://github.com/proverbiallemon/spotify-doom
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (spotify-doom "0.1.1"))

;;; Commentary:

;; This package extends the functionality of the spotify-doom package.

;;; Code:

(require 'spotify-doom)

(defun spotify-doom-extension-play-pause ()
  (interactive)
  (spotify-doom-api-request "me/player/play" "PUT" nil t))

(defun spotify-doom-extension-next-track ()
  (interactive)
  (spotify-doom-api-request "me/player/next" "POST"))

(defun spotify-doom-extension-previous-track ()
  (interactive)
  (spotify-doom-api-request "me/player/previous" "POST"))

(defun spotify-doom-extension-api-request (endpoint method &optional data no-auth)
  (let ((url (concat "https://api.spotify.com/v1/" endpoint))
        (headers `(("Content-Type" . "application/json")
                   ,@(unless no-auth
                       `(("Authorization" . ,(concat "Bearer " spotify-doom-access-token)))))))
    (request url
             :type method
             :data (when data (json-encode data))
             :headers headers
             :parser 'json-read
             :sync t
             :complete (cl-function
                        (lambda (&key response &allow-other-keys)
                          (message "Request completed."))))))

(defvar spotify-doom-extension-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC k p") 'spotify-doom-extension-play-pause)
    (define-key map (kbd "SPC k n") 'spotify-doom-extension-next-track)
    (define-key map (kbd "SPC k b") 'spotify-doom-extension-previous-track)
    map)
  "Keymap for `spotify-doom-extension-mode'.")

(define-minor-mode spotify-doom-extension-mode
  "Minor mode for controlling Spotify."
  :lighter " SD"
  :keymap spotify-doom-extension-mode-map)

(provide 'spotify-doom-extension)

;;; spotify-doom-extension.el ends here

;;; spotify-doom-api.el --- Spotify-Doom API Functions -*- lexical-binding: t -*-

;; Author: Lemon Newyby
;; URL: https://github.com/proverbiallemon/spotify-doom
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (spotify-doom "0.1.1"))

;;; Commentary:

;; This package provides API functions for interacting with the Spotify Web API in the context of the spotify-doom package.


(require 'url)
(require 'json)
(require 'spotify-doom) ;; Assuming you have already defined spotify-doom-access-token in spotify-doom.el

(defun spotify-doom-api-request (endpoint &optional data method)
  (let ((url-request-method (or method "GET"))
        (url-request-data (and data (json-encode data)))
        (url-request-extra-headers `(("Content-Type" . "application/json")
                                     ("Authorization" . ,(concat "Bearer " spotify-doom-access-token)))))
    (with-current-buffer (url-retrieve-synchronously (concat "https://api.spotify.com/v1" endpoint))
      (goto-char url-http-end-of-headers)
      (json-parse-buffer))))

(provide 'spotify-doom-api)
;;; spotify-doom-api.el ends here

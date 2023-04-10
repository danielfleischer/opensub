;;; opensub.el --- search and download from open-subtitles  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Daniel Fleischer

;; Author: Daniel Fleischer <danflscr@gmail.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'webjump)

(webjump-url-encode "robocop is great")

(defcustom opensub-api-key ""
  "API key for opensubtitles.com."
  :type 'string)

(let ((url-request-method "GET")
      (url-request-extra-headers
       `(("Api-Key" . ,opensub-api-key)
         ("Content-Type" . "application/json"))))
  (url-retrieve
   "https://api.opensubtitles.com/api/v1/subtitles?query=robocop"
   'my-switch-to-url-buffer))

(defun my-switch-to-url-buffer (status)
      "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
      (switch-to-buffer (current-buffer)))

;;; Querying
;; curl --request GET \
;;   --url 'https://api.opensubtitles.com/api/v1/subtitles?query=robocop' \
;;   --header 'Api-Key: APIKEY' \
;;   --header 'Content-Type: application/json'


;;; Downloading
;; curl --request POST \
;;   --url https://api.opensubtitles.com/api/v1/download \
;;     --header 'Api-Key: APIKEY' \
;;   --header 'Content-Type: application/json' \
;;   --data '{
;;   "file_id": 12345678
;; }'



(defun opensub ()
  (interactive)
  (message "This is all, folks."))

(provide 'opensub)
;;; opensub.el ends here

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
(require 'json)
(require 'cl)

(defgroup opensub nil
  "Search and fetch from open-subtitles."
  :group 'multimedia)

(defcustom opensub-api-key ""
  "API key for opensubtitles.com."
  :type 'string)

(defcustom opensub-download-directory "~/Downloads/"
  "Directory where subtitles will downloaded."
  :type 'directory)

(defun opensub--curl-retrieve (query)
  (let ((clean-query (webjump-url-encode query))
        (url-request-method "GET")
        (url-request-extra-headers
         `(("Api-Key" . ,opensub-api-key)
           ("Content-Type" . "application/json"))))
    (with-current-buffer
        (url-retrieve-synchronously
         (format "https://api.opensubtitles.com/api/v1/subtitles?query=%s"
                 clean-query))
      (delete-region (point-min) (point))
      (json-read))))

(defun opensub--get-file-id (results)
  (let* ((items (alist-get 'data results))
         (options (cl-mapcar
                   (lambda (item)
                     (let-alist item .attributes.release))
                   items))
         (option (completing-read "Select: " options))
         (item  (cl-find-if (lambda (x) (equal option (let-alist x .attributes.release))) items)))
    (alist-get 'file_id (aref (let-alist item .attributes.files) 0))))


(defun opensub--curl-get-download-info (media-id)
  (let ((url-request-method "POST")
        (url-request-extra-headers
         `(("Api-Key" . ,opensub-api-key)
           ("Content-Type" . "application/json")))
        (url-request-data
         (format "{\"file_id\": %s}" media-id)))
    (with-current-buffer
        (url-retrieve-synchronously
         "https://api.opensubtitles.com/api/v1/download")
      (delete-region (point-min) (point))
      (json-read))))

(defun opensub--download-subtitle (item)
  (let* ((link (alist-get 'link item))
         (filename (file-name-concat opensub-download-directory
                                           (alist-get 'file_name item))))
    (unless (url-copy-file link filename)
      (error "Couldn't download the subtitle. Try again."))))

(defun opensub ()
  (interactive)
  (let* ((query (read-from-minibuffer "Search a show/movie: "))
         (results (opensub--curl-retrieve query))
         (item-id (opensub--get-file-id results))
         (item (opensub--curl-get-download-info item-id)))
    (opensub--download-subtitle item)
    (message "Subtitle downloaded.")))

(provide 'opensub)
;;; opensub.el ends here

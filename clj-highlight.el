;; clj-highlight.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2021 Ewen Grosjean

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Commentary:

;; Code:

(defun clj-highlight/make-overlay ()
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority -50)           ;(bug#16192)
    (overlay-put ol 'face 'isearch)
    ol))

(defun clj-highlight/make-line-overlay ()
  (let ((ol (make-overlay (point) (point))))
    (overlay-put ol 'priority -50)           ;(bug#16192)
    (overlay-put ol 'face 'highlight)
    ol))

(defvar clj-highlight/overlay (clj-highlight/make-overlay))
(defvar clj-highlight/line-overlay (clj-highlight/make-line-overlay))

(defun clj-highlight/highlight (&optional b e)
  (move-overlay clj-highlight/line-overlay
                (line-beginning-position)
                (line-beginning-position 2)
                (current-buffer))
  (if (and b e)
      (move-overlay clj-highlight/overlay b e (current-buffer))
    (delete-overlay clj-highlight/overlay)))

(defun clj-highlight/highlight-no-line (&optional b e)
  (delete-overlay clj-highlight/line-overlay)
  (if (and b e)
      (move-overlay clj-highlight/overlay b e (current-buffer))
    (delete-overlay clj-highlight/overlay)))

(defun clj-highlight/unhighlight ()
  (delete-overlay clj-highlight/overlay)
  (delete-overlay clj-highlight/line-overlay))

(provide 'clj-highlight)

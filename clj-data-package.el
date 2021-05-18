;; clj-data-package.el ---   -*- lexical-binding: t; -*-

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

(require 'package-x)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defvar local-archive
  (expand-file-name "packages/" "~/clj-data.el")
  "Location of the package archive.")
(setq package-archive-upload-base local-archive)

(defun make-package (version)
  (let ((default-directory "~/"))
    (shell-command (format "cp -R clj-data.el clj-data-%s" version))
    (shell-command (format "COPYFILE_DISABLE=1 tar -cvf clj-data-%s.tar --exclude=\"clj-data-%s/.*\" --exclude=\"clj-data-%s/packages\" clj-data-%s/" version version version version))
    (shell-command (format "rm -r ~/clj-data-%s" version))
    (package-upload-file (format "~/clj-data-%s.tar" version))
    (shell-command (format "rm ~/clj-data-%s.tar" version))))

(comment
 (make-package "0.0.1")
 )

;; package-upload-file

(provide 'clj-data-package)

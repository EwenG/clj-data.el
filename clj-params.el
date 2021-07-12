;; clj-params.el ---   -*- lexical-binding: t; -*-

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

(require 'clj-data)
(require 'ivy)

(defun clj-params/param->history (param)
  (make-symbol (format "clj-params/history-%s" param)))

(defun clj-params/boolean->string (b)
  (if b "true" "false"))

(defun clj-params/minibuffer-numeric-inc ()
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (cond ((equal "" content)
           (delete-minibuffer-contents)
           (insert "nil"))
          ((equal "nil" content)
           (delete-minibuffer-contents)
           (insert "0"))
          (t
           (delete-minibuffer-contents)
           (insert (number-to-string (+ 1 (string-to-number content))))))))

(defun clj-params/minibuffer-numeric-dec ()
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (cond ((equal "0" content)
           (delete-minibuffer-contents)
           (insert "nil"))
          (t
           (let ((n (string-to-number content)))
             (when (> n 0)
               (delete-minibuffer-contents)
               (insert (number-to-string (- n 1)))))))))

(defun clj-params/minibuffer-boolean-toggle ()
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (cond ((equal "true" content)
           (delete-minibuffer-contents)
           (insert "false"))
          ((equal "false" content)
           (delete-minibuffer-contents)
           (insert "true"))
          (t
           (delete-minibuffer-contents)
           (insert "false")))))

(defvar clj-params/minibuffer-map-numerical
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "+" 'clj-params/minibuffer-numeric-inc)
    (define-key map "-" 'clj-params/minibuffer-numeric-dec)
    map))

(defvar clj-params/minibuffer-map-boolean
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "+" 'clj-params/minibuffer-boolean-toggle)
    (define-key map "-" 'clj-params/minibuffer-boolean-toggle)
    map))

(defun clj-params/edit-numerical (action-fn param)
  (let ((continue t)
        (value nil)
        (history-add-new-input nil)
        (param-history (get-text-property 0 'clj-params/history param))
        (default-val (prin1-to-string
                      (get-text-property 0 'clj-params/default-val param))))
    (while continue
      (setq value (read-from-minibuffer (concat param ": ")
                                        default-val
                                        clj-params/minibuffer-map-numerical
                                        nil
                                        param-history
                                        default-val))
      (cond ((equal "nil" value)
             (add-to-history param-history value)
             (setq value value)
             (setq continue nil))
            ((string-match-p "^[0-9]+$" value)
             (add-to-history param-history value)
             (setq value value)
             (setq continue nil)))
      (when continue
        (message (concat param " must be nil or a positive numeric value"))
        (sit-for 1)))
    (funcall action-fn param value)))

(defun clj-params/edit-boolean (action-fn param)
  (let ((continue t)
        (value nil)
        (history-add-new-input nil)
        (param-history (get-text-property 0 'clj-params/history param))
        (default-val (clj-params/boolean->string
                      (get-text-property 0 'clj-params/default-val param))))
    (while continue
      (setq value (read-from-minibuffer (concat param ": ")
                                        default-val
                                        clj-params/minibuffer-map-boolean
                                        nil
                                        param-history
                                        default-val))
      (when (or (equal "false" value) (equal "true" value))
        (add-to-history param-history value)
        (setq value value)
        (setq continue nil))
      (when continue
        (message (concat param " must be a boolean (true or false)"))
        (sit-for 1)))
    (funcall action-fn param value)))

(defun clj-params/edit-param (action-fn param)
  (let* ((edit-fn (get-text-property 0 'clj-params/edit-fn param)))
    (funcall edit-fn action-fn param)))

(defun clj-params/param->param-candidate (param)
  (propertize param 'clj-params/history (clj-params/param->history param)))

(defun clj-params/params->params-candidate (params)
  (let ((candidates nil))
    (dolist (param params)
      (push (clj-params/param->param-candidate param) candidates))
    candidates))

(defun clj-params/params* (params action-fn)
  (ivy-read "Parameters: " (clj-params/params->params-candidate params)
            :require-match t
            :action (apply-partially 'clj-params/edit-param action-fn)))

(provide 'clj-params)

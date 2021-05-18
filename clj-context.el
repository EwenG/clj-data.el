;; clj-context.el ---   -*- lexical-binding: t; -*-

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

(defun clj-context/forward-comment ()
  (forward-comment (buffer-size))
  (while (> (skip-chars-forward ",") 0)
    (forward-comment (buffer-size))))

(defvar clj-context/symbol-separators "][\s,\(\)\{\}\"\n\t~@^`;")
(defvar clj-context/symbol-separator-re (concat "^" clj-context/symbol-separators))

(defclass clj-context/object-dispatch-macro ()
  ((dispatch-macro :initarg :dispatch-macro)
   (data :initarg :data)
   (data-start :initarg :data-start)
   (data-end :initarg :data-end)
   (value :initarg :value)))

(defclass clj-context/object-quoted ()
  ((quoted :initarg :quoted)
   (backtick? :initarg :backtick?)
   (splice? :initarg :splice?)
   (value :initarg :value)))

(defclass clj-context/object-deref ()
  ((value :initarg :value)))

(defclass clj-context/object-delimited ()
  ((delimited :initarg :delimited)
   (start :initarg :start)
   (end :initarg :end)))

(defclass clj-context/object-string ()
  ((string :initarg :string)
   (start :initarg :start)
   (end :initarg :end)))

(defclass clj-context/object-symbol ()
  ((symbol :initarg :symbol)
   (start :initarg :start)
   (end :initarg :end)))

(defun clj-context/delimited? (open close from &optional to)
  (let ((to (or to (ignore-errors (scan-sexps from 1)))))
    (when to
      (and (eq (char-after from) open)
           (eq (char-before to) close)))))

(defvar clj-context/platform-tag nil)

(defun clj-context/meta-value (with-meta)
  (if (and (cl-typep with-meta 'clj-context/object-dispatch-macro)
           (eq :meta (oref with-meta :dispatch-macro)))
      (let ((maybe-value (oref with-meta :value)))
        (while (and (cl-typep maybe-value 'clj-context/object-dispatch-macro)
                    (eq :meta (oref maybe-value :dispatch-macro)))
          (setq maybe-value (oref maybe-value :value)))
        maybe-value)
    with-meta))

;; Reader conditional is more complex than others reader macros and requires special treatment
;; We read the pairs of [platform-tag object] and return the object that match the current
;; platform tag, or the :default platform tag
(defun clj-context/read-conditional-reader ()
  (let ((exit nil)
        (reader-conditional-candidate nil))
    (while (null exit)
      (let* ((platform-tag-object (clj-context/read-one))
             (platform-tag-object-meta-value (clj-context/meta-value platform-tag-object))
             (_ (clj-context/forward-comment))
             (object (clj-context/read-one)))
        (if (and platform-tag-object-meta-value object)
            (cond ((and (cl-typep platform-tag-object-meta-value 'clj-context/object-symbol)
                        (equal clj-context/platform-tag
                               (oref platform-tag-object-meta-value :symbol)))
                   (setq reader-conditional-candidate object)
                   (setq exit t))
                  ((and (cl-typep platform-tag-object-meta-value 'clj-context/object-symbol)
                        (equal ":default" (oref platform-tag-object-meta-value :symbol)))
                   (setq reader-conditional-candidate object)
                   (clj-context/forward-comment))
                  (t (clj-context/forward-comment)))
          (setq exit t))))
    reader-conditional-candidate))

;; When read-one reaches a splice-end, it jumps out of the wrapping form
;; Useful to implement reader splicing forms
(defvar clj-context/splice-ends nil)

(defun clj-context/splice-end-comparator (x1 x2)
  (> (car x1) (car x2)))

(defun clj-context/find-splice-end (p)
  (let ((splice-ends clj-context/splice-ends)
        (candidate nil))
    (while (and splice-ends (null candidate))
      (let ((splice-from (caar splice-ends)))
        (cond ((equal splice-from p)
               (setq candidate (car splice-ends)))
              ((> p splice-from)
               (setq splice-ends nil))
              (t (setq splice-ends (cdr splice-ends))))))
    candidate))

;; Keeps splice-ends sorted by decreasing order
(defun clj-context/add-splice-end (new-splice-end)
  (let ((splice-end (car clj-context/splice-ends)))
    (if (or (null splice-end)
            (> (car new-splice-end) (car splice-end)))
        (push new-splice-end clj-context/splice-ends)
      (push new-splice-end clj-context/splice-ends)
      (setq clj-context/splice-ends (sort clj-context/splice-ends
                                               'clj-context/splice-end-comparator))
      (delete-dups clj-context/splice-ends))))

(defun clj-context/read-one ()
  (let* ((p (point))
         (p1+ (+ 1 p))
         (p2+ (+ 2 p))
         (p3+ (+ 3 p))
         (char1+ (char-before p1+))
         (char2+ (char-before p2+))
         (char3+ (char-before p3+)))
    (cond ((eq char1+ ?\()
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\) (char-before forward-p)))
               (goto-char forward-p)
               (clj-context/object-delimited
                :delimited :list
                :start p
                :end forward-p))))
          ((eq char1+ ?\[)
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\] (char-before forward-p)))
               (goto-char forward-p)
               (clj-context/object-delimited
                :delimited :vector
                :start p
                :end forward-p))))
          ((eq char1+ ?\{)
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\} (char-before forward-p)))
               (goto-char forward-p)
               (clj-context/object-delimited
                :delimited :map
                :start p
                :end forward-p))))
          ((eq char1+ ?\")
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\" (char-before forward-p)))
               (goto-char forward-p)
               (clj-context/object-string
                :string (buffer-substring-no-properties (+ p 1) (- forward-p 1))
                :start p
                :end forward-p))))
          ((eq ?# char1+)
           (cond ((eq ?: char2+)
                  (skip-chars-forward clj-context/symbol-separator-re)
                  (let ((namespace (buffer-substring-no-properties p1+ (point))))
                    (clj-context/forward-comment)
                    (clj-context/object-dispatch-macro
                     :dispatch-macro :namespaced-map
                     :data namespace
                     :value (clj-context/read-one))))
                 ((and (eq ?? char2+) (eq ?@ char3+))
                  (goto-char p3+)
                  (clj-context/forward-comment)
                  (let ((reader-conditional-content (clj-context/read-one)))
                    (if (and (cl-typep reader-conditional-content
                                       'clj-context/object-delimited)
                             (eq :list (oref reader-conditional-content :delimited)))
                        (progn
                          (goto-char (+ 1 (oref reader-conditional-content :start)))
                          (clj-context/forward-comment)
                          (let ((object (clj-context/read-conditional-reader)))
                            (cond ((null object)
                                   (goto-char (oref reader-conditional-content :end))
                                   (clj-context/forward-comment)
                                   (clj-context/read-one))
                                  ((cl-typep object 'clj-context/object-delimited)
                                   (clj-context/add-splice-end
                                    `(,(- (oref object :end) 1) .
                                      ,(oref reader-conditional-content :end)))
                                   (goto-char (+ 1 (oref object :start)))
                                   (clj-context/forward-comment)
                                   (clj-context/read-one))
                                  (t
                                   (goto-char (oref reader-conditional-content :end))
                                   object))))
                      reader-conditional-content)))
                 ((and (eq ?? char2+))
                  (goto-char p2+)
                  (clj-context/forward-comment)
                  (let ((reader-conditional-content (clj-context/read-one)))
                    (if (and (cl-typep reader-conditional-content
                                       'clj-context/object-delimited)
                             (eq :list (oref reader-conditional-content :delimited)))
                        (progn
                          (goto-char (+ 1 (oref reader-conditional-content :start)))
                          (clj-context/forward-comment)
                          (let ((object (clj-context/read-conditional-reader)))
                            (goto-char (oref reader-conditional-content :end))
                            (or object
                                (progn
                                  (clj-context/forward-comment)
                                  (clj-context/read-one)))))
                      reader-conditional-content)))
                 ((eq ?# char2+)
                  (goto-char p2+)
                  (clj-context/forward-comment)
                  (clj-context/object-dispatch-macro
                   :dispatch-macro :symbolic-value
                   :data nil
                   :value (clj-context/read-one)))
                 ((eq ?' char2+)
                  (goto-char p2+)
                  (clj-context/forward-comment)
                  (clj-context/object-dispatch-macro
                   :dispatch-macro :var
                   :data nil
                   :value (clj-context/read-one)))
                 ((eq ?_ char2+)
                  (goto-char p2+)
                  (clj-context/forward-comment)
                  (clj-context/object-dispatch-macro
                   :dispatch-macro :discard
                   :data nil
                   :value (clj-context/read-one)))
                 ((eq ?= char2+)
                  (goto-char p2+)
                  (clj-context/object-dispatch-macro
                   :dispatch-macro :eval
                   :data nil
                   :value (clj-context/read-one)))
                 ((eq ?^ char2+)
                  (goto-char p2+)
                  (clj-context/forward-comment)
                  (let ((data-start (point))
                        (data (clj-context/read-one))
                        (data-end (point))
                        (_ (clj-context/forward-comment))
                        (value (clj-context/read-one)))
                    (clj-context/object-dispatch-macro
                     :dispatch-macro :meta
                     :data data
                     :data-start data-start
                     :data-end data-end
                     :value value)))
                 ((clj-context/delimited? ?\( ?\) p1+)
                  (goto-char p1+)
                  (clj-context/object-dispatch-macro
                   :dispatch-macro :fn
                   :data nil
                   :value (clj-context/read-one)))
                 ((clj-context/delimited? ?\{ ?\} p1+)
                  (goto-char p1+)
                  (clj-context/object-dispatch-macro
                   :dispatch-macro :set
                   :data nil
                   :value (clj-context/read-one)))
                 ((clj-context/delimited? ?\" ?\" p1+)
                  (goto-char p1+)
                  (clj-context/object-dispatch-macro
                   :dispatch-macro :regexp
                   :data nil
                   :value (clj-context/read-one)))
                 (t (goto-char p1+)
                    (clj-context/forward-comment)
                    (let ((dispatch-macro :tagged-literal)
                          (data-start (point))
                          (data (clj-context/read-one))
                          (data-end (point))
                          (_ (clj-context/forward-comment))
                          (value (clj-context/read-one)))
                      (clj-context/object-dispatch-macro
                       :dispatch-macro dispatch-macro
                       :data data
                       :data-start data-start
                       :data-end data-end
                       :value value)))))
          ((eq ?^ char1+)
           (goto-char p1+)
           (clj-context/forward-comment)
           (let ((dispatch-macro :meta)
                 (data-start (point))
                 (data (clj-context/read-one))
                 (data-end (point))
                 (_ (clj-context/forward-comment))
                 (value (clj-context/read-one)))
             (clj-context/object-dispatch-macro
              :dispatch-macro dispatch-macro
              :data data
              :data-start data-start
              :data-end data-end
              :value value)))
          ((eq ?~ char1+)
           (if (eq ?@ char2+)
               (progn
                 (goto-char p2+)
                 (clj-context/object-quoted
                  :quoted :unquote
                  :backtick? nil
                  :splice? t
                  :value (clj-context/read-one)))
             (goto-char p1+)
             (clj-context/object-quoted
              :quoted :unquote
              :backtick? nil
              :splice? nil
              :value (clj-context/read-one))))
          ((or (eq ?' char1+) (eq ?` char1+))
           (goto-char p1+)
           (clj-context/object-quoted
            :quoted :quote
            :backtick? (eq ?` char1+)
            :splice? nil
            :value (clj-context/read-one)))
          ((eq ?@ char1+)
           (goto-char p1+)
           (clj-context/forward-comment)
           (clj-context/object-deref
            :value (clj-context/read-one)))
          (t
           (let ((skip-end-at-point (clj-context/find-splice-end (point))))
             (if skip-end-at-point
                 (progn
                   (goto-char (cdr skip-end-at-point))
                   (clj-context/forward-comment)
                   (clj-context/read-one))
               (skip-chars-forward clj-context/symbol-separator-re)
               (when (> (point) p)
                 (clj-context/object-symbol
                  :symbol (buffer-substring-no-properties p (point))
                  :start p
                  :end (point)))))))))

(defun clj-context/comint-previous-prompt-position ()
  (let ((p (point)))
    (when (null (comint-previous-prompt 1))
      (goto-char (point-min)))
    (let ((comint-previous-prompt-position (point)))
      (goto-char p)
      comint-previous-prompt-position)))

(defun clj-context/syntax-ppss (pos)
  (if (derived-mode-p 'comint-mode)
      (parse-partial-sexp (clj-context/comint-previous-prompt-position) pos)
    (syntax-ppss pos)))

(defun clj-context/skip-delimited-backward ()
  (let ((p (point)))
    (when (or (equal (char-before p) ?\})
              (equal (char-before p) ?\))
              (equal (char-before p) ?\]))
      (ignore-errors (goto-char (scan-sexps p -1))))))

;; Skip backward the dispatch macro characters, or quoted characters, if any
(defun clj-context/maybe-skip-dispatch-macro-or-quoted-backward ()
  (when (not (bobp))
    (let ((p (point)))
      (skip-chars-backward ",\s")
      (if (not (bobp))
          (progn
            (clj-context/skip-delimited-backward)
            (skip-chars-backward clj-context/symbol-separator-re)
            (let ((object-start (point))
                  (object (clj-context/read-one))
                  (object-end (point)))
              (if (and object (> object-end p))
                  (goto-char object-start)
                (goto-char p))))
        (goto-char p)))))

(provide 'clj-context)

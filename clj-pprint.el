;; clj-pprint.el ---   -*- lexical-binding: t; -*-

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

(require 'clj-context)
(require 'clj-print)

(defvar clj-pprint/threshold 20)
(defvar clj-pprint/large-threshold 40)

(defvar clj-pprint/line-length nil)
(defvar clj-pprint/line-length-max nil)
(defvar clj-pprint/is-multi-line? nil)

;; Different then the clj-context symbol separators since the pretty printer is intended
;; at printing data, not code entered by the user
;; For example (symbol "ee~ee") prints ee~ee
;; Although we cannot handle all cases, for example, (symbol "ee ee") prints ee ee
(defvar clj-pprint/symbol-separators "][\s,\(\)\{\}\n\t")
(defvar clj-pprint/symbol-separator-re (concat "^" clj-pprint/symbol-separators))

(defun clj-pprint/read-one ()
  (let* ((p (point))
         (p1+ (+ 1 p))
         (p2+ (+ 2 p))
         (p3+ (+ 3 p))
         (char1+ (char-before p1+))
         (char2+ (char-before p2+))
         (char3+ (char-before p3+)))
    (cond ((eq char1+ ?\() :list)
          ((eq char1+ ?\[) :vector)
          ((eq char1+ ?\{) :map)
          ((eq char1+ ?\") :string)
          ((eq ?# char1+)
           (cond ((eq ?: char2+) :namespaced-map)
                 ((and (eq ?? char2+) (eq ?@ char3+)) :reader-conditional-spliced)
                 ((and (eq ?? char2+)) :reader-conditional)
                 ((eq ?# char2+) :symbolic-value)
                 ((eq ?' char2+) :var)
                 ((eq ?_ char2+) :discard)
                 ((eq ?= char2+) :eval)
                 ((eq ?^ char2+) :dispatch-macro-meta)
                 ((eq ?\( char2+) :fn)
                 ((eq ?\{ char2+) :set)
                 ((eq ?\" char2+) :regexp)
                 (t :tagged-literal)))
          ((eq ?^ char1+) :meta)
          ((eq ?~ char1+) (if (eq ?@ char2+) :unquote-splicing :unquote))
          ((or (eq ?' char1+) (eq ?` char1+)) (if (eq ?` char1+) :quote-backtick :quote))
          ((eq ?@ char1+) :deref)
          (t :symbol))))

(defun clj-pprint/pprint-symbol ()
  (setq clj-pprint/is-multi-line? nil)
  (let ((start (point)))
    (skip-chars-forward clj-pprint/symbol-separator-re)
    (when (> (point) start)
      (- (point) start))))

(defun clj-pprint/pprint-string ()
  (setq clj-pprint/is-multi-line? nil)
  (let* ((start (point))
         (forward-p (ignore-errors (scan-sexps start 1))))
    (when forward-p
      (goto-char forward-p)
      (- (point) start))))

(defun clj-pprint/indent
    (object-line-length-max object-start object-end indent-length)
  (let ((line-length-before-object clj-pprint/line-length))
    (if clj-pprint/is-multi-line?
        (progn
          (beginning-of-line)
          (setq clj-pprint/line-length (+ (- object-end (point)) indent-length))
          (while (> (point) object-start)
            (insert-char ?\s indent-length t)
            (setq object-end (+ indent-length object-end))
            (forward-line -1))
          (goto-char object-end)
          (setq clj-pprint/line-length-max (max
                                                  clj-pprint/line-length-max
                                                  (+ line-length-before-object
                                                     object-line-length-max))))
      (setq clj-pprint/line-length (+ clj-pprint/line-length object-line-length-max))
      (setq clj-pprint/line-length-max (max clj-pprint/line-length-max
                                                  clj-pprint/line-length)))))

(defun clj-pprint/pprint-sequential (seq)
  (let ((continue t)
        (first-element? t)
        (is-multi-line? nil))
    (let* ((open-delimiter-length (if (eq :set seq) 2 1))
           (clj-pprint/line-length open-delimiter-length)
           (clj-pprint/line-length-max open-delimiter-length))
      (forward-char open-delimiter-length)
      (while continue
        (let ((object-start (point)))
          (clj-context/forward-comment)
          (delete-region object-start (point))
          (let ((object-line-length-max (clj-pprint/pprint)))
            (if (null object-line-length-max)
                (setq continue nil)
              (let ((object-end (point)))
                (cond (first-element?
                       (clj-pprint/indent
                        object-line-length-max object-start object-end open-delimiter-length)
                       (when clj-pprint/is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq is-multi-line? t)
                         (setq clj-pprint/line-length 0))
                       (setq first-element? nil))
                      ((equal clj-pprint/line-length 0)
                       (goto-char object-start)
                       (insert-char ?\s open-delimiter-length t)
                       (setq object-start (+ open-delimiter-length object-start))
                       (setq object-end (+ open-delimiter-length object-end))
                       (setq clj-pprint/line-length open-delimiter-length)
                       (goto-char object-end)
                       (clj-pprint/indent
                        clj-pprint/line-length-max
                        object-start object-end open-delimiter-length)
                       (when clj-pprint/is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq clj-pprint/line-length 0)))
                      ((> (+ clj-pprint/line-length object-line-length-max 1)
                          clj-pprint/threshold)
                       (goto-char object-start)
                       (insert-char ?\n 1 t) (insert-char ?\s open-delimiter-length t)
                       (setq object-start (+ 1 open-delimiter-length object-start))
                       (setq object-end (+ 1 open-delimiter-length object-end))
                       (setq clj-pprint/line-length open-delimiter-length)
                       (goto-char object-end)
                       (clj-pprint/indent
                        object-line-length-max object-start object-end open-delimiter-length)
                       (setq is-multi-line? t)
                       (when clj-pprint/is-multi-line?
                         (setq is-multi-line? t)
                         (insert-char ?\n 1 t)
                         (setq clj-pprint/line-length 0)))
                      (t
                       (goto-char object-start)
                       (insert-char ?\s 1 t)
                       (setq object-start (+ 1 object-start))
                       (setq object-end (+ 1 object-end))
                       (setq clj-pprint/line-length (+ 1 clj-pprint/line-length))
                       (goto-char object-end)
                       (clj-pprint/indent
                        object-line-length-max object-start object-end
                        clj-pprint/line-length)
                       (when clj-pprint/is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq is-multi-line? t)
                         (setq clj-pprint/line-length 0)))))))))
      (when (equal clj-pprint/line-length 0)
        (delete-char -1))
      (setq clj-pprint/is-multi-line? is-multi-line?)
      (forward-char)
      (max clj-pprint/line-length-max (+ clj-pprint/line-length 1)))))

(defun clj-pprint/pprint-map ()
  (let ((i 0)
        (continue t)
        (clj-pprint/line-length 1)
        (clj-pprint/line-length-max 1)
        (first-element? t)
        (is-multi-line? nil))
    (forward-char)
    (while continue
      (let ((object-start (point)))
        (clj-context/forward-comment)
        (delete-region object-start (point))
        (let ((object-line-length-max (clj-pprint/pprint)))
          (if (null object-line-length-max)
              (setq continue nil)
            (let ((object-end (point)))
              (cond (first-element?
                     (clj-pprint/indent object-line-length-max object-start object-end 1)
                     (when clj-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq is-multi-line? t)
                       (setq clj-pprint/line-length 0))
                     (setq first-element? nil))
                    ((equal clj-pprint/line-length 0)
                     (goto-char object-start)
                     (insert-char ?\s 1 t)
                     (setq object-start (+ 1 object-start))
                     (setq object-end (+ 1 object-end))
                     (setq clj-pprint/line-length 1)
                     (goto-char object-end)
                     (clj-pprint/indent object-line-length-max object-start object-end 1)
                     (setq is-multi-line? t)
                     (when clj-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq clj-pprint/line-length 0)))
                    ((and
                      (> clj-pprint/line-length clj-pprint/threshold)
                      (> (+ clj-pprint/line-length object-line-length-max 1)
                         clj-pprint/large-threshold))
                     (goto-char object-start)
                     (insert-char ?\n 1 t) (insert-char ?\s 1 t)
                     (setq object-start (+ 2 object-start))
                     (setq object-end (+ 2 object-end))
                     (setq clj-pprint/line-length 1)
                     (goto-char object-end)
                     (clj-pprint/indent
                      object-line-length-max object-start object-end 1)
                     (setq is-multi-line? t)
                     (when clj-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq clj-pprint/line-length 0)))
                    (t
                     (goto-char object-start)
                     (insert-char ?\s 1 t)
                     (setq object-start (+ 1 object-start))
                     (setq object-end (+ 1 object-end))
                     (setq clj-pprint/line-length (+ 1 clj-pprint/line-length))
                     (goto-char object-end)
                     (clj-pprint/indent
                      object-line-length-max object-start object-end clj-pprint/line-length)
                     (when clj-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq is-multi-line? t)
                       (setq clj-pprint/line-length 0))))
              (when (and (equal 1 (logand i 1)) (> clj-pprint/line-length 0))
                (insert-char ?\n 1 t)
                (setq clj-pprint/line-length 0))
              (setq i (+ 1 i)))))))
    (when (equal clj-pprint/line-length 0)
      (delete-char -1))
    (setq clj-pprint/is-multi-line? is-multi-line?)
    (forward-char)
    (max clj-pprint/line-length-max (+ clj-pprint/line-length 1))))

(defun clj-pprint/pprint-dispatch-macro (dm)
  (cond ((or
          (eq dm :meta)
          (eq dm :dispatch-macro-meta)
          (eq dm :tagged-literal)
          (eq dm :namespaced-map))
         (let ((dispatch-macro-length (if (eq :dispatch-macro-meta dm) 2 1)))
           (forward-char dispatch-macro-length)
           (let ((data-start (point)))
             (let ((clj-pprint/line-length dispatch-macro-length)
                   (clj-pprint/line-length-max dispatch-macro-length))
               (clj-context/forward-comment)
               ;; Special case for print-level "#" symbol
               (if (and (equal dm :tagged-literal)
                        (> (point) data-start))
                   (progn
                     (goto-char data-start)
                     (setq clj-pprint/is-multi-line? nil)
                     1)
                 (delete-region data-start (point))
                 (let ((data-line-length-max (or (clj-pprint/pprint) 0))
                       (data-end (point))
                       (data-is-multi-line? clj-pprint/is-multi-line?))
                   (clj-pprint/indent
                    data-line-length-max data-start data-end dispatch-macro-length)
                   (let ((data-end (point))
                         (value-start (point)))
                     (clj-context/forward-comment)
                     (delete-region value-start (point))
                     (let* ((value-line-length-max (or (clj-pprint/pprint) 0))
                            (value-end (point))
                            (value-is-multi-line? clj-pprint/is-multi-line?)
                            (break-line? (or
                                          data-is-multi-line? value-is-multi-line?
                                          (and
                                           (> data-line-length-max clj-pprint/threshold)
                                           (> (+ data-line-length-max value-line-length-max 1)
                                              clj-pprint/large-threshold)))))
                       (cond (break-line?
                              (goto-char value-start)
                              (insert-char ?\n 1 t) (insert-char ?\s dispatch-macro-length t)
                              (setq value-start (+ value-start 1 dispatch-macro-length))
                              (setq value-end (+ value-end 1 dispatch-macro-length))
                              (setq clj-pprint/line-length dispatch-macro-length)
                              (setq clj-pprint/is-multi-line? t)
                              (goto-char value-end)
                              (clj-pprint/indent
                               value-line-length-max value-start value-end dispatch-macro-length)
                              (let ((value-end (point)))
                                (goto-char value-start)
                                (let ((next-thing (clj-pprint/read-one)))
                                  (when (or (eq :list next-thing)
                                            (eq :vector next-thing)
                                            (eq :set next-thing)
                                            (eq :map next-thing))
                                    (goto-char value-start)
                                    (let ((delimiter-start (char-after)))
                                      (delete-char 1)
                                      (insert-char ?\s 1 t)
                                      (goto-char value-end)
                                      (let ((delimiter-end (char-before)))
                                        (delete-char -1)
                                        (insert-char ?\n 1 t)
                                        (goto-char data-end)
                                        (insert-char ?\s 1 t)
                                        (insert-char delimiter-start 1 t)
                                        (goto-char (+ 2 value-end))
                                        (insert-char ?\s 1 t)
                                        (insert-char delimiter-end 1 t)))))))
                             ((> value-line-length-max 0)
                              (goto-char value-start)
                              (insert-char ?\s 1 t)
                              (setq value-start (+ 1 value-start))
                              (setq value-end (+ 1 value-end))
                              (setq clj-pprint/line-length
                                    (+ 1 clj-pprint/line-length))
                              (setq clj-pprint/is-multi-line? nil)
                              (goto-char value-end)
                              (clj-pprint/indent
                               value-line-length-max value-start value-end
                               clj-pprint/line-length)))
                       clj-pprint/line-length-max))))))))
        ((or (eq dm :reader-conditional-spliced)
             (eq dm :reader-conditional)
             (eq dm :symbolic-value)
             (eq dm :var)
             (eq dm :discard)
             (eq dm :eval)
             (eq dm :fn)
             (eq dm :regexp))
         (let ((dispatch-macro-length (cond ((or (eq dm :fn) (eq dm :regexp)) 1)
                                            ((eq dm :reader-conditional-spliced) 3)
                                            (t 2))))
           (forward-char dispatch-macro-length)
           (let ((clj-pprint/line-length dispatch-macro-length)
                 (clj-pprint/line-length-max dispatch-macro-length)
                 (object-start (point)))
             (clj-context/forward-comment)
             (delete-region object-start (point))
             (let ((object-line-length-max (or (clj-pprint/pprint) 0)))
               (clj-pprint/indent
                object-line-length-max object-start (point) dispatch-macro-length)
               clj-pprint/line-length-max))))))

(defun clj-pprint/pprint-quoted (q)
  (let ((quote-length (if (eq q :unquote-splicing) 2 1)))
    (let ((clj-pprint/line-length quote-length)
          (clj-pprint/line-length-max quote-length))
      (forward-char quote-length)
      (let* ((object-start (point))
             (object-line-length-max (or (clj-pprint/pprint) 0)))
        (clj-pprint/indent
         object-line-length-max object-start (point) quote-length)
        clj-pprint/line-length-max))))

(defun clj-pprint/pprint-deref ()
  (forward-char 1)
  (let ((clj-pprint/line-length 1)
        (clj-pprint/line-length-max 1)
        (object-start (point)))
    (clj-context/forward-comment)
    (delete-region object-start (point))
    (let ((object-line-length-max (or (clj-pprint/pprint) 0)))
      (clj-pprint/indent
       object-line-length-max object-start (point) 1)
      clj-pprint/line-length-max)))

(defun clj-pprint/pprint-dispatch (next-thing)
  (cond ((eq :symbol next-thing) (clj-pprint/pprint-symbol))
        ((eq :string next-thing) (clj-pprint/pprint-string))
        ((or (eq :list next-thing) (eq :vector next-thing) (eq :set next-thing))
         (clj-pprint/pprint-sequential next-thing))
        ((or (eq :map next-thing)) (clj-pprint/pprint-map))
        ((or (eq :quote next-thing)
             (eq :unquote next-thing)
             (eq :unquote-splicing next-thing)
             (eq :quote-backtick next-thing))
         (clj-pprint/pprint-quoted next-thing))
        ((eq :deref next-thing) (clj-pprint/pprint-deref))
        (t
         (clj-pprint/pprint-dispatch-macro next-thing))))

(defun clj-pprint/pprint ()
  (when-let (next-thing (clj-pprint/read-one))
    (clj-pprint/pprint-dispatch next-thing)))

(defun clj-pprint/walk-init ()
  (let ((p (point)))
    (if (or (equal ?\) (char-before p))
            (equal ?\} (char-before p))
            (equal ?\] (char-before p)))
        (forward-char -1)
      (skip-chars-backward clj-pprint/symbol-separator-re))
    (if (or (not (derived-mode-p 'comint-mode))
            (and (equal (get-char-property (point) 'field) 'output)
                 (>= p (comint-line-beginning-position))))
        (let ((top-level (or (syntax-ppss-toplevel-pos (clj-context/syntax-ppss (point)))
                             (point))))
          (goto-char top-level)
          (let ((object (clj-context/read-one)))
            (if object
                (progn
                  (goto-char top-level)
                  (clj-context/maybe-skip-dispatch-macro-or-quoted-backward)
                  (point))
              (goto-char p)
              nil)))
      (goto-char p)
      nil)))

(defun clj-pprint/pprint** ()
  (let ((clj-context/splice-ends '())
        (clj-pprint/is-multi-line? nil))
    (let ((top-level (clj-pprint/walk-init)))
      (when top-level
        (let ((inhibit-field-text-motion t)
              (inhibit-read-only t))
          (when (not (bolp))
            (insert ?\n))
          (clj-pprint/pprint)
          (when (derived-mode-p 'comint-mode)
            (put-text-property top-level (point) 'field 'output)))))))

(defun clj-pprint/pprint* ()
  (if font-lock-mode
      (progn
        (font-lock-mode -1)
        (clj-pprint/pprint**)
        (font-lock-mode 1))
    (clj-pprint/pprint**)))

(defun clj-pprint/pprint-str (o)
  (with-temp-buffer
    (clj-print/print o)
    (let ((clj-context/platform-tag ":clj"))
      (clj-pprint/pprint*))
    (buffer-substring (point-min) (point-max))))

(defun clj-pprint/pprint-error-str (o)
  (let ((clj-pprint/threshold 80))
    (clj-pprint/pprint-str o)))

(provide 'clj-pprint)

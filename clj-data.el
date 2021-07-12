;; clj-data.el ---   -*- lexical-binding: t; -*-

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

;; Version 0.0.3-SNAPSHOT
;; Package-Requires: ((emacs "27.1") (ivy "0.13.0"))

;; Commentary:

;; Code:

(defconst clj-data/version "0.0.3-SNAPSHOT")

(defun clj-data/hash-map (&rest data)
  (let ((l (length data)))
    (when (not (= 0 (logand l 1)))
      (error "Map must contain an even number of forms"))
    (let ((m (make-hash-table :test 'equal))
          (data-rest data))
      (while data-rest
        (puthash (car data-rest) (cadr data-rest) m)
        (setq data-rest (cddr data-rest)))
      m)))

(defun clj-data/get (hash key &optional default)
  (if (null hash)
      nil
    (gethash key hash default)))

(defun clj-data/get-in-helper (hash ks default-val)
  (let ((k (car ks))
        (ks-rest (cdr ks)))
    (if (or (null ks-rest) (null hash))
        (clj-data/get hash k default-val)
      (clj-data/get-in-helper (clj-data/get hash k) ks-rest default-val))))

(defun clj-data/get-in (hash ks &optional default-val)
  (if (equal 0 (length ks))
      hash
    (clj-data/get-in-helper hash (append ks '()) default-val)))

(defun clj-data/copy-hash-table (hash)
  (if (hash-table-p hash)
      (copy-hash-table hash)
    (clj-data/hash-map)))

(defun clj-data/assoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (puthash (car kvs) (cadr kvs) hash)
      (clj-data/assoc-helper hash (cddr kvs)))))

(defun clj-data/assoc (hash &rest kvs)
  (let ((args-length (1+ (length kvs))))
    (when (= 0 (logand 1 args-length))
      (error
       "clj-data/assoc expects even number of arguments after hashtable, found odd number"))
    (clj-data/assoc-helper (clj-data/copy-hash-table hash) kvs)))

(defun clj-data/assoc-in-helper (hash ks v ks-length-dec ks-index)
  (let ((copy (clj-data/copy-hash-table hash)))
    (if (equal ks-index ks-length-dec)
        (progn (puthash (elt ks ks-index) v copy)
               copy)
      (progn (puthash (elt ks ks-index)
                      (clj-data/assoc-in-helper
                       (gethash (elt ks ks-index) copy) ks v ks-length-dec (+ 1 ks-index)) 
                      copy)
               copy))))

(defun clj-data/assoc-in (hash ks v)
  (let ((ks-length (length ks)))
    (if (equal 0 ks-length)
        (clj-data/copy-hash-table hash)
      (clj-data/assoc-in-helper hash ks v (- ks-length 1) 0))))

(defun clj-data/dissoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (remhash (car kvs) hash)
      (clj-data/dissoc-helper hash (cdr kvs)))))

(defun clj-data/dissoc (hash &rest kvs)
  (clj-data/dissoc-helper (clj-data/copy-hash-table hash) kvs))

(defun clj-data/update-in-helper (hash ks f args)
  (let ((k (car ks))
        (k-rest (cdr ks)))
    (if (null k-rest)
        (clj-data/assoc hash k (apply f (clj-data/get hash k) args))
      (clj-data/assoc
       hash k
       (clj-data/update-in-helper (clj-data/get hash k) k-rest f args)))))

(defun clj-data/update-in (hash ks f &rest args)
  (let ((ks (append ks '())))
    (if (equal 0 (length ks))
        (clj-data/copy-hash-table hash)
      (clj-data/update-in-helper hash ks f args))))

(defun clj-data/keys (hash)
  (when hash (hash-table-keys hash)))

(defun clj-data/vals (hash)
  (when hash (hash-table-values hash)))

(defconst clj-data/nothing (make-symbol "nothing"))

(defun clj-data/contains? (hash key)
  (not (eq clj-data/nothing (gethash key hash clj-data/nothing))))

(defun clj-data/all? (pred hash)
  (let ((res t))
    (maphash (lambda (k v)
               (when res
                 (when (not (funcall pred k v))
                   (setq res nil))))
             hash)
    res))

(defun clj-data/any? (pred hash)
  (let ((res nil))
    (maphash (lambda (k v)
               (when (not res)
                 (when (funcall pred k v)
                   (setq res t))))
             hash)
    res))

;; Not hash-map specific
(defun clj-data/conj (coll x &rest xs)
  (cond ((listp coll) (if (null xs)
                          (cons x coll)
                        (apply 'clj-data/conj (cons x coll) (car xs) (cdr xs))))
        ((arrayp coll) (vconcat coll `[,x] xs))
        ;; conj to hash-tables not yet implemented
        (t (error "cannot conj to %s" coll))))

(defun clj-data/into (to from)
  (cond ((listp from) (dolist (x from)
                        (setq to (clj-data/conj to x))))
        ((arrayp from) (let* ((l (length from))
                              (i 0))
                         (while (< i l)
                           (setq to (clj-data/conj to (aref from i)))
                           (setq i (+ i 1)))))
        ;; into from hash-tables not yet implemented
        (t (error "cannot into from %s" from)))
  to)

(defun clj-data/count (coll)
  (if (hash-table-p coll)
      (hash-table-count coll)
    (length coll)))

(defun clj-data/merge (hash &rest xs)
  (let ((merged (clj-data/copy-hash-table hash)))
    (dolist (x xs)
      (when x
        (maphash (lambda (k v) (puthash k v merged)) x)))
    merged))

(provide 'clj-data)

;; byte-recompile to check warnings ----  C-u 0 M-x byte-recompile-directory

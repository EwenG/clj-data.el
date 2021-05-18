;; clj-browse.el ---   -*- lexical-binding: t; -*-

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
(require 'clj-pprint)
(require 'clj-print)
(require 'clj-context)

(defvar-local clj-browse/browse-path nil)

(defvar clj-browse/no-candidate "")

(defvar clj-browse/temporary-browse-path nil)
(defvar clj-browse/candidate->index nil)
(defvar clj-browse/index->pos nil)

(defvar clj-browse/value nil)

(defun clj-browse/pprint (val)
  (let ((inhibit-read-only t)
        (clj-pprint/is-multi-line? nil)
        (p (point)))
    (font-lock-mode -1)
    (erase-buffer)
    (insert val)
    (goto-char (point-min))
    (let* ((p-min (point-min))
           (p-max (point-max))
           (printed-raw (buffer-substring-no-properties p-min p-max)))
      (clj-pprint/pprint)
      (put-text-property p-min (max p-min (+ 1 p-min))
                         'clj-browse/raw-printed
                         printed-raw))
    (font-lock-mode 1)
    (goto-char p)
    (set-buffer-modified-p nil)))

(defun clj-browse/init-browse-buffer (browse-buffer)
  (with-current-buffer browse-buffer
    (buffer-disable-undo browse-buffer)
    (clojure-mode)
    (setq buffer-read-only t)
    (setq clj-browse/browse-path '())))

(comment
 (clj-browse/init-browse-buffer (get-buffer "test.result"))
 )

(defun clj-browse/do-browse (browse-buffer value)
  (clj-browse/init-browse-buffer browse-buffer)
  (with-current-buffer browse-buffer
    (clj-browse/minor-mode)
    (setq clj-browse/value value)
    (clj-browse/refresh))
  (pop-to-buffer-same-window browse-buffer))

(defun clj-browse/browse-get (val k)
  (cond ((hash-table-p val) (clj-data/get val k))
        ((and (listp val) (numberp k)) (nth k val))
        ((and (vectorp val) (numberp k) (< k (length val))) (elt val k))
        (t nil)))

(defun clj-browse/browse-get-in (val ks)
  (seq-reduce 'clj-browse/browse-get ks val))

(defun clj-browse/browse-path-values (browse-path)
  (nreverse
   (mapcar (apply-partially 'get-text-property 0 'clj-browse/browse-value)
           browse-path)))

(comment
 (clj-browse/browse-get-in (clj-data/hash-map :id "blank-channel"
                                              :title "Blank Channel"
                                              :content (list (vector "Channel content1")))
                           (list :content 0 0))

 (clj-browse/browse-get-in (clj-data/hash-map :id "blank-channel"
                                              :title "Blank Channel"
                                              :content (list (vector "Channel content1")))
                           nil)
 )

(defun clj-browse/refresh ()
  (interactive)
  (if (not (bound-and-true-p clj-browse/minor-mode))
      (user-error "clj-browse/refresh can only be used from a browse buffer")
    (let* ((browse-path (clj-browse/browse-path-values clj-browse/browse-path))
           (val (clj-browse/browse-get-in clj-browse/value browse-path))
           (val-str (clj-print/print-str val)))
      (clj-browse/pprint val-str))))

(defun clj-browse/browse-done (candidate)
  (if (equal clj-browse/no-candidate candidate)
      (progn
        (setq clj-browse/browse-path clj-browse/temporary-browse-path)
        (clj-browse/refresh)
        (goto-char (point-min)))
    (let* ((browse-index (clj-data/get clj-browse/candidate->index candidate))
           (candidate (propertize candidate 'clj-browse/browse-index browse-index)))
      (setq clj-browse/browse-path
            (cons candidate clj-browse/temporary-browse-path))
      (clj-browse/refresh)
      (goto-char (point-min)))))

(defun clj-browse/browse-backward-delete-char ()
  (interactive)
  (if (equal "" ivy-text)
      (let ((new-browse-path (cdr clj-browse/temporary-browse-path)))
        (ivy-quit-and-run
          (let ((clj-browse/temporary-browse-path new-browse-path))
            (clj-browse/browse* 'clj-browse/browse-candidates*
                                'clj-browse/browse-done
                                clj-browse/browse-map))))
    (ivy-backward-delete-char)))

(defun clj-browse/browse-alt-done ()
  (interactive)
  (when-let (candidate (nth ivy--index ivy--all-candidates))
    (when (not (equal clj-browse/no-candidate candidate))
      (let* ((browse-index (clj-data/get clj-browse/candidate->index candidate))
             (candidate (propertize candidate 'clj-browse/browse-index browse-index))
             (new-browse-path (cons candidate clj-browse/temporary-browse-path)))
        (ivy-quit-and-run
          (let ((clj-browse/temporary-browse-path new-browse-path))
            (clj-browse/browse* 'clj-browse/browse-candidates*
                                'clj-browse/browse-done
                                clj-browse/browse-map)))))))

(defvar clj-browse/browse-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") 'clj-browse/browse-backward-delete-char)
    (define-key map (kbd "C-j") 'clj-browse/browse-alt-done)
    map))

(defun clj-browse/browse-path->string (browse-path)
  (if (eq '() browse-path)
      ""
    (concat (string-join (reverse browse-path) " ") " ")))

(defun clj-browse/tokenize-prefix (prefix)
  (with-temp-buffer
    (insert prefix)
    (goto-char (point-min))
    (let* ((continue t)
           (tokens '()))
      (while continue
        (let ((c (char-after)))
          (cond ((eobp) (progn (push (buffer-substring-no-properties (point-min) (point)) tokens)
                               (setq continue nil)))
                ((equal c ?-) (let ((token (buffer-substring-no-properties (point-min) (point))))
                                (when (not (string= "" token))
                                  (push token tokens))
                                (delete-region (point-min) (1+ (point)))))
                ((equal c ?.) (let ((token (buffer-substring-no-properties (point-min) (point))))
                                (when (not (string= "" token))
                                  (push token tokens))
                                (delete-region (point-min) (1+ (point)))))
                ((string= (get-char-code-property c 'general-category) "Lu")
                 (let ((token (buffer-substring-no-properties (point-min) (point))))
                   (when (not (string= "" token))
                     (push token tokens))
                   (delete-region (point-min) (point))
                   (forward-char)))
                (t (forward-char)))))
      (nreverse tokens))))

(comment

 (get-char-code-property ?L 'general-category)

 (upcase-initials-region)
 
 (print (clj-browse/tokenize-prefix "aa--bb.cckDdnnff"))

 (print (clj-browse/tokenize-prefix "aa---bb.hhCFd"))
 
 )

(defun clj-browse/char-upcase? (c)
  (and (= ?w (char-syntax c))
       (= c (upcase c))))

(defun clj-browse/last-index-of (str regex &optional ignore-case)
  (let ((start 0)
        (case-fold-search ignore-case)
        idx)
    (while (string-match regex str start)
      (setq idx (match-beginning 0))
      (setq start (match-end 0)))
    idx))

(defun clj-browse/browse-candidates-matches? (candidate prefix-tokens)
  (let* ((candidate (clj-print/print-str candidate))
         (match-index nil)
         (continue t))
    (while (and prefix-tokens continue)
      (let* ((token (car prefix-tokens))
             (maybe-match-index (if (and
                                     (> (length token) 0)
                                     (clj-browse/char-upcase? (aref token 0)))
                                    (clj-browse/last-index-of candidate token)
                                  (let* ((maybe-match-index (clj-browse/last-index-of candidate (concat "." token)))
                                         (maybe-match-index (if (null maybe-match-index)
                                                                (clj-browse/last-index-of candidate (concat "-" token))
                                                              maybe-match-index)))
                                    (if maybe-match-index
                                        (+ maybe-match-index 1)
                                      (when (equal (clj-browse/last-index-of candidate token) 0)
                                        0))))))
        (if maybe-match-index
          (let* ((maybe-match-index (+ (length token) maybe-match-index)))
            (when (or (null match-index) (> maybe-match-index match-index))
              (setq match-index maybe-match-index))
            (setq prefix-tokens (cdr prefix-tokens)))
          (setq continue nil))))
    match-index))

(comment
 
 (clj-browse/last-index-of "eee" " d")
 
 (char-syntax "")
 (= ?w (char-syntax (aref " h" 0)))
 (upcase (aref " h" 0))

 (clj-browse/browse-candidates-matches? "fEe" '("Ee"))
 (clj-browse/last-index-of "ff" "ff")

 (cdr '("rr"))

 (clj-browse/last-index-of ".rrr" (concat "." "rrr"))

 (clj-print/print-str :ee)
 
 )

(defun clj-browse/browse-candidates-filter (prefix-tokens candidates)
  (let* ((filtered-candidates '()))
    (while candidates
      (let* ((candidate (car candidates)))
        (when (clj-browse/browse-candidates-matches? candidate prefix-tokens)
          (setq filtered-candidates (cons candidate filtered-candidates)))
        (setq candidates (cdr candidates))))
    (nreverse filtered-candidates)))

(defun clj-browse/browse-candidates-filter-index (prefix-tokens candidates)
  (let* ((index 0)
         (filtered-candidates '()))
    (while candidates
      (let* ((candidate (car candidates)))
        (when (clj-browse/browse-candidates-matches? candidate prefix-tokens)
          (setq filtered-candidates (cons index filtered-candidates)))
        (setq index (+ index 1))
        (setq candidates (cdr candidates))))
    (nreverse filtered-candidates)))

(defun candidates-with-browse-value (candidate)
  (propertize (clj-print/print-str candidate) 'clj-browse/browse-value candidate))

(defun clj-browse/browse-candidates* (prefix)
  (print clj-browse/temporary-browse-path)
  (let* ((browse-path (clj-browse/browse-path-values clj-browse/temporary-browse-path))
         (value (clj-browse/browse-get-in clj-browse/value browse-path))
         (prefix-tokens (clj-browse/tokenize-prefix prefix))
         (candidates (cond ((hash-table-p value)
                            (clj-browse/browse-candidates-filter prefix-tokens (clj-data/keys value)))
                           ((and (arrayp value) (null (stringp value)))
                            (clj-browse/browse-candidates-filter-index prefix-tokens (mapcar 'identity value)))
                           ((listp value) (clj-browse/browse-candidates-filter-index prefix-tokens value))
                           (t nil)))
         (candidates (mapcar 'candidates-with-browse-value candidates))
         (candidates (if (or (null prefix) (equal "" prefix))
                         (cons "" candidates)
                       candidates)))
    candidates))

(comment
 (clj-print/print-str (clj-data/hash-map :eee 333))

 (clj-data/keys (clj-data/hash-map :eee 333))

 (let ((clj-browse/value ["rr"]))
   (clj-browse/browse-candidates* ""))

 (let ((clj-browse/value (list "rr")))
   (clj-browse/browse-candidates* ""))

 (mapcar 'identity (list "ee" "ff"))
 
 (let ((clj-browse/value (clj-data/hash-map :eee 333)))
   (clj-browse/browse-candidates* ":e"))
 
 (mapcar 'identity [11 22])

 (arrayp ["ee" 3333])

 (stringp "ee")
 
 (listp '(1 2))

 (let* ((candidates (cond ((listp 66) 44))))
   candidates))

(defun clj-browse/browse-candidates (browse-candidates-fn user-input)
  (with-ivy-window
    (funcall browse-candidates-fn user-input)))

(defun clj-browse/read-one ()
  (let ((clj-context/splice-ends '())
        (clj-context/symbol-separators clj-pprint/symbol-separators)
        (clj-context/symbol-separator-re clj-pprint/symbol-separator-re))
    (clj-context/read-one)))

(defun clj-browse/compute-browse-positions-sequential (index->pos seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while continue
      (clj-context/forward-comment)
      (let ((p-start (point))
            (object (clj-browse/read-one)))
        (if (null object)
            (setq continue nil)
          (puthash index p-start index->pos)
          (setq index (+ 1 index)))))
    index->pos))

(defun clj-browse/compute-browse-positions-map (index->pos map)
   (let ((continue t)
         (index 0))
     (goto-char (+ 1 (oref map :start)))
     (while continue
       (clj-context/forward-comment)
       (let ((p-start (point))
             (object (clj-browse/read-one)))
         (if (null object)
             (setq continue nil)
           (when (equal 0 (logand index 1))
             (puthash index p-start index->pos))
           (setq index (+ 1 index)))))
     index->pos))

(defun clj-browse/compute-browse-positions-dispatch-macro (index->pos dm)
   (let ((dm-type (oref dm :dispatch-macro))
         (dm-value (clj-context/meta-value (oref dm :value))))
     (cond ((and (eq :set dm-type)
                 (cl-typep dm-value 'clj-context/object-delimited))
            (clj-browse/compute-browse-positions-sequential index->pos dm-value))
           ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                 (cl-typep dm-value 'clj-context/object-delimited))
            (if (equal :map (oref dm-value :delimited))
                (clj-browse/compute-browse-positions-map index->pos dm-value)
              (clj-browse/compute-browse-positions-sequential index->pos dm-value))))
     index->pos))

(defun clj-browse/compute-browse-positions-dispatch (index->pos)
  (let* ((object (clj-browse/read-one))
         (object-meta-value (clj-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'clj-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (clj-browse/compute-browse-positions-map index->pos object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-delimited)
             (clj-browse/compute-browse-positions-sequential index->pos object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-dispatch-macro)
             (clj-browse/compute-browse-positions-dispatch-macro
              index->pos object-meta-value)))))
  index->pos)

(defun clj-browse/goto-indexes-path-position-sequential (target-index seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while (and (< index target-index) continue)
      (clj-context/forward-comment)
      (let ((object (clj-browse/read-one)))
        (if (null object)
            (setq continue nil)
          (setq index (+ 1 index)))))
    (>= index target-index)))

(defun clj-browse/goto-indexes-path-position-map (target-index seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while (and (<= index target-index) continue)
      (clj-context/forward-comment)
      (let ((object (clj-browse/read-one)))
        (if (null object)
            (setq continue nil)
          (setq index (+ 1 index)))))
    (> index target-index)))

(defun clj-browse/goto-indexes-path-position-dispatch-macro (target-index dm)
  (let ((dm-type (oref dm :dispatch-macro))
        (dm-value (clj-context/meta-value (oref dm :value))))
    (cond ((and (eq :set dm-type)
                (cl-typep dm-value 'clj-context/object-delimited))
           (clj-browse/goto-indexes-path-position-sequential target-index dm-value))
          ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                (cl-typep dm-value 'clj-context/object-delimited))
           (if (equal :map (oref dm-value :delimited))
               (clj-browse/goto-indexes-path-position-map target-index dm-value)
             (clj-browse/goto-indexes-path-position-sequential target-index dm-value))))))

(defun clj-browse/goto-indexes-path-position-dispatch (index)
  (let* ((object (clj-browse/read-one))
         (object-meta-value (clj-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'clj-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (clj-browse/goto-indexes-path-position-map index object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-delimited)
             (clj-browse/goto-indexes-path-position-sequential index object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-dispatch-macro)
             (clj-browse/goto-indexes-path-position-dispatch-macro
              index object-meta-value))))))

(defun clj-browse/compute-browse-positions (indexes-path)
  (let ((index->pos (clj-data/hash-map))
        (at-position? t))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (and at-position? (consp indexes-path))
          (if (null (car indexes-path))
              (setq at-position? nil)
            (clj-context/forward-comment)
            (setq at-position?
                  (clj-browse/goto-indexes-path-position-dispatch (car indexes-path)))
            (setq indexes-path (cdr indexes-path))))
        (when at-position?
          (clj-context/forward-comment)
          (puthash -1 (point) index->pos)
          (clj-browse/compute-browse-positions-dispatch index->pos))))
    index->pos))

(defun clj-browse/compute-browse-indexes-sequential (candidate->index seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while continue
      (clj-context/forward-comment)
      (let* ((object (clj-browse/read-one)))
        (if (null object)
            (setq continue nil)
          (puthash (number-to-string index) index candidate->index)
          (setq index (+ 1 index)))))
    candidate->index))

(defun clj-browse/compute-browse-indexes-map (candidate->index map)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref map :start)))
    (while continue
      (clj-context/forward-comment)
      (let* ((object-start (point))
             (object (clj-browse/read-one)))
        (if (null object)
            (setq continue nil)
          (when (equal 0 (logand index 1))
            (let ((k-str (buffer-substring-no-properties object-start (point))))
              ;; If there are multiple identical keys in a map, then we cannot
              ;; distinguish between them
              (if (clj-data/contains? candidate->index k-str)
                  (remhash k-str candidate->index)
                (puthash k-str index candidate->index))))
          (setq index (+ 1 index)))))
    candidate->index))

(defun clj-browse/compute-browse-indexes-dispatch-macro (candidate->index dm)
  (let ((dm-type (oref dm :dispatch-macro))
        (dm-value (clj-context/meta-value (oref dm :value))))
    (cond ((and (eq :set dm-type)
                (cl-typep dm-value 'clj-context/object-delimited))
           (clj-browse/compute-browse-indexes-sequential candidate->index dm-value))
          ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                (cl-typep dm-value 'clj-context/object-delimited))
           (if (equal :map (oref dm-value :delimited))
               (clj-browse/compute-browse-indexes-map candidate->index dm-value)
             (clj-browse/compute-browse-indexes-sequential candidate->index dm-value))))
    candidate->index))

(defun clj-browse/compute-browse-indexes-dispatch (candidate->index)
  (let* ((object (clj-browse/read-one))
         (object-meta-value (clj-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'clj-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (clj-browse/compute-browse-indexes-map candidate->index object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-delimited)
             (clj-browse/compute-browse-indexes-sequential candidate->index
                                                           object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-dispatch-macro)
             (clj-browse/compute-browse-indexes-dispatch-macro
              candidate->index object-meta-value)))))
  candidate->index)

(defun clj-browse/compute-browse-indexes (indexes-path)
  (let ((candidate->index (clj-data/hash-map))
        (at-position? t))
    (goto-char (point-min))
    (while (and at-position? (consp indexes-path))
      (if (null (car indexes-path))
          (setq at-position? nil)
        (clj-context/forward-comment)
        (setq at-position?
              (clj-browse/goto-indexes-path-position-dispatch (car indexes-path)))
        (setq indexes-path (cdr indexes-path))))
    (when at-position?
      (clj-context/forward-comment)
      (clj-browse/compute-browse-indexes-dispatch candidate->index))
    candidate->index))

(defun clj-browse/compute-init-candidate-sequential (target-point seq)
  (let ((continue t)
        (index 0))
    (goto-char (+ 1 (oref seq :start)))
    (while continue
      (clj-context/forward-comment)
      (let* ((object (clj-browse/read-one)))
        (cond ((null object)
               (setq index (max 0 (- index 1)))
               (setq continue nil))
              ((>= (point) target-point)
               (setq continue nil))
              (t (setq index (+ 1 index))))))
    (number-to-string index)))

(defun clj-browse/compute-init-candidate-map (target-point map)
  (let ((continue t)
        (candidate nil)
        (index 0))
    (goto-char (+ 1 (oref map :start)))
    (while continue
      (clj-context/forward-comment)
      (let* ((object-start (point))
             (object (clj-browse/read-one)))
        (cond ((null object)
               (setq continue nil))
              ((>= (point) target-point)
               (when (equal 0 (logand index 1))
                 (setq candidate (buffer-substring-no-properties object-start (point))))
               (setq continue nil))
              (t (when (equal 0 (logand index 1))
                   (setq candidate (buffer-substring-no-properties object-start (point))))))
        (setq index (+ 1 index))))
    candidate))

(defun clj-browse/compute-init-candidate-dispatch-macro (target-point dm)
  (let ((dm-type (oref dm :dispatch-macro))
        (dm-value (clj-context/meta-value (oref dm :value))))
    (cond ((and (eq :set dm-type)
                (cl-typep dm-value 'clj-context/object-delimited))
           (clj-browse/compute-init-candidate-map target-point dm-value))
          ((and (or (eq :tagged-literal dm-type) (eq :namespaced-map dm-type))
                (cl-typep dm-value 'clj-context/object-delimited))
           (if (equal :map (oref dm-value :delimited))
               (clj-browse/compute-init-candidate-map target-point dm-value)
             (clj-browse/compute-init-candidate-sequential target-point dm-value))))))

(defun clj-browse/compute-init-candidate-dispatch (target-point)
  (let* ((object (clj-browse/read-one))
         (object-meta-value (clj-context/meta-value object)))
    (when object-meta-value
      (cond ((and (cl-typep object-meta-value 'clj-context/object-delimited)
                  (equal :map (oref object-meta-value :delimited)))
             (clj-browse/compute-init-candidate-map target-point object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-delimited)
             (clj-browse/compute-init-candidate-sequential target-point object-meta-value))
            ((cl-typep object-meta-value 'clj-context/object-dispatch-macro)
             (clj-browse/compute-init-candidate-dispatch-macro
              target-point object-meta-value))))))

(defun clj-browse/compute-init-candidate ()
  (save-excursion
    (save-restriction
      (widen)
      (let ((target-point (point)))
        (goto-char (point-min))
        (clj-browse/compute-init-candidate-dispatch target-point)))))

(defun clj-browse/compute-path-indexes ()
  (let ((browse-path (reverse clj-browse/browse-path))
        (temporary-browse-path (reverse clj-browse/temporary-browse-path)))
    (while (and
            (car browse-path)
            (car temporary-browse-path)
            (equal (car browse-path) (car temporary-browse-path)))
      (setq browse-path (cdr browse-path))
      (setq temporary-browse-path (cdr temporary-browse-path)))
    (if (null (car browse-path))
        (mapcar (apply-partially 'get-text-property 0 'clj-browse/browse-index)
                temporary-browse-path)
      :negative-path)))

(defun clj-browse/browse* (browse-candidates-fn browse-done browse-map &optional init-candidate)
  (let* ((printed-raw (get-text-property (point-min) 'clj-browse/raw-printed))
         (path-indexes (clj-browse/compute-path-indexes))
         (clj-browse/candidate->index (when (not (equal :negative-path path-indexes))
                                        (with-temp-buffer
                                          (insert printed-raw)
                                          (clj-browse/compute-browse-indexes
                                           path-indexes))))
         (clj-browse/index->pos (when (not (equal :negative-path path-indexes))
                                  (clj-browse/compute-browse-positions path-indexes)))
         ;; for whatever reason :preset with :dynamic-collection does not work when :preselect
         ;; is a string, but its works if it is a number
         ;; Thus we start be prefectching the candidates to find the index of the init-candidate
         (initial-candidates (funcall browse-candidates-fn ""))
         (preselect (when init-candidate (seq-position initial-candidates init-candidate))))
    (ivy-read
     (concat "Browse path: "
             (clj-browse/browse-path->string
              clj-browse/temporary-browse-path))
     (apply-partially 'clj-browse/browse-candidates browse-candidates-fn)
     :dynamic-collection t
     :action browse-done
     :preselect preselect
     :update-fn (lambda ()
                  (with-ivy-window
                    (clj-highlight/unhighlight)
                    (when-let ((candidate (nth ivy--index ivy--all-candidates)))
                      (if (equal clj-browse/no-candidate candidate)
                          (when-let (pos (clj-data/get clj-browse/index->pos -1))
                            (goto-char pos)
                            (clj-highlight/highlight-no-line
                             pos (min (point-max) (+ 1 pos))))
                        (when-let (index (clj-data/get clj-browse/candidate->index
                                                       candidate))
                          (when-let (pos (clj-data/get clj-browse/index->pos index))
                            (goto-char pos)
                            (clj-highlight/highlight-no-line
                             pos (min (point-max) (+ 1 pos)))))))))
     :require-match t
     :keymap browse-map
     :caller 'clj-browse/browse
     :unwind (lambda () (clj-highlight/unhighlight)))))

(defun clj-browse/browse ()
  (interactive)
  (if (not (bound-and-true-p clj-browse/minor-mode))
      (user-error "clj-browse/browse can only be used from a clj-browse buffer")
    (let* ((clj-browse/temporary-browse-path clj-browse/browse-path)
           (init-candidate (clj-browse/compute-init-candidate)))
      (clj-browse/browse* 'clj-browse/browse-candidates*
                          'clj-browse/browse-done
                          clj-browse/browse-map
                          init-candidate))))

(comment
 (let* ((test-val (vector (clj-data/hash-map :id "blank-channel"
                                             :title "Blank Channel"
                                             :featureImage "/img/channel-template_blank-channel.png"
                                             :description "<p>
Idea collection and collaboration happen in channels. A channel is a place for your employees to share ideas, collaborate and give feedback. When you create a channel you choose a topic, define a workflow, decide who can participate and who should evaluate. <a href=\"https://help.nos.co/hc/en-us/articles/360019128211\">Learn more</a>
</p>
<hr/>
<p>Template details:</p>
<ul>
<li><strong>Topic</strong> - give your channel a title and description</li>
<li><strong>Process</strong> - set up a stages to create your workflow</li>
<li><strong>Submission form</strong> - fully customizable submission form</li>
<li><strong>Evaluation</strong> - fully customizable evaluation criteria</li>
<li><strong>Audience</strong> - decide who can access and participate</li>
</ul>
")
                          (clj-data/hash-map :id "innovation-challenge"
                                             :template/url "https://challenge-template.nos.co/ideaboxes/5cda7a153e188e3408d36aa2"
                                             :title "Innovation Challenge"
                                             :featureImage "/img/channel-template_innovation-challenge.png"
                                             :description "<p>
Rally your entire organisation around a key strategic challenge, to stimulate game-changing innovation opportunities and build your innovation culture.
</p>
<hr/>
<p>Template details:</p>
<ul>
<li><strong>6 stages</strong> - idea sharing, evaluation, team assembly, selection, maturation & final</li>
<li><strong>Submission form</strong> - Nosco’s best practice for collecting & maturing ideas and assembling idea teams</li>
<li><strong>Evaluation parameters</strong> - Nosco’s best practice for pre-screening and selecting ideas</li>
<li><strong>Call for ideas</strong> - Nosco’s template for an engaging call for ideas</li>
<li><strong>Fully editable</strong> - add your own description, images, forms or stages</li>
</ul>
"))))
   (clj-browse/do-browse (get-buffer "test.result") test-val))
 
 )

(defvar clj-browse/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'clj-browse/refresh)
    (define-key map "b" 'clj-browse/browse)
    (easy-menu-define clj-browse/minor-mode-menu map
      "clj-browse Minor Mode Menu"
      '("clj-browse"
        ["Refresh browsed" clj-browse/refresh t]
        ["Browse" clj-browse/browse t]))
    map))

(define-minor-mode clj-browse/minor-mode
  "Minor mode for interacting with a clj-browse buffer."
  :lighter "clj-browse" :keymap clj-browse/minor-mode-map)

(provide 'clj-browse)


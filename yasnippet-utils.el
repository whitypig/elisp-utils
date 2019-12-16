;;; yasnippet-utils.el

(defun my-yas-expand-from-trigger-key (arg)
  (interactive "p")
  (pcase arg
    (1 (call-interactively #'yas-expand-from-trigger-key))
    ((and (pred integerp) (guard (> 0)))
     (my-yas-expand-snippet (my-yas-make-snippet (my-yas-get-snippet) arg)))
    (_ nil)))

(defun my-yas-expand-snippet (snippet-and-pos)
  (pcase snippet-and-pos
    (`(,snippet-text ,start ,end)
     ;; (message "DEBUG: my-yas-expand-snippet(), snippet-text=%s, start=%d, end=%d" snippet-text start end)
     (yas-expand-snippet snippet-text start end))
    (_ nil)))

;; <input type="radio" id="${1:id}" name="${2:name}" value="${3:value}" />
;; <label for="$1">${4:desc}</label>

;; <input type="radio" id="${1:id}" name="${2:name}" value="${3:value}" />
;; <label for="$1">${4:desc}</label>


;; <input type="radio" id="${1:id}" name="${2:name}" value="${3:value}" />
;; <label for="$1">${4:desc}</label>

;; <input type="radio" id="${5:id}" name="${6:name}" value="${7:value}" />
;; <label for="$5">${8:desc}</label>

(defun my-yas-make-snippet (snippet count)
  (pcase snippet
    (`(,snippet-text ,start ,end)
     (list (my-yas-format-snippet snippet-text count)
           start
           end))
    (_ nil)))

;; If snippet text contains something like "\$3", we are doomed.
(defconst my-yas--placeholder-regexp "${\\([0-9]+\\)[^}]+}\\|$\\([0-9]+\\)")

;; <input type="radio" id="${1:id}" name="${2:name}" value="${3:value}" />
;; <label for="$1">${4:desc}</label>

;; <input type="radio" id="${1:id}" name="${2:name}" value="${3:value}" />
;; <label for="$1">${4:desc}</label>
(defun my-yas-format-snippet (snippet-text count)
  (message "DEBUG: [my-yas-format-snippet] snippet-text=%s" snippet-text)
  (cl-loop with lst = nil
           ;; 100 should be enough for the total number of placeholder
           ;; fileds.
           with total-fields-num = 100
           repeat count
           ;; ix becomes a new placeholder number.
           with ix = 0
           collect
           (with-temp-buffer
             (cl-loop with cnt = 0
                      with occ = (make-vector total-fields-num nil)
                      initially
                      (progn (insert
                              ;; insert base snippet.
                              (if (zerop ix) snippet-text
                                  ;; Delete $0 from snippet other than
                                  ;; the first one.
                                (replace-regexp-in-string "$0" "" snippet-text)))
                             (goto-char (point-min)))
                      while (re-search-forward my-yas--placeholder-regexp nil t)
                      do
                      (pcase (string-to-number (or (match-string 1)
                                                   (match-string 2)))
                        (0
                         ;; For placeholder $0, do nothing.
                         nil)
                        ((and (pred (aref occ)) i)
                         ;; This is backreference.  Just replace the
                         ;; placeholder number with the new index
                         ;; stored in occ.
                         (replace-match (number-to-string (aref occ i))
                                        nil nil nil 1))
                        (i
                         (progn
                           ;; placeholder ${i} becomes placeholder
                           ;; ${ix}.  Default value should be mirrored
                           ;; from the one in the very first snippet,
                           ;; i.e. ${i} and $i become ${ix:$i}.
                           (aset occ i (incf ix))
                           (when (not (= i ix))
                             (replace-match (format "${%d:$%d}" ix i)))
                           ;; (replace-match (number-to-string ix) nil nil nil 1)
                           )))
                      finally return (buffer-string)))
           into lst
           finally return (mapconcat #'identity lst "\n")))

(defun my-yas-format-snippet-1 (snippet-text index)
  (with-temp-buffer
    (insert snippet-text)
    (goto-char (point-min))
    (cl-loop with cnt = 0
             with occ = (make-vector 100 nil)
             while (re-search-forward my-yas--placeholder-regexp nil t)
             do (pcase (string-to-number (match-string 1))
                  ((pred (aref occ)) nil)
                  (ix (and (aset occ ix t) (incf cnt))))
             finally return cnt)))

(defun my-yas-get-snippet ()
  (pcase (yas--templates-for-key-at-point)
    (`(,templates ,start ,end)
     (list (yas--template-content
            (or (and (cl-rest templates) ;; more than one
                     (yas--prompt-for-template (mapcar #'cdr templates)))
                (cdar templates)))
           start
           end))
    (_ nil)))

(provide 'yasnippet-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; yasnippet-utils.el ends here

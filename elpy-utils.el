;;; elpy-utils.el

(defun my-elpy-company--expand-snippet (annotation name)
  "Post completion function for `elpy-company-backend'.  If candidate
is a function, then try to make and expand yasnippet."
  (when (and (fboundp 'yas-expand-snippet) annotation name)
    (let ((doc (elpy-rpc-get-completion-docstring name))
          (snippet nil))
      (cond
       ((member annotation '("function" "class"))
        (setq snippet (my-elpy-company--make-yasnippet-template
                       name
                       (my-elpy-company--get-signature name doc)))
        (and snippet (yas-expand-snippet snippet)))
       (t
        nil)))))

(defun my-elpy-company--get-signature (name doc)
  "Try to extract the simplest form of signatures in DOC. There are
  two cases to consider.
1. DOC contains no description.
2. DOC contains description.

For case 1, the last line in DOC is highly likely to be the line we
want.
For case 2, we assume that the last line of signatures that leads to
description in DOC is what we need.

Take \"map\" as an example. This is case 2.
Its DOC is as follows:
map(func: Callable[..., _S], iter1: Iterable[Any], iter2: Iterable[Any], iter3: Iterable[Any], iter4: Iterable[Any], iter5: Iterable[Any], iter6: Iterable[Any], /, *iterables: Iterable[Any]) -> List[_S]
...
...
...
map(func: None, iter1: Iterable[_T1], /) -> List[_T1]

map(function, sequence[, sequence, ...]) -> list   # <--- THIS LINE IS WHAT WE NEED.

Return a list of the results of applying the function to the items of
the argument sequence(s).  If more than one sequence is given, the
function is called with an argument list consisting of the corresponding
item of each sequence, substituting None for missing values when not all
sequences have the same length.  If the function is None, return a list of
the items of the sequence (or a list of tuples if more than one sequence)."
  (let ((lines (split-string doc "\n" t " "))
        (reg (format "^%s(.*)" name)))
    ;; (message "DEBUG: doc=%s" doc)
    (cond
     ((cl-every (lambda (line) (string-match-p reg line)) lines)
      ;; case 1
      (car (last lines)))
     (t
      ;; case 2
      (cl-loop for next in (cdr lines)
               for cur in lines
               when (not (string-match-p reg next))
               return cur)))))

(defun my-elpy-company--make-yasnippet-template (name signature)
  ;; For now, we remove "self" in a parameter like the one in
  ;; "split(self)" because I cannot imagine the case where that self
  ;; is needed.
  (message "DEBUG: signature=%s" signature)
  (when (stringp name)
    (let* ((signature (and signature
                           (replace-regexp-in-string "(self)" "()" signature)))
           (params
            (and signature
                 (string-match (format "%s(\\([^)]*\\))" name) signature)
                 (cl-remove-if
                  (lambda (elt) (member elt '("/" "*")))
                  (elpy-company--split-string-by-toplevel-comma
                   (replace-regexp-in-string " /$"
                                             ""
                                             (match-string 1 signature))))))
           (ix 1))
      (message "DEBUG: params=%s" params)
      (concat "${1:("
              (if params
                  (cl-reduce
                   (lambda (x y)
                     (concat
                      x
                      (cond
                       ((string-match-p "=" y)
                        ;; default argument
                        (concat (if (= ix 1)
                                    (format "${%d:%s}" (incf ix) y)
                                  (format "${%d:, ${%d:%s}}" (incf ix) (incf ix) y))))
                       ((string-match-p "[*]\\{1,2\\}" y)
                        ;; variable length or dictionary
                        (format "${%d:, ${%d:%s}}" (incf ix) (incf ix) y))
                       ((string= y "...")
                        ;; variable length arguemnts
                        (format "${%d:, ${%d:%s}}" (incf ix) (incf ix) y))
                       (t
                        ;; normal argument
                        (format "%s${%d:%s}"
                                (if (= 1 ix) "" ", ")
                                (incf ix)
                                y)))))
                   params
                   :initial-value "")
                "")
              ")}$0"))))

;; (defun my-elpy-company--make-yasnippet-template (name signature)
;;   ;; For now, we remove "self" in a parameter like the one in
;;   ;; "split(self)" because I cannot imagine the case where that self
;;   ;; is needed.
;;   (message "DEBUG: signature=%s" signature)
;;   (when (and name signature)
;;     (let* ((signature (replace-regexp-in-string "(self)" "()" signature))
;;            (params
;;             (and (string-match (format "%s(\\([^)]*\\))" name) signature)
;;                  (cl-remove-if
;;                   (lambda (elt) (member elt '("/" "*")))
;;                   (elpy-company--split-string-by-toplevel-comma
;;                    (replace-regexp-in-string " /$"
;;                                              ""
;;                                              (match-string 1 signature))))))
;;            (ix 1))
;;       (message "DEBUG: params=%s" params)
;;       (concat "${1:("
;;               (if params
;;                   (cl-reduce
;;                    (lambda (x y)
;;                      (concat
;;                       x
;;                       (cond
;;                        ((string-match-p "=" y)
;;                         ;; default argument
;;                         (concat (if (= ix 1)
;;                                     (format "${%d:%s}" (incf ix) y)
;;                                   (format "${%d:, ${%d:%s}}" (incf ix) (incf ix) y))))
;;                        ((string-match-p "\\*\\*" y)
;;                         ;; dictionary
;;                         (format "${%d:, ${%d:%s}}" (incf ix) (incf ix) y))
;;                        ((string= y "...")
;;                         ;; variable length arguemnts
;;                         (format "${%d:, ${%d:%s}}" (incf ix) (incf ix) y))
;;                        (t
;;                         ;; normal argument
;;                         (format "%s${%d:%s}"
;;                                 (if (= 1 ix) "" ", ")
;;                                 (incf ix)
;;                                 y)))))
;;                    params
;;                    :initial-value "")
;;                 "${2:}")
;;               ")}$0"))))

(defun my-elpy-company--split-string-by-toplevel-comma (s)
  "Split string S by toplevel SEPARATORS.

Examples:
\"function: Callable[[_T], Any], iterable: Iterable[_T], /\"
=> (\"function: Callable[[_T], Any]\" \"iterable: Iterable[_T]\" \"/\")"
  (cl-loop for ch in (split-string s "" t)
           with depth = 0
           with acc = nil
           with ret = nil
           with separator = "[,]"
           do (cond
               ((string-match-p separator ch)
                (cond
                 ((zerop depth)
                  (when acc (push (cl-reduce #'concat (nreverse acc)) ret))
                  (setq acc nil))
                 ((> depth 0)
                  (push ch acc))))
               ((member ch '("(" "["))
                (incf depth)
                (push ch acc))
               ((member ch '(")" "]"))
                (decf depth)
                (push ch acc))
               (t
                (push ch acc)))
           finally (progn
                     (when acc
                       (push (cl-reduce #'concat (nreverse acc)) ret))
                     (return (mapcar #'s-trim (nreverse ret))))))

(defun my-elpy-eval-defun-or-send-region (&optional arg)
  (interactive "P")
  (cond
   ((use-region-p)
    (elpy-shell-send-region-or-buffer-and-step)
    (deactivate-mark))
   (t
    (elpy-shell-send-defun))))

(provide 'elpy-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; elpy-utils.el ends here

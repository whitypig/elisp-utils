;;; python-mode-utils.el

(defun py-utils-up-list (arg)
  (interactive "p")
  (let ((depth (car (syntax-ppss))))
    (condition-case err
        ;; First, try to get out of inside of parentheses.
        (backward-up-list arg)
      ;; (car (syntax-ppss)) represents the depth of parentheses.  If
      ;; we have got out of some parens by backward-up-list, then
      ;; subtract that depth from ARG.
      (error (py-utils-up-block (max 0 (- arg depth)))))))

(defun py-utils-up-block (arg)
  (cl-loop repeat arg
           do (py-utils-up-block-1)))

(defun py-utils-up-block-1 ()
  (let* ((cur-col (save-excursion
                    (back-to-indentation)
                    (current-column)))
         (target-col (- cur-col py-indent-offset))
         (reg (and
               (>= target-col 0)
               (format "^%s[^ ]" (make-string target-col ? )))))
    (cond
     ((< target-col 0)
      (user-error "We are on the apex"))
     (reg
      (push-mark)
      ;; This searching must find the target.
      (re-search-backward reg nil t)
      (back-to-indentation))
     (t
      nil))))

(define-key python-mode-map (kbd "C-M-u") #'py-utils-up-list)

(provide 'python-mode-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; python-mode-utils.el ends here

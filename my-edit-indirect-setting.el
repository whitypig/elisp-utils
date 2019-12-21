;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; edit-indirect.el
;; https://github.com/Fanael/edit-indirect
;; https://github.com/whitypig/edit-indirect
(when (require 'edit-indirect nil t)
  (define-key global-map (kbd "C-x C-@ r") #'edit-indirect-region)
  (defun my-edit-indirect-guess-mode (parent beg _end)
    (pcase (save-excursion (set-buffer parent)
                           ;; We cannot rely on `buffer-local-value'
                           ;; for major-mode.  This is because with
                           ;; mmm-mode on in parent buffer,
                           ;; major-mode does not necessarily matches
                           ;; with the major-mode in the region.
                           (goto-char beg)
                           major-mode)
      ((and (pred fboundp) mode) (funcall mode))
      (_ (normal-mode))))
  (add-hook 'edit-indirect-after-commit-functions #'indent-region)
  (defun my-edit-indirect-after-creation-hook ()
    (indent-region (point-min) (point-max)))
  (add-hook 'edit-indirect-after-creation-hook #'my-edit-indirect-after-creation-hook))

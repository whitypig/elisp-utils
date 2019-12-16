;;; my-helm-utils.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For helm-color.el
(require 'helm-color)

(defun helm-color-convert-rgb-to-hsl (candidate)
  (cl-loop with rgb = (helm-colors-get-rgb candidate)
           repeat 3
           for i from 1 by 2
           ;; #'color-rgb-to-hsl expectes arguments to be between
           ;; [0.0, 1.0]
           collect (string-to-number (cl-subseq rgb i (+ i 2)) 16)
           into lst
           finally return
           (helm :sources (helm-build-sync-source "hsl or hsla"
                            :candidates (helm-color-hsl-make-candidates
                                         (mapcar (lambda (elt) (/ elt 255.0)) lst)
                                         lst))
                 :buffer "*helm-color-hsl*")))

(defun helm-color-hsl-make-candidates (values rgb-values)
  (cl-loop with hsl-values = nil
           for elt in (apply #'color-rgb-to-hsl values) and ix from 0
           collect (if (zerop ix)
                       ;; Hue
                       (format "%.2f" (* 360 elt))
                     ;; Saturation and lightness
                     (format "%d%%" (truncate (* 100 elt))))
           into hsl-values
           finally return
           (let ((s1 (mapconcat #'identity hsl-values ", "))
                 (s2 (mapconcat #'number-to-string rgb-values ", ")))
             (list (format "hsl(%s)" s1)
                   (format "hsla(%s, 0.5)" s1)
                   ;; Collect rgb() and rgba() as well.
                   (format "rgb(%s)" s2)
                   (format "rgba(%s, 0.5)" s2)))))

(defun helm-color-insert-hsl (candidate)
  (with-helm-current-buffer
    (insert (helm-color-convert-rgb-to-hsl candidate))))

(defun helm-color-kill-hsl (candidate)
  (kill-new (helm-color-convert-rgb-to-hsl candidate)))

(defun helm-color-run-insert-hsl ()
  "Insert HSL of color from `helm-source-colors'"
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-color-insert-hsl)))

(put 'helm-color-run-insert-hsl 'helm-only t)

(defun helm-color-run-kill-hsl ()
  "Kill HSL of color from `helm-source-colors'"
  (interactive)
  (with-helm-alive-p (helm-exit-and-execute-action 'helm-color-kill-hsl)))
(put 'helm-color-run-kill-hsl 'helm-only t)

(define-key helm-color-map (kbd "C-c h") 'helm-color-run-insert-hsl)
(define-key helm-color-map (kbd "C-c H") 'helm-color-run-kill-hsl)

(provide 'my-helm-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; my-helm-utils.el ends here

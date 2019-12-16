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
           collect (/ (string-to-number (cl-subseq rgb i (+ i 2))
                                        16)
                      255.0)
           into lst
           finally return
           (helm :sources (helm-build-sync-source "hsl or hsla"
                            :candidates (helm-color-hsl-make-candidates lst))
                 :buffer "*helm-color-hsl*")))

(defun helm-color-hsl-make-candidates (hsl-values)
  (cl-loop for elt in (apply #'color-rgb-to-hsl hsl-values) and ix from 0
           collect (if (zerop ix)
                       ;; Hue
                       (format "%.2f" (* 360 elt))
                     ;; Saturation and lightness
                     (format "%d%%" (truncate (* 100 elt))))
           into values
           finally return
           (let ((s (mapconcat #'identity values ", ")))
             (list (format "hsl(%s)" s)
                   (format "hsla(%s, 1.0)" s)))))

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

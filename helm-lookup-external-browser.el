;;; helm-lookup-external-browser.el -*- coding: utf-8-unix; -*-

(defcustom helm-lookup-ext-url-template-alist
  '(("Collins" . "https://www.collinsdictionary.com/dictionary/english/%s")
    ("英辞郎 Pro Lite" . "https://eowf.alc.co.jp/search?q=%s"))
  "List of cons cell whose car is name and cdr is URL template.  URL
template must contain one \"%s\", which is replaced with a query
word."
  :type 'list
  :group 'helm-lookup-external-browser)

(defconst helm-lookup-ext-browser-function
  (cond
   ((memq system-type '(windows-nt ms-dos cygwin))
    #'browse-url-default-windows-browser)
   ((memq system-type '(darwin))
    #'browse-url-default-macosx-browser)
   (t
    #'browse-url-default-browser)))


(defvar helm-lookup-ext--last-query-word nil)

(defun helm-lookup-ext-lookup (word)
  "Lookup for WORD in specified Web sites with external browser."
  (interactive (list (read-from-minibuffer
                      "Word: " (thing-at-point 'word))))
  (when (stringp word)
    (setq helm-lookup-ext--last-query-word word)
    (helm-lookup-ext--run-helm)))

(defun helm-lookup-ext--run-helm ()
  (helm :sources (helm-build-sync-source "helm-lookup-ext-sites"
                   :candidates helm-lookup-ext-url-template-alist
                   :action (helm-make-actions
                            "Default action"
                            (lambda (_)
                              (helm-lookup-ext--excute-default-action
                               (helm-marked-candidates)))))))

(defun helm-lookup-ext--excute-default-action (candidates)
  (pcase helm-lookup-ext--last-query-word
    ((and stringp word)
     (cl-loop for url in candidates
              do (funcall helm-lookup-ext-browser-function
                          (format url word)
                          t)))
    (_ (user-error "Word is empty"))))

(provide 'helm-lookup-external-browser)

;; Local Variables:
;; coding: utf-8-unix
;; indent-tabs-mode: nil
;; End:

;;; helm-lookup-external-browser.el ends here

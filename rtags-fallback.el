(defun use-rtags-p (&optional useFileManager)
  (and (rtags-executable-find "rc")
       (cond ((not ggtags-project-root) t)
             ((and (not (eq major-mode 'c++-mode))
                   (not (eq major-mode 'c-mode)))
              (rtags-has-filemanager))
             (useFileManager (rtags-has-filemanager))
             (t (rtags-is-indexed)))))

(defun tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if use-rtags
      (rtags-find-symbol-at-point prefix)
    (call-interactively 'ggtags-find-tag-dwim)))

(defun tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if use-rtags
      (rtags-find-references-at-point prefix)
    (call-interactively 'ggtags-find-reference)))

(defun tags-find-symbol ()
  (interactive)
  (call-interactively (if (use-rtags-p) 'rtags-find-symbol 'ggtags-find-other-symbol)))

(defun tags-find-references ()
  (interactive)
  (call-interactively (if (use-rtags-p) 'rtags-find-references 'ggtags-find-reference)))

(defun tags-find-file ()
  (interactive)
  (call-interactively (if (use-rtags-p t) 'rtags-find-file 'ggtags-find-file)))

;; (defun tags-imenu ()
;;   (interactive)
;;   (call-interactively (if (use-rtags-p t) 'rtags-imenu 'idomenu)))

(defun init-rtags-fallback-map ()
  (interactive)

  (when (boundp 'ggtags-mode-map)
    (define-key ggtags-mode-map (kbd "M-.") nil)
    (define-key ggtags-mode-map (kbd "M-,") nil))
  
  (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
  (define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
  (define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
  (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  ;;(define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

  (define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
  (define-key global-map (kbd "M-,") (function tags-find-references-at-point))
  (define-key global-map (kbd "M-;") (function tags-find-file))
  (define-key global-map (kbd "C-.") (function tags-find-symbol))
  (define-key global-map (kbd "C-,") (function tags-find-references))
  (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
  ;;(define-key global-map (kbd "M-i") (function tags-imenu))
  )

(provide 'rtags-fallback)

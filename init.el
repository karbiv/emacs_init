;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; packages to load in (package-initialize)
;; must include all dependencies for use as `package-selected-packages source
(setq package-load-list
      '(
        ;;; helm deps
        (helm-core t) (popup t) (async t) (helm t)
        (web-mode t)
        (web-beautify t) ; requires "js-beautify" in npm 
        (php-mode t)
        (highlight-symbol t)
        (auto-complete t)
        (epc t)
        (ggtags t)
        ;;; dired
        (dired-toggle-sudo t) (dired+ t)
        ;;; jedi deps
        (python-environment t) (ctable t) (deferred t) (concurrent t) (jedi-core t) (jedi t)
        (bash-completion t) ; for shell mode
        ;;; ace-window dep
        (avy t) (ace-window t)
        ;;; magit deps
        (magit-popup t) (git-commit t) (with-editor t) (dash t) (magit t)
        (nginx-mode t)
        (apache-mode t)
        (yaml-mode t)
        (macrostep t)
        (paredit t)
        (geiser t)
        ))

(setq package-enable-at-startup nil) ; in manual control mode
(package-initialize t) ; NO-ACTIVATE `t
(unless package-archive-contents
  (package-refresh-contents))
(setq package-selected-packages (mapcar 'car package-load-list))
;; 25.1+
(package-install-selected-packages) ; ensure packages
(package-initialize) ; activate

(when (display-graphic-p)
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
  (toggle-frame-maximized)
  (add-to-list 'default-frame-alist '(background-color . "#fafffa"))
  ; selection color
  (set-face-attribute 'region nil :background "#4AC0C9" :foreground "#ffffff")
  (setq make-backup-files nil)

  ;; desktop
  (global-set-key (kbd "C-c d r") 'desktop-read)
  (global-set-key (kbd "C-c d s") 'desktop-save)

  ;; Show file path in frame title
  (setq-default frame-title-format "%b (%f)")

  ;; disable toolbar
  (tool-bar-mode -1))

;;(when (not (display-graphic-p)))

;;****************************************************************

;; github.com/karbiv/cython-semantic in development
(add-to-list 'load-path "~/.emacs.d/cython-semantic")
(require 'cython-semantic-mode)
(add-to-list 'auto-mode-alist '("\\.py$" . cython-semantic-mode))

;;****************************************************************

(setq inhibit-startup-screen t)
;;(set-face-attribute 'default nil :font "Liberation Mono")
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono")
;;(set-face-attribute 'default nil :font "M+ 1m")
(set-face-attribute 'default nil :font "Ubuntu Mono")
(set-face-attribute 'default nil :height 132)
;;(setq ring-bell-function 'ignore) ; ignore sound notifications
(show-paren-mode 1)
(column-number-mode)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c s") 'delete-trailing-whitespace)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
;; Path to Emacs C source, for functions help system
(setq find-function-C-source-directory "/usr/src/debug/emacs-25.2/src")
;; Disable menu and scrollbars
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;(global-flycheck-mode)
;;(set-default 'truncate-lines t)
(setq dired-listing-switches "-hal --group-directories-first") ; dired format, options of 'ls' command

(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'auto-complete-mode)
(setq-default indent-tabs-mode nil) ; use spaces

;;; helm
(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;replace `find-file
(global-set-key (kbd "M-n") 'helm-semantic-or-imenu)
(helm-mode 1)

;;; dired
(define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
(setq dired-listing-switches "-hal --time-style=iso")

;; https://www.emacswiki.org/emacs/DiredGetFileSize
(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(define-key dired-mode-map (kbd "?") 'dired-get-size)

;;; tramp

;; (eval-after-load 'tramp
;;  '(progn
;;     ;; Allow to use: /sudo:user@host:/path/to/file
;;     (add-to-list 'tramp-default-proxies-alist
;;        '(".*" "\\`.+\\'" "/ssh:%h:"))))

;; something from stackoverflow
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
;; customizable
;;(setq-default password-cache-expiry 3600) ; How many seconds passwords are cached

;;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;;; macrostep
(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

;;; semantic
(semantic-mode 1)
;; disable semantic in some modes
(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)
        (html-mode . semantic-default-html-setup)
        (java-mode . wisent-java-default-setup)
        ;;(js-mode . wisent-javascript-setup-parser) ; js-mode's imenu is better
        (python-mode . wisent-python-default-setup)
        ;;(scheme-mode . semantic-default-scheme-setup) ; crashes in some scheme variants
        (srecode-template-mode . srecode-template-setup-parser)
        (texinfo-mode . semantic-default-texi-setup)
        (makefile-automake-mode . semantic-default-make-setup)
        (makefile-gmake-mode . semantic-default-make-setup)
        (makefile-makepp-mode . semantic-default-make-setup)
        (makefile-bsdmake-mode . semantic-default-make-setup)
        (makefile-imake-mode . semantic-default-make-setup)
        (makefile-mode . semantic-default-make-setup)))
(add-hook 'semantic-inhibit-functions (lambda () (member major-mode '(js-mode))))
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;;(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)

;;; shell mode
(add-hook 'shell-mode-hook #'bash-completion-setup)

;;; ggtags
(setenv "GTAGSCONF" "/home/alex/.globalrc")
;; github.com/universal-ctags/ctags
(setenv "GTAGSLABEL" "new-ctags") ; use Universal ctags
;;(setenv "GTAGSLABEL" "ctags") ; use Exuberant ctags, probably not maintained

;;----------------------------------------------------

(autoload 'python-mode "python" "Python Mode." t) ; built-in
;;(autoload 'python-mode "python-mode" "Python Mode." t)

(defvar ak-flycheck-toggle nil)
(defun ak-flycheck-mode ()
  (interactive)
  (if ak-flycheck-toggle
      (flycheck-mode -1)
    (flycheck-mode 1))
  (setq ak-flycheck-toggle (not ak-flycheck-toggle)))

(add-to-list 'auto-mode-alist '("\\.kv$"  . yaml-mode))
;;(add-hook 'python-mode-hook 'auto-complete-mode)
;;; to customize
;;(setq jedi:server-args '("--sys-path" "/home/...somepath.../venv/lib/python3.5/site-packages"))

(defun python-mode-func ()
  (setq tab-width 4)
  (hs-minor-mode)
  (jedi:setup)
  ;;(enable-paredit-mode) ; some problems in python buffers
  (abbrev-mode 1)
  (define-abbrev python-mode-abbrev-table  "pdb" "import pdb;pdb.set_trace()")
  (define-abbrev python-mode-abbrev-table  "here" "raise Exception('here')")
  (define-abbrev python-mode-abbrev-table  "pp" "import pprint; pp=pprint.PrettyPrinter(); pp.pprint()")
  (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer)
  ;;(setq python-shell-interpreter "ipython" python-shell-interpreter-args "-i")

  (local-unset-key (kbd "C-c !")) ; unhide flycheck
  (local-unset-key (kbd "C-c .")) ; unhide ecb
  (local-set-key (kbd "C-c f") 'ak-flycheck-mode)
  ;;(define-key jedi-mode-map (kbd "C-c ,") nil) ; unhide Semantic
  (define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "C-c p") 'jedi:goto-definition-pop-marker))
(add-hook 'python-mode-hook 'python-mode-func)

;;----------------------------------------------------

;; highlight-symbol
(global-set-key [(control f9)] 'highlight-symbol-at-point)
(global-set-key [f9] 'highlight-symbol-next)
(global-set-key [(shift f9)] 'highlight-symbol-prev)
(global-set-key [(meta f9)] 'highlight-symbol-query-replace)

(add-hook 'prog-mode-hook #'highlight-symbol-mode)
;;(setq highlight-symbol-on-navigation-p nil)

;;----------------------------------------------------

;;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;;----------------------------------------------------

;; http://stackoverflow.com/questions/7022898/emacs-autocompletion-in-emacs-lisp-mode
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;;(ido-mode t)
;;(global-set-key [(meta n )] 'idomenu)

(add-to-list 'auto-mode-alist '("\\.glsl$"  . c-mode))

;;; SQL mode
;;----------------------------------------------------

;;(load "~/.emacs.d/epsql/epsql.el")

;;----------------------------------------------------

(eval-after-load "php-mode"
  '(progn
     (define-abbrev php-mode-abbrev-table  "vdd" "var_dump(  );die();")
     (define-abbrev php-mode-abbrev-table  "vdb" "var_dump( debug_backtrace() );die();")
     ))
(add-hook 'php-mode-hook (lambda ()
                           (ggtags-mode 1)
                           (local-unset-key "C-M-\\")
                           (local-unset-key "C")
                           (setq indent-tabs-mode nil
                                 tab-width 4
                                 c-basic-offset 4)
                           (setq-local imenu-create-index-function #'ggtags-build-imenu-index)
                           ))
(global-set-key (kbd "<f11>") 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php$"  . php-mode))

;;----------------------------------------------------

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.css$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(add-hook 'web-mode-hook
          (lambda ()
            (ggtags-mode 1)
            (define-abbrev web-mode-abbrev-table  "vdd" "var_dump(  );die();")
            (define-abbrev web-mode-abbrev-table  "cnl" "console.log();") ;
            (setq web-mode-enable-part-face t)
            (abbrev-mode 1)
            (setq web-mode-enable-current-element-highlight t)
            (set-face-attribute 'web-mode-current-element-highlight-face nil :background "#CCCCCC")
            (set-face-attribute 'web-mode-html-tag-face nil :foreground "#0000CD")
            (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#007700")
            (setq web-mode-enable-block-face t)
            (set-face-attribute 'web-mode-block-face nil :background "#E3F2E1")
            ))
(setq web-mode-extra-snippets
      '((nil . (("div" . ("<div class=\"\">" . "</div>"))
                ;;("name" . ("beg" . "end"))
                ))
        ))
(global-set-key (kbd "<f12>") 'web-mode)

;;----------------------------------------------------

;;; web-beautify.el
;; requires "beautifier" through npm
(eval-after-load 'css-mode  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
(eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'web-mode  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))

;; linum-mode
(global-set-key (kbd "C-c l") 'linum-mode)

(global-set-key (kbd "C-c C-h") 'hl-line-mode)

(global-set-key [f1] 'speedbar-get-focus)
(add-hook 'speedbar-load-hook (lambda () (require 'semantic/sb)))

(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;; js-mode
(eval-after-load 'js-mode
  '(progn
     (define-abbrev js-mode-abbrev-table  "cnl" "console.log(  );")))

(add-hook 'js-mode-hook
          #'(lambda ()
              (setq tab-width 4)
              (abbrev-mode 1)
              (ggtags-mode 1)
              ;;(setq imenu-create-index-function #'ggtags-build-imenu-index)
              ;;(setq imenu-create-index-function #'js--imenu-create-index)
              (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)
              (define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-definition)
              ;;(define-key ggtags-mode-map (kbd "M-n") 'idomenu)
              ))

;; flymake jslint
;;(add-hook 'js2-mode-hook 'flymake-jslint-load)

(add-hook 'js-mode-hook
          #'(lambda ()
              (setq js-indent-level 2)))

(global-set-key (kbd "C-c C-r") 'revert-buffer)

;;; c mode
;; prepaint for C multiline macros, git submodule
(autoload 'prepaint-mode
  (concat (file-name-directory load-file-name) "prepaint/prepaint"))

(add-hook 'c-mode-common-hook
          (lambda ()
            (ggtags-mode)
            (prepaint-mode 1)
            (setq c-macro-preprocessor "cpp -CC")
            ;;(setq-local imenu-create-index-function #'ggtags-build-imenu-index)
            ;;(flycheck-mode)
            (hs-minor-mode)
            (define-key c-mode-map "\C-c\C-f" 'ff-find-other-file)
            (define-key c++-mode-map "\C-c\C-f" 'ff-find-other-file)))

;;****************************************************************

;;; utility functions

;; (defmacro measure-time (&rest body)
;;   "Measure the time it takes to evaluate BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06f" (float-time (time-since time)))))

;; (defun my-refresh ()
;;   "Force a full refresh of the current buffer's tags.
;; Throw away all the old tags, and recreate the tag database."
;;   (interactive)
;;   (measure-time
;;    (semantic-fetch-tags)
;;    (message "Buffer reparsed.")))

;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq my-package-list
      '(
        iedit
        desktop-registry
        rainbow-mode
        helm
	helm-swoop ; advanced search results presentation
        helm-systemd
        helm-ag ; alternative to grep
        highlight-symbol
	jedi
        go-mode
        go-autocomplete
        go-gopath ; set GOPATH in Emacs, gb build tool
        web-mode
	web-mode-edit-element
        ini-mode ; systemd, PKGBUILD
        ggtags
        dired-toggle-sudo
        bash-completion
        ace-window
	magit
        nginx-mode
        apache-mode
        yaml-mode
        macrostep
        paredit
        buffer-move
        php-mode
	flycheck
        company
	rust-mode cargo flycheck-rust racer
        smartparens ;; exact matching for Semantic lexer
        ssass-mode
        cmake-mode

        ))

(package-initialize) ; activate
(defun install-packages ()
  "Install packages listed in `my-package-list'"
  (interactive)
  (package-refresh-contents)
  (mapc #'package-install my-package-list))

(when (display-graphic-p)
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
  (toggle-frame-maximized)
  ;;(add-to-list 'default-frame-alist '(background-color . "#EEFFCC"))
  ;; selection color
  ;;(set-face-attribute 'region nil :background "#4AB0C9" :foreground "#ffffff")
  (setq make-backup-files nil)

  ;; desktop
  (global-set-key (kbd "C-c d s") 'desktop-save-in-desktop-dir)
  ;;(global-set-key (kbd "C-c d r") 'desktop-read)  
  (global-set-key (kbd "C-c d r") 'desktop-registry-change-desktop)

  ;; Show file path in frame title
  (setq-default frame-title-format "%b (%f)")

  ;; disable toolbar
  (tool-bar-mode -1))

;;(when (not (display-graphic-p)))

;;****************************************************************

(setq inhibit-startup-screen t)
;;(set-face-attribute 'default nil :font "Liberation Mono")
;;(set-face-attribute 'default nil :font "Hack")
;;(set-face-attribute 'default nil :font "Ubuntu Mono")
;;(set-face-attribute 'default nil :height 132)
(setq ring-bell-function 'ignore) ; ignore sound notifications
;;(setq visible-bell 1)
(show-paren-mode 1)
(column-number-mode)
;; https://stackoverflow.com/questions/5738170/why-does-emacs-create-temporary-symbolic-links-for-modified-files
(setq create-lockfiles nil)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c s") 'delete-trailing-whitespace)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
;; Path to Emacs C source, for functions help system
(setq find-function-C-source-directory "~/soft/emacs/src")
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

(global-set-key (kbd "<f8>") #'buf-move)
(global-set-key (kbd "C-c C-r")
                (lambda () ; `rever-buffer', noconfirm, preserve-modes
                  (interactive)
                  (revert-buffer t t t)
                  ))
(global-set-key (kbd "C-x s") #'save-buffer) ; redefine from `save-some-buffers with prompt

;; http://stackoverflow.com/questions/7022898/emacs-autocompletion-in-emacs-lisp-mode
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;;----------------------------------------------------
;;; helm

;; (require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;replace `find-file
(global-set-key (kbd "M-n") 'helm-semantic-or-imenu)

;;; helm-ag
(setq-default helm-ag-insert-at-point 'symbol)

;;; helm-swoop

(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;;----------------------------------------------------

;; highlight-symbol
(global-set-key [(control f9)] 'highlight-symbol-at-point)
(global-set-key [f9] 'highlight-symbol-next)
(global-set-key [(shift f9)] 'highlight-symbol-prev)
(global-set-key [(meta f9)] 'highlight-symbol-query-replace)

(add-hook 'prog-mode-hook #'highlight-symbol-mode)
;;(setq highlight-symbol-on-navigation-p nil)


;;----------------------------------------------------
;;; dired

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

(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
            (setq dired-listing-switches "-hal --time-style=iso")
            (define-key dired-mode-map (kbd "?") 'dired-get-size)))

;;; tramp

;; something from stackoverflow
;; (setq tramp-ssh-controlmaster-options
;;       "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
(setq-default password-cache-expiry 3600) ; How many seconds passwords are cached

;;----------------------------------------------------
;;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;;; macrostep
(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

;;----------------------------------------------------
;;; semantic
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
        (makefile-mode . semantic-default-make-setup)
        (hs-minor-mode 1)))

(add-hook 'semantic-inhibit-functions
          (lambda ()
            (when (member major-mode '(js-mode php-mode))
              t)))

;; (setq semantic-default-submodes '(
;;                                   global-semantic-idle-scheduler-mode
;;                                   global-semantic-decoration-mode
;;                                   global-semantic-stickyfunc-mode
;;                                   global-semantic-highlight-func-mode
;;                                   global-semantic-idle-completions-mode
;;                                   global-semanticdb-minor-mode
;;                                   ))

;;----------------------------------------------------
;;; shell mode
(add-hook 'shell-mode-hook #'bash-completion-setup)

;;; ggtags
(setq ggtags-highlight-tag nil)
;; cp /usr/share/gtags/gtags.conf ~/.globalrc
;; github.com/universal-ctags/ctags
;; for Universal Ctags installed as ctags
(setenv "GTAGSLABEL" "new-ctags")
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;;----------------------------------------------------
;; rust-mode

;; dev
;; (add-to-list 'load-path "~/rust-semantic")
;; (require 'rust-semantic-mode)

;; rustup component add rls-preview rust-analysis rust-src
;; cargo install racer
;; cargo install --force rustfmt-nightly

;;(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda ()
            (racer-mode 1)
            (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
            (define-key rust-mode-map (kbd "C-c C-c") #'rust-compile)
            (setq company-tooltip-align-annotations t)
            (flycheck-mode 1)
            (flycheck-rust-setup)
            ;; "M-{" is already used for `backward-paragraph'
            (define-key rust-mode-map (kbd "C-{") #'insert-pair)
            ;;(rust-semantic-mode 1)
            (abbrev-mode 1)
            (define-mode-abbrev "pnl" "println!(\"{:?}\",  );")
            (eldoc-mode -1) ;; disable, slow
            ))
(add-hook 'racer-mode-hook #'company-mode)
;; customizable vars `company-idle-delay' and `company-minimum-prefix-length'

;;----------------------------------------------------
;; makefile-mode

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq tab-width 4)))

;;----------------------------------------------------

;; go-mode

(add-to-list 'auto-mode-alist '("\\.mod$" . go-mode)) ; go.mod files

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq go-packages-function 'go-packages-go-list)
            (define-key go-mode-map (kbd "M-.") #'godef-jump) ; or `#'godef-jump-other-window'
            (define-key go-mode-map (kbd "C-.") #'ggtags-find-tag-dwim) ; for CGO
            (define-key go-mode-map (kbd "C-<tab>") #'auto-complete)
            (iedit-mode 1)
            (auto-complete-mode 1)
            (abbrev-mode 1)
            ;; print all methods that item implements
            (define-abbrev go-mode-abbrev-table  "impls"
              "foo := reflect.TypeOf();
    for i:=0; i<foo.NumMethod();i++ {method:=foo.Method(i);Println(method.Name)}")
            
            ;;; Bindings in  go-goto-map
            ;; (define-key m "a" #'go-goto-arguments)
            ;; (define-key m "d" #'go-goto-docstring)
            ;; (define-key m "f" #'go-goto-function)
            ;; (define-key m "i" #'go-goto-imports)
            (define-key go-goto-map "i" #'ak-go-goto-imports)
            ;; (define-key m "m" #'go-goto-method-receiver)
            ;; (define-key m "n" #'go-goto-function-name)
            ;; (define-key m "r" #'go-goto-return-values)
            ))
;; requires github.com/nsf/gocode
;; `go-gopath-set-gopath'
(eval-after-load "go-mode" '(require 'go-autocomplete))

(defun ak-go-goto-imports ()
  "Uses xref mark ring to return back from imports section, when quick (un)comment of import is done."
  (interactive)
  (xref-push-marker-stack)
  (go-goto-imports))

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
(add-hook 'yaml-mode-hook
          (lambda ()
            (setq yaml-indent-offset 2)))
;;(add-hook 'python-mode-hook 'auto-complete-mode)
;;; to customize
;;(setq jedi:server-args '("--sys-path" "/home/...somepath.../venv/lib/python3.5/site-packages"))

(defun python-mode-func ()
  (setq tab-width 4)
  (hs-minor-mode)
  (jedi:setup)
  (abbrev-mode 1)
  (define-abbrev python-mode-abbrev-table  "pdb" "import pdb;pdb.set_trace()")
  (define-abbrev python-mode-abbrev-table  "here" "raise Exception('here')")
  (define-abbrev python-mode-abbrev-table  "pp" "import pprint; pp=pprint.PrettyPrinter(); pp.pprint()")
  (define-key python-mode-map (kbd "C-c C-r") 'revert-buffer) ; orig is send region to python shell
  (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer)
  ;;(setq python-shell-interpreter "ipython" python-shell-interpreter-args "-i")

  (local-unset-key (kbd "C-c !")) ; unhide flycheck
  (local-unset-key (kbd "C-c .")) ; unhide ecb
  (local-set-key (kbd "C-c f") 'ak-flycheck-mode)
  (define-key jedi-mode-map (kbd "C-c p") 'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
  )
(add-hook 'python-mode-hook 'python-mode-func)

;;----------------------------------------------------

;;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;;----------------------------------------------------

;;(ido-mode t)
;;(global-set-key [(meta n )] 'idomenu)

;;----------------------------------------------------
;; ini-mode

(add-to-list 'auto-mode-alist '("\\.ini$"  . ini-mode))
(add-to-list 'auto-mode-alist '("\\.service\\|\\.target$"  . ini-mode)) ; Systemd
(add-to-list 'auto-mode-alist '("PKGBUILD$"  . ini-mode)) ; Arch package file
(add-to-list 'auto-mode-alist '("\\.toml$"  . ini-mode)) ; Cargo.toml

;;----------------------------------------------------

(eval-after-load "php-mode"
  '(progn
     (define-abbrev php-mode-abbrev-table  "vdd" "var_dump(  );die();")
     (define-abbrev php-mode-abbrev-table  "vdb" "var_dump( debug_backtrace() );die();")
     ))
(add-hook 'php-mode-hook
          (lambda ()
            (hs-minor-mode 1)
            (local-unset-key "C-M-\\")
            (local-unset-key "C")
            (c-set-style "symfony2")
            (setq indent-tabs-mode t
                  tab-width 4
                  c-basic-offset 4) 
            (define-key php-mode-map (kbd "M-n") 'imenu)
            ;;(setq imenu-create-index-function #'ggtags-build-imenu-index)
            (define-key php-mode-map (kbd "C-c C-r") 'revert-buffer)
            ))
(global-set-key (kbd "<f11>") 'php-mode)
;;(add-to-list 'auto-mode-alist '("\\.php$"  . php-mode))
(add-to-list 'auto-mode-alist '("\\.php$"  . web-mode))

;;----------------------------------------------------
;;; web-mode
(add-to-list 'auto-mode-alist '("\\.qtpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

(eval-after-load 'web-mode
  '(progn
     (define-key web-mode-map (kbd "C-M-n") 'web-mode-element-next)
     (define-key web-mode-map (kbd "C-M-p") 'web-mode-element-previous)
     ))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(add-hook 'web-mode-hook
          (lambda ()
            (setq tab-width 2)
            (ggtags-mode 1)
            ;;(hs-minor-mode)
            (setq web-mode-script-padding 2) ; indent in script tag
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-enable-current-element-highlight t)
            ;;(setq web-mode-enable-current-column-highlight t)
            (face-spec-set 'web-mode-current-element-highlight-face
                           '((t :foreground "DimGray" :background "Aquamarine"))
                           'face-defface-spec)
            (face-spec-set 'web-mode-current-column-highlight-face
                           '((t :background "Gainsboro"))
                           'face-defface-spec)
            ;;(setq web-mode-enable-part-face t)
            (abbrev-mode 1)
            (define-mode-abbrev "vdd" "var_dump(  );die();")
            (define-mode-abbrev "cnl" "console.log();")
            (define-mode-abbrev "vdb" "var_dump( debug_backtrace() );die();")
            ;;(set-face-attribute 'web-mode-html-tag-face nil :foreground "#0000CD")
            ;;(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#007700")
            ;;(setq web-mode-enable-block-face t)
            ;;(set-face-attribute 'web-mode-block-face nil :background "#EBFAE8")
            ;;(set-face-attribute 'web-mode-block-face nil :background "#8fbc8f")
            (setq imenu-create-index-function #'ggtags-build-imenu-index)
            ))
(setq web-mode-extra-snippets
      '((nil . (("div" . ("<div class=\"\">" . "</div>"))
                ;;("name" . ("beg" . "end"))
                ))
        ))
(global-set-key (kbd "<f12>") 'web-mode)

;;----------------------------------------------------
;;; xml

;;(add-to-list 'auto-mode-alist '("\\.xml$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"  . web-mode))

;;----------------------------------------------------
;;; css

(add-hook 'css-mode-hook
          #'(lambda ()
              (define-key css-mode-map (kbd "M-.") 'helm-ag)))

;;----------------------------------------------------
;;; sass|scss-mode
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . ssass-mode))
(eval-after-load 'scss-mode '(define-key scss-mode-map (kbd "C-c b") 'web-beautify-css))

;;----------------------------------------------------
;;; web-beautify.el
;; requires "beautifier" through npm
(eval-after-load 'css-mode  '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
(eval-after-load 'json-mode '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
(eval-after-load 'sgml-mode '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
(eval-after-load 'web-mode  '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
;; npm config set prefix ~/.npm
(eval-after-load "web-beautify"
  #'(lambda ()
      (setq web-beautify-html-program "~/.npm/bin/html-beautify")
      (setq web-beautify-css-program "~/.npm/bin/css-beautify")
      (setq web-beautify-js-program "~/.npm/bin/js-beautify")))

;;----------------------------------------------------
;;; linum-mode
(global-set-key (kbd "C-c l") 'linum-mode)

;;(global-set-key (kbd "C-c C-h") 'hl-line-mode)

;;(global-set-key [f1] 'speedbar-get-focus)

(setq recentf-auto-cleanup 'never)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;;----------------------------------------------------
;;; js-mode

(defun abbrev-console-log ()
  (backward-char 2) t)
(put 'abbrev-console-log 'no-self-insert t)

(add-hook 'js-mode-hook
          #'(lambda ()
              (setq tab-width 4)
              (abbrev-mode 1)
              (setq imenu-create-index-function #'js--imenu-create-index)
              (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)
              ;;(define-key js-mode-map (kbd "M-.") 'helm-ag)
              (define-abbrev js-mode-abbrev-table  "conl" "console.log();"
                'abbrev-console-log)
              (setq js-indent-level 2)
              ))

;;----------------------------------------------------
;;; c mode

;; git submodule, prepaint for C multiline macros
;; (autoload 'prepaint-mode
;;   (concat (file-name-directory load-file-name) "prepaint/prepaint"))

(add-to-list 'auto-mode-alist '("\\.glsl$"  . c-mode)) ; GL shaders

;; for rtags-fallback.el
(add-to-list 'load-path (file-name-directory load-file-name))

;; rtags from git submodule
;;(setq rtags-dir (concat (file-name-directory load-file-name) "rtags/"))
;;(add-to-list 'load-path (concat rtags-dir "src")) ; for rtags.el

;; load compile_commands.json in `pwd'
;; (defun rtags-load-cmds ()
;;   (interactive)
;;   (shell-command (concat rtags-dir "bin/rc -J .") nil))

(defun c-cpp-init ()
  (setq c-macro-preprocessor "cpp -CC")
  (hs-minor-mode 1)
  (hs-minor-mode) ; hide/show blocks
  (define-key c-mode-map "\C-c\C-f" 'ff-find-other-file)
  (define-key c++-mode-map "\C-c\C-f" 'ff-find-other-file)
  (define-key c-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)
  (define-key c++-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)
  (flycheck-mode 1)
  (when (member major-mode '(c-mode c++-mode))
    ;;(require 'rtags) ; rtags.el must be from git submodule
    ;;(setq rtags-path (concat rtags-dir "bin"))
    ;;(rtags-start-process-unless-running)
    ;;(setq rtags-display-result-backend 'helm)
    ;;(require 'rtags-fallback)
    ;;(init-rtags-fallback-map)
    ;;(require 'flycheck-rtags)
    ;;(my-flycheck-rtags-setup)
    )
  ;;(prepaint-mode 1)
  )

(add-hook 'c-mode-common-hook 'c-cpp-init)

(defun my-flycheck-rtags-setup ()
  "Configure flycheck-rtags for better experience."
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-check-syntax-automatically nil)
  (setq-local flycheck-highlighting-mode nil))

;; (defface prepaint-face
;;   '((((class color) (background light)) (:background "azure")))
;;   "Face for prepaint."
;;   :group 'prepaint)

;;----------------------------------------------------

;; Slime
(setq inferior-lisp-program "/usr/bin/sbcl")
;;(setq inferior-lisp-program "~/")
(setq slime-contribs '(slime-fancy))
(add-hook
 'slime-mode-hook
 (lambda ()
   (define-key slime-mode-map (kbd "C-c M-n") 'slime-next-note)
   (define-key slime-mode-map (kbd "C-c M-p") 'slime-previous-note)
   (enable-paredit-mode)
   (load "slime-asdf")
   (setq common-lisp-hyperspec-root
         (expand-file-name "~/Documents/CommonLisp/HyperSpec-7-0/HyperSpec/"))
   ))


;;----------------------------------------------------

;;; utility functions

;; (defmacro measure-time (&rest body)
;;   "Measure the time it takes to evaluate BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06f" (float-time (time-since time)))))

;; dev

(add-to-list 'load-path "~/worksp/emacs_earley")
(load "earley")

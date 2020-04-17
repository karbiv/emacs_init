;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

; Some combination of GNU TLS and Emacs fail to retrieve archive
; contents over https.
; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version< emacs-version "26.3") (>= libgnutls-version 30604))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq package-selected-packages
      '(
        ;;undo-tree
        org-super-agenda
        realgud
        ;;preproc-font-lock
        ac-geiser
        geiser
        which-key
        cython-mode
        js2-mode
        tern ; js code analysis
        json-mode
        company
        company-tern
        web-beautify
        imenu
        multiple-cursors
        cmake-mode
        desktop-registry
        helm
        helm-tramp
        helm-systemd
        helm-swoop
        helm-ag
        helm-gtags
        ssass-mode
        smartparens
        rainbow-mode
        flycheck

        projectile
        yasnippet

        ng2-mode

        lsp-mode
        helm-lsp
        lsp-java
        hydra ; key bindings like "C-c jjkk3j5k"
        dap-mode
        company-lsp
        lsp-ui
        kotlin-mode
        gradle-mode
        flycheck-kotlin
        lsp-typescript
        treemacs
        lsp-treemacs
        treemacs-projectile
        groovy-mode ; gradle scripts

        rust-mode
        cargo
        flycheck-rust
        racer

        web-mode
        yaml-mode
        web-mode-edit-element
        php-mode
        paredit
        nginx-mode
        magit
        macrostep
        jedi
        ini-mode
        highlight-symbol
        go-mode
        go-gopath
        company-go
        ggtags
        rtags ; c++, clang
        helm-rtags
        dired-toggle-sudo
        buffer-move
        bash-completion
        ascii
        apache-mode
        ace-window
        glsl-mode

	;; themes
	cloud-theme
	farmhouse-theme
	lab-themes
	material-theme
	flucui-themes
	one-themes

	))

(if (getenv "DEV")
    (progn
      ;; disable activation of some installed packages
      (setq package-load-list
            (append '((helm nil))
                    '((helm-core nil))
                    package-load-list))

      ;; add dev repository of disable packages to `load-path'
      (add-to-list 'load-path "~/worksp/emacs_package_dev/helm")

      (package-initialize)

      ;; now load package from dev repository as its docs recommend
      (load "helm-config"))
  ;; else
  (package-initialize))

(when (display-graphic-p)
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
  (toggle-frame-maximized)
  ;; selection color
  ;;(set-face-attribute 'region nil :background "#4AB0C9" :foreground "#ffffff")
  ;; Show file path in frame title
  (setq-default frame-title-format "%b (%f)"))

(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)

(tool-bar-mode -1) ; disable toolbar
(line-number-mode 1) ; show line numbers in modeline

(global-set-key (kbd "C-c r") 'rgrep) ;; recursive grep

;; undo-tree
;; (global-undo-tree-mode 1)
;; (global-set-key (kbd "C-z") 'undo)
;; (defalias 'redo 'undo-tree-redo)
;; (global-set-key (kbd "C-S-z") 'redo)

(setq make-backup-files nil)
;; desktop
(global-set-key (kbd "C-c d s") 'desktop-save-in-desktop-dir)
;;(global-set-key (kbd "C-c d r") 'desktop-read)
(global-set-key (kbd "C-c d r") 'desktop-registry-change-desktop)
(setq inhibit-startup-screen t)
;;(set-face-attribute 'default nil :font "Liberation Mono")
;;(set-face-attribute 'default nil :font "Ubuntu Mono")
(set-face-attribute 'default nil :font "Iosevka")
(set-face-attribute 'default nil :height 108)
(setq ring-bell-function 'ignore) ; ignore sound notifications
;;(setq visible-bell 1)
(show-paren-mode 1)
(column-number-mode)
;; https://stackoverflow.com/questions/5738170/why-doeppps-emacs-create-temporary-symbolic-links-for-modified-files
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
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(setq-default indent-tabs-mode nil) ; use spaces

(global-set-key (kbd "C-o") #'ace-window)
(global-set-key (kbd "M-p") (lambda ()
                              (interactive)
                              (set-window-buffer (selected-window) (other-buffer))))

;; http://stackoverflow.com/questions/7022898/emacs-autocompletion-in-emacs-lisp-mode
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(global-set-key (kbd "C-x s") 'save-buffer) ; replace `save-some-buffers'

(global-set-key (kbd "C-`") 'kill-current-buffer)
(global-set-key (kbd "C-;") 'mc/mark-all-dwim)

(global-set-key (kbd "C-c C-r") 'revert-buffer)

(global-company-mode 1)

;;(global-set-key (kbd "<f8>") #'buf-move)
(global-set-key (kbd "<f8>") #'rgrep)

;;----------------------------------------------------
;;; helm

;; (require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c j") 'helm-mini)
(global-set-key (kbd "C-c C-j") 'helm-mini)
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
            ;;(setq dired-listing-switches "-hal --time-style=iso")
            (setq dired-listing-switches "-hal")
            (define-key dired-mode-map (kbd "?") 'dired-get-size)))

;;; tramp
(setq-default password-cache-expiry 604800) ; How many seconds passwords are cached
(setq auth-source-debug t)
(eval-after-load 'tramp
  (setenv "SHELL" "/bin/bash"))


;;; macrostep
(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

;;----------------------------------------------------
;;; semantic
;; disable semantic in some modes
(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)
        (html-mode . semantic-default-html-setup)
        (java-mode . wisent-java-default-setup) ; lsp-java
        ;;(js-mode . wisent-javascript-setup-parser) ;
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
            (when
                ;;(member major-mode '(js-mode php-mode))
                (member major-mode '(js-mode))
              t)))

(semantic-mode 1)
;; semantic fixes
(load-file
 (expand-file-name "semantic_fixes.el"
		   (file-name-directory load-file-name)))

(add-hook 'semantic-decoration-mode-hook
          (lambda ()
            (setq semantic-decoration-styles
                  '(("semantic-decoration-on-includes" . t)
                    ("semantic-decoration-on-protected-members" . nil)
                    ("semantic-decoration-on-private-members" . nil)
                    ("semantic-tag-boundary" . t)))))
(global-semantic-decoration-mode 1)
(global-semantic-stickyfunc-mode 1)

;;----------------------------------------------------
;;; shell mode
(add-hook 'shell-mode-hook #'bash-completion-setup)

;;----------------------------------------------------
;;; makefile-mode

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq tab-width 4)))

;;----------------------------------------------------
;;; go-mode

(add-to-list 'auto-mode-alist '("\\.mod$" . go-mode)) ; go.mod files

(defun abbrev-iferr ()
  (indent-for-tab-command)
  (forward-line -1)
  (delete-trailing-whitespace))

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq go-packages-function 'go-packages-go-list)

            ;; for CGO
            (ggtags-mode 1)
            (define-key go-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)
            (define-key go-mode-map (kbd "C-,") #'ggtags-prev-mark)

            ;;(local-unset-key (kbd "M-.")) ; unmask ggtags
            (define-key ggtags-mode-map (kbd "M-.") nil)
            (define-key go-mode-map (kbd "M-.") #'godef-jump-other-window)

            ;; TODO change to company-mode
            ;;(define-key go-mode-map (kbd "C-<tab>") #'auto-complete)
            ;;(auto-complete-mode 1)
            (abbrev-mode 1)
            ;; print all methods that item implements
            (define-abbrev go-mode-abbrev-table "impls"
              "foo := reflect.TypeOf();
    for i:=0; i<foo.NumMethod();i++ {method:=foo.Method(i);Println(method.Name)}")
            (define-abbrev go-mode-abbrev-table "ifer"
              "if err != nil {

}" 'abbrev-iferr)

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
;;; to customize
;;(setq jedi:server-args '("--sys-path" "/home/...somepath.../venv/lib/python3.5/site-packages"))

(defun python-mode-abbrev-debug-handler ()
  (forward-line -1)
  (funcall indent-line-function)
  (move-end-of-line 1)
  (save-buffer 0))

(defun python-mode-func ()
  (setq tab-width 4)
  (hs-minor-mode)
  (jedi:setup)
  (abbrev-mode 1)
  (semantic-mode 1)
  (define-abbrev python-mode-abbrev-table  "pd" "import os
if os.getenv('AKDEBUG'):import pdb;pdb.set_trace()
" #'python-mode-abbrev-debug-handler)
  (define-abbrev python-mode-abbrev-table  "tr" "import os
if os.getenv('AKDEBUG'):from trepan.api import debug;debug()
" #'python-mode-abbrev-debug-handler)
  (define-abbrev python-mode-abbrev-table  "ipd" "import os
if os.getenv('AKDEBUG'):import ipdb;ipdb.set_trace()
" #'python-mode-abbrev-debug-handler)
  (define-abbrev python-mode-abbrev-table  "pp" "import pprint; pp=pprint.PrettyPrinter(); pp.pprint()")
  (define-key python-mode-map (kbd "C-c C-r") 'revert-buffer) ; orig is send region to python shell
  (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer)
  ;;(setq python-shell-interpreter "ipython" python-shell-interpreter-args "-i")

  (local-unset-key (kbd "C-c !")) ; unhide flycheck
  (local-set-key (kbd "C-c f") #'ak-flycheck-mode)
  (define-key jedi-mode-map (kbd "C-c p") #'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "C-c ,") nil) ; unhide `semantic-force-refresh'
  (define-key jedi-mode-map (kbd "M-.") (lambda ()
                                          (interactive)
                                          (xref-push-marker-stack)
                                          (jedi:goto-definition)))
  (define-key jedi-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)
  )
(add-hook 'python-mode-hook 'python-mode-func)

;;----------------------------------------------------
;;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;;----------------------------------------------------
;;; ini-mode

(add-to-list 'auto-mode-alist '("\\.ini$"  . ini-mode))
(add-to-list 'auto-mode-alist '("\\.service\\|\\.target$"  . ini-mode)) ; Systemd
(add-to-list 'auto-mode-alist '("PKGBUILD$"  . ini-mode)) ; Arch package file
(add-to-list 'auto-mode-alist '("\\.toml$"  . ini-mode)) ; Cargo.toml

;;----------------------------------------------------
;;; php-mode

(eval-after-load "php-mode"
  '(progn
     (define-abbrev php-mode-abbrev-table  "vdd" "var_dump(  );die();")
     (define-abbrev php-mode-abbrev-table  "vdb" "var_dump( debug_backtrace() );die();")
     ))
(add-hook 'php-mode-hook
          (lambda ()
            (semantic-mode 1)
            (hs-minor-mode 1)
            (local-unset-key "C-M-\\")
            (local-unset-key "C")
            (c-set-style "symfony2")
            (setq indent-tabs-mode t
                  tab-width 4
                  c-basic-offset 4)
            (define-key php-mode-map (kbd "M-n") 'imenu)
            (define-key php-mode-map (kbd "C-c C-r") 'revert-buffer)
            (define-key php-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
            ))
(add-to-list 'auto-mode-alist '("\\.php$"  . php-mode))
;;(add-to-list 'auto-mode-alist '("\\.php$"  . web-mode))
(global-set-key (kbd "<f11>") 'php-mode)
(global-set-key (kbd "<f12>") 'web-mode)

;;----------------------------------------------------
;;; web-mode

(add-to-list 'auto-mode-alist '("\\.qtpl$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

(eval-after-load 'web-mode
  '(progn
     (define-key web-mode-map (kbd "C-M-n") 'web-mode-element-next)
     (define-key web-mode-map (kbd "C-M-p") 'web-mode-element-previous)
     ))

(setq web-mode-engines-alist
      '(("django" . "\\.html\\'")))
(add-hook 'web-mode-hook
          (lambda ()
            (setq tab-width 4)
            ;;(hs-minor-mode)
            (setq web-mode-script-padding 2) ; indent in script tag
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 4)
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
            (ggtags-mode 1)
            (define-key web-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
            (define-key web-mode-map (kbd "M-,") #'ggtags-prev-mark)
            ;;(set-face-attribute 'web-mode-html-tag-face nil :foreground "#0000CD")
            ;;(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#007700")
            ;;(setq web-mode-enable-block-face t)
            ;;(set-face-attribute 'web-mode-block-face nil :background "#EBFAE8")
            ;;(set-face-attribute 'web-mode-block-face nil :background "#8fbc8f")
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
          (lambda ()
            (rainbow-mode t)
            (define-key css-mode-map (kbd "M-.") 'helm-ag)))

;;----------------------------------------------------
;;; sass|scss-mode

(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . ssass-mode))
(eval-after-load 'scss-mode '(define-key scss-mode-map (kbd "C-c b") 'web-beautify-css))

(add-hook 'ssass-mode-hook
          (lambda ()
            (rainbow-mode t)
            (define-key ssass-mode-map (kbd "M-.") 'helm-ag)))

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

;;;
;;(global-set-key (kbd "C-c C-h") 'hl-line-mode)

;;----------------------------------------------------
;;; js2-mode

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'company-backends 'company-tern)
;; npm install tern
(setq tern-command (list (expand-file-name "~/soft/tern/bin/tern")))

(defun abbrev-console-log ()
  (backward-char 2) t)
(put 'abbrev-console-log 'no-self-insert t)


;; (load-file
;;  (expand-file-name "javascript_imenu.el"
;;                    (file-name-directory load-file-name)))

;; (defun my-js2-imenu ()
;;   (interactive)
;;   (js2-mode-create-imenu-index))

(defun my-imenu-rescan ()
  (interactive)
  (imenu--menubar-select imenu--rescan-item))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq tab-width 4)
            (abbrev-mode 1)
            (rainbow-mode 1)
            (tern-mode 1)
            (company-mode 1)
            (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)
            ;;(define-key js-mode-map (kbd "M-.") 'helm-ag)
            (define-abbrev js-mode-abbrev-table  "cnl" "console.log();"
              'abbrev-console-log)
            (setq js-indent-level 2)
            ))

;;----------------------------------------------------
;; JSON

(add-to-list 'auto-mode-alist '("\\.tern-project$" . json-mode))

;;----------------------------------------------------
;;; Scheme mode

(add-hook 'scheme-mode-hook
          #'(lambda ()
              (abbrev-mode 1)
              (define-abbrev scheme-mode-abbrev-table  "nl" "(newline)")
              (define-abbrev scheme-mode-abbrev-table  "dl" "(display )")))

;;----------------------------------------------------
;;; lsp-mode

(add-hook 'lsp-ui-mode-hook
          (lambda ()

            (lsp-ui-sideline-mode -1)
            ;;(global-set-key (kbd "C-c l") 'linum-mode)
            (define-prefix-command 'lsp-prefix)
            (define-key lsp-prefix (kbd "d") #'lsp-ui-doc-hide)
            ;; toggle Sideline mode
            (define-key lsp-prefix (kbd "s") #'lsp-ui-sideline-mode)
            ;; execute code action
            (define-key lsp-prefix (kbd "a") #'lsp-execute-code-action)
            ;; toggle treemacs
            (define-key lsp-prefix (kbd "t") #'treemacs)
            ;; lsp workspaces
            ;;(define-key lsp-prefix (kbd "w") #')

            (global-set-key (kbd "C-c l") 'lsp-prefix)

            (setq lsp-ui-sideline-show-code-actions nil
                  lsp-ui-sideline-show-symbol nil
                  lsp-ui-sideline-ignore-duplicate t
                  lsp-ui-sideline-show-diagnostics t
                  lsp-ui-sideline-show-hover t
                  lsp-ui-flycheck-enable t
                  lsp-ui-flycheck-list-position 'right
                  lsp-ui-flycheck-live-reporting t
                  lsp-ui-peek-enable t
                  lsp-ui-peek-list-width 60
                  lsp-ui-peek-peek-height 25
                  )

            (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
            (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
            ))


;;----------------------------------------------------
;;; java mode

(setq lsp-java-vmargs
      (list
       "-noverify"
       "-Xmx1G"
       "-XX:+UseG1GC"
       "-XX:+UseStringDeduplication"
       ;;"-javaagent:/path/to/lombok-1.18.6.jar"
       ))

(setq ak-lsp-java-boot-enabled nil)

(defun spring-boot-init ()
  (interactive)
  (require 'lsp-java-boot)
  ;; to enable the lenses
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

  (setq ak-lsp-java-boot-enabled t)

  ;; revert all java-mode buffers
  (dolist ($buf (buffer-list (current-buffer)))
    (with-current-buffer $buf
      (when (eq major-mode 'java-mode)
        ;; revert buffer to execute hooks
        (revert-buffer nil ; IGNORE-AUTO
                       t ; NOCONFIRM
                       nil ; PRESERVE-MODES
                       )))))

(add-hook 'java-mode-hook
          (lambda ()
            (setq tab-width 4)
            (yas-minor-mode t)

            ;; do not restore Spring Tools Suite language Server in desktop-read, "boot-ls"
            (if ak-lsp-java-boot-enabled
                (setq lsp-java-boot-enabled t)
              (setq lsp-java-boot-enabled nil))

            (setq lsp-java-references-code-lens-enabled nil)
            (setq lsp-java-implementations-code-lens-enabled nil)
            (require 'lsp-java)
            (setq lsp-prefer-flymake nil)
            (lsp nil)

            (lsp-ui-sideline-mode -1)
            (gradle-mode t)
            (projectile-mode t)
            ;;(define-key java-mode-map "M-." 'lsp-find-type-definition)
            (hl-line-mode t)

            ))


;;----------------------------------------------------
;;; c mode

(add-hook 'c-mode-hook
          (lambda ()
            ;; gnu, k&r, bsd, stroustrup, whitesmith, ellemtel, linux, python, java, awk
            (c-set-style "gnu")
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (setq c-macro-preprocessor "cpp -CC")
              ;;(preproc-font-lock-mode 1)
              (hs-minor-mode 1) ; hide/show blocks
              (define-key c-mode-map "\C-c\C-f" 'ff-find-other-file)
              (define-key c++-mode-map "\C-c\C-f" 'ff-find-other-file)
              ;;(flycheck-mode 1)
              (define-key c-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
              (define-key c-mode-map (kbd "M-,") #'ggtags-prev-mark)
              ;;(semantic-mode 1)
              (semantic-idle-breadcrumbs-mode 1)
              (c-add-style "python-new"
                           '("python"
                             (c-basic-offset . 4))
                           t)
              ;; disable auto-align of endline backslashes in multiline macros
              (setq c-auto-align-backslashes nil)
              (abbrev-mode 1)
              (define-abbrev c-mode-abbrev-table "err" "#error \"stop here\"")
              ;; expand and print macros for diagnostic
              (define-abbrev c-mode-abbrev-table "def"
                "#define XSTR(x) STR(x)
#define STR(x) #x
#pragma message \"The value: \" XSTR()
#error \"stop here\"")
              )))

;;----------------------------------------------------
;;; c++ mode

(add-hook 'c++-mode-hook
          (lambda ()
            (rtags-start-process-unless-running)
            (setq rtags-jump-to-first-match nil) ; show multiple matches
            (setq rtags-display-result-backend 'helm) ; show it in helm
            (setq rtags-results-buffer-other-window t) ; in other window

            ;; mask a key binding of ggtags minor mode
            (let ((oldmap (cdr (assoc 'ggtags-mode minor-mode-map-alist)))
                  (newmap (make-sparse-keymap)))
              (set-keymap-parent newmap oldmap)
              (define-key newmap (kbd "M-.") 'rtags-find-symbol-at-point)
              (define-key newmap (kbd "M-,") 'rtags-location-stack-back)
              (make-local-variable 'minor-mode-overriding-map-alist)
              (push `(ggtags-mode . ,newmap) minor-mode-overriding-map-alist))
            ))

;;----------------------------------------------------
;; rust-mode

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
;;; org mode

;;(global-set-key (kbd "C-c a") 'org-agenda)
;;(global-set-key (kbd "C-c c") 'org-capture)

;;----------------------------------------------------

;;; utility functions

;; (defmacro measure-time (&rest body)
;;   "Measure the time it takes to evaluate BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (message "%.06f" (float-time (time-since time)))))


;;; dev

;; (add-to-list 'load-path "~/worksp/cython-semantic")
;; (require 'cython-semantic-mode)

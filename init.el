
;;----------------------------------------------------

;;; selected packages
;;----------------------------------------------------

;; remap left ALT below X key to CTRL
;;(setq x-meta-keysym 'ctrl)
;;(setq x-ctrl-keysym 'meta)
;; better in https://github.com/rvaiya/keyd

(setenv "LSP_USE_PLISTS" "true")
;; Emacs default is too low 4k considering that the some of the
;; language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; Set it to big number(100mb) like most of the popular kits like Spacemacs/Doom/Prelude do:
(setq gc-cons-threshold (* 100 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq
 package-selected-packages
 '(
   desktop-registry
   ace-window ;; for ace-swap-window
   tramp
   undo-tree
   expand-region
   smartparens
   highlight-symbol
   wrap-region
   ;;exec-path-from-shell
   lsp-mode

   ;;helm
   helm-lsp
   ;;helm-ls-git
   async ;; required by Helm, separately installed for dev setup
   
   vertico
   consult
   consult-lsp
   embark-consult
   embark
   marginalia
   orderless
   
   ;;corfu

   ;;lsp-ui
   ;; Dart
   dart-mode
   lsp-dart
   ;;flutter
   
   company
   which-key
   js2-mode
   json-mode
   web-beautify
   imenu
   multiple-cursors
   cmake-mode
   ssass-mode
   flycheck
   projectile
   yasnippet
   realgud
   
   electric-case
   string-inflection
   gn-mode

   elpy
   ;;jedi
   direnv
   envrc
   meson-mode
   ninja-mode

   go-mode
   ccls

   lua-mode
   clang-format

   ;; web-mode
   ;; web-mode-edit-element
   yaml-mode
   php-mode
   magit
   macrostep
   ini-mode
   ggtags
   bash-completion

   dired-toggle-sudo
   dired-filter
   casual-suite

   slime
   ;;helm-slime
   ;;sly
   ;;sly-asdf

   rust-mode
   ;;; or
   ;;rustic
   ;;cargo

   ;; geiser
   ;; geiser-guile
   ;; macrostep-geiser

   git-timemachine

   ;;dirvish

   d-mode

;;; themes
   dracula-theme
   standard-themes
   spacemacs-theme
   humanoid-themes
   doom-themes
   pastelmac-theme
   
   ))

(package-initialize)

;;-------------------------------------------------------
;;-------------------------------------------------------
;;-------------------------------------------------------

(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c C-h") 'hl-line-mode)
(global-set-key (kbd "<f6>") 'revert-buffer)
(global-set-key (kbd "C-x b") 'bs-show)
(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "M-o") 'ace-swap-window)
(global-set-key (kbd "M-p") 'scroll-up-line)
(global-set-key (kbd "M-n") 'scroll-down-line)
(global-set-key (kbd "C-c r") 'rgrep)
;;-------------------------------------------------------
;;-------------------------------------------------------
;;-------------------------------------------------------

(when (display-graphic-p)  
  
  (toggle-frame-maximized) ;; maximize Emacs
  (setq-default frame-title-format "%b (%f)")

  ;;(set-face-attribute 'default nil :font "Hack" :height 105)
  (set-face-attribute 'default nil :font "UbuntuMono Nerd Font Mono" :height 120)
  ;;(set-face-attribute 'default nil :font "Liberation Mono" :height 105)

  (tool-bar-mode -1)                   ; disable toolbar
  (line-number-mode)                   ; show line numbers in modeline
  (menu-bar-mode -1)                   ; disable menu
  (scroll-bar-mode -1)                 ; disable scrollbars
  (column-number-mode)
  (setq ring-bell-function 'ignore)
  
  ;; Path to Emacs C source, for functions help system
  ;;(setq find-function-C-source-directory "~/path/to/emacs/src")

  ;; check package archives cache
  (unless package-archive-contents
    (package-refresh-contents))

  (require 'compile)                    ; for 'recompile'

  (mapcar                               ; install selected packages
   (lambda (pkg)
     (when (not (package-installed-p pkg))
       (package-install pkg)))
   package-selected-packages))

(setq
 create-lockfiles nil
 completion-ignore-case t
 make-backup-files nil
 inhibit-startup-screen t ; no startup screen
 resize-mini-windows t
 ;;mode-line-compact t
 )
(setq-default indent-tabs-mode nil)   ; use spaces

(recentf-mode)
;;(electric-pair-mode)

(defun ak-killbuf ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-`") 'ak-killbuf)

(setq tab-always-indent 'complete)

(custom-set-variables
 '(show-paren-when-point-in-periphery nil)
 '(show-paren-when-point-inside-paren t)
 ;;'(show-paren-style 'mixed)
 )
(add-hook 'prog-mode-hook
          (lambda ()
            (show-paren-mode)
            ))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (smartparens-strict-mode)
            ))

(defmacro conf (name &rest init-code)
  (declare (indent defun))
  (when (member name package-selected-packages)
    `(if (package-installed-p ',name)
         (progn ,@init-code)
       (message "EMACS PACKAGES, %s not installed" ',name))))


;;----------------------------------------------------
;; (conf exec-path-from-shell
;;   (when (memq window-system '(mac ns x))
;;     (exec-path-from-shell-initialize)
;;     (exec-path-from-shell-copy-env "LSP_USE_PLISTS")
;;     ))


;;----------------------------------------------------
(conf dart-mode
  (add-hook 'dart-mode-hook
            (lambda ()
              (require 'electric-case)
              (electric-case-mode)
              (subword-mode)
              (smartparens-strict-mode)
              
              ;; important, dart server hangs w/o it
              ;; (projectile-mode +1)
              ;; (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")

              ;; (add-hook 'eglot-managed-mode-hook
              ;;           (lambda ()
              ;;             (eglot-inlay-hints-mode -1)) nil
              ;;           t ;; local
              ;;           )
              ;; (eglot-ensure)

              (lsp-deferred)
              (setq
               lsp-ui-doc-show-with-mouse nil
               )
              (define-key dart-mode-map (kbd "s-l d d") #'lsp-ui-doc-show)
              (define-key dart-mode-map (kbd "s-l d f") #'lsp-ui-doc-focus-frame)
              (define-key dart-mode-map (kbd "M-.") #'lsp-find-definition)
              (define-key dart-mode-map (kbd "C-c C-u") #'string-inflection-java-style-cycle)

              ))
  )


(conf lsp-mode
  (add-hook 'lsp-mode-hook
            (lambda ()
              (company-mode -1)
              ;;(corfu-mode)
              ;; (setq
              ;;  corfu-auto-prefix 2
              ;;  )
              ))
  )


;;----------------------------------------------------
(conf go-mode

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
              ;; (ggtags-mode 1)
              ;; (define-key go-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)
              ;; (define-key go-mode-map (kbd "C-,") #'ggtags-prev-mark)
              (subword-mode)
              (abbrev-mode 1)

              ;; (eglot-ensure)
              ;; (define-key go-mode-map (kbd "C-c C-u") #'string-inflection-java-style-cycle)

              (lsp-deferred)
              ;; requires GO111MODULE=on go get golang.org/x/tools/gopls@latest
              (setq lsp-ui-doc-show-with-mouse nil)

              ;; Bindings in  go-goto-map
              ;; (define-key m "a" #'go-goto-arguments)
              ;; (define-key m "d" #'go-goto-docstring)
              ;; (define-key m "f" #'go-goto-function)
              ;; (define-key m "i" #'go-goto-imports)
              ;; (define-key go-goto-map "i" #'ak-go-goto-imports)
              ;; (define-key m "m" #'go-goto-method-receiver)
              ;; (define-key m "n" #'go-goto-function-name)
              ;; (define-key m "r" #'go-goto-return-values)
              ))

  (defun ak-go-goto-imports ()
    "Uses xref mark ring to return back from imports section, when quick (un)comment of import is done."
    (interactive)
    (xref-push-marker-stack)
    (go-goto-imports))

;;; go assembly

  (defun ak-go-asm-comment-char ()
    (interactive)
    (setq-local comment-start "// ")))


;;----------------------------------------------------
;; Python

(autoload 'python-mode "python" "Python Mode." t) ; built-in
;;(autoload 'python-mode "python-mode" "Python Mode." t)

(envrc-global-mode) ;; buffer local direnv
(with-eval-after-load 'python
  (elpy-enable))

(defun python-mode-abbrev-debug-handler ()
  (forward-line -1)
  (funcall indent-line-function)
  (move-end-of-line 1)
  (save-buffer 0))

(defun python-mode-func ()
  (setq tab-width 4)
  (hs-minor-mode)
  (abbrev-mode 1)
  (wrap-region-mode 1)
  ;; for python block comments
  (wrap-region-add-wrapper "\"\"\"" "\"\"\"" "\\")
  
  ;; (jedi:setup)
  ;; (setq jedi:complete-on-dot t) ; optional
  ;; (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
  ;; (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (lsp-deferred)
  )
(add-hook 'python-mode-hook 'python-mode-func)

;;----------------------------------------------------
;;; eglot

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                `((dart-mode dart-ts-mode) .
;;                  ("/opt/dart-sdk/bin/dart"
;;                   "language-server" "--client-id" "emacs.eglot-dart"
;;                   ;; "--packages"
;;                   ;; ;; set in .emacs
;;                   ;; ,dart-project-package-config-path
;;                   )))
;;   (add-to-list 'eglot-server-programs
;;                '(d-mode . ("serve-d"))
;;                ;;'(python-mode . ("pyright-langserver"))
;;                ))


;;;;; Configure packages

(conf smartparens
  (add-hook 'smartparens-mode-hook
            (lambda ()
              ;; ("C-M-f" . sp-forward-sexp)
              ;; ("C-M-b" . sp-backward-sexp)
              ;; ("C-M-d" . sp-down-sexp)
              ;; ("C-M-a" . sp-backward-down-sexp)
              ;; ("C-S-d" . sp-beginning-of-sexp)
              ;; ("C-S-a" . sp-end-of-sexp)
              ;; ("C-M-e" . sp-up-sexp)
              ;; ("C-M-u" . sp-backward-up-sexp)
              ;; ("C-M-n" . sp-next-sexp)
              ;; ("C-M-p" . sp-previous-sexp)
              ;; ("C-M-k" . sp-kill-sexp)
              ;; ("C-M-w" . sp-copy-sexp)
              ;; ("M-<delete>" . sp-unwrap-sexp)
              ;; ("M-<backspace>" . sp-backward-unwrap-sexp)
              ;; ("C-<right>" . sp-forward-slurp-sexp)
              ;; ("C-<left>" . sp-forward-barf-sexp)
              ;; ("C-M-<left>" . sp-backward-slurp-sexp)
              ;; ("C-M-<right>" . sp-backward-barf-sexp)
              ;; ("M-D" . sp-splice-sexp)
              ;; ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ;; ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ;; ("C-S-<backspace>" . sp-splice-sexp-killing-around)
              ;; ("C-]" . sp-select-next-thing-exchange)
              ;; ("C-M-]" . sp-select-next-thing)
              ;; ("C-M-SPC" . sp-mark-sexp)
              ;; ("M-F" . sp-forward-symbol)
              ;; ("M-B" . sp-backward-symbol)
              (sp-use-smartparens-bindings)
              (define-key smartparens-mode-map (kbd "M-<backspace>") #'sp-backward-delete-word)
              (define-key smartparens-mode-map (kbd "C-{") #'sp-select-previous-thing)
              (global-set-key (kbd "C-k") 'sp-kill-hybrid-sexp)
              (sp-local-pair '(emacs-lisp-mode) "'" nil :actions nil)
              ))
  )

(conf d-mode
  (add-hook 'd-mode-hook
            (lambda ()
              ;;(eglot-ensure)
              ))
  )

(conf casual-suite
  (require 'casual-suite)
  (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
  (keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
  ;;(keymap-set calc-mode-map "C-o" #'casual-calc-tmenu)
  ;; (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)
  ;; (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
  ;; (keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
  ;; (keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)
  ;; (keymap-set reb-mode-map "C-o" #'casual-re-builder-tmenu)
  ;; (keymap-set reb-lisp-mode-map "C-o" #'casual-re-builder-tmenu)
  ;; (keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
  ;; (keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
  ;; (keymap-global-set "M-g" #'casual-avy-tmenu)
  ;; (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)
  ;; (keymap-global-set "C-o" #'casual-editkit-main-tmenu)
  )

(conf which-key
  (which-key-mode))

(conf undo-tree
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo)
  )

(conf desktop-registry
  ;; some chunks from desktop+ to handle *shell* buf 
  
  (defun desktop-restore-shell (file-name buffer-name misc)
    (let* ((dir (plist-get misc :dir))
           (default-directory (if (file-directory-p dir) dir "/")))
      (with-current-buffer (shell)
        (rename-buffer buffer-name))))

  ;; restore *shell* buffer
  (add-hook 'shell-mode-hook
            (lambda ()
              (setq desktop-save-buffer
                    (lambda (dirname)
                      (list :dir default-directory)))))
  (add-to-list
   'desktop-buffer-mode-handlers
   '(shell-mode . desktop-restore-shell))
  
  (global-set-key (kbd "C-c d s") 'desktop-save-in-desktop-dir)
  (global-set-key (kbd "C-c d r") 'desktop-registry-change-desktop))

(conf yasnippet
  (yas-global-mode))


;;----------------------------------------------------
;; rust

;; rustup component add rls rust-analysis rust-src
;; or
;; rustup component add --toolchain "1.42.0-x86_64-unknown-linux-gnu" rls rust-analysis rust-src

(conf rust-mode
  (add-hook 'rust-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (define-mode-abbrev "pnl" "println!(\"{:?}\",  );")
              (setq
               indent-tabs-mode nil
               rust-format-on-save t
               lsp-ui-sideline-enable nil
               )
              (lsp-deferred)
              
              )))

(conf lua-mode
  (add-hook 'lua-mode-hook
            (lambda ()
              (lsp-deferred)
              )))

(conf tramp
  (add-hook 'tramp--startup-hook
            (lambda ()
              (progn
                ;;(setq auth-source-debug t)
                (setq-default password-cache-expiry 604800) ; seconds
                (setq tramp-default-method "ssh")
                ;;(setenv "SHELL" "/bin/bash")
                (tramp-set-completion-function
                 "ssh"
                 '((tramp-parse-sconfig "/etc/ssh/ssh_config")
                   (tramp-parse-sconfig "~/.ssh/config")))))))

(conf js2-mode
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  (defun abbrev-console-log ()
    (backward-char 2) t)
  (put 'abbrev-console-log 'no-self-insert t)

  (add-hook 'js2-mode-hook
            (lambda ()
              (setq tab-width 4)
              (abbrev-mode 1)
              (rainbow-mode 1)
              (js2-imenu-extras-mode)
              (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)
              (define-abbrev js-mode-abbrev-table  "cnl" "console.log();"
                'abbrev-console-log)
              (setq js-indent-level 2)
              )))

(conf web-beautify
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
  )

(conf multiple-cursors
  (global-set-key (kbd "C-;") 'mc/mark-all-dwim)
  )

(conf lsp-ui
  (setq
   lsp-headerline-breadcrumb-icons-enable nil
   lsp-headerline-breadcrumb-enable-diagnostics nil
   ))

;;; sass|scss
(conf ssass-mode
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.sass$" . ssass-mode))
  (eval-after-load 'scss-mode '(define-key scss-mode-map (kbd "C-c b") 'web-beautify-css))

  (add-hook 'ssass-mode-hook
            (lambda ()
              (rainbow-mode t)
              )))

(conf highlight-symbol
  (global-set-key (kbd "C-<f3>") 'highlight-symbol)
  (global-set-key (kbd  "<f3>") 'highlight-symbol-next)
  (global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
  (global-set-key (kbd "M-<f3>") 'highlight-symbol-query-replace)
  )

(conf wrap-region
  )

(conf web-mode
  (add-to-list 'auto-mode-alist '("\\.qtpl$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

  (eval-after-load 'web-mode
    '(progn
       (define-key web-mode-map (kbd "C-c-n") 'web-mode-element-next)
       (define-key web-mode-map (kbd "C-c-p") 'web-mode-element-previous)
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
  (global-set-key (kbd "<f12>") 'web-mode))

(conf web-mode-edit-element)

(conf yaml-mode
  (add-to-list 'auto-mode-alist '("\\.kv$"  . yaml-mode))
  (add-hook 'yaml-mode-hook
            (lambda ()
              (setq yaml-indent-offset 2))))

(conf php-mode
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
              (define-key php-mode-map (kbd "C-c i") 'imenu)
              (define-key php-mode-map (kbd "C-c C-r") 'revert-buffer)
              ))
  (add-to-list 'auto-mode-alist '("\\.php$"  . php-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.php$"  . web-mode))
  (global-set-key (kbd "<f11>") 'php-mode)
  (global-set-key (kbd "<f12>") 'web-mode))


(conf magit
  (global-set-key (kbd "C-c m") 'magit-status))

(conf macrostep
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

(conf ini-mode
  (add-to-list 'auto-mode-alist '("\\.ini$"  . ini-mode))
  (add-to-list 'auto-mode-alist '("\\.service\\|\\.target$"  . ini-mode)) ; Systemd
  (add-to-list 'auto-mode-alist '("PKGBUILD$"  . ini-mode)) ; Arch package file
  )

(conf symbol-overlay
  (global-set-key [(control f9)] 'symbol-overlay-put)
  (global-set-key [f9] 'symbol-overlay-jump-next)
  (global-set-key [(shift f9)] 'symbol-overlay-jump-next)
  (global-set-key [(meta f9)] 'symbol-overlay-query-replace)
  )

(conf glsl-mode
  (add-to-list 'auto-mode-alist '("\\.comp$" . glsl-mode)))

(conf doom-modeline
  ;; leftmost part:
  ;;(setq doom-modeline-bar-width 0)
  (doom-modeline-mode 1))

;;----------------------------------------------------
;; helm

;; ;; dev
;; (add-to-list 'load-path (expand-file-name "~/helm"))
;; (require 'helm)
;; (require 'helm-autoloads)
;; (require 'helm-buffers)

;; (
;;  ;;conf helm
;;  progn
;;   (helm-mode)
  
;;   (global-set-key (kbd "M-x") 'helm-M-x)
;;   (global-set-key (kbd "C-c j") 'helm-mini)
;;   (global-set-key (kbd "C-c C-j") #'helm-command-prefix)
;;   (global-set-key (kbd "C-x C-f") #'helm-find-files)
;;   (global-set-key (kbd "C-c C-s") #'helm-toggle-full-frame)
;;   (global-set-key (kbd "C-c l") #'helm-occur)
;;   (global-set-key (kbd "C-c i") #'helm-imenu)
;;   (define-key helm-command-map (kbd "g") #'helm-browse-project) ; uses 'helm-ls-git
;;   (setq
;;    helm-echo-input-in-header-line t
;;    helm-M-x-fuzzy-match nil
;;    helm-imenu-fuzzy-match t
;;    helm-move-to-line-cycle-in-source nil
;;    helm-truncate-lines t

;;    ;;completion-styles '(orderless basic)
;;    ;;helm-completion-style 'emacs
;;    ;; helm-completion-style-alist
;;    ;; '((default . orderless)
;;    ;;   (file . orderless)
;;    ;;   (buffer . orderless)
;;    ;;   (symbol . orderless))
   
;;    helm-always-two-windows nil
;;    helm-split-window-default-side 'other
;;    ;; for toggling on/off Helm full frame
;;    helm-split-window-other-side-when-one-window 'right

;;    helm-update-edebug t ;; dev
;;    ))


(conf expand-region
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region)
  )

;;----------------------------------------------------
;; vertico consult orderless embark marginalia corfu

(conf vertico
  ;;(vertico-mode)
  ;;(vertico-multiform-mode)
  (vertico-buffer-mode)
  (define-key vertico-map (kbd "C-o") #'vertico-next-group)
  (setq
   vertico-count 16
   vertico-cycle nil
   )
  ;; show code completion candidates in vertico
  (setq completion-in-region-function #'consult-completion-in-region)
  )

(conf consult
  
  (global-set-key (kbd "C-c j") 'consult-buffer)
  (global-set-key (kbd "C-c C-j") 'consult-buffer)
  (global-set-key (kbd "C-c i") 'consult-imenu)
  (global-set-key (kbd "C-c l") 'consult-line) ; similar to swiper

  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-modified-buffer
          consult--source-buffer
          ;;consult--source-recent-file
          consult--source-file-register
          ;;consult--source-bookmark
          consult--source-project-buffer-hidden
          consult--source-project-recent-file-hidden
          ))

  ;; for 'consult-customize' which is not autoloaded
  (require 'consult)
  
  ;;;; remove Dired buffers from common completions, they are grouped separately
  (consult-customize
   consult--source-buffer
   :items (lambda ()
            (consult--buffer-query
             :sort 'visibility :as #'buffer-name
             ;; Buffers excluding Dired
             :predicate
             (lambda (buf) (not (eq (buffer-local-value 'major-mode buf)
                                    'dired-mode))))))

  ;;;; Dired buffers group
  (add-to-list
   'consult-buffer-sources
   (list :name "Dired"
         :category 'buffer
         :narrow ?d
         :face 'consult-buffer
         :items (lambda () (consult--buffer-query
                            :sort 'visibility :as #'buffer-name
                            :predicate
                            (lambda (buf) (eq (buffer-local-value 'major-mode buf)
                                              'dired-mode))))
         :state #'consult--buffer-preview
         :action #'consult--buffer-action
         )
   'append)
  )

(conf orderless
  (setq
   completion-styles '(orderless basic)
   ;;completion-styles '(basic hotfuzz)
   ;;completion-styles '(hotfuzz orderless)
   completion-category-overrides '((file (styles basic partial-completion)))
   ))

(conf embark
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "C-'") 'embark-dwim)
  (global-set-key (kbd "C-h b") 'embark-bindings)
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))
  )

(conf marginalia
  (marginalia-mode))

(conf corfu
  (global-corfu-mode))


;;----------------------------------------------------
;; counsel, ivy, swiper, prescient

(conf counsel
  (ivy-mode)
  (ivy-prescient-mode)
  (counsel-mode)
  ;;(ivy-rich-mode)

  (setq
   ;; ivy-use-virtual-buffers t
   enable-recursive-minibuffers t
   ivy-use-selectable-prompt t
   ivy-wrap t
   ivy-re-builders-alist '((t . ivy--regex-ignore-order))
   ivy-fixed-height-minibuffer t
   ivy-height 21
   ;;ibuffer-always-show-last-buffer
   counsel-find-file-at-point t)

  (global-set-key (kbd "C-c l") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c i") 'counsel-imenu)
  (global-set-key (kbd "C-c f") 'counsel-recentf)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-x j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

  (global-set-key (kbd "C-c j") 'counsel-switch-buffer)
  (global-set-key (kbd "C-c C-j") 'counsel-switch-buffer)
  ;;(global-set-key (kbd "C-c C-j") 'ivy-ibuffer)
  )

;;----------------------------------------------------
;;; ninja, *.gn and *.gni

(conf gn-mode
  (add-to-list 'auto-mode-alist '("\\.\\(gn\\|gni\\)$" . gn-mode))
  )

;;----------------------------------------------------
;;; shell mode
(add-hook 'shell-mode-hook #'bash-completion-setup)

;;----------------------------------------------------
;;; markdown-mode
(add-to-list 'auto-mode-alist '("\\.mm$" . markdown-mode))


;;----------------------------------------------------
;;; xml

;;(add-to-list 'auto-mode-alist '("\\.xml$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"  . web-mode))

;;----------------------------------------------------
;;; css

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode t)))

;;----------------------------------------------------
;;; Scheme mode

(add-hook
 'scheme-mode-hook
 (lambda ()
   (smartparens-strict-mode)
   (abbrev-mode 1)
   (define-abbrev scheme-mode-abbrev-table  "nl" "(newline)")
   (define-abbrev scheme-mode-abbrev-table  "dl" "(display )")

   ;; added `define*' to scheme mode
   ;; added `define-syntax-rule' for macros
   (setq scheme-imenu-generic-expression
         '((nil
            "^(define\\(\\|\\*\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
           ("Types"
            "^(define-class\\s-+(?\\(\\sw+\\)" 1)
           ("Macros"
            "^(\\(defmacro\\|define-macro\\|define-syntax\\|define-syntax-rule\\)\\s-+(?\\(\\sw+\\)" 2)))

   (macrostep-geiser-setup)))

;;----------------------------------------------------
;;; c mode

(add-hook 'c-mode-hook
          (lambda ()
            ;; gnu, k&r, bsd, stroustrup, whitesmith, ellemtel, linux, python, java, awk
            ;;(c-set-style "python")
            (setq-default indent-tabs-mode nil)
            (setq
             tab-width 4
             c-default-style "bsd"
             c-basic-offset 2
             )
            ;;(setq c-macro-preprocessor "cpp -CC")
            ;;(preproc-font-lock-mode 1)
            (hs-minor-mode 1) ;; hide/show blocks
            (define-key c-mode-map "\C-c\C-f" 'ff-find-other-file)
            (define-key c++-mode-map "\C-c\C-f" 'ff-find-other-file)

            ;;(lsp-deferred)
            ;; (define-key c-mode-map (kbd "M-.") 'lsp-find-definition)
            (ggtags-mode)

            (smartparens-strict-mode)
            ;;(company-mode)
            (electric-indent-local-mode 1)
            (c-toggle-comment-style -1) ;; line comments not blocks
            
            ;; disable auto-align of endline backslashes in multiline macros
            ;;(setq c-auto-align-backslashes nil)
            (abbrev-mode 1)
            (define-abbrev c-mode-abbrev-table "err" "#error \"stop here\"")
            ))

;;----------------------------------------------------
;;; c++ mode

(add-hook 'c++-mode-hook
          (lambda ()
            (ggtags-mode)
            ;; mask a key binding of ggtags minor mode
            (let ((oldmap (cdr (assoc 'ggtags-mode minor-mode-map-alist)))
                  (newmap (make-sparse-keymap)))
              (set-keymap-parent newmap oldmap)
              (make-local-variable 'minor-mode-overriding-map-alist)
              (push `(ggtags-mode . ,newmap) minor-mode-overriding-map-alist))
            ))

;;----------------------------------------------------
;; slime
;; building SBCL's ./doc/manual requires 'texinfo-plaingeneric' OS package

(conf slime
;;; quicklisp, misc config is in ~/.sbclrc
  (add-to-list 'slime-contribs 'slime-asdf)
  ;;(slime-setup)
  (add-hook
   'lisp-mode-hook
   (lambda ()
     (smartparens-strict-mode)
     (setq
      inferior-lisp-program (expand-file-name "/usr/bin/sbcl")
      browse-url-browser-function 'eww-browse-url
      common-lisp-hyperspec-root
      "file:///usr/share/doc/common-lisp-hyperspec/HyperSpec/"
      )
     (setq slime-lisp-implementations
           '((sbcl ("sbcl") :coding-system utf-8-unix)))
     )))


;;----------------------------------------------------
;; sly

(conf sly
  (setq
   ;;inferior-lisp-program (expand-file-name "/usr/bin/sbcl")
   sly-lisp-implementations '((sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix))
   browse-url-browser-function 'eww-browse-url
   common-lisp-hyperspec-root
        "file:///usr/share/doc/common-lisp-hyperspec/HyperSpec/"
        )
  (add-hook 'sly-mode-hook
            (lambda ()
              (smartparens-strict-mode)
              (define-key sly-mode-map (kbd "C-c i") 'imenu)
              ))
  )

;;;;;----------------------------------------------------
;;; selected packages
;;;;;----------------------------------------------------

;;; LSP optimization start
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")
;; Set it to big number(100mb) like most of the popular kits like Spacemacs/Doom/Prelude do:
(setq gc-cons-threshold 100000000)
;; Again the emacs default is too low 4k considering that the some of the
;; language server responses are in 800k - 3M range.
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;;; LSP optimization end
;;; doom-modeline recommendation
;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq
 package-selected-packages
 '(
   desktop-registry
   tramp
   undo-tree
   evil ; for windows movement

   ;; helm
   ;; helm-ls-git
   async ; required by Helm, separately installed for dev setup

   ;; vertico
   ;; consult
   ;; orderless
   ;; embark-consult
   ;; embark
   ;; corfu
   ;; marginalia

   ;; counsel
   ;; hydra
   ;; ivy-hydra
   ;; ivy-prescient
   ;; ivy-rich

   company
   which-key
   js2-mode
   tern ; js code analysis
   json-mode
   web-beautify
   imenu
   multiple-cursors
   cmake-mode
   ssass-mode
   smartparens
   flycheck
   projectile
   yasnippet
   realgud

   lsp-ui
   ;; Dart
   dart-mode
   lsp-dart
   flutter
   electric-case
   string-inflection

   jedi

   go-mode
   ;;company-go
   ccls
   flycheck-irony

   lua-mode

   web-mode
   web-mode-edit-element
   yaml-mode
   php-mode
   paredit
   magit
   ghub
   macrostep
   ini-mode
   symbol-overlay
   ggtags
   dired-toggle-sudo
   bash-completion

   slime

   ;;rust-mode
   ;; or
   ;; rustic
   ;; cargo

   geiser
   geiser-guile
   macrostep-geiser

   doom-modeline
   git-timemachine

   ;;dirvish
   ;;treemacs

   ;; themes

   ;; avk-emacs-themes
   ;; moe-theme
   dakrone-light-theme
   pastelmac-theme

   ))

(package-initialize)

(when (display-graphic-p)
  (toggle-frame-maximized) ; maximize Emacs
  (setq-default frame-title-format "%b (%f)")

  (set-face-attribute 'default nil :font "Consolas" :height 113)
  ;;(set-face-attribute 'default nil :font "UbuntuMono Nerd Font Mono" :height 120)
  ;;(set-face-attribute 'default nil :font "Liberation Mono" :height 105)

  ;; test vary width font
  ;;(set-face-attribute 'default nil :font "DejaVu Sans" :height 110)

  (tool-bar-mode -1)   ; disable toolbar
  (line-number-mode)   ; show line numbers in modeline
  (menu-bar-mode -1)   ; disable menu
  (scroll-bar-mode -1) ; disable scrollbars
  (setq-default indent-tabs-mode nil) ; use spaces
  (setq make-backup-files nil)
  (setq inhibit-startup-screen t) ; no startup screen

  ;; Path to Emacs C source, for functions help system
  ;;(setq find-function-C-source-directory "~/path/to/emacs/src")

  ;; check package archives cache
  (unless package-archive-contents
    (package-refresh-contents))

  (require 'compile) ; for M-x recompile

  (mapcar ;; install selected packages
   (lambda (pkg)
     (when (not (package-installed-p pkg))
       (package-install pkg)))
   package-selected-packages))

(setq create-lockfiles nil)
(global-set-key (kbd "C-c c") 'comment-region)

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(show-paren-mode)
(column-number-mode)
(recentf-mode)
(global-set-key (kbd "C-c C-h") 'hl-line-mode)
(global-set-key (kbd "<f6>") 'revert-buffer)

(global-set-key (kbd "C-x b") 'bs-show)

(defmacro conf (name &rest init-code)
  (declare (indent defun))
  (when (member name package-selected-packages)
    `(if (package-installed-p ',name)
         (progn ,@init-code)
       (message "EMACS PACKAGES, %s not installed" ',name))))


;;;;; Configure packages

(conf which-key
  (which-key-mode))


(conf evil
  (evil-mode 1)
  (setq evil-undo-system 'undo-tree
        evil-want-c-w-delete nil)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-backward-char)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-forward-char)
  (define-key evil-insert-state-map (kbd "C-k") 'evil-previous-line)
  (define-key evil-insert-state-map (kbd "C-j") 'evil-next-line)
  (define-key evil-insert-state-map (kbd "C-f") 'indent-for-tab-command)
 
  (define-key evil-command-line-map (kbd "C-h") 'evil-backward-char)
  (define-key evil-command-line-map (kbd "C-l") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  (define-key evil-command-line-map (kbd ";") 'evil-ex)
  )


(conf undo-tree
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil)
  ;; (global-set-key (kbd "C-/") 'undo)
  ;; (global-set-key (kbd "C-M-/") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "u") 'undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  )


(conf desktop-registry
  (global-set-key (kbd "C-c d s") 'desktop-save-in-desktop-dir)
  (global-set-key (kbd "C-c d r") 'desktop-registry-change-desktop))


(conf yasnippet
  (yas-global-mode))

(conf electric-case)

(conf string-inflection)

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

              (eglot-ensure)
              ;;(define-key go-mode-map (kbd "M-.") #'xref-find-definitions)
              ;;(define-key go-mode-map (kbd "C-c C-u") #'string-inflection-java-style-cycle)

              ;; (lsp-deferred)
              ;; ;; requires GO111MODULE=on go get golang.org/x/tools/gopls@latest
              ;; (define-key go-mode-map (kbd "M-.") #'lsp-find-definition)
              ;; (setq lsp-ui-doc-show-with-mouse nil)

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

(conf tramp
  (add-hook 'tramp--startup-hook
            (lambda ()
              (progn
                ;;(setq auth-source-debug t)
                (setq-default password-cache-expiry 604800) ; seconds
                (setq tramp-default-method "ssh")
                (setenv "SHELL" "/bin/bash")
                (tramp-set-completion-function
                 "ssh"
                 '((tramp-parse-sconfig "/etc/ssh/ssh_config")
                   (tramp-parse-sconfig "~/.ssh/config")))))))


(conf js2-mode
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (conf tern)
  ;; npm install tern
  (setq tern-command (list (expand-file-name "~/node_modules/tern/bin/tern")))

  (defun abbrev-console-log ()
    (backward-char 2) t)
  (put 'abbrev-console-log 'no-self-insert t)

  (add-hook 'js2-mode-hook
            (lambda ()
              (setq tab-width 4)
              (abbrev-mode 1)
              (rainbow-mode 1)
              (tern-mode 1)
              ;;(company-mode 1)
              ;;(add-to-list 'company-backends 'company-tern)
              (js2-imenu-extras-mode)
              (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)
              (define-abbrev js-mode-abbrev-table  "cnl" "console.log();"
                'abbrev-console-log)
              (setq js-indent-level 2)
              ))

  ;;; JSON
  (add-to-list 'auto-mode-alist '("\\.tern-project$" . json-mode)))

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

(conf json-mode)

(conf imenu)

(conf multiple-cursors
  (global-set-key (kbd "C-;") 'mc/mark-all-dwim))

(conf cmake-mode)

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

(conf smartparens)

(conf projectile)

(conf highlight-indentation)

(conf flycheck-irony)

(conf lua-mode)

(conf web-mode
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
              ;;(define-key web-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
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
              (semantic-mode 1)
              (hs-minor-mode 1)
              (local-unset-key "C-M-\\")
              (local-unset-key "C")
              (c-set-style "symfony2")
              (setq indent-tabs-mode t
                    tab-width 4
                    c-basic-offset 4)
              (define-key php-mode-map (kbd "C-c i") 'imenu)
              (define-key php-mode-map (kbd "C-c C-r") 'revert-buffer)
              ;;(define-key php-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
              ))
  (add-to-list 'auto-mode-alist '("\\.php$"  . php-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.php$"  . web-mode))
  (global-set-key (kbd "<f11>") 'php-mode)
  (global-set-key (kbd "<f12>") 'web-mode))

;; (conf paredit
;;   (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(conf magit
  (global-set-key (kbd "C-c m") 'magit-status))

(conf ghub
  )

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

(conf ggtags)

(conf glsl-mode
  (add-to-list 'auto-mode-alist '("\\.comp$" . glsl-mode)))

;; (conf company
;;   (global-set-key (kbd "M-n") #'company-complete)
;;   (company-mode))

(conf git-timemachine)

(conf doom-modeline
  ;; leftmost part:
  ;;(setq doom-modeline-bar-width 0)
  (doom-modeline-mode 1))

;;----------------------------------------------------
;; helm

;; dev
(add-to-list 'load-path (expand-file-name "~/helm"))
(require 'helm)
(require 'helm-autoloads)
(require 'helm-buffers)

(progn ;;conf helm
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c j") 'helm-mini)
  (global-set-key (kbd "C-c C-j") #'helm-command-prefix)
  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-c C-s") #'helm-toggle-full-frame)
  (global-set-key (kbd "M-l") #'helm-occur)
  (global-set-key (kbd "C-c i") #'helm-imenu)
  (define-key helm-command-map (kbd "g") #'helm-browse-project) ; uses 'helm-ls-git
  (setq
   helm-echo-input-in-header-line t
   helm-M-x-fuzzy-match nil

   helm-always-two-windows nil
   helm-split-window-default-side 'other
   ;; for toggling on/off Helm full frame
   helm-split-window-other-side-when-one-window 'right

   helm-update-edebug t ; dev
   ))


;;----------------------------------------------------
;; vertico consult orderless embark marginalia corfu

(conf vertico
  (vertico-mode)
  (vertico-buffer-mode)
  (define-key vertico-map (kbd "C-o") #'vertico-next-group)
  (setq vertico-cycle t
        ))

(conf consult
  (global-set-key (kbd "C-c j") 'consult-buffer)
  (global-set-key (kbd "C-c C-j") 'consult-buffer)
  (global-set-key (kbd "C-c i") 'consult-imenu)
  (global-set-key (kbd "M-l") 'consult-line) ; similar to swiper

  (require 'consult)

  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-modified-buffer
          consult--source-buffer
          consult--source-recent-file ; expands common column width on long paths
          consult--source-file-register
          consult--source-bookmark
          consult--source-project-buffer-hidden
          consult--source-project-recent-file-hidden
          ))

  ;; remove Dired buffers from common completions, they are grouped separately
  (consult-customize
   consult--source-buffer
   :items (lambda ()
            (consult--buffer-query
             :sort 'visibility :as #'buffer-name
             ;; Buffers excluding Dired
             :predicate
             (lambda (buf) (not (eq (buffer-local-value 'major-mode buf)
                                   'dired-mode))))))

  ;; Dired buffers group
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
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

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
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

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

  (global-set-key (kbd "M-l") 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
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
;;; dart

(conf dart-mode
  (add-hook 'dart-mode-hook
            (lambda ()
              (require 'electric-case)
              (electric-case-mode)
              (smartparens-strict-mode)
              (subword-mode)

              (projectile-mode +1)
              (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
              (add-to-list 'projectile-project-root-files-bottom-up "BUILD")
              (lsp-deferred)
              ;;(define-key dart-mode-map (kbd "M-.") #'lsp-find-definition)
              ;;(setq lsp-ui-doc-show-with-mouse nil)
              (define-key dart-mode-map (kbd "s-l d d") #'lsp-ui-doc-show)

              (define-key dart-mode-map (kbd "s-l d f") #'lsp-ui-doc-focus-frame)

              (define-key dart-mode-map (kbd "C-M-x") #'flutter-run-or-hot-reload)
              (define-key dart-mode-map (kbd "C-c C-u") #'string-inflection-java-style-cycle)
              )))

;;----------------------------------------------------
;;; shell mode
(add-hook 'shell-mode-hook #'bash-completion-setup)

;;----------------------------------------------------
;;; makefile-mode

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq tab-width 4)))

;;----------------------------------------------------
;;; markdown-mode

(add-to-list 'auto-mode-alist '("\\.mm$" . markdown-mode))

;;----------------------------------------------------

(autoload 'python-mode "python" "Python Mode." t) ; built-in
;;(autoload 'python-mode "python-mode" "Python Mode." t)

(defun python-mode-abbrev-debug-handler ()
  (forward-line -1)
  (funcall indent-line-function)
  (move-end-of-line 1)
  (save-buffer 0))

(conf cython-mode)

(defun python-mode-func ()
  (setq tab-width 4)
  (hs-minor-mode)
  (abbrev-mode 1)
  (semantic-mode 1)
  (define-abbrev python-mode-abbrev-table  "pd" "import os; 'ak'; import pdb;pdb.set_trace()"
    #'python-mode-abbrev-debug-handler)
  (define-abbrev python-mode-abbrev-table  "tr" "import os; from trepan.api import debug;debug()"
    #'python-mode-abbrev-debug-handler)
  (define-abbrev python-mode-abbrev-table  "ipd" "import os; import ipdb;ipdb.set_trace()"
    #'python-mode-abbrev-debug-handler)
  (define-abbrev python-mode-abbrev-table  "pp" "import pprint; pp=pprint.PrettyPrinter(); pp.pprint()")
  (define-key python-mode-map (kbd "C-c C-r") 'revert-buffer) ; orig is send region to python shell

  (conf jedi)
  (jedi:setup)
  (define-key python-mode-map (kbd "C-c x") 'jedi-direx:pop-to-buffer)
  ;;(setq python-shell-interpreter "ipython" python-shell-interpreter-args "-i")
  (local-unset-key (kbd "C-c !")) ; unhide flycheck
  (define-key jedi-mode-map (kbd "C-c p") #'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "C-c ,") nil) ; unhide `semantic-force-refresh'
  ;; (define-key jedi-mode-map (kbd "M-.") (lambda () (interactive)
  ;;                                         (xref-push-marker-stack)
  ;;                                         (jedi:goto-definition)))
  ;;(define-key jedi-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)
  (define-key python-mode-map (kbd "C-c i") 'imenu)

  (auto-complete-mode -1))
(add-hook 'python-mode-hook 'python-mode-func)

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
   (enable-paredit-mode)
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
            (c-set-style "gnu")
            (ggtags-mode)
            (setq c-macro-preprocessor "cpp -CC")
            ;;(conf preproc-font-lock-mode)
            ;;(preproc-font-lock-mode 1)
            (hs-minor-mode 1) ; hide/show blocks
            (define-key c-mode-map "\C-c\C-f" 'ff-find-other-file)
            (define-key c++-mode-map "\C-c\C-f" 'ff-find-other-file)
            ;; (define-key c-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
            (define-key c-mode-map (kbd "M-,") #'ggtags-prev-mark)

            (semantic-mode 1)
            ;;(lsp-deferred)

            ;; (flycheck-mode 1)
            ;; (flycheck-clang-analyzer-setup)

            (semantic-idle-breadcrumbs-mode 1)
            (c-add-style "python-new"
                         '("python"
                           (c-basic-offset . 4))
                         t)
            ;; disable auto-align of endline backslashes in multiline macros
            (setq c-auto-align-backslashes nil)
            (abbrev-mode 1)
            (define-abbrev c-mode-abbrev-table "err" "#error \"stop here\"")
            ))

;;----------------------------------------------------
;;; c++ mode

;; C++ language server
(conf ccls)

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
;; rustic-mode

;; rustup component add rls rust-analysis rust-src
;; or
;; rustup component add --toolchain "1.42.0-x86_64-unknown-linux-gnu" rls rust-analysis rust-src

;; (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))

;; ;;(setq rust-format-on-save t)
;; (add-hook 'rustic-mode-hook
;;           (lambda ()
;;             (abbrev-mode 1)
;;             (define-mode-abbrev "pnl" "println!(\"{:?}\",  );")
;;             ))

;;----------------------------------------------------
;; slime

(conf slime
  (add-hook 'lisp-mode-hook
            (lambda ()
              (enable-paredit-mode)
              (setq inferior-lisp-program "sbcl --noinform")
              ;;(setq slime-contribs '(slime-scratch slime-editing-commands))

              ;; (setq slime-lisp-implementations
              ;;       '((sbcl ("sbcl") :coding-system utf-8-unix)))
              )))

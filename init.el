;;; package --- Summary

;;; Commentary:

;;; Code:

(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(setq package-selected-packages
      '(
        slime
        slime-company
        undo-tree
        org-super-agenda
        realgud
        preproc-font-lock
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
        helm-systemd
        helm-swoop
        helm-ag
        helm-gtags
        ssass-mode
        smartparens
        rainbow-mode
        rust-mode
        cargo
        flycheck
        web-mode
        yaml-mode
        web-mode-edit-element
        racer
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
        go-autocomplete
        ggtags
        rtags ; c++, clang
        helm-rtags
        dired-toggle-sudo
        buffer-move
        bash-completion
        ascii
        apache-mode
        ace-window
        glsl-mode))

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
      (load "helm-config.el"))
  ;; else
  (package-initialize))


(when (display-graphic-p)
  ;;(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
  (toggle-frame-maximized)
  ;;(add-to-list 'default-frame-alist '(background-color . "#EEFFCC"))
  ;; selection color
  ;;(set-face-attribute 'region nil :background "#4AB0C9" :foreground "#ffffff")
  ;; Show file path in frame title
  (setq-default frame-title-format "%b (%f)")
  )

;; disable toolbar
(tool-bar-mode -1)

(line-number-mode t)

;; recursive grep
(global-set-key (kbd "C-c r") 'rgrep)

;; undo-tree
(global-undo-tree-mode 1)
(global-set-key (kbd "C-z") 'undo)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)

(setq make-backup-files nil)
;; desktop
(global-set-key (kbd "C-c d s") 'desktop-save-in-desktop-dir)
;;(global-set-key (kbd "C-c d r") 'desktop-read)
(global-set-key (kbd "C-c d r") 'desktop-registry-change-desktop)
(setq inhibit-startup-screen t)
;;(set-face-attribute 'default nil :font "Liberation Mono")
;;(set-face-attribute 'default nil :font "Ubuntu Mono")
;;(set-face-attribute 'default nil :height 124)
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
(add-hook 'emacs-lisp-mode-hook #'auto-complete-mode)
(setq-default indent-tabs-mode nil) ; use spaces

;;(global-set-key (kbd "<f8>") #'buf-move)
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
;;; slime

(setq inferior-lisp-program "/usr/bin/sbcl")
(setq slime-contribs '(slime-fancy))
(add-hook 'slime-load-hook
          (lambda ()
            (define-key slime-prefix-map (kbd "M-h") 'slime-documentation-lookup))
          )

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

(setq-default password-cache-expiry 3600) ; How many seconds passwords are cached

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
            (when (member major-mode '(js-mode php-mode))
              t)))

;;----------------------------------------------------
;;; shell mode
(add-hook 'shell-mode-hook #'bash-completion-setup)

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
;; makefile-mode

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq tab-width 4)))

;;----------------------------------------------------

;; go-mode

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

            (define-key go-mode-map (kbd "C-<tab>") #'auto-complete)
            (auto-complete-mode 1)
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

;;----------------------------------------------------
;;; linum-mode
(global-set-key (kbd "C-c l") 'linum-mode)
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
;;; c mode

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (setq c-macro-preprocessor "cpp -CC")
              (preproc-font-lock-mode 1)
              (hs-minor-mode 1)
              (hs-minor-mode) ; hide/show blocks
              (define-key c-mode-map "\C-c\C-f" 'ff-find-other-file)
              (define-key c++-mode-map "\C-c\C-f" 'ff-find-other-file)
              ;;(flycheck-mode 1)
              (ggtags-mode 1)
              (define-key c-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
              (define-key c-mode-map (kbd "M-,") #'ggtags-prev-mark)
              (semantic-mode 1)
              ;;(semantic-decoration-mode 1)
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

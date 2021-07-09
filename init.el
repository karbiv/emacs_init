
(when (display-graphic-p)
  (toggle-frame-maximized)
  (setq-default frame-title-format "%b (%f)"))

(tool-bar-mode -1) ; disable toolbar
(line-number-mode) ; show line numbers in modeline

(setq make-backup-files nil)
(setq inhibit-startup-screen t)
(set-face-attribute 'default nil :font "Ubuntu Mono")
(set-face-attribute 'default nil :height 130)
(setq ring-bell-function 'ignore) ; ignore sound notifications
(show-paren-mode)
(column-number-mode)
;; https://stackoverflow.com/questions/5738170/why-does-emacs-create-temporary-symbolic-links-for-modified-files
(setq create-lockfiles nil)
(global-set-key (kbd "C-c c") 'comment-region)

;; Path to Emacs C source, for functions help system
;;(setq find-function-C-source-directory "~/soft/emacs/src")

;; Disable menu and scrollbars
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq-default indent-tabs-mode nil) ; use spaces

;; http://stackoverflow.com/questions/7022898/emacs-autocompletion-in-emacs-lisp-mode
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(global-set-key (kbd "C-;") 'mc/mark-all-dwim)
(global-set-key (kbd "<f7>")
                (lambda () (interactive)
                  (revert-buffer t ; IGNORE-AUTO
                                 t ; NOCONFIRM
                                 t ; PRESERVE-MODES
                                 )))
;; (global-set-key (kbd "C-c o")
;;                 (lambda () (interactive)
;;                   (other-window 1)))
;; for menu key to the right of space and AltGr

(setq enable-recursive-minibuffers t)
(recentf-mode)
(global-set-key (kbd "C-c C-h") 'hl-line-mode)


;;;;;----------------------------------------------------
;;; Selected packages
;;;;;----------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defmacro conf (name &rest init-code)
  (declare (indent defun))
  `(if (package-installed-p ',name)
       (progn
         ,@init-code
         )
     (progn
       (package-install ',name))))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;;;;; Configure packages

(conf desktop-registry
  (global-set-key (kbd "C-c d s") 'desktop-save-in-desktop-dir)
  (global-set-key (kbd "C-c d r") 'desktop-registry-change-desktop))

(conf selectrum
  (selectrum-mode))

;;(init selectrum-prescient-mode
;;  (selectrum-prescient-mode))

(conf orderless
  (setq completion-styles '(orderless)))

(conf yasnippet
  (yas-global-mode))

(conf vertico
  (vertico-mode))

(conf consult
  (global-set-key (kbd "C-c j") 'consult-buffer)
  (global-set-key (kbd "C-c i") 'consult-imenu)
  (global-set-key (kbd "M-i")   'consult-line)
  (global-set-key (kbd "M-s g") 'consult-grep)
  (global-set-key (kbd "M-s G") 'consult-git-grep))

(conf marginalia
  (marginalia-mode))

(conf embark
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  (global-set-key (kbd "M-;") 'embark-act)
  (global-set-key (kbd "C-;") 'embark-dwim)
  (global-set-key (kbd "C-h B") 'embark-bindings))

(conf embark-consult)

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
	      (ggtags-mode 1)
	      (define-key go-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)
	      (define-key go-mode-map (kbd "C-,") #'ggtags-prev-mark)
	      (subword-mode)
	      (lsp-deferred)

	      ;;(local-unset-key (kbd "M-.")) ; unmask ggtags
	      (define-key ggtags-mode-map (kbd "M-.") nil)
	      ;; requires GO111MODULE=on go get golang.org/x/tools/gopls@latest
	      (define-key go-mode-map (kbd "M-.") #'lsp-find-definition)

	      (abbrev-mode 1)
	      ;; print all methods that item implements
	      (define-abbrev go-mode-abbrev-table "impls"
		"foo := reflect.TypeOf();
    for i:=0; i<foo.NumMethod();i++ {method:=foo.Method(i);Println(method.Name)}")
	      (define-abbrev go-mode-abbrev-table "ifer"
		"if err != nil {

}" 'abbrev-iferr)

	      ;; Bindings in  go-goto-map
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


  ;;; go assembly

  (defun ak-go-asm-comment-char ()
    (interactive)
    (setq-local comment-start "// ")))

;;(conf go-gopath)

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

(conf undo-tree
  (global-undo-tree-mode)
  (global-set-key (kbd "C-/") 'undo)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))

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
              (company-mode 1)
              ;;(add-to-list 'company-backends 'company-tern)
              (js2-imenu-extras-mode)
              (define-key js-mode-map (kbd "C-c b") 'web-beautify-js)
              ;;(define-key js-mode-map (kbd "M-.") 'helm-ag)
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

(conf multiple-cursors)

(conf cmake-mode)

(conf lsp-ui)

;;; sass|scss
(conf ssass-mode
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (add-to-list 'auto-mode-alist '("\\.sass$" . ssass-mode))
  (eval-after-load 'scss-mode '(define-key scss-mode-map (kbd "C-c b") 'web-beautify-css))

  (add-hook 'ssass-mode-hook
	    (lambda ()
	      (rainbow-mode t)
	      ;;(define-key ssass-mode-map (kbd "M-.") 'helm-ag)
	      )))

(conf smartparens)

(conf flycheck
  (defvar ak-flycheck-toggle nil)
  (defun ak-flycheck-mode ()
    (interactive)
    (if ak-flycheck-toggle
	(flycheck-mode -1)
      (flycheck-mode 1))
    (setq ak-flycheck-toggle (not ak-flycheck-toggle))))

(conf projectile)

(conf highlight-indentation)

;; C++ language server
(conf ccls)

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
              (define-key php-mode-map (kbd "M-n") 'imenu)
              (define-key php-mode-map (kbd "C-c C-r") 'revert-buffer)
              (define-key php-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
              ))
  (add-to-list 'auto-mode-alist '("\\.php$"  . php-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.php$"  . web-mode))
  (global-set-key (kbd "<f11>") 'php-mode)
  (global-set-key (kbd "<f12>") 'web-mode))

(conf paredit
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

(conf magit
  (global-set-key (kbd "C-c m") 'magit-status))

(conf gitignore-mode)

(conf macrostep
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand))

(conf ini-mode
  (add-to-list 'auto-mode-alist '("\\.ini$"  . ini-mode))
  (add-to-list 'auto-mode-alist '("\\.service\\|\\.target$"  . ini-mode)) ; Systemd
  (add-to-list 'auto-mode-alist '("PKGBUILD$"  . ini-mode)) ; Arch package file
  )

(conf highlight-symbol
  (global-set-key [(control f9)] 'highlight-symbol-at-point)
  (global-set-key [f9] 'highlight-symbol-next)
  (global-set-key [(shift f9)] 'highlight-symbol-prev)
  (global-set-key [(meta f9)] 'highlight-symbol-query-replace)

  (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  ;;(setq highlight-symbol-on-navigation-p nil)
  )

(conf ggtags)

(conf ace-window
  (global-set-key (kbd "C-c o") #'ace-window)
  (global-set-key (kbd "C-c s") #'ace-swap-window)
  ;; (global-set-key (kbd "M-p") (lambda ()
  ;;                               (interactive)
  ;;                               (set-window-buffer (selected-window) (other-buffer))))
  )

(conf glsl-mode
  (add-to-list 'auto-mode-alist '("\\.comp$" . glsl-mode)))

(conf company)

;; Themes

(conf base16-theme)
(conf doom-modeline)

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
	  (conf dired-toggle-sudo)
          (lambda ()
            (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
            ;;(setq dired-listing-switches "-hal --time-style=iso")
            (setq dired-listing-switches "-hal --group-directories-first")
            (define-key dired-mode-map (kbd "?") 'dired-get-size)))


;;----------------------------------------------------
;;; helm

;; (conf helm)
;; (require 'helm-config)
;; (helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-c j") 'helm-mini)
;; (global-set-key (kbd "C-c C-j") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files) ;replace `find-file
;; (global-set-key (kbd "M-n") 'helm-semantic-or-imenu)

;; ;;; helm-ag
;; (setq-default helm-ag-insert-at-point 'symbol)

;; ;;; helm-swoop

;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)


;;----------------------------------------------------
;;; semantic
;; disable semantic in some modes
(setq semantic-new-buffer-setup-functions
      '((c-mode . semantic-default-c-setup)
        (c++-mode . semantic-default-c-setup)
        (html-mode . semantic-default-html-setup)
        (java-mode . wisent-java-default-setup)
        ;;(js-mode . wisent-javascript-setup-parser) ;
        ;;(python-mode . wisent-python-default-setup) ; no decoration for type hints
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
(add-hook 'semantic-decoration-mode-hook
          (lambda ()
            (setq semantic-decoration-styles
                  '(("semantic-decoration-on-protected-members" . nil)
                    ("semantic-decoration-on-private-members" . nil)
                    ("semantic-tag-boundary" . t)))))
(global-semantic-decoration-mode 1)

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

;;(elpy-enable)

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
  (local-set-key (kbd "C-c f") #'ak-flycheck-mode)
  (define-key jedi-mode-map (kbd "C-c p") #'jedi:goto-definition-pop-marker)
  (define-key jedi-mode-map (kbd "C-c ,") nil) ; unhide `semantic-force-refresh'
  (define-key jedi-mode-map (kbd "M-.") (lambda () (interactive)
                                          (xref-push-marker-stack)
                                          (jedi:goto-definition)))
  ;;(define-key jedi-mode-map (kbd "C-.") #'ggtags-find-tag-dwim)

  )
(add-hook 'python-mode-hook 'python-mode-func)

;;----------------------------------------------------
;;; xml

;;(add-to-list 'auto-mode-alist '("\\.xml$"  . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$"  . web-mode))

;;----------------------------------------------------
;;; css

(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode t)
            ;;(define-key css-mode-map (kbd "M-.") 'helm-ag)
            ))


;;----------------------------------------------------
;;; Scheme mode

(add-hook 'scheme-mode-hook
          #'(lambda ()
              (enable-paredit-mode)
              (abbrev-mode 1)
              (define-abbrev scheme-mode-abbrev-table  "nl" "(newline)")
              (define-abbrev scheme-mode-abbrev-table  "dl" "(display )"))

          ;; added `define*' to scheme mode
          ;; added `define-syntax-rule' for macros
          (setq scheme-imenu-generic-expression
                '((nil
                   "^(define\\(\\|\\*\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
                  ("Types"
                   "^(define-class\\s-+(?\\(\\sw+\\)" 1)
                  ("Macros"
                   "^(\\(defmacro\\|define-macro\\|define-syntax\\|define-syntax-rule\\)\\s-+(?\\(\\sw+\\)" 2)))
          )

;;----------------------------------------------------
;;; org mode

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;----------------------------------------------------
;;; c mode

(add-hook 'c-mode-hook
          (lambda ()
            ;; gnu, k&r, bsd, stroustrup, whitesmith, ellemtel, linux, python, java, awk
            (c-set-style "gnu")
            (ggtags-mode)
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (setq c-macro-preprocessor "cpp -CC")
              ;;(conf preproc-font-lock-mode)
              ;;(preproc-font-lock-mode 1)
              (hs-minor-mode 1) ; hide/show blocks
              (define-key c-mode-map "\C-c\C-f" 'ff-find-other-file)
              (define-key c++-mode-map "\C-c\C-f" 'ff-find-other-file)
              (define-key c-mode-map (kbd "M-.") #'ggtags-find-tag-dwim)
              (define-key c-mode-map (kbd "M-,") #'ggtags-prev-mark)

              (semantic-mode 1)
              ;;(lsp)
              
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
            (ggtags-mode)
            ;; mask a key binding of ggtags minor mode
            (let ((oldmap (cdr (assoc 'ggtags-mode minor-mode-map-alist)))
                  (newmap (make-sparse-keymap)))
              (set-keymap-parent newmap oldmap)
              (make-local-variable 'minor-mode-overriding-map-alist)
              (push `(ggtags-mode . ,newmap) minor-mode-overriding-map-alist))
            ))

;; ;;----------------------------------------------------
;; ;; rustic-mode

;; ;; rustup component add rls rust-analysis rust-src
;; ;; or
;; ;; rustup component add --toolchain "1.42.0-x86_64-unknown-linux-gnu" rls rust-analysis rust-src

;; (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))

;; ;;(setq rust-format-on-save t)
;; (add-hook 'rustic-mode-hook
;;           (lambda ()
;;             (abbrev-mode 1)
;;             (define-mode-abbrev "pnl" "println!(\"{:?}\",  );")
;;             ))

;; ;;----------------------------------------------------
;; ;; slime
;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (setq slime-lisp-implementations
;;                   '(;;(cmucl ("cmucl" "-quiet"))
;;                     (sbcl ("sbcl") :coding-system utf-8-unix))))
;;           )

;; ;;----------------------------------------------------
;; ;;; java mode

;; (setq
;;  lsp-java-vmargs (list
;;                   "-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:9125"
;;                   "-noverify"
;;                   "-Xmx2G")

;;  lsp-file-watch-ignored
;;  '(".idea" ".ensime_cache" ".eunit" "node_modules"
;;    ".git" ".hg" ".fslckout" "_FOSSIL_"
;;    ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
;;    "build")
;;  lsp-java-import-order '["" "java" "javax" "#"]
;;  lsp-java-save-action-organize-imports nil)

;; (add-hook
;;  'java-mode-hook
;;  (lambda ()
;;    (setq tab-width 4)

;;    ;; eglot
;;    ;;(setenv "CLASSPATH" "/home/ak/.emacs.d/.cache/lsp/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar")

;;    ;;; LSP

;;    (setq
;;     lsp-response-timeout 10
;;     ;;lsp-log-io t
;;     lsp-file-watch-threshold 11000
;;     lsp-ui-doc-enable nil
;;     lsp-java-boot-enabled nil
;;     lsp-signature-auto-activate t
;;     lsp-signature-doc-lines 1
;;     lsp-ui-sideline-enable t
;;     ;;lsp-ui-sideline-show-diagnostics t
;;     lsp-ui-sideline-show-hover t)

;;    (lsp)
;;    (lsp-ui-mode)
;;    (lsp-lens-mode)
;;    ;; (projectile-mode t)
;;    (define-key java-mode-map (kbd "M-'") #'lsp-ui-doc-focus-frame)

;;    ;; Spring Boot server requires JDK-9 or higher
;;    ;;(lsp-java-boot-lens-mode)

;;    ;; ;; Enabling only some features
;;    ;; (setq dap-auto-configure-features '(sessions locals controls tooltip))

;;    (require 'dap-java)
;;    (dap-register-debug-template "My Remote"
;;                                 (list :type "java"
;;                                       :request "attach"
;;                                       :hostName "localhost"
;;                                       :port "9125"
;;                                       ))
;;    ;;(setq dap-auto-configure-features '(sessions locals controls tooltip))
;;    (setq dap-auto-configure-features '()
;;          dap-print-io t)
;;    (dap-mode 1)
;;    (dap-ui-mode 1)
;;    (dap-tooltip-mode 1)
;;    ;; if it is not enabled `dap-mode' will use the minibuffer.
;;    (tooltip-mode 1)

;;    (define-key java-mode-map (kbd "C-c C-n") #'dap-next)
;;    (define-key java-mode-map (kbd "C-c C-r") #'dap-continue)
;;    ))


;; ;;----------------------------------------------------

;; ;;; utility functions

;; ;; (defmacro measure-time (&rest body)
;; ;;   "Measure the time it takes to evaluate BODY."
;; ;;   `(let ((time (current-time)))
;; ;;      ,@body
;; ;;      (message "%.06f" (float-time (time-since time)))))

(princ package-selected-packages)

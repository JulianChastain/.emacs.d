;; Native Comp Hack-fix
(defvar native-comp-deferred-compilation-deny-list nil)

;; Straight Package Manage
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))



;; Buffer file name completion
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
            completion-ignore-case t)

;; LSP Support

;; Eglot
(straight-use-package 'eglot)

;; Evil Mode
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
(straight-use-package 'evil-nerd-commenter)
(evilnc-default-hotkeys)

;; Indent Guide
(straight-use-package 'indent-guide)
(require 'indent-guide)
(defun my-insert-tab-char ()
  "Insert a tab char. (ASCII 9, \t)"
  (interactive)
  (insert "\t"))
(global-set-key (kbd "TAB") 'my-insert-tab-char)

;; Undo Tree
(straight-use-package 'undo-tree)
(global-undo-tree-mode)

;; Vimish Fold
(straight-use-package 'vimish-fold)
(require 'vimish-fold)
(vimish-fold-global-mode)
(straight-use-package 'evil-vimish-fold)
(require 'evil-vimish-fold)
(global-evil-vimish-fold-mode)

;; Ace Window
(straight-use-package 'ace-window)
(require 'ace-window)
(define-key evil-normal-state-map (kbd "SPC w") 'ace-window)

;; Projectile
(straight-use-package 'projectile)
(projectile-mode +1)
(define-key evil-normal-state-map (kbd "SPC c") 'projectile-compile-project)
(define-key evil-normal-state-map (kbd "SPC r") 'projectile-run-project)

;; RealGud - gdb wrapper
(straight-use-package 'realgud)

;; Simple Clipboard
(straight-use-package 'simpleclip)
(define-key evil-normal-state-map (kbd "SPC s") 'simpleclip-mode)

;; Hydra
(straight-use-package 'hydra)
(defhydra comandtree (global-map "<f2>")
  "tree"
  ("c" 'projectile-compile-project "Compile")
  ("r" 'projectile-run-project "Run")
  ("l" 'linum-mode "Lines")
  ("t" 'treemacs "TreeView")
  ("i" 'ibuffer "Show Buffer")
  ("b" 'list-bookmarks "Bookmarks")
  ("w" 'ace-window "Swap Window"))

;; Treemacs
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-evil)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)
(define-key evil-normal-state-map (kbd "SPC t") 'treemacs)

;; Magit
(straight-use-package 'magit)

;;; Inline static analysis

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)

;; Display messages when idle, without prompting
(setq help-at-pt-display-when-idle t)

;; Message navigation bindings
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
    (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error))

;;; Go Support
(straight-use-package 'go-mode)

;;; JSON Support
(straight-use-package 'json-mode)

;;; Julia Support
(straight-use-package 'julia-mode)

;;; Lua Support
(straight-use-package 'lua-mode)

;;; Rust Support
(straight-use-package 'rust-mode)

;;; Markdown support
(straight-use-package 'markdown-mode)

;;; Outline-based notes management and organizer
(global-set-key (kbd "C-c a") #'org-agenda)


;; Ivy Autocomplete
(straight-use-package 'ivy)
(require 'ivy)
(ivy-mode 1)

;; Company Autocomplete
(straight-use-package 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck Syntax Checking
(straight-use-package 'flycheck)
(global-flycheck-mode)

;; Nano config
(cond ((eq system-type 'gnu/linux)
       (add-to-list 'load-path "~/.config/nano-emacs"))
      ((eq system-type 'windows-nt)
       (add-to-list 'load-path "~/nano-emacs")))

;; Other theme option
(straight-use-package 'gruvbox-theme)

;; Set colorscheme based on whether we are in the terminal
(cond
 (window-system
					;(require 'nano-layout)
					;(require 'nano-modeline)
					;(require 'nano-splash)
  (require 'nano-theme-dark)
  (require 'nano-faces)
  (require 'nano-theme)
  (nano-faces)
  (nano-theme))
 (t (load-theme 'gruvbox-dark-soft t)))

(add-hook 'after-make-frame-functions
	  (lambda ()
	    (cond (window-system
					;(require 'nano-layout)
					;(require 'nano-modeline)
					;(require 'nano-splash)
		   (require 'nano-theme-dark)
		   (require 'nano-faces)
		   (require 'nano-theme)
		   (nano-faces)
		   (nano-theme))
		  (t (load-theme gruvbox-dark-soft t)))))

;; ??
(straight-use-package 'telephone-line)
(require 'telephone-line)
(telephone-line-mode 1)

;; Scrolling
(scroll-bar-mode -1)
(straight-use-package 'good-scroll)
(good-scroll-mode 1)

;; Better Dashboard
(straight-use-package 'dashboard-ls)
(require 'dashboard)
(dashboard-setup-startup-hook)

;; Set Font
(setq default-frame-alist
      (append (list
               '(font . "Roboto Mono:style=Light:size=14"))))




;; Vterm Customization
(cond ((eq system-type 'gnu/linux)
       (straight-use-package 'vterm)
       (require 'vterm))
      ((eq system-type 'windows-nt)
       (setq shell-file-name "c:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")))


;; C Customization
;; LSP Mode
(straight-use-package 'cc-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; Common Lisp Customization
;(straight-use-package 'yasnippets)
;(yas-global-mode 1)
(straight-use-package 'slime)
(straight-use-package 'lispy)
(straight-use-package 'paredit)
(add-to-list 'load-path "~/.emacs.d/slime-star")
(setq slime-contribs '(slime-fancy))
(setq slime-contribs '(slime-star))
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook '(lambda ()
			     (slime-mode)))
(cond ((eq system-type 'gnu/linux)
       (setq inferior-lisp-program "ros -Q run"))
      ((eq system-type 'windows-nt)
       (setq inferior-lisp-program "sbcl.exe")))
;(straight-use-package 'common-lisp-snippets)

;; Better Defaults
(setq visible-bell 1)
(setq-default indent-tabs-mode t)
(setq tab-width 2)
;;(setq 'c-basic-offset 'tab-width
(define-key evil-normal-state-map (kbd "SPC") 'comandtree/body)
(prefer-coding-system 'utf-8)
(prettify-symbols-mode)

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                              (unless buffer-file-name
                                                (let ((buffer-file-name (buffer-name)))
                                                                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

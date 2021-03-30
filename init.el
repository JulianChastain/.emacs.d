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

;; Evil Mode
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
(straight-use-package 'evil-nerd-commenter)
(evilnc-default-hotkeys)

;; Indent Guide
(straight-use-package 'indent-guide)
(require 'indent-guide)

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

;; Treemacs
(straight-use-package 'treemacs)
(straight-use-package 'treemacs-evil)
(straight-use-package 'treemacs-projectile)
(straight-use-package 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)
(define-key evil-normal-state-map (kbd "SPC t") 'treemacs)

;; Magit
(straight-use-package 'magit)

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

;; Cosmetic config
(add-to-list 'load-path "~/.config/nano-emacs")
(require 'nano-layout)
;(require 'nano-theme-dark)
;(require nano-faces)
;(nano-faces)
(require 'nano-theme)
(set-frame-parameter nil 'fullscreen 'fullboth)
(straight-use-package 'gruvbox-theme)
(load-theme 'gruvbox-dark-soft t)

;; Splash Screen
(add-hook 'after-init-hook 'about-emacs)

;; Vterm
(straight-use-package 'vterm)
(require 'vterm)

;;;C Customization
;; LSP Mode
(straight-use-package 'cc-mode)
(straight-use-package 'lsp-mode)
(straight-use-package 'modern-cpp-font-lock)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; Lisp Customization
(straight-use-package 'slime)
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(straight-use-package 'paredit)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'display-line-numbers-mode)
(load "~/quicklisp/log4slime-setup.el")
(global-log4slime-mode 1)
      

;; Better Defaults
(setq visible-bell 1)
(setq-default indent-tabs-mode t)
(setq tab-width 4)
;;(setq 'c-basic-offset 'tab-width)
(define-key evil-normal-state-map (kbd "SPC b") 'ibuffer)
(prefer-coding-system 'utf-8)
(prettify-symbols-mode)

;; Custom Set Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
 '(hl-sexp-background-color "#efebe9")
 '(org-agenda-files '("~/Documents/todo/thruDenver.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

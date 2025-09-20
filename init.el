;;; Package Management
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

;; Add sepi directory to load-path
(add-to-list 'load-path (expand-file-name "sepi" user-emacs-directory))

;;; Core Settings
(use-package emacs
  :ensure nil
  :init
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil
        custom-file (expand-file-name "custom.el" user-emacs-directory)
        inhibit-startup-message t
        initial-buffer-choice t
        initial-scratch-message ""
        visible-bell t
        ring-bell-function 'ignore
        tab-width 2
        scroll-step 1
        scroll-conservatively 10000
        use-short-answers t)
  :custom
  (frame-resize-pixelwise t)
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-use-momentum nil)
  (use-dialog-box nil)
  (use-file-dialog nil)
  
  (setq frame-title-format
	'((:eval (if (buffer-file-name)
		     (abbreviate-file-name (buffer-file-name))
		   "%b"))))

  

  :config
  ;; (add-hook 'prog-mode-hook #'hs-minor-mode)
  (delete-selection-mode 1)
  (put 'upcase-region 'disabled nil)
  (when (file-exists-p custom-file) (load custom-file))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (global-set-key (kbd "s-n") (lambda () (interactive) (call-process "open" nil 0 nil "-n" "-a" "Emacs" "--args" "--chdir" (expand-file-name "~"))))
  (global-set-key (kbd "s-S-n") (lambda () (interactive) (call-process "open" nil 0 nil "-n" "-a" "Emacs" "--args" "--chdir" default-directory)))

  (global-unset-key (kbd "C-t"))
  (global-unset-key (kbd "C-\\"))

  ;; (define-key input-decode-map [escape] [escape])  ;; make ESC send only ESC
  ;; (global-set-key [escape] 'keyboard-quit)

  (global-set-key (kbd "s-<up>") 'beginning-of-buffer)
  (global-set-key (kbd "s-<down>") 'end-of-buffer)

  (global-set-key (kbd "M-<left>") 'backward-word)
  (global-set-key (kbd "M-<right>") 'forward-word)
  (global-set-key (kbd "M-<up>") 'backward-paragraph)
  (global-set-key (kbd "M-<down>") 'forward-paragraph)
  
  (global-set-key (kbd "C-S-g") 'top-level)
  (global-set-key (kbd "C-l") #'goto-line)
  (global-set-key (kbd "C-\\") 'dabbrev-expand)
  (global-set-key (kbd "s-s") (lambda () (interactive)
                                (call-interactively 'occur)
                                (pop-to-buffer "*Occur*"))))

;;; UI Customization
(use-package frame
  :ensure nil
  :hook (emacs-startup . (lambda ()
                           (setq-default mode-line-format
                                         '("%e" mode-line-front-space
                                           "%b ░ %* ░ %l:%c"
					   " ░ " vc-mode
					   mode-line-format-right-align
                                           " ░ " mode-name
                                           ;; mode-line-end-spaces
					   ))))
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  ;; (load-theme 'naysayer t)
  (load-theme 'default-black t)
  (set-display-table-slot standard-display-table 'truncation ?░)
  (set-fringe-mode 0)
  (setq-default line-spacing 0.0)
  ;; (set-face-attribute 'default nil :font "Cascadia Code Light" :height 140)
  ;; (set-face-attribute 'default nil :font "Cascadia Code" :height 150)
  ;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
  ;; (set-face-attribute 'default nil :font "Hack" :height 140)
  ;; (set-face-attribute 'default nil :font "Anonymous Pro" :height 150)
  ;; (set-face-attribute 'default nil :font "PT Mono" :height 140)
  ;; (set-face-attribute 'default nil :font "Input Mono" :height 130)
  (set-face-attribute 'default nil :font "Source Code Pro" :height 130)
  
  (global-display-line-numbers-mode -1)
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;;; Keybindings
(use-package windmove
  :bind (("C-<left>" . windmove-left)
         ("C-<right>" . windmove-right)
         ("C-<up>" . windmove-up)
         ("C-<down>" . windmove-down)))

(use-package project
  :bind (("s-<escape>" . project-shell-command)
         ("M-ESC" . project-async-shell-command)
         ("s-b" . project-switch-to-buffer)
         ("s-f" . project-find-file)
         ("s-d" . project-dired)))

(use-package helpful
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)))

;;; Custom Sepi Packages
(use-package sepi-recenter
  :ensure nil
  :bind ("s-l" . sepi-recenter-and-pulse))

(use-package sepi-pulse
  :ensure nil
  :config (sepi-enable-pulse-copy-paste))

(use-package sepi-toggle
  :ensure nil
  :bind (("C-t l" . sepi-toggle-line-numbers-and-fringe)
         ("C-t w" . sepi-toggle-line-wrap)))

(use-package sepi-init
  :ensure nil
  :bind ("s-," . sepi-open-init-file))

(use-package sepi-smart-bol
  :ensure nil
  :config
  (sepi-smart-bol-mode 1)
  :bind (("C-a" . sepi-smart-bol)))

(use-package sepi-c-comment-box
  :ensure nil)

(use-package sepi-bitwise
  :ensure nil)

(use-package dired
  :ensure nil
  :init
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t
        dired-listing-switches "-lh"
        dired-kill-when-opening-new-dired-buffer t
        dired-dwim-target t)
  :config
  (global-set-key (kbd "C-x d") #'dired)
  (global-set-key (kbd "C-x C-d") #'dired)
  :hook (dired-mode . (lambda ()
                        (local-set-key (kbd ".") 'dired-toggle-hidden)
                        (dired-hide-details-mode 1)))
  :bind (:map dired-mode-map
              ("!" . wdired-change-to-wdired-mode))
  :config
  (defun dired-toggle-hidden ()
    "Toggle hidden files in Dired."
    (interactive)
    (if (string-match "a" dired-actual-switches)
        (dired-sort-other (replace-regexp-in-string "a" "" dired-actual-switches))
      (dired-sort-other (concat dired-actual-switches "a")))))

;;; Buffer Management
(use-package ibuffer
  :ensure nil
  :config (defalias 'list-buffers 'ibuffer))

;;; Completion
(use-package minibuffer
  :ensure nil
  :init
  (setq completion-show-help nil
        completion-styles '(basic flex)
        completions-detailed nil
        enable-recursive-minibuffers t
        completion-auto-select 'second-tab
        completion-auto-help 'always
        completions-format 'one-column
        completions-sort 'historical
        completions-max-height 20
        completion-ignore-case t)
  :config (completion-preview-mode t))

(use-package python
  :ensure nil
  :hook (python-mode . (lambda () (setq python-indent-offset 2))))

(use-package lua-mode
  :defer t
  :hook (lua-mode . (lambda () (setq lua-indent-level 2))))

(use-package clang-format
  :ensure t
  :hook (c-mode . my-c-mode-clang-format-on-save)
  :config
  (defun my-c-mode-clang-format-on-save ()
    (add-hook 'before-save-hook
              (lambda ()
                (let ((pos (point)))
                  (clang-format-buffer)
                  (goto-char pos)))
              nil t)))

(use-package isearch
  :ensure nil
  :init
  (advice-add 'isearch-repeat :after
	      (lambda (&rest _) (recenter)))
  (setq isearch-wrap-pause 'no
        isearch-lazy-count t
        isearch-lazy-highlight t))

(use-package rg
  :defer t
  :commands (rg-run)
  :bind (:map rg-mode-map
              ("h" . rg-menu))
  :config
  (rg-enable-default-bindings)
  (define-key rg-mode-map (kbd "e") #'wgrep-change-to-wgrep-mode)
  (setq rg-regexp nil)
  (setq display-buffer-alist
        '(("\\*rg\\*" display-buffer-reuse-window display-buffer-same-window))))

(use-package multiple-cursors
  :ensure t
  :bind (("C-." . mc/mark-next-like-this)
         ("C-," . mc/mark-previous-like-this)
         ("C-s-<return>" . mc/mark-all-like-this))
  :config
  (setq mc/keymap (let ((map (make-sparse-keymap)))
                    (define-key map (kbd "C-g") 'mc/keyboard-quit)
                    map)))

(use-package expreg
  :defer t
  :bind (("C-=" . expreg-expand)
	 ("C--" . expreg-contract)))

(use-package ctrlf
  :defer t
  :init
  (ctrlf-mode +1)
  (setq ctrlf-auto-recenter 1)
  :bind (("M-s-." . ctrlf-forward-symbol-at-point)))

(use-package wgrep
	:ensure t
	:defer t
	:config)

;;; External Packages
(use-package markdown-mode :defer t)
(use-package format-all :defer t)
(use-package magit :defer t)
(use-package which-key :config (which-key-mode))

;;; init.el ends here
(put 'downcase-region 'disabled nil)

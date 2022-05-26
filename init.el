;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sepimacs version 0.2                   ;;
;;; https://github.com/sepisoad/sepimacs   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; who am I?
(setq-default user-full-name "Sepehr Aryani")
(setq-default user-mail-address "sepehr.aryani@gmail.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup emacs package manager  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;
;; setup use-package  ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;
;; global vars  ;;
;;;;;;;;;;;;;;;;;;

;; define temp dir
(defconst emacs-tmp-dir
  (expand-file-name
   (format "emacs%d" (user-uid)) temporary-file-directory))

;;;;;;;;;;;;;;;;;;;;;;
;; custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; function to toggle line numbers
(defun toggle-line-numbers ()
    (interactive)
    (global-display-line-numbers-mode 'toggle))

;; function to reload init file
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))


;;;;;;;;;;;;;;;;;;;;;;;;
;; basic emacs config ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :init
  ;; emacs garbase collector threshold (performance)
  (setq gc-cons-threshold 800000)
  ;; tweak the size of process output read (1mb)::(performace)
  (setq read-process-output-max (* 1024 1024))
    ;; no startup screen
  (setq inhibit-startup-screen t)
  ;; no startup message
  (setq initial-scratch-message "")
  ;; enhance title bar text format
  (setq ring-bell-function 'ignore)
  ;; set custom file for emacs!
  (setq custom-file "~/.emacsp/sepi-v2/custom.el")
  ;; get rid of lock files
  (setq create-lockfiles nil)
  ;; show line number
  (setq line-number-mode t)
  ;; show column number
  (setq column-number-mode t)
  
  ;; disable line wrapping
  (setq-default truncate-lines t)
  ;; set the line spacing
  (setq-default frame-title-format '("%b"))
  ;; disable bell noise
  (setq-default line-spacing 0)
  ;; ruler!
  (setq-default fill-column 80) ;;<| WTF
  ;; set a max for kill ring
  (setq-default kill-ring-max 128)
  
  ;; disable menubar
  (menu-bar-mode -1)
  ;; disable toolbar
  (tool-bar-mode -1)
  ;; disable scrollbar
  (scroll-bar-mode -1)
  ;; config border sizes
  (fringe-mode '(0 . 10))
  ;; use y/n instead of yes/no
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; show closing paren/bracket/...
  (show-paren-mode 1)
  ;; delete selected text on change
  (delete-selection-mode 1)
  ;; refresh buffer on external chenage
  (global-auto-revert-mode t)
  ;; load custom file
  (load custom-file nil t)
  ;; pixel based scrolling
  (pixel-scroll-mode)
  ;; font face
  (set-face-attribute 'default nil :family "Cascadia Code" :height 140 :weight 'normal )
  ;; theme |> WARNING: this is installed manually <|
  (load-theme 'plan9 t)

  :config
  ;; if custom file does not exist create one
  (unless (file-exists-p custom-file) (write-region "" nil custom-file))
  ;; load user shell exec path
  (when (memq window-system '(mac ns))  (exec-path-from-shell-initialize))

  :config
  ;; properly handle temp files
  (setq
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   auto-save-list-file-prefix emacs-tmp-dir
   auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))
   backup-directory-alist `((".*" . ,emacs-tmp-dir)))

  :config ;; for some reason it is not possible to use :bind
  (global-set-key (kbd "M-s-l") #'toggle-line-numbers)
  (global-set-key (kbd "M-s-r") 'reload-init-file)

  ;; DONE
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all external packages goes here  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use system shell inside emacs
(use-package exec-path-from-shell
  :ensure t)

;; setup ibuffer
(use-package ibuffer
  :diminish ibuffer-mode
  :bind (("C-x C-b" . ibuffer)))

;; better window jump. C-x w number
(use-package winum
  :config (winum-mode))

;; better terminal emulator
(use-package vterm
  :ensure t)

;; 256 terminal color
(use-package eterm-256color
  :ensure t)

;; crux
(use-package crux
  :ensure t
  :diminish 
  :bind (("C-k" . crux-smart-kill-line)
         ("C-x f" . crux-recentf-find-file )
         ("C-c d" . crux-duplicate-current-line-or-region )
         ("C-c r" . crux-rename-file-and-buffer )
         ("C-c k" . crux-kill-other-buffers )
         ("C-<return>" . crux-smart-open-line)
         ("C-s-<return>" . crux-smart-open-line-above)))

;; avy, jump to anywhere
(use-package avy
  :ensure t
  :diminish avy-mode
  :bind (("C-'" . avy-goto-char)))

;; config ido, interactive do
(use-package ido
  :diminish ido-mode
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode 1))

;; flx, fuzzy matching
(use-package flx-ido
  :ensure t
  :diminish flx-ido-mode
  :config
  (setq flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))
  

;; smex, better M-x
(use-package smex
  :ensure t
  :config (smex-initialize)
  :bind (("M-x" . 'smex)
	 ("M-X" . 'smex-major-mode-command)))

;; which-key ;;TODO: do i really need it?
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (setq which-key-idle-delay 2.0)
  (setq  which-key-idle-secondary-delay 1.0)
  :config
  (which-key-mode))

;; helpful, better emacs help
(use-package helpful
  :ensure t
  :diminish helpful-mode
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))

;; dashboard, awesome!
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "درود بی نهایت بر سپهر بزرگ")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t))

;; multiple cursors, awesome!
(use-package multiple-cursors
  :ensure t
  :diminish multiple-cursors-mode
  :bind (("C-." . mc/mark-next-like-this)
	 ("C-," . mc/mark-previous-like-this)
	 ("M-RET" . mc/mark-all-like-this)))

;; expand region, awesome!
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C-c f f" . er/mark-defun)
	 ("C-c c c" . er/mark-comment)
	 ("C-c i '" . er/mark-inside-quotes)
	 ("C-c a '" . er/mark-outside-quotes)
	 ("C-c i p" . er/mark-inside-pairs)
	 ("C-c a p" . er/mark-outside-pairs)))

;; setup dired
(use-package dired
  :ensure nil
  :diminish dired-mode
  :hook (dired-mode . dired-hide-details-mode)
  :config (setq dired-use-ls-dired nil))

;; ctrlf, better buffer local search
(use-package ctrlf
  :ensure t
  :diminish ctrlf-mode
  :bind ("C-s" . ctrlf-forward-default)
  :bind ("C-r" . ctrlf-backward-default)
  :bind ("M-s" . ctrlf-forward-alternate)
  :bind ("M-r" . ctrlf-backward-alternate))

;; rg, rip-grep fast search across files
(use-package rg
  :ensure t
  :diminish rg-mode
  :config (rg-enable-default-bindings)
  :bind (("C-M-f" . rg)))

;; projectile, project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

;; rg/projectile integration
(use-package projectile-ripgrep
  :ensure t
  :bind (("C-M-s" . projectile-ripgrep)))

;; visual feedback
(use-package goggles
  :ensure t
  :diminish goggles-mode
  :hook ((prog-mode text-mode) . goggles-mode)
  :config (setq-default goggles-pulse t))

;; jump last changes
(use-package goto-chg
  :ensure t
  :bind ("s-[" . goto-last-change)
  :bind ("s-]" . goto-last-change-reverse))

;; company mode, autocompletion
(use-package company
  :ensure t
  :defer t
  :init (global-company-mode t)
  :bind (("C-\\" . 'company-complete))
  :config
  (setq company-idle-delay nil)
  (setq company-minimum-prefix-length 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this section is all about languages modes  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; zig lang
(use-package zig-mode
  :ensure t)

;; ocaml lang
(use-package tuareg
  :ensure t)

;; janet lang
(use-package janet-mode
  :ensure t)

;; go lang
(use-package go-mode
  :ensure t
  :hook (go-mode . (lambda ()
		     (setq tab-width 2)
		     (setq indent-tabs-mode 1)))
  :hook (before-save-hook . gofmt-before-save))

;; javascritp lang
(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :config (js-indent-level 2))

;; elm lang
(use-package elm-mode
  :ensure t
  :hook (elm-mode . elm-format-on-save-mode))

;; markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; json
(use-package json-mode
  :ensure t)

;; yaml
(use-package yaml-mode
  :ensure t)

;; graphql
(use-package graphql-mode
  :ensure t)

;; dhall
(use-package dhall-mode
  :ensure t
  :config
  (setq dhall-type-check-inactivity-timeout 5))

;; jq
(use-package jq-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;
;; LSP config   ;;
;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure t
  :bind (("<f12>" . xref-find-definitions) ;; find def
	 ("s-<f12>" . eglot-find-typeDefinition) ;; find type def
	 ("<f11>" . eglot-find-implementation) ;; find impl
	 ("<f10>" . xref-find-references) ;; find refs
	 ("<f2>" . eglot-rename)
	 ("C-M-s-f" . eglot-format)
	 ("s-." . eglot-code-actions)
	 ("s-h" . eldoc)
	 ))

;;;;;;;;;;;;;;;;;;;;;;
;; config ligature  ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package ligature
  :load-path "~/.emacsp/sepi-v2/ligature"
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  (global-ligature-mode t))

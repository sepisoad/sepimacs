(unless (>= emacs-major-version 24)
  (error "The yaysayer-old theme requires Emacs 24 or later!"))

(deftheme yaysayer-old "The yaysayer-old color theme - old version")

;; Monokai colors
(defcustom yaysayer-old-theme-yellow "#E6DB74" "Primary colors - yellow" :type 'string :group 'monokai)
(defcustom yaysayer-old-theme-orange "#FD971F" "Primary colors - orange" :type 'string :group 'monokai)
(defcustom yaysayer-old-theme-red "#F92672" "Primary colors - red" :type 'string :group 'monokai)
(defcustom yaysayer-old-theme-magenta "#FD5FF0" "Primary colors - magenta" :type 'string :group 'monokai)
(defcustom yaysayer-old-theme-blue "#66D9EF" "Primary colors - blue" :type 'string :group 'monokai)
(defcustom yaysayer-old-theme-green "#A6E22E" "Primary colors - green" :type 'string :group 'monokai)
(defcustom yaysayer-old-theme-cyan "#A1EFE4" "Primary colors - cyan" :type 'string :group 'monokai)
(defcustom yaysayer-old-theme-violet "#AE81FF" "Primary colors - violet" :type 'string :group 'monokai)

;; (comments   "#44b340")
;; (selection  "#0000ff")
;; (builtin    "#ffffff")
;; (numbers    "#7ad0c6")
;; (variables  "#c1d1e3")
(let ((background "#062329")
      (gutters    "#062329")
      (gutter-fg  "#062329")
      (gutters-active "#062329")
      (builtin      "#ffffff")
      (selection  "#2D3F43")
      (text       "#d1b897")
      (comments   "#607276")
      (punctuation "#8cde94")
      (keywords "#ffffff")
      (variables "#80918C")      
      (functions "#ffffff")
      (methods    "#c1d1e3")
      (strings    "#2ec09c")
      (constants "#7ad0c6")
      (macros "#8cde94")
      (numbers "#8E8091")
      (white     "#ffffff")
      (error "#ff0000")
      (warning "#ffaa00")
      (highlight-line "#0b3335")
      (line-fg "#126367"))

  (custom-theme-set-faces
   'yaysayer-old

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,white                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; which-func
   `(which-func ((t (:inverse-video unspecified
                                    :underline unspecified
                                    :foreground ,background
                                    :weight bold
                                    :box nil))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box nil))))
   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
					    :foreground ,white
					    :background ,comments
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))

    ;; better compatibility with default DOOM mode-line
   `(error ((t (:foreground nil :weight normal))))
   `(doom-modeline-project-dir ((t (:foreground nil :weight bold))))
   
   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))

   ;; tab-bar-mode
   `(tab-bar ((t (:inherit modeline))))
   `(tab-bar-tab ((t (:foreground ,background :background ,text))))
   `(tab-bar-tab-inactive ((t (:foreground ,text :background ,background))))
  )

  (custom-theme-set-variables
    'yaysayer-old
    '(linum-format " %5i ")
  )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'yaysayer-old)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'yaysayer-old-theme)

;;; yaysayer-old-theme.el ends here

(unless (>= emacs-major-version 29)
  (error "The yaysayer theme requires Emacs 29 or later with Tree-sitter support!"))

(deftheme yaysayer "Improved Naysayer color theme with Tree-sitter support")

;; Colors
(let ((background "#062329")
      (gutters    "#062329")
      (gutter-fg  "#062329")
      (gutters-active "#062329")
      (builtin      "#ffffff")  ;; White for function calls and built-ins
      (selection  "#284858")    ;; Improved softer selection
      (text       "#d1b897")
      (comments   "#3E5A3E")    ;; Slightly darker green-gray for comments (previously #4A704A)
      (punctuation "#8cde94")
      (keywords "#FFDD88")      ;; Warmer yellow for contrast
      (variables "#c1d1e3")
      (functions "#ffffff")     ;; White for function names
      (methods    "#c1d1e3")
      (strings    "#2ec09c")
      (constants "#7ad0c6")
      (macros "#8cde94")
      (numbers "#E6A86E")       ;; Distinct warm orange for numbers
      (error "#D75F5F")         ;; Softer red for errors
      (warning "#E69C4D")       ;; Burnt orange for warnings
      (highlight-line "#0b3335")
      (cursor "#B0E0E6")        ;; Slightly bluish cursor for visibility
      (mode-line-active "#8e8a6b")  ;; Brighter mode-line for focus
      (mode-line-inactive "#1C3C45")) ;; Darker inactive mode-line

  (custom-theme-set-faces
   'yaysayer

   ;; Default colors (font-lock fallback)
   `(default                          ((t (:foreground ,text :background ,background :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,cursor))))
   `(fringe                           ((t (:background ,background :foreground ,text))))
   `(highlight                        ((t (:background ,selection))))

   ;; Font lock faces (fallback for non-Tree-sitter modes)
   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))  ;; White for built-ins
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))  ;; White for function names
   `(font-lock-warning-face           ((t (:foreground ,warning :weight bold))))
   `(font-lock-number-face            ((t (:foreground ,numbers))))  ;; Distinct color for numbers

   ;; Mode-line
   `(mode-line                        ((t (:foreground ,background :background ,mode-line-active :weight bold))))
   `(mode-line-inactive               ((t (:foreground ,text :background ,mode-line-inactive))))

   ;; Rainbow delimiters (fallback)
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,keywords))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,functions))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,comments))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,variables))))

   ;; Git diff (Magit)
   `(diff-added                       ((t (:foreground "#7ad07a"))))
   `(diff-removed                     ((t (:foreground "#d07a7a"))))
   
   ;; Indent guides
   `(indent-guide-face                ((t (:foreground "#2B8E99"))))

   ;; Optional: Ensure function calls in js2-mode are white (fallback)
   `(js2-function-call                ((t (:foreground ,builtin))))

   ;; Tree-sitter faces (for Tree-sitter-enabled modes)
   `(tree-sitter-hl-face:keyword      ((t (:foreground ,keywords))))
   `(tree-sitter-hl-face:type         ((t (:foreground ,punctuation))))
   `(tree-sitter-hl-face:constant     ((t (:foreground ,constants))))
   `(tree-sitter-hl-face:variable     ((t (:foreground ,variables))))
   `(tree-sitter-hl-face:function     ((t (:foreground ,functions))))
   `(tree-sitter-hl-face:method       ((t (:foreground ,methods))))
   `(tree-sitter-hl-face:builtin      ((t (:foreground ,builtin))))
   `(tree-sitter-hl-face:string       ((t (:foreground ,strings))))
   `(tree-sitter-hl-face:comment      ((t (:foreground ,comments))))
   `(tree-sitter-hl-face:property     ((t (:foreground ,variables))))
   `(tree-sitter-hl-face:number       ((t (:foreground ,numbers))))
   `(tree-sitter-hl-face:operator     ((t (:foreground ,punctuation))))
   `(tree-sitter-hl-face:delimiter    ((t (:foreground ,punctuation))))
   `(tree-sitter-hl-face:tag          ((t (:foreground ,keywords))))
   `(tree-sitter-hl-face:error        ((t (:foreground ,error :weight bold))))
   `(tree-sitter-hl-face:warning      ((t (:foreground ,warning :weight bold))))
   )

  (custom-theme-set-variables
   'yaysayer
   '(linum-format "%5i"))
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'yaysayer)

;;; yaysayer-theme.el ends here

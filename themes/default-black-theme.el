(deftheme default-black
  "Automatically created 2013-05-20.")

(custom-theme-set-faces
 'default-black
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :height 130))))
 '(highlight ((t (:background "#ec702c" :foreground "#ffffff"))))
 '(region ((nil (:background "#5f5f5b")))) ;;464740 ;;363635
 '(hl-line ((nil (:background "#222222"))))
 '(yas-field-highlight-face ((nil (:background "#333399"))))
 '(js2-function-param-face ((t (:foreground "LightGoldenrod"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(font-lock-comment-face ((t (:foreground "#91908f"))))
 '(show-paren-match ((nil (:background "#333399"))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))

(provide-theme 'default-black)

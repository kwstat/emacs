;; Base16 Mocha (https://github.com/chriskempson/base16)
;; Scheme: Chris Kempson (http://chriskempson.com)

;;; base16-mocha-theme.el

;;; Code:
;; http://www.workwithcolor.com/hsl-color-picker-01.htm
(deftheme base16-mocha)

(let ((background "#534636")
      (current-line "#7e705a")
      (selection "#483828")
      (foreground "#f1ecea")
      (comment "#c6bfbe")
      (cursor "#e9e1dd")
      (red "#f3bbb6")     ;(red "#cb6077")    
      (orange "#fdd58c")  ;(orange "#d28b71") 
      (yellow "#efef71")  ;(yellow "#f4bc87") 
      (green "#b4df90")   ;(green "#cec56b")  
      (aqua "#9cdec4")    ;(aqua "#7bbda4")   
      (blue "#b2cdec")    ;(blue "#8ab3b5")   
      (purple "#d0bee9")) ;(purple "#a89bb9"))

  (custom-theme-set-faces
   'base16-mocha

   ;; Built-in stuff
   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background ,current-line))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(mode-line ((t (:background ,current-line :foreground ,foreground))))
   `(region ((t (:background ,selection))))

   ;; flyspell Note, flyspell-incorrect inherits from error face
   `(flyspell-incorrect ((t (:inherit unspecified :underline ,red))))
   `(flyspell-duplicate ((t (:inherit unspecified :underline ,yellow))))

   ;; Font-lock stuff
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,blue))))
   `(font-lock-keyword-face ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,red))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;; ido
   `(ido-first-match ((t (:foreground ,yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,green :weight bold))))
   `(ido-subdir ((t (:foreground ,yellow))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; linum-mode
   `(linum ((t (:background ,current-line :foreground ,foreground))))

   ;; org-mode
   `(org-date ((t (:foreground ,purple))))
   `(org-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,current-line))))
   `(org-link ((t (:foreground ,blue))))
   `(org-table ((t (:foreground ,blue))))
   `(org-todo ((t (:foreground ,red))))
   `(org-level-1 ((t (:foreground ,red :weight bold))))
   `(org-level-2 ((t (:foreground ,blue :weight bold))))
   `(org-level-3 ((t (:foreground ,green :weight bold))))
   `(org-level-4 ((t (:foreground ,yellow :weight bold))))

   ;; show-paren-mode
   `(show-paren-match ((t (:background ,blue :foreground ,current-line))))
   `(show-paren-mismatch ((t (:background ,orange :foreground ,current-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,aqua))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,yellow))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,comment))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,foreground))))
   )

  (custom-theme-set-variables
   'base16-mocha

   `(ansi-color-names-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])
   `(ansi-term-color-vector
     ;; black, red, green, yellow, blue, magenta, cyan, white
     [unspecified ,background ,red ,green ,yellow ,blue ,purple ,blue ,foreground])))

(provide-theme 'base16-mocha)

;;; base16-mocha-theme.el ends here

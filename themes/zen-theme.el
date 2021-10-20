;; zen-theme.el
;; Subtle revision of the zenburn theme to be slightly more colorful.

;; GPL-3. Copyright (c) Kevin Wright.

;; Jani Nurminen created the original theme for vim
;; Daniel Brockman made a port to Emacs
;; Bozhidar Batsov updated it further, released under GPL-3
;;   https://github.com/bbatsov/zenburn-emacs
;; Kevin Wright modified further, calling it Zen, with more/brighter colors


(deftheme zen "The Zen color theme")

(let (
      (gray-9   "#eeeeee")
      (gray-8   "#dddddd")
      (gray-7   "#bbbbbb")
      (gray-6   "#999999")
      (gray-5   "#777777")
      (gray-4   "#555555")
      (gray-3   "#444444") ; zen background
      (gray-2   "#333333")
      (gray-1   "#222222")
      (gray-0   "#111111")
      (red-5    "#f0c0d0")
      (red-4    "#d797a0")
      (red-3    "#c07070")
      (red-2    "#a74040")
      (red-1    "#901010")
      (red-0    "#500000")
      (orange-5 "#ffd080")
      (orange-4 "#ffc040")
      (orange-3 "#f0a720")
      (orange-2 "#e09000")
      (orange-1 "#ff7000")
      (orange-0 "#b04000")
      (magenta-2 "#fe87f4")
      (magenta-1 "#d700d7")
      (magenta-0 "#a420a4")
      (brown-3  "#706050")
      (brown-2  "#605040")
      (brown-1  "#504030") ; autumn background
      (brown-0  "#302010")
      (yellow-5 "#fafad2") ; lightgoldenrod = summer background
      (yellow-4 "#f0f090")
      (yellow-3 "#e6e66a")
      (yellow-2 "#dcdc37")
      (yellow-1 "#f9f900") ; yellow
      (yellow-0 "#b8860b") ; darkgoldenrod
      (green-5  "#dbf7c0")
      (green-4  "#acd595")
      (green-3  "#7eb26a")
      (green-2  "#509040")
      (green-1  "#006000") ; darkgreen = #006400
      (green-0  "#004000") ; Others: olivedrab = #6b8e23 lime "#b0f010"
      (cyan-2   "#90f0f0")
      (cyan-1   "#00ffff") ; cyan
      (cyan-0   "#207777")
      (blue-5   "#cee7ff")
      (blue-4   "#93b4d4")
      (blue-3   "#5982aa")
      (blue-2   "#205080")
      (blue-1   "#0000f0") ; blue
      (blue-0   "#000060")
      (purple-5 "#ecd9ff")
      (purple-4 "#ddbbff")
      (purple-3 "#bb99dd")
      (purple-2 "#9977bb")
      (purple-1 "#8000a0") ; purple
      (purple-0 "#401050")
      (tan-5    "#fff0c0")
      (tan-4    "#ead0a0")
      (tan-3    "#d5b080")
      (tan-2    "#c09060")
      (tan-1    "#947249")
      (tan-0    "#573700") ; saddlebrown = #8b4513
      (class '((class color) (min-colors 89)))
      (header1 '(:foreground "#dbf7c0" :background "#006000" :height 1.4 :weight bold)) ;; :overline "#006000"
      (header2 '(:foreground "#cee7ff" :background "#0000f0" :height 1.3 :weight bold))
      (header3 '(:foreground "#f0c0d0" :background "#901000" :height 1.2 :weight bold))
      (header4 '(:foreground "#fff0c0" :background "#573700" :height 1.1 :weight bold))
      (paren-matched '(:background "black" :weight bold))
      (paren-unmatched '(:foreground "white" :background "#901010" :weight bold))
      )

  ;; :box (:line-width 1)
  ;; :underline t / "red" / ,red-3
  ;; :underline (:style wave :color , red-3)  ; line/wave
  ;; :weight bold/normal
  ;; :inherit unspecified
  ;; :slant italic
  ;; :style released-button
  ;; :italic t
      
  (custom-theme-set-faces
   'zen

   `(link ((t (:foreground ,tan-5 :underline t))))
   `(link-visited ((t (:foreground ,yellow-3 :underline t :weight normal))))

   ;; Basic UI
   `(default ((t (:foreground ,gray-9 :background ,gray-3))))
   `(button ((t (:foreground ,blue-4 :underline t))))
   `(cursor ((t (:foreground ,gray-8 :background ,yellow-5))))
   `(escape-glyph-face ((t (:foreground ,red-3))))
   `(header-line ((t (:foreground ,yellow-5 :background ,gray-0))))
   `(highlight ((t (:background ,gray-3))))
   `(fringe ((t (:foreground ,gray-4 :background ,gray-1))))
   `(minibuffer-prompt ((t (:foreground ,blue-5 :background ,blue-0))))
   `(mode-line ((t (:foreground ,gray-2
                                :background ,gray-8
                                :box (:line-width 1 :color "white" :style released-button)))))
   `(mode-line-inactive ((t (:foreground ,gray-8  :background ,gray-4 :box (:line-width 1 :color "black") ))))
   `(mode-line-buffer-id ((t (:foreground ,yellow-5 :weight bold))))
   `(region ((t (:background ,gray-1))))
   `(secondary-selection ((t (:background ,gray-4))))
   `(vertical-border ((t (:foreground ,gray-8))))

   ;; apropos
   `(apropos-symbol ((,class (:height 1.3 :weight bold :foreground ,tan-4))))
   `(apropos-function-button ((,class (:foreground ,blue-4))))
   `(apropos-variable-button ((,class (:foreground ,blue-4))))
   `(apropos-misc-button ((,class (:foreground ,green-4))))
   
   ;; comint
   `(comint-highlight-prompt ((t (:foreground ,blue-4 :weight bold :background ,gray-2))))

   ;; compilation (used for grep results)
   `(compilation-info ((t (:foreground ,green-4 :weight bold))))
   `(compilation-line-number ((t (: foreground ,blue-4))))

   ;; ediff  there are A and B faces
   `(ediff-current-diff-A ((t (:background "#405060" :foreground ,gray-8))))
   `(ediff-fine-diff-A ((t (:background "#668b8b"))))
   `(ediff-current-diff-B ((t (:background "#406050" :foreground ,gray-8))))
   `(ediff-fine-diff-B ((t (:background "#668b8b" :foreground ,gray-8))))

   `(ediff-current-diff-Ancestor ((t (:background "#405070" :foreground ,gray-8))))

   `(ediff-current-diff-C ((t (:background "#495766" :foreground ,gray-8))))
   `(ediff-fine-diff-C ((t (:background "#668b8b" :foreground ,gray-8))))

   `(ediff-even-diff-A ((t (:background ,gray-3))))
   `(ediff-even-diff-Ancestor ((t (:background ,gray-3))))
   `(ediff-even-diff-B ((t (:background ,gray-3))))
   `(ediff-even-diff-C ((t (:background ,gray-3))))

   `(ediff-odd-diff-A ((t (:background ,gray-3))))
   `(ediff-odd-diff-Ancestor ((t (:background ,gray-3))))
   `(ediff-odd-diff-B ((t (:background ,gray-3))))
   `(ediff-odd-diff-C ((t (:background ,gray-3))))

   `(ediff-fine-diff-Ancestor ((t (:background "#668b8b" :foreground ,gray-8))))

   ;; eshell
   `(eshell-prompt ((t (:foreground ,yellow-5 :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,red-5))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment))))
   `(eshell-ls-directory ((t (:foreground ,blue-4 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,red-4 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreliground ,gray-8))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning))))
   `(eshell-ls-product ((t (:inherit font-lock-doc))))
   `(eshell-ls-special ((t (:foreground ,yellow-5 :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,cyan-2 :weight bold))))

   ;; ess
   `(ess-debug-current-debug-line-face ((t (:background ,gray-1)))) ; region
   `(ess-tracebug-last-input-fringe-face ((t (:foreground ,gray-8  :overline ,gray-8 :background ,gray-1))))

   ;; flyspell Note, flyspell-incorrect inherits from error face
   `(flyspell-incorrect ((t (:inherit unspecified :underline "#ff7bbb"))))
   `(flyspell-duplicate ((t (:inherit unspecified :underline ,orange-5))))

   ;; font-lock
   ;; In R, library (constant), function (function), TRUE (type)
   ;;       <- (constant)
   `(font-lock-builtin-face ((t (:foreground ,blue-5))))
   `(font-lock-comment-face ((t (:foreground ,green-4))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,green-4))))
   `(font-lock-constant-face ((t (:foreground ,yellow-3))))
   `(font-lock-doc-face ((t (:foreground ,green-5))))
   `(font-lock-doc-string-face ((t (:foreground ,blue-5))))
   `(font-lock-function-name-face ((t (:foreground ,orange-5))))
   `(font-lock-keyword-face ((t (:foreground ,blue-5))))
   `(font-lock-negation-char-face ((t (:foreground ,gray-8))))
   `(font-lock-preprocessor-face ((t (:foreground ,blue-5))))
   `(font-lock-string-face ((t (:foreground ,red-5))))
   `(font-lock-type-face ((t (:foreground ,yellow-5 :weight bold))))
   `(font-lock-variable-name-face ((t (:foreground ,orange-4))))
   `(font-lock-warning-face ((t (:foreground ,yellow-1 :background "black" :underline t))))

   ;; grep
   `(grep-context-face ((t (:foreground ,gray-8))))
   `(grep-error-face ((t (:foreground ,red-2 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,blue-5))))
   `(grep-match-face ((t (:foreground ,orange-3 :weight bold))))
   `(match ((t (:background ,gray-1 :foreground ,orange-3 :weight bold))))

   ;; ido
   `(ido-first-match ((t (:foreground ,green-5 :background ,green-1 :weight bold))))
   `(ido-only-match ((t (:foreground ,green-5 :background ,green-1 :weight bold))))
   `(ido-subdir ((t (:foreground ,yellow-5))))

   ;; isearch
   `(isearch ((t (:foreground ,green-5 :background ,green-1))))
   `(lazy-highlight ((t (:foreground ,blue-5 :background ,blue-0))))
   `(isearch-fail ((t (:foreground ,red-5 :background ,red-0))))

   ;; ivy
   `(ivy-current-match ((t (:inherit highlight))))
   
   ;
   `(hl-line-face ((t (:background ,gray-1))))

   ;; latex
   `(font-latex-sedate-face ((t (:foreground ,orange-5))))
   `(font-latex-warning-face ((t (:foreground ,orange-1))))

   ;; markdown
   `(markdown-header-delimiter-face ((t (:foreground ,gray-8 :weight bold))))
   ;;`(markdown-metadata-value-face  ((,class (:weight bold :foreground "black"))))
   `(markdown-header-face-1 ((,class ,header1)))
   `(markdown-header-face-2 ((,class ,header2)))
   `(markdown-header-face-3 ((,class ,header3)))
   `(markdown-header-face-4 ((,class ,header4)))
 
   ;; org
   `(org-agenda-date-today
     ((t (:foreground "white" :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,gray-8 :weight bold))))
   `(org-checkbox ((t (:background ,gray-4 :foreground "white"
                                   :box (:line-width 1 :style released-button)))))
   ; for some reason, :inherit unspecified did not work right for org-date
   `(org-date ((t (:foreground ,red-5 :weight bold :underline ,blue-3))))
   `(org-deadline-announce ((t (:foreground ,red-2))))
   `(org-document-title ((t (:height 1.6 :weight bold))))
   `(org-done ((t (:bold t :weight bold :foreground ,green-5))))
   `(org-formula ((t (:foreground ,yellow-3))))
   `(org-headline-done ((t (:foreground ,green-5))))
   `(org-hide ((t (:foreground ,gray-1))))
   `(org-level-1 ((,class ,header1)))
   `(org-level-2 ((,class ,header2)))
   `(org-level-3 ((,class ,header3)))
   `(org-level-4 ((,class ,header4)))
   `(org-level-5 ((t (:foreground ,cyan-2))))
   `(org-level-6 ((t (:foreground ,green-3))))
   `(org-level-7 ((t (:foreground ,red-2))))
   `(org-level-8 ((t (:foreground ,blue-3))))
   `(org-link ((t (:foreground , blue-4 :underline t))))
   `(org-scheduled ((t (:foreground ,green-5))))
   `(org-scheduled-previously ((t (:foreground ,red-2))))
   `(org-scheduled-today ((t (:foreground ,blue-5))))
   `(org-special-keyword ((t (:foreground ,yellow-4))))
   `(org-table ((t (:foreground ,green-5))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,orange-3))))
   `(org-todo ((t (:bold t :foreground ,red-3 :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,red-3 :weight bold))))

   ;; outline
   `(outline-8 ((t (:inherit default))))
   `(outline-7 ((t (:inherit outline-8 :height 1.0))))
   `(outline-6 ((t (:inherit outline-7 :height 1.0))))
   `(outline-5 ((t (:inherit outline-6 :height 1.0))))
   `(outline-4 ((t (:inherit outline-5 :height 1.0))))
   `(outline-3 ((t (:inherit outline-4 :height 1.0))))
   `(outline-2 ((t (:inherit outline-3 :height 1.0))))
   `(outline-1 ((t (:inherit outline-2 :height 1.0))))

   ;; paren
   `(show-paren-mismatch  ((,class ,paren-unmatched)))
   `(show-paren-match ((,class ,paren-matched)))

   ;; rainbow-delimiters.  Max 9 colors? purple-2 red-4 yellow-4
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,red-4 :weight bold))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,yellow-3 :weight bold))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,orange-4 :weight bold))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,blue-4 :weight bold))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,tan-2 :weight bold))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,green-4 :weight bold))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,purple-3 :weight bold))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,cyan-1 :weight bold))))
   `(rainbow-delimiters-mismatched-face ((,class ,paren-unmatched)))
   `(rainbow-delimiters-unmatched-face ((,class ,paren-unmatched)))

   ;; which
   `(which-func ((t (:foreground , purple-2))))

)

(custom-theme-set-variables
 'zen
 '(ansi-color-names-vector
   [gray-2 red-3 green-4 yellow-5 blue-5 purple-2 cyan-2 gray-8])))

(provide-theme 'zen)

;; zen-theme.el ends here.

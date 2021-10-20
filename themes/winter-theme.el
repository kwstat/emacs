;; winter-theme.el
;; A dark on light-green theme.

;; MIT license. Copyright (c) Kevin Wright.

(require 'autothemer)
;; https://github.com/jasonm23/autothemer

(autothemer-deftheme
 winter "The colors of winter"
 ((((class color) (min-colors #xFFFFFF)))

  ;; Define colors
  (gray-9   "#eeeeee")
  (gray-8   "#dddddd")
  (gray-7   "#bbbbbb")
  (gray-6   "#999999")
  (gray-5   "#777777")
  (gray-4   "#555555")
  (gray-3   "#444444")
  (gray-2   "#333333") ; zen background
  (gray-1   "#222222")
  (gray-0   "#111111")
  (red-5    "#f0c0d0") ; pink = #ffc0b0
  (red-4    "#d797a0")
  (red-3    "#c07070")
  (red-2    "#a74040")
  (red-1    "#901010")
  (red-0    "#500000") ; Others: magenta
  (orange-5 "#ffd080")
  (orange-4 "#ffc040")
  (orange-3 "#f0a720")
  (orange-2 "#e09000") ; peru = #cd853f
  (orange-1 "#ff7000")
  (orange-0 "#b03000")
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
  (green-0  "#004000") ; Others: olivedrab = #6b8e23 lime "#b0f010" olive
  (cyan-2   "#90f0f0")
  (cyan-1   "#00ffff") ; cyan
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
  (purple-0 "#400050")
  (tan-5    "#fff0c0") ; cornsilk = #fff8dc
  (tan-4    "#ead0a0") ; wheat = #f5deb3
  (tan-3    "#d5b080")
  (tan-2    "#c09060") ; peru = #cd853f
  (tan-1    "#947249")
  (tan-0    "#573700") ; saddlebrown = #8b4513 
  )

 ;; Customize faces
 (  
  ;; Basic UI
  (default (:foreground gray-0 :background "#d9e9ff")) ; cee7ff
  (button  (:foreground blue-1 :underline t))
  (cursor  (:foreground gray-8 :background gray-2))
  (fringe  (:foreground blue-3 :background blue-4))
  (minibuffer-prompt  (:foreground blue-1 :background blue-5))
  (mode-line  (:foreground blue-5 :background blue-2
               :box (:line-width 1 :color blue-3 )))
  (mode-line-inactive  (:foreground blue-2 :background gray-8))
  (region  (:background blue-4))

  ;; comint
  (comint-highlight-prompt (:foreground blue-1 :bold t))

  ;; dired
  (dired-filetype-plain    (:foreground gray-1))
  (dired-filetype-source   (:foreground tan-0))

  ;; eshell
  (eshell-prompt           (:foreground "#901000" :bold t))
  
  ;; ess
  (ess-debug-current-debug-line-face (:background green-4)) ; region
  
  ;; flyspell
  (flyspell-incorrect (:underline "red"))
  (flyspell-duplicate (:underline orange-5))
  
  ;; font-lock
  (font-lock-builtin-face           (:foreground  blue-2))
  (font-lock-comment-face           (:foreground gray-4 :italic t))
                                        ;(font-lock-comment-delimiter-face (:foreground gray-4))
  (font-lock-constant-face          (:foreground green-1))
  (font-lock-doc-face               (:foreground red-0))
  (font-lock-doc-string-face        (:foreground "#7285b7"))
  (font-lock-function-name-face     (:foreground blue-0))
  (font-lock-keyword-face           (:foreground purple-1 :bold t))
  (font-lock-string-face            (:foreground green-1))
  (font-lock-type-face              (:foreground tan-2 :bold t))
  (font-lock-variable-name-face     (:foreground "saddlebrown" :bold t))
  (font-lock-warning-face           (:foreground red-1 :bold t))
  (hl-line                          (:background "#b5ba99"))
  (linum                            (:background "peru" :foreground "#ffffff"))
  (compilation-error                (:foreground red-1 :background red-5 :bold t))
  
  ;; ido
  (ido-first-match (:foreground green-0 :background green-5 :bold t))
  (ido-only-match  (:foreground green-0 :background green-5 :bold t))
  (ido-subdir      (:foreground red-0))
  
  ;; isearch
  (isearch        (:foreground green-0 :background green-5)) ; first match
  (lazy-highlight (:foreground blue-0 :background blue-5)) ; other matches
  (isearch-fail   (:foreground red-0 :background red-5))
  
  ;; latex
  (font-latex-sedate-face  (:foreground orange-2))
  (font-latex-warning-face (:foreground red-1))
  
  ;; markdown
  (markdown-header-delimiter-face (:foreground gray-3 :bold t))
  (markdown-metadata-value-face   (:foreground "black":bold t ))
  (markdown-header-face-1         (:foreground "#0000f0" :background "#cee7ff" :height 1.4 :bold t))
  (markdown-header-face-2         (:foreground "#006000" :background "#dbf7c0" :height 1.3 :bold t))
  (markdown-header-face-3         (:foreground "#901000" :background "#f0c0d0" :height 1.2 :bold t))
  (markdown-header-face-4         (:foreground "#573700" :background "#fff0c0" :height 1.1 :bold t))
  
  ;; paren
  (show-paren-match    (:background "#bbdaff" :foreground "#00346e"))
  (show-paren-mismatch (:background "#ffc58f" :foreground "#00346e"))
  
  ;; rainbow-delimiters
  (rainbow-delimiters-depth-1-face (:foreground gray-1 :bold t))
  (rainbow-delimiters-depth-2-face (:foreground red-1 :bold t))
  (rainbow-delimiters-depth-3-face (:foreground blue-1 :bold t))
  (rainbow-delimiters-depth-4-face (:foreground green-1 :bold t))
  (rainbow-delimiters-depth-5-face (:foreground orange-1 :bold t))
  (rainbow-delimiters-depth-6-face (:foreground purple-1 :bold t))
  (rainbow-delimiters-depth-7-face (:foreground tan-1 :bold t))
  (rainbow-delimiters-depth-8-face (:foreground blue-3 :bold t))
  (rainbow-delimiters-depth-9-face (:foreground red-4 :bold t))
  (rainbow-delimiters-unmatched-face (:foreground red-5 :background red-0))
  
  ;; which
  (which-func (:foreground  purple-5))
  
  )
 (custom-theme-set-variables
  'winter
  
  '(ansi-color-names-vector
    ["#00346e" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#bbdaff" "#ffffff"]))
 )

(provide-theme 'winter)

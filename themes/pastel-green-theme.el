;; pastel-green-theme.el -- A base16 colorscheme

(require 'pastel-themes)

;; Define the theme
(deftheme pastel-green)

;; Add all the faces to the theme
(base16-theme-define 'pastel-green pastel-green-colors)

;; Mark the theme as provided
(provide-theme 'pastel-green)

(provide 'pastel-green-theme)

;;; pastel-green-theme.el ends here

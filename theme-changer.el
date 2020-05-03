;; theme-changer.el

;; MIT license. Copyright (c) Kevin Wright.

;; This package is used to cycle through a set of themes.
;; Note: In polymode, the indirect buffers are not correctly re-themed.

;; Similar packages on MELPA:
;;   cycle-themes: very similar, but it requires themes to be packages
;;   theme-looper: has a better design
;; In 2019 I modified this to use an 'index' into the list of themes, because I
;; couldn't have theme-changer-theme-list be both a defcustom variable AND modify
;; it inside a function.

;; (custom-available-themes) ;; return a list of available custom themes

(defcustom theme-changer-theme-list (custom-available-themes)
  "List of themes to cycle through"
  :group 'theme-changer)


(defun theme-changer-disable-all-themes ()
  "Disable all the enabled themes"
  (interactive)
  (mapcar 'disable-theme custom-enabled-themes))


(defun theme-changer-reload-theme ()
  "Disable and enable the currently used theme."
  (interactive)
  (let ((theme-changer-current-theme (first custom-enabled-themes)))
    (theme-changer-disable-all-themes)  
    (load-theme theme-changer-current-theme t) ) )


(defun theme-changer-get-current-theme ()
  "Return the first currently enabled theme"
  ;;(interactive)
  (first custom-enabled-themes)) ;; built-in. loaded themes


(defun theme-changer-get-current-theme-index ()
  "Find index of currently enabled theme in theme-changer-theme-list"
  ;;(interactive)
  (cl-position (theme-changer-get-current-theme)
               theme-changer-theme-list :test #'equal))


(defun theme-changer-load-theme (theme)
  "Prompt for theme name. Similar to built-in `load-theme' except disable the current theme first."
  (interactive
   (list (intern (completing-read
                  "Load custom theme: "
                  (mapcar #'symbol-name (custom-available-themes))))))
  (theme-changer-disable-all-themes)
  (load-theme theme t) ; t disables lisp warning about executing code
  (message "Loaded theme: '%S'." theme))


(defun theme-changer-get-next-theme-index ()
  "Return the index of the next color-theme in the list"
  (let ((theme-changer-current-theme-index (theme-changer-get-current-theme-index)))
    (cond
     ;; if current theme is not in the list, return 0
     ((equal theme-changer-current-theme-index
	     'nil)
      0)
     ;; if current theme is last, return 1
     ((equal theme-changer-current-theme-index
	     (- (length theme-changer-theme-list)
		1))
      0)
     ;; otherwise, add 1
     ((+ 1
         theme-changer-current-theme-index)))))


(defun theme-changer-enable-next-theme ()
  "Enables the next color-theme in theme-changer-theme-list"
  (interactive)
  ;;(let ((theme-changer-next-theme (theme-changer-get-next-theme)))
  ;;(theme-changer-enable-theme theme-changer-next-theme)))
  (theme-changer-enable-theme (nth (theme-changer-get-next-theme-index) theme-changer-theme-list)))

(defun theme-changer-enable-theme (theme)
  "Enables the specified color-theme"
  (theme-changer-disable-all-themes)
  (load-theme theme t)
  (message "Switched to theme: %s" theme))


(provide 'theme-changer)

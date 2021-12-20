;; kw-diary.el

;; MIT license. Copyright (c) Kevin Wright.

;; Personal diary mode for Kevin Wright

(require 'calendar)

(defvar kw-diary-font-lock-keywords
  (list
   ;; Date + Day
   '("^[0-9\\.]* \\(Sun\\|Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\)" . font-lock-constant-face)
   ;; Strings
   '("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" . font-lock-string-face)
   ;; Travel
   `(,(regexp-opt '("Flew:" "Tent:" "Camper:" "Car camp:")) . font-lock-function-name-face)
   ;; Entertainment
   `(,(regexp-opt '("Book:" "Audiobook:" "Ebook:" "Concert:" "Play:"
                    "Sport:" "Movie:" "TV:" "Video:") ) . font-lock-keyword-face)
   ;; Activity
   `(,(regexp-opt '("Bike:" "Cook:" "Conf:" "Game night:")) . font-lock-variable-name-face)
   )
  "Syntax highlighting for kw-diary" )

; Derive from text-mode
(define-derived-mode kw-diary-mode  text-mode "Diary"
   "Kevin's diary mode."
   (set (make-local-variable 'font-lock-defaults)
        '(kw-diary-font-lock-keywords))   )

;; The original diary-make-entry appended diary entries at the end of
;; the buffer.  I want to insert in a specific place.
(defun kw-diary-insert-date-string (string)
  "Insert date string like 'dd.mm.yyyy day' in diary file."
  (interactive "p")

  ;; Move cursor to calendar window so that find-file-other-window doesn't
  ;; cause the calendar window to disappear or move
  (select-window (get-buffer-window "*Calendar*"))
  (find-file-other-window
   (substitute-in-file-name diary-file))

  ;; Jump to end of buffer, go back one line at a time
  (goto-char (point-max))
  
  (defvar kw-diary-loopdone) ; needed to stop compile warnings
  (setq kw-diary-loopdone nil)
  (while (and (not (bobp)) (not kw-diary-loopdone))
    (cond

     ;; If blank line, go back
     ((looking-at "[ \t]*$")
      (forward-line -1))
         
      ;; If the chosen date exists at point, move cursor after date.
      ((string= (buffer-substring (point) (+ 14 (point))) string )
        (progn
          (forward-char 16)
          (recenter)
          (setq kw-diary-loopdone t)))

      ;; If string date < greater than the current line, go backward
      ((string< string (buffer-substring (point) (+ 14 (point))) )
       (forward-line -1))
      
      ;; If string date > current line, forward 1 line, insert date here
      ((not (string< string (buffer-substring (point) (+ 14 (point))) ))
       (progn
         ;(previous-logical-line 1) ; b/c of wrapped visual line mode
         ;(move-beginning-of-line)
         (forward-line)
         (insert string "  \n")
         (backward-char 1)
         (recenter)
         (setq kw-diary-loopdone t)))
    )
   )
)

(defun kw-diary-insert-date ()
  "Insert a date string into diary for the date indicated by point."
  (interactive)
  ;;(calendar) ; calendar will already be open
  (kw-diary-insert-date-string (calendar-date-string (calendar-cursor-to-date t) t nil) ))

(defun kw-diary-insert-today ()
  "Insert a date string into diary for today's date."
  (interactive)
  (calendar) ; in case it is not already open
  (kw-diary-insert-date-string (calendar-date-string (calendar-current-date) t nil)  ))

(provide 'kw-diary)

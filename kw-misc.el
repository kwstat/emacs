;; kw-misc.el

;; MIT license. Copyright (c) Kevin Wright.

;; Miscellaneous personal functions for Kevin Wright

;; ---------------------------------------------------------------------------

;; https://stackoverflow.com/questions/25408349/multikeys-shortcuts-in-emacs

;; Capital letters are also allowed
(defun kw-bookmark (choice)
  "Choices for directories and files."
  (interactive "c[a]ccounts | [d]ownloads | [t]odo.md | [g]oals")
    (cond
     ((eq choice ?a)
      (find-file "c:/one/misc/accounts.md"))
     ((eq choice ?d)
      (dired "c:/Users/Wrightkevi/Downloads"))
     ((eq choice ?t)
      (find-file "c:/one/misc/todo.md"))
     ((eq choice ?g)
      (find-file "c:/x/job/2021_goals.Rmd"))
     (t (message "Quit"))))

;; ---------------------------------------------------------------------------

;; Add 'fixme' and 'todo' to the watchwords we want highlighted in files.
;; https://github.com/areina/emacs.d/blob/master/custom/01lookandfeel.el
(defun kw-add-watchwords ()
  "Highlight FIXME, and TODO in buffers."
  (font-lock-add-keywords
   nil '(("\\<\\(fixme\\|FIXME\\|todo\\|TODO\\)\\>"
          1 '((:foreground "#000000" :background "yellow") (:weight bold)) t))))
;; another version here
;; https://github.com/tarsius/hl-todo

;; ---------------------------------------------------------------------------

;; https://emacs.stackexchange.com/questions/3441/removing-smart-quotes-automatically
(defun kw-cleanup (beg end)
  "Replace smart quotes and other non-ascii characters with ascii."
  (interactive "r")
  (save-excursion
    (goto-char (point-min))
    (format-replace-strings
     '(("—" . "--")
       ("–" . "--")
       ("“" . "\"")
       ("”" . "\"")
       ("’" . "'")
       ("‘" . "'")
       ("…" . "...")
       ("©" . "(c)")) nil beg end)))

;; ---------------------------------------------------------------------------

;; Note: Emacs 28.1 has make-separator-line, M-: (insert (make-separator-line 70))
;; It is only a line of dash, without leading comment character

;; comment-char is only ";" in elisp
;; comment-add is 1, which means add an extra comment character at start

(defun kw-line-of-dash()
 (interactive)
 (beginning-of-line)
 (insert (make-string 75 ?-) )
 (comment-or-uncomment-region (line-beginning-position) (line-end-position))
 (insert "\n")  )

(defun kw-line-of-doubledash()
 (interactive)
 (beginning-of-line)
 (insert (make-string 75 ?=) )
 (comment-or-uncomment-region (line-beginning-position) (line-end-position))
 (insert "\n")  )

;; (defun kw-line-of-dash()
;;   "Insert a comment-line of dashes, conditional on the major mode."
;;   (interactive)
;;   (beginning-of-line)
;;   (let ((left
;;          (cond ((member mode-name '("ELisp")) ";; ")
;;                ((member mode-name '("ESS[R]" "Python")) "# ")
;;                ((member mode-name '("Text" "Markdown")) "")
;;                (t "") )) )
;;     (insert (concat left
;;                     (make-string (- 78 (length left)) ?-)
;;                     "\n"))))

;; (defun kw-line-of-underscore()
;;   "Insert a comment-line of underscores, condtional on the major mode."
;;   (interactive)
;;   (beginning-of-line)
;;   (let ((left
;;          (cond ((member mode-name '("Emacs-Lisp" "Lisp Interaction")) ";; ")
;;                ((member mode-name '("ESS[R]")) "# ")
;;                ((member mode-name '("Text")) "")
;;                (t "") )) )
;;     (insert (concat left
;;                     (make-string (- 78 (length left)) ?_)
;;                     "\n"))))

;; ---------------------------------------------------------------------------

;; Modified from
;; https://github.com/remyferre/comment-dwim-2

(defun kw-comment-dwim (&optional arg)
  "Replacement for the comment-dwim command.
   If current line is blank, insert line of dash.
   If current line is non-blank and we are not at end of line, then comment line.
   If region is active and is not all comments, then comment-region.
   If region is active and is all comments, then uncomment-region."
  (interactive "*P")
  (comment-normalize-vars)
  ;; no region, AND blank line
  (if (and (not (region-active-p))
           (looking-at "[ \t]*$"))
      (kw-line-of-dash)
  ;; no region, non-blank line
  (if (and (not (region-active-p))
           (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
)

;; ---------------------------------------------------------------------------

(defun kw-match-paren (arg)
  "Jump to the matching parenthesis if on parenthesis.
   On '(', press C-( or C-)"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;; ---------------------------------------------------------------------------

;; Taken from
;; http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
(defun kw-new-buffer ()
  "Open a new empty buffer named 'untitled'."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t) ; prompt to save when closing buffer
    (r-mode)
    ))

;; ---------------------------------------------------------------------------

;; Wanted: Use ivy-completing-read for these

;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
;; https://emacs.stackexchange.com/questions/36299/move-file-associated-with-open-buffer-to-a-specific-folder
(defun kw-move-file-and-buffer (new-dir)
 "Moves both current buffer and file it is visiting to DIR."
 (interactive "sNew directory: ") ;; put readline result into new-dir
 (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (new-dir (directory-file-name new-dir))
        (newname (expand-file-name name new-dir)))
   ;; body of let
   (if (not filename)
       (message "Buffer '%s' is not visiting a file!" name)
     (progn (copy-file filename newname 1)
            (delete-file filename)
            (set-visited-file-name newname)
            (set-buffer-modified-p nil) 	t))))

;; From BBatsov
;;(defun crux-rename-file-and-buffer ()
(defun kw-rename-file-and-buffer()
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; ---------------------------------------------------------------------------

(defun kw-swap-windows (arg)
  "Swap the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; ---------------------------------------------------------------------------

(defun kw-toggle-kbd-macro ()
  (interactive)
  "Start or end a keyboard macro."
  (if defining-kbd-macro ( end-kbd-macro() )  ( start-kbd-macro())  )  )

;; ---------------------------------------------------------------------------

(defun kw-show-face-at-point (pos)
  "Show (in minibuffer) the name of the face at point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; ---------------------------------------------------------------------------

;; https://www.emacswiki.org/emacs/InsertingTodaysDate
(defun kw-insert-iso-date ()
  (interactive)
  ;;(insert (format-time-string "%Y-%m-%dT%H:%M:%S")))
  (insert (format-time-string "%Y-%m-%d")))

(provide 'kw-misc)


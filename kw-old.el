;; old.el

;; MIT license. Copyright (c) Kevin Wright.

;; Snippets of elisp I no longer use.

;; ---------------------------------------------------------------------------

;; Macro to conditionally execute for Windows or linux
(defmacro Windows (&rest x)
  (list 'if (string-match "windows-nt" (prin1-to-string system-type))
        (cons 'progn x)))
(defmacro Linux (&rest x)
  (list 'if (string-match "gnu/linux" (prin1-to-string system-type))
        (cons 'progn x)))

;; Set the default directory, lisp directory
(Windows
  (setq drop-dir "c:/Dropbox/")
  (setq default-directory "c:/x/"))
(Linux
  (setq drop-dir "/home/kw/Dropbox/")
  (setq default-directory "/home/kw/"))

;; Make copy-paste work between firefox/emacs, for example
(Linux
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; ---------------------------------------------------------------------------

;; Allow home-home to jump beginning of buffer

(defvar kw-double-key-timeout 0.25
  "The number of seconds to wait for a second key press.")

(defun kw-end-key ()
  "Move to the end of the current line on the first key stroke,
and to the end of the buffer if there is a second key stroke
within `kw-double-key-timeout' seconds."
  (interactive)
  (let ((last-called (get this-command 'my-last-call-time)))
    (if (and (eq last-command this-command)
             last-called
             (<= (time-to-seconds (time-since last-called))
                 kw-double-key-timeout))
        (goto-char (point-max)) ; end of buffer
      (move-end-of-line nil)))
  (put this-command 'my-last-call-time (current-time)))

(defun kw-home-key ()
  "Move to the beginning of the current line on the first key stroke,
and to the beginning of the buffer if there is a second key stroke
within `kw-double-key-timeout' seconds."
  (interactive)
  (let ((last-called (get this-command 'my-last-call-time)))
    (if (and (eq last-command this-command)
             last-called
             (<= (time-to-seconds (time-since last-called))
                 kw-double-key-timeout))
        (goto-char (point-min)) ; beginning of buffer
      (move-beginning-of-line nil)))
  (put this-command 'my-last-call-time (current-time)))

;; ---------------------------------------------------------------------------

;; Needed for linux
(use-package scroll-bar
  :defer t
  :config (set-scroll-bar-mode 'right))

;; ---------------------------------------------------------------------------

;; Use Ghostview for printing on Windows

(use-package printing
  :init
  (setq pr-gv-command "c:/Progra~1/Ghostgum/gsview/gsview64.exe"))
  
(setq pr-gv-command "c:/Progra~1/Ghostgum/gsview/gsview64.exe")
(setq preview-gs-command "C:/Progra~1/gs/gs8.64/bin/gswin32c.exe")
(require 'printing)

;; Faces for postcript printing dark on light
;; (face-name foreground background extension)
(ps-extend-face '(font-lock-builtin-face "darkslateblue" nil nil) 'MERGE)
(ps-extend-face '(font-lock-comment-face "DarkGreen" nil nil) 'MERGE)
(ps-extend-face '(font-lock-comment-delimiter-face "DarkGreen" nil nil) 'MERGE)
(ps-extend-face '(font-lock-constant-face "brown" nil nil) 'MERGE)
(ps-extend-face '(font-lock-doc-face "MediumSeaGreen" nil nil) 'MERGE)
(ps-extend-face '(font-lock-function-name-face "DarkViolet" nil nil) 'MERGE)
(ps-extend-face '(font-lock-keyword-face "navy" nil nil) 'MERGE)
(ps-extend-face '(font-lock-string-face "firebrick" nil nil) 'MERGE)
(ps-extend-face '(font-lock-type-face "peru" nil nil) 'MERGE) ;; for T/F/NULL
(ps-extend-face '(font-lock-variable-name-face "saddlebrown" nil nil) 'MERGE)
(ps-extend-face '(font-lock-warning-face "red" nil nil) 'MERGE)
(ps-extend-face '(default "black" nil nil) 'MERGE)

(setq pr-faces-p t                       ; Use faces (colors) for printing
      ps-print-header-frame nil          ; Box around header
      ps-header-title-font-size '(8 . 8) ; Default: (12 . 14)
      ps-header-font-size '(6 . 6)       ; Default: (10 . 12)
      ps-font-size '(10 . 10)            ; body font size
      ps-left-header (quote (ps-get-buffer-name ps-header-dirpart))
      ps-left-margin 36                  ; Left margin in points (1/72 inch)
      ps-right-margin 36                 ; Default 57 = .8 inch
)
(pr-update-menus t)

;; ---------------------------------------------------------------------------

;; Customizations for org mode.  Now I use markdown.

(use-package org
  :mode ("\\.org$" . org-mode)
  :defer 2
  :hook (org-mode . turn-on-flyspell)
  :custom
  (org-list-allow-alphabetical t) ; allow lists like a. b. c.
  (org-startup-folded "showall")  ; never fold sections
  (org-yank-folded-subtree nil)  ; yank unfolded
  :config
  (define-key org-mode-map (kbd "<C-tab>") nil) ; unset
  (define-key org-mode-map (kbd "C-c e") 'org-time-stamp)
  (use-package ox-publish
    :custom
    (org-export-with-toc nil) ; No table of contents    
    :config
    ;; Define org to docx backend. 'html is placeholder
    ;; http://kitchingroup.cheme.cmu.edu/blog/category/docx/
    (org-export-define-derived-backend 'pandoc 'html
      :translate-alist '((template . org-pandoc-template))
      :export-block "PANDOC"
      :menu-entry
      `(?w "Export to Word via pandoc" kw-org-to-docx-and-open))
    
    (defun kw-org-to-docx-and-open
        (&optional subtreep visible-only body-only ext-plist pub-dir)
      "Export the current org file as a docx via markdown."
      (interactive)
      (let* ((current-file (buffer-file-name))
             (basename (file-name-sans-extension current-file))
             (docx-file (concat basename ".docx")))
        (save-buffer)
        (when (file-exists-p docx-file) (delete-file docx-file))
        (shell-command (format
                        "pandoc -s -S %s -o %s" current-file docx-file))
        (org-open-file docx-file '(16)))) ; open at byte 16
    ) )

;; ---------------------------------------------------------------------------

;; These line-of-dash functions have both left-end and right-end delimiters
;; like /*-----*/, but I never need right-end delimiters anymore

(defun kw-line-of-dash()
  "Insert a commented line of dashes condtional on the major mode."
  (interactive)
  (beginning-of-line)
  (let ((left
         (cond ((member mode-name '("Emacs-Lisp" "Lisp Interaction")) ";; ")
               ((member mode-name '("ESS[R]")) "# ")
               ((member mode-name '("Text" "Markdown")) "")
               (t "") ))
        (right
         (cond ((member mode-name '("ESS[SAS]")) " ;")
               (t "") )))
    ;; After defining 'left' and 'right', now assemble the string and insert
    (insert (concat left
                    (make-string (- 78 (length left) (length right)) ?-)
                    right "\n"))))

(defun kw-line-of-underscore()
  "Insert a commented line of underscores condtional on the major mode."
  (interactive)
  (beginning-of-line)
  (let ((left
         (cond ((member mode-name '("Emacs-Lisp" "Lisp Interaction")) ";; ")
               ((member mode-name '("ESS[R]")) "# ")
               ((member mode-name '("Text")) "")
               (t "") ))
        (right
         (cond ((member mode-name '("C")) " */")
               (t "")  )))
    ;; After defining 'left' and 'right', now assemble the string and insert
    (insert (concat left
                    (make-string (- 78 (length left) (length right)) ?_)
                    right "\n"))))

;; ---------------------------------------------------------------------------

;; Why did I switch from ido to ivy???

(use-package ido
  :disabled
  :custom
  (ido-create-new-buffer 'always)
  (ido-decorations ; display vertically
   '("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]"
     " [Not readable]" " [Too big]" " [Confirm]"))
  (ido-enable-flex-matching t) ; fuzzy matching
  (ido-enable-last-directory-history nil)
  (ido-everywhere t)
  (ido-ignore-extensions t)
  (ido-last-directory-list nil) ; disable 'Update header' warning
  (ido-max-dir-file-cache 0) ; caching is unreliable on windows
  (ido-max-prospects 20)
  (ido-max-work-directory-list 0)
  (ido-record-commands nil)
  :config
  (add-to-list 'ido-ignore-buffers "\*") ; if you want scratch or R just type it
  (ido-mode 1) )

;; ---------------------------------------------------------------------------
;; I switched from desktop to minimal-session-saver

;; M-x desktop-save, desktop-revert
(use-package desktop
  :disabled
  :commands (desktop-save desktop-revert)
  :config
  ;;(setq desktop-dirname "c:/kw/.emacs.d/")
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'help-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'completion-list-mode)
  :custom
  (desktop-dirname user-emacs-directory)
  (desktop-base-file-name "emacs-desktop") )

;; ---------------------------------------------------------------------------

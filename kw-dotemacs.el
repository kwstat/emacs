;; kw-dotemacs.el

;; History
;; 2020 Markdown image functions
;; 2019 Switch from ido to ivy
;; 2018 Switch from org to markdown
;; 2016 Adopted use-package
;; 2010 Switch from muse to org
;; 2006 Removed htmlize--use printing package, added muse
;; 2002 Created diary-mode for font-locking diary file
;; 1999 Added ESS
;; 1996 Wrote font-locking for sas-mode in Emacs 19.34
;; 1992 Began using emacs

;; C-c C-e d, md export to docx
;; C-c e p, md export to pdf

;; Use M-x re-builder to test regexprs

;; leave this as nil, so as not to trigger ACL operation error
;; http://bug-gnu-emacs.gnu.narkive.com/pRQgqRm6/
;; (setq debug-on-error nil)

;; might need this for some pandoc???
;; (setq TeX-engine 'xelatex)
;; (setq latex-run-command "xelatex")

(when (version< emacs-version "27.0") (package-initialize))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; ---------------------------------------------------------------------------

(require 'use-package)
;; https://jwiegley.github.io/use-package/keywords/
;; :bind - define keybindings BEFORE loading. Defers loading.
;; :config - code executed AFTER package is loaded
;; :defer - delay seconds before loading
;; :demand - Prevent delayed loading
;; :diminish - Remove from modeline
;; :disabled - do not load
;; :ensure - Install if needed
;; :init - code executed BEFORE package loaded
;; :mode - choose file extensions for the package mode. Defers loading.
;; :commands - commands that auto-load package. Defers loading.
;; :custom - set variables

;; -------------------------------------------------------------------------

;; Set the default directory, lisp directory
(setq default-directory "c:/x/"
      dropbox-dir "c:/Dropbox/" )
(add-to-list 'load-path (concat dropbox-dir "emacs/"))

(add-to-list 'same-window-buffer-names "*grep*")
                               

(use-package emacs
  :custom
  (completion-ignored-extensions
      '("~" ".aux" ".bbl" ".blg" ".brf" ".bst" ".log" ".out" ".pdf" ".ps"
        ".doc" ".docx" ".ppt" ".pptx" ".xls" ".xlsx"))
  (completion-show-help nil)
  (completions-format 'vertical) ; Use vertical sorting, not horiz
  ;; Stop emacs from modifying my .emacs file with custom variables
  (custom-file  (concat dropbox-dir "emacs/custom.el"))
  (inhibit-splash-screen t) 
  ;;(initial-major-mode 'text-mode) ; default is lisp
  (load-prefer-newer t) ; load .el over .elc
  (max-mini-window-height 20) ; minibuffer max size
  (text-quoting-style "straight")
  (user-full-name "Kevin Wright")
  (user-mail-address "kw.stat@gmail.com")
  (vc-handled-backends nil)
  ;; Cursor has become a thin line, does not obey cursur-type variable.
  ;; https://stackoverflow.com/questions/47299632/windows-emacs-25-3-cursor-type
  ;; Happens when Emacs detects the Windows Accessibility API is active (screen
  ;; readers and speech recognition).
  (w32-use-visible-system-caret nil)
  
  :config
  ;; Add R to the path so that "Rcmd build" will work
  (setenv "PATH" (concat "c:/Progra~1/R/R-3.6.3/bin/x64/" ";" (getenv "PATH")))
  (setenv "CYGWIN" "nodosfilewarning") ; Why do I have this?
  (setenv "LANG" "en_US.UTF-8") ; To fix pandoc-cite bug in R
  ;;(setenv "SHELL" "sh")
  (setenv "SHELL" "c:/Rtools/bin/sh")
  (setq-default buffers-menu-max-size 20)
  (setq-default cursor-type 'box)
  (setq-default fill-column 78)
  (setq-default mouse-drag-copy-region t)
  (setq-default indent-tabs-mode nil) ; use spaces, not tab
  (setq-default tab-stop-list '(2 4 6))
  (setq-default tab-width 2)
  )

;; Appearance
(use-package emacs
  :custom
  (frame-title-format "%b     %f") ; titlebar: buffer name, path
  :config
  ;; Fonts
  (set-face-attribute 'default nil :family "Consolas" :height 100) ;; height / 10 = points
  ;;(set-face-attribute 'fixed-pitch nil :family "Andale Mono")
  ;;(set-face-attribute 'fixed-pitch nil :family "Anonymous Pro-12")
  ;;(set-face-attribute 'fixed-pitch nil :family "Anonymous Pro" :height 80)
  ;;(set-face-attribute 'fixed-pitch nil :family "Consolas")
  ;;(set-face-attribute 'fixed-pitch nil :family "Dina")
  (set-face-attribute 'fixed-pitch nil :family "Lucida Console" :height 90)
  ;;(setq default-frame-alist '((width . 90) (height . 32) (font . "Consolas-10")))
  (setq default-frame-alist '((width . 90) (height . 32)))
  ;; (font . "Dina-9"))) ; Only 6,9 seem to work on Windows
  ;; (font . "Liberation Mono-12") ; Linux
  
  ;; Modeline format
  (setq-default mode-line-format
                '(" %l:%C" ; line/column
                  " %b " ; buffer name
                  mode-line-modes
                  (which-func-mode (" " which-func-format "")) ))
  (tool-bar-mode -1)
  )

;; Re-define yes-or-no (like file save) to use y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; -------------------------------------------------------------------------

(use-package kw-misc
  :hook
  ((markdown-mode prog-mode) . kw-add-watchwords)  )

;; ---------------------------------------------------------------------------

(use-package autorevert
  ;; reload these files if they change on disk
  :mode ("\\.pvs$" . auto-revert-mode) ;; asreml predictions
  :mode ("c:/kw/.emacs.d/recentf" . auto-revert-mode)
  :commands (auto-revert-mode) )

;; ---------------------------------------------------------------------------

(use-package bibtex
  :mode ("\\.bib$" . bibtex-mode)
  :commands (bibtex-mode)
  :bind
  (:map bibtex-mode-map
        ("M-q" . bibtex-fill-entry) )) ; default is C-c C-q

;; ---------------------------------------------------------------------------

(use-package bookmark
  :commands (bookmark-jump bookmark-all-names)
  :bind
  ("C-c b" . bookmark-jump) ; default is C-x r b
  :custom
  (bookmark-save-flag 1) )

;; -------------------------------------------------------------------------

(use-package calendar
  :custom
  (diary-file (concat dropbox-dir "blog/diary.txt"))
  ;; When we insert a diary entry, have emacs use a date like: 08.07.2002 Thu
  (calendar-date-display-form
        '((format "%02d.%02d.%4s %2s"
                  (string-to-number month) (string-to-number day) year dayname)))
  ;; Look for dates in the diary file like this: 08.28.2002 Thu
  ;; NOTE: This MUST be a list of lists.  Otherwise list-diary-entries
  ;; will evaluate (car (car d)) and say "wrong-type-argument listp month"
  (diary-date-forms '((month "." day "." year " " dayname)
                           (day "/" month "/" year "[^0-9]")))
  :bind
  (:map calendar-mode-map
        ("id" . nil) ; insert in diary using ordinary date strings
        ;; Use "ij" to insert in diary (journal) using my date pattern
        ("ij" . kw-diary-insert-date))   )

(use-package kw-diary
  :mode ("diary.txt" . kw-diary-mode)
  :bind
  ("C-c d" . kw-diary-insert-today)
  :commands (kw-diary-insert-today kw-diary-insert-date)
  :hook (kw-diary-mode . turn-off-auto-fill))


;; ---------------------------------------------------------------------------

(use-package comint
  :commands (shell-mode)
  :config
  ;; From Tudor Hulubei
  ;; In comint modes [up] and [down] keys will use history only when
  ;; the point is at the command line. Otherwise they behave normally.
  (defun smart-comint-up ()
    "At the end of buffer, do comint-previous-matching-input-from-input,
otherwise just move in the buffer."
    (interactive)
    (let ((previous-region-status nil))
      (if (= (point) (point-max))
          (comint-previous-input 1)
        (forward-line -1)) ))
  
  (defun smart-comint-down ()
    "At the end of buffer, do comint-next-matching-input-from-input,
otherwise just move in the buffer."
    (interactive)
    (let ((previous-region-status nil))
      (if (= (point) (point-max))
          (comint-next-input 1)
        (forward-line 1)) ))

  :custom
  (comint-move-point-for-output t)
  (comint-move-point-for-output t)
  (comint-scroll-to-bottom-on-output t)
  :bind
  (:map comint-mode-map
        ("<M-up>" . counsel-shell-history) ; rstudio
        ("<up>" . smart-comint-up)
        ("<down>" . smart-comint-down)
        ("<C-left>" . comint-bol)
        ("<home>" . comint-bol)))

;; ---------------------------------------------------------------------------

;; recursive listing of directory
;; C-u C-x d   -aFR

;; colored file names in dired
(use-package dired-filetype-face)

;; this works, but is not widely used
;; (use-package dired-filetype-face
;;   ;; regexp to match rich document filetypes
;;   :config
;;   (custom-declare-face 'dired-filetype-document '((t (:foreground "blue"))) "face for rich document files " :group 'dired-filetype-face)
;;   (custom-declare-variable 'dired-filetype-document-regexp
;;                            '"^  .*\\.\\(pdf\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx)$"
;;                            '(#$ . 1780) :type 'string :group 'dired-filetype-face)
;;   )

(use-package dired
  :commands (dired)
  :config
  ;; Use same buffer instead of opening new buffers
  (put 'dired-find-alternate-file 'disabled nil)
  ; all,long,recursive,filetype (for highlighting)
  (setq dired-listing-switches "-aF") 
  :bind
  (:map dired-mode-map
        ( "C-c C-c" . wdired-change-to-wdired-mode) ) )

(use-package dired-x
  :commands (dired-omit-mode)
  :init
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  ;; we cannot set dired-omit-files in :custom
  :config
    (progn
      (setq dired-omit-verbose nil)
      ;; toggle `dired-omit-mode' with C-x M-o
      (add-hook 'dired-mode-hook #'dired-omit-mode)
      ;;(setq dired-omit-files (concat dired-omit-files "\\~$\\|^.DS_STORE$\\|^.projectile$"))
      ))

;; ---------------------------------------------------------------------------

(use-package ediff
  :commands (ediff)
  :custom
  (ediff-diff-options "-w") ) ; ignore whitespace

;; ---------------------------------------------------------------------------

(use-package eldoc
  :disabled
  ;; used by ess to show func
  :defer
  :diminish
  )

;; ---------------------------------------------------------------------------

(use-package elisp-mode
  :commands (lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-f" . eval-defun)
        ("C-c C-j" . eval-print-last-sexp) ; same as C-j
        ("C-c C-r" . eval-region))  )

;; ---------------------------------------------------------------------------
;; R stuff

(defun kw-switch-to-inferior-R-other-window ()
  "Switches to inferior R process."
  (interactive)
  (switch-to-buffer-other-window "*R*") )

(use-package ess-r-mode
  :defer t
  :mode ("\\.[rR]$" . r-mode)
  :custom
  (ess-default-style 'RStudio)
  (ess-eval-visibly 'nowait) ;; t seems to make emacs hang
  (ess-history-file nil)
  (ess-indent-with-fancy-comments nil) ; rstudio style also disables this?
  (ess-pdf-viewer-pref "c:/Progra~1/R/R-3.6.3/bin/x64/open.exe")
  ;; Simplified pattern, asreml prints WARNING to console
  (ess-R-message-prefixes '("Error" "nfault" "singularities" "Warning" "WARNING"))
  (ess-r-versions nil)
  (ess-roxy-str "#'") ; default is "##'"
  (ess-roxy-template-alist '(("title" . ".title\n#'")
                            ("description" . ".desc")
                            ("details" . ".details\n#'")
                            ("param" . "")
                            ("return" . "")
                            ("author" . "Kevin Wright")
                            ("examples" . "\n#' \\dontrun{}")
                            ("references" . "\n#' None")
                            ("export" . "")))
  (ess-tab-complete-in-script t)
  ;;(ess-use-eldoc 'script-only) ; no argument hints in inferior ess
  ;; While editing R script in Emacs 27, I get random freezes...
  ;; but still happens when eldoc is disabled
  (ess-use-eldoc nil)
  (ess-use-flymake nil) ; turn it off
  (inferior-ess-r-help-command ".ess.help('%s', help.type='text')\n" )
  (inferior-ess-r-program "c:/Progra~1/R/R-3.6.3/bin/x64/rterm.exe" )
  (inferior-R-args "--no-restore-history --no-save --quiet")
  :config
  (setq-default ess-dialect "R") ; prevent prompts about S/Stata 
  :bind
  (:map ess-mode-map
        ;; line
        ( "<M-down>" . ess-eval-line-and-step )
        ( "M-n" . ess-eval-line-and-step )
        ( "C-M-n" . ess-eval-line )
        ;; paragraph
        ( "M-p" . ess-eval-function-or-paragraph-and-step )
        ( "C-M-p" . ess-eval-function-or-paragraph )
        ;; region
        ( "M-r" . ess-eval-region )
        ;; keys similar to rstudio
        ("M--" . ess-insert-assign)
        ("M-," . ess-insert-assign)
        ("M-." . (lambda () (interactive) (insert " %>% "))) ; pipe
        ("C-S-e" . ess-r-devtools-check-package) ; devtools::check() 
        ("C-S-d" . ess-r-devtools-document-package); devtools::document() 
        ("C-S-i" . ess-r-devtools-install-package) ; devtools::install()
        ("C-S-l" . ess-r-devtools-load-package) ; devtools::load_all(".")
        ("C-S-r" . ess-r-devtools-revdep-check-package) ; devtools::revdep_check()
        ("C-S-t" . ess-r-devtools-test-package) ; devtools::test()
        ("<C-S-f10>" . inferior-ess-reload) ; similar: startup::restart
        ;;("C-S-u" . ess-r-devtools-unload-package) ; devtools::unload()
        ("C-'" . ess-roxy-toggle-roxy-region)
        ("C-{" . ess-beginning-of-function )
        ("C-}" . ess-end-of-function)
        ("C-c i l" . kw-insert-include-graphics-link)
        )    
  )

(use-package ess-inf
  :after ess-r-mode
  :bind
  (:map inferior-ess-mode-map
        ("M--" . ess-insert-assign)
        ("M-," . ess-insert-assign)
        ("M-." . (lambda () (interactive) (insert " %>% ")))
        ("C-S-e" . ess-r-devtools-check-package)
        ("C-S-d" . ess-r-devtools-document-package)
        ("C-S-i" . ess-r-devtools-install-package)
        ("C-S-l" . ess-r-devtools-load-package)
        ("C-S-r" . ess-r-devtools-revdep-check-package)
        ("C-S-t" . ess-r-devtools-test-package)
        ("<C-S-f10>" . inferior-ess-reload)
        ;;("C-S-u" . ess-r-devtools-unload-package)
        ("<C-up>" . beginning-of-buffer)
        ("<C-down>" . end-of-buffer) )
  )


(use-package ess-help
  :after ess-r-mode
  :bind
  (:map ess-help-mode-map
        ("<M-down>" . ess-eval-line-and-step))
  :commands (ess-help-mode) )

(use-package ess-rd
  ;;:mode ("\\.Rd$" . Rd-mode)
  ;;:hook (Rd-mode-hook . font-lock-mode)
  :bind
  (:map Rd-mode-map
        ;; line
        ( "<M-down>" . ess-eval-line-and-step )
        ( "M-n" . ess-eval-line-and-step )
        ( "C-M-n" . ess-eval-line )
        ;; paragraph
        ( "M-p" . ess-eval-function-or-paragraph-and-step )
        ( "C-M-p" . ess-eval-function-or-paragraph )
        ;; region
        ( "M-r" . ess-eval-region ) )
  :config
  (use-package ess-r-mode)
  (setq-local comment-start "# ")
  ;;(set (make-local-variable 'comment-start) "#")
  )

;; ---------------------------------------------------------------------------

(use-package poly-R)

;; polymode weaver: knitr vs knitr-ESS
;; https://emacs.stackexchange.com/questions/18364/
;; knitr-ESS weaver will use the CURRENT session--can use and modify objects

(use-package polymode
  :mode (;;("\\.Rd" . poly-Rd-mode)
         ("\\.Rmd" . poly-markdown+R-mode)
         )
  :custom
  (polymode-exporter-output-file-format "%s")
  :bind
  (:map polymode-mode-map
        ;; md: pandoc/docx Rmd: Rmarkdown/AUTO
        ("C-S-k" . ess-render-rmarkdown)
        ("C-`" . ess-insert-rmd-chunk)
        ("C-~" . ess-insert-inverse-rmd-chunk)
        ("C-M-i" . ess-insert-rmd-chunk) ;; like rstudio
        )
  :config
  (use-package poly-R)
  (use-package poly-markdown)
  )

;; Another version is here:
;; https://stackoverflow.com/questions/28324288/how-to-create-a-pdf-from-a-rmd-file-using-emacs-ess-pandoc-mode-and-polymode
; delhey.info/inc/ess-rmarkdown.pdf
(defun ess-render-rmarkdown ()
  "Compile R markdown .Rmd to any output type."
  (interactive)
  ;; Check if attached R-session
  (condition-case nil
      (ess-get-process)
    (error
     (ess-switch-process)))
  (basic-save-buffer) ;; save current buffer before we render
  (let* ((rmd-buf (current-buffer)))
    (save-current-buffer ;;save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (buf-coding (symbol-name buffer-file-coding-system))
             (R-cmd
              (format ".outfile=rmarkdown::render(\"%s\"); shell.exec(.outfile)"
                      buffer-file-name)))
        (message "Running rmarkdown on %s" buffer-file-name)
        (ess-execute R-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf) ; switch back to Rmd buffer
        ) )
    ))

;; Some code derived from wrap-region
;; https://github.com/rejeep/wrap-region.el/blob/master/wrap-region.el
(defun ess-insert-rmd-chunk (arg)
    "Insert R chunk delimiters for Rmd buffers"
  (interactive "*P")
  ;; no region, and blank line
  (if (and (not (region-active-p))
           (looking-at "[ \t]*$"))
      (let ((pos (point)))
            (save-excursion
              (insert "```{r}\n\n```")
              (goto-char (+ pos 3) ) ) ) ) ; does NOT move forward.  Why?
  ;; region active
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end))
            (pos (point)))
        (save-excursion
          (goto-char end)
          (forward-line) ; region-end seems to be at start of previous line ???
          (insert "```\n")
          (goto-char beg)
          (insert "```{r}\n")
          ) ) ) )

;; Some code derived from wrap-region
;; https://github.com/rejeep/wrap-region.el/blob/master/wrap-region.el
(defun ess-insert-inverse-rmd-chunk (arg)
    "Insert inverse R chunk delimiters for Rmd buffers"
  (interactive "*P")
  (insert "```\n\n```{r}")
  (goto-char (- pos 3) ) )

;; ---------------------------------------------------------------------------

(use-package files
  :custom
  (make-backup-files nil) ; no automatic backups  
  ;; do NOT expand links, also affects recentf
  (find-file-visit-truename nil)
  ;; custom abbreviations for these directories
  ;; https://emacs.stackexchange.com/questions/37253/ibuffer-hide-beginning-of-lines
  ;; https://stackoverflow.com/questions/19032166/how-to-abbreviate-path-in-emacs
  ;; (directory-abbrev-alist
  ;;  '( ("c:/Users/wrightkevi/Dropbox/kw_work" . "c:/Dropbox/")
  ;;     ("c:/Users/wrightkevi/OneDrive - Corteva" . "/xx") ) )
  )

;; ---------------------------------------------------------------------------

(use-package flyspell
  :disabled ;; makes scrolling too slow
  ;; use ispell-buffer, NOT flyspell-buffer
  :hook ( (markdown-mode . flyspell-mode)
          (text-mode . flyspell-mode) ) )

;; ---------------------------------------------------------------------------

(use-package goto-addr 
  ;; goto-addr makes hyperlinks clickable in markdown mode
  :after (:any (markdown-mode polymode-minor-mode)))

;; ---------------------------------------------------------------------------

(use-package ibuffer
  :commands (ibuffer)
  :custom
  (ibuffer-default-sorting-mode 'recency)
  (ibuffer-display-summary nil)
  ;; Remove 'modified' and 'read-only' indicators
  (ibuffer-formats '(((name 30 30 :left) " "
                      filename)))
  :hook (ibuffer-mode . ibuffer-auto-mode)) ; auto-refresh

(use-package ibuf-ext
  :after ibuffer
  :config
  (add-to-list 'ibuffer-never-show-predicates "^\\*ESS")
  ;; DONT do this...hides buffers like agridat.Rd[R]
  ;; Hide indirect polymode buffers ending in []
  ;; (add-to-list 'ibuffer-never-show-predicates ".*\\[.*\\]$")
  )

;; ---------------------------------------------------------------------------
;; show images in markdown mode

(use-package iimage 
  :commands (iimage-mode turn-on-iimage-mode))

;; ---------------------------------------------------------------------------

(use-package ispell
  :commands (ispell-buffer)
  :custom
  (ispell-program-name "c:/bin/hunspell/bin/hunspell.exe")
  :config
  (defvar limit-ispell-choices-to 10
    "Number indicating the maximum number of choices to present")
  
  (defadvice ispell-parse-output (after limit-ispell-choices activate)
    (when (and (listp ad-return-value)
               ad-return-value)
      (let* ((miss-list-end (nthcdr (- limit-ispell-choices-to 1)
                                    (nth 2 ad-return-value)))
             (guess-list-end (nthcdr (- limit-ispell-choices-to 1)
                                     (nth 3 ad-return-value))))
        (when miss-list-end (setcdr miss-list-end nil))
        (when guess-list-end (setcdr guess-list-end nil))))))
        
;; ---------------------------------------------------------------------------
;; ivy is a completion framework
;; counsel provides most of the user-facing functions
;; swiper is used for searching the current buffer
;; https://oremacs.com/2019/06/27/ivy-directory-improvements/

;; C-M-j ivy-immediate-done  create file???
;; C-j or TAB TAB - descend into directory
(use-package ivy
  :demand
  :diminish
  :custom
  (ivy-count-format "%d/%d ") ; show current/total count
  (ivy-height 15)    ; number of results shown
  ;; add recentf-mode and bookmarks to ‘ivy-switch-buffer’.
  (ivy-use-virtual-buffers t)
  :custom-face
  (ivy-current-match ((t (:foreground "yellow" :background "#006000" :weight bold))))
  :config
  ;;    ivy--regex
  ;;    ivy--regex-plus ; default, split by spaces
  ;;    ivy--regex-ignore-order
  ;;    ivy--regex-fuzzy ; uses every character
  ;;    regexp-quote
  ;; use standard match for swiper, fuzzy match elsewhere
  (setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-plus))) ; ivy--regex-fuzzy
  (ivy-mode 1)  )

(use-package counsel
  :bind
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-x C-f" . counsel-find-file)
  ("C-x f" . counsel-find-file)
  ("M-x" . counsel-M-x)
  (:map ivy-minibuffer-map
        ("C-r" . counsel-minibuffer-history)
        ("<left>" . ivy-backward-kill-word) ; up directory
        ("<right>" . ivy-alt-done) ; descend into directory
        ("<return>" . ivy-alt-done) ; descend into directory
        ("<C-return>" . ivy-done) ; open dired
        )
  :custom
  (counsel-find-file-ignore-regexp
   (concat
    ;; start of filename = \\`
    "\\(\\`#\\|\\`\\.\\)\\|"
    ;; end of filename = \\'
    "\\(~\\'\\|docx\\'\\|pdf\\'\\|pptx\\'\\|xls\\'\\|xlsx\\'\\)"))
  )

(use-package swiper
  :ensure t
  :after ivy
  :bind
  ("C-s" . swiper)
  )

;; -------------------------------------------------------------------------
;; key bindings (global)
;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/
;; Use C-h k, press the key combination.  Use this in bind-key

;; function keys
(unbind-key "<f2>") ; I accidentally hit this
(bind-keys ("<f1>" . ibuffer)
           ("<M-f4>" . kill-this-buffer)
           ("<f3>" . query-replace)
           ("<S-f3>" . replace-string)
           ("<C-f3>" . replace-regexp)
           ("<f6>" . kw-comment-dwim)
           ;; ("<C-S-c>" . kw-comment-dwim) ; rstudio
           ("<S-f6>" . kw-line-of-underscore)
           ("<f8>" . kw-switch-to-inferior-R-other-window)
           ("<S-f8>" . eshell)
           ("<f9>" . call-last-kbd-macro)  ; Same as C-x e
           ("<C-f9>" . kw-toggle-kbd-macro)
           ("<f10>" . fill-region)
           ("<S-f10>" . unfill-region)
           ("<C-f10>" . auto-fill-mode) )

;; window management
(bind-keys ("C-=" . text-scale-increase)
           ("C--" . text-scale-decrease)
           ("C-+" . enlarge-window)
           ("C-_" . shrink-window)
           ("<C-S-up>" . enlarge-window)
           ("<C-S-down>" . shrink-window)
           ("<C-S-right>" . enlarge-window-horizontally)
           ("<C-S-left>" . shrink-window-horizontally))

;; arrow keys
(bind-keys ("<C-left>" . beginning-of-line)
           ("<C-right>". end-of-line)
           ("<C-up>"   . beginning-of-buffer)
           ("<C-down>" . end-of-buffer)
           ("<S-down>". scroll-up-line)
           ("<S-up>". scroll-down-line) )

;; misc
(bind-keys ("C-f" . isearch-forward) ; like MS programs
           ("C-h a" . apropos) ; Default is apropos-command
           ("C-o" . other-window)
           ("C-v" . yank)  ; Like MS windows paste
           ("C-z" . undo) ) ; Like MS Windows undo
(unbind-key "C-t") ; I accidentally hit this

;; kw 
(bind-keys ("C-c m" . kw-move-file-and-buffer)
           ("C-c n" . kw-new-buffer)
           ("C-c r" . kw-rename-file-and-buffer)
           ("C-c s" . kw-swap-windows)
           ("C-c ?" . kw-show-face-at-point)
           ("C-c (" . kw-match-paren)
           ("C-c )" . kw-match-paren)
           )

;; ---------------------------------------------------------------------------

;; key tips.  Inspired by prelude-mode
(defvar kw-tips
  '("C-x r l bookmaRks List"
    "C-x r m bookmaRks Mark"
    "C-c b bookmark jump"
    "C-c f ivy find recent file."
    "C-c s swap buffers."
    "C-c t TODO file."
    "C-c ? show the face at point."
    "C-c ( jump to the matching parenthesis."
    "C-c ) jump to the matching parenthesis."
    "C-c C-i i markdown insert image link"
    "C-o jump to the Other window."
    "C-u C-SPC jump to previous mark."
    "C-u C-x = show text properties at point."
    "C-y M-y yank previous kills."
    "M-w copy region"
    "C-` to insert Rmd chunk"
    "M-nr to render, then open Rmd file"
    ;; defaults I need to learn
    "C-h m describe-mode"
    "C-x d dired"
))

(defun kw-tip-of-the-day ()
  "Display a random entry from `kw-tips'."
  (interactive)
  (unless (window-minibuffer-p)
    (random t) ;; pick a new random seed
    (message "%s" (propertize
     (concat "Tip: " (nth (random (length kw-tips)) kw-tips))
     ;;'face '(:background "yellow" :foreground "black")))))
     'face '(:inherit warning)))))

;; ---------------------------------------------------------------------------

;; Note: We use key-combo because key-chord does not support using <home>

(use-package key-combo
  :hook ((ess-mode inferior-ess) . key-combo-mode)
  :demand ;; no delay
  :diminish
  :config
  (key-combo-mode t)
  (setq key-combo-ess
        '((">"  . (" > " " %>% " " -> "))
          ("<>" . " %<>% ")
          ("~" . " ~ ")
          ("_" . ("_" " <- "))
          ;; without the following line, C-h k says <home> is mapped to comint-bol, but
          ;; actually executes beginning-of-visual-line. Weird.
          ("<home>" . (comint-bol beginning-of-buffer key-combo-return))
        ))
  (key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook) ; hooks
                         'ess-key-combo-load-default ; name
                         key-combo-ess) ; keys
  ;; global
  (global-key-combo-mode t)
  (setq key-combo-kw-global
        '(;;("<f4>" . (ignore kill-this-buffer))
          ("C-a" . (beginning-of-visual-line beginning-of-buffer key-combo-return))
          ("<home>" . (beginning-of-visual-line beginning-of-buffer key-combo-return))
          ("C-e" . (end-of-visual-line end-of-buffer key-combo-return))
          ("<end>" . (end-of-visual-line end-of-buffer key-combo-return))
          ("M-q" . (fill-paragraph unfill-paragraph))))
  (key-combo-load-default-1 (current-global-map) key-combo-kw-global)
  )

;; ---------------------------------------------------------------------------

(use-package magit
  :bind
  ("C-c g" . magit-status) )

(defun magit-mode ()
  "Reminder how to use magit"
  (interactive)
  ( message "Use M-x magit-status [C-c g] in the right directory. sS(tage) cc(ommit)"))

;; https://emacs.stackexchange.com/questions/19440/magit-extremely-slow-in-windows-how-do-i-optimize
;; WORKAROUND https://github.com/magit/magit/issues/2395
(define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
  "Mode for showing staged and unstaged changes."
  :group 'magit-status)
(defun magit-staging-refresh-buffer ()
  (magit-insert-section (status)
    (magit-insert-untracked-files)
    (magit-insert-unstaged-changes)
    (magit-insert-staged-changes)))
(defun magit-staging ()
  (interactive)
  (magit-mode-setup #'magit-staging-mode))

;; ---------------------------------------------------------------------------

(use-package markdown-mode
  :commands (markdown-mode)
  ;; gfm-mode allows underscores
  :mode (("README\\.md" . gfm-mode)
         ("\\.md" . gfm-mode)) ; was markdown-mode
  :bind
  (:map markdown-mode-map
        ("C-c i c" . kw-markdown-screenshot-capture-and-paste)
        ("C-c i v" . kw-markdown-screenshot-paste)
        ; ("C-c i d" . kw-markdown-delete-image)  ;; not yet written
        ("C-c i l" . kw-markdown-insert-image-link)
        ("C-c i t" . markdown-toggle-inline-images)
        ;;("C-c C-w" . kw-md-to-docx-and-open)
        ;; ("C-S-k" . kw-md-to-docx-and-open)
        ;; use other window if one is open
        ("C-c C-o" . (lambda () (interactive) (markdown-follow-thing-at-point t)))
        )
  :custom
  (markdown-command "pandoc")
  (markdown-enable-wiki-links t)
  (markdown-gfm-use-electric-backquote nil)
  (markdown-hide-urls nil) ; no hyperlink infinity symbol
  (markdown-link-space-sub-char "_") ; change space to underscore wiki links
  :config
  (defun kw-md-to-docx-and-open()
    "Export the current md file as a docx via pandoc."
    (interactive)
    (let* ((current-file (buffer-file-name))
           (basename (file-name-sans-extension current-file))
           (docx-file (concat basename ".docx")))
      (save-buffer)
      (when (file-exists-p docx-file) (delete-file docx-file))
      ;; -f format
      ;; -o --output=FILENAME
      (shell-command (format
                      "pandoc -f markdown %s -o %s" current-file docx-file))
      (shell-command (format "open %s" docx-file))
      ))
  )

;; ---------------------------------------------------------------------------

(use-package minimal-session-saver
  ;; Only saves visited files (no inferior buffers)
  :custom
  (minimal-session-saver-data-file "c:/kw/.emacs.d/minimal-session-saver-data.el")
  :config
  (defalias 'session-save 'minimal-session-saver-store)
  (defalias 'session-restore 'minimal-session-saver-load)
  )

;; ---------------------------------------------------------------------------
;; use non-emacs programs to open files

(use-package openwith
  :custom
  (openwith-associations
   '(("\\.pdf$" "pdfxcview.exe" (file))
     ("\\.\\(doc\\|docx\\)$" "winword.exe" (file))
     ("\\.\\(ppt\\|pptx\\)$" "powerpnt.exe" (file))
     ("\\.\\(xls\\|xlsx\\)$" "excel.exe" (file))
     ) )
  :config
  (openwith-mode t) )

;; ---------------------------------------------------------------------------

(use-package paren
  :config
  (show-paren-mode t) )

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
    echo area. Has no effect if the character before point is not of
    the syntax class ')'."
  (interactive)
  (let* (
         (cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open)
                             ) ))))

;; ---------------------------------------------------------------------------

(use-package python
  :mode ("\\.py$" . python-mode)
  :commands (run-python)
  :custom
  (python-shell-interpreter "C:/bin/WPy64-3741/python-3.7.4.amd64/python.exe") )

;; ---------------------------------------------------------------------------
;; colorize color names

(use-package rainbow-mode
  :diminish
  :hook (emacs-lisp-mode . rainbow-mode) )

;; ---------------------------------------------------------------------------

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode ess-mode inferior-ess-mode Rd-mode) . rainbow-delimiters-mode)
  )

;; ---------------------------------------------------------------------------

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-max-menu-items 15)
  :bind
  ("C-c f" . kw-ivy-recentf-open)
  :config
  (recentf-mode t)
  (defun kw-ivy-recentf-open ()
    "Use `ivy-completing-read' to find a recent file."
    (interactive)
    (if (find-file (completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
)

;; allow multiple emacs sessions to use recentf
(use-package sync-recentf
  :disabled
  :custom
  (recentf-auto-cleanup 60))

;; ----------------------------------------------------------------------------
;; recover files - make the message "FILE has auto save data" unmissable

(defface recover-this-file
  '((t (:foreground "#ffa0c0" :background "#331818")))
  "Face for buffers visiting files with auto save data."
  :group 'files)

(defvar recover-this-file nil
  "If non-nil, an overlay indicating that the visited file has auto save data.")

(defun recover-this-file-find-file-hook ()
  ;; see after-find-file
  (let ((warn (not buffer-read-only)))
    (when (and warn
               ;; No need to warn if buffer is auto-saved
               ;; under the name of the visited file.
               (not (and buffer-file-name
                         auto-save-visited-file-name))
               (file-newer-than-file-p (or buffer-auto-save-file-name
                                           (make-auto-save-file-name))
                                       buffer-file-name))
      (set (make-local-variable 'recover-this-file)
           (make-overlay (point-min) (point-max)))
      (overlay-put recover-this-file 'face 'recover-this-file))))

(add-hook 'find-file-hook 'recover-this-file-find-file-hook)

(defun rtf ()
  "Recover this file and restore background color"
  (interactive)
  (recover-this-file)
  (delete-overlay recover-this-file))

;; -------------------------------------------------------------------------

;; I don't use these anymore???

(use-package reftex
  :hook ((latex-mode . turn-on-reftex)) ; with Emacs latex mode
  )

(use-package reftex-vars
  :commands (turn-on-reftex)
  :custom
  (reftex-bibpath-environment-variables '("c:/x/notes/") )
  (reftex-cite-format 'natbib)
  (reftex-default-bibliography '("c:/x/notes/kw.bib"))
  (reftex-file-extensions '("Rnw" "rnw" "tex" "sty") )
  (reftex-use-multiple-selection-buffers t) ; faster for large documents
  )

;; ---------------------------------------------------------------------------
;; Ideas here:
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;; eshell is better (most of the time)
;; use find-file instead of "open" from Rtools
;; In eshell, "open somefile.pdf" gives
;; Wrong type argument: stringp, ("somefile.pdf")

(use-package eshell
  :commands (eshell eshell-command)
  )

;; shell loses track of directory when using 'cd'
(use-package shell
  :commands (shell)
  :custom
  (explicit-shell-file-name (getenv "SHELL"))
  )

;; ---------------------------------------------------------------------------

(use-package super-save
  :diminish
  ;; save files upon: focus-out-hook and switch-to-buffer
  :config
  (super-save-mode +1))

;; ---------------------------------------------------------------------------

;; template-new-file called manually, prompts for and inserts template
;; template-expand-template (template) ; calls template-new-file ; could run after counsel

;; does not seem to work with counsel-find-file ???
;; counsel-find-file runs
;;  find-file, which in turns uses this list
;;    find-file-not-found-functions to call
;;      template-not-found-function - not called via counsel-find-file, nor template-new-file
;; template-new-file - not used by counsel-find-file
;; find-file is used by counsel-find-file
(use-package template
  :custom
  (template-auto-insert t) ; no query
  (template-default-directories (list (concat dropbox-dir "emacs/templates/")))
  :bind
  ("C-c p" . template-new-file)
  :config
  ;;(add-to-list 'template-find-file-commands 'counsel-find-file)
  ;;(add-to-list 'template-find-file-commands 'counsel-find-file-action)
  ;;(add-to-list 'template-file-select-commands 'counsel-find-file)
  ;;(add-to-list 'template-file-select-commands 'counsel-find-file-action)
  ;; template-use-package
  (template-initialize)
  )

;; ----------------------------------------------------------------------------

(use-package text-mode
  :commands (text-mode)
  :hook (text-mode . visual-line-mode))

;; ---------------------------------------------------------------------------

(use-package theme-changer
  :demand ; override delayed loading caused by :bind
  :custom
  (custom-theme-directory (concat dropbox-dir "emacs/themes/"))
  (theme-changer-theme-list '(zen leuven spring summer autumn winter
                              base16-mocha twilight-bright monokai))
  :bind
  ("<f5>" . theme-changer-load-theme)
  ("<S-f5>" . theme-changer-reload-theme)
  ("<C-f5>" . theme-changer-enable-next-theme)
  :config
  (load-theme 'zen t) )

;; ---------------------------------------------------------------------------

(use-package time-stamp
  :custom
  (time-stamp-format "%02d %3b %:y %02H:%02M:%02S %F")
  :hook
  (before-save . time-stamp) )

;; ---------------------------------------------------------------------------
;; M-x transpose-frame

(use-package transpose-frame)

;; ---------------------------------------------------------------------------

(use-package unfill
  :bind
  ("<C-?>" . fill-paragraph) ; C-S-/ in rstudio
  )

;; ---------------------------------------------------------------------------

(use-package which-function-mode
  :defer t
  :hook ((prog-mode . which-function-mode)) )

;; ---------------------------------------------------------------------------

(run-at-time 5 nil 'kw-tip-of-the-day) ; After 5 sec, show tip

;; ---------------------------------------------------------------------------
;; like rstudio, Alt-drag selects a rectangle
;; https://emacs.stackexchange.com/questions/7244/
(defun mouse-start-rectangle (start-event)
  (interactive "e")
  (deactivate-mark)
  (mouse-set-point start-event)
  (rectangle-mark-mode +1)
  (let ((drag-event))
    (track-mouse
      (while (progn
               (setq drag-event (read-event))
               (mouse-movement-p drag-event))
        (mouse-set-point drag-event)))))
(global-set-key (kbd "M-<down-mouse-1>") #'mouse-start-rectangle)

;; ---------------------------------------------------------------------------

;; Ideas for pasting clipboard or screenshot into markdown
;; https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it
;; http://shallowsky.com/blog/linux/editors/graphics-in-emacs.html

;; Windows
;; Greenshot cannot capture screenshot via shell command.  See:
;; https://superuser.com/questions/1396439
;; IrfanView could probably be used

(defun kw-markdown-insert-screenshot (capture-first)
  "Take a screenshot into a time-stamped file in the
   same directory as the markdown-buffer and insert a link to this file."  
  (interactive)
   (setq filename
         (concat (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                 (format-time-string "_%Y%m%d_%H%M%S.png")))
  
   ;; allow user to change the filename
   (setq filename (read-string "Screenshot file name: " filename nil filename))
   
   ;; Capture screenshot to clipboard with Snipping tool (Windows only)
   (if capture-first
       (shell-command "snippingtool /clip"))

   ;; Convert clipboard image into file
   (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
   (insert (concat "![](" filename ")"))
   ;; Pasting a link doesn't actually show the image until we turn it off/on
   (markdown-remove-inline-images)
   (markdown-display-inline-images)   
   )

(defun kw-markdown-screenshot-capture-and-paste ()
  ""
  (interactive)
  (kw-markdown-insert-screenshot t)  )

(defun kw-markdown-screenshot-paste ()
  ""
  (interactive)
  (kw-markdown-insert-screenshot nil)  )

;; Note: Images 800 pixels wide look good in Word.
(defun kw-markdown-insert-image-link (&optional arg)
  "Insert link to image, using ivy-read-file-name to get filename.
Insert image markup using region or word as alt text if possible.
If there is an active region, use the region as the alt text.  If the
point is at a word, use the word as the alt text.  In these cases, the
point will be left at the position for inserting a URL.  If there is no
active region and the point is not at word, simply insert image markup and
place the point in the position to enter alt text.  If ARG is nil, insert
inline image markup.  Otherwise, insert reference image markup.
Tip: Images 800 pixels wide look good when exported to Word."
  (interactive "*P")
  (setq linkpart (concat "]("
                         (file-relative-name (read-file-name "Find filename: "))
                         "){width=5in}" ))
  (let ((bounds (if arg
                    (markdown-wrap-or-insert "![" "][]")
                  (markdown-wrap-or-insert "![" linkpart))))
    (when bounds
      (goto-char (- (cdr bounds) 1)))))

(defun kw-insert-include-graphics-link ()
  "In R files, navigate to a file and insert relative link
along with 'include_graphics' around the link"
  (interactive)
  (unless (featurep 'counsel) (require 'counsel))
  (ivy-read "Find filename: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :action
            (lambda (x)
              (insert
               (concat "knitr::include_graphics('"
                       (file-relative-name x)
                       "')"))
              )))

;; ---------------------------------------------------------------------------

;; https://stackoverflow.com/questions/2901541/which-coding-system-should-i-use-in-emacs/2903256#2903256
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))
(prefer-coding-system 'utf-8)


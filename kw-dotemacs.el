; kw-dotemacs.el

;; Message: Package cl is deprecated is coming from key-combo, which is not maintained

;;(eval-after-load "cl" '(debug))

;; David Wilson
;; https://github.com/daviwil/emacs-from-scratch
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org
;; Emacs from Scratch 1-12. Tips 1-2 6-7.

;; Review
;; https://github.com/jinnovation/dotemacs
;; https://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs
;; https://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html

;; todo: Emacs 28.1 has make-separator-line, M-: (insert (make-separator-line 70))

;; todo: fix template + counsel
;; fix line-of-dash

;; flyspell-default-delayed-commands
;; (add-to-list 'flyspell-delayed-commands 'self-insert-command)

;; Fans of clutter-free home directories, rejoice.
;; Emacs will prefer ~/.config/... (or XDG_CONFIG_HOME) to the usual ~/.emacs.d.

;; History
;; 2021 Add dubplicate-thing, helpful, ivy-rich, dimmer, neotree
;; 2020 Markdown image functions
;; 2019 Switch from ido to ivy
;; 2018 Switch from org to markdown and Rmarkdown
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

;; might need this for some pandoc???
;; (setq TeX-engine 'xelatex)
;; (setq latex-run-command "xelatex")

;; ---------------------------------------------------------------------------

;; Set GC to 100mb
(setq gc-cons-threshold 100000000)

;; Set the default directory, lisp directory
(setq default-directory "c:/x/"
      onedrive-dir "c:/one/"
      ;; location of auto-saves. Used by rtf.
      auto-save-list-file-prefix "c:/one/.emacs.d/.saves-"
      )
(add-to-list 'load-path (concat onedrive-dir "emacs/") )

;;(when (version< emacs-version "27.0") (package-initialize))

;; orig ~/.emacs.d/
(setq user-emacs-directory "c:/one/.emacs.d/") ; recentf, session-saver, etc
;(setq package-user-dir "~/.emacs.d/elpa") ; packages are inside elpa
(setq package-user-dir "c:/one/.emacs.d/elpa") ; packages are inside elpa

;; Set up package archive (new computer only)
(package-initialize)
;; (unless package-archive-contents
;;   (package-refresh-contents))
;; Install use-package if needed
;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))

;; https://jwiegley.github.io/use-package/keywords/
;; :bind  - define keybindings BEFORE loading. Defers loading.
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
(require 'use-package)

;;(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
;;                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-compute-statistics t) ; M-x use-package-report
;;(setq use-package-always-ensure t) ;; new computer only

;; -------------------------------------------------------------------------

(add-to-list 'same-window-buffer-names "*grep*")
                               

(use-package emacs
  :custom
  (completion-ignored-extensions
      '("~" ".aux" ".bbl" ".blg" ".brf" ".bst" ".log" ".out" ".ps"
        ".doc" ".docx" ".ppt" ".pptx" ".xls" ".xlsx"))
  ;;(completion-show-help nil)
  (completions-format 'vertical) ; Use vertical sorting, not horiz
  ;; Stop emacs from modifying my .emacs file with custom variables
  (custom-file  (concat onedrive-dir "emacs/custom.el"))
  (inhibit-splash-screen t) 
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
  ;; Get qpdf from https://sourceforge.net/projects/qpdf/
  (setenv "PATH" (concat "c:/bin/qpdf/bin" ";"
                         "c:/Progra~1/R/R-4.1.1/bin/x64/" ";"
                         (getenv "PATH") ";"
                         "c:/rtools40/usr/bin" ;; for unzip
                         ))
  (setenv "CYGWIN" "nodosfilewarning") ; Why do I have this?
  (setenv "LANG" "en_US.UTF-8") ; To fix pandoc-cite bug in R
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
  (set-face-attribute 'fixed-pitch nil :family "Consolas")
  ;;(set-face-attribute 'fixed-pitch nil :family "Dina")
  ;;(set-face-attribute 'fixed-pitch nil :family "Lucida Console" :height 100)
  (setq default-frame-alist '((width . 100) (height . 32)))
  ;; (font . "Dina-9"))) ; Only 6,9 seem to work on Windows
  ;; (font . "Liberation Mono-12") ; Linux
  
  ;; Modeline format
  (setq-default mode-line-format
                '(" %l:%C" ; line/column
                  " %b " ; buffer name
                  mode-line-modes
                  ;; (which-func-mode (" " which-func-format ""))
                  ))
  (tool-bar-mode -1)
  )

;; Re-define yes-or-no (like file save) to use y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; -------------------------------------------------------------------------

(use-package kw-misc
  :hook
  ((markdown-mode prog-mode) . kw-add-watchwords)  ) ; highlight fixme/todo

;; ---------------------------------------------------------------------------
;; reload files if they change on disk

(use-package autorevert
  :mode ("\\.pvs$" . auto-revert-mode) ;; asreml predictions
  ;;:mode ("c:/kw/.emacs.d/recentf" . auto-revert-mode)
  :mode ("c:/one/.emacs.d/recentf" . auto-revert-mode)
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
  (diary-file (concat onedrive-dir "blog/diary.txt"))
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
;; In comint modes [up] and [down] keys use history only when point is on
;; command line. Otherwise they behave normally. From Tudor Hulubei.

(use-package comint
  :commands (shell-mode)
  :config
  (defun smart-comint-up ()
    "At the end of buffer, do comint-previous-input,
otherwise just move in the buffer."
    (interactive)
    (let ((previous-region-status nil))
      (if (= (point) (point-max))
          (comint-previous-input 1)
        (forward-line -1)) ))
  
  (defun smart-comint-down ()
    "At the end of buffer, do comint-next-input,
otherwise just move in the buffer."
    (interactive)
    (let ((previous-region-status nil))
      (if (= (point) (point-max))
          (comint-next-input 1)
        (forward-line 1)) ))

  :custom
  (comint-move-point-for-output t)
  (comint-scroll-to-bottom-on-output t)
  (comint-prompt-regexp "^[^#$%>\\n]*[#$%>] *")
  :bind
  (:map comint-mode-map
        ("<M-up>" . counsel-shell-history) ; rstudio
        ("<up>" . smart-comint-up)
        ("<down>" . smart-comint-down)
        ("<C-left>" . comint-bol)
        ("<home>" . comint-bol))
  )

;; ---------------------------------------------------------------------------

(use-package dashboard
  :diminish
  :custom
  ;;(dashboard-banner-logo-title "your custom text")
  (dashboard-footer-messages
   '("C-x r l bookmaRks List"
     "C-x r m bookmaRks Mark"
     "C-x 8 C-h show unicode"
     "C-c b bookmark jump"
     "C-c f ivy find recent file."
     "C-c s swap buffers."
     "C-c t TODO file."
     "C-c ? show the face at point."
     "C-c ( jump to the matching parenthesis."
     "C-c ) jump to the matching parenthesis."
     "C-c C-i i markdown insert image link"
     "C-h P describe-Package"
     "C-o jump to the Other window."
     "C-u C-SPC jump to previous mark."
     "C-u C-x = show text properties at point."
     "C-y M-y yank previous kills."
     "M-m back-to-indentation (also C-left)" 
     "C-/ undo"
     "C-a move-bol (also C-left)"
     "C-e move-eol (also C-right)"
     "C-M-a beginning-of-defun"
     "C-M-e end-of-defun"
     "C-M-f forward sexp"
     "C-M-b backward sexp"
     "C-M-d down list"
     "C-M-u up list"
     "C-M-k kill-sexp"
     "C-` to insert Rmd chunk"
     "M-nr to render, then open Rmd file"
     ;; defaults I need to learn
     "C-h m describe-mode"
     "C-x d dired"
     "C-c i h markdown-toggle-markup-hiding"
     "ESC ESC ESC to return to normal"
     "C-y M-y yank from kill, cycle through kill"
     ))
  (dashboard-startup-banner nil)
  (dashboard-items '((recents . 15)
                     (bookmarks . 5)
                     ;;(projects . 0) ;; not working
                      ))
  :hook
  (after-init . dashboard-setup-startup-hook)
  )

;; ----------------------------------------------------------------------------
;; deadgrep
;; https://github.com/Wilfred/deadgrep
;; Get rg.exe here https://github.com/BurntSushi/ripgrep/releases

(use-package deadgrep
  :defer
  :bind
  ("C-c u" . #'deadgrep) ; hUnt for text in current directory
  :custom (deadgrep-executable "c:/bin/rg.exe")
  )

;; ----------------------------------------------------------------------------

;; https://github.com/gonewest818/dimmer.el
(use-package dimmer
  :custom
  ;; shift inactive buffer background toward text color by .08
  (dimmer-adjustment-mode :background)
  (dimmer-fraction 0.08)
  ;; dont dim these
  (dimmer-exclusion-regexp-list
       '(".*Minibuf.*"
         ".*which-key.*"
         ".*NeoTree.*"
         ".*Warnings.*" ))
  :config
  (dimmer-mode t))

;; ----------------------------------------------------------------------------
;; dired things

;; recursive listing of directory
;; C-u C-x d   -aFR
;; colored file names in dired

(use-package dired
  :commands (dired)
  :config
  ;; Use same buffer instead of opening new buffers
  (put 'dired-find-alternate-file 'disabled nil)
  ;; all,long,filetype (for highlighting)
  ;; G=no group info h=human readable sizes
  (setq dired-listing-switches "-alFGoh --group-directories-first") ;; agho  ;; aF
  :bind
  (:map dired-mode-map
        ( "C-c C-c" . wdired-change-to-wdired-mode) ) )

(use-package dired-x
  :commands (dired-omit-mode)
  :init
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  ;; hide most columns of dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode) ;; toggle with "("
  ;; we cannot set dired-omit-files in :custom
  :config
    (progn
      (setq dired-omit-verbose nil)
      ;; toggle `dired-omit-mode' with C-x M-o
      (add-hook 'dired-mode-hook #'dired-omit-mode)
      ))

;; colorize filenames by type
(use-package dired-filetype-face
  :defer
  )
;; this works, but is not widely used
;; (use-package dired-filetype-face
;;   ;; regexp to match rich document filetypes
;;   :config
;;   (custom-declare-face 'dired-filetype-document '((t (:foreground "blue"))) "face for rich document files " :group 'dired-filetype-face)
;;   (custom-declare-variable 'dired-filetype-document-regexp
;;                            '"^  .*\\.\\(pdf\\|doc\\|docx\\|xls\\|xlsx\\|ppt\\|pptx)$"
;;                            '(#$ . 1780) :type 'string :group 'dired-filetype-face)
;;   )

(use-package all-the-icons
  :disabled
  )
;; https://github.com/domtronn/all-the-icons.el#resource-fonts
(use-package all-the-icons-dired
  :disabled
  :hook (dired-mode . all-the-icons-dired-mode))

;; ---------------------------------------------------------------------------

(use-package duplicate-thing
  :init
  (defun my-duplicate-thing ()
    "Duplicate thing at point without changing the mark."
    (interactive)
    (save-mark-and-excursion (duplicate-thing 1)))
  :bind (("C-c 2" . my-duplicate-thing)
         ))

;; https://stackoverflow.com/questions/23588549/emacs-copy-region-line-and-comment-at-the-same-time
(defun duplicate-region-and-comment (beg end &optional arg)
  "Duplicate the region below and comment-out the original text.
See `comment-region' for behavior of a prefix arg."
  (interactive "r\nP")
  (copy-region-as-kill beg end)
  (goto-char end)
  (yank)
  (comment-region beg end arg)
  (forward-line))
(bind-keys ("C-c <f6>" . duplicate-region-and-comment))

;; https://emacs.stackexchange.com/questions/47200/copy-paste-text-among-split-window-buffers
(defun duplicate-region-to-other-window (beg end)
  "Duplicate the region to other window."
  (interactive "r")
  (pcase (window-list)
    (`(,w0 ,w1)
     (with-selected-window w1
       (insert-buffer-substring (window-buffer w0) beg end)))
    (t (user-error "Only works with 2 windows"))))
(bind-keys ("C-c 5" . duplicate-region-to-other-window))


;; ---------------------------------------------------------------------------

(use-package ediff
  :commands (ediff)
  :custom
  (ediff-diff-options "-w") ; ignore whitespace
  )

;; ---------------------------------------------------------------------------

(use-package eldoc
  ;;:disabled
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

(use-package ess-julia
  :commands (julia)
  :mode ("\\.jl\\'" . ess-julia-mode)
  :custom
  (inferior-julia-program-name
   "C:/Users/wrightkevi/AppData/Local/Programs/Julia 1.5.2/bin/julia.exe")
  (inferior-julia-args "-i --color=yes -L c:/One/emacs/julia_setup.jl")
  )

;; ---------------------------------------------------------------------------
;; R stuff

(defun kw-switch-to-inferior-R-other-window ()
  "Switches to inferior R process."
  (interactive)
  (switch-to-buffer-other-window "*R*") )

(use-package ess-r-mode
  :defer
  :mode ("\\.[rR]$" . r-mode)
  :custom
  (ess-style 'RStudio)
  (ess-eval-visibly 'nowait) ;; t seems to make emacs hang
  (ess-history-file nil) ; do not write history
  ;;(ess-indent-with-fancy-comments nil) ; do not distinguish # ## ###
  (ess-pdf-viewer-pref "c:/Progra~1/R/R-4.1.1/bin/x64/open.exe")
  ;; Simplified pattern, asreml prints WARNING to console
  (ess-R-message-prefixes '("Error" "nfault" "singularities" "Warning" "WARNING"))
  
  ;;(ess-r-versions nil)
  ;;(ess-r-runner-prefixes nil) ; Do not search for multiple R versions
  
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
  (ess-use-eldoc nil) ; no eldoc in ess-mode and inferior-ess-mode
  (ess-use-flymake nil) ; turn it off
  (inferior-ess-r-help-command ".ess.help('%s', help.type='text')\n" )
  (inferior-ess-r-program "c:/Progra~1/R/R-4.1.1/bin/x64/rterm.exe" )
  (inferior-R-args "--no-restore-history --no-save --quiet")
  :config 
  ;; In R code, add markdown headers.  Note RStudio uses four trailing hyphens.
  ;; https://blog.rstudio.com/2020/12/02/rstudio-v1-4-preview-little-things/
  (require 'markdown-mode) ; needed for markdown-header-face
  (font-lock-add-keywords
   'ess-r-mode
   ;; start, space/tab 0 or more times, pound sign; space/tab 1+, alnum at least 1, space/tab 1+, ---
   '(("^\s*#\s+.+\s+---" 0 'markdown-header-face-1 t)
     ("^\s*##\s+.+\s+---" 0 'markdown-header-face-2 t)
     ("^\s*###\s+.+\s+---" 0 'markdown-header-face-3 t)
     ("^\s*####\s+.+\s+---" 0 'markdown-header-face-4 t)
     ))

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
  :defer
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
  )

;; ---------------------------------------------------------------------------

;(use-package poly-R)

;; polymode weaver: knitr vs knitr-ESS
;; https://emacs.stackexchange.com/questions/18364/
;; knitr-ESS weaver will use the CURRENT session--can use and modify objects

(use-package polymode
  :mode (;;("\\.Rd" . poly-Rd-mode)
         ;;("\\.Rmd" . poly-markdown+R-mode)
         ("\\.Rmd" . poly-gfm+r-mode)
         )
  :custom
  (polymode-exporter-output-file-format "%s")
  :bind
  (:map polymode-mode-map
        ;; md: pandoc/docx Rmd: Rmarkdown/AUTO
        ;;("C-S-k" . ess-render-rmarkdown)
        ("C-S-b" . recompile) ; run makefile like RStudio 
        ("C-S-k" . polymode-export) ; M-n e
        ("C-`" . ess-insert-rmd-chunk)
        ("C-~" . ess-insert-inverse-rmd-chunk)
        ("C-M-i" . ess-insert-rmd-chunk) ;; like rstudio
        )
  :config
  (use-package poly-R)
  (use-package poly-markdown)
  )

;; See also
;; https://stackoverflow.com/questions/28324288/how-to-create-a-pdf-from-a-rmd-file-using-emacs-ess-pandoc-mode-and-polymode
;; delhey.info/inc/ess-rmarkdown.pdf
;; https://www.stefanavey.com/lessons/2018/01/04/ess-render
(defun ess-render-rmarkdown ()
  "Compile .Rmd file to any output type."
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
              (format ".outfile=rmarkdown::render(\"%s\" , clean=TRUE); shell.exec(.outfile)"
                      buffer-file-name)))
        (message "Running rmarkdown on %s" buffer-file-name)
        (message "With string %s" R-cmd)
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
  
  ;; Do NOT expand links, also affects recentf.
  ;; This was working in 2020, but stopped working when I upgraded
  ;; packages on 25 Jan 2021.  Use directory-abbrev-alist instead
  (find-file-visit-truename nil)
  
  ;; Custom abbreviations for directories
  ;; https://emacs.stackexchange.com/questions/37253
  ;; https://stackoverflow.com/questions/19032166 
  (directory-abbrev-alist
   '( ("c:/Users/wrightkevi/OneDrive/kw_work" . "c:/one")
      ("c:/Users/wrightkevi/OneDrive - Corteva" . "c:/x") ) )
  )

;; ---------------------------------------------------------------------------

(use-package flyspell
  ;; :disabled ;; makes scrolling too slow
  ;; use ispell-buffer, NOT flyspell-buffer
  :hook ( (markdown-mode . flyspell-mode)
          (text-mode . flyspell-mode) ) )

;; ---------------------------------------------------------------------------
;; make hyperlinks clickable

(use-package goto-addr
  :bind
  (:map goto-address-highlight-keymap
        ("C-c C-o" . goto-address-at-point))
  :hook ((prog-mode . goto-address-prog-mode)
         (Rd-mode . goto-address-mode)
         (text-mode . goto-address-mode)))

;; ---------------------------------------------------------------------------
;; helpful provides more information
;; https://github.com/Wilfred/helpful

(use-package helpful
  :commands (helpful-callable helpful-command helpful-key helpful-variable)
  ;; :bind
  ;; (("C-h f" . helpful-callable)
  ;;  ("C-h v" . helpful-variable)
  ;;  ("C-h k" . helpful-key)
  ;;  ("C-c C-d" . helpful-at-point)
  ;;  ("C-h F" . helpful-function)
  ;;  ("C-h C" . helpful-command)))
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
   :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

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

;; I did not like this format...too cluttered.
;; (use-package all-the-icons-ibuffer
;;   :ensure t
;;   :init (all-the-icons-ibuffer-mode 1))

;; ---------------------------------------------------------------------------
;; used to show images in markdown mode

(use-package iimage 
  :commands (iimage-mode turn-on-iimage-mode))

;; ---------------------------------------------------------------------------
;; hunspell uses c:/kw/hunspell_en_US for personal dictionary

(use-package ispell
  :commands (ispell-buffer)
  :custom
  (ispell-program-name "c:/bin/hunspell/bin/hunspell.exe")
  (ispell-silently-savep t) ; dont prompt to save dictionary
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

;; ----------------------------------------------------------------------------
;; image

;; (use-package image
;;   :bind
;;   (:map image-mode-map
;;         ("C-S--" . image-decrease-size)
;;         ("C-S-=" . image-increase-size)
;;         ("C-S-0" . image-transform-fit-to-width))
;;   )

;; ---------------------------------------------------------------------------
;; ivy is a completion framework
;; counsel provides most of the user-facing functions
;; swiper is used for searching the current buffer
;; https://oremacs.com/2019/06/27/ivy-directory-improvements/

;; C-M-j ivy-immediate-done  create file???
;; C-j or TAB TAB - descend into directory
(use-package ivy
  ;;:demand
  :defer 0.5
  :diminish
  :bind
  ("C-s" . swiper)
  (:map ivy-minibuffer-map
        ("C-j" . ivy-next-line)
        ("C-k" . ivy-previous-line)
        ("C-r" . counsel-minibuffer-history)
        ("<left>" . ivy-backward-kill-word) ; up directory
        ("<right>" . ivy-alt-done) ; descend into directory
        ("<return>" . ivy-alt-done) ; descend into directory
        ("<C-return>" . ivy-done) ; open dired
        ;; C-c C-o ; ivy-occur
        )
        
  :custom
  (ivy-count-format "%d/%d ") ; show current/total count
  (ivy-height 15)    ; number of results shown
  ;; add recentf-mode and bookmarks to ‘ivy-switch-buffer’.
  (ivy-use-virtual-buffers t)
  :custom-face
  (ivy-current-match ((t (:foreground "yellow" :background "#006000"))))
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
  :custom
  (counsel-find-file-ignore-regexp
   (concat
    ;; start of filename = \\`
    "\\(\\`#\\|\\`\\.\\)\\|"
    ;; end of filename = \\'
    "\\(~\\'\\|docx\\'\\|pptx\\'\\|xls\\'\\|xlsx\\'\\)"))
  )

;; show the key binding and primary doc string with counsel-M-x
(use-package ivy-rich
  :after counsel
  :config
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :disabled
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

;; -------------------------------------------------------------------------
;; Global key bindings
;; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/
;; Use C-h k, press the key combination.  Use this in bind-key

;; Single ESC Cancels All (instead of 3 ESC)
(bind-keys ("<escape>" . keyboard-escape-quit))

;; function keys
(unbind-key "<f2>") ; I accidentally hit this
(bind-keys ("<f1>" . ibuffer)
           ("<M-f4>" . kill-this-buffer)
           ("<f3>" . query-replace)
           ("<S-f3>" . replace-string)
           ("<C-f3>" . replace-regexp)
           ("<f6>" . kw-comment-dwim)
           ;; ("<C-S-c>" . kw-comment-dwim) ; rstudio
           ("<f7>" . kw-bookmark)
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
(bind-keys ("<C-left>" . back-to-indentation) ;; beginning-of-line
           ("<C-right>". end-of-line)
           ("<C-up>"   . beginning-of-buffer)
           ("<C-down>" . end-of-buffer)
           ("<S-down>". scroll-up-line)
           ("<S-up>". scroll-down-line) )

;; misc
(bind-keys ;;("C-F" . isearch-forward) ; like MS programs
           ("C-h a" . apropos) ; Default is apropos-command
           ("C-o" . other-window)
           ("C-v" . yank)  ; Like MS windows paste
           ("C-z" . undo) ) ; Like MS Windows undo
; I accidentally hit this
;;(unbind-key "C-d")
(unbind-key "C-t")

;; functions in kw-misc
(bind-keys ("C-c m" . kw-move-file-and-buffer)
           ("C-c n b" . kw-new-buffer)
           ("C-c r" . kw-rename-file-and-buffer)
           ("C-c s" . kw-swap-windows)
           ("C-c ?" . kw-show-face-at-point)
           ("C-c (" . kw-match-paren)
           ("C-c )" . kw-match-paren)
           )

;; ---------------------------------------------------------------------------
;; Note: We use key-combo because key-chord does not support using <home>
;; https://stackoverflow.com/questions/25572489/how-to-create-an-emacs-key-chord-with-home-key
:; use-package-chords is another alternative
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
        '(("<f4>" . (ignore kill-this-buffer))
          ("C-a" . (beginning-of-visual-line beginning-of-buffer key-combo-return))
          ("<home>" . (beginning-of-visual-line beginning-of-buffer key-combo-return))
          ("C-e" . (end-of-visual-line end-of-buffer key-combo-return))
          ("<end>" . (end-of-visual-line end-of-buffer key-combo-return))
          ("M-q" . (fill-paragraph unfill-paragraph))))
  (key-combo-load-default-1 (current-global-map) key-combo-kw-global)
  )

;; ---------------------------------------------------------------------------
;; DANGER, do NOT edit in loccur mode
;; occur uses a new buffer, loccur uses the same buffer

(use-package loccur
  :commands (loccur)
  :bind ("M-o" . loccur-no-selection)
  :config
  (defun loccur-no-selection () ; no auto-fill of loccur prompt
    (interactive)
    (let ((current-prefix-arg 1))
      (call-interactively
       'loccur)))
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
        ("C-S-b" . recompile) ; run makefile like RStudio 
        ("C-c i c" . kw-markdown-screenshot-capture-and-paste)
        ("C-c i v" . kw-markdown-screenshot-paste)
        ("C-c i l" . kw-markdown-insert-image-link)
        ("C-c i t" . markdown-toggle-inline-images)
        ("C-c i h" . markdown-toggle-markup-hiding)
        ;;("C-c C-w" . kw-md-to-docx-and-open)
        ;; ("C-S-k" . kw-md-to-docx-and-open)
        ;; use other window if one is open
        ("C-c C-o" . (lambda () (interactive) (markdown-follow-thing-at-point t)))
        ;; image-mode
        ("C-S--" . image-decrease-size)
        ("C-S-=" . image-increase-size)
        ("C-S-0" . image-transform-fit-to-width)
        )
  :custom
  (markdown-code-block-braces t) ;; Use braces when inserting code blocks
  (markdown-max-image-size '(800 . 800))
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
;; save a list of currently-visited files

(use-package minimal-session-saver
  :commands (minimal-session-saver-store minimal-session-saver-load
             session-save session-restore)
  :custom
  (minimal-session-saver-data-file "c:/one/.emacs.d/minimal-session-saver-data.el")
  :config
  (defalias 'session-save 'minimal-session-saver-store)
  (defalias 'session-restore 'minimal-session-saver-load)
  )

;; ------------------------------------------------------------------------------
;; Make emacs use UTF-8 everywhere by default
;; https://stackoverflow.com/questions/2901541/

(use-package mule
  :defer 1
  :config
  (setq locale-coding-system 'utf-8)
  :init
  (set-default-coding-systems 'utf-8)
  (set-language-environment 'utf-8) ;; should it be UTF-8 ??
  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
  (set-selection-coding-system
   (if (eq system-type 'windows-nt)
       'utf-16-le  
     'utf-8))
  (set-terminal-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

;; ---------------------------------------------------------------------------

;; https://github.com/jaypei/emacs-neotree
(use-package neotree
  :commands neotree
  :config
  ;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-theme 'ascii)  ;; ascii icons
  (setq neo-smart-open t)
  )

;; ---------------------------------------------------------------------------
;; Mode for reading epub
;; t=toc, p=prev, n=next, l=hist back, r=hist forward

(use-package nov
  :custom
  (nov-unzip-program "c:/rtools40/usr/bin/unzip.exe")
  :mode ("\\.epub\\'" . nov-mode) )

;; ---------------------------------------------------------------------------
;; use non-emacs programs to open files

(use-package openwith
  :custom
  (openwith-associations
   '(("\\.pdf$" "pdfxcview.exe" (file))
     ("\\.\\(doc\\|docx\\)$" "winword.exe" (file))
     ("\\.\\(ppt\\|pptx\\)$" "powerpnt.exe" (file))
     ("\\.\\(xls\\|xlsx\\)$" "excel.exe" (file))
     ("\\.Rproj$" "RStudio.exe" (file))
     ) )
  :config
  (openwith-mode t) )

;; ---------------------------------------------------------------------------

(use-package paren
  :hook
  (prog-mode . show-paren-mode)
  ;; :config
  ;; (show-paren-mode t)
  )

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
  ;; inferior-ess-mode is not a prog-mode, so include it here
  :hook ((prog-mode inferior-ess-mode)
         . rainbow-delimiters-mode)
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
  (setq recentf-exclude '("sync-recentf-marker"))
  (defun kw-ivy-recentf-open ()
    "Use `ivy-completing-read' to find a recent file."
    (interactive)
    (if (find-file (completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
)

;; allow multiple emacs sessions to use recentf
(use-package sync-recentf
  ;; :defer Maybe I should NOT defer?
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
  (reftex-default-bibliography '("c:/One/notes/refs.bib"))
  (reftex-file-extensions '("Rmd" "tex" "sty") )
  (reftex-use-multiple-selection-buffers t) ; faster for large documents
  )

;; ---------------------------------------------------------------------------
;; saveplace
;; remembers your location in a file when saving files
;; save-place-local-mode to remember cursor position in only 1 file

(use-package saveplace
  :init
  (save-place-mode t)
  )

;; ---------------------------------------------------------------------------
;; shell stuff

;; term might be better on Linux, but doesn't work on Windows
;; eshell is better than shell (most of the time)
;; In eshell, "open somefile.pdf" gives
;;   Wrong type argument: stringp, ("somefile.pdf")
;; Instead, use find-file instead of "open" from Rtools
;; Ideas here:
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

(use-package esh-mode
  :commands (eshell eshell-command)
  :bind (:map eshell-mode-map
              ("<C-left>" . eshell-bol)))
  
(use-package eshell
  :commands (eshell eshell-command)
  :config
  ;;(defalias 'btsat '"C:/Temp/BT-SAT/BetaVersion/btsat.exe $1")
  (defalias 'ff 'find-file)
  ;; make is built-in eshell, but does not work
  ;;(defalias 'mkk '"c:/rtools40/usr/bin/make.exe $1")
  ;; In eshell, type this (stored in .emacs.d/eshell/alias
  ;; alias btsat 'C:/Temp/BT-SAT/BetaVersion/btsat.exe $1'
  ;; alias mkk 'c:/rtools40/usr/bin/make.exe $1'
  )

;; This will force shell to use Windows' PowerShell
;; Kudos to Jeffrey Snover: https://docs.microsoft.com/en-us/archive/blogs/dotnetinterop/run-powershell-as-a-shell-within-emacs
;; (setq explicit-shell-file-name "powershell.exe")
;; (setq explicit-powershell.exe-args '())

;; shell loses track of directory when using 'cd'
(use-package shell
  :commands (shell)
  )

;; ---------------------------------------------------------------------------
;; simple has visual-line-mode, movement function

(use-package simple
  :diminish
  :hook (text-mode . visual-line-mode)
  )

;; ---------------------------------------------------------------------------
;; save files upon focus-out-hook and switch-to-buffer

(use-package super-save
  :diminish
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
  (template-default-directories (list (concat onedrive-dir "emacs/templates/")))
  :bind
  ("C-c n p" . template-new-file)
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
  :defer  
  ;; C-x 8 C-h ;; list unicode characters
  :config
  ;; Swedish characters
  (define-key key-translation-map (kbd "<f12> a") (kbd "å")) ; C-x 8 / a
  (define-key key-translation-map (kbd "<f12> A") (kbd "Å")) ; C-x 8 / A
  (define-key key-translation-map (kbd "<f12> e") (kbd "ä")) ; C-x 8 " a
  (define-key key-translation-map (kbd "<f12> E") (kbd "Ä")) ; C-x 8 " A
  (define-key key-translation-map (kbd "<f12> o") (kbd "ö")) ; C-x 8 " o
  (define-key key-translation-map (kbd "<f12> O") (kbd "Ö")) ; C-x 8 " O
)

;; ----------------------------------------------------------------------------

(use-package theme-looper
  ;;:demand ; override delayed loading caused by :bind
  :custom
  (custom-theme-directory (concat onedrive-dir "emacs/themes/"))  
  :bind
  ("<f5>" . theme-looper-select-theme)
  ("<S-f5>" . theme-looper-reload-current-theme)
  ("<C-f5>" . theme-looper-enable-next-theme)
  :config
  (theme-looper-set-favorite-themes
   '(zen leuven spring summer autumn winter base16-mocha twilight-bright monokai faff modus-operandi
         pastel-red pastel-orange pastel-yellow pastel-green pastel-blue pastel-purple
         pastel-brown pastel-gray))
  )
;; load the color definitions for the pastel themes
(use-package base16-pastel-themes
  :commands load-theme)

(use-package modus-themes
  :defer
  :custom
  (modus-themes-diffs 'deuteranopia)
  (modus-themes-org-blocks t)
  (modus-themes-slanted-constructs t)
  (modus-themes-syntax 'green-strings)
  (modus-themes-fringes t)
  (modus-themes-headings
   '((1 . rainbow-section)
     (2 . rainbow-section)
     (3 . rainbow-section)
     (4 . rainbow-section)
     (5 . rainbow-section) ))
  (modus-themes-scale-headings t)
  (modus-themes-scale-1 1.01)
  (modus-themes-scale-2 1.1)
  (modus-themes-scale-3 1.2)
  (modus-themes-scale-4 1.3)
  (modus-themes-scale-5 1.4)  
  ;;(load-theme 'modus-operandi t)
  )

;; ---------------------------------------------------------------------------

(use-package time-stamp
  :commands (time-stamp)
  :custom
  (time-stamp-format "%02d %3b %:y %02H:%02M:%02S %F")
  :hook
  (before-save . time-stamp) )

;; ---------------------------------------------------------------------------

(use-package transpose-frame
  :bind ("C-c C-T" . transpose-frame)
  :commands (transpose-frame))

;; ---------------------------------------------------------------------------
;; unfill unwraps

(use-package unfill
  :defer
  :bind
  ;;  ("<C-?>" . fill-paragraph) ; C-S-/ in rstudio
  ("M-q" . unfill-toggle)
  ("<C-?>" . fill-paragraph) ; C-S-/ in rstudio
  )

;; ---------------------------------------------------------------------------
;; visual regexp

;; https://github.com/benma/visual-regexp.el
(use-package visual-regexp
  :disabled
  :bind ("C-c 5" . #'vr/replace))

;; Use modern regexp without extra parens
;; https://github.com/benma/visual-regexp-steroids.el/
(use-package visual-regexp-steroids
  :defer
  :disabled
  :bind (("C-S-r" . vr/isearch-backward) ;; C-M-r
         ("C-S-s" . vr/isearch-forward)
         ("<S-f3>" . vr/replace)
         ("<f3>" . vr/query-replace)) ;; C-M-s
  :custom
  (vr/match-separator-use-custom-face nil)
  )

;; ---------------------------------------------------------------------------
;; I don't use this very much and I don't like it in markdown mode.
;; which-func-modes controls which modes use it
;; https://emacsredux.com/blog/2014/04/05/which-function-mode/

(use-package which-function-mode
  :disabled
  :defer t
  :hook ((prog-mode . which-function-mode)) )

;; ----------------------------------------------------------------------------
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :diminish
  :defer 1
  :config
  (which-key-mode) )

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

(message "Emacs ready in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time)))
         gcs-done)


;; ---------------------------------------------------------------------------


;; https://stackoverflow.com/questions/27307757
;; todo, create a version that inserts this to other .R buffer
(defun ess-readline ()
  "Move to previous command entered from script *or* R-process and copy 
   to prompt for execution or editing"
  (interactive)
  ;; See how many times function was called
  (if (eq last-command 'ess-readline)
      (setq ess-readline-count (1+ ess-readline-count))
    (setq ess-readline-count 1))
  ;; Move to prompt and delete current input
  (comint-goto-process-mark)
  (end-of-buffer nil) ;; tweak here
  (comint-kill-input)
  ;; Copy n'th command in history where n = ess-readline-count
  (comint-previous-prompt ess-readline-count)
  (comint-copy-old-input)
  ;; Need to remove extra prompts like "R> R>"
  
  ;; Below is needed to update counter for sequential calls
  (setq this-command 'ess-readline)
)
(global-set-key (kbd "\C-cp") 'ess-readline)


;; ---------------------------------------------------------------------------


(defun new-move-file-and-buffer (new-dir)
 "Moves both current buffer and file it is visiting to DIR."
 ;;(interactive "sNew directory: ") ;; s=put readline result into new-dir
 (interactive)
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

(defun my--choose-directory (directory-to-start-in)
  "Return a directory chosen by the user.  The user will be prompted
to choose a directory starting with `directory-to-start-in'"
  (let* ((ivy-read-prompt "Choose directory: ")
         (counsel--find-file-predicate #'file-directory-p)
         (default-directory directory-to-start-in)
         (selected-directory
          (ivy-read
           ivy-read-prompt
           #'read-file-name-internal
           :matcher #'counsel--find-file-matcher)))
    selected-directory))



;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; https://www.anghyflawn.net/blog/2014/emacs-give-a-doi-get-a-bibtex-entry/
(defun get-bibtex-from-doi (doi)
 "Get a BibTeX entry from the DOI"
 (interactive "MDOI: ")
 (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
   (with-current-buffer 
     (url-retrieve-synchronously 
       (format "http://dx.doi.org/%s" 
       	(replace-regexp-in-string "http://dx.doi.org/" "" doi)))
     (switch-to-buffer (current-buffer))
     (goto-char (point-max))
     (setq bibtex-entry 
     	  (buffer-substring 
          	(string-match "@" (buffer-string))
              (point)))
     (kill-buffer (current-buffer))))
 (insert (decode-coding-string bibtex-entry 'utf-8))
 (bibtex-fill-entry))


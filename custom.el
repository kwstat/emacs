(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bookmark-save-flag 1 t)
 '(calendar-date-display-form
   (quote
    ((format "%02d.%02d.%4s %2s"
             (string-to-number month)
             (string-to-number day)
             year dayname))))
 '(comint-move-point-for-output t)
 '(comint-scroll-to-bottom-on-output t)
 '(completion-ignored-extensions
   (quote
    ("~" ".aux" ".bbl" ".blg" ".brf" ".bst" ".log" ".out" ".pdf" ".ps" ".doc" ".docx" ".ppt" ".pptx" ".xls" ".xlsx")))
 '(completion-show-help nil)
 '(completions-format (quote vertical))
 '(counsel-find-file-ignore-regexp
   "\\(\\`#\\|\\`\\.\\)\\|\\(~\\'\\|docx\\'\\|pdf\\'\\|pptx\\'\\|xls\\'\\|xlsx\\'\\)")
 '(custom-file "c:/Dropbox/emacs/custom.el")
 '(custom-theme-directory "c:/Dropbox/emacs/themes/")
 '(diary-date-forms
   (quote
    ((month "." day "." year " " dayname)
     (day "/" month "/" year "[^0-9]"))))
 '(diary-file "c:/Dropbox/blog/diary.txt")
 '(ediff-diff-options "-w" t)
 '(ess-R-message-prefixes
   (quote
    ("Error" "nfault" "singularities" "Warning" "WARNING")) t)
 '(ess-default-style (quote RStudio))
 '(ess-eval-visibly (quote nowait))
 '(ess-history-file nil)
 '(ess-indent-with-fancy-comments nil)
 '(ess-pdf-viewer-pref "c:/Progra~1/R/R-3.6.3/bin/x64/open.exe" t)
 '(ess-r-runner-prefixes nil)
 '(ess-r-versions nil)
 '(ess-roxy-str "#'")
 '(ess-roxy-template-alist
   (quote
    (("title" . ".title
#'")
     ("description" . ".desc")
     ("details" . ".details
#'")
     ("param" . "")
     ("return" . "")
     ("author" . "Kevin Wright")
     ("examples" . "
#' \\dontrun{}")
     ("references" . "
#' None")
     ("export" . ""))))
 '(ess-style (quote RStudio))
 '(ess-tab-complete-in-script t)
 '(ess-use-eldoc nil)
 '(ess-use-flymake nil)
 '(explicit-shell-file-name "c:/Rtools/bin/sh")
 '(find-file-visit-truename nil)
 '(frame-title-format "%b     %f" t)
 '(ibuffer-default-sorting-mode (quote recency))
 '(ibuffer-display-summary nil)
 '(ibuffer-formats (quote (((name 30 30 :left) " " filename))))
 '(inferior-R-args "--no-restore-history --no-save --quiet")
 '(inferior-ess-r-help-command ".ess.help('%s', help.type='text')
")
 '(inferior-ess-r-program "c:/Progra~1/R/R-3.6.3/bin/x64/rterm.exe")
 '(inhibit-splash-screen t)
 '(ispell-program-name "c:/bin/hunspell/bin/hunspell.exe" t)
 '(ivy-count-format "%d/%d ")
 '(ivy-height 15)
 '(ivy-use-virtual-buffers t)
 '(load-prefer-newer t)
 '(make-backup-files nil)
 '(markdown-command "pandoc")
 '(markdown-enable-wiki-links t)
 '(markdown-gfm-use-electric-backquote nil)
 '(markdown-hide-urls nil)
 '(markdown-link-space-sub-char "_")
 '(max-mini-window-height 20)
 '(minimal-session-saver-data-file "c:/kw/.emacs.d/minimal-session-saver-data.el")
 '(openwith-associations
   (quote
    (("\\.pdf$" "pdfxcview.exe"
      (file))
     ("\\.\\(doc\\|docx\\)$" "winword.exe"
      (file))
     ("\\.\\(ppt\\|pptx\\)$" "powerpnt.exe"
      (file))
     ("\\.\\(xls\\|xlsx\\)$" "excel.exe"
      (file)))))
 '(package-selected-packages
   (quote
    (theme-looper use-package unicode-fonts unfill transpose-frame sync-recentf super-save rainbow-mode rainbow-delimiters poly-R openwith minimal-session-saver mic-paren magit-popup magit key-combo key-chord ht ghub focus-autosave-mode ess dired-hacks-utils dired-filetype-face diminish counsel buttercup)))
 '(polymode-exporter-output-file-format "%s")
 '(python-shell-interpreter "C:/bin/WPy64-3741/python-3.7.4.amd64/python.exe" t)
 '(recentf-max-menu-items 15)
 '(recentf-max-saved-items 1000)
 '(reftex-bibpath-environment-variables (quote ("c:/x/notes/")))
 '(reftex-cite-format (quote natbib))
 '(reftex-default-bibliography (quote ("c:/x/notes/kw.bib")))
 '(reftex-file-extensions (quote ("Rnw" "rnw" "tex" "sty")))
 '(reftex-use-multiple-selection-buffers t)
 '(template-auto-insert t t)
 '(template-default-directories (quote ("c:/Dropbox/emacs/templates/")) t)
 '(text-quoting-style "straight")
 '(theme-changer-theme-list
   (quote
    (zen leuven spring summer autumn winter base16-mocha twilight-bright monokai)))
 '(time-stamp-format "%02d %3b %:y %02H:%02M:%02S %F")
 '(user-full-name "Kevin Wright")
 '(user-mail-address "kw.stat@gmail.com")
 '(vc-handled-backends nil)
 '(w32-use-visible-system-caret nil t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-current-match ((t (:foreground "yellow" :background "#006000" :weight bold)))))

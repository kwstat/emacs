;;; monokai-theme.el --- REQUIRES EMACS 24: Monokai Color Theme for Emacs.

;; Copyright (C) 2012 Lorenzo Villani.
;;

(deftheme monokai
  "Monokai color theme")

(let ((monokai-blue-light "#89BDFF")
      (monokai-gray "#595959")
      (monokai-gray-darker "#383830")
      (monokai-gray-darkest "#141411")
      (monokai-gray-lightest "#999989")
      (monokai-gray-light "#E6E6E6")
      (monokai-green "#A6E22A")
      (monokai-green-light "#A6E22E")
      (monokai-grey-dark "#272822")
      (monokai-magenta "#Fb86b5")
      (monokai-purple "#b791ff")
      (monokai-purple-light "#Fe5FF1")
      (monokai-yellow "#E6DB74")
      (monokai-yellow-dark "#b3ad66")
      (monokai-yellow-light "#F8F8F2"))

  (custom-theme-set-faces
   'monokai

   ;; Frame
   `(default ((t (:foreground ,monokai-yellow-light :background ,monokai-grey-dark))))
   `(button ((t (:foreground ,monokai-blue-light :underline t))))
   `(cursor ((t (:foreground ,monokai-magenta))))
   `(hl-line ((t (:background ,monokai-gray-darkest))))
   `(minibuffer-prompt ((t (:foreground ,monokai-yellow-dark))))
   `(modeline ((t (:background ,monokai-gray-lightest :foreground ,monokai-gray-light))))
   `(region ((t (:background ,monokai-gray-darker))))
   ;`(show-paren-match-face ((t (:background ,monokai-gray-darker))))

   ;; Main
   `(font-lock-builtin-face ((t (:foreground ,monokai-green))))
   ;`(font-lock-comment-face ((t (:foreground ,monokai-yellow-dark))))
   `(font-lock-comment-face ((t (:foreground ,monokai-gray-lightest))))
   `(font-lock-constant-face ((t (:foreground ,monokai-purple))))
   `(font-lock-doc-string-face ((t (:foreground ,monokai-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,monokai-green))))
   `(font-lock-keyword-face ((t (:foreground ,monokai-magenta))))
   `(font-lock-string-face ((t (:foreground ,monokai-yellow))))
   `(font-lock-type-face ((t (:foreground ,monokai-blue-light))))
   `(font-lock-variable-name-face ((t (:foreground ,monokai-magenta))))
   `(font-lock-warning-face ((t (:bold t :foreground ,monokai-purple-light))))

   ;; CUA
   `(cua-rectangle ((t (:background ,monokai-gray-darkest))))

   ;; IDO
   `(ido-first-match ((t (:foreground ,monokai-purple))))
   `(ido-only-match ((t (:foreground ,monokai-green))))
   `(ido-subdir ((t (:foreground ,monokai-blue-light))))

   ;; Whitespace
   `(whitespace-space ((t (:foreground ,monokai-gray))))

   ;; comint
   `(comint-highlight-prompt ((t (:foreground ,monokai-blue-light :weight bold))))

   ;; ess
   `(ess-tracebug-last-input-fringe-face ((t (:foreground ,monokai-blue-light  :overline ,monokai-blue-light))))

   ;; latex
   `(font-latex-sedate-face ((t (:foreground ,monokai-yellow))))
   `(font-latex-warning-face ((t (:foreground ,monokai-purple-light))))

   ;; org

;;   '(org-agenda-date ((t (:foreground "#66d9ef"))))
;;   '(org-agenda-date-today ((t (:foreground "#a6e22e"))))
;;   '(org-agenda-date-weekend ((t (:foreground "#66d9ef" :weight bold))))
;;   '(org-agenda-structure ((t (:foreground "#66d9ef"))))
   '(org-date ((t (:foreground "#66d9ef" :underline t))))
;;   '(org-done ((t (:foreground "#a6e22e" :weight bold))))
;;   '(org-drawer ((t (:foregruond "#ae81ff"))))
;;   '(org-ellipsis ((t (:foreground "#f9f8f5" :underline t))))
;;   '(org-footnote ((t (:foreground "#a1efe4" :underline t))))
;;   '(org-hide ((t (:foreground "#383830"))))
;;   '(org-latex-and-related ((t (:foreground "#fd971f"))))
   `(org-link ((t (:foreground ,monokai-blue-light :underline t))))
;;   '(org-list-dt ((t (:foreground "#f5f4f1" :weight bold))))
;;   '(org-scheduled ((t (:foreground "#a6e22e"))))
;;   '(org-scheduled-today ((t (:foreground "#a6e22e" :weight bold))))
;;   '(org-table ((t (:foreground "#ae81ff"))))
;;   '(org-todo ((t (:foreground "#f92672" :weight bold))))

   ;; parentheses
  ;; Parentheses
  '(show-paren-match ((t (:background "#80a01a" :foreground "#f9f8f5"))))
  '(show-paren-mismatch ((t (:background "#fd971f" :foreground "#f9f8f5"))))

  '(rainbow-delimiters-depth-1-face ((t (:foreground "#f92672"))))
  '(rainbow-delimiters-depth-2-face ((t (:foreground "#f4bf75"))))
  '(rainbow-delimiters-depth-3-face ((t (:foreground "#fd971f"))))
  '(rainbow-delimiters-depth-4-face ((t (:foreground "#a6e22e"))))
  '(rainbow-delimiters-depth-5-face ((t (:foreground "#ae81ff"))))
  '(rainbow-delimiters-depth-6-face ((t (:foreground "#a1efe4"))))
  '(rainbow-delimiters-depth-7-face ((t (:foreground "#cc6633"))))
  '(rainbow-delimiters-depth-8-face ((t (:foreground "#66d9ef"))))
  '(rainbow-delimiters-depth-9-face ((t (:foreground "#a7e22e"))))
  '(rainbow-delimiters-unmatched-face ((t (:foreground "#f92672"))))


))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background "nil")))))))

(provide-theme 'monokai)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; monokai-theme.el ends here

;; ---------------------------------------------------------------------------

;;  ;; Basics
;;   '(default ((t (:foreground "#f9f8f5" :background "#272822"))))
;;   '(bold ((t (:weight bold))))
;;   '(bold-italic ((t (:slant italic :weight bold))))
;;   '(underline ((t (:underline t))))
;;   '(italic ((t (:slant italic))))
;;   '(shadow ((t (:foreground "#49483e"))))
;;   '(success ((t (:foreground "#a6e22e"))))
;;   '(error ((t (:foreground "#f92672"))))
;;   '(warning ((t (:foreground "#fd971f"))))

;;   ;; Outline
;;   '(outline-1 ((t (:foreground "#f92672"))))
;;   '(outline-2 ((t (:foreground "#a6e22e"))))
;;   '(outline-3 ((t (:foreground "#fd971f"))))
;;   '(outline-4 ((t (:foreground "#a1efe4"))))
;;   '(outline-5 ((t (:foreground "#f4bf75"))))
;;   '(outline-6 ((t (:foreground "#66d9ef"))))
;;   '(outline-7 ((t (:foreground "#fd971f"))))
;;   '(outline-8 ((t (:foreground "#ae81ff"))))

;;   ;; Font-lock stuff
;;   '(font-lock-builtin-face ((t (:foreground "#a1efe4"))))
;;   '(font-lock-comment-delimiter-face ((t (:foreground "#a59f85" :slant italic))))
;;   '(font-lock-comment-face ((t (:foreground "#a59f85" :slant italic))))
;;   '(font-lock-constant-face ((t (:foreground "#a1efe4"))))
;;   '(font-lock-doc-face ((t (:foreground "#a59f85"))))
;;   '(font-lock-doc-string-face ((t (:foreground "#f4bf75"))))
;;   '(font-lock-function-name-face ((t (:foreground "#66d9ef"))))
;;   '(font-lock-keyword-face ((t (:foreground "#ae81ff"))))
;;   '(font-lock-negation-char-face ((t (:foreground "#a6e22e"))))
;;   '(font-lock-preprocessor-face ((t (:foreground "#ae81ff"))))
;;   '(font-lock-regexp-grouping-backslash ((t (:foreground "#f4bf75"))))
;;   '(font-lock-regexp-grouping-construct ((t (:foreground "#ae81ff"))))
;;   '(font-lock-string-face ((t (:foreground "#a6e22e"))))
;;   '(font-lock-type-face ((t (:foreground "#f4bf75"))))
;;   '(font-lock-variable-name-face ((t (:foreground "#fd971f"))))
;;   '(font-lock-warning-face ((t (:foreground "#f92672" :weight bold))))

;;   ;; Emacs interface
;;   '(cursor ((t (:foreground "#272822" :background "#a7e22e"))))
;;   '(fringe ((t (:background "#383830"))))
;;   '(linum ((t (:foreground "#75715e" :background "#383830"))))
;;   '(hl-line ((t (:background "#383830"))))
;;   '(border ((t (:background "#383830"))))
;;   '(border-glyph ((t (:background "#383830"))))
;;   '(highlight ((t (:foreground "#272822" :background "#80a01a"))))
;;   '(link ((t (:foreground "#66d9ef" :underline t))))
;;   '(link-visited ((t (:foreground "#ae81ff" :underline t))))
;;   '(gui-element ((t (:foreground "#f9f8f5" :background "#383830"))))
;;   '(minibuffer-prompt ((t (:foreground "#a7e22e"))))
;;   '(region ((t (:foreground "#272822" :background "#80a01a"))))
;;   '(secondary-selection ((t (:foreground "#272822" :background "#a7e22e"))))
;;   '(header-line ((t (:inherit mode-line))))

;;   ;; Whitespace
;;   '(trailing-whitespace ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-empty ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-hspace ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-indentation ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-line ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-newline ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-space ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-space ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-space ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-tab ((t (:foreground "#a59f85" :background "#49483e"))))
;;   '(whitespace-trailing ((t (:foreground "#a59f85" :background "#49483e"))))

;;   ;; Search
;;   '(match ((t (:foreground "#272822" :background "#80a01a"))))
;;   '(isearch ((t (:foreground "#272822" :background "#80a01a"))))
;;   '(isearch-lazy-highlight-face ((t (:foreground "#272822" :background "#80a01a"))))
;;   '(isearch-fail ((t (:foreground "#272822" :background "#f92672"))))

;;   ;; IDO
;;   '(ido-subdir ((t (:foreground "#a6e22e" :weight bold))))
;;   '(ido-first-match ((t (:foreground "#80a01a"))))
;;   '(ido-only-match ((t (:foreground "#66d9ef" :weight bold))))
;;   '(ido-indicator ((t (:foreground "#80a01a"))))
;;   '(ido-virtual ((t (:foreground "#cc6633"))))

;;   ;; Undo-tree
;;   '(undo-tree-visualizer-default-face ((t (:foreground "#f9f8f5"))))
;;   '(undo-tree-visualizer-current-face ((t (:foreground "#272822" :background "#a6e22e"))))
;;   '(undo-tree-visualizer-active-branch-face ((t (:foreground "#66d9ef"))))
;;   '(undo-tree-visualizer-register-face ((t (:foreground "#f4bf75"))))
;;   '(undo-tree-visualizer-unmodified-face ((t (:foreground "#ae81ff"))))

;;   ;; Magit
;;   '(magit-branch ((t (:foreground "#a6e22e" :weight bold))))
;;   '(magit-cherry-equivalent ((t (:foreground "#a1efe4"))))
;;   '(magit-cherry-unmatched ((t (:foreground "#f92672"))))
;;   '(magit-diff-add ((t (:foreground "#272822" :background "#a6e22e"))))
;;   '(magit-diff-del ((t (:foreground "#272822" :background "#f92672"))))
;;   '(magit-diff-file-header ((t (:foreground "#272822" :background "#a7e22e"))))
;;   '(magit-diff-hunk-header ((t (:foreground "#272822" :background "#a7e22e"))))
;;   '(magit-diff-merge-current ((t (:foreground "#ae81ff"))))
;;   '(magit-diff-merge-diff3-separator ((t (:foreground "#ae81ff"))))
;;   '(magit-diff-merge-proposed ((t (:foreground "#cc6633"))))
;;   '(magit-diff-merge-separator ((t (:foreground "#cc6633"))))
;;   '(magit-diff-none ((t (:slant italic))))
;;   '(magit-header ((t (:background "#383830" :weight bold))))
;;   '(magit-item-highlight ((t (:foreground "#272822" :background "#80a01a"))))
;;   '(magit-item-mark ((t (:foreground "#272822" :background "#80a01a"))))
;;   '(magit-key-mode-args-face ((t (:foreground "#272822" :background "#f5f4f1"))))
;;   '(magit-key-mode-button-face ((t (:foreground "#a1efe4"))))
;;   '(magit-key-mode-header-face ((t (:foreground "#ae81ff"))))
;;   '(magit-key-mode-switch-face ((t (:foreground "#f92672" :weight bold))))
;;   '(magit-log-author ((t (:foreground "#66d9ef"))))
;;   '(magit-log-date ((t (:foreground "#ae81ff"))))
;;   '(magit-log-graph ((t (:foreground "#f9f8f5"))))
;;   '(magit-log-head-label-bisect-bad ((t (:foreground "#272822" :background "#f92672"))))
;;   '(magit-log-head-label-bisect-good ((t (:foreground "#272822" :background "#a6e22e"))))
;;   '(magit-log-head-label-bisect-skip ((t (:foreground "#272822" :background "#f4bf75"))))
;;   '(magit-log-head-label-default ((t (:foreground "#66d9ef" :background "#272822" :box 1))))
;;   '(magit-log-head-label-head ((t (:foreground "#fd971f" :background "#272822" :box 1))))
;;   '(magit-log-head-label-local ((t (:foreground "#ae81ff" :background "#272822" :box 1))))
;;   '(magit-log-head-label-patches ((t (:foreground "#f92672" :background "#272822" :box 1))))
;;   '(magit-log-head-label-remote ((t (:foreground "#a6e22e" :background "#272822" :box 1))))
;;   '(magit-log-head-label-tags ((t (:foreground "#f4bf75" :background "#272822" :box 1))))
;;   '(magit-log-head-wip ((t (:foreground "#a1efe4" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-amend ((t (:foreground "#f4bf75" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-checkout ((t (:foreground "#a1efe4" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-cherry-pick ((t (:foreground "#a6e22e" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-commit ((t (:foreground "#66d9ef" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-merge ((t (:foreground "#cc6633" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-other ((t (:foreground "#f9f8f5" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-rebase ((t (:foreground "#ae81ff" :background "#272822" :box 1))))
;;   '(magit-log-reflog-label-remote ((t (:foreground "#fd971f" :background "#272822" :box 1))))


;;   ;; multiple-cursors
;;   '(mc/cursor-face ((t (:foreground "#272822" :background "#80a01a" :inverse-video nil)))))

;; (provide-theme 'monokai)

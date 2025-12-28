;;; emacspeak-markdown.el --- Speech-enable Markdown-Mode -*- lexical-binding: t; -*-
;; Description: Speech-enable Markdown-Mode for better header and structure navigation
;; Keywords: Emacspeak, Audio Desktop, Markdown, documentation

;;;   Copyright:
;; This file is not part of GNU Emacs, but the same permissions apply.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Markdown-mode provides major mode support for editing Markdown files.
;; This module speech-enables markdown-mode to provide better auditory
;; feedback when navigating headers, lists, links, and other markdown
;; structures. Instead of hearing "three pounds space", users will hear
;; "heading level 3" when navigating headers.
;;
;; This module also provides `emacspeak-markdown-reading-mode`, a minor mode
;; that strips markup syntax when reading content, so you hear "car" instead
;; of "star star car star star" for **car**. Voice personalities still apply
;; to indicate emphasis, headings, etc.
;;
;; Reading mode handles many markdown constructs intelligently:
;; - Images: ![alt](url) → "image: alt"
;; - Links: [text](url) → "text link" (no URL noise)
;; - Task lists: - [x] → "checked: text" or "unchecked: text"
;; - Code fences: ``` → "code block: language"
;; - Tables: Announces "row N column N content"
;; - Footnotes: [^1] → "footnote 1"
;; - Horizontal rules: --- → "section separator"
;; - HTML comments: <!-- --> → "comment"
;; - Reference links: Skipped entirely
;; - Escaped chars: \* → * (removes backslash)
;;
;; Keybindings:
;; - C-c C-s h: Speak current heading
;; - C-c C-s r: Toggle reading mode

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(require 'emacspeak-preamble)
(require 'markdown-mode nil 'noerror)

;;;  Silence byte-compiler:

(defvar markdown-mode-map)

;;;  Forward declarations:

(declare-function markdown-heading-at-point "markdown-mode")
(declare-function markdown-outline-level "markdown-mode")

;;;  Customization:

(defcustom emacspeak-markdown-auto-reading-mode nil
  "When non-nil, automatically enable reading mode in markdown buffers.
Reading mode strips markup syntax when speaking lines, making content
easier to listen to without hearing 'star star' for bold, 'pound' for
headings, etc."
  :type 'boolean
  :group 'emacspeak-markdown)

;;;  Map faces to voices:

(voice-setup-add-map
 '((markdown-header-face-1 voice-bolden-extra)
   (markdown-header-face-2 voice-bolden-medium)
   (markdown-header-face-3 voice-bolden)
   (markdown-header-face-4 voice-animate)
   (markdown-header-face-5 voice-brighten)
   (markdown-header-face-6 voice-lighten)
   (markdown-header-delimiter-face voice-smoothen)
   (markdown-header-rule-face voice-smoothen)
   (markdown-bold-face voice-bolden)
   (markdown-italic-face voice-animate)
   (markdown-strike-through-face voice-smoothen)
   (markdown-code-face voice-monotone)
   (markdown-inline-code-face voice-monotone)
   (markdown-pre-face voice-monotone)
   (markdown-blockquote-face voice-annotate)
   (markdown-list-face voice-brighten)
   (markdown-link-face voice-bolden)
   (markdown-url-face voice-lighten)
   (markdown-markup-face voice-smoothen)
   (markdown-footnote-marker-face voice-annotate)
   (markdown-footnote-text-face voice-annotate)))

;;;  Helper functions:

(defun emacspeak-markdown--get-heading-info ()
  "Return heading information at point as a string.
Returns format like 'heading level 2: Introduction'."
  (save-excursion
    (beginning-of-line)
    (cond
     ;; ATX-style headings: # Heading
     ((looking-at "^\\(#+\\)[ \t]+\\(.*\\)$")
      (let ((level (length (match-string 1)))
            (text (string-trim (match-string 2))))
        (format "heading level %d: %s" level text)))
     ;; Try markdown-mode function if available
     ((and (fboundp 'markdown-heading-at-point)
           (fboundp 'markdown-outline-level))
      (let ((heading (markdown-heading-at-point)))
        (when heading
          (let* ((level (markdown-outline-level))
                 (text (string-trim heading)))
            (format "heading level %d: %s" level text))))))))

(defun emacspeak-markdown--speak-heading ()
  "Speak the heading at point with level information."
  (let ((info (emacspeak-markdown--get-heading-info)))
    (if info
        (dtk-speak info)
      (emacspeak-speak-line))))

(defun emacspeak-markdown--at-list-item-p ()
  "Return non-nil if point is at a list item."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*[-*+][ \t]+\\|^[ \t]*[0-9]+\\.[ \t]+")))

(defun emacspeak-markdown--at-task-list-p ()
  "Return non-nil if point is at a task list item.
Returns 'checked or 'unchecked if it's a task list."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^[ \t]*[-*+][ \t]+\\[x\\]") 'checked)
     ((looking-at "^[ \t]*[-*+][ \t]+\\[X\\]") 'checked)
     ((looking-at "^[ \t]*[-*+][ \t]+\\[ \\]") 'unchecked)
     (t nil))))

(defun emacspeak-markdown--at-horizontal-rule-p ()
  "Return non-nil if point is at a horizontal rule."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*\\(---+\\|\\*\\*\\*+\\|___+\\)[ \t]*$")))

(defun emacspeak-markdown--at-code-fence-p ()
  "Return language name if at code fence start, 'end if at fence end, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^[ \t]*```\\([a-zA-Z0-9_+-]*\\)[ \t]*$")
      (let ((lang (match-string 1)))
        (if (string-empty-p lang) "code" lang)))
     ((looking-at "^[ \t]*~~~\\([a-zA-Z0-9_+-]*\\)[ \t]*$")
      (let ((lang (match-string 1)))
        (if (string-empty-p lang) "code" lang)))
     (t nil))))

(defun emacspeak-markdown--at-html-comment-p ()
  "Return non-nil if point is at an HTML comment line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*<!--.*-->[ \t]*$")))

(defun emacspeak-markdown--at-reference-link-def-p ()
  "Return non-nil if point is at a reference link definition."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*\\[.+\\]:[ \t]+\\S-")))

(defun emacspeak-markdown--at-footnote-def-p ()
  "Return footnote number if at footnote definition, nil otherwise."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\[\\^\\([^]]+\\)\\]:[ \t]*")
      (match-string 1))))

(defun emacspeak-markdown--at-indented-code-p ()
  "Return non-nil if point is at an indented code block line (4+ spaces)."
  (save-excursion
    (beginning-of-line)
    (and (looking-at "^    \\|^\t")
         (not (emacspeak-markdown--at-list-item-p)))))

(defun emacspeak-markdown--at-table-row-p ()
  "Return non-nil if point is at a table row."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*|.*|[ \t]*$")))

(defun emacspeak-markdown--at-table-separator-p ()
  "Return non-nil if point is at a table separator line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[ \t]*|[ \t]*[-:]+[ \t]*|")))

(defun emacspeak-markdown--at-definition-list-p ()
  "Return non-nil if point is at a definition list definition line."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^:[ \t]+")))

(defun emacspeak-markdown--speak-list-item ()
  "Speak list item with appropriate context."
  (if (emacspeak-markdown--at-list-item-p)
      (let ((line (thing-at-point 'line t)))
        (dtk-speak (concat "list item: " (string-trim line))))
    (emacspeak-speak-line)))

;;;  Reading mode - strip markup for clean speech:

(defun emacspeak-markdown--strip-markup (text)
  "Remove markdown markup from TEXT while preserving text properties.
This makes reading more pleasant by removing syntax noise like
**, __, `, and # characters while keeping voice personalities."
  (when text
    (let ((result text))
      ;; Remove ATX heading markers (# at start of line)
      (setq result (replace-regexp-in-string "^#+\\s-*" "" result))
      ;; Remove setext heading underlines
      (setq result (replace-regexp-in-string "^[=-]+$" "" result))
      ;; Remove bold markers ** and __
      (setq result (replace-regexp-in-string "\\*\\*\\([^*]+\\)\\*\\*" "\\1" result))
      (setq result (replace-regexp-in-string "__\\([^_]+\\)__" "\\1" result))
      ;; Remove italic markers * and _
      (setq result (replace-regexp-in-string "\\*\\([^*]+\\)\\*" "\\1" result))
      (setq result (replace-regexp-in-string "_\\([^_]+\\)_" "\\1" result))
      ;; Remove inline code backticks
      (setq result (replace-regexp-in-string "`\\([^`]+\\)`" "\\1" result))
      ;; Remove strikethrough ~~
      (setq result (replace-regexp-in-string "~~\\([^~]+\\)~~" "\\1" result))
      ;; Remove escaped characters (backslashes)
      (setq result (replace-regexp-in-string "\\\\\\(.\\)" "\\1" result))
      ;; Handle images: ![alt](url) -> "image: alt"
      (setq result (replace-regexp-in-string "!\\[\\([^]]+\\)\\](\\([^)]+\\))" "image: \\1" result))
      ;; Remove link markup [text](url) -> text link
      (setq result (replace-regexp-in-string "\\[\\([^]]+\\)\\](\\([^)]+\\))" "\\1 link" result))
      ;; Remove reference-style link markup [text][ref] -> text link
      (setq result (replace-regexp-in-string "\\[\\([^]]+\\)\\]\\[[^]]*\\]" "\\1 link" result))
      ;; Remove autolink angle brackets <url> -> url
      (setq result (replace-regexp-in-string "<\\([^>]+\\)>" "\\1" result))
      ;; Handle footnote markers [^1] -> "footnote 1"
      (setq result (replace-regexp-in-string "\\[\\^\\([^]]+\\)\\]" "footnote \\1" result))
      ;; Remove task list markers but keep state indication
      (setq result (replace-regexp-in-string "^\\s-*[-*+]\\s-+\\[x\\]\\s-+" "checked: " result))
      (setq result (replace-regexp-in-string "^\\s-*[-*+]\\s-+\\[X\\]\\s-+" "checked: " result))
      (setq result (replace-regexp-in-string "^\\s-*[-*+]\\s-+\\[ \\]\\s-+" "unchecked: " result))
      ;; Remove regular list markers (-, *, +, numbers)
      (setq result (replace-regexp-in-string "^\\s-*[-*+]\\s-+" "" result))
      (setq result (replace-regexp-in-string "^\\s-*[0-9]+\\.\\s-+" "" result))
      ;; Remove blockquote markers
      (setq result (replace-regexp-in-string "^>+\\s-*" "" result))
      ;; Remove leading spaces from indented code blocks
      (setq result (replace-regexp-in-string "^    " "" result))
      (setq result (replace-regexp-in-string "^\t" "" result))
      ;; Remove table pipes
      (setq result (replace-regexp-in-string "^\\s-*|\\s-*" "" result))
      (setq result (replace-regexp-in-string "\\s-*|\\s-*$" "" result))
      (setq result (replace-regexp-in-string "\\s-*|\\s-*" " " result))
      ;; Remove definition list markers
      (setq result (replace-regexp-in-string "^:\\s-+" "" result))
      result)))

(defun emacspeak-markdown--speak-table-row ()
  "Speak table row with column announcements."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^[ \t]*|\\(.*\\)|[ \t]*$")
      (let* ((content (match-string 1))
             (cells (split-string content "|" t "[ \t]+"))
             (row-num 1) ; Could be enhanced to track actual row number
             (col-num 1)
             (result ""))
        (setq result (format "row %d " row-num))
        (dolist (cell cells)
          (setq result (concat result (format "column %d %s " col-num (string-trim cell))))
          (setq col-num (1+ col-num)))
        result))))

(defun emacspeak-markdown--speak-line-clean ()
  "Speak current line with markdown markup removed."
  (let* ((start (line-beginning-position))
         (end (line-end-position))
         (text (buffer-substring start end))
         (clean-text (emacspeak-markdown--strip-markup text))
         (heading-info (emacspeak-markdown--get-heading-info))
         (task-state (emacspeak-markdown--at-task-list-p))
         (code-fence (emacspeak-markdown--at-code-fence-p))
         (footnote-num (emacspeak-markdown--at-footnote-def-p))
         (prefix ""))
    ;; Build announcement prefix for special line types
    (cond
     ;; Horizontal rules - just announce and play icon
     ((emacspeak-markdown--at-horizontal-rule-p)
      (emacspeak-icon 'item)
      (dtk-speak "section separator")
      (setq prefix nil)) ; Don't speak the line content

     ;; HTML comments - just say "comment"
     ((emacspeak-markdown--at-html-comment-p)
      (dtk-speak "comment")
      (setq prefix nil))

     ;; Reference link definitions - skip entirely
     ((emacspeak-markdown--at-reference-link-def-p)
      (setq prefix nil))

     ;; Table separator lines - skip
     ((emacspeak-markdown--at-table-separator-p)
      (setq prefix nil))

     ;; Code fences
     (code-fence
      (emacspeak-icon 'open-object)
      (dtk-speak (format "code block: %s" code-fence))
      (setq prefix nil))

     ;; Footnote definitions
     (footnote-num
      (setq prefix (format "footnote %s definition: " footnote-num)))

     ;; Table rows
     ((emacspeak-markdown--at-table-row-p)
      (let ((table-speech (emacspeak-markdown--speak-table-row)))
        (when table-speech
          (dtk-speak table-speech)
          (setq prefix nil))))

     ;; Indented code blocks
     ((emacspeak-markdown--at-indented-code-p)
      (setq prefix "code: "))

     ;; Definition lists
     ((emacspeak-markdown--at-definition-list-p)
      (setq prefix "definition: "))

     ;; Headings get level announcement
     (heading-info
      (when (string-match "heading level \\([0-9]+\\)" heading-info)
        (let ((level (match-string 1 heading-info)))
          (emacspeak-icon 'section)
          (setq prefix (format "level %s " level)))))

     ;; Task list items - already handled in strip-markup with checked/unchecked
     (task-state
      (emacspeak-icon 'mark-object))

     ;; Regular list items get brief announcement
     ((emacspeak-markdown--at-list-item-p)
      (let ((indent (save-excursion
                      (beginning-of-line)
                      (skip-chars-forward " \t")
                      (current-column))))
        (cond
         ((= indent 0) (setq prefix "item "))
         ((< indent 4) (setq prefix "subitem "))
         (t (setq prefix "subsubitem "))))))

    ;; Speak with prefix (only if prefix wasn't set to nil)
    (when prefix
      (dtk-speak (concat prefix (or clean-text ""))))))

(defun emacspeak-markdown--speak-paragraph-clean ()
  "Speak current paragraph with markdown markup removed."
  (let* ((bounds (bounds-of-thing-at-point 'paragraph))
         (text (when bounds
                 (buffer-substring (car bounds) (cdr bounds))))
         (clean-text (when text
                       (emacspeak-markdown--strip-markup text))))
    (dtk-speak (or clean-text ""))))

;;;  Reading mode minor mode:

(defvar-local emacspeak-markdown-reading-mode nil
  "Non-nil when markdown reading mode is active.")

(defun emacspeak-markdown-reading-mode (&optional arg)
  "Toggle markdown reading mode for clean speech without markup noise.
When enabled, speaking lines strips markdown syntax characters while
preserving voice personalities. Heading and list announcements work
regardless of this mode."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq emacspeak-markdown-reading-mode
        (cond
         ((eq arg 'toggle) (not emacspeak-markdown-reading-mode))
         ((null arg) t)
         ((> (prefix-numeric-value arg) 0) t)
         (t nil)))
  (message (if emacspeak-markdown-reading-mode
               "Markdown reading mode enabled - markup stripped from speech"
             "Markdown reading mode disabled")))

(defadvice emacspeak-speak-line (around emacspeak-markdown-reading pre act comp)
  "In markdown mode, improve heading and list announcements.
When reading mode is enabled, also strip markup."
  (if (and (boundp 'major-mode)
           (eq major-mode 'markdown-mode))
      (if emacspeak-markdown-reading-mode
          (emacspeak-markdown--speak-line-clean)
        ;; Even without reading mode, announce headings properly
        (let* ((heading-info (emacspeak-markdown--get-heading-info))
               (is-list (emacspeak-markdown--at-list-item-p)))
          (cond
           (heading-info
            (emacspeak-icon 'section)
            (dtk-speak heading-info))
           (is-list
            (emacspeak-icon 'item)
            ad-do-it)
           (t ad-do-it))))
    ad-do-it))

;;;  Advice heading navigation commands:

(defadvice markdown-next-heading (after emacspeak pre act comp)
  "Speak the heading we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (emacspeak-markdown--speak-heading)))

(defadvice markdown-previous-heading (after emacspeak pre act comp)
  "Speak the heading we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (emacspeak-markdown--speak-heading)))

(defadvice markdown-next-visible-heading (after emacspeak pre act comp)
  "Speak the visible heading we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (emacspeak-markdown--speak-heading)))

(defadvice markdown-previous-visible-heading (after emacspeak pre act comp)
  "Speak the visible heading we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (emacspeak-markdown--speak-heading)))

(defadvice markdown-outline-next (after emacspeak pre act comp)
  "Speak the outline item we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'item)
    (emacspeak-markdown--speak-heading)))

(defadvice markdown-outline-previous (after emacspeak pre act comp)
  "Speak the outline item we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'item)
    (emacspeak-markdown--speak-heading)))

;;;  Advice link navigation:

(defun emacspeak-markdown--get-link-text ()
  "Extract and return the link text at point without the URL.
Returns just the visible text portion of markdown links."
  (save-excursion
    (let* ((line (thing-at-point 'line t))
           (text line))
      ;; Extract text from inline links [text](url)
      (when (string-match "\\[\\([^]]+\\)\\](\\([^)]+\\))" text)
        (setq text (match-string 1 text)))
      ;; Extract text from reference links [text][ref]
      (when (string-match "\\[\\([^]]+\\)\\]\\[[^]]*\\]" text)
        (setq text (match-string 1 text)))
      ;; Extract text from autolinks <url>
      (when (string-match "<\\([^>]+\\)>" text)
        (setq text (match-string 1 text)))
      ;; Clean up the text
      (string-trim text))))

(defadvice markdown-next-link (after emacspeak pre act comp)
  "Speak the link we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'button)
    (let ((link-text (emacspeak-markdown--get-link-text)))
      (dtk-speak (concat "link: " link-text)))))

(defadvice markdown-previous-link (after emacspeak pre act comp)
  "Speak the link we moved to."
  (when (ems-interactive-p)
    (emacspeak-icon 'button)
    (let ((link-text (emacspeak-markdown--get-link-text)))
      (dtk-speak (concat "link: " link-text)))))

;;;  Advice list item movement:

(defadvice markdown-move-list-item-up (after emacspeak pre act comp)
  "Announce list item movement."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (dtk-speak "moved up")))

(defadvice markdown-move-list-item-down (after emacspeak pre act comp)
  "Announce list item movement."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (dtk-speak "moved down")))

;;;  Advice header insertion/modification:

(defadvice markdown-insert-header (after emacspeak pre act comp)
  "Announce header insertion."
  (when (ems-interactive-p)
    (emacspeak-icon 'open-object)
    (emacspeak-markdown--speak-heading)))

(defadvice markdown-remove-header (after emacspeak pre act comp)
  "Announce header removal."
  (when (ems-interactive-p)
    (emacspeak-icon 'delete-object)
    (dtk-speak "header removed")))

(defadvice markdown-cycle-atx (after emacspeak pre act comp)
  "Speak new header level after cycling."
  (when (ems-interactive-p)
    (emacspeak-icon 'select-object)
    (emacspeak-markdown--speak-heading)))

(defadvice markdown-cycle-setext (after emacspeak pre act comp)
  "Speak new header level after cycling."
  (when (ems-interactive-p)
    (emacspeak-icon 'select-object)
    (emacspeak-markdown--speak-heading)))

;;;  Batch advice for ATX header insertion:

(cl-loop
 for f in
 '(markdown-insert-header-atx-1
   markdown-insert-header-atx-2
   markdown-insert-header-atx-3
   markdown-insert-header-atx-4
   markdown-insert-header-atx-5
   markdown-insert-header-atx-6)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Announce header insertion."
     (when (ems-interactive-p)
       (emacspeak-icon 'open-object)
       (emacspeak-markdown--speak-heading)))))

;;;  Advice subtree movement:

(defadvice markdown-move-subtree-up (after emacspeak pre act comp)
  "Announce subtree movement."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (dtk-speak "subtree moved up")))

(defadvice markdown-move-subtree-down (after emacspeak pre act comp)
  "Announce subtree movement."
  (when (ems-interactive-p)
    (emacspeak-icon 'large-movement)
    (dtk-speak "subtree moved down")))

;;;  Interactive convenience command:

(defun emacspeak-markdown-speak-heading ()
  "Speak the current heading with level information."
  (interactive)
  (let ((info (emacspeak-markdown--get-heading-info)))
    (if info
        (dtk-speak info)
      (message "Not at a heading"))))

;;;  Setup:

(defun emacspeak-markdown-setup ()
  "Setup Emacspeak support for Markdown-Mode."
  (when (boundp 'markdown-mode-map)
    (define-key markdown-mode-map (kbd "C-c C-s h") 'emacspeak-markdown-speak-heading)
    (define-key markdown-mode-map (kbd "C-c C-s r") 'emacspeak-markdown-reading-mode)))

(defun emacspeak-markdown-mode-hook ()
  "Hook function to setup Emacspeak markdown features in markdown buffers."
  (when emacspeak-markdown-auto-reading-mode
    (emacspeak-markdown-reading-mode 1)))

(eval-after-load "markdown-mode"
  (lambda ()
    (emacspeak-markdown-setup)
    (add-hook 'markdown-mode-hook 'emacspeak-markdown-mode-hook)
    ;; Enable heading/list announcements by default
    (ad-enable-advice 'emacspeak-speak-line 'around 'emacspeak-markdown-reading)
    (ad-activate 'emacspeak-speak-line)))

;;;  Enable/Disable support:

(defvar emacspeak-markdown--advice-list
  '((markdown-next-heading after)
    (markdown-previous-heading after)
    (markdown-next-visible-heading after)
    (markdown-previous-visible-heading after)
    (markdown-outline-next after)
    (markdown-outline-previous after)
    (markdown-next-link after)
    (markdown-previous-link after)
    (markdown-move-list-item-up after)
    (markdown-move-list-item-down after)
    (markdown-insert-header after)
    (markdown-remove-header after)
    (markdown-cycle-atx after)
    (markdown-cycle-setext after)
    (markdown-insert-header-atx-1 after)
    (markdown-insert-header-atx-2 after)
    (markdown-insert-header-atx-3 after)
    (markdown-insert-header-atx-4 after)
    (markdown-insert-header-atx-5 after)
    (markdown-insert-header-atx-6 after)
    (markdown-move-subtree-up after)
    (markdown-move-subtree-down after))
  "List of advised functions for Emacspeak Markdown support.")

(defun emacspeak-markdown-enable ()
  "Enable Emacspeak support for Markdown."
  (interactive)
  (dolist (advice emacspeak-markdown--advice-list)
    (ad-enable-advice (car advice) (cadr advice) 'emacspeak)
    (ad-activate (car advice)))
  ;; Enable the line-reading advice for headings/lists
  (ad-enable-advice 'emacspeak-speak-line 'around 'emacspeak-markdown-reading)
  (ad-activate 'emacspeak-speak-line)
  (emacspeak-markdown-setup)
  (message "Enabled Emacspeak Markdown support"))

(defun emacspeak-markdown-disable ()
  "Disable Emacspeak support for Markdown."
  (interactive)
  (dolist (advice emacspeak-markdown--advice-list)
    (ad-disable-advice (car advice) (cadr advice) 'emacspeak)
    (ad-activate (car advice)))
  ;; Disable the line-reading advice
  (ad-disable-advice 'emacspeak-speak-line 'around 'emacspeak-markdown-reading)
  (ad-activate 'emacspeak-speak-line)
  (when (boundp 'markdown-mode-map)
    (define-key markdown-mode-map (kbd "C-c C-s h") nil)
    (define-key markdown-mode-map (kbd "C-c C-s r") nil))
  (message "Disabled Emacspeak Markdown support"))

;;;  Provide the module:

(provide 'emacspeak-markdown)

;;; emacspeak-markdown.el ends here

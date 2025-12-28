;;; voiceover-support.el --- VoiceOver-style keybindings for Emacspeak -*- lexical-binding: t; -*-
;; Description: VoiceOver-familiar keybindings for macOS users
;; Keywords: Emacspeak, Audio Desktop, VoiceOver, macOS

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
;; the Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This module provides VoiceOver-style keybindings for macOS users
;; transitioning to Emacspeak. These bindings are only available on macOS
;; and help users leverage their existing VoiceOver muscle memory.
;;
;; Key bindings provided (on macOS):
;;   - Control (by itself): Stop speech immediately (like VoiceOver)
;;   - C-M-a: Read from point to end of buffer (like VO+A)
;;   - C-M-w: Speak current word (like VO+W)
;;   - C-M-s: Speak current sentence (like VO+S)
;;   - C-M-l: Speak current line (like VO+L)
;;
;; Usage:
;;   (require 'voiceover-support)
;;   (voiceover-support-mode 1)  ; Enable the bindings

;;; Code:

;;   Required modules:

(require 'emacspeak-preamble)

;;;  Detection of macOS:

(defun voiceover-support--macos-p ()
  "Return non-nil if running on macOS."
  (eq system-type 'darwin))

;;;  VoiceOver-style commands:

(defun voiceover-support-stop-speech ()
  "Stop speech immediately, mimicking VoiceOver Control key behavior."
  (interactive)
  (dtk-stop 'all)
  (emacspeak-icon 'close-object))

(defun voiceover-support-read-from-here ()
  "Read from point to end of buffer, like VO+A."
  (interactive)
  (emacspeak-speak-rest-of-buffer)
  (emacspeak-icon 'select-object))

(defun voiceover-support-speak-word ()
  "Speak the current word, like VO+W."
  (interactive)
  (emacspeak-speak-word)
  (emacspeak-icon 'item))

(defun voiceover-support-speak-sentence ()
  "Speak the current sentence, like VO+S."
  (interactive)
  (let ((start (save-excursion (backward-sentence) (point)))
        (end (save-excursion (forward-sentence) (point))))
    (emacspeak-speak-region start end))
  (emacspeak-icon 'paragraph))

(defun voiceover-support-speak-line ()
  "Speak the current line, like VO+L."
  (interactive)
  (emacspeak-speak-line)
  (emacspeak-icon 'item))

;;;  Keymap definition:

(defvar voiceover-support-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Control alone stops speech (VoiceOver muscle memory)
    (define-key map (kbd "<control>") #'voiceover-support-stop-speech)
    ;; VO-style commands using Control+Option (Command Meta in Emacs)
    (define-key map (kbd "C-M-a") #'voiceover-support-read-from-here)
    (define-key map (kbd "C-M-w") #'voiceover-support-speak-word)
    (define-key map (kbd "C-M-s") #'voiceover-support-speak-sentence)
    (define-key map (kbd "C-M-l") #'voiceover-support-speak-line)
    map)
  "Keymap for VoiceOver-style bindings in Emacspeak.")

;;;  Minor mode definition:

(define-minor-mode voiceover-support-mode
  "Minor mode providing VoiceOver-style keybindings for Emacspeak users.
Only activates on macOS.

Key bindings:
\\{voiceover-support-mode-map}"
  :global t
  :lighter " VO"
  :keymap voiceover-support-mode-map
  (cond
   ((and voiceover-support-mode (not (voiceover-support--macos-p)))
    ;; Trying to enable but not on macOS
    (setq voiceover-support-mode nil)
    (message "VoiceOver support is only available on macOS"))
   (voiceover-support-mode
    ;; Enabling on macOS
    (emacspeak-icon 'on)
    (message "VoiceOver support mode enabled"))
   (t
    ;; Disabling
    (emacspeak-icon 'off)
    (message "VoiceOver support mode disabled"))))

;;;  Auto-enable helper:

(defun voiceover-support-maybe-enable ()
  "Enable VoiceOver support mode if running on macOS."
  (interactive)
  (when (voiceover-support--macos-p)
    (voiceover-support-mode 1)))

;;;  Enable/Disable support:

(defun voiceover-support-enable ()
  "Enable VoiceOver support mode."
  (interactive)
  (voiceover-support-mode 1))

(defun voiceover-support-disable ()
  "Disable VoiceOver support mode."
  (interactive)
  (voiceover-support-mode -1))

;;;  Provide the module:

(provide 'voiceover-support)

;;; voiceover-support.el ends here

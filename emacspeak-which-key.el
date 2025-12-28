;;; emacspeak-which-key.el --- Speech-enable Which-Key -*- lexical-binding: t; -*-
;; Description: Speech-enable Which-Key, a package that displays available keybindings
;; Keywords: Emacspeak, Audio Desktop, Which-Key, keybindings

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
;; Which-Key displays available keybindings in a popup after pressing
;; a prefix key. This module speech-enables Which-Key to provide auditory
;; feedback for available bindings, supporting navigation through pages
;; and speaking the current binding options.

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(require 'emacspeak-preamble)
(require 'which-key nil 'noerror)

;;;  Silence byte-compiler about special variables:

(defvar which-key--buffer)
(defvar which-key--pages-obj)
(defvar which-key-init-buffer-hook)

;;;  Forward declarations:

(declare-function which-key--pages-height "which-key")
(declare-function which-key--pages-widths "which-key")
(declare-function which-key--popup-showing-p "which-key")
(declare-function which-key--current-key-string "which-key")

;;;  Map faces to voices:

(voice-setup-add-map
 '((which-key-key-face voice-bolden)
   (which-key-separator-face voice-smoothen)
   (which-key-note-face voice-annotate)
   (which-key-command-description-face voice-monotone)
   (which-key-local-map-description-face voice-brighten)
   (which-key-highlighted-command-face voice-animate)
   (which-key-group-description-face voice-bolden-medium)
   (which-key-special-key-face voice-lighten)
   (which-key-docstring-face voice-annotate)))

;;;  Bookkeeping variables:

(defvar emacspeak-which-key--page-cache nil
  "Cache of last which-key page content for speaking.")

(defvar emacspeak-which-key--auto-speak t
  "When non-nil, automatically speak which-key popups.")

;;;  Helper functions:

(defun emacspeak-which-key--speak-page ()
  "Speak current which-key page content."
  (when (and (bound-and-true-p which-key--buffer)
             (buffer-live-p which-key--buffer))
    (with-current-buffer which-key--buffer
      (let ((content (buffer-string)))
        (unless (string-empty-p content)
          (setq emacspeak-which-key--page-cache content)
          (dtk-speak content))))))

(defun emacspeak-which-key--page-info ()
  "Return current page info as string."
  (when (bound-and-true-p which-key--pages-obj)
    (let* ((pages which-key--pages-obj)
           (current (1+ (plist-get pages :page-nums)))
           (total (plist-get pages :num-pages)))
      (when (and current total (> total 1))
        (format "page %d of %d" current total)))))

(defun emacspeak-which-key--speak-page-with-info ()
  "Speak current page with page number info."
  (let ((page-info (emacspeak-which-key--page-info)))
    (when page-info
      (dtk-speak page-info))
    (emacspeak-which-key--speak-page)))

;;;  Interactive command to speak cached content:

(defun emacspeak-which-key-speak ()
  "Speak the current or last which-key popup content."
  (interactive)
  (cond
   ((and (bound-and-true-p which-key--buffer)
         (buffer-live-p which-key--buffer)
         (which-key--popup-showing-p))
    (emacspeak-which-key--speak-page-with-info))
   (emacspeak-which-key--page-cache
    (dtk-speak emacspeak-which-key--page-cache))
   (t (message "No which-key content available"))))

;;;  Advice interactive commands:

(defadvice which-key--show-page (after emacspeak pre act comp)
  "Speak the which-key page."
  (when emacspeak-which-key--auto-speak
    (emacspeak-icon 'help)
    (emacspeak-which-key--speak-page)))

(defadvice which-key--hide-popup (after emacspeak pre act comp)
  "Announce popup hidden."
  (when (ems-interactive-p)
    (dtk-stop 'all)
    (emacspeak-icon 'close-object)))

(defadvice which-key-abort (after emacspeak pre act comp)
  "Speak abort feedback."
  (when (ems-interactive-p)
    (dtk-stop 'all)
    (emacspeak-icon 'close-object)))

(defadvice which-key-undo-key (after emacspeak pre act comp)
  "Speak undo feedback."
  (when (ems-interactive-p)
    (emacspeak-icon 'item)
    (dtk-speak "undo")))

(defadvice which-key-show-standard-help (before emacspeak pre act comp)
  "Announce showing help."
  (when (ems-interactive-p)
    (emacspeak-icon 'help)))

;;; Batch advice for paging commands:

(cl-loop
 for (f icon) in
 '((which-key-show-next-page-cycle scroll)
   (which-key-show-previous-page-cycle scroll)
   (which-key-show-next-page-no-cycle scroll)
   (which-key-show-previous-page-no-cycle scroll))
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak page info after paging."
     (when (ems-interactive-p)
       (emacspeak-icon ',icon)
       (let ((page-info (emacspeak-which-key--page-info)))
         (when page-info (dtk-speak page-info)))))))

;;; Batch advice for show commands:

(cl-loop
 for f in
 '(which-key-show-top-level
   which-key-show-major-mode
   which-key-show-full-major-mode
   which-key-show-minor-mode-keymap
   which-key-show-keymap)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Announce which-key display."
     (when (ems-interactive-p)
       (emacspeak-icon 'open-object)))))

;;;  Toggle auto-speak:

(defun emacspeak-which-key-toggle-auto-speak ()
  "Toggle automatic speaking of which-key popups."
  (interactive)
  (setq emacspeak-which-key--auto-speak (not emacspeak-which-key--auto-speak))
  (emacspeak-icon (if emacspeak-which-key--auto-speak 'on 'off))
  (message "Which-key auto-speak %s"
           (if emacspeak-which-key--auto-speak "enabled" "disabled")))

;;;  Setup:

(defun emacspeak-which-key-setup ()
  "Setup Emacspeak support for Which-Key."
  (when (boundp 'which-key-init-buffer-hook)
    (add-hook 'which-key-init-buffer-hook
              #'(lambda () (emacspeak-icon 'open-object)))))

(eval-after-load "which-key" #'emacspeak-which-key-setup)

;;;  Enable/Disable support:

(defvar emacspeak-which-key--advice-list
  '((which-key--show-page after)
    (which-key--hide-popup after)
    (which-key-abort after)
    (which-key-undo-key after)
    (which-key-show-standard-help before)
    (which-key-show-next-page-cycle after)
    (which-key-show-previous-page-cycle after)
    (which-key-show-next-page-no-cycle after)
    (which-key-show-previous-page-no-cycle after)
    (which-key-show-top-level after)
    (which-key-show-major-mode after)
    (which-key-show-full-major-mode after)
    (which-key-show-minor-mode-keymap after)
    (which-key-show-keymap after))
  "List of advised functions for Emacspeak Which-Key support.")

(defun emacspeak-which-key-enable ()
  "Enable Emacspeak support for Which-Key."
  (interactive)
  (dolist (advice emacspeak-which-key--advice-list)
    (ad-enable-advice (car advice) (cadr advice) 'emacspeak)
    (ad-activate (car advice)))
  (emacspeak-which-key-setup)
  (message "Enabled Emacspeak Which-Key support"))

(defun emacspeak-which-key-disable ()
  "Disable Emacspeak support for Which-Key."
  (interactive)
  (dolist (advice emacspeak-which-key--advice-list)
    (ad-disable-advice (car advice) (cadr advice) 'emacspeak)
    (ad-activate (car advice)))
  (when (boundp 'which-key-init-buffer-hook)
    (remove-hook 'which-key-init-buffer-hook
                 #'(lambda () (emacspeak-icon 'open-object))))
  (message "Disabled Emacspeak Which-Key support"))

;;;  Provide the module:

(provide 'emacspeak-which-key)

;;; emacspeak-which-key.el ends here

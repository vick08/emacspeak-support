;;; emacspeak-support.el --- Speech-enable additional packages -*- lexical-binding: t; -*-
;; Description: Entry point for loading Emacspeak extensions for third-party packages
;; Keywords: Emacspeak, Audio Desktop, accessibility

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
;; This package provides Emacspeak extensions for third-party packages
;; not included in the main Emacspeak distribution.
;;
;; Currently supported:
;; - Corfu (in-buffer completion UI)
;; - Which-Key (keybinding popup display)
;;
;; Extensions are loaded automatically when their target packages are available.

;;; Code:

(require 'emacspeak-corfu nil t)
(require 'emacspeak-which-key nil t)

(provide 'emacspeak-support)

;;; emacspeak-support.el ends here

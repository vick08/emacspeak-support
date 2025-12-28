;;; example-config.el --- Example Emacspeak support configuration

;; Add to your ~/.emacs or init.el:

;; 1. Add the emacspeak-support directory to your load path
(add-to-list 'load-path "~/projects/robertmeta/emacspeak-support")

;; 2. Load the main support module
(require 'emacspeak-support)

;; 3. Enable all extensions (optional - can enable individually)
(emacspeak-support-enable-all)

;; Or enable selectively when packages load:
;; (with-eval-after-load 'corfu
;;   (emacspeak-support-enable-corfu))
;;
;; (with-eval-after-load 'which-key
;;   (emacspeak-support-enable-which-key))
;;
;; (with-eval-after-load 'markdown-mode
;;   (emacspeak-support-enable-markdown))

;; Toggle commands available:
;; M-x emacspeak-support-toggle-corfu
;; M-x emacspeak-support-toggle-which-key
;; M-x emacspeak-support-toggle-markdown
;; M-x emacspeak-support-status

;;; example-config.el ends here

;;; consult-ripgrep-same-ext.el --- consult-ripgrep restricted to current extension -*- lexical-binding: t; -*-

;; Author: Christophe Gouel <christophe.gouel@inrae.fr>
;; Version: 0.1
;; Keywords: grep, ripgrep, consult
;; Package-Requires: ((emacs "27.1") (consult "1.7"))

;;; Commentary:

;; This package provides a command `consult-ripgrep-same-ext` that runs
;; `consult-ripgrep` restricted to files with the same extension as the
;; current buffer. The search is case-robust with respect to the extension,
;; e.g. `.R` and `.r` are both searched when the current buffer is `foo.R`
;; or `foo.r`.

;;; Code:

(defvar consult-ripgrep-same-ext--ext-history nil
  "Minibuffer history for `consult-ripgrep-same-ext' extension fallback prompt.")

;;;###autoload
(defun consult-ripgrep-same-ext (&optional dir initial)
  "Run `consult-ripgrep' restricted to files with the same extension as the current buffer.

The search is case-robust with respect to the extension, e.g. `.R` and `.r`
are both searched when the current buffer is `foo.R` or `foo.r`."
  (interactive)
  (require 'consult)

  (let* ((ext
          (or
           ;; Visiting a file
           (and buffer-file-name
                (file-name-extension buffer-file-name))
           ;; Dired: file at point
           (and (derived-mode-p 'dired-mode)
                (fboundp 'dired-get-file-for-visit)
                (ignore-errors
                  (file-name-extension (dired-get-file-for-visit))))
           ;; Buffer name fallback
           (and (string-match-p "\\.[^./\\\\]+\\'" (buffer-name))
                (file-name-extension (buffer-name)))))

         (ext
          (or ext
              (when (called-interactively-p 'interactive)
                (let ((input
                       (read-string "Extension (without dot, e.g. R): "
                                    nil
                                    'consult-ripgrep-same-ext--ext-history)))
                  (setq input (string-remove-prefix "." input))
                  (unless (string= input "") input))))))

    (unless (and ext (stringp ext) (not (string= ext "")))
      (user-error "Cannot determine a file extension for this buffer"))

    (let* ((glob (concat "*." ext))
	   (base-args
            (if (stringp consult-ripgrep-args)
		(split-string-and-unquote consult-ripgrep-args)
              consult-ripgrep-args))
	   (consult-ripgrep-args
            (append base-args (list "--iglob" glob))))
      (if (called-interactively-p 'interactive)
	  (call-interactively #'consult-ripgrep)
	(consult-ripgrep dir initial)))))

;;; consult-ripgrep-same-ext.el ends here

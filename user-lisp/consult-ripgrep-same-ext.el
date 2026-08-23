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

(require 'consult)

(defgroup consult-ripgrep-same-ext nil
  "Consult ripgrep restricted to current/related file extensions."
  :group 'consult)

(defcustom consult-ripgrep-same-ext-extension-groups nil
  "List of extension groups.

Each element is a list of extensions (without dot) considered equivalent.
If the current extension appears in a group, `consult-ripgrep-same-ext'
searches all extensions in that group; otherwise it searches only the
current extension."
  :type '(repeat (repeat string))
  :group 'consult-ripgrep-same-ext)

(defvar consult-ripgrep-same-ext--ext-history nil
  "Minibuffer history for `consult-ripgrep-same-ext' extension fallback prompt.")

(defun consult-ripgrep-same-ext--current-extension ()
  "Return current buffer's extension (without dot), or nil if none."
  (or (and buffer-file-name (file-name-extension buffer-file-name))
      (and (derived-mode-p 'dired-mode)
           (fboundp 'dired-get-file-for-visit)
           (ignore-errors (file-name-extension (dired-get-file-for-visit))))
      (and (string-match-p "\\.[^./\\\\]+\\'" (buffer-name))
           (file-name-extension (buffer-name)))))

(defun consult-ripgrep-same-ext--extensions-for (ext)
  "Return list of extensions to search for EXT, using configured groups."
  (let* ((ext (downcase ext))
         (grp (seq-find (lambda (g)
                          (member ext (mapcar #'downcase g)))
                        consult-ripgrep-same-ext-extension-groups)))
    (if grp
        (delete-dups (mapcar #'downcase grp))
      (list ext))))

;;;###autoload
(defun consult-ripgrep-same-ext (&optional dir initial)
  "Run `consult-ripgrep' restricted to files with the sameor related extension.

Related extensions are configured via
`consult-ripgrep-same-ext-extension-groups'.

When called interactively:
- In file or Dired buffers: uses the implicit directory like `consult-ripgrep'.
- In buffers without an associated file, prompt for an extension.

When called non-interactively:
- Calls (consult-ripgrep DIR INITIAL) after temporarily extending
  `consult-ripgrep-args'.

DIR is the directory to search.
INITIAL is the initial input string for the minibuffer."
  (interactive)
  (let* ((ext (or (consult-ripgrep-same-ext--current-extension)
                  (when (called-interactively-p 'interactive)
                    (let ((input (read-string "Extension (without dot, e.g. R): "
                                              nil
                                              'consult-ripgrep-same-ext--ext-history)))
                      (setq input (string-remove-prefix "." input))
                      (unless (string= input "") input))))))
    (unless (and ext (stringp ext) (not (string= ext "")))
      (user-error "Cannot determine a file extension for this buffer"))

    (let* ((exts (consult-ripgrep-same-ext--extensions-for ext))
           (iglob-args (apply #'append
                              (mapcar (lambda (e) (list "--iglob" (concat "*." e)))
                                      exts)))
           (base-args (if (stringp consult-ripgrep-args)
                          (split-string-and-unquote consult-ripgrep-args)
                        consult-ripgrep-args))
           (consult-ripgrep-args (append base-args iglob-args))
           (from-file-or-dired (or buffer-file-name (derived-mode-p 'dired-mode))))
      (cond
       ;; Interactive from a context-free buffer: prompt for directory explicitly.
       ((and (called-interactively-p 'interactive) (not from-file-or-dired))
        (let ((dir (read-directory-name "Ripgrep in directory: " default-directory nil t)))
          (consult-ripgrep dir nil)))
       ;; Normal interactive case: preserve consult-ripgrep UX.
       ((called-interactively-p 'interactive)
        (call-interactively #'consult-ripgrep))
       ;; Non-interactive case: respect args.
       (t
        (consult-ripgrep dir initial))))))

(provide 'consult-ripgrep-same-ext)

;;; consult-ripgrep-same-ext.el ends here

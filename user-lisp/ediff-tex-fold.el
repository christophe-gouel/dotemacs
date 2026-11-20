;;; ediff-tex-fold.el --- Suspend AUCTeX folding during Ediff -*- lexical-binding: t; -*-

;;; Commentary:

;; Keep TeX buffers unfolded throughout an Ediff comparison.  Buffers shared
;; by several sessions are refolded only after the last session releases them.
;; Loading this library installs the hooks without loading Ediff or AUCTeX.

;;; Code:

(declare-function TeX-fold-buffer "tex-fold")
(declare-function TeX-fold-mode "tex-fold")
(defvar TeX-fold-mode)
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
(defvar ediff-ancestor-buffer)

(defvar-local ediff-tex-fold--buffers nil
  "TeX buffers whose folding the current Ediff session suspends.")

(defvar-local ediff-tex-fold--sessions nil
  "Ediff control buffers temporarily suspending this buffer's TeX folding.")

(defun ediff-tex-fold--suspend ()
  "Suspend TeX folding in the current Ediff session's buffers."
  (let ((control-buffer (current-buffer)))
    (dolist (buffer (list ediff-buffer-A ediff-buffer-B
                          ediff-buffer-C ediff-ancestor-buffer))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'tex-mode)
                     (or (bound-and-true-p TeX-fold-mode)
                         ediff-tex-fold--sessions))
            ;; A buffer already suspended by another session still needs
            ;; to record this session as an owner.
            (unless (memq control-buffer ediff-tex-fold--sessions)
              (push control-buffer ediff-tex-fold--sessions))
            (with-current-buffer control-buffer
              (unless (memq buffer ediff-tex-fold--buffers)
                (push buffer ediff-tex-fold--buffers)))
            (when (bound-and-true-p TeX-fold-mode)
              (TeX-fold-mode -1)))))))
  (add-hook 'kill-buffer-hook #'ediff-tex-fold--restore nil t))

(defun ediff-tex-fold--restore ()
  "Release this Ediff session's buffers, restoring folding after the last owner."
  (let ((control-buffer (current-buffer)))
    (dolist (buffer ediff-tex-fold--buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (memq control-buffer ediff-tex-fold--sessions)
            (setq ediff-tex-fold--sessions
                  (delq control-buffer ediff-tex-fold--sessions))
            (when (and (null ediff-tex-fold--sessions)
                       (derived-mode-p 'tex-mode))
              (TeX-fold-mode 1)
              (TeX-fold-buffer)))))))
  (setq ediff-tex-fold--buffers nil))

;; Remove handlers from earlier versions when reloading this library.
(remove-hook 'ediff-prepare-buffer-hook 'my-ediff-unfold-tex-buffer)
(remove-hook 'ediff-prepare-buffer-hook 'ediff-unfold-tex-buffer)
(remove-hook 'ediff-startup-hook 'my-ediff-unfold-tex-buffers)
(remove-hook 'ediff-startup-hook 'ediff-unfold-tex-buffers)
(remove-hook 'ediff-cleanup-hook 'my-ediff-refold-tex-buffers)
(remove-hook 'ediff-cleanup-hook 'ediff-refold-tex-buffers)
(add-hook 'ediff-startup-hook #'ediff-tex-fold--suspend)
(add-hook 'ediff-cleanup-hook #'ediff-tex-fold--restore)

(provide 'ediff-tex-fold)

;;; ediff-tex-fold.el ends here

;; (byte-recompile-directory "c:/Program Files/GNU Emacs 23.3/site-lisp" 1)
;; (byte-recompile-directory "~/.emacs.d/site-lisp" 1)

(defconst mswindows (equal window-system 'w32))

;;; My location for external packages.
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; ==========
;;;  Org mode
;;; ==========
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-hide-leading-stars t)
(setq org-export-with-LaTeX-fragments t)       ; Export LaTeX fragment to HTML

(setq backup-directory-alist
     	  '(("." . "~/.emacs.d/backup")))

;;; Activate lower- and upper-case commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(if (display-graphic-p)
    (server-start)
  (xterm-mouse-mode))

;;; ===============
;;;  Look and feel
;;; ===============
(blink-cursor-mode nil)                  ; curseur ne clignote pas
(setq-default cursor-type 'bar)          ; curseur étroit
(set-face-background 'cursor "#CC0000")  ; curseur rouge foncé

(global-font-lock-mode t)                ; colorisation du texte
(transient-mark-mode t)                  ; mode de sélection "normal"
(delete-selection-mode t)                ; entrée efface texte sélectionné
(setq-default mouse-yank-at-point t)     ; coller avec la souris
(show-paren-mode t)                      ; coupler les parenthèses
(setq-default case-fold-search t)        ; recherche sans égard à la casse

(setq display-time-24hr-format t)        ; Affichage de l'heure format 24h
(display-time)                           ; Affichage de l'errur dans le bandeau

(fset 'yes-or-no-p 'y-or-n-p)            ; Replace yes or no with y or n

(add-hook 'write-file-hooks 'time-stamp) ; Time-stamp

(setq default-major-mode 'text-mode)     ; mode par défaut
(setq text-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; wrap long lines in text mode

(setq column-number-mode t)              ; affichage du numéro de la colonne

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b")) ; Affiche le chemin complet du fichier dans la barre de titre d'Emacs

;;; set up unicode
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(if mswindows    ;; MS Windows clipboard is UTF-16LE
    (set-clipboard-coding-system 'utf-16le-dos))

;;; Police Consolas
(if mswindows
  (progn
    (set-face-font 'default "-outline-Consolas-normal-r-normal-normal-*-*-96-96-c-*-iso8859-1")
    (set-face-font 'bold "-outline-Consolas-bold-r-normal-normal-*-*-96-96-c-*-iso8859-1")
    (set-face-font 'italic "-outline-Consolas-normal-i-normal-normal-*-*-96-96-c-*-iso8859-1")
    (set-face-font 'bold-italic "-outline-Consolas-bold-i-normal-normal-*-*-96-96-c-*-iso8859-1"))
  (set-default-font "DejaVu Sans Mono 10"))

(setq inhibit-startup-screen t)

;;; ========
;;;  Unfill
;;; ========
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(defun unfill-region (start end)
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

;;; ============================================
;;;  Pager - http://user.it.uu.se/~mic/pager.el
;;; ============================================
(require 'pager)
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)

;;; ================
;;;  Recent Files
;;; ================
(require 'recentf)
(recentf-mode 1)

;;; ===================================================
;;;  Template - http://emacs-template.sourceforge.net/
;;; ===================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/template/lisp")
(require 'template)
(template-initialize)
;; Rajouter le chemin vers les templates

;;; ========
;;;  Ispell
;;; ========
(require 'ispell)
;;; Use Aspell for spell checking.
(if mswindows
    (setq-default ispell-program-name "C:/Program Files/GNU Emacs 23.3/aspell/bin/aspell.exe")
  (setq-default ispell-program-name "aspell"))
(setq ispell-dictionary "american")
(setq ispell-list-command "list")
(setq flyspell-issue-welcome-flag nil)

;;; ========================================================
;;;  Gams - http://shirotakeda.org/home/gams/gams-mode.html
;;; ========================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/gams-3.6.3")
(require 'gams)
(if mswindows
  (progn
    (setq gams:process-command-name "C:/Progra~1/GAMS23.7/gams.exe")
    (setq gams-system-directory "c:/Progra~1/GAMS23.7/")
    (setq gams-docs-directory "C:/Program Files/GAMS23.7/docs")
    (setq gams-docs-view-program "C:/Program Files/Tracker Software/PDF Viewer/PDFXCview.exe")
    (setq load-path
	  (cons "c:/Program Files/Gams23.7/" ;; Set the installed directory!
		load-path))))
(setq gams:process-command-option "ll=0 lo=3 pw=153 ps=9999")
(setq gams-statement-upcase t)
(setq gams-fill-column 80)
(setq gams-recenter-font-lock t)
(setq gams-statement-name "Parameter")
(setq gams-dollar-control-name "exit")
(setq gams-default-pop-window-height 20)
(setq gams-inlinecom-symbol-start-default "{")
(setq gams-inlinecom-symbol-end-default "}")
(defun find-in-gms-files (string)
  "Find a regular expression in gms files"
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nH -i -r -e " string " --include=*.gms *" )))
(define-key gams-mode-map "\C-cf" 'find-in-gms-files)

;; Indent
(setq gams-indent-on t)
(setq gams-indent-number 2)
(setq gams-indent-number-loop 2)
(setq gams-indent-number-mpsge 2)
(setq gams-indent-number-equation 2)
(setq indent-tabs-mode nil) ; Not use tabs for indent

;;; =========================================================================
;;;  Lancer une recherche d'article sous IDEAS ou google-search depuis Emacs
;;; =========================================================================
(defun ideas (command)
;"Uses `browse-url' to submit keywords to IDEAS and open result in an external browser defined in `browse-url-browser-function'."
  (interactive "sCommand:")
  (browse-url
   (concat "http://ideas.repec.org/cgi-bin/htsearch?words="command)))

(defun google-scholar (command)
  (interactive "sCommand:")
  (browse-url
   (concat "http://scholar.google.com/scholar?q="command)))

;;; ==================================================
;;;  Yaml mode - https://github.com/yoshiki/yaml-mode
;;; ==================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/yaml")
(require 'yaml-mode)
(setq auto-mode-alist (cons '("\\(\\.yml$\\|\\.yaml$\\)" . yaml-mode) auto-mode-alist))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;; ==================================================
;;;  Color Theme - http://www.nongnu.org/color-theme/
;;; ==================================================
(if (display-graphic-p)
  (progn
    (if mswindows
      (add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme-6.6.0"))
    (require 'color-theme)
    (color-theme-initialize)
    (color-theme-blue-mood)))

;;; ===============================================================
;;;  Toolbar+ - http://www.emacswiki.org/cgi-bin/wiki/tool-bar+.el
;;; ===============================================================
(require 'tool-bar+)			 ; Activation of the pop-up tool bar
(tool-bar-pop-up-mode 1)

;;; ============
;;;  LaTeX-mode
;;; ============
(setq reftex-bibpath-environment-variables (quote ("BIBINPUTS")))
(setq-default TeX-auto-parse-length 200)
(setq-default TeX-master nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq LaTeX-default-options "12pt")
(setq reftex-cite-format (quote natbib))
(setq reftex-sort-bibtex-matches (quote author))
;; (setq reftex-plug-into-AUCTeX t)
(setq LaTeX-math-abbrev-prefix "²")
(setq TeX-source-specials-mode 1)

(add-hook 'TeX-mode-hook 'flyspell-mode)
(add-hook 'TeX-mode-hook 'auto-fill-mode)
(add-hook 'TeX-mode-hook 'latex-math-mode)

(add-hook 'TeX-mode-hook 'imenu-add-menubar-index)
(add-hook 'TeX-mode-hook (lambda ()
 			     (TeX-fold-mode 1)
			     (auto-fill-mode)
			     (turn-on-reftex)
			     (setq reftex-plug-into-AUCTeX t)))
(add-hook 'TeX-mode-hook 'TeX-fold-buffer t)

(add-hook 'LaTeX-mode-hook
     '(lambda nil
	(define-key LaTeX-mode-map "\C-ce" 'TeX-next-error)
	(define-key LaTeX-mode-map [S-mouse-3] 'imenu)
	(define-key LaTeX-mode-map "\C-cf" 'reftex-fancyref-fref)
	(define-key LaTeX-mode-map "\C-cF" 'reftex-fancyref-Fref)
	(setq fill-column 80)))
(add-hook 'TeX-mode-hook
          '(lambda ()
            (define-key TeX-mode-map (kbd "<f9>")
              (lambda ()
                (interactive)
                (save-buffer)
                (TeX-command-menu "Latex")))
            (define-key TeX-mode-map (kbd "<f10>")
              (lambda ()
                (interactive)
		(TeX-fold-buffer)
                (preview-at-point)))
            (define-key TeX-mode-map (kbd "<f12>")
              (lambda ()
                (interactive)
                (TeX-view)
                [return]))))

(setq TeX-electric-sub-and-superscript 1)
(setq LaTeX-math-list
      '(
	(?) "right)")
        (?( "left(")
	(?/ "frac{}{}")
	)
	)

;; Preview
(setq preview-scale-function 1.7)      ; Higher preview images in TeX buffers
(setq preview-auto-cache-preamble t)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(if mswindows
    (progn
      (require 'tex-mik)
      (eval-after-load "tex"
	'(add-to-list 'TeX-command-list
		      '("htlatex" "htlatex %s" TeX-run-command t t :help "Run htlatex") t))
      (eval-after-load "tex"
	'(add-to-list 'TeX-command-list
		      '("htlatexword" "htlatexword %s" TeX-run-command nil t :help "Run htlatex with Word options") t))
      (eval-after-load "tex"
	'(add-to-list 'TeX-command-list
		      '("PDFViewerClose" "PDFXCview-close.bat %s" TeX-run-command nil t :help "Close PDF open in PDF-XChange Viewer") t)))
  (progn
    (setq TeX-view-program-list '(("Adobe Reader" "acroread /a page=%(outpage) %o ")))
    (setq TeX-view-program-selection '((output-pdf "Adobe Reader")))))

;; Beamer
(defun tex-frame ()
  "Run pdflatex on current frame. Frame must be declared as an environment."
  (interactive)
  (let (beg)
    (save-excursion
      (search-backward "\\begin{frame}")
      (setq beg (point))
      (forward-char 1)
      (LaTeX-find-matching-end)
      (TeX-pin-region beg (point))
      (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
	(TeX-command-region)))))
(add-hook 'TeX-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'tex-frame)))

;;; ========
;;;  Dynare
;;; ========
;; (require 'dynare)
;; (autoload 'dynare-mode "dynare" "Enter dynare mode." t)
;; (setq auto-mode-alist (cons '("\\.mod\\'" . dynare-mode) auto-mode-alist))

;;; ===========================================================
;;;  Mode imaxima - http://sites.google.com/site/imaximaimath/
;;; ===========================================================
(if mswindows
  (load "~/.emacs.d/site-lisp/imaxima-imath-1.0/setup-imaxima-imath.el"))
;; (require 'setup-imaxima-imath)
(autoload 'imaxima "imaxima" "Image support for Maxima." t)
(autoload 'imath-mode "imath" "Interactive Math minor mode." t)
(autoload 'maxima "maxima" "Original Maxima mode" t)
(setq imaxima-use-maxima-mode-flag t)
(setq imaxima-fnt-size "LARGE")

;;; ============================================
;;;  LUA - https://github.com/immerrr/lua-mode/
;;; ============================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/lua")
(require 'lua-mode)
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;; ================================================================================
;;;  Iswitch buffer - http://www.emacswiki.org/emacs/download/iswitchb-highlight.el
;;; ================================================================================
(iswitchb-mode 1)
(require 'iswitchb-highlight)

;;; ===================================================
;;;  Mode CSV - http://centaur.maths.qmul.ac.uk/Emacs/
;;; ===================================================
(require 'csv-mode)
(autoload 'csv-mode "csv-mode"
   "Major mode for editing comma-separated value files." t)
(setq csv-separators '("," ";"))

;;; =====
;;;  PDF
;;; =====
(require 'doc-view)

;;; ===============
;;;  Remote access
;;; ===============
;;; Tramp
(require 'tramp)
(if mswindows
  (setq tramp-default-method "plink"))
;; (setq tramp-password-end-of-line "xx")
(setq tramp-verbose 10)
;; (setq tramp-debug-buffer t)

;;; ==================
;;;  Auto-compression
;;; ==================
(auto-compression-mode t)

;;; ================
;;;  Ibuffer
;;; ================
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;; ===============================================
;;;  Matlab - http://matlab-emacs.sourceforge.net/
;;; ===============================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/matlab-emacs")
(require 'matlab)
; Set up matlab-mode to load on .m files
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

;; Customization:
(setq matlab-indent-function t)	; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
(setq matlab-indent-level 2)
(setq matlab-comment-region-s "% ")
(defun my-matlab-mode-hook ()
  (setq fill-column 80))		; where auto-fill should wrap
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
(defun my-matlab-shell-mode-hook ()
  '())
(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)

;; Turn off Matlab desktop
(setq matlab-shell-command-switches '("-nojvm"))

(defun find-in-m-files (string)
  "Find a regular expression in m files"
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nH -i -r -e " string " --include=*.m *" )))
(define-key matlab-mode-map "\C-cf" 'find-in-m-files)

;;; =====================================
;;;  Activation du clic droit comme aide
;;; =====================================
(define-key global-map [(mouse-3)] 'mouse-me)

;;; =================
;;;  Easier printing
;;; =================
(if mswindows
  (require 'w32-winprint))
(require 'htmlize-view)
(htmlize-view-add-to-files-menu)

;;; =====
;;;  ESS
;;; =====
(require 'ess-site)
(require 'ess-eldoc)
(if (display-graphic-p)
  (progn
    (require 'ess-mouse)
    (define-key ess-mode-map [(mouse-3)] 'ess-mouse-me)
    (define-key inferior-ess-mode-map [(mouse-3)] 'ess-mouse-me)))

;; Following the "source is real" philosophy put forward by ESS, one
;; should not need the command history and should not save the
;; workspace at the end of an R session. Hence, both options are
;; disabled here.
(setq-default inferior-R-args "--no-restore-history --no-save ")

;; Set code indentation following the standard in R sources.
(setq ess-default-style 'C++)
;; (setq-default c-default-style "bsd")
;; (setq-default c-basic-offset 4)
(add-hook 'ess-mode-hook
	  '(lambda()
	     (add-hook 'write-file-functions
                           (lambda ()
                             (ess-nuke-trailing-whitespace)))
	     (setq ess-nuke-trailing-whitespace-p t)))

(autoload 'ess-rdired "ess-rdired" "View *R* objects in a dired-like buffer." t)

;;;Affichage d'une aide sur la fonction en cours d'usage
;; ess-r-args-noargsmsg is printed, if no argument information could
;; be found. You could set it to an empty string ("") for no message.
(setq ess-r-args-noargsmsg "No args found.")

;; ess-r-args-show-as determines how (where) the information is
;; displayed. Set it to "tooltip" for little tooltip windows or to
;; nil (the default) which will use the echo area at the bottom of
;; your Emacs frame.
(setq ess-r-args-show-as nil)

;; ess-r-args-show-prefix is a string that is printed in front of the
;; arguments list. The default ist "ARGS: ".
(setq ess-r-args-show-prefix "ARGS: ")

;; bind ess-r-args-show to F2
(define-key ess-mode-map [f2] 'ess-r-args-show)
(define-key inferior-ess-mode-map [f2] 'ess-r-args-show)

;; bind ess-r-args-show to F2
(define-key ess-mode-map [f1] 'ess-rdired)
(define-key inferior-ess-mode-map [f1] 'ess-rdired)

;; call ess-r-args-show automatically
(define-key ess-mode-map "(" '(lambda nil "" (interactive)
				  (skeleton-pair-insert-maybe nil)
				  (ess-r-args-show)))

;; bind ess-r-args-insert to F3
(define-key ess-mode-map [f3] 'ess-r-args-insert)

(define-key inferior-ess-mode-map [home] 'comint-bol)
;; (define-key ess-mode-map [(control ?c) (?;)] 'comment-region)
;; (define-key ess-mode-map [(control ?c) (?:)] 'uncomment-region)

(setq comint-scroll-to-bottom-on-output t)
(setq ess-help-own-frame 'one)

;;;Tout évaluer avec Shift-Enter
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-move-point-for-output t)

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
    (progn
      (delete-other-windows)
      (setq w1 (selected-window))
      (setq w1name (buffer-name))
      (setq w2 (split-window w1))
      (R)
      (set-window-buffer w2 "*R*")
      (set-window-buffer w1 w1name))))

(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
    (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))

(add-hook 'ess-mode-hook
	  '(lambda()
	     (local-set-key [(shift return)] 'my-ess-eval)))

(add-hook 'inferior-ess-mode-hook
	  '(lambda()
	     (local-set-key [C-up] 'comint-previous-input)
	     (local-set-key [C-down] 'comint-next-input)))

(if (display-graphic-p)
  (require 'ess-rutils))

; Call imenu with \C-c =
(define-key ess-mode-map "\C-c=" 'imenu)

;;; Sweave
(add-hook 'Rnw-mode-hook
          (lambda ()
            (add-to-list 'TeX-command-list
                         '("Sweave" "R CMD Sweave %s"
                           TeX-run-command nil t :help "Run Sweave") t)
            (add-to-list 'TeX-command-list
                         '("LatexSweave" "%l \"%(mode)\\input{%s}\""
                           TeX-run-TeX nil t :help "Run Latex after Sweave") t)
            (setq TeX-command-default "Sweave")))
;; Add standard Sweave file extensions to the list of files recognized
;; by AUCTeX.
(setq TeX-file-extensions
      '("Rnw" "rnw" "Snw" "snw" "tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx"))

(custom-set-variables
 '(inferior-ess-start-args "-j" t))

;;; ===================================
;;;  Définition de touches perso global
;;; ===================================
(define-key global-map [(M-f1)] 'bookmark-bmenu-list)
(define-key global-map [(f5)] 'revert-buffer)
;; (define-key global-map "\C-c;" 'comment-region)
;; (define-key global-map "\C-c:" 'uncomment-region)





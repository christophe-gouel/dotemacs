(package-initialize)
;; (byte-recompile-directory "~/.emacs.d/site-lisp" 1)

(defconst mswindows (equal window-system 'w32))

;;; My location for external packages.
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq backup-directory-alist
     	  '(("." . "~/.emacs.d/backup")))

;;; ==================
;;;  Packages manager
;;; ==================
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(setq package-enable-at-startup nil)

;;; Activate lower- and upper-case commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(if (display-graphic-p)
    (server-start)
  (xterm-mouse-mode))

;;; ==========
;;;  Org mode
;;; ==========
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-hide-leading-stars t)
(setq org-export-with-LaTeX-fragments t)       ; Export LaTeX fragment to HTML
(setq org-todo-keywords '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)")))
(setq org-tag-alist '(("OFFICE" . ?o) ("COMPUTER" . ?c) ("HOME" . ?h) ("PROJECT" . ?p) ("CALL" . ?a) ("ERRANDS" . ?e) ("TASK" . ?t)))
(setq org-hide-leading-stars t)
(setq org-directory "c:/Users/Christophe/Dropbox/Org")
(setq org-agenda-files (quote ("c:/Users/Christophe/Dropbox/Org/mygtd.org")))

;; Integration of RefTeX in org
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
        (global-auto-revert-mode t)
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;;; ===============
;;;  Look and feel
;;; ===============
(blink-cursor-mode nil)                  ; curseur ne clignote pas
(setq-default cursor-type 'bar)          ; curseur �troit
(set-face-background 'cursor "#CC0000")  ; curseur rouge fonc�
(setq jit-lock-chunk-size 50000)

(require 'font-lock)
(global-font-lock-mode t)                ; colorisation du texte
;; (transient-mark-mode t)                  ; mode de s�lection "normal"
(delete-selection-mode t)                ; entr�e efface texte s�lectionn�
(setq-default mouse-yank-at-point t)     ; coller avec la souris
(show-paren-mode t)                      ; coupler les parenth�ses
(setq-default case-fold-search t)        ; recherche sans �gard � la casse

(setq display-time-24hr-format t)        ; Affichage de l'heure format 24h
(display-time)                           ; Affichage de l'errur dans le bandeau

(fset 'yes-or-no-p 'y-or-n-p)            ; Replace yes or no with y or n

(add-hook 'write-file-hooks 'time-stamp) ; Time-stamp

(setq default-major-mode 'text-mode)     ; mode par d�faut
(setq text-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; wrap long lines in text mode

(setq column-number-mode t)              ; affichage du num�ro de la colonne

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b")) ; Affiche le chemin complet du fichier dans la barre de titre d'Emacs

;;; set up unicode
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
;; (setq locale-coding-system 'utf-8) ; Mess up dired buffer under windows
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
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

;;; Remove menu bar in terminal mode
(if (display-graphic-p)
    ()
    (menu-bar-mode -1))

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
;;;  Pager - From ELPA
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

;;; ========
;;;  Ispell
;;; ========
(require 'ispell)
;;; Use Aspell for spell checking.
(if mswindows
    (setq-default ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")
  (setq-default ispell-program-name "aspell"))
(setq ispell-dictionary "american")
(setq ispell-list-command "list")
(setq flyspell-issue-welcome-flag nil)

;;; ========================================================
;;;  Gams - http://shirotakeda.org/home/gams/gams-mode.html
;;; ========================================================
(add-to-list 'load-path "~/.emacs.d/site-lisp/gams-4.2")
(require 'gams)
(if mswindows
  (progn
    (setq gams:process-command-name "c:/Programs/GAMS/win64/24.7/gams.exe")
    (setq gams-system-directory "c:/Programs/GAMS/win64/24.7/")
    (setq gams-docs-directory "c:/Programs/GAMS/win64/24.7/docs")
    (setq gams-docs-view-program "C:/Program Files/Tracker Software/PDF Viewer/PDFXCview.exe")
    (setq load-path
	  (cons "c:/Programs/GAMS/win64/24.7/" ;; Set the installed directory!
		load-path)))
  (progn
    (setq gams-docs-directory "/opt/gams/gams24.6_linux_x64_64_sfx/docs")
    (setq gams-docs-view-program "qpdfview")))
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
;; (setq indent-tabs-mode nil) ; Not use tabs for indent

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
;; (require 'tool-bar+)			 ; Activation of the pop-up tool bar
;; (tool-bar-pop-up-mode 1)
(if window-system
    (tool-bar-mode 0))

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
(setq LaTeX-math-abbrev-prefix "�")
(setq TeX-source-specials-mode 1)
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method (quote synctex))
(setq TeX-source-correlate-start-server (quote ask))
(setq TeX-PDF-mode t)

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
(setq preview-gs-command (executable-find "gswin64c"))
(setq doc-view-ghostscript-program "C:\\Program Files\\gs\\gs9.20\\bin\\gswin64c.exe")

(load "auctex.el" nil t t)
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
		      '("PDFViewerClose" "PDFXCview-close.bat %s" TeX-run-command nil t :help "Close PDF open in PDF-XChange Viewer") t))))

(if mswindows
    (progn
      (setq TeX-view-program-list (quote (("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n") " %o")))))
      (setq TeX-view-program-selection (quote ((output-pdf "Sumatra PDF")))))
    (progn
      (setq TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"%o#src:%b:%n:0\"")))
      (setq TeX-view-program-selection '((output-pdf "qpdfview")))))



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
;; (if mswindows
;;   (load "~/.emacs.d/site-lisp/imaxima-imath-1.0/setup-imaxima-imath.el"))
;; ;; (require 'setup-imaxima-imath)
;; (autoload 'imaxima "imaxima" "Image support for Maxima." t)
;; (autoload 'imath-mode "imath" "Interactive Math minor mode." t)
;; (autoload 'maxima "maxima" "Original Maxima mode" t)
;; (setq imaxima-use-maxima-mode-flag t)
;; (setq imaxima-fnt-size "LARGE")

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

;;; ======================
;;;  Mode CSV - From ELPA
;;; ======================
;; (require 'csv-mode)
;; (autoload 'csv-mode "csv-mode"
;;    "Major mode for editing comma-separated value files." t)
;; (setq csv-separators '("," ";"))

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
(load-library "matlab-load")
(require 'matlab)

;; Matlab mode
;;; Set up matlab-mode to load on .m files
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))

;;; Customization:
(matlab-cedet-setup)
(setq matlab-indent-function t)	; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
(setq matlab-indent-level 2)
(setq matlab-comment-region-s "% ")
(defun my-matlab-mode-hook ()
  (setq fill-column 80) 	        ; where auto-fill should wrap
  (setq matlab-show-mlint-warnings t)   ; Activate mlint
  (mlint-minor-mode))                   ; Activate mlint minor mode
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

(defun find-in-m-files (string)
  "Find a regular expression in m files"
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nH -i -r -e " string " --include=*.m *" )))
(define-key matlab-mode-map "\C-cf" 'find-in-m-files)

;; mlint
(if mswindows
    (setq mlint-programs (quote ("C:/Program Files/MATLAB/R2016b/bin/win64/mlint.exe")))
  (setq mlint-programs (quote ("/usr/local/MATLAB/R2016b/bin/glnxa64/mlint"))))

;; Matlab shell
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(defun my-matlab-shell-mode-hook ()
  '())
(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))


;;; ========
;;;  Octave
;;; ========
;; Octave major mode
(add-hook 'octave-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (auto-fill-mode 1)
	    (if (eq window-system 'x)
		(font-lock-mode 1))))
;; Octave shell
(add-hook 'inferior-octave-mode-hook
	  (lambda ()
	    (turn-on-font-lock)
	    (define-key inferior-octave-mode-map [up]
	      'comint-previous-input)
	    (define-key inferior-octave-mode-map [down]
	      'comint-next-input)))

;;; =====================================
;;;  Activation du clic droit comme aide
;;; =====================================
(define-key global-map [(mouse-3)] 'mouse-me)

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
;(setq-default c-basic-offset 2)
(setq ess-indent-level 2)
(setq ess-arg-function-offset 2)
(setq ess-else-offset 2)
(add-hook 'ess-mode-hook
	  '(lambda()
	     (add-hook 'write-file-functions
		       (lambda ()
			 (ess-nuke-trailing-whitespace)))
	     (setq ess-nuke-trailing-whitespace-p t)))
(setq ess-first-continued-statement-offset 2
      ess-continued-statement-offset 0)

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

;;;Tout �valuer avec Shift-Enter
(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-move-point-for-output t)

(setq inferior-ess-start-args "-j")

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

(defun find-in-R-files (string)
  "Find a regular expression in R files"
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nH -i -r -e " string " --include=*.{R,r}* *" )))
(define-key ess-mode-map "\C-cf" 'find-in-R-files)

;;; ===================================
;;;  D�finition de touches perso global
;;; ===================================
(define-key global-map [(M-f1)] 'bookmark-bmenu-list)
(define-key global-map [(f5)] 'revert-buffer)
;; (define-key global-map "\C-c;" 'comment-region)
;; (define-key global-map "\C-c:" 'uncomment-region)

;;; ===============
;;;  auto-complete
;;; ===============
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140824.1658/dict")
(ac-config-default)

;;; ===============
;;;  Markdown mode
;;; ===============
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;; ==========
;;;  Polymode
;;; ==========
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;;; ========
;;;  ccrypt
;;; ========
(setq load-path (cons "path" load-path))
(require 'ps-ccrypt "ps-ccrypt.el")

;;; Sent deleted files to trash
(setq delete-by-moving-to-trash t)

(require 'vlf)

(require 'ein)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t))))
 '(package-selected-packages
   (quote
    (latex-preview-pane magit pandoc pandoc-mode yaml-mode vlf polymode pager ein auto-complete))))

(setenv "CYGWIN" "nodosfilewarning")
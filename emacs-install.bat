md site-lisp
cd site-lisp

wget http://sourceforge.net/projects/emacs-template/files/template/3.1c/template-3.1c.tar.gz/download
7z x template-3.1c.tar.gz
7z x template-3.1c.tar
rm template-3.1c.*

wget http://www.emacswiki.org/emacs/download/iswitchb-highlight.el

wget http://matlab-emacs.cvs.sourceforge.net/viewvc/*checkout*/matlab-emacs/matlab-emacs/dl_emacs_support.m
REM After downloading the archive from http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/
REM 7z x matlab-emacs-matlab-emacs.tar.gz
REM 7z x matlab-emacs-matlab-emacs.tar
REM rm matlab-emacs-matlab-emacs.*

git clone https://github.com/immerrr/lua-mode.git lua


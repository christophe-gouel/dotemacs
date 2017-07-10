md site-lisp
cd site-lisp

wget https://github.com/ShiroTakeda/gams-mode/archive/6.2.zip --no-check-certificate -Ogams-mode-6.2.zip
unzip gams-mode-6.2.zip
rm gams-mode-6.2.zip

wget http://sourceforge.net/projects/emacs-template/files/template/3.1c/template-3.1c.tar.gz/download
7z x template-3.1c.tar.gz
7z x template-3.1c.tar
rm template-3.1c.*

wget http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip
unzip color-theme-6.6.0.zip
rm color-theme-6.6.0.zip

wget http://www.emacswiki.org/emacs/download/tool-bar%2b.el

wget http://www.emacswiki.org/emacs/download/iswitchb-highlight.el

wget http://matlab-emacs.cvs.sourceforge.net/viewvc/*checkout*/matlab-emacs/matlab-emacs/dl_emacs_support.m
REM After downloading the archive from http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/
REM 7z x matlab-emacs-matlab-emacs.tar.gz
REM 7z x matlab-emacs-matlab-emacs.tar
REM rm matlab-emacs-matlab-emacs.*

git clone https://github.com/immerrr/lua-mode.git lua

git clone https://github.com/yoshiki/yaml-mode.git yaml

REM git clone http://www.dynare.org/git/dynare.git dynare
REM copy dynare/dynare.el .
REM rm -r dynare

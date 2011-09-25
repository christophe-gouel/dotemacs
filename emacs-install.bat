cd site-lisp

wget http://user.it.uu.se/~mic/pager.el

wget http://shirotakeda.org/assets/files/gams/gams-3.6.3.zip
unzip gams-3.6.3.zip
rm gams-3.6.3.zip

wget http://sourceforge.net/projects/emacs-template/files/template/3.1c/template-3.1c.tar.gz/download
7z x template-3.1c.tar.gz
7z x template-3.1c.tar
rm template-3.1c.*

wget http://download.savannah.gnu.org/releases/color-theme/color-theme-6.6.0.zip
unzip color-theme-6.6.0.zip
rm color-theme-6.6.0.zip

wget http://www.emacswiki.org/emacs/download/tool-bar%2b.el

wget http://www.emacswiki.org/emacs/download/iswitchb-highlight.el

wget http://centaur.maths.qmul.ac.uk/Emacs/files/csv-mode.el

REM After downloading the archive from http://matlab-emacs.cvs.sourceforge.net/viewvc/matlab-emacs/matlab-emacs/
7z x matlab-emacs-matlab-emacs.tar.gz
7z x matlab-emacs-matlab-emacs.tar
rm matlab-emacs-matlab-emacs.*

git clone https://github.com/immerrr/lua-mode.git lua

git clone https://github.com/yoshiki/yaml-mode.git yaml

REM git clone http://www.dynare.org/git/dynare.git dynare
REM copy dynare/dynare.el .
REM rm -r dynare

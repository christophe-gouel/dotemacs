# Need to create site-lisp before:
# mkdir site-lisp
cd site-lisp

sudo apt-get install emacs-goodies-el texlive texlive-math-extra maxima maxima-emacs ess r-base auctex

wget https://github.com/ShiroTakeda/gams-mode/archive/6.2.zip --no-check-certificate -Ogams-mode-6.2.zip
unzip gams-mode-6.2.zip
rm gams-mode-6.2.zip

wget http://sourceforge.net/projects/emacs-template/files/template/3.1c/template-3.1c.tar.gz
tar xvfz template-3.1c.tar.gz
rm template-3.1c.tar.gz

wget http://www.emacswiki.org/emacs/download/tool-bar%2b.el

wget http://www.emacswiki.org/emacs/download/iswitchb-highlight.el

git clone https://github.com/immerrr/lua-mode.git lua

git clone https://github.com/yoshiki/yaml-mode.git yaml

# git clone http://www.dynare.org/git/dynare.git dynare
# copy dynare/dynare.el .
# rm -r dynare

# Need to create site-lisp before:
# mkdir site-lisp
cd site-lisp

sudo apt-get install emacs-goodies-el texlive texlive-math-extra maxima maxima-emacs ess r-base auctex

wget http://sourceforge.net/projects/emacs-template/files/template/3.1c/template-3.1c.tar.gz
tar xvfz template-3.1c.tar.gz
rm template-3.1c.tar.gz

git clone https://github.com/immerrr/lua-mode.git lua


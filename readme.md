# readme

to use any dotfiles in here..

create a symlink and just point to this cloned directory :)

this will simplify any version control

## git dotfiles setup

move to home directory
```
cd 
```
clone repository
```
git clone https://github.com/owalk/dot_files.git
```
if it exists, remove your current emacs dotfiles
```
rm -r .emacs.d
```
make a symlink and use that as your .emacs.d
```
ln -s dot_files/.emacs.d/ ./emacs.d
```

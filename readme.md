# Readme

To use any dotfiles in here..

Create a symlink and just point to this cloned directory :)

This will simplify any version control

## Emacs dotfiles setup

Move to home directory
```
cd 
```
Clone repository
```
git clone https://github.com/owalk/dot_files.git
```
If it exists, remove your current emacs dotfiles
```
rm -r .emacs.d
```
Make a symlink and use that as your .emacs.d
```
ln -s dot_files/.emacs.d/ ./emacs.d
```

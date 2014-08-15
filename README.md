# Home Directory Configuration Files

Install the files as:

```sh
git clone git@github.com:T-J-Teru/dotfiles.git
cd dotfiles
./bin/install.pl --install-dir=/path/to/home/directory/
```
The `dotfiles` repository has directories `bin`, `lib` and `home`.  The `home` directory contains the files to be installed, while the `bin` and `lib` directories contain scripts and libraries required to run the install script.

Within `dotfiles/home` only the following things should appear, directories, files, or symlinks.  Any symlinks should be relative symlinks only to other files, directories, or symlinks under `dotfiles/home`.

The install script does the following:

  - Installing a directory will create a directory in the destination location.
  - Installing a file will create a symlink from the destination location to the file in the source location.
  - Installing a symlink from the source location will create an identical symlink in the destination location.

When installing files from `dotfiles/home` occurences of `DOT_` are replaces with `.`.

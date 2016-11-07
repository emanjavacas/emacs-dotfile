
Emacs dot file
---

The idea is to have a branch for each distro.
Then, to point emacs to the right dot file:

1. Make sure to symbolically link ~/.emacs.d/init.el
   to the file in this repository.
   
   `ln -s ~/.emacs.d/init.el /path/to/repo/init.el`
   
2. Make sure you are in the right branch. For instance,
   for my ubuntu machine:
   
   `git checkout beast`

3. Done


Other stuff
---

If you (yes, this is me talking to myself) need to modify files
in the master branch (e.g. README, gitignore), to reflect those
changes in a distro-specific branch (e.g. beast), you can use
`git cherry-pick`, which allows you to "[a]pply the changes
introduced by some existing commits" (possibly in a different branch):

``` bash
enrique@beast:~/code/emacs$ git checkout beast

enrique@beast:~/code/emacs$ git cherry-pick master

[beast d38db73] filled README.md file & gitignore
 Date: Mon Nov 7 10:28:03 2016 +0100
 3 files changed, 15 insertions(+), 1 deletion(-)
 create mode 100644 .gitignore
 delete mode 100644 README
 create mode 100644 README.md
 
enrique@beast:~/code/emacs$ git push origin beast
```

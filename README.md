
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

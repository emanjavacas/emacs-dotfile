;; /This/ file (~init.el~) that you are reading
;; should be in this folder
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Package Manager
;; See ~Cask~ file for its configuration
;; https://github.com/cask/cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Keeps ~Cask~ file in sync with the packages
;; that you install/uninstall via ~M-x list-packages~
;; https://github.com/rdallasgray/pallet
(require 'pallet)

;; Root directory
(setq root-dir (file-name-directory
                (or (buffer-file-name) load-file-name)))

(ido-mode t)

;; CUSTOM
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(csv-separators (quote (";" "	")) t)
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(fci-rule-color "#383838" t)
 '(httpd-port 8082 t)
 '(js2-basic-offset 2 t)
 '(js2-bounce-indent-p nil t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-latex-create-formula-image-program (quote imagemagick))
 '(org-latex-table-caption-above nil t)
 '(org-preview-latex-default-process (quote imagemagick))
 '(vc-annotate-background "#2B2B2B" t)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))) t)
 '(vc-annotate-very-old-color "#DC8CC3" t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
(defun my/->string (str)
  (cond
   ((stringp str) str)
   ((symbolp str) (symbol-name str))))

(defun my/->mode-hook (name)
  "Turn mode name into hook symbol"
  (intern (replace-regexp-in-string
	   "\\(-mode\\)?\\(-hook\\)?$"
	   "-mode-hook"
	   (my/->string name))))

(defun my/->mode (name)
  "Turn mode name into mode symbol"
  (intern (replace-regexp-in-string
	   "\\(-mode\\)?$"
	   "-mode"
	   (my/->string name))))

(defun my/set-modes (arg mode-list)
  (dolist (m mode-list)
    (funcall (my/->mode m) arg)))

(defun my/turn-on (&rest mode-list)
  "Turn on the given (minor) modes."
  (my/set-modes +1 mode-list))

(defvar my/normal-base-modes
  (mapcar 'my/->mode '(text prog))
  "The list of modes that are considered base modes for
  programming and text editing. In an ideal world, this should
  just be text-mode and prog-mode, however, some modes that
  should derive from prog-mode derive from fundamental-mode
  instead. They are added here.")

(defun my/normal-mode-hooks ()
  "Returns the mode-hooks for `my/normal-base-modes`"
  (mapcar 'my/->mode-hook my/normal-base-modes))

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))
 
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))
 
(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.
   See `flush-lines' or `keep-lines' for behavior of this command.
   If the buffer is read-only, Emacs will beep and refrain from deleting
   the line, but put the line in the kill ring anyway.  This means that
   you can use this command to copy text from a read-only buffer.
   If the variable `kill-read-only-ok' is non-nil, then this won't even beep."
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))

(defun open-with ()
  "Simple function that allows us to open the underlying
file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if (eq system-type 'darwin)
                        "open"
                      (read-shell-command "Open current file with: "))
                    " "
                    buffer-file-name))))

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun kebab-case-region ()
  (interactive)
  (replace-region-by 's-dashed-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPEREANCE
;;;;;;;;;;;;;;;;;;;;
;; Theme
;; https://github.com/bbatsov/zenburn-emacs
(load-theme 'zenburn t)
(set-cursor-color "firebrick")

;;; Save desktop on exit
(desktop-save-mode 1)

;;; Don't break hard-links
(setq backup-by-copying-when-linked t)

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Deactivate pop-up windows
(defadvice yes-or-no-p (around prevent-dialog activate)
  "Prevent 'yes-or-no-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))
(defadvice y-or-n-p (around prevent-dialog-yorn activate)
  "Prevent 'y-or-n-p' from activating a dialog."
  (let ((use-dialog-box nil))
    ad-do-it))

;;; Misc
(setq password-cache-expiry nil)
(global-hl-line-mode 1)
(global-auto-revert-mode t)
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;
;; KEYSTROKES
;; Show keystrokes
(setq echo-keystrokes 0.02)
(defun switch-fullscreen nil
  (interactive)
  (let* ((modes '(nil fullboth fullwidth fullheight))
         (cm (cdr (assoc 'fullscreen (frame-parameters) ) ) )
         (next (cadr (member cm modes) ) ) )
    (modify-frame-parameters
     (selected-frame)
     (list (cons 'fullscreen next)))))

(define-key global-map [f11] 'switch-fullscreen)

;; Use shift to move around windows
(windmove-default-keybindings 'shift)

;; Swap lines
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))
(global-set-key (kbd "M-<up>") 'move-line-up)

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Shortcuts resize window
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<up>") 'shrink-window)
(global-set-key (kbd "S-C-<down>") 'enlarge-window)

(global-set-key (kbd "<C-S-u>")     'buf-move-up)
(global-set-key (kbd "<C-S-d>")   'buf-move-down)
(global-set-key (kbd "<C-S-l>")   'buf-move-left)
(global-set-key (kbd "<C-S-r>")  'buf-move-right)

;; Remove clutter
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Pretty lambdas
(add-hook 'prog-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil `(("(?\\(lambda\\>\\)"
                     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                               ,(make-char 'greek-iso8859-7 107))
                               nil)))))))
;; modeline
;; (sml/setup)
;; (sml/apply-theme 'dark)
;; (setq sml/shorten-directory t)
;; (setq sml/shorten-modes t)

;; Scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Path
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;; Wrap region
(require 'wrap-region)
(wrap-region-add-wrapper "*" "*")
(wrap-region-add-wrapper "+" "+")
(wrap-region-add-wrapper "-" "-")
(wrap-region-add-wrapper "/" "/")
(wrap-region-mode t)

;;; UNICODE 
(global-set-key (kbd "C-c u u") (lambda () (interactive) (insert "ü")))
(global-set-key (kbd "C-c u i") (lambda () (interactive) (insert "ï")))
(global-set-key (kbd "C-c u e") (lambda () (interactive) (insert "ë")))
(global-set-key (kbd "C-c u a") (lambda () (interactive) (insert "ä")))
(global-set-key (kbd "C-c u o") (lambda () (interactive) (insert "ö")))

(global-set-key (kbd "C-c u U") (lambda () (interactive) (insert "Ü")))
(global-set-key (kbd "C-c u I") (lambda () (interactive) (insert "Ï")))
(global-set-key (kbd "C-c u E") (lambda () (interactive) (insert "Ë")))
(global-set-key (kbd "C-c u A") (lambda () (interactive) (insert "Ä")))
(global-set-key (kbd "C-c u O") (lambda () (interactive) (insert "Ö")))

(global-set-key (kbd "C-c e e") (lambda () (interactive) (insert "é")))
(global-set-key (kbd "C-c e a") (lambda () (interactive) (insert "á")))
(global-set-key (kbd "C-c e o") (lambda () (interactive) (insert "ó")))
(global-set-key (kbd "C-c e i") (lambda () (interactive) (insert "í")))
(global-set-key (kbd "C-c e u") (lambda () (interactive) (insert "ú")))

(global-set-key (kbd "C-c e E") (lambda () (interactive) (insert "É")))
(global-set-key (kbd "C-c e A") (lambda () (interactive) (insert "Á")))
(global-set-key (kbd "C-c e O") (lambda () (interactive) (insert "Ó")))
(global-set-key (kbd "C-c e I") (lambda () (interactive) (insert "Í")))
(global-set-key (kbd "C-c e U") (lambda () (interactive) (insert "Ú")))

(global-set-key (kbd "C-c s s") (lambda () (interactive) (insert "ß")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yasnippet
(require 'yasnippet)
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(setq yas-snippet-dirs '("~/.emacs.d/plugins/yasnippet/snippets"))
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
(require 'magit)
(eval-after-load 'magit
  (progn '(global-set-key (kbd "C-x g") 'magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-specific tweaks
;;;;;;;;;;;;;;;;;;;;

;; Lisps
(setq my/lisps '(emacs-lisp lisp clojure))

(defun my/general-lisp-hooks ()
  (my/turn-on 'paredit
              'rainbow-delimiters
              'highlight-parentheses))

(dolist (mode (mapcar 'my/->mode-hook my/lisps))
  (add-hook mode 'my/general-lisp-hooks))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<C-M-left>") nil)
     (define-key paredit-mode-map (kbd "<C-M-right>") nil)))

;;;;;;;;;;;;;;;;;;;;
;; Clojure
(require 'clj-refactor)
;; (require 'cider-eval-sexp-fu)
(require 'cider)

(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")

(require 'ac-cider)

(defun my/cider-mode-hooks ()
  "Clojure specific setup code that should only be run when we
  have a CIDER REPL connection"
  (eldoc-mode)
  (company-mode)
 (ac-cider-popup-doc))

(add-hook 'cider-mode-hook 'my/cider-mode-hooks)

(defun my/clojure-mode-hooks ()
  (my/turn-on 'subword 'paredit 'rainbow-delimiters 'highlight-parentheses)
  (auto-complete-mode 1)
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(add-hook 'clojure-mode-hook 'my/clojure-mode-hooks)

(defun my/cider-repl-mode-hooks ()
  (my/turn-on 'paredit
              'rainbow-delimiters
              'highlight-parentheses
              'subword)
  (company-mode))

(add-hook 'cider-repl-mode-hook 'my/cider-repl-mode-hooks)

;;;;;;;;;;;;;;;;;;;;
;; Python
(require 'package)
(require 'nose)
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-hook 'python-mode-hook
	  (lambda ()
	    (pyenv-mode)
	    (nose-mode t)
	    (autopair-mode t)
	    (setq autopair-handle-action-fns
		  (list #'autopair-default-handle-action
			#'autopair-python-triple-quote-action))))
(elpy-enable)
(exec-path-from-shell-copy-env "PYTHONPATH")
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "LD_LIBRARY_PATH")
(elpy-use-ipython)
(setq python-remove-cwd-from-path nil)
(setq python-shell-interpreter "/home/manjavacas/.pyenv/shims/ipython"
      python-shell-interpreter-args "--simple-prompt -i")

;;;;;;;;;;;;;;;;;;;;
;;; Org-Mode

;;; Easy templates
(require 'org)
(add-to-list 'org-structure-template-alist '("n" "#+begin_notes\n?\n#+end_notes"))
(add-to-list 'org-structure-template-alist '("r" "#+attr_reveal: :"))

;;; Presentations
(require 'ox-reveal)
(setq org-reveal-root "file:///home/manjavacas/.emacs.d/lisp/reveal.js/")
(setq org-reveal-external-plugins
      '((toc-progress . "{src: 'file:///home/manjavacas/.emacs.d/list/reveal.js/plugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize(); toc_progress.create(); }}")
	))

;;; Blogging
(require 'ox-publish)
(require 'ox-html)

(setq org-publish-project-alist
      '(
        ;; interstylar directory
        ("interstylar-src"
         :base-directory "~/Documents/interstylar/org/" 
         :base-extension "org" 
         :publishing-directory "~/Documents/interstylar/jekyll/"
         :recursive t
         :publishing-function org-html-publish-to-html 
         :headline-levels 4
         :html-extension "markdown"
         :body-only t)  ;; Only export section between <body> </body>
        ("interstylar-static"
         :base-directory "~/Documents/interstylar/jekyll/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php" 
         :publishing-directory "~/Documents/interstylar/jekyll/" 
         :recursive t
         :publishing-function org-publish-attachment)
        ("interstylar" :components ("interstylar-src" "interstylar-static"))))

(defun partition-list (list length)
  (loop
     while list
     collect (subseq list 0 length)
     do (setf list (nthcdr length list))))

(defun get-hash-keys (table)
  (let ((keys '()))
    (maphash (lambda (k _) (push k keys)) table)
    keys))

(defun list->hash-table (alist)
  (let ((out (make-hash-table :test 'equal)))
    (dolist (elem (partition-list alist 2) out)
      (puthash (car elem) (car (cdr elem)) out))
    out))

(defvar org-publish-project-alist-hash-table
  (let ((out (make-hash-table :test 'equal)))
    (dolist (elem org-publish-project-alist out)
      (puthash (car elem) (list->hash-table (cdr elem)) out))
    out))

;; Improve our blogging experience with Org-Jekyll. This code sets four
;; functions with corresponding key bindings:
;;
;; C-c j n - Create new draft
;; C-c j P - Post current draft
;;
;; Once a draft has been posted (i.e., moved from the _drafts
;; directory to _post with the required date prefix in the filename), we
;; then need to html-export it to the jekyll rootdir (with org-publish).

(global-set-key (kbd "C-c J n") 'jekyll-draft-post)
(global-set-key (kbd "C-c J Q") 'jekyll-publish-post)

(global-set-key (kbd "C-c J t") 'insert-jekyll-date)

(defun insert-jekyll-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defvar jekyll-drafts-dir "_drafts/"
  "Relative path to drafts directory.")
(defvar jekyll-post-ext ".org"
  "File extension of Jekyll posts.")
(defvar jekyll-post-template
  "#+STARTUP: showall\n#+STARTUP: hidestars\n#+OPTIONS: H:2 num:nil tags:nil toc:nil timestamps:t
#+BEGIN_HTML\n---\nlayout: post\ntitle: %s\ndescription: %s\ndate: %s\n---\n#+END_HTML\n\n"
  "Default template for Jekyll posts. %s will be replace by the post title.")

(defun jekyll-make-slug (s)
  "Turn a string S into a slug."
  (replace-regexp-in-string
   " " "-" (downcase
	    (replace-regexp-in-string
	     "[^A-Za-z0-9 ]" "" s))))

(defun jekyll-yaml-escape (s)
  "Escape a string S for YAML."
  (if (or (string-match ":" s)
	  (string-match "\"" s))
      (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")
    s))

(defun jekyll-draft-post (project title description)
  "Create a new Jekyll blog post."
  (interactive
   (list (completing-read "Target project: " (get-hash-keys org-publish-project-alist-hash-table))
	 (read-string "Post Title: ")
	 (read-string "Post Description: ")))
  (let ((draft-file (concat (gethash :base-directory (gethash project org-publish-project-alist-hash-table))
			    jekyll-drafts-dir
			    (jekyll-make-slug title)
			    jekyll-post-ext))
	(current-time (format-time-string "%Y-%m-%d %H:%M:%S")))
    (if (file-exists-p draft-file)
	(find-file draft-file)
      (find-file draft-file)
      (insert (format jekyll-post-template (jekyll-yaml-escape title) description current-time)))))

(defun jekyll-publish-post (target-directory)
  "Move a draft post to the posts directory, and rename it so that it contains the date."
  (interactive
   (list (read-directory-name "Post target directory: ")))
  (cond
   ((equal
     (file-name-directory (buffer-file-name (current-buffer)))
     target-directory)
    (message "Target directory can't be drafts directory [%s]"
	     (file-name-directory (buffer-file-name (current-buffer))))
    (insert (file-name-directory (buffer-file-name (current-buffer))) "\n"
	    target-directory))
   ((buffer-modified-p)
    (message "Can't publish post; buffer has modifications."))
   (t
    (let ((filename
	   (concat target-directory
		   (format-time-string "%Y-%m-%d-")
		   (file-name-nondirectory
		    (buffer-file-name (current-buffer)))))
	  (old-point (point)))
      (if (not (file-exists-p (file-name-directory filename)))
	  (make-directory (file-name-directory filename)))
      (rename-file (buffer-file-name (current-buffer)) filename)
      (kill-buffer nil)
      (find-file filename)
      (set-window-point (selected-window) old-point)))))

;;;;;;;;;;;;;;;;;;;;
;;; ORG-BABEL

;;; fontify source code
(require 'ox-latex)
;; Add minted to the defaults packages to include when exporting.
(add-to-list 'org-latex-packages-alist '("" "minted"))
;; Tell the latex export to use the minted package for source code coloration.
(setq org-latex-listings 'minted)
;; Let the exporter use the -shell-escape option to let latex execute external programs.
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex %f %f"
	"latexmk -pdflatex='pdflatex -shell-escape -interaction nonstopmode' -pdf -bibtex %f %f"))

(setq org-latex-minted-options
      '(("frame" "lines")
	("fontsize" "\\small")
	("linenos" "")))

;;; Show syntax highlighting per language native mode in *.org
(setq org-src-fontify-natively t)
;;; smart quotes
(setq org-export-with-smart-quotes t)
;;; Syntax highlighting
(setq-default org-src-fontify-natively t)
;;; don't ask for confirmation
(setq org-confirm-babel-evaluate nil)
;;; don't evaluate when exporting
(setq org-export-babel-evaluate nil)
;;; For languages with significant whitespace like Python:
(setq org-src-preserve-indentation t)

(defvar my/org-babel-evaluated-languages
  '(emacs-lisp clojure R python dot)
  "List of languages that may be evaluated in Org documents")

(org-babel-do-load-languages
 'org-babel-load-languages
 (mapcar (lambda (lang)
           (cons lang t))
         my/org-babel-evaluated-languages))

(require 'ob-clojure)
(setq org-babel-clojure-backend 'cider)

;;; Diagramming
(add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
(add-to-list 'my/org-babel-evaluated-languages 'dot)
(add-to-list 'my/org-babel-evaluated-languages 'plantuml)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;
;; Ess (Emacs speaks statistics)
(setq ess-ask-for-ess-directory nil)

;;;;;;;;;;;;;;;;;;;;
;;; Tramp
;; (setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;
;;; LATEX
;;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
 
;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -synctex=1 -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
 
;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (setq TeX-view-program-list
;;       '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;;; (helm)-BIBTEX
(autoload 'helm-bibtex "helm-bibtex" "" t)
(setq helm-bibtex-bibliography '("/home/manjavacas/Documents/texts/bib/library.bib"))

;;; LATEX
(setq latex-run-command "pdflatex")

;;;;;;;;;;;;;;;;;;;;;
;;; Webdev
(require 'nodejs-repl)
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js-mode-hook 'autopair-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

(require 'flycheck)
(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))

;;; HTML
(add-hook 'html-mode-hook 'autopair-mode)

;;; CSS
(add-hook 'css-mode-hook 'autopair-mode)

;;; XML
;;; http://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)

(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]*[^/]>"
               "-->\\|</[^/>]*[^/]>"

               "<!--"
               sgml-skip-tag-forward
               nil))

(add-hook 'nxml-mode-hook 'hs-minor-mode)
(define-key nxml-mode-map (kbd "C-c h") 'hs-toggle-hiding)

(defun nxml-where ()
      "Display the hierarchy of XML elements the point is on as a path."
      (interactive)
      (let ((path nil))
        (save-excursion
          (save-restriction
            (widen)
            (while (and (< (point-min) (point))
                        (condition-case nil
                            (progn
                              (nxml-backward-up-element) ; always returns nil
                              t)
                          (error nil)))
              (setq path (cons (xmltok-start-tag-local-name) path)))
            (if (called-interactively-p t)
                (message "/%s" (mapconcat 'identity path "/"))
              (format "/%s" (mapconcat 'identity path "/")))))))

;;; Java
(require 'eclim)
(require 'eclimd)
(global-eclim-mode)

;;; Markdown
(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
	   (format
	    "<!DOCTYPE html><html><title>Impatient Markdown</title>
             <xmp theme=\"united\" style=\"display:none;\">%s</xmp>
             <script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>"
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (current-buffer)))

;;; Octave
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;; Txt
(add-hook 'text-mode-hook 'wc-mode)

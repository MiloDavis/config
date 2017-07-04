;; -*- mode: emacs-lisp ;-*-
;;; Commentary:

;;; Code:
(setq user-emacs-directory "~/.emacs.d/")

(require 'cl)

(defvar osx (equal system-type 'darwin))

(require 'server)
(unless (server-running-p)
  (server-start))

;; Packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  (setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("marmalade" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))
  (unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
  (eval-when-compile
    (require 'use-package))
  (setq use-package-always-ensure t)
  (setq use-package-ensure-function
		(lambda (package-name ensure-val state ctx)
		  (package-installed-p package-name)))
  (use-package diminish)
  (use-package bind-key)
  (global-set-key (kbd "C-c p") 'package-list-packages)
  )

(setq vc-follow-symlinks t)

(defun open-file-binding (file)
  "FILE The name of the file to be oppened."
  (lexical-let ((file file))
	(lambda () (interactive) (find-file file))))

;; Set open notes.org key binding
(defvar notes-directory "~/notes/")
(defun open-notes-file (file)
  "FILE The name of the notes file."
  (open-file-binding (concat notes-directory file)))

(global-set-key (kbd "C-c n r") (open-notes-file "research.org"))
(global-set-key (kbd "C-c n m") (open-notes-file "misc.org"))
(global-set-key (kbd "C-c n s") (open-notes-file "scouting.org"))
(global-set-key (kbd "C-c n n") (open-notes-file "nutrons.org"))
(global-set-key (kbd "C-c n p") (open-notes-file "personal.org"))
(global-set-key (kbd "C-c n a") (open-notes-file "analysis.org"))
(global-set-key (kbd "C-c n h") (open-notes-file "hacks.org"))
(global-set-key (kbd "C-c n c") (open-notes-file "courses.org"))
(global-set-key (kbd "C-c n t") (open-notes-file "tezos.org"))

(require 'linum)
(defun my/turn-off-linum-mode ()
  (linum-mode -1))

;; Modes for file types
(add-to-list 'auto-mode-alist '("emacs'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.scsh\\'" . scheme-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s\\'" . nasm-mode))

(setq-default tab-width 4)

;; Sets compile configuration
(global-set-key (kbd "C-<tab> ") 'compile)
(setq compilation-scroll-output t)

;; Remove extra UI elements
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if osx nil (menu-bar-mode -1)) ; On OSX show menus, but on arch don't

;; Scroll by one
(global-set-key (kbd "<M-up>") (lambda () (interactive) (scroll-down-command 1)))
(global-set-key (kbd "<M-down>") (lambda () (interactive) (scroll-up-command 1)))

;; Set open .emacs key binding
(global-set-key (kbd "C-c c e") (open-file-binding "~/.emacs"))
(global-set-key (kbd "C-c c b") (open-file-binding "~/.bash_profile"))
(global-set-key (kbd "C-c c g") (open-file-binding "~/.gitconfig"))
(global-set-key (kbd "C-c c s") (open-file-binding "~/.ssh/config"))


;; Sets eval-buffer key binding
(global-set-key (kbd "C-c c C-e") 'eval-buffer)

;; Sets reverse window bindings
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

;; Sets indent-region keybinding
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-M-q ") 'prog-indent-sexp)

;; Sets man key binding
(global-set-key (kbd "C-c m") 'man)

;; Abbreviations
(define-abbrev-table 'global-abbrev-table
  '(("8alpha"   "α")
    ("8beta"    "β")
    ("8gamma"   "γ")
    ("8Delta"   "Δ")
    ("8delta"   "δ")
    ("8theta"   "θ")
    ("8lambda"  "λ")
    ("8mu"      "µ")
    ("8nu"      "ν")
    ("8pi"      "π")
    ("8Sigma"   "Σ")
    ("8sigma"   "σ")
    ("8tau"     "τ")
    ("8phi"     "φ")
    ("8psi"     "ψ")
    ("8Omega"   "Ω")
    ("8omega"   "ω")
	("8eta"     "η")
    ("8in"      "∈")
    ("8nin"     "∉")
    ("8inf"     "∞")
    ("8forall"  "∀")
    ("8exists"  "∃")
    ("8neq"     "≠")
    ("8in"      "∈")
    ("8tab"     "	")
	("8rarrow"  "→")
	("8bot"  "⊥")
	("8top"  "⊤")
	("8intersect"  "∩")
	("8and" "∧")
	("8or" "∨")
	("8subset" "⊆")
	("8ssubset" "⊂")
	("8null" "∅")
	("8cdot" "·")
	("8join" "⊔")
	("8meet" "⊓")
	("8sqsubseteq" "⊑")
	("8nobreak" "﻿")
	))
(setq save-abbrevs t)                 ;; (ask) save abbrevs when files are saved
(setq-default abbrev-mode t)          ;; turn it on for all modes


;; Sets navigation Key bindings
(global-set-key (kbd "M-N") (lambda () "Move down 5 lines." (interactive) (next-line 5)))
(global-set-key (kbd "M-n") (lambda () "Move down 5 lines." (interactive) (next-line 5)))
(global-set-key (kbd "M-P") (lambda () "Move up 5 lines." (interactive) (previous-line 5)))
(global-set-key (kbd "M-p") (lambda () "Move up 5 lines." (interactive) (previous-line 5)))


;; Jump to character
(defun jump-to-char-forward (arg char)
  (interactive "p\ncJump to char (forward): ")
  (progn
	(forward-char 1)
	(search-forward (char-to-string char) nil nil arg)
	(point)))

(defun jump-to-char-backward (arg char)
  (interactive "p\ncJump to char (backwards): ")
  (progn
	(backward-char 1)
	(search-backward (char-to-string char) nil nil arg)
	(point)))


;; Moves to first non-whitespace character, then character 0
(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed.
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

(global-set-key (kbd "M-o") 'other-window)

;; Zap up to character
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "M-j") 'jump-to-char-forward)
(global-set-key (kbd "M-J") 'jump-to-char-backward)

;; Sets meta key to be command
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'control)

;; Sets undo keybindings
(global-set-key (kbd "C--") 'undo)

;; Shell keybinding and settings
(global-set-key (kbd "C-;") 'shell)
;; (comint-process-echoes 't)
;; This code allows programs to keep their formatting when shell frame size changes
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))

(defun my-shell-mode-hook ()
  "Add this hook as buffer local, so it will run once per window."
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(add-hook 'shell-mode-hook 'dirtrack-mode)
(add-hook 'shell-mode-hook 'my/turn-off-linum-mode)

;; Unsets frame suspension behavior in gui mode because I kept hitting it by accident
(if (window-system )
    (global-unset-key (kbd "C-z"))
  nil)

;; C mode settings
(setq-default c-basic-offset 4)
(add-hook 'c-mode-common-hook
          (lambda ()
	    (c-set-offset 'case-label '+)))

;; Sets locking file behavior
(setq create-lockfiles nil)

;; Sets autosave directory
(setq backup-directory-alist
      `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves" t)))

;; Makes scripts executable if file is a script
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
	("d5f17ae86464ef63c46ed4cb322703d91e8ed5e718bf5a7beb69dd63352b26b2" default)))
 '(debug-on-error nil)
 '(default-input-method "TeX")
 '(ecb-layout-name "left2")
 '(ecb-layout-window-sizes
   (quote
	(("left2"
	  (ecb-directories-buffer-name 0.14444444444444443 . 0.49411764705882355)
	  (ecb-sources-buffer-name 0.14444444444444443 . 0.49411764705882355)))))
 '(ecb-options-version "2.40")
 '(hl-sexp-background-color "#efebe9")
 '(ido-ignore-files
   (quote
	("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".+~" "*.aux" "*.log" "*.pyc" "*.native" "*.byte")))
 '(jdee-server-dir "~/.emacs.d/jdee-server/target/")
 '(kill-ring-max 100000)
 '(org-directory "~/notes")
 '(org-src-tab-acts-natively t)
 '(org-trello-current-prefix-keybinding "C-c o")
 '(package-selected-packages
   (quote
	(tuareg json json-snatcher web-mode forth-mode ox-md uniquify vlf-setup utop tuareg-mode merlin-mode yafolding which-key vlf use-package realgud ox-gfm org-gnome org-fstree org-elisp-help org-edit-latex org-easy-img-insert org-drill-table org-dp org-download org-dashboard org-clock-today org-clock-convenience org-babel-eval-in-repl org-ac ocp-indent ob-typescript ob-swift ob-sql-mode ob-prolog ob-php ob-mongo ob-kotlin ob-ipython ob-http ob-async nodejs-repl nasm-mode mysql2sqlite mysql-to-org multi-term marmalade-client markdown-mode leuven-theme latex-preview-pane latex-math-preview latex-extra jumblr json-mode js2-mode jedi jdee irony iedit idris-mode ido-vertical-mode highlight-indentation haskell-mode gnuplot-mode gnuplot github-clone git-timemachine framemove frame-cmds flyspell-lazy flycheck-ocaml flycheck-clangcheck fireplace exec-path-from-shell ensime emacsql-mysql elm-mode elisp-depend el-get egg edts edit-color-stamp discover-my-major discover diff-hl debbugs company-jedi company-coq column-enforce-mode color-theme cl-lib-highlight chicken-scheme cdlatex bury-successful-compilation bison-mode auto-complete-clang auto-auto-indent async-await ac-math ac-ispell ac-html 2048-game)))
 '(paradox-github-token t)
 '(selected-packages
   (quote
	(ox-md use-package 2048-game ac-html ac-ispell ac-math async-await auctex auto-auto-indent auto-complete auto-complete-clang bison-mode bury-successful-compilation cdlatex chicken-scheme cl-generic cl-lib-highlight color-theme column-enforce-mode company-jedi debbugs diff-hl discover discover-my-major edit-color-stamp edts egg el-get elisp-depend elm-mode emacs-eclim emacsql emacsql-mysql ensime exec-path-from-shell flycheck-clangcheck flycheck-ocaml flyspell-lazy frame-cmds framemove git-timemachine github-clone gnuplot gnuplot-mode haskell-mode highlight-indentation htmlize iedit irony jdee jedi js2-mode json-mode jumblr latex-extra latex-math-preview latex-preview-pane leuven-theme markdown-mode marmalade-client multi-term mysql-to-org mysql2sqlite nasm-mode nodejs-repl ob-async ob-http ob-ipython ob-kotlin ob-mongo ob-php ob-prolog ob-sql-mode ob-swift ob-typescript ocp-indent org-ac org-babel-eval-in-repl org-clock-convenience org-clock-today org-dashboard org-download org-dp org-drill-table org-easy-img-insert org-edit-latex org-elisp-help org-fstree org-gnome org-grep org-jira org-journal org-link-travis org-mac-link org-magit org-notebook org-octopress org-outlook org-page org-parser org-password-manager org-pdfview org-pomodoro org-present org-preview-html org-projectile org-protocol-jekyll org-publish-agenda org-random-todo org-readme org-recent-headings org-redmine org-ref org-repo-todo org-review org-rtm org-seek org-sticky-header org-sync-snippets org-table-comment org-tree-slide org-vcard org-wc orgbox orgit orglink orglue orgtbl-aggregate orgtbl-ascii-plot orgtbl-join orgtbl-show-header ox-gfm ox-jira ox-twbs php-mode projectile projectile-codesearch projectile-git-autofetch prolog pyvenv racket-mode realgud regex-tool repl-toggle scala-mode2 scheme-complete scheme-here scribble-mode symon tabbar tuareg unbound vlf web-completion-data web-mode which-key wrap-region yafolding yaml-mode)))
 '(setq ecb-tip-of-the-day)
 '(which-key-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "black" :foreground "green3"))))
 '(magit-diff-removed ((t (:background "black" :foreground "red3")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; VLF mode
(use-package vlf-setup
  :config (add-hook 'vlf-mode-hook 'vlf-toggle-follow))

(global-set-key (kbd "M-Y") (lambda () (interactive) (yank-pop -1)))

;; Sets kill and close
(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))
(global-set-key (kbd "C-c k") 'close-and-kill-this-pane)




;; Sets path variable
(exec-path-from-shell-initialize)
(add-to-list 'exec-path-from-shell-variables "TEXINPUTS")
(exec-path-from-shell-initialize )


;; Macros:
(fset 'clear-repl
      (lambda (&optional arg)
		"Keyboard macro." 
		(interactive "p") 
		(kmacro-exec-ring-item 
		 (quote ([134217790 1 67108896 5 backspace 16 16 67108896 134217788 backspace 14 backspace 134217848 
							100 105 115 M-backspace 98 117 102 102 101 114 32 100 105 115 97 98 108 101 32 
							117 110 100 111 return 134217848 98 117 102 102 101 114 32 101 110 97 98 108 101 
							32 117 110 100 111 return 134217790 32] 0 "%d")) arg)))


(global-set-key (kbd "C-c b a") 'clear-repl)

(fset 'backward-delete-sexp
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108896 134217730 backspace] 0 "%d")) arg)))

(fset 'forward-delete-sexp
      [?\C-  ?\C-\M-f backspace])

(global-set-key (kbd "<C-M-backspace>") 'backward-delete-sexp)
(global-set-key (kbd "C-M-d") 'forward-delete-sexp) ; For some reason this doesn't work unless the shift key is held

;; Delete key
(defun delete-word (arg)
  "ARG Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "ARG Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; Changes kill word behavior to delete words instead
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-delete-word)
(global-set-key (read-kbd-macro "M-d") 'delete-word)


;; Folding
(yafolding-mode 1)
(global-set-key (kbd "C-c f") 'yafolding-toggle-element)
(global-set-key (kbd "C-c C-f") 'yafolding-toggle-element)

;; Stop annoying startup messages
(setq inhibit-startup-message t) ; Emacs splash screen

;; ocaml
(use-package merlin-mode
  :config
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-command "~/.opam/system/bin/ocamlmerlin")
  (setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
  (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
  (setq merlin-use-auto-complete-mode 'easy)
  (setq merlin-ac-setup 'easy)
  (set-face-background 'merlin-type-face "#88FF44")
  :bind (:map merlin-mode-map
			  ("C-c <up>" . merlin-type-enclosing-go-up)
			   ("C-c <down>" . merlin-type-enclosing-go-down)))
(use-package ocp-indent)
(use-package tuareg-mode
  :config
  (dolist
	  (var (car (read-from-string
				 (shell-command-to-string "opam config env --sexp"))))
	(setenv (car var) (cadr var)))
  (setq auto-mode-alist 
		(append '(("\\.ml[ily]?$" . tuareg-mode))
				auto-mode-alist))
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode))
			  auto-mode-alist))
  (add-hook 'tuareg-mode-hook 'auto-complete-mode)
  :bind (:map tuareg-mode-map
			  ("M-C-." . completion-at-point)))
(use-package utop
  :config (setq utop-command "opam config exec -- utop -emacs"))


;; show-file-name
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun toggle-window-split ()
  "Toggle direction of window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))
	(message "toggle-window-split only works with exactly two buffers")))

;; Sets buffer move and info
(global-set-key (kbd "C-c b r") 'buf-move-right)
(global-set-key (kbd "C-c b l") 'buf-move-left)
(global-set-key (kbd "C-c b u") 'buf-move-up)
(global-set-key (kbd "C-c b d") 'buf-move-down)

(global-set-key (kbd "C-c b p") 'show-file-name)
(global-set-key (kbd "C-c b v") 'split-window-right)
(global-set-key (kbd "C-c b h") 'split-window-below)
(global-set-key (kbd "C-c b n") 'rename-buffer)
(global-set-key (kbd "C-c b t") 'toggle-window-split)
(global-set-key (kbd "C-c b w")
				(lambda ()
				  (interactive)
				  (kill-new (buffer-file-name))
				  (message (concat "Copied path " (buffer-file-name ) " to kill ring"))))

;; Org mode
(use-package org
  :ensure nil
  :config
  (setq org-agenda-files `(,notes-directory))
  (setq org-directory notes-directory)
  (setq org-src-tab-acts-natively t)
  (setq org-src-fontify-natively t)
  (add-hook 'org-mode-hook 'my/turn-off-linum-mode)
  (define-key global-map "\C-cl" 'org-store-link)
  (setq org-log-done t)
  (setq org-startup-folded t)
  (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED")))
  (add-hook 'org-mode-hook 'visual-line-mode)
  (use-package ox-md)
  (use-package ox-gfm)
  (use-package ob-async)
  (setq org-babel-sh-command "/bin/bash")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
	 (ocaml . t)
	 (sh . t)
	 (shell . t)
	 (java . t)
	 (emacs-lisp . t)
	 (prolog . t)
	 (C . t)))
  (setq org-export-with-toc nil)
  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (defalias 'org-insert-timestamp-inactive 'org-time-stamp-inactive)
  (defun insert-answers ()
	"Insert the answers HTML class attribute for nutrons training."
	(interactive)
	(search-backward-regexp "^*")
	(next-line 1)
	(insert ":PROPERTIES:\n:HTML_CONTAINER_CLASS: answers\n:END:\n"))
  :bind (:map org-mode-map
			  ("C-c C-M-f" . org-metaright)
			  ("C-c C-M-b" . org-metaleft)
			  ("C-c C-1" . org-time-stamp-inactive)
			  ("C-c a t" . org-todo-list))
  :init (use-package htmlize))


;; Yes or no alias
(defalias 'yes-or-no-p 'y-or-n-p)

(defun copy-buffer-name ()
  "Add buffer name to the kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(global-set-key (kbd "C-c y f") 'copy-buffer-name)

;; Duplicate line
(defun duplicate-line ()
  "Copy a line onto the line below."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )
(global-set-key (kbd "C-c d") 'duplicate-line)

;; Makes deleted files go to the trash
(setq delete-by-moving-to-trash t)

;; Dired settings
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))))
(put 'dired-find-alternate-file 'disabled nil)
(global-set-key (kbd "C-x C-d") 'ido-dired)

;; Adds a newline at the end of every file
(setq require-final-newline t)

;; Removes duplicates from history
(setq history-delete-duplicates t)

;; Ido settings
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always)
  (setq ido-file-extensions-order
		'(".org" ".ml" ".rkt" ".py" "Makefile" ".java" ".txt" ".emacs" ".php" ".js" ".html" ".tex" ".scrbl"))
  (setq ido-create-new-buffer 'always
		ido-use-filename-at-point nil)
  (ido-mode 1))


;; Input unicode using tex with: M-x set-input-method RET tex RET

;; Coq
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-subdirs-to-load-path))
(defvar proof-site-location "~/.emacs.d/lisp/PG/generic/proof-site.elc")
(if (file-exists-p proof-site-location)
	(progn (load proof-site-location)
		   (require 'proof-site)
		   (setq proof-splash-seen t)
		   (setq proof-three-window-mode-policy 'hybrid)
		   (setq proof-script-fly-past-comments t))
  (message "Proof general not installed"))

(use-package company-coq
  :config
  (add-hook 'coq-mode-hook #'company-coq-initialize))
(add-hook 'coq-mode-hook (lambda ()
							 (abbrev-mode -1)
							 (flycheck-mode -1)))

;; Jedi mode
(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
  (autoload 'jedi:setup "jedi" nil t))

(defvar python-base-shebang "#!/usr/bin/env python")

(defun _insert-at-beginning-of-file (str)
  "Insert a string at the beginning of a file.
STR String to be inserted"
  (interactive)
  (goto-char 0)
  (insert str))

(defun _insert-python-shebang (ending)
  "ENDING The last characters of the python shebang.  Characters are insterted after `python-base-shebang`."
  (_insert-at-beginning-of-file (concat python-base-shebang ending)))

(defun insert-python-shebang ()
  "Insert a shebang line for default python (python2)."
  (interactive)
  (_insert-python-shebang "\n"))

(defun insert-python3-shebang ()
  "Insert a shebang line for python3."
  (interactive)
  (_insert-python-shebang "3\n"))

(defun insert-bash-shebang ()
  "Insert a shebang line for bash."
  (interactive)
  (_insert-at-beginning-of-file "#!/usr/bin/env bash\n"))

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; Improved splitting functions
;; https://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldury/
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
   (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)



;; Tramp mode
(setq tramp-default-method "ssh")

(setq ring-bell-function 'ignore)

(defalias 'sir 'string-insert-rectangle)

;; Markdown
(add-hook 'markdown-mode 'markdown-preview-mode)
(add-hook 'markdown-mode 'markdown-preview-open-browser)

;; Enabled commands
(put 'set-goal-column 'disabled nil)
(put 'set-mark-command-repeat-pop 'disabled nil)

;; REPLs
(global-set-key (kbd "C-c r o") 'utop)
(global-set-key (kbd "C-c r r") 'racket-repl)
(global-set-key (kbd "C-c r p") 'run-python)

(set-face-attribute 'default nil :height (if osx 125 100))

;; Sets multiple buffer names to prefix with path
(use-package uniquify
  :config 
  (setq uniquify-buffer-name-style 'forward))

;; Saves things from clipboard if emacs text is
;; killed before the clipboard text was yanked
(setq save-interprogram-paste-before-kill t)

;; Magit config
(use-package magit
  :init (global-set-key (kbd "C-x g") 'magit-status)
  :config (setq magit-push-current-set-remote-if-missing (quote default)))
  

(load-file "~/config/michelson-mode/michelson-mode.el")

;; Global modes
(use-package diff-hl
  :config (global-diff-hl-mode t))

(global-linum-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)

(use-package auto-complete
  :config (global-auto-complete-mode t))
(use-package which-key
  :config (which-key-mode))
(use-package color-theme
  :config (if (window-system)
			  (load-theme 'leuven)
			nil))

(column-number-mode)
(use-package flycheck
  :config (add-hook 'after-init-hook #'global-flycheck-mode))
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)

(provide 'emacs)
;;; emacs ends here

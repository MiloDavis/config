;; -*- mode: Lisp;-*-
(require 'cl)

(setq user-emacs-directory "~/.emacs.d/")

(require 'server)
(unless (server-running-p)
  (server-start))
(setq vc-follow-symlinks t)

;; Set open notes.org key binding
(defvar notes-directory "~/notes/")
(defvar research-notes "research.org")
(defvar general-notes "general.org")
(defvar misc-notes "misc.org")
(defvar scouting-notes "scouting.org")
(defvar nutrons-notes "nutrons.org")
(defvar personal-notes "personal.org")
(defvar analysis-notes "analysis.org")
(global-set-key (kbd "C-c n g") (lambda() (interactive) (find-file (concat notes-directory general-notes))))
(global-set-key (kbd "C-c n r") (lambda() (interactive) (find-file (concat notes-directory research-notes))))
(global-set-key (kbd "C-c n m") (lambda() (interactive) (find-file (concat notes-directory misc-notes))))
(global-set-key (kbd "C-c n s") (lambda() (interactive) (find-file (concat notes-directory scouting-notes))))
(global-set-key (kbd "C-c n n") (lambda() (interactive) (find-file (concat notes-directory nutrons-notes))))
(global-set-key (kbd "C-c n p") (lambda() (interactive) (find-file (concat notes-directory personal-notes))))
(global-set-key (kbd "C-c n a") (lambda() (interactive) (find-file (concat notes-directory analysis-notes))))
(setq org-agenda-files '("~/notes/"))

(defun my/turn-off-linum-mode ()
  (linum-mode -1))

;; Modes for file types
(add-to-list 'auto-mode-alist '("\\emacs\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.scsh\\'" . scheme-mode))

(setq-default tab-width 4)

;; Sets compile configuration
(global-set-key (kbd "C-<tab> ") 'compile)
(setq compilation-scroll-output t)

;; Remove extra UI elements
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (eq system-type 'darwin) nil (menu-bar-mode -1)) ; On OSX show menus, but on arch don't

;; Scroll by one
(global-set-key (kbd "<M-up>") (lambda () (interactive) (scroll-down-command 1)))
(global-set-key (kbd "<M-down>") (lambda () (interactive) (scroll-up-command 1)))

;; Set open .emacs key binding
(global-set-key (kbd "C-c c e") (lambda() (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-c c b") (lambda() (interactive) (find-file "~/.bash_profile")))
(global-set-key (kbd "C-c c g") (lambda() (interactive) (find-file "~/.gitconfig")))


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
(define-abbrev-table 'global-abbrev-table '(
    ("8alpha"   "α")
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
    ("8in"      "∈")
    ("8nin"     "∉")
    ("8inf"     "∞")
    ("8forall"  "∀")
    ("8exists"  "∃")
    ("8neq"     "≠")
    ("8in"      "∈")
    ("8tab"     "	")
	("8rarrow"  "→")
	("8bottom"  "⊥")
	("8top"  "⊤")
	("8intersect"  "∩")
	("8and" "∧")
	("8or" "∨")
    ))
(setq save-abbrevs t)                 ;; (ask) save abbrevs when files are saved
(setq-default abbrev-mode t)          ;; turn it on for all modes


;; Sets navigation Key bindings
(global-set-key (kbd "M-N") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-P") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))

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

;; Sets iedit global key binding
(global-set-key (kbd "C-c ;") 'iedit-mode)
;; I'm not sure if this fixes an issue where sometimes I end up in iedit mode accidentally
(add-hook 'iedit-mode-hook (lambda () (define-key flyspell-mode-map (kbd "C-;") nil)))

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
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)
(add-hook 'shell-mode-hook 'dirtrack-mode)
(add-hook 'shell-mode-hook 'my/turn-off-linum-mode)

;; Sets regexp search/replace keybindings
(global-set-key (kbd "C-c s r") 'replace-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Unsets frame suspension behavior in gui mode because I kept hitting it on OSX
(if (window-system )
    (global-unset-key (kbd "C-z"))
  nil)

;; Sets Scheme interpreter to be scsh 0.6.7
(setq scheme-program-name "scsh67")

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

;; Packages
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
  )

(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("marmalade" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

(global-set-key (kbd "C-c p") 'package-list-packages)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
	("89b5c642f4bbcf955215c8f756ae352cdc6b7b0375b01da1f1aa5fd652ae822e" "6e4f8aba68e6934ad0e243f2fc7e6778d87f7d9b16e069cb9fec0cfa7f2f845a" "bb749a38c5cb7d13b60fa7fc40db7eced3d00aa93654d150b9627cabd2d9b361" "4bf9b00abab609ecc2a405aa25cc5e1fb5829102cf13f05af6a7831d968c59de" "0dfa1f356bdb48aa03088d4034b90c65290eb4373565f52f629fdee0af92a444" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(debug-on-error t)
 '(default-input-method "TeX")
 '(ecb-layout-name "left2")
 '(ecb-layout-window-sizes
   (quote
	(("left2"
	  (ecb-directories-buffer-name 0.14444444444444443 . 0.49411764705882355)
	  (ecb-sources-buffer-name 0.14444444444444443 . 0.49411764705882355)))))
 '(ecb-options-version "2.40")
 '(ido-ignore-files
   (quote
	("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" ".+~" "*.aux" "*.log" "*.pyc")))
 '(kill-ring-max 100000)
 '(org-src-tab-acts-natively t)
 '(package-selected-packages
   (quote
	(org-clock-today htmlize matlab-mode color-theme auto-complete bury-successful-compilation latex-math-preview company-jedi ob-applescript ob-axiom ob-browser ob-coffee ob-cypher ob-dart ob-diagrams ob-elixir ob-go ob-http ob-ipython ob-kotlin ob-lfe ob-ml-marklogic ob-mongo ob-nim ob-php ob-prolog ob-redis ob-restclient ob-sagemath ob-smiles ob-sml ob-spice ob-swift ob-translate ob-typescript org-clock-convenience company-coq latex-extra cdlatex org-beautify-theme leuven-theme marmalade-client haskell-mode yaml-mode yafolding wrap-region web-completion-data vlf utop unbound tuareg totd tabbar symon solarized-theme smooth-scroll scribble-mode scheme-here scheme-complete scala-mode2 repl-toggle regex-tool racket-mode pyvenv php-mode origami org-bullets ocp-indent nodejs-repl nim-mode multi-term markdown-mode latex-preview-pane jumblr json-mode js2-mode jedi jdee irony iedit highlight-indentation gruvbox-theme god-mode github-clone git-timemachine framemove frame-cmds flyspell-lazy flycheck-ocaml flycheck-clangcheck fastnav faff-theme exec-path-from-shell evil-visual-mark-mode ensime emacs-eclim elm-mode elisp-depend el-get egg edts edit-color-stamp ecb-snapshot ecb docean discover-my-major discover diff-hl debbugs darkroom column-enforce-mode color-theme-solarized color-theme-cobalt cl-lib-highlight cl-generic chicken-scheme buffer-move bookmark+ auto-complete-clang auto-auto-indent auctex anzu ample-zen-theme ac-math ac-ispell ac-html)))
 '(paradox-github-token t)
 '(setq ecb-tip-of-the-day))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; VLF mode
(require 'vlf-setup)
(add-hook 'vlf-mode-hook 'vlf-toggle-follow)

(defun rev-yank-pop ()
  (interactive)
  (yank-pop -1))

(global-set-key (kbd "M-Y") 'rev-yank-pop)

;; sets backtab to remove 4 spaces
(global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
(defun un-indent-by-removing-4-spaces ()
  "remove 4 spaces from beginning of of line"
  (interactive)
  (save-excursion
    (save-match-data
      (beginning-of-line)
      ;; get rid of tabs at beginning of line
      (when (looking-at "^\\s-+")
        (untabify (match-beginning 0) (match-end 0)))
      (when (looking-at "^    ")
        (replace-match "")))))

;; Sets kill and close
(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))
(global-set-key (kbd "C-c k") 'close-and-kill-this-pane)




;; Sets path variable
(exec-path-from-shell-initialize )
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
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
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
(setq ecb-tip-of-the-day nil) ; ECB tip of the day

;; ocaml
(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode))
	      auto-mode-alist))
;;(add-hook 'tuareg-mode-hook 'column-enforce-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode 'easy)
(add-hook 'tuareg-mode-hook (lambda () (local-set-key (kbd "M-C-.") 'completion-at-point)))

(add-to-list 'load-path "/Users/milodavis/.opam/system/share/emacs/site-lisp")
(require 'ocp-indent)

;; -- opam and utop setup --------------------------------
;; Setup environment variables using opam
(dolist
    (var (car (read-from-string
	       (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Merlin mode
(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(setq merlin-command "/home/milo/.opam/system/bin/ocamlmerlin")
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)

;; Enable Merlin for ML buffers
(add-hook 'tuareg-mode-hook 'merlin-mode)

;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
;; by spaces.
(define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
(set-face-background 'merlin-type-face "#88FF44")

(add-hook 'tuareg-mode-hook 'auto-complete-mode)


;; show-file-name
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun toggle-window-split ()
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

(fset 'make-jira-link
   "\C-w[[https://basistech.atlassian.net/browse/\C-y][\C-y]]")


;; Org mode
(setq org-src-fontify-natively t)
(add-hook 'org-mode-hook 'my/turn-off-linum-mode)
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)
(setq org-startup-folded t)
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CANCELLED")))
;; (add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c C-M-f") 'org-metaright)
	    (local-set-key (kbd "C-c C-M-b") 'org-metaleft)
	    (local-set-key (kbd "C-c C-1") 'org-time-stamp-inactive)
	    (local-set-key (kbd "C-c a t") 'org-todo-list)))
(eval-after-load "org"
  '(require 'ox-md nil t))

(setq org-babel-sh-command "/bin/bash")
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ocaml . t)
   (sh . t)
   (java . t)
   (emacs-lisp . t)))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Yes or no alias
(defalias 'yes-or-no-p 'y-or-n-p)

(defun copy-buffer-name ()
  (interactive)
  (kill-new (buffer-file-name)))

(global-set-key (kbd "C-c y f") 'copy-buffer-name)

;; Duplicate line
(defun duplicate-line()
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
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".rkt" ".ml" ".py" ".java" ".txt" ".emacs" ".php" ".js"))


;; Input unicode using tex with: M-x set-input-method RET tex RET

;; Coq
(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-subdirs-to-load-path))
(defun load-proof ()
  (interactive)
  (load-library "proof"))
;;(load-proof)
(setq proof-splash-seen t)
(setq proof-three-window-mode-policy 'hybrid)
(setq proof-script-fly-past-comments t)
(require 'proof-site)
(add-hook 'coq-mode-hook #'company-coq-initialize)
(add-hook 'coq-mode-hook (lambda () (abbrev-mode -1)))

;; Jedi mode
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)                      ; optional
(setq jedi:complete-on-dot t)                 ; optional
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:server-command
	  (list "python" "/Users/milo/.emacs.d/elpa/jedi-0.1.2/jediepcserver.py"))
;; This stops the stupid log messages from jedi mode
;; (jedi:install-server )

(defun add-python-shebang ()
  (interactive)
  (goto-char 0)
  (insert "#!/usr/bin/env python3\n"))


(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(defun my-flymd-browser-function (url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
           nil
           "/usr/bin/open"
           (list "-a" "firefox" url))))
(setq flymd-browser-open-function 'my-flymd-browser-function)


;; Tramp mode
(setq tramp-default-method "ssh")

(setq ring-bell-function 'ignore)

(defalias 'sir 'string-insert-rectangle)
(defalias 'org-insert-timestamp-inactive 'org-time-stamp-inactive)

;; Markdown
(add-hook 'markdown-mode 'markdown-preview-mode)
(add-hook 'markdown-mode 'markdown-preview-open-browser)

;; Enabled commands
(put 'set-goal-column 'disabled nil)
(put 'set-mark-command-repeat-pop 'disabled nil)

;; REPLs
(global-set-key (kbd "C-c r o") 'run-ocaml)
(global-set-key (kbd "C-c r r") 'racket-repl)
(global-set-key (kbd "C-c r p") 'run-python)

(set-face-attribute 'default nil :height 100)

;; Global modes
(define-globalized-minor-mode global-wrap-region-mode wrap-region-mode
  (lambda () (wrap-region-mode 1)))
(require 'diff-hl)
(define-globalized-minor-mode global-diff-hl-mode diff-hl-mode
  (lambda () (diff-hl-mode 1)))
(global-linum-mode t)
(delete-selection-mode 1)
(show-paren-mode 1)
(require 'auto-complete)
(global-auto-complete-mode t)
(column-number-mode)
(global-diff-hl-mode t)

(require 'color-theme)
(cond ((window-system )
	   (load-theme 'leuven)	; Only use theme in GUI emacs
	   )
	  (t nil))
(add-hook 'after-init-hook #'global-flycheck-mode)
;;(if (display-graphic-p)
;;     (ecb-activate ))
(ido-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
(provide '.emacs)
;;; .emacs ends here

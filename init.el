
;; Meta

;;    Emacs can only load =.el=-files. We can use =C-c C-v t= to run
;;    =org-babel-tangle=, which extracts the code blocks from the current file
;;    into a source-specific file (in this case a =.el=-file).

;;    To avoid doing this each time a change is made we can add a function to
;;    the =after-save-hook= ensuring to always tangle and byte-compile the
;;    =org=-document after changes.

(defun tangle-init ()
   "If the current buffer is 'init.org' the code-blocks are
 tangled, and the tangled file is compiled."
   (when (equal (buffer-file-name)
                "/home/austin/.dotfiles/emacs.d/init.org")
     ;; Avoid running hooks when tangling.
     (let ((prog-mode-hook nil))
       (org-babel-tangle)
       (byte-compile-file (concat user-emacs-directory "init.el")))))

 (add-hook 'after-save-hook 'tangle-init)

;; I'm adding code blocks to this file all the time. Org-mode provides a few
;;    [[http://orgmode.org/manual/Easy-Templates.html][structure templates]] for quickly adding new blocks, but I can make it even
;;    better. Everything in here is Emacs lisp, so let's alter the source code
;;    template a bit when I'm in this file.

(defun ow/init-org-elisp-template ()
    (when (equal (buffer-file-name)
                 "/home/oliver/.dotfiles/emacs.d/init.org")
      (setq-local org-structure-template-alist
                  '(("s" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC" "<src lang="emacs lisp">\n?\n</src>")))))

(add-hook 'org-mode-hook 'ow/init-org-elisp-template)


;; Package

;;    Managing extensions for Emacs is simplified using =package= which is
;;    built in to Emacs 24 and newer. To load downloaded packages we need to
;;    initialize =package=. =cl= is a library that contains many functions from
;;    Common Lisp, and comes in handy quite often, so we want to make sure it's
;;    loaded, along with =package=, which is obviously needed.

(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Packages can be fetched from different mirrors, [[http://melpa.org][melpa]] is the largest
;;    archive and is well maintained.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("MELPA" . "http://melpa.org/packages/")))

;; Keep a list of packages we want to have installed.
;;    if you move your config around this will keep everything in sync, otherwise
;;    M-x list-packages and C-s to search through it    also, can you help me get
;;    my screens to stay in this extended set up. you

(defvar required-packages
       '(ac-octave           ; Auto-completion for octave
         auto-compile        ; Automatically compile Emacs Lisp libraries
         cider               ; Clojure repl
         clojure-mode        ; Mode for .clj files
         csharp-mode         ; Mode for C# files
         company             ; Auto-completion engine
         diminish            ; Clean up the status line a but
         evil                ; Vi and Emacs, in harmony
         evil-leader         ; Bring back the leader key
         evil-nerd-commenter ; Quickly comment out lines
         evil-surround       ; Tim Pope's vim plugin to surround objects
         expand-region       ; Increase selected region by semantic units
         flx-ido             ; flx integration for ido
         flycheck            ; On-the-fly syntax checking
         helm                ; Super powerful completion tool
         helm-gtags          ; Use gtags for semantic completion
         helm-projectile     ; Projectile as a helm completion source
         idle-require        ; load elisp libraries while Emacs is idle
         ido-vertical-mode   ; Makes ido-mode display vertically.
         jedi                ; Python auto-completion for Emacs
         key-chord           ; Run commands with multiple key strokes (Helpful for Evil)
         magit               ; Git integration for Emacs
         markdown-mode       ; Emacs Major mode for Markdown-formatted files.
         move-text           ; Move current line or region with M-up or M-down
         multi-term          ; Better terminals
         multiple-cursors    ; Multiple cursors for Emacs.
         org-trello
         paredit             ; minor mode for editing parentheses
         powerline           ; Rewrite of Powerline
         projectile          ; Easy navigation for files in a project
         slime               ; Superior Lisp Interaction Mode for Emacs
         solarized-theme     ; Great color theme
         smex                ; M-x interface with Ido-style fuzzy matching.
         undo-tree           ; Treat undo history as a tree
         yasnippet           ; Snippet engine
         zenburn-theme       ; Nice looking low-contrast theme
         )
      "Packages which should be installed upon launch")

;; We can define a predicate that tells us whether or not the newest version
;;    of a package is installed.

(defun newest-package-installed-p (package)
  "Return true if the newest available PACKAGE is installed."
  (when (package-installed-p package)
    (let* ((local-pkg-desc (or (assq package package-alist)
                               (assq package package--builtins)))
           (newest-pkg-desc (assq package package-archive-contents)))
      (and local-pkg-desc newest-pkg-desc
           (version-list-= (package-desc-vers (cdr local-pkg-desc))
                           (package-desc-vers (cdr newest-pkg-desc)))))))

;; Let's write a function to install a package if it is not installed or
;;    upgrades it if a new version has been released. Here our predicate comes
;;    in handy.

(defun upgrade-or-install-package (package)
  "Unless the newest available version of PACKAGE is installed
PACKAGE is installed and the current version is deleted."
  (unless (newest-package-installed-p package)
    (let ((get-desc (if (version< emacs-version "24.4") 'cdr 'cadr))
          (pkg-desc (assq package package-alist)))
      (when pkg-desc
        (if (version< emacs-version "24.4")
            (package-delete (symbol-name package)
                            (package-version-join
                             (package-desc-vers (get-desc pkg-desc))))
          (package-delete pkg-desc)))
      (and (assq package package-archive-contents)
           (package-install package)))))

;; Also, we will need a function to find all dependencies from a given package.

(defun dependencies (package)
  "Returns a list of dependencies from a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
         (reqs (and pkg-desc (package-desc-reqs (cdr pkg-desc)))))
    (mapcar 'car reqs)))

;; The =package-refresh-contents= function downloads archive descriptions,
;;    this is a major bottleneck in this configuration. To avoid this we can
;;    try to only check for updates once every day or so. Here are three
;;    variables. The first specifies how often we should check for updates. The
;;    second specifies whether one should update during the initialization. The
;;    third is a path to a file where a time-stamp is stored in order to check
;;    when packages were updated last.

(defvar days-between-updates 7)
(defvar do-package-update-on-init t)
(defvar package-last-update-file
  (expand-file-name (concat user-emacs-directory ".package-last-update")))

;; The tricky part is figuring out when packages were last updated. Here is
;;    a hacky way of doing it, using [[http://www.gnu.org/software/emacs/manual/html_node/emacs/Time-Stamps.html][time-stamps]]. By adding a time-stamp to the
;;    a file, we can determine whether or not to do an update. After that we
;;    must run the =time-stamp=-function to update the time-stamp.

(require 'time-stamp)
;; Open the package-last-update-file
(with-temp-file package-last-update-file
  (if (file-exists-p package-last-update-file)
      (progn
        ;; Insert it's original content's.
        (insert-file-contents package-last-update-file)
        (let ((start (re-search-forward time-stamp-start nil t))
              (end (re-search-forward time-stamp-end nil t)))
          (when (and start end)
            ;; Assuming we have found a time-stamp, we check determine if it's
            ;; time to update.
            (setq do-package-update-on-init
                  (<= days-between-updates
                      (days-between
                       (current-time-string)
                       (buffer-substring-no-properties start end))))
            ;; Remember to update the time-stamp.
            (when do-package-update-on-init
              (time-stamp)))))
    ;; If no such file exists it is created with a time-stamp.
    (insert "Time-stamp: <>")
    (time-stamp)))

(when (and do-package-update-on-init
           (y-or-n-p "Update all packages?"))
  (package-refresh-contents)

  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Sane defaults

;;    These are what /I/ consider to be saner defaults.

;;    We can set variables to whatever value we'd like using =setq=.

(setq default-input-method "TeX"    ; Use TeX when toggling input method.
      doc-view-continuous t         ; At page edge goto next/previous.
      echo-keystrokes 0.1           ; Show keystrokes asap.
      inhibit-startup-message t     ; No splash screen please.
      initial-scratch-message nil   ; Clean scratch buffer.
      electric-pair-mode 1          ; Insert brackets, parentheses in pairs
      ring-bell-function 'ignore    ; Quiet.
      byte-compile-warnings nil     ; Don't show warnings when compiling elisp
      require-final-newline t       ; End files with \n
      ;; Save undo history between sessions, if you have an undo-dir
      undo-tree-auto-save-history
      (file-exists-p
       (concat user-emacs-directory "undo"))
      undo-tree-history-directory-alist
      ;; Put undo-history files in a directory, if it exists.
      (let ((undo-dir (concat user-emacs-directory "undo")))
        (and (file-exists-p undo-dir)
             (list (cons "." undo-dir)))))

;; Some variables are buffer-local, so changing them using =setq= will only
;;    change them in a single buffer. Using =setq-default= we change the
;;    buffer-local variable's default value.

(setq-default fill-column 80                    ; Maximum line width.
              indent-tabs-mode nil              ; Use spaces instead of tabs.
              split-width-threshold 100         ; Split verticly by default.
              compilation-scroll-output 1       ; Follow compilation buffer
              compilation-ask-about-save nil    ; Automatically save when compiling
              auto-fill-function 'do-auto-fill  ; Auto-fill-mode everywhere.
              indent-tabs-mode nil)             ; don't let spaces convert to tab

;; Answering /yes/ and /no/ to each question from Emacs can be tedious, a
;;    single /y/ or /n/ will suffice.

(fset 'yes-or-no-p 'y-or-n-p)

;; To avoid file system clutter we put all auto saved files in a single
;;    directory.

(defvar emacs-autosave-directory
  (concat user-emacs-directory "autosaves/")
  "This variable dictates where to put auto saves. It is set to a
  directory called autosaves located wherever your .emacs.d/ is
  located.")

;; Sets all files to be backed up and auto saved in a single directory.
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))

;; The scratch buffer is a useful place to test out bits of elisp or store some
;;    text temporarily. It would be nice if it was persistent, though. The
;;    following code will save the buffer every 5 minutes, and reload it on startup.

(defun save-persistent-scratch ()
  "Save the contents of *scratch*"
       (with-current-buffer (get-buffer-create "*scratch*")
         (write-region (point-min) (point-max)
                       (concat user-emacs-directory "scratch"))))

(defun load-persistent-scratch ()
  "Reload the scratch buffer"
  (let ((scratch-file (concat user-emacs-directory "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file)))))

(add-hook 'emacs-startup-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)

(run-with-idle-timer 300 t 'save-persistent-scratch)

;; Set =utf-8= as preferred coding system.

(set-language-environment "UTF-8")

;; By default the =narrow-to-region= command is disabled and issues a
;;    warning, because it might confuse new users. I find it useful sometimes,
;;    and don't want to be warned.

(put 'narrow-to-region 'disabled nil)

;; Call =auto-complete= default configuration, which enables =auto-complete=
;;    globally.

(eval-after-load 'auto-complete-config `(ac-config-default))

;; Automaticly revert =doc-view=-buffers when the file changes on disk.

(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(when (fboundp 'windmove-default-keybindings)
   (windmove-default-keybindings))

;; Visual

;;    Change the color-theme to =solarized=.

(setq solarized-scale-org-headlines nil)
(load-theme 'solarized-dark t)

;; Use the [[http://www.levien.com/type/myfonts/inconsolata.html][Inconsolata]] font if it's installed on the system.

(when (member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-12"))

;; When interactively changing the theme (using =M-x load-theme=), the
;;    current custom theme is not disabled. This often gives weird-looking
;;    results; we can advice =load-theme= to always disable themes currently
;;    enabled themes.

(defadvice load-theme
  (before disable-before-load (theme &optional no-confirm no-enable) activate)
  (mapc 'disable-theme custom-enabled-themes))

;; I like how Vim shows you empty lines using tildes. Emacs can do something
;;    similar with the variable =indicate-empty-lines=, but I'll make it look a bit
;;    more familiar. ([[http://www.reddit.com/r/emacs/comments/2kdztw/emacs_in_evil_mode_show_tildes_for_blank_lines/][Source]])

(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)
(set-fringe-bitmap-face 'tilde 'font-lock-function-name-face)

;; Modes

;;    There are some modes that are enabled by default that I don't find
;;    particularly useful. We create a list of these modes, and disable all of
;;    these.

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           menu-bar-mode                ; No menu bar
           scroll-bar-mode              ; No scroll bars either.
           blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

;; Let's apply the same technique for enabling modes that are disabled by
;;    default.

(dolist (mode
         '(column-number-mode         ; Show column number in mode line.
           delete-selection-mode      ; Replace selected text.
           dirtrack-mode              ; directory tracking in *shell*
           recentf-mode               ; Recently opened files.
           show-paren-mode))          ; Highlight matching parentheses.
  (funcall mode 1))

(when (version< emacs-version "24.4")
  (eval-after-load 'auto-compile
    '((auto-compile-on-save-mode 1))))  ; compile .el files on save.

;; This makes =.md=-files open in =markdown-mode=.

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; We want to have autocompletion by default. Load company mode everywhere.

(add-hook 'after-init-hook 'global-company-mode)
;; (diminish 'company-mode)
 (setq company-idle-delay 0)

;; Helm

;;    Helm is an amazing completion tool for finding almost anything. We can
;;    replace many default functions with the helm equivalent.

(eval-after-load 'helm
  '(progn
     (global-set-key (kbd "M-y") 'helm-show-kill-ring)
     (global-set-key (kbd "C-x b") 'helm-mini)
     (global-set-key (kbd "C-x C-f") 'helm-find-files)
     (global-set-key (kbd "M-x") 'helm-M-x)
     (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
     (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

     (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)))

;;(diminish 'helm-mode)

(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-M-x-fuzzy-match                  t ; fuzzy matching M-x
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-recentf-fuzzy-match              t ; fuzzy matching recent files
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Helm-gtags

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; Key bindings

;;   Inspired by [[http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs][this StackOverflow post]] I keep a =custom-bindings-map= that
;;   holds all my custom bindings. This map can be activated by toggling a
;;   simple =minor-mode= that does nothing more than activating the map. This
;;   inhibits other =major-modes= to override these bindings. I keep this at
;;   the end of the init-file to make sure that all functions are actually
;;   defined.

(defvar custom-bindings-map (make-keymap)
  "A keymap for custom bindings.")

;; Binding to use Ibuffer over BufferMenu

(define-key custom-bindings-map (kbd "C-x C-b")  'ibuffer)

;; Bindings for [[https://github.com/magnars/expand-region.el][expand-region]].

(define-key custom-bindings-map (kbd "C-'") 'er/expand-region)
(define-key custom-bindings-map (kbd "C-;") 'er/contract-region)

;; Bindings for multi-term

(define-key custom-bindings-map (kbd "C-c t") 'multi-term-dedicated-toggle)
(define-key custom-bindings-map (kbd "C-c T") 'get-term)

;; Bindings for [[http://magit.github.io][Magit]].

(define-key custom-bindings-map (kbd "C-c m") 'magit-status)

;; Bindings for [[http://emacs-helm.github.io/helm/][Helm]].

(define-key custom-bindings-map (kbd "C-c h g") 'helm-google-suggest)

;; Bind some native Emacs functions.

(define-key custom-bindings-map (kbd "C-x p") 'proced)
(define-key custom-bindings-map (kbd "C-c r") 'rename-buffer)
(define-key custom-bindings-map (kbd "C-c s") 'ispell-word)
(define-key custom-bindings-map (kbd "C-c a") 'org-agenda-list)

;; Lastly we need to activate the map by creating and activating the
;;   =minor-mode=.

(define-minor-mode custom-bindings-mode
  "A mode that activates custom-bindings."
  t nil custom-bindings-map)

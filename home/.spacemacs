;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     ;; Git layer does not load on Emacs 24.3.
     ;; https://github.com/syl20bnr/spacemacs/issues/2272
     ;; (git :variables
     ;;      git-gutter-use-fringe t)
     html
     javascript
     ;; markdown
     ;; org
     python
     ;; shell
     ;; sql
     sql-rs
     ;; syntax-checking
     )
   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   ;;
   ;; Disabling anaconda-mode, company-anaconda, and eldoc for now (impacts the
   ;; Python layer). With a fresh install of spacemacs 0.104.2 on 2015-10-11,
   ;; anaconda-mode seems to install, but I seem to be running into the
   ;; following disruptive bug:
   ;; https://github.com/proofit404/anaconda-mode/issues/124
   dotspacemacs-excluded-packages '(anaconda-mode company-anaconda eldoc)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq temporary-file-directory "~/tmp/emacs")

  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq spacemacs-indent-sensitive-modes '(python-mode sql-mode))

  ;; Bind C-c r and C-c C-r to replace-regexp
  (global-set-key (kbd "C-c r") 'replace-regexp)
  (global-set-key (kbd "C-c C-r") 'replace-regexp)

  ;; Use spaces instead of tabs (use C-q C-i to override)
  (setq-default indent-tabs-mode nil)

  ;; Show trailing whitespace by defalt
  ;; See http://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
  (setq-default show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)

  ;; Show matching parents
  (show-paren-mode 1)

  ;; Disable activating the region for C-x C-x.
  (defun exchange-point-and-mark-no-activate ()
    "Identical to \\[exchange-point-and-mark] but will not activate the region."
    (interactive)
    (exchange-point-and-mark)
    (deactivate-mark nil))
  (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

  ;; Define revert-all-buffers command (useful after some Git commands).
  (defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files."))

  ;; Transpose windows command
  ;; See http://www.emacswiki.org/emacs-en/TransposeWindows

  (defun transpose-windows (arg)
    "Transpose the buffers shown in two windows."
    (interactive "p")
    (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
      (while (/= arg 0)
        (let ((this-win (window-buffer))
              (next-win (window-buffer (funcall selector))))
          (set-window-buffer (selected-window) next-win)
          (set-window-buffer (funcall selector) this-win)
          (select-window (funcall selector)))
        (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

  (define-key ctl-x-4-map (kbd "t") 'transpose-windows)

  ;; Fix sql-mode indentation.
  ;; See https://gist.github.com/stuntgoat/8912558

  (defun get-previous-indentation ()
    "Get the column of the previous indented line"
    (interactive)
    (save-excursion
      (progn
        (move-beginning-of-line nil)
        (skip-chars-backward "\n \t")
        (back-to-indentation))
      (current-column)))

  (defun get-current-indentation ()
    "Return column at current indentation"
    (interactive)
    (save-excursion
      (progn
        (back-to-indentation)
        (current-column))))

  (defun point-at-current-indentation ()
    "Return point at current indentation"
    (interactive)
    (save-excursion
      (progn
        (move-to-column (get-current-indentation))
        (point))))

  (defun point-at-column-on-line (col)
    "Returns the point at `col` on the current line"
    (interactive)
    (save-excursion
      (progn
        (move-to-column col)
        (point))))

  (defun ig-move-line-to-column (col)
    "Move the line to col; fill with all spaces if moving forward"
    (interactive "p")
    (let ((point-at-cur-indent (point-at-current-indentation))
          (col-at-cur-indent (get-current-indentation)))
      (cond (
             (= col 0)
             ;; delete to beginning of line or do nothing
             (if (= col-at-cur-indent 0)
                 nil
               (delete-region point-at-cur-indent (point-at-column-on-line 0))))
            (
             (< col col-at-cur-indent)
             ;; delete from our current point BACK to col
             (delete-region (point-at-column-on-line col) point-at-cur-indent))
            (
             (> col col-at-cur-indent)
             ;; delete all text from indent to beginning of line
             (progn
               (delete-region point-at-cur-indent (point-at-column-on-line 0))
               (move-beginning-of-line nil)
               ;; add spaces forward
               (insert-string (make-string col ?\s)))))))

  (defun ig-indent-sql ()
    "Indent by `tab-width` at most 1 time greater than the previously indented line otherwise go to the beginning of the line indent forward by `tab-width`"
    (let ((previous (get-previous-indentation))
          (current (get-current-indentation)))
      (cond ( ;; exactly at previous line's indentation
             (= previous current)
             (ig-move-line-to-column (+ current tab-width)))

            ( ;; current is greater than previous
             (> current previous)
             ;; exactly at one indentation forward from previous lines indent
             (if (= tab-width (- current previous))
                 ;; move line to beginning
                 (ig-move-line-to-column 0)
               ;; go back to previous indentation level
               (ig-move-line-to-column previous)))

            (t
             (ig-move-line-to-column (+ current tab-width))))))

  (add-hook 'sql-mode-hook
            (lambda ()
                (make-local-variable 'indent-line-function)
                (setq indent-line-function 'ig-indent-sql
                      tab-width 4
                      tab-stop-list (4, 8, 12, 16, 20, 24, 28, 32, 36, 40,
                                        44, 48, 52, 56, 60, 64, 68, 72, 76,
                                        80, 84, 88, 92, 96, 100, 104, 108, 112,
                                        116, 120, 124, 128, 132, 136, 140, 144,
                                        148, 152, 156, 160, 164, 168, 172, 176,
                                        180, 184, 188, 192, 196))
                ))

  ;; Disable electric-indent mode globally while we are on Emacs 24.3.
  ;; In Emacs 24.4, we can use electric-indent-local-mode instead.
  (electric-indent-mode -1)
  (add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

; Load Emacs package repositories; see http://www.emacswiki.org/emacs/ELPA
; This depends on emacs 24.
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; Put emacs backup files in a central location, not all over the source tree.
; See http://www.emacswiki.org/emacs/BackupDirectory
; Note that this directory must be created manually first.
(setq temporary-file-directory "~/tmp/emacs")

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Disable the menu bar.
(menu-bar-mode -1)

; Show the column number in the status line.
(column-number-mode)

; Set default directory
(setq default-directory "~/")

; Enable ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

; Bind C-c r and C-c C-r to replace-regexp
(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c C-r") 'replace-regexp)

; Use spaces instead of tabs (use C-q C-i to override)
(setq-default indent-tabs-mode nil)

; Show trailing whitespace by defalt
; See http://www.gnu.org/software/emacs/manual/html_node/emacs/Useless-Whitespace.html
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)

; Show matching parents
(show-paren-mode 1)

; Use js2-mode for .js files. Depends on js2-mode being installed.
; See http://www.emacswiki.org/emacs/Js2Mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

; Set indent level to 4 spaces for HTML code.
; See http://www.emacswiki.org/emacs/IndentingHtml
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))

; Fix mark commands.
; See http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/

; The following two key bindings do not seem to work on my MacBook.
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-,") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

; Disable activating the region for C-x C-x.
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

; Define revert-all-buffers command (useful after some Git commands).
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))


; Transpose windows command
; See http://www.emacswiki.org/emacs-en/TransposeWindows

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

; Fix sql-mode indentation.
; See https://gist.github.com/stuntgoat/8912558

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
          (function (lambda ()
                      (make-local-variable 'indent-line-function)
                      (setq indent-line-function 'ig-indent-sql)
                      (setq tab-width 4)
                      (setq tab-stop-list (4, 8, 12, 16, 20, 24, 28, 32, 36, 40,
                                              44, 48, 52, 56, 60, 64, 68, 72, 76,
                                              80, 84, 88, 92, 96, 100, 104, 108, 112,
                                              116, 120, 124, 128, 132, 136, 140, 144,
                                              148, 152, 156, 160, 164, 168, 172, 176,
                                              180, 184, 188, 192, 196))
                      )))

; Load .R files in R mode.
(autoload 'R-mode "ess-site.el" "" t)
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))

;; EVERYTHING BELOW THIS LINE IS COMMENTED OUT.

; Enable auto-fill-mode for text files.
;(add-hook 'text-mode-hook 'auto-fill-mode)

; (message "Deleting old backup files...")
; (let ((week (* 60 60 24 7))
;       (current (float-time (current-time))))
;   (dolist (file (directory-files temporary-file-directory t))
;     (when (and (backup-file-name-p file)
;                (> (- current (float-time (fifth (file-attributes file))))
;                   week))
;       (message "%s" file)
;       (delete-file file))))

; Enable fci-mode.
; See http://www.emacswiki.org/emacs/FillColumnIndicator
; The package needs to be installed first before this will work.
;(require 'fill-column-indicator)
;(add-hook 'text-mode-hook 'fci-mode)
;(add-hook 'javascript-mode-hook 'fci-mode)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

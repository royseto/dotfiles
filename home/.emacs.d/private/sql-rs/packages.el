;;; packages.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Brian Hicks & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar sql-rs-packages '(sql))

(defvar sql-rs-excluded-packages '())

(defun sql-rs/init-sql-mode ()
  (use-package sql
    :defer t
    :config
    (progn
      (setq spacemacs-sql-highlightable sql-product-alist
            spacemacs-sql-startable (remove-if-not
                                (lambda (product) (sql-get-product-feature (car product) :sqli-program))
                                sql-product-alist)

            ;; should not set this to anything else than nil
            ;; the focus of SQLi is handled by spacemacs conventions
            sql-pop-to-buffer-after-send-region nil)

      (defun spacemacs//sql-source (products)
        "return a source for helm selection"
        `((name . "SQL Products")
          (candidates . ,(mapcar (lambda (product)
                                   (cons (sql-get-product-feature (car product) :name)
                                         (car product)))
                                 products))
          (action . (lambda (candidate) (helm-marked-candidates)))))

      (defun spacemacs/sql-highlight ()
        "set SQL dialect-specific highlighting"
        (interactive)
        (let ((product (car (helm
                             :sources (list (spacemacs//sql-source spacemacs-sql-highlightable))))))
          (sql-set-product product)))

      (defun spacemacs/sql-start ()
        "set SQL dialect-specific highlighting and start inferior SQLi process"
        (interactive)
        (let ((product (car (helm
                             :sources (list (spacemacs//sql-source spacemacs-sql-startable))))))
          (sql-set-product product)
          (sql-product-interactive product)))

      (defun spacemacs/sql-send-string-and-focus ()
        "Send a string to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (call-interactively 'sql-send-string)
          (evil-insert-state)))

      (defun spacemacs/sql-send-buffer-and-focus ()
        "Send the buffer to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-buffer)
          (evil-insert-state)))

      (defun spacemacs/sql-send-paragraph-and-focus ()
        "Send the paragraph to SQLi and switch to SQLi in `insert state'."
        (interactive)
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-paragraph)
          (evil-insert-state)))

      (defun spacemacs/sql-send-region-and-focus (start end)
        "Send region to SQLi and switch to SQLi in `insert state'."
        (interactive "r")
        (let ((sql-pop-to-buffer-after-send-region t))
          (sql-send-region start end)
          (evil-insert-state)))

      (evil-leader/set-key-for-mode 'sql-mode
        ;; sqli buffer
        "mbb" 'sql-show-sqli-buffer
        "mbs" 'sql-set-sqli-buffer

        ;; dialects
        "mhk" 'spacemacs/sql-highlight

        ;; interactivity
        "msb" 'sql-send-buffer
        "msB" 'spacemacs/sql-send-buffer-and-focus
        "msi" 'spacemacs/sql-start
        ;; paragraph gets "f" here because they can be assimilated to functions.
        ;; If you separate your commands in a SQL file, this key will send the
        ;; command under the point, which is what you probably want.
        "msf" 'sql-send-paragraph
        "msF" 'spacemacs/sql-send-paragraph-and-focus
        "msq" 'sql-send-string
        "msQ" 'spacemacs/sql-send-string-and-focus
        "msr" 'sql-send-region
        "msR" 'spacemacs/sql-send-region-and-focus

        ;; listing
        "mla" 'sql-list-all
        "mlt" 'sql-list-table)

      (evil-leader/set-key-for-mode 'sql-interactive-mode
        ;; sqli buffer
        "mbr" 'sql-rename-buffer
        "mbS" 'sql-save-connection)

      (add-hook 'sql-interactive-mode-hook
                (lambda () (toggle-truncate-lines t)))

      ;; ;; Fix sql-mode indentation.
      ;; ;; See https://gist.github.com/stuntgoat/8912558

      ;; (defun get-previous-indentation ()
      ;;   "Get the column of the previous indented line"
      ;;   (interactive)
      ;;   (save-excursion
      ;;     (progn
      ;;       (move-beginning-of-line nil)
      ;;       (skip-chars-backward "\n \t")
      ;;       (back-to-indentation))
      ;;     (current-column)))

      ;; (defun get-current-indentation ()
      ;;   "Return column at current indentation"
      ;;   (interactive)
      ;;   (save-excursion
      ;;     (progn
      ;;       (back-to-indentation)
      ;;       (current-column))))

      ;; (defun point-at-current-indentation ()
      ;;   "Return point at current indentation"
      ;;   (interactive)
      ;;   (save-excursion
      ;;     (progn
      ;;       (move-to-column (get-current-indentation))
      ;;       (point))))

      ;; (defun point-at-column-on-line (col)
      ;;   "Returns the point at `col` on the current line"
      ;;   (interactive)
      ;;   (save-excursion
      ;;     (progn
      ;;       (move-to-column col)
      ;;       (point))))

      ;; (defun ig-move-line-to-column (col)
      ;;   "Move the line to col; fill with all spaces if moving forward"
      ;;   (interactive "p")
      ;;   (let ((point-at-cur-indent (point-at-current-indentation))
      ;;         (col-at-cur-indent (get-current-indentation)))
      ;;     (cond (
      ;;            (= col 0)
      ;;            ;; delete to beginning of line or do nothing
      ;;            (if (= col-at-cur-indent 0)
      ;;                nil
      ;;              (delete-region point-at-cur-indent (point-at-column-on-line 0))))
      ;;           (
      ;;            (< col col-at-cur-indent)
      ;;            ;; delete from our current point BACK to col
      ;;            (delete-region (point-at-column-on-line col) point-at-cur-indent))
      ;;           (
      ;;            (> col col-at-cur-indent)
      ;;            ;; delete all text from indent to beginning of line
      ;;            (progn
      ;;              (delete-region point-at-cur-indent (point-at-column-on-line 0))
      ;;              (move-beginning-of-line nil)
      ;;              ;; add spaces forward
      ;;              (insert-string (make-string col ?\s)))))))

      ;; (defun ig-indent-sql ()
      ;;   "Indent by `tab-width` at most 1 time greater than the previously indented line otherwise go to the beginning of the line indent forward by `tab-width`"
      ;;   (let ((previous (get-previous-indentation))
      ;;         (current (get-current-indentation)))
      ;;     (cond ( ;; exactly at previous line's indentation
      ;;            (= previous current)
      ;;            (ig-move-line-to-column (+ current tab-width)))

      ;;           ( ;; current is greater than previous
      ;;            (> current previous)
      ;;            ;; exactly at one indentation forward from previous lines indent
      ;;            (if (= tab-width (- current previous))
      ;;                ;; move line to beginning
      ;;                (ig-move-line-to-column 0)
      ;;              ;; go back to previous indentation level
      ;;              (ig-move-line-to-column previous)))

      ;;           (t
      ;;            (ig-move-line-to-column (+ current tab-width))))))

      ;; (add-hook 'sql-mode-hook
      ;;           '(lambda ()
      ;;              (setq indent-line-function 'ig-indent-sql
      ;;                    tab-width 4
      ;;                    tab-stop-list (4, 8, 12, 16, 20, 24, 28, 32, 36, 40,
      ;;                                      44, 48, 52, 56, 60, 64, 68, 72, 76,
      ;;                                      80, 84, 88, 92, 96, 100, 104, 108, 112,
      ;;                                      116, 120, 124, 128, 132, 136, 140, 144,
      ;;                                      148, 152, 156, 160, 164, 168, 172, 176,
      ;;                                      180, 184, 188, 192, 196))
      ;;              ))

      )))

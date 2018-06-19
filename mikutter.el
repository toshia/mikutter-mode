;;; mikutter.el --- mikutter開発用のマイナーモード

;; Copyright (C) 2012  Toshiaki Asai @toshi_a

;; Author: Toshiaki Asai <toshi.alternative@gmail.com> @toshi_a
;; Keywords: convenience, files
;; Package-Requires: ((cl-lib "0.5"), (yasnippets "0.8.0"), (ruby-mode "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'mikutter-utils)
(require 'onthefly-executer)

(defcustom mikutter:dir nil "開発用 mikutter.rb のあるディレクトリ")
(defcustom mikutter:confroot "~/.mikutter/" "デバッグ用 confroot")
(defvar mikutter:process nil "起動中のmikutterのプロセス")

(easy-mmode-define-minor-mode mikutter-mode
  "mikutterコア・プラグイン開発用モード"
  nil
  " Mikutter"
  '(("\C-c\C-c" . onthefly-executer-current-buffer)
	("\C-c\C-e" . onthefly-executer-within-current-plugin)))

;; mikutter mode自動で有効
(add-hook 'ruby-mode-hook
          #'(lambda ()
              (if (and buffer-file-name (string-match "mikutter" buffer-file-name))
                  (mikutter-mode))))

;; imenu

(defun ruby-imenu-create-index-in-block (prefix beg end)
  (let ((index-alist '()) (case-fold-search nil)
        name next pos decl sing)
    (goto-char beg)
    (while (re-search-forward "^\\s *\\(\\(class\\>\\(\\s *<<\\)?\\|module\\>\\)\\s *\\([^\(<\n ]+\\)\\|\\(def\\|alias\\)\\>\\s *\\([^\(\n ]+\\)\\|Plugin.create[ (]:\\([^)]*+\\)[ )]\\|\\(on\\|filter\\|hook\\)_?\\([a-zA-Z0-9_]+\\)\\)" end t)
      (setq sing (match-beginning 3))
      (setq decl (or (match-string 5) (match-string 8)))
      (setq next (match-end 0))
      (setq name (or (match-string 4) (match-string 6) (match-string 7) (match-string 9)))
      (setq pos (match-beginning 0))
      (message decl)
      (cond
       ((or (string= "on" decl) (string= "filter" decl) (string= "hook" decl))
        (if prefix (setq name (concat prefix name)))
        (push (cons (concat decl " " name) pos) index-alist))
       ((string= "alias" decl)
        (if prefix (setq name (concat prefix name)))
        (push (cons name pos) index-alist))
       ((string= "def" decl)
        (if prefix
            (setq name
                  (cond
                   ((string-match "^self\." name)
                    (concat (substring prefix 0 -1) (substring name 4)))
                   (t (concat prefix name)))))
        (push (cons name pos) index-alist)
        (ruby-accurate-end-of-block end))
       (t
        (message (concat name (if sing "." "#")))
        (if (string= "self" name)
            (if prefix (setq name (substring prefix 0 -1)))
          (if prefix (setq name (concat (substring prefix 0 -1) "::" name)))
          (push (cons name pos) index-alist))
        (ruby-accurate-end-of-block end)
        (setq beg (point))
        (setq index-alist
              (nconc (ruby-imenu-create-index-in-block
                      (concat name (if sing "." "#"))
                      next beg) index-alist))
        (goto-char beg))))
    index-alist))

;; yasnippet

(add-hook 'mikutter-mode-hook
             #'(lambda ()
                 (add-to-list 'yas-extra-modes 'mikutter-mode)))

;; キー押したらmikutter起動する奴
(defun mikutter-boot (&optional arguments)
  (interactive)
  (when (and mikutter:process (eq 'run (process-status mikutter:process)))
    (delete-process mikutter:process))
  (let ((process-connection-type nil))
    (with-current-buffer (get-buffer-create "*mikutter-log*")
      (erase-buffer))
	(message (concat "mikutter: start process: ruby " mikutter:dir "mikutter.rb " (apply #'concat arguments)))
    (setq mikutter:process
		  (apply #'start-process `("mikutter-test-process"
								   "*mikutter-log*"
								   "ruby"
								   ,(concat mikutter:dir "mikutter.rb")
								   . ,(or arguments (list "--debug")))))))

(defun mikutter-boot-local (&optional arguments)
  (interactive)
  (when (and mikutter:process (eq 'run (process-status mikutter:process)))
    (delete-process mikutter:process))
  (let ((process-connection-type nil))
    (with-current-buffer (get-buffer-create "*mikutter-log*")
      (erase-buffer))
	(message (concat "mikutter: start process: ruby " mikutter:dir "mikutter.rb " (apply #'concat arguments)))
    (setq mikutter:process
		  (let ((process-environment (cons (concat "MIKUTTER_CONFROOT=" (mikutter-confroot-auto-select)) process-environment)))
			(apply #'start-process `("mikutter-test-process"
									 "*mikutter-log*"
									 "ruby"
									 ,(concat mikutter:dir "mikutter.rb")
									 . ,(or arguments (list "--debug"))))))))

(defun mikutter-confroot-auto-select ()
  (interactive)
  (let* ((branch-name (shell-command-to-string (concat "git --git-dir=" (file-name-as-directory mikutter:dir) ".git/ rev-parse --abbrev-ref HEAD")))
		 (sanitized (mikutter-branch-name-sanitize branch-name))
		 (dirname (if (string= "master" sanitized) (expand-file-name mikutter:confroot) (concat "/tmp/mikutter-confroots/" sanitized))))
	(when (not (file-accessible-directory-p dirname))
	  (make-directory dirname t)
	  (mikutter-confroot-mirror mikutter:confroot dirname))
	dirname))
(defun mikutter-branch-name-sanitize (branch-name)
  (replace-regexp-in-string "[\n\r]" "" branch-name))
(defmacro mikutter-confroot-copy (target)
  `(shell-command
	(concat "cp -r "
			(file-name-as-directory src) ,target " "
			(file-name-as-directory dest))))
(defun mikutter-confroot-mirror (src dest)
  (mikutter-confroot-copy "plugin/")
  (mikutter-confroot-copy "settings/"))

(provide 'mikutter)

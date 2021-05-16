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
(require 'cl-lib)

(defcustom mikutter:ruby "ruby -x" "mikutterを起動するのに使うrubyのパス")
(defcustom mikutter:dir nil "開発用 mikutter.rb のあるディレクトリ")
(defcustom mikutter:confroot "~/.mikutter/" "デバッグ用 confroot")
(defvar mikutter:process nil "起動中のmikutterのプロセス")
(defcustom mikutter:snippet-command-conditions
  '("true"
    "opt.messages.size == 1"
    "opt.messages.size == 1 && opt.messages.first.repliable?"
    "opt.messages.size == 1 && opt.widget.selected_text(opt.messages.first)"
    "opt.messages.all? &:repliable?"
    "opt.messages.all? { |m| m.retweetable? && !m.retweeted_by_me? }"
    "opt.messages.all? { |m| m.favoritable? && !m.favorited_by_me? }")
  "commandスニペットで、condition:引数に渡される無名関数の内容の選択候補")

(easy-mmode-define-minor-mode mikutter-mode
  "mikutterコア・プラグイン開発用モード"
  nil
  " Mikutter"
  '(("\C-c\C-c" . onthefly-executer-current-buffer)
    ("\C-c\C-e" . onthefly-executer-within-current-plugin)))

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

;; company
(defun company-advanced--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-advanced--candidates (prefix)
  (let (res)
    (dolist (item (mikutter:events))
      (when (string-prefix-p prefix item)
        (push (company-advanced--make-candidate (list item "event")) res)))
    res))

(defun company-advanced--meta (candidate)
  (format "This will use %s of %s"
          (get-text-property 0 'meta candidate)
          (substring-no-properties candidate)))

(defun company-advanced--annotation (candidate)
  (format " (%s)" (get-text-property 0 'meta candidate)))

(defun mikutter:company-event-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'mikutter:company-event-backend))
    (prefix (mikutter:company-grab-line-cond "Plugin\\.call(:\\(.*\\)"
                                             "filter_\\(.*\\)"
                                             "on_\\(.*\\)"
                                             "on\\(.*\\)"))
    (candidates (seq-filter (lambda (s) (string-prefix-p arg s))
                            (mikutter:events)))
    (annotation " (event)")
    (meta (company-advanced--meta arg))))

(eval-after-load "company"
  (add-hook 'mikutter-mode-hook
            #'(lambda ()
                (add-to-list 'company-backends 'mikutter:company-event-backend))))

;; yasnippet

(add-hook 'mikutter-mode-hook
             #'(lambda ()
                 (add-to-list 'yas-extra-modes 'mikutter-mode)))

;; キー押したらmikutter起動する奴
(defun mikutter-boot (&optional arguments)
  "mikutterをデバッグモードで起動する"
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
                                   ,@(split-string mikutter:ruby)
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
                                     ,@(split-string mikutter:ruby)
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

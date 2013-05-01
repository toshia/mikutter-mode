;;; mikutter.el --- mikutter開発用のマイナーモード

;; Copyright (C) 2012  Toshiaki Asai @toshi_a

;; Author: Toshiaki Asai <toshi.alternative@gmail.com> @toshi_a
;; Keywords: convenience, files

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
(defvar mikutter:process nil "起動中のmikutterのプロセス")

(easy-mmode-define-minor-mode mikutter-mode
  "mikutterコア・プラグイン開発用モード"
  nil
  " Mikutter"
  '(("\C-c\C-c" . onthefly-executer-current-buffer)))

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
    (while (re-search-forward "^\\s *\\(\\(class\\>\\(\\s *<<\\)?\\|module\\>\\)\\s *\\([^\(<\n ]+\\)\\|\\(def\\|alias\\)\\>\\s *\\([^\(\n ]+\\)\\|Plugin.create[ (]:\\(.*+\\)[ )]\\|\\(on\\|filter\\|hook\\)_?\\([a-zA-Z0-9_]+\\)\\)" end t)
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
                 (setq yas/mode-symbol 'mikutter-mode)))

;; キー押したらmikutter起動する奴
(defun mikutter-boot ()
  (interactive)
  (when (and mikutter:process (eq 'run (process-status mikutter:process)))
    (delete-process mikutter:process))
  (let ((process-connection-type nil))
    (with-current-buffer (get-buffer-create "*mikutter-log*")
      (erase-buffer))
    (setq mikutter:process
          (start-process "mikutter-test-process" "*mikutter-log*"
                         "ruby" (concat mikutter:dir "mikutter.rb") "--debug"))))

(provide 'mikutter)

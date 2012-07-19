;;; onthefly_executer.el --- mikutter上でコードをリアルタイム実行

;; Copyright (C) 2012  Toshiaki Asai

;; Author: Toshiaki Asai <toshi.alternative@gmail.com>
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
(require 'dbus)
(require 'mikutter-utils)
(defun onthefly-executer (ruby-code &optional file)
  "Execute ruby code on mikutter"
  (dbus-call-method
    :session
    "org.mikutter.dynamic"
    "/org/mikutter/MyInstance"
    "org.mikutter.eval"
    "ruby" `(:array
             (:struct "code" (:variant ,ruby-code))
             (:struct :string "file" (:variant ,(or file "org.mikutter.eval"))))))

(defun onthefly-executer-current-buffer ()
  (interactive)
  (let ((current-plugin (mikutter:current-plugin)))
    (when current-plugin
      (message (concat "mikutter: plugin \"" current-plugin "\" uninstall."))
      (onthefly-executer (concat "Plugin.uninstall(:" current-plugin ")")))
    (onthefly-executer (buffer-string) (buffer-file-name))
    (if current-plugin
        (message (concat "mikutter: plugin \"" current-plugin "\" installed"))
      (message "mikutter: executed"))))

(defun mikutter:make-console-buffer ()
  (with-current-buffer (get-buffer-create "*mikutter-console*")
    (ruby-mode)
    (mikutter-mode)
    (current-buffer)))

(defun mikutter-console ()
  (interactive)
  (pop-to-buffer (mikutter:make-console-buffer)))

(provide 'onthefly-executer)

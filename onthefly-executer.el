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
(defun onthefly-executer (ruby-code)
  "Execute ruby code on mikutter"
  (apply 'dbus-call-method
    :session
    "org.mikutter.dynamic"
    "/org/mikutter/MyInstance"
    "org.mikutter.eval"
    "ruby" (list ruby-code)))

(defun onthefly-executer-current-buffer ()
  (interactive)
  (let ((current-plugin (mikutter:current-plugin)))
    (when current-plugin
      (onthefly-executer (concat "Plugin.uninstall(:" current-plugin ")")))
    (onthefly-executer (buffer-string))))

(provide 'onthefly-executer)

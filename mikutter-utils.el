;;; mikutter-utils.el --- mikutter編集用のユーティリティメソッド

;; Copyright (C) 2012  Toshiaki Asai

;; Author: Toshiaki Asai <toshi.alternative@gmail.com>
;; Keywords: extensions

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

;;編集中のプラグイン名を返す
(defun mikutter:current-plugin ()
  (let ((bstr (buffer-string)))
    (string-match "Plugin.create\\s\\*(:\\(.+?\\))" bstr)
    (match-string 1 bstr)))

(provide 'mikutter-utils)
;;; mikutter-utils.el ends here

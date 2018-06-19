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
    (and (string-match "Plugin\\(?:.\\|::\\)create\s*\\(?:(:\\([a-zA-Z0-9_]+\\))\\|:\\([a-zA-Z0-9_]+\\)\\)" bstr)
         (or (match-string 1 bstr) (match-string 2 bstr)))))

;; ":twitter, :web" のようなシンボルを列挙した文字列から、対応するhuman readableな変数名を列挙した文字列にして返す。
;; 基本的にはコロンを取り除くだけ("twitter, web")。
;; 2個目以降の値が "1個目_" で始まってる場合は、その部分を取り除く(":twitter, :twitter_tweet" → "twitter, tweet")。
(defun mikutter:defspell-arguments (symbol-listed-text)
  (let* ((model-slugs (mapcar (lambda (x) (substring (string-trim x) 1))
							  (split-string symbol-listed-text ",")))
		 (prefix (format "%s_" (car model-slugs))))
	(mapconcat 'identity
			   (mapcar (lambda (x)
						 (if (string-prefix-p prefix x)
							 (substring x (length prefix))
						   x))
					   model-slugs) ", ")))

(provide 'mikutter-utils)
;;; mikutter-utils.el ends here

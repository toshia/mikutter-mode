;;; e2wm-mikutter.el --- e2wm parsepective for mikutter development

;; Copyright (C) 2012  Toshiaki Asai

;; Author: Toshiaki Asai <toshi@myhost>
;; Keywords: c

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

(require 'e2wm)

(setq e2wm:c-mikutter-recipe
      '(| (:left-max-size 24)
          (- (:upper-size-ratio 0.7)
             imenu
             history)
          (| (:left-max-size 120)
             main
             (- (:upper-size-ratio 0.33)
                sub
                (- (:upper-size-ratio 0.5)
                   result
                   log)))))

(setq e2wm:c-mikutter-winfo
      '((:name imenu :plugin imenu :default-hide nil)
        (:name history :plugin history-list)
        (:name main)
        (:name sub :buffer nil :default-hide t)
        (:name result :buffer "*mikutter-result*")
        (:name log :buffer "*mikutter-log*")))

(defvar e2wm:c-mikutter-right-default 'main)

(e2wm:pst-class-register
  (make-e2wm:$pst-class
   :name   'mikutter
   :extend 'code
   :title  "mikutter plugin edit"
   :init   'e2wm:dp-mikutter-init
   :main   'main
   ;; :switch 'e2wm:dp-mikutter-switch
   ;; :popup  'e2wm:dp-mikutter-popup
   ;; :keymap 'e2wm:dp-mikutter-minor-mode-map
   ))

;; mikutterパースペクティブに切り替えられた時の処理
(defun e2wm:dp-mikutter-init ()
  (with-current-buffer (get-buffer-create "*mikutter-result*")
    (ruby-mode))
  (get-buffer-create "*mikutter-log*")
  (let*
      ((mikutter-wm
        (wlf:no-layout
         e2wm:c-mikutter-recipe
         e2wm:c-mikutter-winfo))
       (buf (or e2wm:prev-selected-buffer
                (e2wm:history-get-main-buffer))))
    ;; (wlf:set-buffer mikutter-wm 'left buf)
    ;; (cond
    ;;  ((eq e2wm:c-two-right-default 'left)
    ;;   (wlf:set-buffer mikutter-wm 'right buf))
    ;;  ((eq e2wm:c-two-right-default 'prev)
    ;;   (wlf:set-buffer mikutter-wm 'right (e2wm:history-get-prev buf)))
    ;;  (t
    ;;   (wlf:set-buffer mikutter-wm 'right (e2wm:history-get-prev buf))))

    mikutter-wm)
)

(defun e2wm:dp-mikutter ()
  (interactive)
  (e2wm:pst-change 'mikutter))
(e2wm:dp-mikutter)

(provide 'e2wm-mikutter)
;;; e2wm-mikutter.el ends here


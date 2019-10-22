;;; mikutter-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))



(autoload 'mikutter-mode "mikutter" "\
mikutterコア・プラグイン開発用モード
" t nil)

(autoload 'mikutter-boot "mikutter" "\
mikutterをデバッグモードで起動する
" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "mikutter" '("mikutter-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; mikutter-mode-autoloads.el ends here

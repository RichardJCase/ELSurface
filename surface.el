(provide 'surface)

(defun point-list (xi yi ret)
  (if (= xi (length x-vals))
      (point-list 0 (+ 1 yi) ret)
    (if (= yi (length y-vals))
	ret
      (point-list (+ xi 1) yi (cons (list (nth xi x-vals) (nth yi y-vals) (nth yi (nth xi z-vals))) ret)))))

(defun point-to-str (point)
  (string-join point " "))

(defun gnuplot-show (gpl-cmds)
    (let* ((temporary-file-directory ".")
           (cmdfile (make-temp-file "gnuplot-cmds-" nil ".gpl"))
           (shellcmd (format "gnuplot --persist -c \"%s\"" cmdfile))
           (cmds gpl-cmds))
      (with-temp-file cmdfile
        (insert cmds))
      (shell-command shellcmd)
      (delete-file cmdfile)
      cmds))

(defun org-table-surface-points ()
  (interactive)
  (let* ((data (org-table-to-lisp))
    (title (nth 0 (nth 0 data)))
    (x-vals (cdr (car data)))
    (y-vals (mapcar 'car (cdr (cdr data))))
    (z-vals (mapcar 'cdr (cdr (cdr data))))
    (points (point-list 0 0 ()))
    (str-points (mapcar 'point-to-str points))
    (pointstr (string-join str-points "\n"))
    (gpl-cmds (string-join (list "set grid\n"
				 "set key off\n"
				 "set hidden3d\n"
				 "set title \"" title "\"\n"
				 "$grid << EOD\n"
				 pointstr "\n"
				 "EOD\n"
				 "splot '$grid' matrix with lines") "")))
  (gnuplot-show gpl-cmds)))

(defun circle-area (radix)
  (let ((pi 3.1415926) area)
    (setq area (* pi radix radix))
    (message "面积是%.2f" area)))

(circle-area 3)

(funcall (lambda (name)
           (message "Hello, %s!" name))
	 "Emacser")

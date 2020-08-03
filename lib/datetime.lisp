(in-package :lispyboi)
(provide "time")

(defstruct datetime
  (second)
  (minute)
  (hour)
  (day)
  (month)
  (year)
  (week-day)
  (year-day))

(let ((days-since-jan1
        #(#(0 31 59 90 120 151 181 212 243 273 304 334 365) ;; 365 days
          #(0 31 60 91 121 152 182 213 244 274 305 335 366) ;; 366 days, leap year
          )))
  (defun seconds-since-epoch-to-datetime (seconds)
    (let ((sec)
          (quadricentennials) (centennials) (quadrennials) (annuals)
          (year) (leap)
          (year-day) (month-day) (week-day)
          (month) (hour) (minute))

      ;;  400 years: 
      ;; 
      ;;  1st hundred, starting immediately after a leap year that's a multiple of 400: 
      ;;  n n n l  \ 
      ;;  n n n l   } 24 times 
      ;;  ...      / 
      ;;  n n n l / 
      ;;  n n n n 
      ;; 
      ;;  2nd hundred: 
      ;;  n n n l  \ 
      ;;  n n n l   } 24 times 
      ;;  ...      / 
      ;;  n n n l / 
      ;;  n n n n 
      ;; 
      ;;  3rd hundred: 
      ;;  n n n l  \ 
      ;;  n n n l   } 24 times 
      ;;  ...      / 
      ;;  n n n l / 
      ;;  n n n n 
      ;; 
      ;;  4th hundred: 
      ;;  n n n l  \ 
      ;;  n n n l   } 24 times 
      ;;  ...      / 
      ;;  n n n l / 
      ;;  n n n L <- 97'th leap year every 400 years 
      
      ;; Re-bias from 1970 to 1601:
      ;; 1970 - 1601 = 369 = 3 * 100 + 17*4 + 1 years (incl. 89 leap days) =
      ;; (3*100*(365+24/100) + 17*4*(365+1/4) + 1*365)*24*3600 seconds
      (setf sec (+ seconds 11644473600))

      (setf week-day (rem (+ 1 (/ sec 86400)) 7))

      ;; remove multiples of 400 years (incl. 97 leap days)
      (setf quadricentennials (/ sec 12622780800)) ;; 400*365.2425*24*3600
      (setf sec (rem sec 12622780800))

      ;; remove multples of 100 years (incl. 24 leap days), can't be more than 3
      ;; because multiples of 4*100=400 years (incl. leap days) have been removed
      (setf centennials (/ sec 3155673600)) ;; 100*(365+24/100)*24*3600
      (when (> centennials 3)
        (setf centennials 3))
      (decf sec (* centennials 3155673600))

      ;; remove multiples of 4 years (incl. 1 leap day), can't be more than 24
      ;; because multiples of 25*4=100 years (incl. leap days) have been removed
      (setf quadrennials (/ sec 126230400)) ;; 4*(365+1/4)*24*3600
      (when (> quadrennials 24)
        (setf quadrennials 24))
      (decf sec (* quadrennials 126230400))

      ;; remove multiples of years (incl. 0 leap days) can't be more than 3
      ;; because multiples of 4 years (incl. leap days) have been removed
      (setf annuals (/ sec 31536000)) ;; 365*24*3600
      (when (> annuals 3)
        (setf annuals 3))
      (decf sec (* annuals 31536000))

      ;; calculate the year and find out if it's leap
      (setf year (+ 1601
                    (* quadricentennials 400)
                    (* centennials 100)
                    (* quadrennials 4)
                    annuals))
      (setf leap (if (and (= 0 (rem year 4))
                          (or (/= 0 (rem year 100))
                              (= 0 (rem year 400))))
                     1
                     0))

      (setf year-day (/ sec 86400))
      (setf sec (rem sec 86400))
      (setf hour (/ sec 3600))
      (setf sec (rem sec 3600))
      (setf minute (/ sec 60))
      (setf sec (rem sec 60))

      (setf month 1)
      (setf month-day 1)
      (let ((done nil))
        (while (and (not done) (< month 13))
               (if (< year-day (aref (aref days-since-jan1 leap) month))
                   (progn
                     (incf month-day (- year-day (aref (aref days-since-jan1 leap) (- month 1))))
                     (setf done t))
                   (incf month))))

      (make-datetime
       sec
       minute
       hour
       month-day
       month
       year 
       week-day
       year-day))))

(defun datetime-now ()
  (seconds-since-epoch-to-datetime (/ (get-clock-ticks) (clocks-per-second))))

(defun timestamp ()
  (with-slots (hour minute second day month year)
      (datetime-now)
    (format nil "[~d:~d:~d-~d-~d-~d]"
            hour minute second
            year month day)))


(export '(datetime
          datetimep
          datetime-now
          timestamp))

(in-package #:jfh-utility)

(defun generate-unique-token ()
  "create a token based on the date and RANDOM"
  (flet ((coalesce (original alternative)
           "Coalesce to alternative value if original is zero."
           (if (zerop original) alternative original)))
    (multiple-value-bind
          (current-second current-minute current-hour current-day current-month _year current-day-of-the-week _daylight-p current-zone)
        (decode-universal-time (get-universal-time))
      (declare (ignore _year _daylight-p))
      (let* ((hour (coalesce current-hour (1+ (random 24))))
             (minute (coalesce current-minute (1+ (random 60))))
             (second (coalesce current-second (1+ (random 60))))
             (salt (random (* hour minute second)))
             (date-based-random-number (format nil "~s~s~d~s~s~s~s"
                                               (* salt current-month)
                                               (* salt current-day)
                                               (* salt current-day-of-the-week)
                                               (* salt current-zone)
                                               current-hour
                                               current-minute
                                               current-second)))
        (format nil "~a-~a-~a-~a"
                (subseq date-based-random-number 0 7)
                (format nil "~5,'0d" (random (- (expt 10 5) 1)))
                (subseq (reverse date-based-random-number) 0 7)
                (format nil "~5,'0d" (random (- (expt 10 5) 1))))))))

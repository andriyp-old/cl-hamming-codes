(defpackage :cl-hamming-codes
  (:use :cl :iter)
  (:export #:hamming-distance
           #:hamming-code/build
           #:hamming-code/fix))

(in-package :cl-hamming-codes)

;; макрос взятия n-го бита
(defmacro nth-bit (n place)
  `(ldb (byte 1 (1- ,n)) ,place))
  
;; функция нахождения целого количества
;; битов, которыми можно представить число x
(defun bits-to-represent (x)
  (iter (until (= 0 x))
        (setf x (floor (/ x 2)))
        (sum 1)))
  
;; функция подсчёта расстояния Хэмминга
(defun hamming-distance (x y &optional (bits-to-scan 0))
  (let ((difference (logxor x y)))
    (when (= 0 bits-to-scan)
      (setf bits-to-scan (max (bits-to-represent x)
                              (bits-to-represent y))))
    (iter (for _ from 1 to bits-to-scan)
          (sum (logand 1 difference))
          (setf difference (ash difference -1)))))

;; функция построения кода Хэмминга
(defun hamming-code/build (msg)
  (let ((len (bits-to-represent msg))
        (checksum 0) (result 0))
    ;; [ формирование контрольной суммы ]
    (iter (with n = len) ;; счётчик количества бит
          (with m = 0) ;; счётчик степеней двоек
          ;; голова цикла
          (for i from 1)
          (while (<= i n))
          ;; если индекс i не является степенью двойки
          (if (/= 0 (rem (log i 2) 1))
              ;; если соответствующий бит в msg ненулевой
              (when (/= 0 (nth-bit (- i m) msg))
                ;; записать i в список indexes
                (collect i into indexes))
              ;; иначе инкрементировать счётчики
              (setf n (1+ n)
                    m (1+ m)))
          ;; напоследок, вычислить контрольную сумму как
          ;; как сумму по модулю 2 всех элементов indexes
          (finally (setf checksum (apply #'logxor indexes))))
    ;; [ формирование кода ]
    (iter (with n = len) ;; счётчик количества бит
          (with m = 0) ;; счётчик степеней двоек
          ;; голова цикла
          (for i from 1)
          (while (<= i n))
          ;; вычислить целую и дробную части
          ;; двоичного логарифма индекса
          (for (quot rem)
             = (multiple-value-list
                   (floor (log i 2))))         
          ;; если индекс является степенью двойки
          (if (= 0 rem)
              (setf ;; то записать бит из checksum
                    (nth-bit i result) (nth-bit (1+ quot) checksum)
                    ;; и инкрементировать счётчики
                    n (1+ n) m (1+ m))
              ;; иначе записать бит из msg
              (setf (nth-bit i result) (nth-bit (- i m) msg))))
    result))
    
;; функция исправления кода Хэмминга    
(defun hamming-code/fix (code)
  (iter ;; для всех индексов битов i в code
        (for i from 1 to (bits-to-represent code))
        ;; если i-й бит code не равен нулю
        (when (/= 0 (ldb (byte 1 (1- i)) code))
          ;; записать i в список indexes
          (collect i into indexes))
        (finally ;; напоследок,
          ;; просчитать checksum как сумму по модулю
          ;; два всех элементов списка indexes
          (let ((checksum (apply #'logxor indexes)))
            ;; если checksum /= 0 (то есть, произошла ошибка)
            (if (/= 0 checksum)
                ;; исправить ошибку, инвертировав бит
                ;; с индексом, равным checksum
                (setf (nth-bit checksum code)
                      (lognot (nth-bit checksum code))))
            ;; вернуть code
            (return code)))))

            
;(format t "~%~b~%" (hamming-code/build #b1110011))
;(format t "~%~b~%" (hamming-code/build #b10110100101000111101010010101001))
;(format t "~b" (hamming-code/fix #b11110100010100011110101100101011000101))
;(format t "~b" (hamming-code/fix #b10110011110))
(defpackage :cl-hamming-codes
  (:use :cl :iter)
  (:export #:hamming-distance
           #:hamming-code/build
           #:hamming-code/fix))

(in-package :cl-hamming-codes)

;; ������ ������ n-�� ����
(defmacro nth-bit (n place)
  `(ldb (byte 1 (1- ,n)) ,place))
  
;; ������� ���������� ������ ����������
;; �����, �������� ����� ����������� ����� x
(defun bits-to-represent (x)
  (iter (until (= 0 x))
        (setf x (floor (/ x 2)))
        (sum 1)))
  
;; ������� �������� ���������� ��������
(defun hamming-distance (x y &optional (bits-to-scan 0))
  (let ((difference (logxor x y)))
    (when (= 0 bits-to-scan)
      (setf bits-to-scan (max (bits-to-represent x)
                              (bits-to-represent y))))
    (iter (for _ from 1 to bits-to-scan)
          (sum (logand 1 difference))
          (setf difference (ash difference -1)))))

;; ������� ���������� ���� ��������
(defun hamming-code/build (msg)
  (let ((len (bits-to-represent msg))
        (checksum 0) (result 0))
    ;; [ ������������ ����������� ����� ]
    (iter (with n = len) ;; ������� ���������� ���
          (with m = 0) ;; ������� �������� �����
          ;; ������ �����
          (for i from 1)
          (while (<= i n))
          ;; ���� ������ i �� �������� �������� ������
          (if (/= 0 (rem (log i 2) 1))
              ;; ���� ��������������� ��� � msg ���������
              (when (/= 0 (nth-bit (- i m) msg))
                ;; �������� i � ������ indexes
                (collect i into indexes))
              ;; ����� ���������������� ��������
              (setf n (1+ n)
                    m (1+ m)))
          ;; ����������, ��������� ����������� ����� ���
          ;; ��� ����� �� ������ 2 ���� ��������� indexes
          (finally (setf checksum (apply #'logxor indexes))))
    ;; [ ������������ ���� ]
    (iter (with n = len) ;; ������� ���������� ���
          (with m = 0) ;; ������� �������� �����
          ;; ������ �����
          (for i from 1)
          (while (<= i n))
          ;; ��������� ����� � ������� �����
          ;; ��������� ��������� �������
          (for (quot rem)
             = (multiple-value-list
                   (floor (log i 2))))         
          ;; ���� ������ �������� �������� ������
          (if (= 0 rem)
              (setf ;; �� �������� ��� �� checksum
                    (nth-bit i result) (nth-bit (1+ quot) checksum)
                    ;; � ���������������� ��������
                    n (1+ n) m (1+ m))
              ;; ����� �������� ��� �� msg
              (setf (nth-bit i result) (nth-bit (- i m) msg))))
    result))
    
;; ������� ����������� ���� ��������    
(defun hamming-code/fix (code)
  (iter ;; ��� ���� �������� ����� i � code
        (for i from 1 to (bits-to-represent code))
        ;; ���� i-� ��� code �� ����� ����
        (when (/= 0 (ldb (byte 1 (1- i)) code))
          ;; �������� i � ������ indexes
          (collect i into indexes))
        (finally ;; ����������,
          ;; ���������� checksum ��� ����� �� ������
          ;; ��� ���� ��������� ������ indexes
          (let ((checksum (apply #'logxor indexes)))
            ;; ���� checksum /= 0 (�� ����, ��������� ������)
            (if (/= 0 checksum)
                ;; ��������� ������, ������������ ���
                ;; � ��������, ������ checksum
                (setf (nth-bit checksum code)
                      (lognot (nth-bit checksum code))))
            ;; ������� code
            (return code)))))

            
;(format t "~%~b~%" (hamming-code/build #b1110011))
;(format t "~%~b~%" (hamming-code/build #b10110100101000111101010010101001))
;(format t "~b" (hamming-code/fix #b11110100010100011110101100101011000101))
;(format t "~b" (hamming-code/fix #b10110011110))
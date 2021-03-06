;; util.scm
;; Copyright (C) 2021 Michael Rosset <mike.rosset@gmail.com>

;; This file is part of Prospect

;; Prospect is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Prospect is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (prospect util)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt base16)
  #:export (difficulty
            int2hex
            hex->number
            olog
            swap-order))

(define-class <test-util> (<test-case>))

(define (swap-order str)
  "Swaps base16 @var{str}'s little endianness to big endianness"
  (let swap ((n "")
             (i 0))
    (if (< i (string-length str))
        (let ((sub (string-reverse (substring str i (+ i 2)))))
          (swap (string-append n sub) (+ i 2)))
        (string-reverse n))))

(define (hex->number str)
  "Converts a hexadecimal @var{str} to integer"
  (string->number (string-append "#x" str)))

(define-method (test-hex->number (self <test-util>))
  (assert-equal 386923168 (hex->number "170ffaa0")))

;; 0x0404cb * 2**(8*(0x1b - 3))
(define (difficulty bits)
  "Returns the target hash for difficulty @var{bits}"
  (let ((exp  (round-ash bits -24))
        (mant (logand bits #xffffff)))
    (format #f "~64,'0x" (* mant (round-ash 1 (* 8 (- exp 3)))))))

(define-method (test-diff (self <test-util>))
  (assert-equal "00000000000000015f5300000000000000000000000000000000000000000000"
                (difficulty #x19015f53))
  (assert-equal "00000000000000015f5300000000000000000000000000000000000000000000"
                (difficulty (hex->number "19015f53"))))

(define (int2hex int)
  "Converts a integer to a hex string."
  (bytevector->base16-string
   (uint-list->bytevector (list int) (endianness little) 4)))

(define-method (test-int2hex (self <test-util>))
  ;; Version
  (assert-equal "01000000" (int2hex 1))
  ;; Time
  (assert-equal "c7f5d74d" (int2hex 1305998791))
  ;; Bits
  (assert-equal "f2b9441a" (int2hex 440711666))
  ;; Nonce
  (assert-equal "42a14695" (int2hex 2504433986))
  (assert-equal "00000000" (int2hex 0)))

(define* (olog prefix #:optional (obj ""))
  (format #t ";; ~a ~a\n" prefix obj)
  obj)

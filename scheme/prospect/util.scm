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
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base16)
  #:export (int2hex))

(define-class <test-util> (<test-case>))

(define (swap-order str)
  "Swaps base16 @var{str}'s little endianness to big endianness"
  (let swap ((n "")
	     (i 0))
    (if (< i (string-length str))
	(let ((sub (string-reverse (substring str i (+ i 2)))))
	  (swap (string-append n sub) (+ i 2)))
	(string-reverse n))))

(define (hash-header bv)
  "Hashes a block header @var{bv} a bytevector and returns it's hash as a
base16 string the endianness is big"
  (bytevector->base16-string (sha256 (sha256 bv))))

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

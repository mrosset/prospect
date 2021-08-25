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

(define-module (prospect work)
  #:use-module (prospect util)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base16)
  #:export (work))

(define-class <test-work> (<test-case>))

(define (hash-header bv)
  "Hashes a block header @var{bv} a bytevector and returns it's hash as a
base16 string the endianness is big"
  (bytevector->base16-string (sha256 (sha256 bv))))

(define* (work #:key version prev root time bits nonce)
  (let* ((hex (string-append version prev root time bits nonce))
	 (bv   (base16-string->bytevector hex)))
    (swap-order
     (hash-header bv))))

;; FIXME: This should hash correctly
;;
;; According to
;; http://www.righto.com/2014/02/bitcoin-mining-hard-way-algorithms.html?m=1
;; this should successfully find
;; 0000000000000000e067a478024addfecdc93628978aa52d91fabd4292982a50
;; with a nonce of 856192328, but this hash fails. could be that the
;; python packing is different the using hexadecimal strings?
(define-method (test-work-complex (self <test-work>))
  (let ((result (work
		 #:version (int2hex 1)
		 #:prev    "7179b5791ce81d7f2e55da2f7995b95533e0ad8b87308c711000000000000000"
		 #:root    "a87992a572744b1f0a3b49d83f9930440c1ef96a2b9bb2a3918c6eabcd417178"
		 #:time    (int2hex 1392872245)
		 #:bits    (int2hex 419520339)
		 #:nonce   (int2hex 856192328))))
    (assert-false (string=? "0000000000000000e067a478024addfecdc93628978aa52d91fabd4292982a50"
			    result))))

(define-method (test-work-simple (self <test-work>))
  (let ((result (work
		 #:version "01000000"
		 #:prev    "81cd02ab7e569e8bcd9317e2fe99f2de44d49ab2b8851ba4a308000000000000"
		 #:root    "e320b6c2fffc8d750423db8b1eb942ae710e951ed797f7affc8892b0f1fc122b"
		 #:time    "c7f5d74d"
		 #:bits    "f2b9441a"
		 #:nonce   "42a14695")))
    (assert-equal "00000000000000001e8d6829a8a21adc5d38d0a473b144b6765798e61f98bd1d"
		  result)))

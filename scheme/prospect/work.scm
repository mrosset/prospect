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

;; Simulate proof of work with two rounds. The staring nonce in once less
;; then a know found nonce.
(define-method (test-work-rounds (self <test-work>))
  (let* ((version "02000000")
	 (prev    "17975b97c18ed1f7e255adf297599b55330edab87803c8170100000000000000")
	 (root    "8a97295a2747b4f1a0b3948df3990344c0e19fa6b2b92b3a19c8e6badc141787")
	 (time    (int2hex 1392872245))
	 (bits    419520339)
	 (nonce   856192327)
	 (target  (difficulty bits)))
    (define (step incr)
      (work #:version version
	    #:prev prev
	    #:root root
	    #:time time
	    #:bits (int2hex bits)
	    #:nonce (int2hex (+ nonce incr))))
    ;; Check we have the right target hash.
    (assert-equal "00000000000000015f5300000000000000000000000000000000000000000000"
		  target)
    ;; Check we are starting with the right nonce.
    (assert-equal "8cca5b98c8adeb059907c6b94dd513e72c51d0fd2f95922b57004f567d10e497"
		  (step 0))
    ;; Check that incrementing the nonce by one finds the block hash.
    (assert-equal "0000000000000000e067a478024addfecdc93628978aa52d91fabd4292982a50"
		  (step 1))
    ;; Check we have not found the hash
    (assert-false (string< (step 0) target))
    ;; Check we have found the hash
    (assert-true (string< (step 1) target))))

;; This tests bitcoin's second block. The block after the genesis block. Since
;; the genesis block does not have a previous block hash I'm not sure
;; how the proof of work algorithm handles that.
(define-method (test-work-block-two (self <test-work>))
  (let ((result (work
		 #:version (int2hex 1)
		 #:prev    (swap-order "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
		 #:root    (swap-order "0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098")
		 #:time    (int2hex 1231469665)
		 #:bits    (int2hex 486604799)
		 #:nonce   (int2hex 2573394689))))
    (assert-equal "00000000839a8e6886ab5951d76f411475428afc90947ee320161bbf18eb6048"
		  result)))

(define-method (test-work-complex (self <test-work>))
  (let ((result (work
		 #:version "02000000"
		 #:prev    "17975b97c18ed1f7e255adf297599b55330edab87803c8170100000000000000"
		 #:root    "8a97295a2747b4f1a0b3948df3990344c0e19fa6b2b92b3a19c8e6badc141787"
		 #:time    (int2hex 1392872245)
		 #:bits    (int2hex 419520339)
		 #:nonce   (int2hex 856192328))))
    (assert-equal "0000000000000000e067a478024addfecdc93628978aa52d91fabd4292982a50"
		  result)))

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

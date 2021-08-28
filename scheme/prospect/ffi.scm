;; ffi.scm
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

(define-module (prospect ffi)
  #:use-module (prospect util)
  #:use-module (prospect rpc)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (system foreign)
  #:use-module (system foreign-library))

(define-class <test-ffi> (<test-case>))

(define libblkmaker "libblkmaker-0.1")

(define libblkmaker-jansson "libblkmaker_jansson-0.1")

(define libjansson "libjansson")

(define gnu-version
  (foreign-library-function
   #f
   "gnu_get_libc_version"
   #:return-type '*))

(define make-template
  (foreign-library-function
   libblkmaker
   "blktmpl_create"
   #:return-type '*))

(define add-caps
  (foreign-library-function
   libblkmaker
   "blktmpl_addcaps"
   #:return-type uint32
   #:arg-types (list '*)))

(define request-jansson
  (foreign-library-function
   libblkmaker-jansson
   "blktmpl_request_jansson"
   #:return-type '*
   #:arg-types (list uint32 '*)))

(define (request->string req)
  "Returns the json string for @var{req}"
  (pointer->string (json-dumps req 2)))

(define json-dumps
  (foreign-library-function
   libjansson
   "json_dumps"
   #:return-type '*
   #:arg-types (list '* size_t)))

(define-method (test-template (self <test-ffi>))
  (assert-true (pointer? (make-template)))    )

(define-method (test-request (self <test-ffi>))
  (let* ((tmpl  (make-template))
	 (req   (request-jansson (add-caps tmpl)
				 %null-pointer))
	 (res  (post-json (request->string req))))
    (assert-true (pointer? req))
    (assert-true (string? (request->string req)))
    (assert-true (string? res))))

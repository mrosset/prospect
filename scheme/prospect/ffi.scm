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
  #:use-module (json)
  #:use-module (unit-test)
  #:use-module (system foreign)
  #:use-module (system foreign-library))

(load-extension "libguile-prospect" "init_prospect")

(define-class <test-ffi> (<test-case>))

(define libblkmaker "libblkmaker-0.1")

(define libblkmaker-jansson "libblkmaker_jansson-0.1")

(define libjansson "libjansson")

(define gnu-version
  (foreign-library-function
   #f
   "gnu_get_libc_version"
   #:return-type '*))

;; Jansson
(define json-dumps
  (foreign-library-function
   libjansson
   "json_dumps"
   #:return-type '*
   #:arg-types (list '* size_t)))

(define json-loads
  (foreign-library-function
   libjansson
   "json_loads"
   #:return-type '*
   #:arg-types (list '* size_t '*)))

(define json-load-file
  (foreign-library-function
   libjansson
   "json_load_file"
   #:return-type '*
   #:arg-types (list '* size_t '*)))

(define-method (test-json-load-file (self <test-ffi>))
  (let ((file (string->pointer "data.json")))
    (assert-false (null-pointer? (json-load-file file 0 %null-pointer)))
    (assert-true #t)))

;; libblkmaker
(define make-template
  (foreign-library-function
   libblkmaker
   "blktmpl_create"
   #:return-type '*))

(define-method (test-make-template (self <test-ffi>))
  (assert-true (pointer? (make-template))))

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

(define add-jansson
  (foreign-library-function
   libblkmaker-jansson
   "blktmpl_add_jansson"
   #:return-type '*
   #:arg-types (list '* '* long)))

(define-method (test-add-jansson (self <test-ffi>))
  (let* ((tmpl (make-template))
	 (file (string->pointer "data.json"))
	 (json (json-load-file file 0 %null-pointer)))
    (assert-true (null-pointer? (add-jansson tmpl json 0)))))

(define init-generation
  (foreign-library-function
   libblkmaker
   "blkmk_init_generation"
   #:return-type '*
   #:arg-types (list '* '* long)))

(define-method (test-init-generation (self <test-ffi>))
  (let* ((tmpl (make-template))
	 (file (string->pointer "data.json"))
	 (json (json-load-file file 0 %null-pointer)))
    (assert-true (null-pointer? (add-jansson tmpl json 0)))
    (assert-true (init-generation tmpl %null-pointer 0))))

(define-method (test-get-data (self <test-ffi>))
  (let* ((tmpl   (make-template))
       (file   (string->pointer "data.json"))
       (json   (json-load-file file 0 %null-pointer)))
  (assert-true (prospect-extention?))
  (assert-true (pointer? tmpl))
  (assert-true (pointer? file))
  (assert-true (pointer? json))
  (assert-true (null-pointer? (add-jansson tmpl json 0)))
  (assert-true (init-generation tmpl %null-pointer 0))
  (assert-equal 76 (get-data tmpl))
  (assert-equal "ab52937526190b791f641a6c5c3b0c4ca78cfa35fb31398618787b49fbd2449a"
		(merkle-root tmpl))))

(define (request->string req)
  "Returns the json string for @var{req}"
  (pointer->string (json-dumps req 2)))

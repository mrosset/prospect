;; rpc.scm
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

(define-module (prospect rpc)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (json)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (rnrs bytevectors)
  #:use-module (unit-test)
  #:export (post-json))

(define-class <test-rpc> (<test-case>))

(define host (make-parameter  "http://localhost:8333"))

(define headers '((Authorization .
                                 "Basic c3RyaW5nczp0VnNnWW5rYWlQUnhUSElNV1NscUdycXZyUWNTaEJ3SXJBeTV1Qi1vRVZnPQ==")))

(define-json-type <result>
  (result)
  (error))

(define-json-type <error>
  (id)
  (message))

(define-json-type <post>
  (jsonrpc)
  (id)
  (method)
  (params))

(define (post-json str)
  "Posts @var{str} request to rpc server, returns response string"
  (receive (res body)
      (http-post (host) #:body str #:headers headers)
    (utf8->string body)))

;; FIXME: This does not actually post anything.  Find a simple string
;; to post without effecting the RPC server.
(define-method (test-post-json (self <test-rpc>))
  (let ((p (make-post "2.0" "test" "logging" #())))
    (assert-true (string? (post-json (post->json p))))))

(define* (post method #:optional (param '()))
  (let ((p (make-post "2.0" "(prospect post)" method param)))
    (receive (res body)
        (http-post (host) #:body (post->json p) #:headers headers)
      (json->result (utf8->string body)))))

(define-json-type <chain-info>
  (chain)
  (blocks)
  (headers)
  (bestblockhash)
  (mediatime)
  (chainwork))

(define-json-type <template-request>
  (rules))

(define-json-type <template-result>
  (version))

(define (blockchain-info)
  "Returns blockchain information"
  (let ((r (post "getblockchaininfo" '())))
    (if r
        (scm->chain-info (result-result r))
        r)))

(define-method (test-info (self <test-rpc>))
  (assert-true #t))

(define (get-block-template)
  "Returns a mining block template"
  (let* ((t (make-template-request #("segwit")))
         (r (post "getblocktemplate" (vector (template-request->scm t)))))
    (template-result-version (result-result r))))

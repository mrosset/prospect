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
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-43)
  #:use-module (json)
  #:use-module (oop goops)
  #:use-module (prospect util)
  #:use-module (rnrs bytevectors)
  #:use-module (unit-test)
  #:use-module (web client)
  #:use-module (web response)
  #:export (post-json))

(define-class <test-rpc> (<test-case>))

(define host (make-parameter  "http://localhost:8333"))

(define headers '((Authorization .
                                 "Basic c3RyaW5nczp0VnNnWW5rYWlQUnhUSElNV1NscUdycXZyUWNTaEJ3SXJBeTV1Qi1vRVZnPQ==")))

(define default-cap #("coinbasetxn"
                      "workid"
                      "coinbase/append"
                      ;; "time/increment"
                      ;; "version/force"
                      ;; "version/reduce"
                      ;; "submit/coinbase"
                      ;; "submit/truncate"
                      ))

(define-json-type <result>
  (result)
  (error))

(define-json-type <error>
  (id)
  (message))

(define-json-type <post>
  (id)
  (jsonrpc)
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
  (rules)
  (capabilities))

(define-json-type <transaction>
  (data)
  (txid)
  (hash)
  (depends)
  (fee)
  (sigops)
  (weight))

(define-json-type <template-result>
  (version)
  (rules)
  (previousblockhash)
  (transactions)
  (coinbasetxn)
  (coinbaseaux)
  (coinbasevalue)
  (longpollid)
  (target)
  (mintime)
  (mutable)
  (noncerange)
  (sioplimit)
  (sizelimit)
  (weightlimit)
  (curtime)
  (bits)
  (height)
  (default-witness-commitment))

(define (chain-info)
  "Returns blockchain information"
  (let ((r (post "getblockchaininfo" '())))
    (if r
        (scm->chain-info (result-result r))
        r)))

(define-method (test-chain-info (self <test-rpc>))
  (let ((info (chain-info)))
    (assert-true (chain-info? info))
    (assert-equal "regtest" (chain-info-chain info))))

(define-method (test-info (self <test-rpc>))
  (assert-true #t))

(define* (get-block-template #:key (cap default-cap))
  "Returns a mining block template"
  (let* ((t (make-template-request #("segwit")
                                   cap))
         (r (post "getblocktemplate" (vector (template-request->scm t)))))
    (scm->template-result (result-result r))))

(define (merkle-root tmpl)
  "Returns the templates Merle for transactions."
  (let ((txns (template-result-transactions tmpl)))
    (vector-map (lambda (i v)
                  (display (transaction-data (scm->transaction v)))
                  (newline))
                txns)))

(define (read-json file)
  "Reads the json @var{file} and returns the json string as a <result>"
  (json->result
        (call-with-input-file file get-string-all)))

(define-method (test-read-json (self <test-rpc>))
  (let* ((res  (read-json "data.json"))
         (tmpl (scm->template-result (result-result res))))
    (assert-true (result? res))
    (assert-true (template-result? tmpl))))

(define-method (test-template (self <test-rpc>))
  (let* ((res  (read-json "data.json"))
         (tmpl (scm->template-result (result-result res))))
    (assert-true (result? res))
    (assert-true (template-result? tmpl))
    (assert-true (integer? (template-result-height tmpl)))
    (assert-true (vector? (template-result-transactions tmpl)))))

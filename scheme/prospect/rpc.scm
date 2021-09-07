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
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-43)
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
  (txid)
  (data)
  (hash)
  (depends)
  (fee)
  (sigops)
  (weight))

(define-json-type <template>
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
    (assert-equal "main" (chain-info-chain info))))

(define-method (test-info (self <test-rpc>))
  (assert-true #t))

(define* (get-block-template #:key (cap default-cap))
  "Returns a mining block template"
  (let* ((t (make-template-request #("segwit")
                                   cap))
         (r (post "getblocktemplate" (vector (template-request->scm t)))))
    (scm->template (result-result r))))

(define (read-json file)
  "Reads the json @var{file} and returns the json string as a <result>"
  (json->result
        (call-with-input-file file get-string-all)))

(define-method (test-read-json (self <test-rpc>))
  (let* ((res  (read-json "data.json"))
         (tmpl (scm->template (result-result res))))
    (assert-true (result? res))
    (assert-true (template? tmpl))
    (assert-equal 105 (vector-length (template-transactions tmpl)))))

(define (read-template)
  "Returns a <template> after reading data.json"
  (scm->template (result-result (read-json "data.json"))))

(define-method (test-read-tmpl (self <test-rpc>))
  (assert-true (template? (read-template))))

(define (template->txns tmpl)
  "Returns then transactions for @var{tmpl} as a vector of <transaction>"
  (vector-map (lambda (i v)
                (scm->transaction v))
              (template-transactions tmpl)))

(define-method (test-template->txns (self <test-rpc>))
  (let* ((tmpl (read-template))
         (txns (template->txns tmpl)))
    (assert-equal "ab52937526190b791f641a6c5c3b0c4ca78cfa35fb31398618787b49fbd2449a"
                  (transaction-hash (vector-ref txns 0)))))

(define (hash-txn i txn)
  "Returns a double hash for @var{txn}"
  (let ((data (transaction-data txn)))
    (bytevector->base16-string
     (sha256 (sha256 (base16-string->bytevector data))))))

(define-method (test-hash-txn (self <test-rpc>))
  (let* ((tmpl  (read-template))
         (txns  (template->txns tmpl))
         (hashs (vector-map hash-txn txns)))
    (assert-equal "9a44d2fb497b7818863931fb35fa8ca74c0c3b5c6c1a641f790b1926759352ab"
                  (vector-ref hashs 0))
    (assert-equal "641df143b423afc16af61c7d256f452beb2a60a6377c64b13262af4e90afa2ab"
                  (vector-ref hashs (1- (vector-length hashs))))))

(define (read-hashes)
  "Returns a list of hashes read from test data file"
  (call-with-input-file "hashes.scm" read))

(define-method (test-read-hashes (self <test-rpc>))
  (let ((hashes (read-hashes)))
    (assert-equal 105 (length hashes))
    (assert-equal "9a44d2fb497b7818863931fb35fa8ca74c0c3b5c6c1a641f790b1926759352ab"
                  (list-ref hashes 0))
    (assert-equal "652b0d859ee218719fb0faeba7e11ede6a71a546710c12d4b224f73bc184c288"
                  (list-ref hashes 52))
    (assert-equal "641df143b423afc16af61c7d256f452beb2a60a6377c64b13262af4e90afa2ab"
                   (list-ref hashes 104))))

(define (double-sha256 a b)
  "Returns a double base16 sha256 hash for a pair of base16 strings"
  (let* ((str (string-append a b))
         (bv  (base16-string->bytevector str)))
    (bytevector->base16-string (sha256 (sha256 bv)))))

(define-method (test-double-sha256 (self <test-rpc>))
  (assert-equal "c5530ae62eff821c5d2942b3e4b64b73df726cec6244a29656295df56e91a8d8"
                (double-sha256
                 "9a44d2fb497b7818863931fb35fa8ca74c0c3b5c6c1a641f790b1926759352ab"
                 "7e7dcdf355768a031caec139c148f35a77dd5d2fcea8e4684c3fe25e10ebf215")))

(define (hash-list lst)
  "Loops through @var{lst} and hashes all of it's pairs eventually
returning a merkle root."
  (let ((tree '())
        (len  (1- (length lst))))

    (define (append-tree a b)
      "Appends a double hash to the tree"
      (set! tree (append! tree `(,(double-sha256 a b)))))

    (do ((i 0 (+ i 2)))
        ((> i len))
      ;; Concatenate the first two elements and then double hash. If
      ;; the element is the last in the list then concatenate it's
      ;; self and then double hash.
      (if (< i len)
          (append-tree (list-ref lst i) (list-ref lst (1+ i)))
          (append-tree (list-ref lst i) (list-ref lst i))))

    (if (> (length tree) 1)
        (hash-list tree)
        (car tree))))

(define-method (test-hash-list (self <test-rpc>))
  (assert-equal "c5fff939f628a04428c080ed5bd7cd9bc0b4722b2522743049adb18213adf28a"
                (hash-list (read-hashes))))

(define (merkle-root tmpl)
  "Returns the templates merkle root for transactions."
  (let* ((txns   (template->txns tmpl))
         (hashes (vector->list (vector-map hash-txn txns))))
    (hash-list hashes)))

(define-method (test-merkle-root (self <test-rpc>))
  (let* ((res  (read-json "data.json"))
         (tmpl (scm->template (result-result res))))
    (assert-equal "c5fff939f628a04428c080ed5bd7cd9bc0b4722b2522743049adb18213adf28a"
                (merkle-root tmpl))))

(define-method (test-template (self <test-rpc>))
  (let* ((res  (read-json "data.json"))
         (tmpl (scm->template (result-result res))))
    (assert-true (result? res))
    (assert-true (template? tmpl))
    (assert-true (integer? (template-height tmpl)))
    (assert-true (vector? (template-transactions tmpl)))))

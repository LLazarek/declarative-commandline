#lang racket/base

(provide command-line/declarative
         take-latest)

(require syntax/parse/define
         racket/cmdline
         racket/match
         racket/list
         (for-syntax racket/base))

;; goals:
;; - collect cmdline args in a hash table
;; - declaratively specify to collect multi-args in a list
;; - specify exclusive args
;; - specify mandatory args

;; Example syntax:
#;(command-line/declarative
   #:program "foobar"
   #:once-each
   [("-T" "--tests-only")
    'tests-only
    "Only run tests, not benchmarks."
    #:record
    #:conflicts '(benchmarks-only benchmark)]
   [("-B" "--benchmarks-only")
    'benchmarks-only
    "Only run benchmarks, not benchmarks."
    #:record
    #:conflicts '(tests-only test)]
   #:multi
   [("-t" "--test")
    'test
    "Run this specific test; disable all others (except those also specified with this option)"
    #:collect ["path" cons '()]]
   [("-b" "--benchmark")
    'benchmark
    "Run this specific benchmark; disable all others (except those also specified with this option)"
    #:collect ["benchmark name" cons '()]]
   #:args (positional-1 positional-2))


(struct accumulator (current init combine)
  #:transparent
  #:property
  prop:procedure
  (λ (self new-val)
    (match-define (accumulator current init combine) self)
    (accumulator (combine new-val current)
                 init
                 combine)))

(define (record _1 _2) #t)
(define (take-latest new old) new)
(define (make-collector combine init)
  (accumulator init
               init
               (λ (new acc) (combine new acc))))

(define/match (collect-flags flag-accum state)
  [{'() state} state]
  [{(cons (cons acc-key val) rest) state}
   (define accumulate (hash-ref state acc-key))
   (collect-flags rest
                  (hash-set state acc-key (accumulate val)))])

(define (specified-in? collected-flags flag)
  (match collected-flags
    [(hash-table [(== flag) (accumulator current init _)] _ ...)
     (not (equal? current init))]
    [else #f]))

(define (check-conflicts! collected-flags conflict-hash mandatory-args)
  (for ([(k conflicts) (in-hash conflict-hash)]
        #:when (specified-in? collected-flags k)
        [conflicting-k (in-list conflicts)]
        #:when (specified-in? collected-flags conflicting-k))
    (raise-user-error 'command-line/declarative
                      "Argument error: ~a and ~a cannot both be specified"
                      k conflicting-k))
  (define specified-args
    (for/list ([k (in-hash-keys collected-flags)]
               #:when (specified-in? collected-flags k))
      k))
  (define unspecified-mandatory-args
    (for/list ([(flag test) (in-hash mandatory-args)]
               #:when (not (or (specified-in? collected-flags flag)
                               (test specified-args))))
      flag))
  (unless (empty? unspecified-mandatory-args)
    (raise-user-error 'command-line/declarative
                      "Missing mandatory arguments: ~a"
                      unspecified-mandatory-args)))

(define (strip-accumulators flags-hash)
  (for/hash ([(k acc) (in-hash flags-hash)])
    (values k (accumulator-current acc))))



(begin-for-syntax
  (require syntax/parse
           racket/list)
  (define-syntax-class flag-spec
    #:commit
    #:attributes
    [flags name handler help-list
           init-value
           collector-function
           conflicting-flags
           mandatory-kw
           mandatory-unless-pred]

    [pattern
     [flags:expr name:expr desc:expr
                 {~optional {~and mandatory-kw #:mandatory}}
                 {~optional {~seq #:mandatory-unless
                                  mandatory-unless-pred:expr}}
                 {~or* {~and record-kw #:record}
                       {~seq #:collect [arg-name:expr
                                        collector:expr
                                        init:expr]}}
                 {~optional {~seq #:conflicts conflicts:expr}}]
     #:with handler #`(let ([flag-name name])
                        (λ (_ #,@(if (attribute collector) #'(arg) #'()))
                          #,(cond [(attribute record-kw)
                                   #'(cons flag-name #t)]
                                  [else
                                   #'(cons flag-name arg)])))
     #:with help-list #'(desc {~? arg-name})
     #:with init-value #'{~? init #f}
     #:with collector-function (if (attribute record-kw)
                                   #'record
                                   #'collector)
     #:with conflicting-flags (if (attribute conflicts)
                                  #'conflicts
                                  #''())]))

(define-simple-macro
  (command-line/declarative
   {~optional {~seq #:program name:expr}}
   {~optional {~seq #:argv argv:expr}}
   {~seq kw:keyword spec:flag-spec ...} ...
   {~optional {~seq #:args {~or* (pos-arg:id ...)
                                 pos-arg-rest:id}}})
  #:with [pos-arg-inferred-name ...] #'{~? (pos-arg ...) {~? (pos-arg-rest) ()}}
  #:with flag-init-hash #'(hash {~@ spec.name
                                    (make-collector spec.collector-function
                                                    spec.init-value)}
                                ...
                                ...)
  #:with flag-conflict-hash #'(make-immutable-hash
                               (list (cons spec.name spec.conflicting-flags)
                                     ...
                                     ...))
  #:with [{~alt (mandatory-arg _) (_)} ...]
  #'((spec.name {~? spec.mandatory-kw}) ... ...)
  #:with [{~alt (maybe-mandatory-arg test) (_)} ...]
  #'((spec.name {~? spec.mandatory-unless-pred}) ... ...)
  (command-line
   {~? {~@ #:program name}}
   {~? {~@ #:argv argv}}
   {~@ kw [spec.flags => spec.handler 'spec.help-list] ...}
   ...
   #:handlers
   (λ {~? (flag-accum pos-arg ...)
          {~? (flag-accum . pos-arg-rest)
              (flag-accum)}}
     (define flags-hash
       (collect-flags flag-accum
                      flag-init-hash))
     (check-conflicts! flags-hash
                       flag-conflict-hash
                       (hash {~@ mandatory-arg (λ _ #f)} ...
                             {~@ maybe-mandatory-arg test} ...))
     (cons (strip-accumulators flags-hash)
           {~? (list pos-arg ...)
               {~? pos-arg-rest
                   '()}}))
   (map symbol->string '(pos-arg-inferred-name ...))))

(module+ main
  (command-line/declarative
   #:multi
   [("-n") 'thing
           ("A thing to process" "Mandatory.")
           #:mandatory
           #:collect ["name" cons '()]]
   #:once-each
   [("-F" "--only-f") 'only-f
                      "Only do F's"
                      #:record]
   [("-A") 'A
           ("Only do F's"
            "Mandatory unless -F specified.")
           #:mandatory-unless (λ (flags) (member 'only-f flags))
           #:record]
   [("-N" "--only-not-f") 'only-not-f
                          "Only do not F's"
                          #:record
                          #:conflicts '(only-f)]
   #:args (stuff)))

#!/usr/bin/env scheme-script
;; opencog-cogutil.scm
;;
;; OpenCog Cogutil - Scheme/Lisp Utility Library
;; A collection of utility functions for OpenCog framework
;;
;; This single-file implementation demonstrates Scheme's strengths:
;; - S-expressions for code and data
;; - First-class functions and closures
;; - Macros for meta-programming
;; - Tail recursion optimization
;; - Homoiconicity (code is data)
;;
;; Note: This implementation uses some functions that may require:
;; - SRFI-19 for time functions (current-time, time-second, etc.)
;; - SRFI-1 for list utilities (iota, etc.)
;; Most Scheme implementations (Racket, Guile, Chez) include these by default

;; ===== Logger System =====
;; Demonstrates: Records, closures, higher-order functions

(define-record-type logger
  (make-logger-impl name level)
  logger?
  (name logger-name)
  (level logger-level logger-set-level!))

(define (make-logger name level)
  (make-logger-impl name level))

(define *log-levels*
  '((debug . 0)
    (info . 1)
    (warn . 2)
    (error . 3)))

(define (log-level-value level)
  (cdr (assq level *log-levels*)))

(define (log-message logger level message)
  (when (>= (log-level-value level)
            (log-level-value (logger-level logger)))
    (let* ((timestamp (current-timestamp))
           (level-str (symbol->string level)))
      (display (string-append "[" timestamp "] " 
                            (string-upcase level-str) ": " 
                            message "\n")))))

;; Convenience logging functions using currying
(define (log-debug logger message) (log-message logger 'debug message))
(define (log-info logger message) (log-message logger 'info message))
(define (log-warn logger message) (log-message logger 'warn message))
(define (log-error logger message) (log-message logger 'error message))

;; Get current timestamp (simplified)
(define (current-timestamp)
  (let* ((time (current-time))
         (secs (time-second time)))
    (format-time secs)))

(define (format-time secs)
  (let* ((hours (modulo (quotient secs 3600) 24))
         (mins (modulo (quotient secs 60) 60))
         (sec (modulo secs 60)))
    (format #f "~2,'0d:~2,'0d:~2,'0d" hours mins sec)))

;; ===== Configuration Manager =====
;; Demonstrates: Association lists, functional updates

(define (make-config)
  '())

(define (config-set config key value)
  (cons (cons key value)
        (filter (lambda (pair) (not (equal? (car pair) key)))
                config)))

(define (config-get config key . default)
  (let ((pair (assoc key config)))
    (if pair
        (cdr pair)
        (if (null? default) #f (car default)))))

(define (config-has? config key)
  (if (assoc key config) #t #f))

(define (config-dump config)
  (for-each (lambda (pair)
              (display (string-append (car pair) " = " (cdr pair) "\n")))
            (sort-config config)))

(define (sort-config config)
  (list-sort (lambda (a b) (string<? (car a) (car b)))
             config))

;; ===== Timer Utility =====
;; Demonstrates: Closures, mutable state in functional style

(define (make-timer name)
  (let ((start-time #f)
        (elapsed-time #f))
    (lambda (operation . args)
      (case operation
        ((start)
         (set! start-time (current-time)))
        ((stop)
         (when start-time
           (set! elapsed-time (time-difference (current-time) start-time)))
         elapsed-time)
        ((elapsed)
         elapsed-time)
        ((elapsed-secs)
         (if elapsed-time
             (+ (time-second elapsed-time)
                (/ (time-nanosecond elapsed-time) 1e9))
             0.0))
        (else
         (error "Unknown timer operation" operation))))))

;; ===== String Utilities =====
;; Demonstrates: List processing, higher-order functions

(define (string-split str delimiter)
  (define (split-helper s delim)
    (let ((pos (string-index s delim)))
      (if pos
          (cons (substring s 0 pos)
                (split-helper (substring s (+ pos 1) (string-length s)) delim))
          (list s))))
  (filter (lambda (s) (not (string-null? (string-trim s))))
          (split-helper str delimiter)))

(define (string-join strings delimiter)
  (if (null? strings)
      ""
      (fold-left (lambda (acc s)
                   (if (string-null? acc)
                       s
                       (string-append acc delimiter s)))
                 ""
                 strings)))

(define (string-trim s)
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (or (>= i len)
                          (not (char-whitespace? (string-ref s i))))
                      i
                      (loop (+ i 1)))))
         (end (let loop ((i (- len 1)))
                (if (or (< i 0)
                        (not (char-whitespace? (string-ref s i))))
                    (+ i 1)
                    (loop (- i 1))))))
    (substring s start end)))

(define (camel->snake str)
  (list->string
   (let loop ((chars (string->list str))
              (result '())
              (first #t))
     (if (null? chars)
         (reverse result)
         (let ((c (car chars)))
           (if (char-upper-case? c)
               (loop (cdr chars)
                     (cons (char-downcase c)
                           (if first result (cons #\_ result)))
                     #f)
               (loop (cdr chars)
                     (cons c result)
                     #f)))))))

;; ===== Performance Measurement =====
;; Demonstrates: Macros, meta-programming

(define-syntax measure-performance
  (syntax-rules ()
    ((measure-performance name body ...)
     (begin
       (display (string-append "[PERF] Starting: " name "\n"))
       (let* ((start (current-time))
              (result (begin body ...))
              (elapsed (time-difference (current-time) start))
              (secs (+ (time-second elapsed)
                      (/ (time-nanosecond elapsed) 1e9))))
         (display (string-append "[PERF] Completed: " name 
                                " in " (number->string secs) "s\n"))
         result)))))

;; ===== Function Timing Macro =====
;; Demonstrates: Hygienic macros, code transformation

(define-syntax time-it
  (syntax-rules ()
    ((time-it name expr)
     (let* ((start (current-time))
            (result expr)
            (elapsed (time-difference (current-time) start))
            (secs (+ (time-second elapsed)
                    (/ (time-nanosecond elapsed) 1e9))))
       (display (string-append "[TIMER] " name 
                              " took " (number->string secs) 
                              " seconds\n"))
       result))))

;; ===== List Utilities =====
;; Demonstrates: Tail recursion, higher-order functions

(define (filter pred lst)
  (let loop ((lst lst) (result '()))
    (cond
      ((null? lst) (reverse result))
      ((pred (car lst)) (loop (cdr lst) (cons (car lst) result)))
      (else (loop (cdr lst) result)))))

(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc (proc init (car lst)) (cdr lst))))

;; ===== Demonstration Function =====

(define (demonstrate-cogutil)
  (display "======================================================================\n")
  (display "OpenCog Cogutil - Scheme/Lisp Utility Library Demo\n")
  (display "Showcasing: S-expressions, macros, closures, homoiconicity\n")
  (display "======================================================================\n")
  (newline)

  ;; 1. Logger demonstration
  (display "1. Logger Demonstration\n")
  (display "--------------------------------------------------\n")
  (let ((logger (make-logger "CogUtil" 'info)))
    (log-info logger "Cogutil library initialized")
    (log-debug logger "This debug message won't show (level too low)")
    (log-warn logger "This is a warning message")
    (log-error logger "This is an error message")
    
    (logger-set-level! logger 'debug)
    (log-debug logger "Now debug messages are visible"))
  (newline)

  ;; 2. Config demonstration
  (display "2. Configuration Manager (Immutable)\n")
  (display "--------------------------------------------------\n")
  (let* ((config (make-config))
         (config (config-set config "opencog.version" "1.0.0"))
         (config (config-set config "atomspace.enabled" "true"))
         (config (config-set config "cogserver.port" "17001")))
    (display "Configuration loaded:\n")
    (config-dump config)
    (newline)
    (display (string-append "Port setting: " 
                          (config-get config "cogserver.port" "unknown") 
                          "\n")))
  (newline)

  ;; 3. Timer demonstration
  (display "3. Timer Utility (Closure-based)\n")
  (display "--------------------------------------------------\n")
  (let ((timer (make-timer "Processing")))
    (timer 'start)
    (display "Simulating some work...\n")
    ;; Simulate work
    (let loop ((n 1000000) (sum 0))
      (if (> n 0)
          (loop (- n 1) (+ sum n))
          sum))
    (timer 'stop)
    (display (string-append "Timer elapsed: " 
                          (number->string (timer 'elapsed-secs))
                          "s\n")))
  (newline)

  ;; 4. Performance measurement macro
  (display "4. Performance Measurement (Macro)\n")
  (display "--------------------------------------------------\n")
  (measure-performance "Data processing"
    (let loop ((n 100000))
      (if (> n 0)
          (loop (- n 1))
          #t)))
  (newline)

  ;; 5. String utilities
  (display "5. String Utilities\n")
  (display "--------------------------------------------------\n")
  (display "String utilities demonstration:\n")
  (let* ((text "OpenCog,AtomSpace,CogServer,Cogutil")
         (parts (string-split text #\,)))
    (display "Split result:\n")
    (for-each (lambda (part)
                (display (string-append "  - " part "\n")))
              parts)
    (let ((joined (string-join parts " + ")))
      (display (string-append "Joined: " joined "\n")))
    
    (display (string-append "Uppercase: " (string-upcase "opencog rocks") "\n"))
    (display (string-append "Lowercase: " (string-downcase "OPENCOG ROCKS") "\n"))
    (display (string-append "Trimmed: '" (string-trim "  spaced out  ") "'\n"))
    
    ;; Scheme-specific: Case conversions
    (display (string-append "camelCase → snake_case: " 
                          (camel->snake "myVariableName") "\n")))
  (newline)

  ;; 6. Macro demonstration
  (display "6. Macro-Based Timing\n")
  (display "--------------------------------------------------\n")
  (let ((result (time-it "fibonacci"
                         (letrec ((fib (lambda (n)
                                        (if (<= n 1)
                                            n
                                            (+ (fib (- n 1))
                                               (fib (- n 2)))))))
                           (fib 20)))))
    (display (string-append "Fibonacci(20) = " (number->string result) "\n")))
  (newline)

  ;; 7. List processing (Scheme-specific)
  (display "7. List Processing (Scheme-specific)\n")
  (display "--------------------------------------------------\n")
  (let* ((squares (map (lambda (x) (* x x)) '(0 1 2 3 4 5 6 7 8 9)))
         (evens (filter even? (iota 20))))
    (display "Squares: ")
    (write squares)
    (newline)
    (display "Even numbers: ")
    (write evens)
    (newline))
  (newline)

  ;; 8. Homoiconicity demonstration
  (display "8. Homoiconicity (Code as Data)\n")
  (display "--------------------------------------------------\n")
  (let ((code '(+ 1 2 3)))
    (display "Code as data: ")
    (write code)
    (newline)
    (display (string-append "Evaluated: " 
                          (number->string (eval code (interaction-environment))) 
                          "\n")))
  (newline)

  ;; 9. Closures
  (display "9. Closures (Lexical Scoping)\n")
  (display "--------------------------------------------------\n")
  (let ((counter (let ((count 0))
                   (lambda (operation)
                     (case operation
                       ((increment) (set! count (+ count 1)) count)
                       ((get) count)
                       ((reset) (set! count 0) count))))))
    (display (string-append "Initial: " (number->string (counter 'get)) "\n"))
    (display (string-append "After increment: " 
                          (number->string (counter 'increment)) "\n"))
    (display (string-append "After increment: " 
                          (number->string (counter 'increment)) "\n"))
    (display (string-append "Get value: " (number->string (counter 'get)) "\n")))
  (newline)

  (display "Cogutil demonstration complete!\n")
  (display "======================================================================\n")
  (display "Scheme/Lisp strengths demonstrated:\n")
  (display "  ✓ S-expressions (code and data unified)\n")
  (display "  ✓ Hygienic macros (compile-time meta-programming)\n")
  (display "  ✓ First-class functions and closures\n")
  (display "  ✓ Tail recursion optimization\n")
  (display "  ✓ Homoiconicity (code is data)\n")
  (display "  ✓ Minimalist elegance\n")
  (display "  ✓ REPL-driven development\n")
  (display "======================================================================\n"))

;; Run demonstration
(demonstrate-cogutil)

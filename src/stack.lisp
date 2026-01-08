;;;; -*- mode: lisp; syntax: common-lisp; base: 10; coding: utf-8-unix; external-format: (:utf-8 :eol-style :lf); -*-
;;;; stack.lisp --- useful utilities for working with the stack on LispWorks
;;;; from: https://github.com/tfeb/tfeb-lisp-implementation-hax/tree/main/lw

(uiop:define-package #:meria/src/stack
  (:use #:cl
        #:marie)
  (:import-from #:conditions #:stack-overflow)
  (:import-from #:hcl #:current-stack-length))

(in-package #:meria/src/stack)

(defv *stack-limit* (current-stack-length)
  "How big the stack is allowed to get by default.

This should be a real, T (always extend), or NIL (never extend).")

;; TODO
(defm with-stack-extensions ((&key
                                (limit '*stack-limit* limitp)
                                (use-stack-limit nil))
                             &body forms)
  "Control stack extensions in LW.

If LIMIT is given this is the limit (see *STACK-LIMIT* for what it can be). If USE-STACK-LIMIT is given as true, then the dynamic value of *STACK-LIMIT* will be used instead: this overrides any LIMIT value."
  ;; It is tempting to allow the limit to be a predicate, but I think
  ;; calling functions at this point is probably not a good idea.
  (when (and use-stack-limit limitp)
    (warn "both LIMIT and USE-STACK-LIMIT given: will use *STACK-LIMIT*"))
  (if use-stack-limit
      `(handler-bind ((stack-overflow
                        (lambda (c)
                          (when (and *stack-limit*
                                     (or (eq *stack-limit* t)
                                         (< (current-stack-length) *stack-limit*)))
                            (let ((r (find-restart 'continue c)))
                              (when r (invoke-restart r)))))))
         ,@forms)
      `(let ((,g!lim ,limit))
         (check-type ,g!lim (or boolean real))
         (handler-bind ((stack-overflow
                          (lambda (c)
                            (when (and ,g!lim
                                       (or (eq ,g!lim t)
                                           (< (current-stack-length) ,g!lim)))
                              (let ((r (find-restart 'continue c)))
                                (when r (invoke-restart r)))))))
           ,@forms))))

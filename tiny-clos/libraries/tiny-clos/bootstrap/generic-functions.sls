; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************
;
; port to R6RS -- 2007 Christian Sloma
; 

(library (clos bootstrap generic-functions)

  (export make
          initialize
          allocate-instance
          compute-getter-and-setter
          compute-precedence-list
          compute-slots
          add-method
          compute-apply-generic
          compute-methods
          compute-method-more-specific?
          compute-apply-methods)

  (import (rnrs)
          (clos bootstrap standard-classes)
          (clos introspection)
          (clos std-protocols make)
          (clos std-protocols allocate-instance)
          (clos std-protocols initialize)
          (clos std-protocols class-initialization)
          (clos std-protocols add-method)
          (clos std-protocols generic-invocation))

  (define make
    (bootstrap-make <generic>))
  
  (define initialize                    
    (bootstrap-make <generic>))
  
  (define allocate-instance             
    (bootstrap-make <generic>))
  
  (define compute-getter-and-setter     
    (bootstrap-make <generic>))
  
  (define compute-precedence-list       
    (bootstrap-make <generic>))
  
  (define compute-slots                 
    (bootstrap-make <generic>))
  
  (define add-method
    (bootstrap-make <generic>))
  
  (define compute-apply-generic         
    (bootstrap-make <generic>))
  
  (define compute-methods               
    (bootstrap-make <generic>))
  
  (define compute-method-more-specific? 
    (bootstrap-make <generic>))
  
  (define compute-apply-methods         
    (bootstrap-make <generic>))

  (define bootstrap-add-method 
    (begin

      (register-generic-invocation-generics! 
       compute-apply-generic
       compute-apply-methods
       compute-methods
       compute-method-more-specific?)

      (lambda (entity method)
        (let ((class (class-of entity)))
          (cond 
            ((eq? class <generic>)
             (generic-add-method entity method generic-compute-apply-generic))
            (else
             (error 'bootstrap-add-method
                    "cannot add method to instance of class ~a" class)))))))

  (bootstrap-add-method make
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (call-next-method class . init-args)
                      (class-make class init-args 
                                  allocate-instance initialize))))

  (bootstrap-add-method allocate-instance
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (call-next-method class)
                      (class-allocate-instance class))))

  (bootstrap-add-method allocate-instance 
    (bootstrap-make <method>
      'specializers (list <entity-class>)
      'procedure    (lambda (call-next-method entity-class)
                      (entity-class-allocate-instance entity-class))))

  (bootstrap-add-method initialize 
    (bootstrap-make <method>
      'specializers (list <object>)
      'procedure    (lambda (call-next-method object init-args) object)))

  (bootstrap-add-method initialize
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (call-next-method class-inst init-args)
                      (call-next-method)
                      (class-initialize class-inst init-args
                                        compute-precedence-list
                                        compute-slots
                                        compute-getter-and-setter))))

  (bootstrap-add-method initialize
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (call-next-method generic-inst init-args)
                      (call-next-method)
                      (generic-initialize generic-inst init-args))))

  (bootstrap-add-method initialize 
    (bootstrap-make <method>
      'specializers (list <method>)
      'procedure    (lambda (call-next-method method-inst init-args)
                      (call-next-method)
                      (method-initialize method-inst init-args))))

  (bootstrap-add-method compute-precedence-list
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (call-next-method class)
                      (class-compute-precedence-list class))))

  (bootstrap-add-method compute-slots 
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (call-next-method class)
                      (class-compute-slots class))))

  (bootstrap-add-method compute-getter-and-setter
    (bootstrap-make <method>
      'specializers (list <class>)
      'procedure    (lambda (call-next-method class slot allocator)
                      (class-compute-getter-and-setter class slot allocator))))

  (bootstrap-add-method add-method
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (call-next-method entity method)
                      (generic-add-method entity method compute-apply-generic))))

  (bootstrap-add-method compute-apply-generic
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (call-next-method generic)
                      (generic-compute-apply-generic generic))))

  (bootstrap-add-method compute-methods
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (call-next-method generic)
                      (generic-compute-methods generic))))

  (bootstrap-add-method compute-method-more-specific?
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (call-next-method generic)
                      (generic-compute-method-more-specific? generic))))

  (bootstrap-add-method compute-apply-methods
    (bootstrap-make <method>
      'specializers (list <generic>)
      'procedure    (lambda (call-next-method generic)
                      (generic-compute-apply-methods generic))))

  ) ;; library (clos bootstrap generic-functions)

#lang racket
(require rackunit)
(require 2htdp/universe)   
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")


(provide
 SBlock<%>)
 
;; A SBlock is an object of any class that implements SBlock<%> 

;;INTERFACE:
 
(define SBlock<%>
  (interface (SWidget<%>)

    ;; get-team : -> ListOfSBlock<%>
    ;; RETURNS: the teammates of this sblock
    get-team

    ;; add-teammate: SBlock -> Void
    ;; EFFECT: adds the given sblock to this block's team
    add-teammate

    ;; sblock-x : -> Integer
    ;; sblock-y : -> Integer
    ;; RETURNS: the x or y coordinates of this sblock
    sblock-x
    sblock-y

    
    ;; make-team : ListOfSBlock -> Void
    ;; GIVEN :list of block
    ;; EFFECT : list of block added to the team
    make-team

    ;; get-sblock : -> Int
    ;; RETURNS : block number
    get-sblock

    ;; add-neighbor : SBlock -> void
    ;; GIVEN : the block to be added
    ;; EFFECT : block is added to the team
    add-neighbor
    

    ;; block-after-drag : Int Int Int Int -> Void
    ;; GIVEN : x and y coordinates of selected block and the block
    ;; which is to be dragged
    ;; EFFECT : block considered as teammate
    block-after-drag
))
   
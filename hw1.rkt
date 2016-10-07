#lang dssl

#| Cem Ozer
HW1: Three Dictionaries
Due: Thursday, Oct. 13 at 11:59 PM, via Canvas

** You may work on your own or with one (1) partner. **
|#

;;;
;;; ACCOUNT DEFINITIONS
;;;

;; Account-Id is Natural

;; An Account is (account Account-Id String Number)
(define-struct account (id owner balance))
;; where
;;   `id` is the account number,
;;   `owner` is the name of the account holder, and
;;   `balance` is the balance.

;; Examples:
(define ACCOUNT0 (account 0  "Alan Turing"    16384))
(define ACCOUNT1 (account 8  "Grace Hopper"   32768))
(define ACCOUNT2 (account 16 "Ada Lovelace"   32))
(define ACCOUNT3 (account 24 "David Parnas"   2048))
(define ACCOUNT4 (account 32 "Barbara Liskov" 8192))
(define ACCOUNT5 (account 40 "Donald Knuth"   1024))

;; Account -> Account
;; Copies an account structure.
(define (account-copy old-account)
  (account (account-id old-account)
           (account-owner old-account)
           (account-balance old-account)))

;; Number Account -> Void
;; Adds the given amount to the given account’s balance.
(define (account-credit! amount account)
  (set-account-balance! account (+ (account-balance account) amount)))

;; ^ FILL IN YOUR DEFINITION FROM HW0

;; Number Account Account -> Void
;; Transfers the specified amount from the first account to the second.
(define (account-transfer! amount from to)
  (set-account-balance! from (- (account-balance from) amount))
  (account-credit! amount to))

;; ^ FILL IN YOUR DEFINITION FROM HW0


;;;
;;; THE LINKED-LIST REPRESENTATION
;;;

;; A List-Ledger is one of:
;; - (nil)
;; - (node Account List-Ledger)
;; where the `account-id` values are unique.
(define-struct nil ())
(define-struct node (element link))

;; Example:
(define LIST-LEDGER
  (node ACCOUNT0
        (node ACCOUNT1
              (node ACCOUNT2
                    (node ACCOUNT3
                          (node ACCOUNT4
                                (node ACCOUNT5
                                      (nil))))))))

;; Account-Id List-Ledger -> [Or #false Account]
;; Finds the account with the given `account-id` or #false if the account
;; doesn’t exist.
(define (list-lookup id ledger)
  (define 
;; ^ YOUR DEFINITION HERE

(check-expect (list-lookup 0 LIST-LEDGER)
              ACCOUNT0)
(check-expect (list-lookup 8 LIST-LEDGER)
              ACCOUNT1)
(check-expect (list-lookup 16 LIST-LEDGER)
              ACCOUNT2)
(check-expect (list-lookup 20 LIST-LEDGER)
              #false)
(check-expect (list-lookup 24 LIST-LEDGER)
              ACCOUNT3)
(check-expect (list-lookup 32 LIST-LEDGER)
              ACCOUNT4)
(check-expect (list-lookup 40 LIST-LEDGER)
              ACCOUNT5)
(check-expect (list-lookup 48 LIST-LEDGER)
              #false)


;;;
;;; THE BINARY SEARCH TREE REPRESENTATION
;;;

;; A Bst-Ledger is one of
;; - (leaf)
;; - (branch Bst-Ledger Account Bst-Ledger)
;; where for a branch (branch l acct r), all the account-ids
;; l are less than (account-id acct), and all the account-ids
;; in r are greater than (account-id acct). (This is the binary
;; search tree property.)
(define-struct leaf ())
(define-struct branch (left element right))

;; Example:
(define BST-LEDGER
  (branch (branch (leaf)
                  ACCOUNT0
                  (branch (leaf) ACCOUNT1 (leaf)))
          ACCOUNT2
          (branch (branch (leaf) ACCOUNT3 (leaf))
                  ACCOUNT4
                  (branch (leaf) ACCOUNT5 (leaf)))))

;; Account-Id Bst-Ledger -> [Or #false Account]
;; Finds the account with the given `account-id` or #false if the account
;; doesn’t exist.
(define (bst-lookup id ledger)
  ...)
;; ^ YOUR DEFINITION HERE

(check-expect (bst-lookup 0 BST-LEDGER)
              ACCOUNT0)
(check-expect (bst-lookup 8 BST-LEDGER)
              ACCOUNT1)
(check-expect (bst-lookup 16 BST-LEDGER)
              ACCOUNT2)
(check-expect (bst-lookup 20 BST-LEDGER)
              #false)
(check-expect (bst-lookup 24 BST-LEDGER)
              ACCOUNT3)
(check-expect (bst-lookup 32 BST-LEDGER)
              ACCOUNT4)
(check-expect (bst-lookup 40 BST-LEDGER)
              ACCOUNT5)
(check-expect (bst-lookup 48 BST-LEDGER)
              #false)


;;;
;;; THE SORTED VECTOR REPRESENTATION
;;;

;; A Vec-Ledger is [Vector-of Account]
;; where the account-id values are strictly ascending (and thus unique).

;; Example:
(define VEC-LEDGER (vector ACCOUNT0 ACCOUNT1 ACCOUNT2
                           ACCOUNT3 ACCOUNT4 ACCOUNT5))

;; Account-Id Vec-Ledger -> [Or #false Account]
;; Finds the account with the given `account-id` or #false if the account
;; doesn’t exist.
(define (vec-lookup id ledger)
  ...)
;; ^ YOUR DEFINITION HERE

(check-expect (vec-lookup 0 VEC-LEDGER)
              ACCOUNT0)
(check-expect (vec-lookup 8 VEC-LEDGER)
              ACCOUNT1)
(check-expect (vec-lookup 16 VEC-LEDGER)
              ACCOUNT2)
(check-expect (vec-lookup 20 VEC-LEDGER)
              #false)
(check-expect (vec-lookup 24 VEC-LEDGER)
              ACCOUNT3)
(check-expect (vec-lookup 32 VEC-LEDGER)
              ACCOUNT4)
(check-expect (vec-lookup 40 VEC-LEDGER)
              ACCOUNT5)
(check-expect (vec-lookup 48 VEC-LEDGER)
              #false)


;; Number Account-Id Account-Id Vec-Ledger -> Void
;; Transfers `amount` from account number `from-id` to account number
;; `to-id`. Calls `error` if an account isn't found.
(define (transfer! amount from-id to-id ledger)
  ...)
;; ^ YOUR DEFINITION HERE

(check-expect
  (begin
    (define ledger (ledger-copy VEC-LEDGER))
    (transfer! 2000 32 24 ledger)
    ledger)
  (vector ACCOUNT0
          ACCOUNT1
          ACCOUNT2
          (account 24 "David Parnas" 4048)
          (account 32 "Barbara Liskov" 6192)
          ACCOUNT5))

(check-error
  (begin
    (define ledger (ledger-copy VEC-LEDGER))
    (transfer! 2000 31 24 ledger))
  "Account not found")

;; Vec-Ledger -> Vec-Ledger
;; Makes a copy of a ledger, deep-copying the accounts within it.
;; This is useful for testing.
(define (ledger-copy ledger)
  (build-vector (vector-length ledger)
                (lambda (i) (account-copy (vector-ref ledger i)))))
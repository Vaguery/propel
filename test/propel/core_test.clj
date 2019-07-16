(ns propel.core-test
  (:require [clojure.test :refer :all]
            [propel.core :refer :all]))

;; instruction behavior

; {'in1 in1
;  'integer_+ integer_+
;  'integer_- integer_-
;  'integer_* integer_*
;  'integer_% integer_%
;  'integer_= integer_=
;  'integer_dup integer_dup
;  'integer_swap integer_swap

;  'exec_dup exec_dup
;  'exec_swap exec_swap
;  'exec_if exec_if

;  'boolean_and boolean_and
;  'boolean_or boolean_or
;  'boolean_not boolean_not
;  'boolean_= boolean_=
;  'boolean_dup boolean_dup
;  'boolean_swap boolean_swap

;  'string_= string_=
;  'string_dup string_dup
;  'string_swap string_swap
;  'string_take string_take
;  'string_drop string_drop
;  'string_reverse string_reverse
;  'string_concat string_concat
;  'string_length string_length
;  'string_includes? string_includes?
;  })

(defn load-state
  [stackmap]
  (reduce
    (fn [new-state [k v]]
      (assoc new-state k v))
    empty-push-state
    stackmap
    ))

(deftest integer_functions
  (testing "integer_+"
    (is
      (= '(0 999)
      (:integer (integer_+ (load-state {:integer '(9 -9 999)})))))
      )
  (testing "integer_-"
    (is
      (= '(90 999)
      (:integer (integer_- (load-state {:integer '(9 99 999)})))))
      )
  (testing "integer_*"
    (is
      (= '(891 999)
      (:integer (integer_* (load-state {:integer '(9 99 999)})))))
      )
  (testing "integer_%"
    (is
      (= '(11 999)
      (:integer (integer_% (load-state {:integer '(9 99 999)})))))
    (is
      (= '(1 999) ;; % returns 1 when dividing by zero
      (:integer (integer_% (load-state {:integer '(0 99 999)})))))
    (is
      (= '(0 999) ;; checks arg order
      (:integer (integer_% (load-state {:integer '(9 0 999)})))))
      )
  (testing "integer_="
    (is
      (= '(false)
      (:boolean (integer_= (load-state {:integer '(9 99 999)})))))
    (is
      (= '(true)
      (:boolean (integer_= (load-state {:integer '(9 9 999)})))))
      )
  (testing "integer_dup"
    (is
      (= '(9 9 99 999)
      (:integer (integer_dup (load-state {:integer '(9 99 999)})))))
      )
  (testing "integer_swap"
    (is
      (= '(99 9 999)
      (:integer (integer_swap (load-state {:integer '(9 99 999)})))))
    (is
      (= '(9)
      (:integer (integer_swap (load-state {:integer '(9)})))))
      ))

(deftest exec_functions
  (testing "exec_dup"
    (is
      (= '(9 9 99 999)
      (:exec (exec_dup (load-state {:exec '(9 99 999)})))))
    (is
      (= '((9 99) (9 99) 999)
      (:exec (exec_dup (load-state {:exec '((9 99) 999)})))))
      )
  (testing "exec_swap"
    (is
      (= '(99 9 999)
      (:exec (exec_swap (load-state {:exec '(9 99 999)})))))
    (is
      (= '(999 (9 99))
      (:exec (exec_swap (load-state {:exec '((9 99) 999)})))))
      )
  (testing "exec_if"
    (is
      (= '(99 999)
      (:exec (exec_if (load-state {:exec '(9 99 999)
                                   :boolean '(false true)})))))
    (is
      (= '(true)
      (:boolean (exec_if (load-state {:exec '(9 99 999)
                                   :boolean '(false true)})))))
    (is
      (= '(9 999)
      (:exec (exec_if (load-state {:exec '(9 99 999)
                                   :boolean '(true false)})))))
    (is
      (= '(false)
      (:boolean (exec_if (load-state {:exec '(9 99 999)
                                   :boolean '(true false)})))))
                                   ))

(deftest boolean_functions
  (testing "boolean_and"
    (is
      (= '(true)
      (:boolean (boolean_and (load-state {:boolean '(true true)})))))
    (is
      (= '(false)
      (:boolean (boolean_and (load-state {:boolean '(false true)})))))
    (is
      (= '(false)
      (:boolean (boolean_and (load-state {:boolean '(true false)})))))
    (is
      (= '(false)
      (:boolean (boolean_and (load-state {:boolean '(false false)})))))
      )
  (testing "boolean_or"
    (is
      (= '(true)
      (:boolean (boolean_or (load-state {:boolean '(true true)})))))
    (is
      (= '(true)
      (:boolean (boolean_or (load-state {:boolean '(false true)})))))
    (is
      (= '(true)
      (:boolean (boolean_or (load-state {:boolean '(true false)})))))
    (is
      (= '(false)
      (:boolean (boolean_or (load-state {:boolean '(false false)})))))
      )
  (testing "boolean_="
    (is
      (= '(true false)
      (:boolean (boolean_= (load-state {:boolean '(true true false)})))))
    (is
      (= '(true false)
      (:boolean (boolean_= (load-state {:boolean '(false false false)})))))
    (is
      (= '(false false)
      (:boolean (boolean_= (load-state {:boolean '(false true false)})))))
      )
  (testing "boolean_swap"
    (is
      (= '(true false false)
      (:boolean (boolean_swap (load-state {:boolean '(false true false)})))))
      ))


(deftest string_functions
  (testing "string_="
    (is
      (= '(false)
      (:boolean (string_= (load-state {:string '("foo" "bar" "baz")})))))
    (is
      (= '(true)
      (:boolean (string_= (load-state {:string '("foo" "foo" "baz")})))))
      )
  (testing "string_dup"
    (is
      (= '("foo" "foo" "bar" "baz")
      (:string (string_dup (load-state {:string '("foo" "bar" "baz")})))))
      )
  (testing "string_swap"
    (is
      (= '("bar" "foo" "baz")
      (:string (string_swap (load-state {:string '("foo" "bar" "baz")})))))
      )
  (testing "string_take"
    (is
      (= '("fo")
      (:string (string_take (load-state {:string '("foo") :integer '(2)})))))
    (is
      (= '("")
      (:string (string_take (load-state {:string '("foo") :integer '(-2)})))))
    (is
      (= '("foo")
      (:string (string_take (load-state {:string '("foo") :integer '(22)})))))
      )
  (testing "string_drop"
    (is
      (= '("o")
      (:string (string_drop (load-state {:string '("foo") :integer '(2)})))))
    (is
      (= '("foo")
      (:string (string_drop (load-state {:string '("foo") :integer '(-2)})))))
    (is
      (= '("")
      (:string (string_drop (load-state {:string '("foo") :integer '(22)})))))
      )
  (testing "string_reverse"
    (is
      (= '("oof" "bar" "baz")
      (:string (string_reverse (load-state {:string '("foo" "bar" "baz")})))))
      )
  (testing "string_concat"
    (is
      (= '("barfoo" "baz") ;; note argument order!
      (:string (string_concat (load-state {:string '("foo" "bar" "baz")})))))
      )
  (testing "string_length"
    (is
      (= '(6)
      (:integer (string_length (load-state {:string '("foo123" "bar" "baz")})))))
      )
  (testing "string_includes?"
    (is
      (= '(false)
      (:boolean (string_includes? (load-state {:string '("foo" "bar" "baz")})))))
    (is
      (= '(true)
      (:boolean (string_includes? (load-state {:string '("foo" "foo" "baz")})))))
    (is
      (= '(true) ;; note argument order!
      (:boolean (string_includes? (load-state {:string '("f" "foo" "baz")})))))
    (is
      (= '(false) ;; note argument order!
      (:boolean (string_includes? (load-state {:string '("foo" "f" "baz")})))))
      )
)

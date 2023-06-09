;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.tools.test.version
  (:require [clojure.test :refer :all]
            [org.soulspace.tools.version :refer :all]))

(deftest numeric-true
  (is (digits-only? "1"))
  (is (digits-only? "10"))
  (is (digits-only? "101")))


(deftest numeric-false
  (is (not (digits-only? nil)))
  (is (not (digits-only? "10a")))
  (is (not (digits-only? "abc")))
  (is (not (digits-only? "ten"))))


(deftest compare-ver-equals
  (is (= (compare-revision nil nil) 0))
  (is (= (compare-revision "0" "0") 0))
  (is (= (compare-revision "1" "1") 0))
  (is (= (compare-revision "a" "a") 0)))


(deftest compare-ver-greater
  (is (> (compare-revision "1" nil) 0))
  (is (> (compare-revision "2" "1") 0))
  (is (> (compare-revision "10" "2") 0)))


(deftest compare-ver-less
  (is (< (compare-revision nil "1") 0))
  (is (< (compare-revision "1" "2") 0))
  (is (< (compare-revision "2" "10") 0)))


(deftest compare-same
  (is (same-version? (new-version nil) (new-version nil)))
  (is (same-version? (new-version "1") (new-version "1")))
  (is (same-version? (new-version "1.1") (new-version "1.1")))
  (is (same-version? (new-version "1.1.1") (new-version "1.1.1")))
  (is (same-version? (new-version "1.1.1rc1") (new-version "1.1.1rc1")))
  (is (same-version? (new-version "1.1.1pl2") (new-version "1.1.1pl2"))))


(deftest compare-greater
  (is (greater-version? (new-version "1") nil))
  (is (greater-version? (new-version "2") (new-version "1")))
  (is (greater-version? (new-version "2") (new-version "1.1")))
  (is (greater-version? (new-version "2.1") (new-version "1.1")))
  (is (greater-version? (new-version "12.1") (new-version "11.1")))
  (is (greater-version? (new-version "2.a") (new-version "1.a")))
  (is (greater-version? (new-version "2.b") (new-version "2.a")))
  (is (greater-version? (new-version "2.b") (new-version "2.a")))
  (is (greater-version? (new-version "2.2") (new-version "2.1.3")))
  (is (greater-version? (new-version "2.2-beta") (new-version "2.2-alpha")))
  (is (greater-version? (new-version "2.b") (new-version "2.a")))
  (is (greater-version? (new-version "10") (new-version "2")))
  (is (greater-version? (new-version "1.10") (new-version "1.2"))))


(deftest compare-lesser
  (is (lesser-version? (new-version nil) (new-version "1")))
  (is (lesser-version? (new-version "1") (new-version "2")))
  (is (lesser-version? (new-version "1.1") (new-version "2")))
  (is (lesser-version? (new-version "1.1") (new-version "2.1")))
  (is (lesser-version? (new-version "1.1") (new-version "1.2")))
  (is (lesser-version? (new-version "1.1") (new-version "1.1.1")))
  (is (lesser-version? (new-version "1.1-alpha") (new-version "1.1-beta")))
  (is (lesser-version? (new-version "2") (new-version "10")))
  (is (lesser-version? (new-version "1.2") (new-version "1.10"))))


(deftest match-version-true
  (is (true? (version-match? nil "1.0.0")))
  (is (true? (version-match? "" "1.0.0")))
  (is (true? (version-match? "1.0.0" "1.0.0"))))

(deftest match-version-false
  (is (false? (version-match? "1.1.0" "1.0.0"))))

(deftest contains-version-true
  (is (true? (contains-version? (new-version-range "1.0.0" "2.0.0") (new-version "1.5.0"))))
  (is (true? (contains-version? (new-version-range nil "2.0.0") (new-version "1.5.0"))))
  (is (true? (contains-version? (new-version-range "1.0.0") (new-version "1.5.0"))))
  (is (true? (contains-version? (new-version-range) (new-version "1.5.0"))))
  (is (true? (contains-version? (new-version-range nil) (new-version "1.5.0"))))
  (is (true? (contains-version? (new-version-range nil nil) (new-version "1.5.0")))))


(deftest contains-version-false
  (is (false? (contains-version? (new-version-range "1.0.0" "2.0.0") (new-version "0.5.0"))))
  (is (false? (contains-version? (new-version-range "1.0.0" "2.0.0") (new-version "2.0.0"))))
  (is (false? (contains-version? (new-version-range nil "2.0.0") (new-version "2.5.0"))))
  (is (false? (contains-version? (new-version-range nil "2.0.0") (new-version "2.0.0"))))
  (is (false? (contains-version? (new-version-range "1.0.0") (new-version "0.5.0"))))
  (is (false? (contains-version? (new-version-range "1.0.0") (new-version "0.9.9"))))
  (is (false? (contains-version? (new-version-range "1.0.0") (new-version nil))))
  (is (false? (contains-version? (new-version-range nil nil) (new-version nil))))
  (is (false? (contains-version? (new-version-range) (new-version nil))))
  (is (false? (contains-version? (new-version-range) (new-version "")))))

(comment
  (run-tests))

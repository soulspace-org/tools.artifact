;;
;;   Copyright (c) Ludger Solbach. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file license.txt at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.
;;

(ns org.soulspace.tools.version
  (:require [clojure.string :as str]))

;
; version comparison utils
;
(defn digits-only?
  "Tests if x contains only digits."
  [x]
  (and (not (nil? x)) (re-matches #"^[0-9]+$" x)))

(defn compare-revision
  "Compares two revision components."
  [c1 c2]
  (if (and (digits-only? c1) (digits-only? c2)) ; compare numerically or lexically?
    (compare (Long/valueOf c1) (Long/valueOf c2))
    (compare c1 c2)))

(defn split-version-string
  "Splits a version string into revision components."
  ([version]
   (str/split version #"[.]"))
  ([version re]
   (str/split version re)))

; TODO still needed?
(defn version-match?
  "Match a version against a version pattern"
  [pattern version]
  (or (nil? pattern)
      (empty? pattern)
      ; TODO use matching
      (= pattern version)))

;
; Version abstractions
;
(defprotocol Version
  "Protocol for a version (of an artifact)."
  (compare-version [v1 v2] "split versions and compare components until difference or equality is established.")
  (lesser-version? [v1 v2] "Returns true if the first version is less than the second version.")
  (greater-version? [v1 v2] "Returns true if the first version is greater than the second version.")
  (same-version? [v1 v2] "Returns true if both versions are equal.")
  (unset-version? [v] "Returns true, if the version is not set (nil or empty string)."))

(defprotocol IncrementableVersion
  "Protocol for incrementable versions."
  (inc-version [v] "Increases the current version by one increment."))

(defprotocol VersionRange
  "Protocol for a version range defined by two versions, from and to.
  The from version is included in the range, the to version is excluded. ([from, to[)"
  (contains-version? [range version]))

(defrecord VersionImpl [string]
  Version
  (compare-version [version1 version2]
    (let [v1 (:string version1)
          v2 (:string version2)]
      (if (or (nil? v1) (nil? v2))
        (compare v1 v2)
        (loop [c1 (split-version-string v1)
               c2 (split-version-string v2)]
          ; split the versions and compare them part for part
          (if (and (seq c1) (seq c1))
            (if (not= (first c1) (first c2))
              (compare-revision (first c1) (first c2))
              (recur (rest c1) (rest c2)))
            (compare-revision (first c1) (first c2)))))))
  (lesser-version? [v1 v2] (< (compare-version v1 v2) 0))
  (greater-version? [v1 v2] (> (compare-version v1 v2) 0))
  (same-version? [v1 v2] (= (compare-version v1 v2) 0))
  (unset-version? [v] (empty? (:string v)))
  VersionRange
  (contains-version? [v1 v2] (same-version? v1 v2)))

(defn create-version [string]
  "Creates a new version."
  (VersionImpl. string))

(defmulti new-version type)
(defmethod new-version String [v] (create-version v))
(defmethod new-version VersionImpl [v] v)
(defmethod new-version nil [v] (VersionImpl. nil))

(defrecord VersionRangeImpl [from to]
  VersionRange
  (contains-version? [range version]
    (cond
      (or (nil? version) (unset-version? version)) false
      (and (or (nil? (:from range)) (unset-version? (:from range))) (or (nil? (:to range)) (unset-version? (:to range)))) true
      (and (or (nil? (:from range)) (unset-version? (:from range))) (lesser-version? version (:to range))) true
      (and (or (nil? (:to range)) (unset-version? (:to range))) (not (lesser-version? version (:from range)))) true
      (and (not (lesser-version? version (:from range))) (lesser-version? version (:to range))) true
      :default false)))

(defn create-version-range
  "Creates a new version range."
  ([]
   (VersionRangeImpl. nil nil))
  ([from]
   (VersionRangeImpl. (new-version from) nil))
  ([from to]
   (VersionRangeImpl. (new-version from) (new-version to))))

(defmulti new-version-range1 type)
(defmethod new-version-range1 String [arg] (create-version-range arg))
(defmethod new-version-range1 clojure.lang.IPersistentVector [[from to] arg] (create-version-range from to))
(defmethod new-version-range1 clojure.lang.ISeq [[from to] arg] (create-version-range from to))
(defmethod new-version-range1 VersionImpl [arg] arg)
(defmethod new-version-range1 VersionRangeImpl [arg] arg)
(defmethod new-version-range1 nil [arg] (create-version-range nil))

(defn new-version-range
  "Builds a new version range."
  ([]
   (create-version-range))
  ([arg]
   (new-version-range1 arg))
  ([from to]
   (create-version-range from to)))

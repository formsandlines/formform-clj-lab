(ns formform-clj-lab.exp6-2
  (:require
    [clojure.core.logic :as l :refer [run run* succeed fail s# u# defne
                                      conde fresh lcons llist]]))

;; 29.07.2022

;; Unfortunately, core.logic has no CLP(Set) implementation yet,
;; so unification in sets with logic variables is not possible
;; see https://github.com/clojure/core.logic/wiki/CLP(Set)

#_:clj-kondo/ignore
(defne pa-j1 "position" [eqL eqR]
  ([ #{ #{p} p } [ :true ] ]))

(run* [q]
  (pa-j1 #{ #{'a} 'a } q)) ;=> ()

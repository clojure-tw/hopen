(ns hopen.runner
  (:require  [cljs.test :as t :include-macros true]
             [doo.runner :refer-macros [doo-all-tests doo-tests]]
             [hopen.core-test]))

(doo-tests 'hopen.core-test)

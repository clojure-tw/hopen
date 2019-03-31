(ns hopen.runner
  (:require  [cljs.test :as t :include-macros true]
             [doo.runner :refer-macros [doo-all-tests doo-tests]]
             [hopen.renderer.xf-test]
             [hopen.syntax.silly-j-test]
             [hopen.util-test]))

(doo-tests 'hopen.renderer.xf-test
           'hopen.syntax.silly-j-test
           'hopen.util-test)

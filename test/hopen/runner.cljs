(ns hopen.runner
  (:require  [cljs.test :as t :include-macros true]
             [doo.runner :refer-macros [doo-all-tests doo-tests]]
             [hopen.renderer.xf-test]
             [hopen.parser.util-test]
             [hopen.util-test]))

(doo-tests 'hopen.renderer.xf-test
           'hopen.parser.util-test
           'hopen.util-test)

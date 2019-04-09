(ns hopen.runner
  (:require  [cljs.test :as t :include-macros true]
             [doo.runner :refer-macros [doo-all-tests doo-tests]]
             [pjstadig.humane-test-output]
             [hopen.renderer.xf-test]
             [hopen.syntax.util-test]
             [hopen.util-test]
             [hopen.syntax.mustache-test]))

(doo-tests 'hopen.renderer.xf-test
           'hopen.syntax.util-test
           'hopen.util-test
           'hopen.syntax.mustache-test)

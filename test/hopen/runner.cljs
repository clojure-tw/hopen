(ns hopen.runner
  (:require  [cljs.test :as t :include-macros true]
             [doo.runner :refer-macros [doo-all-tests doo-tests]]
             [hopen.renderer.helper-test]
             [hopen.renderer.xf-test]
             [hopen.syntax.handlebars-test]
             [hopen.syntax.partition-test]
             [hopen.syntax.util-test]
             [hopen.util-test]))

(doo-tests 'hopen.renderer.helper-test
           'hopen.renderer.xf-test
           'hopen.syntax.handlebars-test
           'hopen.syntax.partition-test
           'hopen.syntax.util-test
           'hopen.util-test)

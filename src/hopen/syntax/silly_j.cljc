(ns hopen.syntax.silly-j
  (:require [hopen.renderer.xf :as rxf]
            [instaparse.core :as insta]))

(declare parse)

;; TODO test case
(defn render
  ([template data]
   (render template data '()))
  ([template data other-env]
   (let [new-env (update rxf/default-env :bindings assoc other-env)
         renderer (rxf/renderer (parse template) new-env)]
     (transduce renderer str data))))

(def ebnf
  (insta/parser "
                S       = [STR | SILLYSTR]+
                STR      = #'[^{}]+'
                SILLYSTR =  OBK SILLY CBK
                OBK      = '{'
                CBK      = '}'
                SILLY    = [FCTX | FN]
                FCTX     = '@' ':' ATTR
                ATTR     = #'[^}]'+
                FN       = FNNAME APPLIES*
                APPLIES  = SPC | FCTX | APPLY
                FNNAME   = #'[a-zA-Z0-9\\-]+'
                SPC      = #'\\ '+
                APPLY    = #'^[^@\\ ][^}\\ ]+'
                "
                ))

(defn parse [msg]
  (->> (ebnf msg)
      (insta/transform
       {:STR str
        ;; :FCTX 'hopen/ctx
        :APPLY str
        :FNNAME str
        :ATTR str
        :FCTX (fn [at comma attr]
                (list 'hopen/ctx (keyword attr)))
        :SPC identity
        :FN (fn [first-elm & applies]
              (cons (symbol first-elm)
                    (filter #(not= " " %) applies)
                    ))
        :APPLIES identity
        :SILLY identity
        :SILLYSTR (fn [obrk silly cbrk]
                    silly)
        :S list
        })))

(ns ulvm.core)

;;;;;;;;;;;;;;;;;;;;;;
; examples
;;;;;;;;;;;;;;;;;;;;;;
(defloader :npm 
  "Description of npm loader"
  {:install-deps-command {:all (npm install)}
   :write-deps-command (npm install --save %&)
   :install-loader-command (npm install)})

(defscope :my-scope 
  "description of scope"
  {:module
    {:loader-name :npm
     :module-descriptor {:name "my-module"}}
   :modules {:adder {:loader-name :npm
                     :module-descriptor {:name "my-adder"}}
             :db-saver {:loader-name :npm
                        :module-descriptor {:name "my-db-saver"}}}
   :init {:my-adder ((adder 42))}})

(defflow :add-something
  "description of flow"
  [value]
  (in-scope :my-scope
    (db-saver (my-adder value))))

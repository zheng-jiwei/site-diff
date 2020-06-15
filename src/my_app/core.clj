(ns my-app.core
  (:gen-class)
	(:require [net.cgrand.enlive-html :as eh]
            [clojure.string :as str]
            [clojure.data.json :as json]
           [clojure.data.csv :as csv]
           [clj-http.client :as client]
	)
   (:import (org.jsoup Jsoup)
   (org.jsoup.select Elements)
   (org.jsoup.nodes Element))
)

(def tag_result (atom {}))

(defn- process-meta [meta]
  (let [attrs (:attrs meta)]
    (cond
      (not (nil? (:name attrs))) (swap! tag_result assoc (str/lower-case (str "meta_name_" (:name attrs))) (:content attrs))
      (not (nil? (:http-equiv attrs))) (swap! tag_result assoc (str/lower-case (str "meta_http-equiv_" (:http-equiv attrs))) (:content attrs))
      (not (nil? (:property attrs))) (swap! tag_result assoc (str/lower-case (str "meta_property_" (:property attrs))) (:content attrs))
      (= (count attrs) 1) (swap! tag_result assoc (str/lower-case (str "meta_" (-> attrs keys first))) (-> attrs vals first))
      (and (= (count attrs) 2) (not (nil? (:content attrs))))
      (swap! tag_result assoc (str/lower-case (str "meta_" (-> (dissoc attrs :content) keys first name) "_" (first (vals (dissoc attrs :content))))) (-> attrs :content))
      ;(dorun
      ;  (map #(swap! tag_result assoc (str/lower-case (str "meta_" (name (key %)))) (val %)) attrs)
      ;  )
      ;:else (spit "./error_meta.log" (str "[" (java.util.Date.) "] " meta "\n") :append true)
      :else (prn meta)
      )
    )
)

(defn- anylize-tag-count [obj]
	(dorun
    (map (fn [input]
			(when-let [tag (:tag input)]
				(let [content (:content input)
              tag_count (if (nil? (get @tag_result tag)) 0 (get @tag_result tag))]
						(condp = tag
							:title (swap! tag_result assoc (str "str_" (name tag)) (first content))
							:meta (process-meta input)
              (swap! tag_result assoc tag (+ tag_count 1))
              )
						(anylize-tag-count content)
				)
			)
		) obj))
)

(defn- get-domain-and-port [url]
  (try
    (let [obj (java.net.URL. url)
          protocol (.getProtocol obj)
          domain (.getHost obj)
          port (.getPort obj)
          port (if (= port -1) (if (= protocol "https") 443 80) port)]
      [domain port]
      )
  (catch Exception e
      (prn "parse url error:" url)
    ))
  )


(defn- html-to-json-file [html out-dir]
  (let [data (eh/html-snippet html)]
    (anylize-tag-count data)
    (swap! tag_result assoc "body" (-> data (eh/select [:body]) eh/texts first (str/replace #"\n" "") (str/replace #"\t" "")))
    (swap! tag_result dissoc :body)
    (let [result (reduce (fn [m element]
                        (if (-> element key keyword?)
                           (assoc m (str "count_" (-> element key name)) (-> element val str))
                           (assoc m (-> element key) (-> element val str))
                          )
                   ) {} @tag_result)
          host (get @tag_result "site_host")
          port (get @tag_result "site_port")
          output-name (if (= -1 port) host (str host "-" port))
          ]
      (spit (str out-dir output-name ".json") (json/write-str result))
      )
    )
  )

(defn path-to-json-file [path out-dir]
  (let [filename (.getName (clojure.java.io/file path))]
     (reset! tag_result {})
     (swap! tag_result assoc "site_host" filename)
     (swap! tag_result assoc "site_port" -1)
     (html-to-json-file (slurp path) out-dir)
     )
  )

(defn batch-path-to-json-file [dir out-dir]
  (let [obj-dir (clojure.java.io/file dir)
        files (-> obj-dir file-seq next)
        ]
    (map #(path-to-json-file (str %) out-dir) files)
    )
  )

(defn- json-to-csv-file [dir]
  (let [out-file (str dir "result.csv")
        _ (when (.exists (clojure.java.io/as-file out-file)) (clojure.java.io/delete-file out-file))
        obj-dir (clojure.java.io/file dir)
        files (-> obj-dir file-seq next)
        all-data (map #(json/read-str (slurp %)) files)
        features (distinct (reduce #(apply merge %1 %2) [] (map #(keys %) all-data)))
        out-data (map (fn [data]
                        (reduce (fn [m column]
                                  (conj m (get data column ""))
                                  ) [] features)
                        ) all-data)
        ]
    (with-open [file (clojure.java.io/writer out-file)]
      (csv/write-csv file (cons features out-data))
      )
    )
  )

(defn- url-to-json-file [url out-dir]
	(let [url-info (get-domain-and-port url)
       url-info (if (nil? url-info) ["" -1] url-info)
       ]
   (prn "####" url)
    (try
      (reset! tag_result {})
      (swap! tag_result assoc "site_host" (first url-info))
      (swap! tag_result assoc "site_port" (second url-info))
      (html-to-json-file (.html (.get (Jsoup/connect url))) out-dir)
    (catch Exception e
        (spit "./error-url.log" (str "[" (java.util.Date.) "] " url " " (:via (Throwable->map e)) "\n") :append true)
      ))
     )
  )

(defn batch-url-to-file [path out-dir]
  (try
    (let [urls (str/split (slurp path) #"\n")
          urls (map #(if (str/ends-with? % "\r") (str/replace % "\r" "") %) urls)]
      (dorun (map #(url-to-json-file % out-dir) urls))
      (json-to-csv-file out-dir)
      )
  (catch Exception e
         (prn "### " e)
    ))
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

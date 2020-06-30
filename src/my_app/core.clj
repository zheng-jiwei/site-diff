(ns my-app.core
  (:gen-class)
	(:require [net.cgrand.enlive-html :as eh]
            [clojure.string :as str]
            [clojure.data.json :as json]
           [clojure.data.csv :as csv]
           [clj-http.client :as client]
            [clojure.java.io :as io]
			[clj-webdriver.taxi :as wd]
	)
  (:import
    (org.apache.commons.io FilenameUtils)
    ;(org.jsoup Jsoup)
    )
)
(declare get-host-port-from-url)
(declare get-file-name-by-host-port)

(defn get-text-from-html [url out-dir]

)

(defn- get-host-port-from-url [url]
  (let [obj (java.net.URL. url)
        protocol (.getProtocol obj)
        host (.getHost obj)
        port (.getPort obj)
        port (if (= port -1) (if (= protocol "https") 443 80) port)
        ]
    [host port]
    )
  )

(defn- get-file-name-by-host-port [host port]
  (str host "----" port)
  )

(defn- get-host-port-by-file-name [filepath]
  (let [obj_file (io/file filepath)
        filename (.getName obj_file)
        filename (FilenameUtils/removeExtension filename)
        ]
    (str/split filename #"----")
    )
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

(defn- analyze-tag-count [obj]
	(dorun
    (map (fn [input]
			(when-let [tag (:tag input)]
				(let [contents (-> input (eh/select [tag]) (eh/at [:script] nil) (eh/at [:style] nil) first :content)
              tag_count (if (nil? (get @tag_result tag)) 0 (get @tag_result tag))]
						(condp = tag
							:title (swap! tag_result assoc (str "str_" (name tag)) (first contents))
							:meta (process-meta input)
              (do
                (swap! tag_result assoc tag (+ tag_count 1))
                (dorun
                  (map (fn [content]
                         (when (string? content)
                           (let [text (-> content (str/replace #"\t" "")  str/trim)]
                             (when (not (empty? text))
                               (if (or (> (count (re-matches #".*(。|、).*" text)) 0) (> (count text) 50))
                                 (swap! tag_result assoc "body" (str (get @tag_result "body") "\n" text))
                                 (swap! tag_result assoc "body_keyword" (str (get @tag_result "body_keyword") "\n" text))
                                 )
                               )
                             )
                           )
                         ) contents))
                )
              )
						(analyze-tag-count contents)
				)
			)
		) obj))
)

;jsons -> [{key json} {key json} ...]
(defn- json-to-csv-file [jsons out-dir append]
  (let [out-dir (if (str/ends-with? out-dir "/") out-dir (str out-dir "/"))
        out-file (str out-dir "site-content.csv")
        _ (when (.exists (clojure.java.io/as-file out-file)) (clojure.java.io/delete-file out-file))
        all-data (map #(-> % nnext first) jsons)
        features (distinct (reduce #(apply merge %1 %2) [] (map #(keys %) all-data)))
        out-data (map (fn [data]
                        (reduce (fn [m column]
                                  (conj m (get data column ""))
                                  ) [] features)
                        ) all-data)
        ]
    (with-open [file (clojure.java.io/writer out-file :append append)]
      (if append
        (csv/write-csv file out-data)
        (csv/write-csv file (cons features out-data))
        )
      (prn "## finish json-to-csv")
      )
    )
  )

; html ->  [str_host str_port str_html]
(defn- html-to-json [html-vec out-dir]
  (try
    (prn "## to-json " (first html-vec))
    (reset! tag_result {})
    (let [data (-> html-vec nnext first eh/html-snippet)]
      (analyze-tag-count data)
      (swap! tag_result dissoc :body)
      (swap! tag_result assoc "site_host" (first html-vec))
      (swap! tag_result assoc "site_port" (second html-vec))
      (let [result (reduce (fn [m element]
                             (if (-> element key keyword?)
                               (assoc m (str "count_" (-> element key name)) (-> element val str))
                               (assoc m (-> element key) (-> element val str))
                               )
                             ) {} @tag_result)
            ]
        (spit (str out-dir "json/" (first html-vec) "----" (second html-vec) ".json") (json/write-str result))
        [(first html-vec) (second html-vec) result]
        )
      )
    (catch Exception e
      (prn e)
      ;(spit "./error-html2json.log" (str "[" (java.util.Date.) "] " (first html-vec) "-" (second html-vec) " " (:via (Throwable->map e)) "\n") :append true)
      (spit "./error-html2json.log" (str "[" (java.util.Date.) "] " (first html-vec) "-" (second html-vec) "\n") :append true)
      ))
  )

(defn- url-to-html [url out-dir]
  (try
    (prn "##" url)
    (let [;result (.html (.get (.followRedirects (Jsoup/connect url) true)))
          result (client/get url {:cookie-policy :standard})
          r-url (-> result :trace-redirects first)]
      (if r-url
        (get-text-from-html r-url out-dir)
        (let [result (:body result)
              metas (eh/select (eh/html-snippet result) [:meta])
              redirect-url (reduce (fn [_ meta]
                                     (let [tag_refresh (-> meta :attrs :http-equiv)
                                           tag_refresh (if (nil? tag_refresh) "" (str/lower-case tag_refresh))]
                                       (when (= tag_refresh "refresh")
                                         (let [content (str/split (-> meta :attrs :content) #";")
                                               new_url (first (keep #(when (str/includes? (str/lower-case %) "url=")
                                                                       (let [str_sep (cond
                                                                                       (str/includes? % "=\"") #"=\""
                                                                                       (str/includes? % "='") #"='"
                                                                                       :else #"="
                                                                                       )]
                                                                         (-> % str/trim (str/split str_sep) (get 1))
                                                                         )
                                                                       ) content))
                                               new_url (if (or (str/starts-with? new_url "https://") (str/starts-with? new_url "http://"))
                                                         new_url
                                                         (let [obj (java.net.URL. url)
                                                               protocol (.getProtocol obj)
                                                               host (.getHost obj)
                                                               path (.getPath obj)
                                                               path (if (str/ends-with? path "/") path (str path "/"))
                                                               port (.getPort obj)
                                                               port (if (= -1 port) "" (str ":" port))]
                                                           (if (str/starts-with? new_url "/")
                                                             (str protocol "://" host port new_url)
                                                             (str protocol "://" host port path new_url)
                                                             )
                                                           )
                                                         )
                                               ]
                                           (reduced new_url)
                                           )
                                         )
                                       )
                                     ) nil metas)
              ]
          (if redirect-url
            (get-text-from-html redirect-url out-dir)
            (let [[host port] (get-host-port-from-url url)
                  file-name (get-file-name-by-host-port host port)]
              (spit (str out-dir "html/" file-name ".html") result)
              [host port result]
              )
            )
          )
        )
      )
    (catch Exception e
      ;(spit "./error-url.log" (str "[" (java.util.Date.) "] " url " " (:via (Throwable->map e)) "\n") :append true)
      (spit "./error-url.log" (str "[" (java.util.Date.) "] " url "\n") :append true)
      ))
  )

(defn batch-url-to-csv-file 
  ([input-path out-dir append]
	  (try
	    (prn "start..." (java.util.Date.))
	    (let [out-dir (if (str/ends-with? out-dir "/") out-dir (str out-dir "/"))
	          urls (str/split (slurp input-path) #"\n")
	          urls (map #(if (str/ends-with? % "\r") (str/replace % "\r" "") %) urls)
	          htmls (keep #(url-to-html % out-dir) urls)
	          jsons (map #(html-to-json % out-dir) htmls)
	          ]
	          (json-to-csv-file jsons out-dir append)
	      )
	    (prn "end..." (java.util.Date.))
	  (catch Exception e
	         (prn "### " e)
	    ))
   )
   ([input-path out-dir]
    (batch-url-to-csv-file false)
   )
  )

(defn batch-path-to-csv-file [input-dir out-dir append]
  (prn "start..." (java.util.Date.))
  (let [out-dir (if (str/ends-with? out-dir "/") out-dir (str out-dir "/"))
        obj-dir (io/file input-dir)
        files (-> obj-dir file-seq next)
        jsons (map (fn [file-path]
                     (let [[host port] (get-host-port-by-file-name file-path)]
                       (html-to-json [host port (slurp file-path)] out-dir)
                       )
                     ) files)
        ]
    (json-to-csv-file jsons out-dir append)
    )
    (prn "end..." (java.util.Date.))
  )

(defn get-docs-url [list_url page_start page_end]
  ;http://www.jpubb.com/list/list.php?listed=1&se=tou&pageID=75
  (try
    (for [index (range page_start (+ page_end 1))]
      (let [url (str list_url "&pageID=" index)
            _ (prn "###" index)
            _ (Thread/sleep 1000)
            response (client/get url {:headers
                                      {"User-Agent" "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.138 Safari/537.36"}})
            str-html (:body response)
            _ (spit "test.html" str-html)
            ;str-html (slurp "test.html")
            html (eh/html-snippet str-html)
            links (eh/select html [:td.other [:a]])
            ]
        (dorun (keep #(let [link (-> % :attrs :href)]
                        (when (not (str/starts-with? link "http://www.jpubb.com"))
                          (spit "./test.txt" (str link "\n") :append true)
                          )
                        ) links))
        )
      )
    (catch Exception e
      (prn e))
    )
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

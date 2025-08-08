(ns fluree.json-ld.processor.crypto-shim
  "Crypto shim for browser environments")

;; Provide a minimal crypto shim for browser environments
(when (and (exists? js/window) (not (exists? js/crypto)))
  (set! js/global #js{})
  (set! (.-crypto js/global)
        #js{:createHash (fn [_algorithm]
                          #js{:update (fn [data]
                                        #js{:digest (fn [_encoding]
                                                      ;; Simple hash fallback
                                                      (str (.toString (.getTime (js/Date.)) 16)
                                                           (.toString (hash data) 16)))})})}))
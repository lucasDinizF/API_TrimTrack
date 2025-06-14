; Rode o server primeiro no REPL depois o fronte (current file)

(ns trintrack-server.core
  (:require [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.body-params :as body-params]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clj-http.client :as client]
            [clj-time.format :as f]
            [clj-time.coerce :as c]
            ))

(def usuario (atom nil))
(def alimentos (atom nil))
(def treinos (atom nil))

(def api-key-alimento "PDSEBKyA7mkKtiKSE6nYpuciLt4ChDkYWy1CWrJV")
(def api-key-exercicio "50FiZTJVYBfyKW+IwX7Njw==MgBQnbHtREkAaDU8")

(def iso-formatter (f/formatter "yyyy/MM/dd"))

(defn traduzir-texto [texto de para]
  (let [url (str "https://api.mymemory.translated.net/get?q="
                 (java.net.URLEncoder/encode texto "UTF-8")
                 "&langpair=" de "|" para)
        resposta (client/get url {:as :json})
        traduzido (get-in resposta [:body :responseData :translatedText])]
    traduzido))

(defn sugestoes-alimentos [request]
  (let [filtro (get-in request [:query-params :filtro] "")
        filtro-en (traduzir-texto filtro "pt" "en")
        filtro-encoded (java.net.URLEncoder/encode  filtro-en "UTF-8")
        url (str "https://api.nal.usda.gov/fdc/v1/foods/search?query="
                 filtro-encoded
                 "&api_key=" api-key-alimento)
        resposta (client/get url {:as :json})
        dados (get-in resposta [:body :foods])
        alimentos (mapv (fn [item]
                          (let [descricao (or (:description item) "Não informado")
                                descricao-pt (traduzir-texto descricao "en" "pt")
                                calorias (or
                                           (some #(when (= (:nutrientName %) "Energy")
                                                    (:value %))
                                                 (:foodNutrients item))
                                           "Não informado")]
                            {:prato descricao-pt
                             :calorias calorias}))
                        dados)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string alimentos)}))

(defn sugestoes-treinos [request]
  (let [filtro (get-in request [:query-params :filtro] "")
        peso   (get-in request [:query-params :peso])
        filtro-en (traduzir-texto filtro "pt" "en")
        base-url (str "https://api.api-ninjas.com/v1/caloriesburned?activity="
                      (java.net.URLEncoder/encode filtro-en "UTF-8"))
        url (if peso
              (str base-url "&weight=" (java.net.URLEncoder/encode peso "UTF-8"))
              base-url)
        resposta (client/get url {:as :string
                                  :headers {"X-Api-Key" api-key-exercicio}})
        dados (json/parse-string (:body resposta) true)
        treinos (mapv (fn [item]
                        (let [nome (or (:name item) "Não informado")
                              nome-pt (traduzir-texto nome "en" "pt")]
                          {:nome nome-pt
                           :calorias-por-hora (:calories_per_hour item)}))
                      dados)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string treinos)}))


;usuario=======================================================================================================================

(defn cadastrar-usuario [request]
  (let [dados (:json-params request)]
    (reset! usuario dados)
    {:status 200 :body {:mensagem "Usuário cadastrado com sucesso!" :usuario dados}}))

(defn verificar-usuario [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (cheshire.core/generate-string {:existe (some? @usuario)})})

(defn mostrar-usuario [request]
  (if (some? @usuario)
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string @usuario)}
    {:status 404
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string {:erro "Nenhum usuário cadastrado."})}))

;extrato================================================================================================================

(defn parse-data [data-str]
  (c/to-long (f/parse iso-formatter data-str)))

(defn filtrar-por-periodo [lista inicio fim]
  (filter (fn [{:keys [data]}]
            (let [data-ms (parse-data data)]
              (and (>= data-ms inicio) (<= data-ms fim))))
          lista))

(defn mostrar-extrato [request]
  (let [inicio-str (get-in request [:query-params :inicio])
        fim-str    (get-in request [:query-params :fim])
        inicio-ms  (parse-data inicio-str)
        fim-ms     (parse-data fim-str)
        alimentos-filtrados  (filtrar-por-periodo @alimentos inicio-ms fim-ms)
        treinos-filtrados    (filtrar-por-periodo @treinos inicio-ms fim-ms)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string
             {:periodo {:inicio inicio-str :fim fim-str}
              :alimentos alimentos-filtrados
              :exercicios treinos-filtrados})}))


;saldo================================================================================================================



(defn calcular-saldo-total-por-periodo [inicio-ms fim-ms]
  (let [alimentos-filtrados (filtrar-por-periodo @alimentos inicio-ms fim-ms)
        treinos-filtrados (filtrar-por-periodo @treinos inicio-ms fim-ms)
        total-consumidas (reduce + (map #(or (:calorias %) 0) alimentos-filtrados))
        total-gastas (reduce + (map #(or (:calorias-por-hora %) 0) treinos-filtrados))]
    (- total-consumidas total-gastas)))


(defn mostrar-saldo [request]
  (let [inicio-str (get-in request [:query-params :inicio])
        fim-str (get-in request [:query-params :fim])
        inicio-ms (parse-data inicio-str)
        fim-ms (parse-data fim-str)
        saldo-total (calcular-saldo-total-por-periodo inicio-ms fim-ms)]
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string
             {:periodo {:inicio inicio-str :fim fim-str}
              :saldo-total saldo-total})}))



;alimento================================================================================================================

(defn cadastrar-alimentos [request]
  (let [dados (:json-params request)]
    (swap! alimentos conj dados)
    {:status 200 :body {:mensagem "alimento cadastrado com sucesso!" :alimentos dados}}))

;(defn cadastrar-alimentos [request]
;  (let [dados (:json-params request)]
;    (swap! alimentos conj dados)
;    (inserir-no-extrato (assoc dados :tipo :entrada))  ;; positivo
;    {:status 200 :body {:mensagem "alimento cadastrado com sucesso!" :alimentos dados}}))

(defn verificar-alimentos [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (cheshire.core/generate-string {:existe (some? @alimentos)})})

(defn mostrar-alimentos [request]
  (if (some? @alimentos)
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string @alimentos)}
    {:status 404
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string {:erro "Nenhum alimento cadastrado."})}))

;treino=======================================================================================================================

(defn cadastrar-treinios [request]
  (let [dados (:json-params request)]
    (swap! treinos conj dados)
    {:status 200 :body {:mensagem "treino cadastrado com sucesso!" :treinos dados}}))

;(defn cadastrar-treinios [request]
;  (let [dados (:json-params request)] ;; negativo
;    (swap! treinos conj dados)
;    (inserir-no-extrato (assoc dados :tipo :saida))
;    {:status 200 :body {:mensagem "treino cadastrado com sucesso!" :treinos dados}}))


(defn verificar-treinios [request]
  {:status 200
   :headers {"Content-Type" "application/json"}
   :body (cheshire.core/generate-string {:existe (some? @treinos)})})

(defn mostrar-treinios [request]
  (if (some? @treinos)
    {:status 200
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string @treinos)}
    {:status 404
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string {:erro "Nenhum treino cadastrado."})}))

;rotas=======================================================================================================================

(def routes
  (route/expand-routes
    #{["/alimentos/sugestoes" :get sugestoes-alimentos :route-name :sugestoes-alimentos]
      ["/treinos/sugestoes" :get sugestoes-treinos :route-name :sugestoes-treinos]
      ["/extrato" :get mostrar-extrato :route-name :mostrar-extrato]
      ["/usuario" :post cadastrar-usuario :route-name :cadastrar-usuario]
      ["/usuario/existe" :get verificar-usuario :route-name :verificar-usuario]
      ["/alimentacao" :post cadastrar-alimentos :route-name :cadastrar-alimentos]
      ["/exercicio" :post cadastrar-treinios :route-name :cadastrar-treinios]

      ["/saldo" :get mostrar-saldo :route-name :saldo]

      ["/usuario/dados" :get mostrar-usuario :route-name :mostrar-usuario]

      ["/alimentacao/existe" :get verificar-alimentos :route-name :verificar-alimentos]
      ["/alimentacao/dados" :get mostrar-alimentos :route-name :mostrar-alimentos]

      ["/exercicio/existe" :get verificar-treinios :route-name :verificar-treinios]
      ["/exercicio/dados" :get mostrar-treinios :route-name :mostrar-treinios]


      }))

(def service-map
  (-> {::http/routes routes
       ::http/type :jetty
       ::http/port 3000
       ::http/join? false
       ::http/secure-headers nil}
      http/default-interceptors
      (update ::http/interceptors conj (body-params/body-params))))

(def server (atom nil))

(defn start-server []
  (reset! server (http/start (http/create-server service-map)))
  (println "Servidor iniciado em http://localhost:3000"))

(start-server)

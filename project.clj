(ns consumir-api.teste
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def api-key-alimento "PDSEBKyA7mkKtiKSE6nYpuciLt4ChDkYWy1CWrJV")
(def api-key-exercicio "50FiZTJVYBfyKW+IwX7Njw==MgBQnbHtREkAaDU8")

(defn calorias-alimento-gramas [calorias-por-100g gramas]
  (let [calorias-consumidas (* calorias-por-100g (/ gramas 100.0))]
    (str "\nVoce consumiu aproximadamente " (format "%.2f" calorias-consumidas)
         " calorias em " gramas " gramas.")))

(defn mostrar-alimento [dados i]
  (if (>= i (count dados))
    (println "Digite o numero do alimento que voce consumiu e a quantidade dele: ")
    (do
      (println (str (inc i) ". " (:description (nth dados i))))
      (recur dados (inc i)))))

(defn buscar-alimento [n dados i gramas]
  (cond (>= i (count dados)) "Alimento nao encontrado"
    (= (inc i) n)
    (let [alimento (nth dados i)
          calorias (some #(when (= (:nutrientName %) "Energy") (:value %)) (:foodNutrients alimento))]
      (if (nil? calorias) "Calorias nao informadas para esse alimento."
        (str "Descricao: " (:description alimento) (calorias-alimento-gramas calorias gramas))))
    :else (recur n dados (inc i) gramas)))

(defn pegar-alimento [descricao]
  (let [url (str "https://api.nal.usda.gov/fdc/v1/foods/search?query="
                 (java.net.URLEncoder/encode descricao "UTF-8")
                 "&api_key=" api-key-alimento)
        resposta (client/get url {:as :json})
        dados (:foods (:body resposta))]
    (mostrar-alimento dados 0)))

(defn pegar-alimento-opcao [descricao n gramas]
  (let [url (str "https://api.nal.usda.gov/fdc/v1/foods/search?query="
                 (java.net.URLEncoder/encode descricao "UTF-8")
                 "&api_key=" api-key-alimento)
        resposta (client/get url {:as :json})
        dados (:foods (:body resposta))]
    (buscar-alimento n dados 0 gramas)))

(defn mostrar-exercicio [dados i]
  (if (>= i (count dados))
    (println "Digite o numero da atividade que voce praticou e o tempo gasto em minutos: ")
    (do
      (println (str (inc i) ". " (:name (nth dados i))))
      (recur dados (inc i)))))

(defn tempo-calorias-exercicio [atividade tempo]
  (let [calorias-gastas (* atividade (/ tempo 60.0))]
    (str "\nVoce queimou aproximadamente " (format "%.2f" calorias-gastas)
         " calorias em " tempo " minutos.")))

(defn buscar-exercicio [n dados i tempo]
  (cond (>= i (count dados)) "Atividade nao encontrada"
    (= (inc i) n)
        (let [exercicio (nth dados i) calorias-str (:calories_per_hour exercicio)]
          (if (nil? calorias-str) "Calorias nao disponiveis para este exercicio."
                                  (let [calorias (Double/parseDouble (str calorias-str))]
                                    (str "Atividade: " (:name exercicio) (tempo-calorias-exercicio calorias tempo)))))
        :else (recur n dados (inc i) tempo)))


(defn pegar-exercicio [atividade]
  (let [url (str "https://api.api-ninjas.com/v1/caloriesburned?activity="
                 (java.net.URLEncoder/encode atividade "UTF-8"))
        resposta (client/get url {:as :string
                                  :headers {"X-Api-Key" api-key-exercicio}})
        dados (json/parse-string (:body resposta) true)]
    (mostrar-exercicio dados 0)))

(defn pegar-exercicio-opcao [atividade n tempo]
  (let [url (str "https://api.api-ninjas.com/v1/caloriesburned?activity="
                 (java.net.URLEncoder/encode atividade "UTF-8"))
        resposta (client/get url {:as :string
                                  :headers {"X-Api-Key" api-key-exercicio}})
        dados (json/parse-string (:body resposta) true)]
    (buscar-exercicio n dados 0 tempo)))


;; Main
(defn main []
  (println "Digite o alimento:")
  (let [descricao (read-line)]
    (pegar-alimento descricao)
    (let [n (Integer/parseInt (read-line))
          gramas (Integer/parseInt (read-line))]
      (println (pegar-alimento-opcao descricao n gramas))))

  (println "\nDigite a atividade:")
  (let [atividade (read-line)]
    (pegar-exercicio atividade)
    (let [n (Integer/parseInt (read-line))
          tempo (Integer/parseInt (read-line))]
      (println (pegar-exercicio-opcao atividade n tempo)))))

(main)


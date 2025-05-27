(ns consumir-api.project
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [cheshire.core :as json]))

(def api-key-alimento "PDSEBKyA7mkKtiKSE6nYpuciLt4ChDkYWy1CWrJV")
(def api-key-exercicio "50FiZTJVYBfyKW+IwX7Njw==MgBQnbHtREkAaDU8")

;; Funções para alimentos
(defn mostrar-alimento [dados i]
  (if (>= i (count dados))
    (println "Digite o número do alimento que você consumiu: ")
    (do
      (println (str (inc i) ". " (:description (nth dados i))))
      (recur dados (inc i)))))

(defn buscar-alimento [n dados i]
  (cond
    (>= i (count dados)) "Alimento não encontrado"
    (= (inc i) n)
    (let [alimento (nth dados i)
          calorias (some #(when (= (:nutrientName %) "Energy") (:value %)) (:foodNutrients alimento))]
      (str "Descrição: " (:description alimento)
           "\nCalorias: " (or calorias "não informado")))
    :else (recur n dados (inc i))))

(defn pegar-alimento [descricao]
  (let [url (str "https://api.nal.usda.gov/fdc/v1/foods/search?query="
                 (java.net.URLEncoder/encode descricao "UTF-8")
                 "&api_key=" api-key-alimento)
        resposta (client/get url {:as :json})
        dados (:foods (:body resposta))]
    (mostrar-alimento dados 0)))

(defn pegar-alimento-opcao [descricao n]
  (let [url (str "https://api.nal.usda.gov/fdc/v1/foods/search?query="
                 (java.net.URLEncoder/encode descricao "UTF-8")
                 "&api_key=" api-key-alimento)
        resposta (client/get url {:as :json})
        dados (:foods (:body resposta))]
    (buscar-alimento n dados 0)))

;; Funções para exercícios
(defn mostrar-exercicio [dados i]
  (if (>= i (count dados))
    (println "Digite o número da atividade que você praticou: ")
    (do
      (println (str (inc i) ". " (:name (nth dados i))))
      (recur dados (inc i)))))

(defn buscar-exercicio [n dados i]
  (cond
    (>= i (count dados)) "Atividade não encontrada"
    (= (inc i) n)
    (let [exercicio (nth dados i)]
      (str "Atividade: " (:name exercicio)
           "\nCalorias queimadas por hora: " (:calories_per_hour exercicio)))
    :else (recur n dados (inc i))))

(defn pegar-exercicio [atividade]
  (let [url (str "https://api.api-ninjas.com/v1/caloriesburned?activity="
                 (java.net.URLEncoder/encode atividade "UTF-8"))
        resposta (client/get url {:as :string
                                  :headers {"X-Api-Key" api-key-exercicio}})
        dados (json/parse-string (:body resposta) true)]
    (mostrar-exercicio dados 0)))

(defn pegar-exercicio-opcao [atividade n]
  (let [url (str "https://api.api-ninjas.com/v1/caloriesburned?activity="
                 (java.net.URLEncoder/encode atividade "UTF-8"))
        resposta (client/get url {:as :string
                                  :headers {"X-Api-Key" api-key-exercicio}})
        dados (json/parse-string (:body resposta) true)]
    (buscar-exercicio n dados 0)))

;; Main
(defn main []
  (println "Digite o alimento:")
  (let [descricao (read-line)]
    (pegar-alimento descricao)
    (let [n (Integer/parseInt (read-line))]
      (println (pegar-alimento-opcao descricao n))))

  (println "\nDigite a atividade:")
  (let [atividade (read-line)]
    (pegar-exercicio atividade)
    (let [n (Integer/parseInt (read-line))]
      (println (pegar-exercicio-opcao atividade n)))))

(main)

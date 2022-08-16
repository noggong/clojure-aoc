(ns aoc2020_8
  (:require [clojure.string :as str]                        ; str 이라는 alias 는 지양함 / 이미 str core 함수가 존재
            [clojure.data :as data]))

(def aoc-input (slurp "resources//2020_8.txt"))
(def sample-input "nop +0
                   acc +1
                   jmp +4
                   acc +3
                   jmp -3
                   acc -99
                   acc +1
                   jmp -4
                   acc +6")

(defn input->instructions
  "input 을 지침 목록(instructions) 으로 변경
  input: nop +0
         acc +1
         jmp +4
         acc +3
         jmp -3
         acc -99
         acc +1
         jmp -4
         acc +6
  output:
  "
  [input]
  (let [commands (re-seq #"nop|acc|jmp" input)
        arguments (re-seq #"[+|-]{1}[0-9]+" input)]
    (->> (mapv #(hash-map (keyword %1) (Integer/parseInt %2)) commands arguments))))

(defn instructions->device
  "
  input
    [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}]
  output
    :instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :next-index 0,
    :ran-instructions #{},
    :acc 0}
  "
  [instructions]
  {:original-instructions instructions
   :fixed-instructions instructions
   :next-index 0
   :ran-instructions #{}
   :acc 0
   :state "working"})

(defn run-instruction
  "
  지침이 누산기 인지 확인
  input
  {:command \"acc\", :arg 0}

  output
  true
  "
  [instruction]
  (let [amount-change {:index 0 :acc 0}
        command (first (keys instruction))
        value (command instruction)]
    (case command
      :acc (assoc amount-change :index 1 :acc value)
      :jmp (assoc amount-change :index value :acc 0)
      :nop (assoc amount-change :index 1 :acc 0))))


(defn state-of-device
  "
  디바이스의 상태를 가져온다.
  input {
  [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
  1
  #{0 7 1 4 6 3 2}}

  ouptut working"
  [instructions ran-instructions next-index]
  (cond
    (= (count instructions) (count ran-instructions)) "terminated"
    (ran-instructions next-index) "error"
    :else "working"))


(defn fixed-instructions

  [original fixed]
  (data/diff (map #(first (keys %)) original) (map #(first (keys %)) fixed)))


(str/index-of [nil :acc :jmp :acc :jmp :acc :acc :jmp :acc] nil)
(str/index-of ( 1 2 3 4) 1)

(fixed-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}]
                    [{:jmp 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}])


(defn run-instructions
  "
  input:
    {:original-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :fixed-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :next-index 1,
    :ran-instructions #{0 7 1 4 6 3 2},
    :acc 5,
    :state \"error\"}
  output:
    {:original-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :fixed-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :next-index 1,
    :ran-instructions #{0 7 1 4 6 3 2},
    :acc 5,
    :state \"error\"}
  "
  [{:keys [original-instructions fixed-instructions next-index ran-instructions] :as device}]
  (let [current-instruction (get fixed-instructions next-index)
        change-amount (run-instruction current-instruction)
        ran-instructions (conj ran-instructions next-index)
        next-index (+ next-index (:index change-amount))
        state (state-of-device fixed-instructions ran-instructions next-index)]
    (cond-> device
            (#{"working" "terminate"} state) (->
                                               (assoc :next-index next-index)
                                               (assoc :ran-instructions ran-instructions)
                                               (update :acc #(+ % (:acc change-amount)))
                                               (assoc :state state))
            (= "error" state) (->
                               (assoc :next-index 0)
                               (assoc :ran-instructions #{})
                               (assoc :acc 0)
                               (assoc :state state)) )




      #_"terminate" #_ (-> device
                           (assoc :next-index next-index)
                           (assoc :ran-instructions ran-instructions)
                           (update :acc #(+ % (:acc change-amount)))
                           (assoc :state state))))

(defn run-device
  "
  input
    [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :next-index 0,
    :ran-instructions #{},
    :acc 0}

  output
    {:instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
     :next-index 1,
     :ran-instructions #{0 7 1 4 6 3 2},
     :acc 5}
  "
  [exit-state device]
  #_(first (drop-while #(not= exit-state (:state %))
                       (iterate run-instructions device)))
  (take 10 (iterate run-instructions device)))

(comment
  ; part 1
  (->> sample-input
       input->instructions
       instructions->device
       (run-device "error")
       #_:acc)

  (->> aoc-input
       input->instructions
       instructions->device
       (run-device "error")
       #_:acc)

  ; part 2
  (->> sample-input
       input->instructions
       instructions->device
       run-device
       :acc)

  (->> aoc-input
       input->instructions
       instructions->device
       run-device
       :acc))

; finite state machine
; :terminate
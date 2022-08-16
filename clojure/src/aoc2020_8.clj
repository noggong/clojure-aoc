(ns aoc2020_8)

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
    (->> (mapv #(hash-map :op (keyword %1) :value (Integer/parseInt %2)) commands arguments))))


(defn nop<->jmp
  "
  오퍼레이터가 nop 이면 jmp, jmp 이면 nop를 반환한다. acc 이면 그대로 반환한다.
  input:
  jmp | acc | nop
  output:
  jmp | nop "
  [op]
  (case op
    :jmp :nop
    :nop :jmp
    :acc))

(defn instructions->all-cases
  "지침들을 모든 경우의 수로 2차원으로 반환한다.
  input:
    [{:value 0, :op :nop}
    {:value 1, :op :acc}
    {:value 4, :op :jmp}
    {:value 3, :op :acc}
    {:value -3, :op :jmp}
    {:value -99, :op :acc}
    {:value 1, :op :acc}
    {:value -4, :op :jmp}
    {:value 6, :op :acc}]
  output
    ([{:value 0, :op :nop}
    {:value 1, :op :acc}
    {:value 4, :op :jmp}
    {:value 3, :op :acc}
    {:value -3, :op :jmp}
    {:value -99, :op :acc}
    {:value 1, :op :acc}
    {:value -4, :op :jmp}
    {:value 6, :op :acc}]
    [{:value 0, :op :jmp}
    {:value 1, :op :acc}
    {:value 4, :op :jmp}
    {:value 3, :op :acc}
    {:value -3, :op :jmp}
    {:value -99, :op :acc}
    {:value 1, :op :acc}
    {:value -4, :op :jmp}
    {:value 6, :op :acc}]
    [{:value 0, :op :nop} ....])"
  [instructions]
  (->> (map-indexed (fn [idx _] (update-in instructions [idx :op] nop<->jmp)) instructions)
       (concat [instructions])
       vec))

(defn instructions->device
  "
  input
    [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}]
  output
    :instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :next-index 0,
    :ran-indices #{},
    :acc 0}
  "
  [instructions]
  {:all-instructions (instructions->all-cases instructions)
   :next-index 0
   :ran-indices #{}
   :acc 0
   :state "working"})

(defn run-instruction
  "
  지침에 따른 변화량을 index / acc 로 나누어 반환한다.
  input
  {:command \"acc\", :arg 0}

  output
  true
  "
  [{:keys [op value] :as instruction}]
  (let [amount-change {:index 0 :acc 0}]
    (case op
      :acc (assoc amount-change :index 1 :acc value)
      :jmp (assoc amount-change :index value :acc 0)
      :nop (assoc amount-change :index 1 :acc 0))))

(defn state-of-instructions
  "
  디바이스의 상태를 가져온다.
  input {
  [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
  1
  #{0 7 1 4 6 3 2}}

  ouptut working"
  [instructions ran-indices next-index]
  (cond
    (<= (count instructions) next-index) "terminate"
    (ran-indices next-index) "error"
    :else "working"))


(defn device->instructions-by-state
  [{:keys [all-instructions state] :as device}]
  (if (= "error" state)
    (-> device
        (assoc :next-index 0)
        (assoc :ran-indices #{})
        (assoc :acc 0)
        (assoc :state "working")
        (assoc :all-instructions (subvec all-instructions 1)))
    device))



(defn run-instructions
  "
  input:
    {:original-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :fixed-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :next-index 1,
    :ran-indices #{0 7 1 4 6 3 2},
    :acc 5,
    :state \"error\"}
  output:
    {:original-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :fixed-instructions [{:nop 0} {:acc 1} {:jmp 4} {:acc 3} {:jmp -3} {:acc -99} {:acc 1} {:jmp -4} {:acc 6}],
    :next-index 1,
    :ran-indices #{0 7 1 4 6 3 2},
    :acc 5,
    :state \"error\"}
  "
  [device]
  (let [{:keys [all-instructions next-index ran-indices acc]} (device->instructions-by-state device)
        instructions (first all-instructions)
        current-instruction (get instructions next-index)
        change-amount (run-instruction current-instruction)
        ran-indices (conj ran-indices next-index)
        next-index (+ next-index (:index change-amount))
        state (state-of-instructions instructions ran-indices next-index)]

    (-> device
        (assoc :next-index next-index)
        (assoc :ran-indices ran-indices)
        (assoc :acc (+ acc (:acc change-amount)))
        (assoc :all-instructions all-instructions)
        (assoc :state state))))



(defn run-device
  "
  기기의 부트 코드를 실행한다.
  input
    {:all-instructions [[{:value 0, :op :nop}
    {:value 1, :op :acc}
    {:value 4, :op :jmp}
    {:value 3, :op :acc}
    {:value -3, :op :jmp}
    {:value -99, :op :acc}
    {:value 1, :op :acc}
    {:value -4, :op :jmp}
    {:value 6, :op :acc}]
    [{:value 0, :op :jmp}
    {:value 1, :op :acc}
    {:value 4, :op :jmp}
    {:value 3, :op :acc}
    {:value -3, :op :jmp}
    {:value -99, :op :acc}
    {:value 1, :op :acc}
    {:value -4, :op :jmp}
    {:value 6, :op :acc}]....],
    :next-index 0,
    :ran-indices #{},
    :acc 0,
    :state \"working\"}

  output
    {:all-instructions [[{:value 0, :op :nop}
    {:value 1, :op :acc}
    {:value 4, :op :jmp}
    {:value 3, :op :acc}
    {:value -3, :op :jmp}
    {:value -99, :op :acc}
    {:value 1, :op :acc}
    {:value -4, :op :jmp}
    {:value 6, :op :acc}]
    [{:value 0, :op :jmp}
    {:value 1, :op :acc}
    {:value 4, :op :jmp}
    {:value 3, :op :acc}
    {:value -3, :op :jmp}
    {:value -99, :op :acc}
    {:value 1, :op :acc}
    {:value -4, :op :jmp}
    {:value 6, :op :acc}]....],
    :next-index 1,
    :ran-indices #{0 7 1 4 6 3 2},
    :acc 5,
    :state \"error\"}
  "
  [exit-state device]
  (first (drop-while #(not= exit-state (:state %))
                     (iterate run-instructions device)))
  #_(take 30 (iterate run-instructions device)))

(take-while (range))

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
       (run-device "terminate")
       #_:acc)

  (->> aoc-input
       input->instructions
       instructions->device
       (run-device "terminate")
       :acc))

; finite state machine
; :terminate
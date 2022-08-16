(ns aoc2018_7
  (:require [clojure.string :as s]))

(def aoc-input (slurp "resources//2018_7.txt"))
(def sample-input "
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(defn input-string->array
  "전달받은 input string 을 필요한 step 이름만 걸러내어 배열화 한다.
  input : \"
    Step C must be finished before step A can begin.
    Step C must be finished before step F can begin.
    Step A must be finished before step B can begin.
    Step A must be finished before step D can begin.
    Step B must be finished before step E can begin.
    Step D must be finished before step E can begin.
    Step F must be finished before step E can begin.\"

  output: `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})
  "
  [input->string]
  (->> input->string
       (re-seq #"Step ([A-Z]) must .+? before step ([A-Z]) can begin.")
       (map (fn [[_ required step]] {:required required :step step}))))

(def worker-count 5)

(defn required-steps-by-step
  "특정의 스텝의 필수 스텝목록을 가져온다
  input
    \"E\"

    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})\"
  output
    #{\"F\" \"B\" \"D\"}"
  [step array]
  (->> array
       (filter #(= (:step %) step))
       (map :required)
       distinct
       sort))

(defn array->required-step
  "step 별 필수 스텝 정보를 가져온다.
  input
    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})

    #{\"E\" \"C\" \"F\" \"B\" \"A\" \"D\"}"
  [array all-steps]
  (->> all-steps
       (map #(hash-map :id %
                       :prerequisite (required-steps-by-step % array)))))

(defn array->all-steps
  "array 에서 전체 스텝을 리스트로 반환한다..
  input: `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})

  output:
    `(\"A\" \"B\" \"C\" \"D\" \"E\" \"F\")"
  [array]
  (->> array
       (map vals)
       flatten
       sort
       distinct))

(defn array->navigation
  "필수 달성 step 과 step 에 대한 정보로 맵정보를 만든다.

  input: `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})"
  [array]
  (let [all-steps (array->all-steps array)]
    {:required-steps (array->required-step array all-steps)
     :visited-steps []
     :on-process-steps []
     :timestamp -1}))

(defn parse-input
  [input-string]
  (->> input-string
       input-string->array
       array->navigation))

(defn step->takes
  "step 의 처리 시간을 가져온다
  input : A
  output : 61"
  [step]
  (- (int (.charAt step 0)) 4))

(defn add-steps-to-process
  "방문 가능한 스텝 (prepared-steps) 을 프로세스에 채워 넣는다.
  input
    [{:id \"C\", :take 3}]
    (\"D\")

    ouput
    [{:id C, :take 3} {:id D :take 4}]"
  [on-process-steps prepared-steps]
  (->> prepared-steps
       (map #(hash-map :id % :take (step->takes %)))
       (concat on-process-steps)))

(defn on-process-steps->finished-steps
  "on-process-steps 에서 take가 0인 (처리가 끝난) step들의 id 를 가져온다
  input
  [{:id \"A\", :take 0} {:id \"C\", :take 3}]
  output
  (\"A\")"
  [on-process-steps]
  (->> on-process-steps
       (filter #(zero? (:take %)))
       (map :id)))

(defn manage-on-process-steps
  "작업중인 스텝들을 takes 를 빼주고 0에 도달한 (작업완료된) 스텝과 함께 반환한다.
  input
  [{:id \"A\", :take 1} {:id \"C\", :take 3}]

  ouput
  {:on-process-steps ({:id \"C\", :take 2})
  :finished-steps (\"A\")
  :count-ready-worker 1}"
  [on-process-steps]
  (let [decreased-steps (map #(update % :take dec) on-process-steps)
        on-process-steps (filter #(not (zero? (:take %))) decreased-steps)]
    {:on-process-steps on-process-steps
     :finished-steps (on-process-steps->finished-steps decreased-steps)
     :count-ready-worker (- worker-count (count on-process-steps))}))

(comment
  (manage-on-process-steps [{:id "A", :take 1} {:id "C", :take 3}]))


(defn remove-finished-steps-from-required-step
  "스텝별 필수 선결 조건 항목에서 완료된 step 을 제거 한다.
  input
  `(\"A\")
  {:prerequisite (\"A\"), :id \"B\"}

  output
  {:prerequisite (), :id \"B\"}

  "
  [finished-steps step]
  (update step :prerequisite #(remove (set finished-steps) %)))

(defn required-steps->prepared-steps
  "필수 선결조건 목록에서 필수선결조건 완료된 steps 들만 가져온다
  input:
    `({:prerequisite (\"C\"), :id \"A\"}
    {:prerequisite (), :id \"B\"}
    {:prerequisite (), :id \"C\"}
    {:prerequisite (), :id \"D\"}
    {:prerequisite (\"B\" \"D\" \"F\"), :id \"E\"}
    {:prerequisite (), :id \"F\"})
  output:
    (\"B\" \"C\" \"D\" \"F\")
  "
  [required-steps]
  (->> required-steps
       (filter #(empty? (:prerequisite %)))
       (map :id)
       sort))

(defn manage-required-steps
  "required-steps 처리 후 선결처리조건 달성한 steps 와 함께 반환한다.
  input:
    `({:prerequisite (\"C\"), :id \"A\"}
    {:prerequisite (\"A\"), :id \"B\"}
    {:prerequisite (), :id \"C\"}
    {:prerequisite (\"A\"), :id \"D\"}
    {:prerequisite (\"B\" \"D\" \"F\"), :id \"E\"}
    {:prerequisite (\"C\"), :id \"F\"})

    (\"C\")

    2
    output:
    {:required-step ({:prerequisite (\"A\"), :id \"B\"}
                     {:prerequisite (\"A\"), :id \"D\"}
                     {:prerequisite (\"B\" \"D\" \"F\"), :id \"E\"}),
     :prepared-steps (\"A\" \"C\")}
  "
  [required-steps finished-steps count-ready-worker]
  (let [required-steps (map #(remove-finished-steps-from-required-step finished-steps %) required-steps)
        prepared-steps (take count-ready-worker (required-steps->prepared-steps required-steps))]
    {:required-steps (filter #(not (some #{(:id %)} prepared-steps)) required-steps)
     :prepared-steps prepared-steps}))

(defn step-by-step
  "지도 정보를 통해 출발지에서 부터 목적지 까지 간다.
   {:all-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
     :possible-steps (\"C\"),
     :goal-steps \"E\",
     :required-steps {:A (\"C\"), :B (\"A\"), :C (), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F (\"C\")},
     :next-steps {:A (\"B\" \"D\"), :B (\"E\"), :C (\"A\" \"F\"), :D (\"E\"), :E (), :F (\"E\")},
     :visited-steps []}
     "
  [{:keys [timestamp on-process-steps required-steps visited-steps]}]
  (let [{:keys [on-process-steps finished-steps count-ready-worker]} (manage-on-process-steps on-process-steps)
        {:keys [required-steps prepared-steps]} (manage-required-steps required-steps finished-steps count-ready-worker)]
    {:required-steps required-steps
     :visited-steps (concat visited-steps finished-steps)
     :on-process-steps (add-steps-to-process on-process-steps prepared-steps)
     :timestamp (inc timestamp)}))

(defn head-for-goal-with-workers
  "지도 정보를 통해 출발지에서 부터 목적지 까지 간다.
  {:all-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
    :possible-steps (\"C\"),
    :goal-steps \"E\",
    :required-steps {:A (\"C\"), :B (\"A\"), :C (), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F (\"C\")},
    :next-steps {:A (\"B\" \"D\"), :B (\"E\"), :C (\"A\" \"F\"), :D (\"E\"), :E (), :F (\"E\")},
    :visited-steps []}
    "
  [navigation]
  (first (drop-while #(or
                        (seq (:required-steps %))
                        (seq (:on-process-steps %)))
                     (iterate step-by-step navigation))))
;recommend
; 지속가능한지 체크하는 함수 별도 분리 추천

(comment
  ;part 1
  (->> sample-input
       parse-input
       head-for-goal-with-workers
       :visited-steps
       (s/join ""))

  (->> aoc-input
       parse-input
       head-for-goal-with-workers
       :visited-steps
       (s/join ""))

  ;part2
  ;- step 진행이 병렬로 가능 (5개)
  ;- 시간 개념 C 를 수행하는데 63초
  ;- A를 수행하는데 61
  ; iterate / take while last/ drop while first


  (->> sample-input
       parse-input
       head-for-goal-with-workers
       :timestamp)

  (->> aoc-input
       parse-input
       head-for-goal-with-workers
       :timestamp))




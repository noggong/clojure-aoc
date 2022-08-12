(ns aoc2018_7
  (:require [clojure.set :as set]
            [clojure.string :as s]))


; part1
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

(defn array->starting-step
  "array 에서 시작 스텝을 가져온다
  input
    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})\"

    #{\"E\" \"C\" \"F\" \"B\" \"A\" \"D\"}
  output :
  #{\"C\"}"
  [array all-steps]
  (set/difference (set all-steps) (set (map :step array))))


(defn array->possible-steps
  "array 에서 시작 스텝을 가져온다
  input
    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})\"

    #{\"E\" \"C\" \"F\" \"B\" \"A\" \"D\"}
  output : #{\"C\"}"
  [array all-steps]
  (->> array
       (map :step)
       set
       (set/difference (set all-steps))
       (into [])
       sort))

(defn array->goal-steps
  "array 에서 목표 스텝을 가져온다
  input
    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})\"

    #{\"E\" \"C\" \"F\" \"B\" \"A\" \"D\"}
  output : #{\"C\"}"
  [array all-steps]
  (->> (set/difference (set all-steps) (set (map :required array)))
       first))


(defn array->starting-step
  "array 에서 시작 스텝을 가져온다
  input
    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})\"

    #{\"E\" \"C\" \"F\" \"B\" \"A\" \"D\"}
  output : #{\"C\"}"
  [array all-steps]
  (set/difference all-steps (set (map :step array))))

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

(defn next-steps-by-step
  "특정의 스텝의 필수 스텝목록을 가져온다
  input
    \"C\"

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
       (filter #(= (:required %) step))
       (map :step)
       distinct
       sort))

(next-steps-by-step "C" `({:required "C", :step "A"}
                          {:required "C", :step "F"}
                          {:required "A", :step "B"}
                          {:required "A", :step "D"}
                          {:required "B", :step "E"}
                          {:required "D", :step "E"}
                          {:required "F", :step "E"}))

(required-steps-by-step "E" `({:required "C", :step "A"}
                              {:required "C", :step "F"}
                              {:required "A", :step "B"}
                              {:required "A", :step "D"}
                              {:required "B", :step "E"}
                              {:required "D", :step "E"}
                              {:required "F", :step "E"}))

(defn array->required-step
  "step 별 필수 스텝 정보를 가져온다.
  input
    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})\"

    #{\"E\" \"C\" \"F\" \"B\" \"A\" \"D\"}"
  [array all-steps]
  (->> (map (fn [step] {(keyword step) (required-steps-by-step step array)}) all-steps)
       (apply merge)))

(defn array->next-step
  "step 별 다음 스텝 정보를 가져온다.
  input
    `({:required \"C\", :step \"A\"}
    {:required \"C\", :step \"F\"}
    {:required \"A\", :step \"B\"}
    {:required \"A\", :step \"D\"}
    {:required \"B\", :step \"E\"}
    {:required \"D\", :step \"E\"}
    {:required \"F\", :step \"E\"})\"

    #{\"E\" \"C\" \"F\" \"B\" \"A\" \"D\"}"
  [array all-steps]
  (->> (map (fn [step] {(keyword step) (next-steps-by-step step array)}) all-steps)
       (apply merge)))

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


(array->all-steps `({:required "C", :step "A"}
                    {:required "C", :step "F"}
                    {:required "A", :step "B"}
                    {:required "A", :step "D"}
                    {:required "B", :step "E"}
                    {:required "D", :step "E"}
                    {:required "F", :step "E"}))

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
    {:all-steps all-steps
     :required-steps (array->required-step array all-steps)
     :visited-steps []
     :on-process-steps []
     :timestamp 0}))

(defn arrive?
  "목적지에 도착했는지 확인 / 모든 step을 방문했으면 도착했다고 간주"
  [all-steps visited-steps]
  (empty? (remove (set visited-steps)  all-steps)))


(defn next-possible-steps
  "다음 단계의 방문 가능한 스텝목록을 가져온다
  input:
    \"C\"
    {:A (), :B `(\"A\"), :D `(\"A\"), :E `(\"B\" \"D\" \"F\"), :F ()}
    `(\"C\")
  output:
    (\"A\" \"F\")
  "
  [step required-steps possible-steps]
  (let [new-possible-steps (remove #{step} possible-steps)]
    (->> required-steps
         (filter (fn [[_ v]] (empty? v)))
         (map (fn [[step _]] (name step)))
         (concat new-possible-steps)
         sort)))

(next-possible-steps "C" {:A (), :B `("A"), :D `("A"), :E `("B" "D" "F"), :F ()} `("C"))

(defn new-required-steps
  "방문한 스텝을 필수 방문 스텝에서 제거

  input:
    \"C\"
    {:A `(\"C\"), :B `(\"A\"), :D `(\"A\"), :E `(\"B\" \"D\" \"F\"), :F `(\"C\")}

  output:
    {:A (), :B (\"A\"), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F ()}
  "
  [step required-steps]
  (->> (dissoc required-steps (keyword step))
       (map (fn [[k v]] {k (remove #{step} v)}))
       (apply merge)))

(new-required-steps "C" {:A `("C") :B `("A") :D `("A")  :E `("B" "D" "F") :F `("C")})
(apply merge `({:A ()} {:B ("A")} {:D ("A")} {:E ("B" "D" "F")} {:F ()}))

(defn head-for-goal
  "지도 정보를 통해 출발지에서 부터 목적지 까지 간다.
  {:all-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
    :possible-steps (\"C\"),
    :goal-steps \"E\",
    :required-steps {:A (\"C\"), :B (\"A\"), :C (), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F (\"C\")},
    :next-steps {:A (\"B\" \"D\"), :B (\"E\"), :C (\"A\" \"F\"), :D (\"E\"), :E (), :F (\"E\")},
    :visited-steps []}
    "
  [{:keys [all-steps possible-steps required-steps visited-steps] :as navigation}]
  (if (arrive? all-steps visited-steps)
    #_(= 1 (count visited-steps))
    visited-steps
    #_navigation
    #_navigation
    (let [visit-step (first possible-steps)
          new-visited-steps (conj visited-steps visit-step)
          new-required-steps (new-required-steps visit-step required-steps)]
      (-> navigation
          (assoc
            :possible-steps (next-possible-steps visit-step new-required-steps possible-steps)
            :required-steps new-required-steps
            :visited-steps new-visited-steps)
          recur))))

(defn parse-input
  [input-string]
  (->> input-string
       input-string->array
       array->navigation))


(comment
  (->> sample-input
       parse-input
       head-for-goal
       (s/join ""))

  (->> aoc-input
       parse-input
       head-for-goal
       (s/join "")))

;part2
;- step 진행이 병렬로 가능 (5개)
;- 시간 개념 C 를 수행하는데 63초
;- A를 수행하는데 61
; iterate / take while last/ drop while first
(def worker-count 2)

(defn step->takes
  "step 의 처리 시간을 가져온다
  input : A
  output : 61"
  [step]
  (- (int (.charAt step 0)) 64))



(defn finish-steps
  "완료된 스텝들 아이디 가져오기
  input
    [{:id \\A, :take 0} {:id \\C, :take 3}]
  output
    `(\\A)"
  [on-process-steps]
  (->> on-process-steps
       (filter (fn [step] (zero? (:take step))))
       (map (fn [{:keys [id]}] id))))

(defn steps-start-process
  "방문 가능한 스텝을 프로세스에 채워 넣는다.
  input
    5
    [{:id \"A\", :take 1} {:id \"C\", :take 3}]
    (\"D\")

    ouput
    [{:id C, :take 3} {:id D :take 4}]"
  [on-process-steps possible-steps]
  (let [new-on-process-steps (filter (fn [step] (not (zero? (:take step)))) on-process-steps)
        worker-count (- worker-count (count new-on-process-steps))
        new-process-steps (take worker-count possible-steps)]
    #_new-on-process-steps
    {:new-process-steps new-process-steps
     :on-process-steps (->> new-process-steps
                            (map (fn [step] {:id step :take (step->takes step)}))
                            (concat new-on-process-steps))}))

;(steps-start-process 2 [{:id "A", :take 0} {:id "C", :take 3}] `("D"))

(defn required-steps->possible-steps
  "스텝별 필수 처리 스텝 항목에서 이제 처리 가능한 스텝을 가져온다
  input:
  {:A () :B (\"A\") :C () :D (\"A\") :E (\"B\" \"D\" \"F\") :F ()}
  output:
  (A C F)
  "
  [required-steps]
  (->> required-steps
       (filter (fn [[_ required]] (empty? required)))
       (map (fn [[k _]] (name k)))))

(required-steps->possible-steps {:A `() :B `("A") :C `() :D `("A") :E `("B" "D" "F") :F `()})

(defn new-required-steps-with-workers
  "방문한 스텝을 필수 방문 스텝에서 제거

  input:
    (\"C\" \"D\")
    {:A `(\"C\"), :B `(\"A\"), :D `(\"A\"), :E `(\"B\" \"D\" \"F\"), :F `(\"C\")}

  output:
    {
    :new-possible-steps (\"A\" \"C\" \"F\")
    :new-required-steps {:A (), :B (\"A\"), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F ()}
    }
  "
  [step required-steps]
  (let [remove-done-steps (apply merge (map (fn [[k v]] {k (remove (set step) v)}) required-steps))
        new-possible-steps (required-steps->possible-steps remove-done-steps)]
    {:new-possible-steps new-possible-steps
     :new-required-steps (apply (partial dissoc remove-done-steps) (map keyword new-possible-steps))}))

(new-required-steps-with-workers `("C") {:A `("C"), :B `("A"), :C (), :D `("A"), :E `("B" "D" "F"), :F `("C")})

(defn work-on-process-steps
  "작업중인 스텝들을 takes 를 빼주고 0에 도달한 (작업완료된) 스텝과 함께 반환한다.
  input
  [{:id \"A\", :take 1} {:id \"C\", :take 3}]

  ouput
  [{:id \"A\", :take 0} {:id \"C\" :take 2}]"
  [on-process-steps]
  (let [new-on-process-steps (map (fn [step] (update step :take dec)) on-process-steps)]
    {:finish-steps (finish-steps new-on-process-steps)
     :on-process-steps new-on-process-steps}))

(update {:id "A", :take 1} :take dec)
;(new-on-process-steps 3 [{:id "A", :take 1} {:id "C", :take 3}] `("D" "F"))
;(work-on-process-steps [{:id "A", :take 1} {:id "C", :take 3}])

(defn next-possible-steps-with-workers
  "다음 단계의 방문 가능한 스텝목록을 가져온다
  input:
    (\"C\" \"A\")
    {:A (), :B `(\"A\"), :D `(\"A\"), :E `(\"B\" \"D\" \"F\"), :F ()}
    `(\"C\")
  output:
    (\"A\" \"F\")
  "
  [step possible-steps new-possible-steps]
  (-> (remove (set step) possible-steps)
      (concat new-possible-steps)))

(defn head-for-goal-with-workers
  "지도 정보를 통해 출발지에서 부터 목적지 까지 간다.
  {:all-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
    :possible-steps (\"C\"),
    :goal-steps \"E\",
    :required-steps {:A (\"C\"), :B (\"A\"), :C (), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F (\"C\")},
    :next-steps {:A (\"B\" \"D\"), :B (\"E\"), :C (\"A\" \"F\"), :D (\"E\"), :E (), :F (\"E\")},
    :visited-steps []}
    "
  [{:keys [all-steps timestamp possible-steps on-process-steps required-steps visited-steps] :as navigation}]
  (if (arrive? all-steps visited-steps)
    #_(= timestamp 3)
    #_visited-steps
    navigation
    #_navigation
    (let [{:keys [finish-steps on-process-steps]} (work-on-process-steps on-process-steps)
          {:keys [new-possible-steps new-required-steps]} (new-required-steps-with-workers finish-steps required-steps)
          new-visited-steps (concat visited-steps finish-steps)
          new-possible-steps (next-possible-steps-with-workers finish-steps possible-steps new-possible-steps)
          {:keys [on-process-steps new-process-steps]} (steps-start-process on-process-steps new-possible-steps)]
      #_finish-steps
      #_{:finish-steps finish-steps
         :on-process-steps on-process-steps
         :new-possible-steps new-possible-steps
         :new-required-steps new-required-steps
         :new-visited-steps new-visited-steps}
      (-> navigation
          (assoc
            :possible-steps (remove (set new-process-steps) new-possible-steps)
            :required-steps new-required-steps
            :visited-steps new-visited-steps
            :on-process-steps on-process-steps
            :timestamp (inc timestamp))
          recur))))

(defn dec-takes-processing-steps
  "작업중인 스텝들을 takes 를 빼주고 0에 도달한 (작업완료된) 스텝과 함께 반환한다.
  input
  [{:id \"A\", :take 1} {:id \"C\", :take 3}]

  ouput
  [{:id \"A\", :take 0} {:id \"C\" :take 2}]"
  [on-process-steps]
  (->> on-process-steps
       (map (fn [step] (update step :take dec)))))

(defn step-by-step
  "지도 정보를 통해 출발지에서 부터 목적지 까지 간다.
   {:all-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
     :possible-steps (\"C\"),
     :goal-steps \"E\",
     :required-steps {:A (\"C\"), :B (\"A\"), :C (), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F (\"C\")},
     :next-steps {:A (\"B\" \"D\"), :B (\"E\"), :C (\"A\" \"F\"), :D (\"E\"), :E (), :F (\"E\")},
     :visited-steps []}
     "
  [{:keys [timestamp possible-steps on-process-steps required-steps visited-steps] :as navigation}]
  #_(-> navigation
        (update :on-process-steps #(dec-takes-processing-steps %))
        (update :visited-steps #(from-process-to-visited (:on-process-steps)))
        (update :timestamp inc))
  (let [{:keys [finish-steps on-process-steps]} (work-on-process-steps on-process-steps)
        {:keys [new-possible-steps new-required-steps]} (new-required-steps-with-workers finish-steps required-steps)
        new-visited-steps (concat visited-steps finish-steps)
        new-possible-steps (next-possible-steps-with-workers finish-steps possible-steps new-possible-steps)
        {:keys [on-process-steps new-process-steps]} (steps-start-process on-process-steps new-possible-steps)]
    (-> navigation
        (assoc
          :possible-steps (remove (set new-process-steps) new-possible-steps)
          :required-steps new-required-steps
          :visited-steps new-visited-steps
          :on-process-steps on-process-steps
          :timestamp (inc timestamp)))))

;1 작업을 한다.
;2 작업이 끝났다면 on-process-steps -> visited-step으로 옮긴다.
;3 끝난 작업은 visited-steps 에 넣는다.
;4 끝난 작업은 on-process-steps 에서 뺀다
;5 on-process-steps 에서 비어있는 작업자게에 작업을 할당해 준다

(defn head-for-goal-with-workers-v2
  "지도 정보를 통해 출발지에서 부터 목적지 까지 간다.
  {:all-steps (\"A\" \"B\" \"C\" \"D\" \"E\" \"F\"),
    :possible-steps (\"C\"),
    :goal-steps \"E\",
    :required-steps {:A (\"C\"), :B (\"A\"), :C (), :D (\"A\"), :E (\"B\" \"D\" \"F\"), :F (\"C\")},
    :next-steps {:A (\"B\" \"D\"), :B (\"E\"), :C (\"A\" \"F\"), :D (\"E\"), :E (), :F (\"E\")},
    :visited-steps []}
    "
  [{:keys [all-steps] :as navigation}]
  (->> (take-while #(not= all-steps (sort (:visited-steps %)))
              (iterate #(step-by-step %) navigation)))
  #_(take 2
                #_#(< (:timestamp %) 5)
                (iterate #(step-by-step %) navigation)))


;drop-while #(arrive? all-steps (:visited-steps %))
;(take 5 (iterate #(step-by-step %) navigation))
;
;(def test {:a 1})
;(defn test-fn [test] (assoc test :a (inc (:a test))))
;(test-fn test)
;
;(drop-while #(> (:a %) 5) (iterate #(test-fn %) test))
;
;
;(take-while #(< % 5) (iterate (fn [number] (inc number)) 1))

(comment
  (->> sample-input
       parse-input
       head-for-goal-with-workers-v2
       #_:timestamp)

  (->> aoc-input
       parse-input
       head-for-goal-with-workers
       :timestamp))




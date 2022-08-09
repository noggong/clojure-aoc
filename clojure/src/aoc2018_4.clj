(ns aoc2018_4
  (:require [clojure.string :as s]))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(def input `(
               "[1518-11-01 00:00] Guard #10 begins shift"
               "[1518-11-01 00:05] falls asleep"
               "[1518-11-01 00:25] wakes up"
               "[1518-11-01 00:30] falls asleep"
               "[1518-11-01 00:55] wakes up"
               "[1518-11-01 23:58] Guard #99 begins shift"
               "[1518-11-02 00:40] falls asleep"
               "[1518-11-02 00:50] wakes up"
               "[1518-11-03 00:05] Guard #10 begins shift"
               "[1518-11-03 00:24] falls asleep"
               "[1518-11-03 00:29] wakes up"
               "[1518-11-04 00:02] Guard #99 begins shift"
               "[1518-11-04 00:36] falls asleep"
               "[1518-11-04 00:46] wakes up"
               "[1518-11-05 00:03] Guard #99 begins shift"
               "[1518-11-05 00:45] falls asleep"
               "[1518-11-05 00:55] wakes up"
              ))

; recommended
; seq 분리할때 partition-by 사용 (https://clojuredocs.org/clojure.core/partition-by)

(defn input->log
  "입력 데이터 Guard를 기준으로 로그로 분리

  입력 : `(\"[1518-11-01 00:00] Guard #10 begins shift\"
         \"[1518-11-01 00:05] falls asleep\"
   출력 : [\"#10 begins shift,[1518-11-01 00:05] falls asleep,[1518-11-01 00:25] wakes up,[1518-11-01 00:30] falls asleep,[1518-11-01 00:55] wakes up\"
         \"#99 begins shift,[1518-11-02 00:40] falls asleep,[1518-11-02 00:50] wakes up\"]
  "
  [input]
  (let [joined-log (s/join "," input)
        split-log (s/split joined-log #"[,]{0,1}\[[^,]+\] Guard ")]
    (subvec split-log 1)))

; recommended input->log 거쳐가는것이 필요없어 보임

(defn even-index?
  "index 가 짝수 인지 확인"
  [key val]
  (when (even? key) val))

(defn odd-index?
  "index 가 홀수 인지 확인"
  [key val]
  (when (odd? key) val))

; recommended
; partition / partition-by 로 대체 할수 있음 (가급적 index 를 사용하지 않는것을 권장)

(defn log->split-by-type
  "
  입력 : \"#10 begins shift,[1518-11-01 00:05] falls asleep,[1518-11-01 00:25] wakes up,[1518-11-01 00:30] falls asleep,[1518-11-01 00:55] wakes up\"
  "
  [log]
  (let [guard-id (re-find #"#[0-9^/s]+" log)
        minutes (re-seq #":[0-9]{2}" log)
        minutes (map #(subs % 1) minutes)
        minutes (map #(Integer/parseInt %) minutes)
        ]
    {:guard-id guard-id
     :fall (keep-indexed even-index? minutes)
     :wake (keep-indexed odd-index? minutes)}))

(defn log->sleep
  "
  입력 : {:guard-id \"#1\", :fall (5 30), :wake (25 55)
  출력 : {:guard-id \"#9\", :sleeps ({:fall 40, :wake 50 })}
  "
  [{:keys [guard-id fall wake]}]
  (let [sleep (for [x (range 0 (count fall))]
                {:fall (nth fall x)
                 :wake (nth wake x)})]
    {:guard-id guard-id
     :sleeps sleep
     }
    ))
; recommended
; map 으로 map $({}) () / 사용 가능 / nth 를 잘 사용하지 않음

(defn refine-sleep
  "입력 :
  guard-id : #10
  sleeps [{:guard-id \"#1\", :sleep ({:fall 5, :wake 25} {:fall 30, :wake 55})}
         {:guard-id \"#1\", :sleep ({:fall 24, :wake 29})}]

  출력 : {:guard-id \"#9\", :sleep ({:fall 5, :wake 25} {:fall 30, :wake 55} {:fall 24, :wake 29}), :sum-minute 50}
  "
  [guard-id sleeps]
  (let [sleeps (map :sleeps sleeps)
        sleeps (flatten sleeps)
        sum-minute (map #(- (% :wake) (% :fall)) sleeps)]
    {
     :guard-id guard-id
     :sleep sleeps
     :sum-minute (reduce + sum-minute)
     }))
; recommended
;(let [sleeps (map #(% :sleeps) sleeps) -> map :sleeps sleeps
;        sleeps (flatten sleeps)
; let 을 다중으로 쓰기보단 ->> 혹은 한줄로 표기하는 것을 지향

(defn group-by-guard
  "가드별로 잠을 잔 시간을 group by 한다."
  [logs]
  logs
  (->> logs
       (group-by :guard-id)
       (map (fn [[k v]] (refine-sleep k v) ))
       ))

(defn log->guard-sleep
  "문자열 로그을 guard 별 잠잤던 로그로 정제한다."
  "입력 : [\"#99 begins shift,[1518-11-02 00:40] falls asleep,[1518-11-02 00:50] wakes up\"
         \"#10 begins shift,[1518-11-03 00:24] falls asleep,[1518-11-03 00:29] wakes up\"
         \"#99 begins shift,[1518-11-04 00:36] falls asleep,[1518-11-04 00:46] wakes up\"
         \"#99 begins shift,[1518-11-05 00:45] falls asleep,[1518-11-05 00:55] wakes up\"]
  출력 : ({:guard-id \"#10\", :sleep ({:fall 5, :wake 25} {:fall 30, :wake 55} {:fall 24, :wake 29}), :sum-minute 50}
        {:guard-id \"#99\", :sleep ({:fall 40, :wake 50} {:fall 36, :wake 46} {:fall 45, :wake 55}), :sum-minute 30})
  "
  [log]
  (->> log
       (map log->split-by-type)
       (map log->sleep)
       group-by-guard))
; naming / split-by-type 이 무엇으로 데이터가 반환될지 명확하지 않음.

(defn guard->sleepyhead
  "가장 많이 잠을 잔 guard 를 가져온다
  입력 : ({:guard-id \"#1\", :sleep ({:fall 5, :wake 25} {:fall 30, :wake 55} {:fall 24, :wake 29}), :sum-minute 50}
         {:guard-id \"#9\", :sleep ({:fall 40, :wake 50} {:fall 36, :wake 46} {:fall 45, :wake 55}), :sum-minute 30})

  출력: {:guard-id \"#9\", :sleep ({:fall 40, :wake 50} {:fall 36, :wake 46} {:fall 45, :wake 55}), :sum-minute 30}"

  [guard-log]
  (->> guard-log
       (sort-by :sum-minute)
       last))
; recommend
; naming sleepyhead?

(defn guard-id->integer
  "guard-id 를 숫자로 변경한다."
  [guard-id]
  (-> guard-id
      (subs 1)
      (Integer/parseInt)))

;recommended
; 파싱단계해서 하는것을 추천

(defn most-sleep
  "잠든 시간과 일어난 시간의 목록으로 가장 많이 잠들어있던 시간을 가져온다
  입력 : `({:fall 5, :wake 25} {:fall 30, :wake 55} {:fall 24, :wake 29})
  출력 : 24"
  [sleeps]
  (let [every-minutes (map
                        #(range (% :fall) (inc (% :wake)))
                        sleeps)]
    (->> every-minutes
         flatten
         frequencies
         (sort-by last >)
         first
         first)))

(defn multiply-most-minute
  "가장 많이 잠들어 있던 분(minute) 와 아이디의 곱을 반환한다.
  입력 : {:guard-id \"#10\", :sleep ({:fall 5, :wake 25} {:fall 30, :wake 55} {:fall 24, :wake 29}), :sum-minute 50}
  출력 : 240
  "
  [sleepyhead]
  (*
    (most-sleep (sleepyhead :sleep))
    (guard-id->integer (sleepyhead :guard-id))))

(comment
  (->> input
       input->log
       log->guard-sleep
       guard->sleepyhead
       multiply-most-minute
       ))
; recommended
; 함수 내부의 것을 꺼내도 된다 (경우에 따라서)

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
(defn multiply-sum-minute
  "가장 많이 잔 guard 의 아이디와 잔시간을 곱한다.
  입력 : {:guard-id \"#10\", :sleep ({:fall 5, :wake 25} {:fall 30, :wake 55} {:fall 24, :wake 29}), :sum-minute 50}
  출력 : 500
  "
  [sleepyhead]
  (* (sleepyhead :sum-minute) (guard-id->integer (sleepyhead :guard-id))))

(comment
  (->> input
       input->log
       log->guard-sleep
       guard->sleepyhead
       multiply-sum-minute
       ))


;recommended
; aggregation 에서는 data 변형을 지양한다 (즉 guard-id 는 미리 cast int)
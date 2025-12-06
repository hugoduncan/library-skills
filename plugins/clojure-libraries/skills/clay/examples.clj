#!/usr/bin/env clojure

;; Clay Examples
;; NOTE: Clay requires Clojure (JVM) and is NOT compatible with Babashka
;; Run with: clojure -M examples.clj
;; Or add deps.edn with clay dependency

(comment
  "To run these examples, create a deps.edn file:"
  {:deps {org.scicloj/clay {:mvn/version "2.0.3"}
          org.scicloj/kindly {:mvn/version "4-beta10"}}})

(require '[scicloj.clay.v2.api :as clay])
(require '[scicloj.kindly.v4.kind :as kind])

(println "=== Clay Visualization Examples ===\n")

;; ============================================================================
;; Basic Kinds
;; ============================================================================

(println "1. Markdown Rendering")
(def md-example
  (kind/md "# Hello Clay

This is **bold** and *italic* text.

- Item 1
- Item 2"))
(println "Created markdown kind:" (type md-example))
(println)

(println "2. Hiccup HTML")
(def hiccup-example
  (kind/hiccup
   [:div {:style {:padding "10px"
                  :background "#f0f0f0"}}
    [:h2 "Welcome"]
    [:p "This is a paragraph."]
    [:ul
     [:li "First"]
     [:li "Second"]
     [:li "Third"]]]))
(println "Created hiccup kind:" (type hiccup-example))
(println)

(println "3. Code Display")
(def code-example
  (kind/code
   "(defn factorial [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))"))
(println "Created code kind:" (type code-example))
(println)

(println "4. LaTeX Math")
(def tex-example
  (kind/tex "E = mc^2"))
(println "Created tex kind:" (type tex-example))
(println)

;; ============================================================================
;; Tables
;; ============================================================================

(println "5. Table with Row Vectors")
(def table-vectors
  (kind/table
   {:column-names [:name :age :city]
    :row-vectors [["Alice" 30 "New York"]
                  ["Bob" 25 "Los Angeles"]
                  ["Carol" 35 "Chicago"]]}))
(println "Created table kind:" (type table-vectors))
(println)

(println "6. Table with Row Maps")
(def table-maps
  (kind/table
   {:row-maps [{:product "Widget" :price 19.99 :qty 100}
               {:product "Gadget" :price 29.99 :qty 50}
               {:product "Gizmo" :price 9.99 :qty 200}]}))
(println "Created table kind:" (type table-maps))
(println)

;; ============================================================================
;; Vega-Lite Charts
;; ============================================================================

(println "7. Vega-Lite Line Chart")
(def line-chart
  (kind/vega-lite
   {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
    :data {:values [{:x 0 :y 0}
                    {:x 1 :y 1}
                    {:x 2 :y 4}
                    {:x 3 :y 9}
                    {:x 4 :y 16}]}
    :mark :line
    :encoding {:x {:field :x :type :quantitative}
               :y {:field :y :type :quantitative}}}))
(println "Created vega-lite kind:" (type line-chart))
(println)

(println "8. Vega-Lite Bar Chart")
(def bar-chart
  (kind/vega-lite
   {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
    :data {:values [{:category "A" :value 28}
                    {:category "B" :value 55}
                    {:category "C" :value 43}
                    {:category "D" :value 91}]}
    :mark :bar
    :encoding {:x {:field :category :type :nominal}
               :y {:field :value :type :quantitative}}}))
(println "Created bar chart kind:" (type bar-chart))
(println)

(println "9. Vega-Lite Scatter Plot")
(def scatter-plot
  (kind/vega-lite
   {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
    :data {:values (for [i (range 20)]
                     {:x (rand-int 100)
                      :y (rand-int 100)
                      :size (+ 10 (rand-int 40))})}
    :mark {:type :circle :opacity 0.7}
    :encoding {:x {:field :x :type :quantitative}
               :y {:field :y :type :quantitative}
               :size {:field :size :type :quantitative}}}))
(println "Created scatter plot kind:" (type scatter-plot))
(println)

;; ============================================================================
;; Plotly Charts
;; ============================================================================

(println "10. Plotly Line Chart")
(def plotly-line
  (kind/plotly
   {:data [{:x [1 2 3 4 5]
            :y [1 4 9 16 25]
            :type :scatter
            :mode :lines+markers
            :name "Squares"}]
    :layout {:title "Square Numbers"
             :xaxis {:title "X"}
             :yaxis {:title "Y"}}}))
(println "Created plotly kind:" (type plotly-line))
(println)

(println "11. Plotly Bar Chart")
(def plotly-bar
  (kind/plotly
   {:data [{:x ["Apples" "Bananas" "Oranges"]
            :y [20 14 23]
            :type :bar}]
    :layout {:title "Fruit Sales"}}))
(println "Created plotly bar kind:" (type plotly-bar))
(println)

;; ============================================================================
;; ECharts
;; ============================================================================

(println "12. ECharts Bar Chart")
(def echarts-bar
  (kind/echarts
   {:xAxis {:type :category
            :data ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]}
    :yAxis {:type :value}
    :series [{:data [120 200 150 80 70 110 130]
              :type :bar}]}))
(println "Created echarts kind:" (type echarts-bar))
(println)

(println "13. ECharts Line Chart")
(def echarts-line
  (kind/echarts
   {:xAxis {:type :category
            :data ["Jan" "Feb" "Mar" "Apr" "May" "Jun"]}
    :yAxis {:type :value}
    :series [{:data [820 932 901 934 1290 1330]
              :type :line
              :smooth true}]}))
(println "Created echarts line kind:" (type echarts-line))
(println)

;; ============================================================================
;; Diagrams
;; ============================================================================

(println "14. Mermaid Flowchart")
(def mermaid-flow
  (kind/mermaid
   "graph TD
    A[Start] --> B{Is it working?}
    B -->|Yes| C[Great!]
    B -->|No| D[Debug]
    D --> B"))
(println "Created mermaid kind:" (type mermaid-flow))
(println)

(println "15. Mermaid Sequence Diagram")
(def mermaid-seq
  (kind/mermaid
   "sequenceDiagram
    Client->>Server: Request
    Server->>Database: Query
    Database-->>Server: Results
    Server-->>Client: Response"))
(println "Created mermaid sequence kind:" (type mermaid-seq))
(println)

;; ============================================================================
;; Fragments
;; ============================================================================

(println "16. Fragment (Multiple Items)")
(def fragment-example
  (kind/fragment
   [(kind/md "## Section Title")
    (kind/hiccup [:p "A paragraph of text."])
    (kind/vega-lite
     {:data {:values [{:x 1 :y 1} {:x 2 :y 2}]}
      :mark :point
      :encoding {:x {:field :x} :y {:field :y}}})]))
(println "Created fragment kind:" (type fragment-example))
(println)

;; ============================================================================
;; Styling
;; ============================================================================

(println "17. Styled Hiccup")
(def styled-div
  (kind/hiccup
   [:div {:style {:border "2px solid blue"
                  :border-radius "8px"
                  :padding "20px"
                  :background "linear-gradient(#e0e0ff, #ffffff)"}}
    [:h3 {:style {:color "navy"}} "Styled Box"]
    [:p "Content with custom styling."]]))
(println "Created styled hiccup:" (type styled-div))
(println)

;; ============================================================================
;; Hidden Content
;; ============================================================================

(println "18. Hidden Kind")
(def hidden-computation
  (kind/hidden
   {:internal-data (range 1000)
    :computed-at (java.util.Date.)}))
(println "Created hidden kind:" (type hidden-computation))
(println)

;; ============================================================================
;; Practical Examples
;; ============================================================================

(println "19. Data Analysis Report Fragment")
(def sales-data
  [{:month "Jan" :sales 12000 :region "North"}
   {:month "Feb" :sales 15000 :region "North"}
   {:month "Mar" :sales 18000 :region "North"}
   {:month "Jan" :sales 8000 :region "South"}
   {:month "Feb" :sales 11000 :region "South"}
   {:month "Mar" :sales 14000 :region "South"}])

(def report
  (kind/fragment
   [(kind/md "# Sales Report Q1

## Overview
This report summarizes Q1 sales performance by region.")

    (kind/md "## Data Table")
    (kind/table {:row-maps sales-data})

    (kind/md "## Sales by Month")
    (kind/vega-lite
     {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
      :data {:values sales-data}
      :mark :bar
      :encoding {:x {:field :month :type :nominal}
                 :y {:field :sales :type :quantitative}
                 :color {:field :region :type :nominal}}})

    (kind/md "## Key Findings
- North region outperformed South
- March showed highest growth
- Total Q1 sales: $78,000")]))
(println "Created report fragment:" (type report))
(println)

(println "20. Interactive Dashboard Elements")
(def dashboard
  (kind/fragment
   [(kind/hiccup
     [:div {:style {:display "flex" :gap "20px"}}
      [:div {:style {:flex 1
                     :padding "20px"
                     :background "#e8f4e8"
                     :border-radius "8px"}}
       [:h4 "Total Users"]
       [:p {:style {:font-size "32px"
                    :font-weight "bold"}} "1,234"]]
      [:div {:style {:flex 1
                     :padding "20px"
                     :background "#e8e8f4"
                     :border-radius "8px"}}
       [:h4 "Revenue"]
       [:p {:style {:font-size "32px"
                    :font-weight "bold"}} "$45,678"]]
      [:div {:style {:flex 1
                     :padding "20px"
                     :background "#f4e8e8"
                     :border-radius "8px"}}
       [:h4 "Orders"]
       [:p {:style {:font-size "32px"
                    :font-weight "bold"}} "567"]]])

    (kind/vega-lite
     {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
      :data {:values (for [day (range 1 31)]
                       {:day day
                        :users (+ 30 (rand-int 20))})}
      :mark {:type :area :opacity 0.5}
      :encoding {:x {:field :day :type :quantitative :title "Day"}
                 :y {:field :users :type :quantitative :title "Active Users"}}
      :width 600
      :height 200})]))
(println "Created dashboard fragment:" (type dashboard))
(println)

;; ============================================================================
;; Rendering Example
;; ============================================================================

(println "21. Rendering to Browser")
(println "To render any of these examples to browser:")
(println "  (clay/make! {:single-form 'line-chart})")
(println "  (clay/make! {:single-form 'report})")
(println)

(println "22. Rendering Namespace")
(println "To render entire namespace:")
(println "  (clay/make! {:source-path \"path/to/notebook.clj\"})")
(println)

(println "\n=== Examples Complete ===")
(println "All examples created Clay visualization kinds.")
(println "Use clay/make! to render them in a browser.")

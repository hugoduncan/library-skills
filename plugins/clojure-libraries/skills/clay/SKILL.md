---
name: clay
description: REPL-friendly data visualization and literate programming for Clojure with Kindly convention
---

# Clay

REPL-friendly Clojure tool for data visualization and literate programming. Renders Clojure namespaces and forms as HTML pages, notebooks, and Quarto documents using the Kindly convention.

## Overview

Clay transforms Clojure code into visual documents by interpreting the Kindly convention for data visualization. It integrates with your REPL workflow and editor, rendering forms and namespaces as HTML with support for charts, tables, markdown, and interactive components.

**Key Features:**
- REPL-driven visualization workflow
- Kindly convention for visualization types
- HTML, Quarto markdown, and reveal.js output
- Live reload with file watching
- Editor integrations (Calva, CIDER, Cursive, Conjure)
- Vega-Lite, Plotly, ECharts, Highcharts support
- Dataset and table rendering
- Reagent component support

**Artifact:** `org.scicloj/clay`
**Latest Version:** 2.0.3
**License:** EPL-1.0
**Repository:** https://github.com/scicloj/clay

## Installation

Add to `deps.edn`:
```clojure
{:deps {org.scicloj/clay {:mvn/version "2.0.3"}}}
```

Or Leiningen `project.clj`:
```clojure
[org.scicloj/clay "2.0.3"]
```

Import in namespace:
```clojure
(ns my-notebook
  (:require [scicloj.clay.v2.api :as clay]
            [scicloj.kindly.v4.kind :as kind]))
```

## Core Concepts

### Kindly Convention

Kindly standardizes how values request visualization. Attach kind metadata to values:

```clojure
;; Using kind function
(kind/md "# Hello World")

;; Using metadata
^:kind/md ["# Hello World"]
```

### make! Function

The primary entry point. Renders forms, namespaces, or files:

```clojure
;; Render single form
(clay/make! {:single-form '(+ 1 2 3)})

;; Render current namespace
(clay/make! {:source-path "notebooks/my_notebook.clj"})

;; Render with options
(clay/make! {:source-path "src/analysis.clj"
             :format [:html]
             :show true})
```

### Output Formats

Clay produces:
- **HTML** - Standalone pages served at `http://localhost:1971/`
- **Quarto Markdown** - For publishing with Quarto
- **reveal.js** - Slideshows

## API Reference

### Core Functions

#### make!

Render content with configuration options.

```clojure
(clay/make! {:source-path "notebooks/analysis.clj"})

(clay/make! {:single-form '(kind/md "# Title")
             :show true})

(clay/make! {:source-path "src/report.clj"
             :format [:quarto :html]
             :base-target-path "docs"})
```

**Options:**
- `:source-path` - Path to .clj file to render
- `:single-form` - Single form to evaluate and render
- `:format` - Output format(s): `:html`, `:quarto`, `:revealjs`
- `:show` - Open browser after rendering (default: true)
- `:browse` - Enable browser opening
- `:title` - Document title
- `:favicon` - Path to favicon
- `:base-target-path` - Output directory (default: "docs")
- `:quarto` - Quarto-specific configuration map

#### browse!

Reopen the Clay view in browser.

```clojure
(clay/browse!)
```

### Configuration

Configuration merges from multiple sources (lowest to highest priority):
1. Default settings (`clay-default.edn`)
2. User config (`~/.config/scicloj-clay/config.edn`)
3. Project config (`clay.edn`)
4. Namespace metadata (`:clay` key)
5. Function call arguments

**Project clay.edn:**
```clojure
{:source-path "notebooks"
 :base-target-path "docs"
 :show true
 :format [:html]
 :quarto {:format {:html {:theme :cosmo}}}}
```

**Namespace metadata:**
```clojure
(ns my-notebook
  {:clay {:title "My Analysis"
          :quarto {:format {:html {:toc true}}}}})
```

## Visualization Kinds

### Markup

#### kind/md - Markdown

```clojure
(kind/md "# Heading

Paragraph with **bold** and *italic*.

- List item 1
- List item 2")

;; With LaTeX
(kind/md "Let $x=9$. Then $x^2=81$.")
```

#### kind/hiccup - HTML

```clojure
(kind/hiccup
 [:div {:style {:color "blue"}}
  [:h1 "Title"]
  [:p "Paragraph"]
  [:ul
   [:li "Item 1"]
   [:li "Item 2"]]])
```

#### kind/html - Raw HTML

```clojure
(kind/html "<div class='alert'>Warning!</div>")
```

#### kind/code - Code Display

```clojure
(kind/code "(defn hello [name]
  (str \"Hello, \" name \"!\"))")
```

#### kind/tex - LaTeX

```clojure
(kind/tex "\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}")
```

### Data Visualization

#### kind/vega-lite - Vega-Lite Charts

```clojure
(kind/vega-lite
 {:data {:values [{:x 1 :y 2}
                  {:x 2 :y 4}
                  {:x 3 :y 3}]}
  :mark :line
  :encoding {:x {:field :x :type :quantitative}
             :y {:field :y :type :quantitative}}})

;; Bar chart
(kind/vega-lite
 {:data {:values [{:category "A" :value 28}
                  {:category "B" :value 55}
                  {:category "C" :value 43}]}
  :mark :bar
  :encoding {:x {:field :category :type :nominal}
             :y {:field :value :type :quantitative}}})
```

#### kind/plotly - Plotly Charts

```clojure
(kind/plotly
 {:data [{:x [1 2 3 4]
          :y [10 15 13 17]
          :type :scatter
          :mode :lines+markers}]
  :layout {:title "Line Chart"}})

;; 3D scatter
(kind/plotly
 {:data [{:x [1 2 3]
          :y [1 2 3]
          :z [1 2 3]
          :type :scatter3d
          :mode :markers}]})
```

#### kind/echarts - Apache ECharts

```clojure
(kind/echarts
 {:xAxis {:type :category
          :data ["Mon" "Tue" "Wed" "Thu" "Fri"]}
  :yAxis {:type :value}
  :series [{:data [150 230 224 218 135]
            :type :bar}]})
```

#### kind/highcharts - Highcharts

```clojure
(kind/highcharts
 {:chart {:type :line}
  :title {:text "Monthly Sales"}
  :xAxis {:categories ["Jan" "Feb" "Mar" "Apr"]}
  :series [{:name "Sales"
            :data [29.9 71.5 106.4 129.2]}]})
```

### Tabular Data

#### kind/table - Tables

```clojure
;; Row vectors
(kind/table
 {:column-names [:name :age :city]
  :row-vectors [["Alice" 30 "NYC"]
                ["Bob" 25 "LA"]
                ["Carol" 35 "Chicago"]]})

;; Row maps
(kind/table
 {:row-maps [{:name "Alice" :age 30}
             {:name "Bob" :age 25}]})

;; With DataTables options
(kind/table
 {:row-maps data
  :use-datatables true})
```

#### kind/dataset - tech.ml.dataset

```clojure
(require '[tablecloth.api :as tc])

(-> (tc/dataset {:x [1 2 3]
                 :y [4 5 6]})
    kind/dataset)

;; With print options
(-> dataset
    (kind/dataset {:dataset/print-range 20}))
```

### Media

#### kind/image - Images

```clojure
;; From URL
(kind/image {:src "https://example.com/image.png"})

;; BufferedImage object
(kind/image buffered-image)

;; With caption
(kind/image {:src "chart.png"
             :alt "Sales Chart"})
```

#### kind/video - Video

```clojure
;; URL
(kind/video {:src "video.mp4"})

;; YouTube
(kind/video {:youtube-id "dQw4w9WgXcQ"})
```

### Interactive Components

#### kind/reagent - Reagent Components

```clojure
(kind/reagent
 ['(fn []
     (let [count (reagent.core/atom 0)]
       (fn []
         [:div
          [:p "Count: " @count]
          [:button {:on-click #(swap! count inc)}
           "Increment"]])))])

;; With dependencies
(kind/reagent
 ['(fn [] [:div "Component"])]
 {:deps [:reagent]})
```

### Structure

#### kind/fragment - Multiple Items

```clojure
(kind/fragment
 [(kind/md "# Section 1")
  (kind/vega-lite chart-spec)
  (kind/md "## Analysis")
  (kind/table data)])
```

#### kind/hidden - Hide Output

```clojure
(kind/hidden (expensive-computation))
```

### Diagrams

#### kind/mermaid - Flowcharts

```clojure
(kind/mermaid
 "graph TD
    A[Start] --> B{Decision}
    B -->|Yes| C[Do Something]
    B -->|No| D[Do Other]")
```

#### kind/cytoscape - Network Graphs

```clojure
(kind/cytoscape
 {:elements {:nodes [{:data {:id "a"}}
                     {:data {:id "b"}}]
             :edges [{:data {:source "a" :target "b"}}]}
  :style [{:selector "node"
           :style {:label "data(id)"}}]
  :layout {:name "preset"}})
```

## Common Patterns

### Basic Notebook Structure

```clojure
(ns my-notebook
  {:clay {:title "Data Analysis"}}
  (:require [scicloj.kindly.v4.kind :as kind]
            [tablecloth.api :as tc]))

;; # Introduction
;; This notebook analyzes...

^:kindly/hide-code
(def data (tc/dataset {:x (range 10)
                       :y (map #(* % %) (range 10))}))

;; ## Data Overview

(kind/table {:row-maps (tc/rows data :as-maps)})

;; ## Visualization

(kind/vega-lite
 {:data {:values (tc/rows data :as-maps)}
  :mark :point
  :encoding {:x {:field :x :type :quantitative}
             :y {:field :y :type :quantitative}}})
```

### Hide Code

```clojure
;; Hide code for specific form
^:kindly/hide-code
(kind/md "Only output visible")

;; Global hide for certain kinds
{:kindly/options {:kinds-that-hide-code #{:kind/md :kind/hiccup}}}
```

### Styling

```clojure
;; CSS via kindly options
(-> (kind/hiccup [:div "Styled"])
    (kind/hiccup {:element/style {:color "red"
                                  :font-size "20px"}}))

;; CSS classes
(kind/hiccup
 [:div {:class "alert alert-warning"}
  "Warning message"])
```

### Quarto Integration

```clojure
(ns my-doc
  {:clay {:quarto {:format {:html {:toc true
                                   :theme :cosmo}}
                   :title "My Document"
                   :author "Name"}}})

;; Generate Quarto output
(clay/make! {:source-path "notebooks/doc.clj"
             :format [:quarto]})
```

### Multi-File Projects

```clojure
;; Render multiple files
(clay/make! {:source-path ["src/intro.clj"
                           "src/analysis.clj"
                           "src/conclusion.clj"]
             :base-target-path "docs"})
```

### File Watching

```clojure
;; Watch for changes
(clay/make! {:source-path "notebooks"
             :watch true})
```

## Editor Integration

### Calva (VSCode)

Use Calva Power Tools or add to `.clojure/calva.exports/config.edn`:
```clojure
{:customREPLCommandSnippets
 [{:name "Clay: Make HTML"
   :snippet "(do (require '[scicloj.clay.v2.api :as clay])
                 (clay/make! {:source-path \"$current-file-name\"
                              :format [:html]}))"}]}
```

### CIDER (Emacs)

Install `clay.el` package or add manually:
```elisp
(defun clay-make ()
  (interactive)
  (cider-interactive-eval
   (format "(do (require '[scicloj.clay.v2.api :as clay])
                (clay/make! {:source-path \"%s\"}))"
           (buffer-file-name))))
```

### Cursive (IntelliJ)

Add to `.idea/repl-commands.xml`:
```xml
<command command="(do (require '[scicloj.clay.v2.api :as clay])
                      (clay/make! {:source-path &quot;$FilePath$&quot;}))"
         name="Clay: Make HTML"/>
```

### Conjure (Neovim)

Install `clay.nvim` plugin.

## Performance Considerations

### Delays for Expensive Computations

```clojure
;; Computation runs only when visualized
(delay
  (Thread/sleep 5000)
  (kind/md "Slow result"))
```

### Large Datasets

```clojure
;; Limit displayed rows
(-> large-dataset
    (kind/dataset {:dataset/print-range 100}))
```

### Subdirectory Syncing

```clojure
;; Only sync specified directories (excludes src)
{:subdirs-to-sync ["notebooks" "data"]}
```

## Error Handling

### Missing Dependencies

Ensure visualization libraries are loaded:
```clojure
;; For datasets
{:deps {scicloj/tablecloth {:mvn/version "..."}}}

;; For specific chart types
{:deps {applied-science/darkstar {:mvn/version "..."}}}
```

### Kind Not Rendering

Verify kind is attached correctly:
```clojure
;; Check metadata
(meta (kind/md "test"))
;;=> {:kindly/kind :kind/md}
```

## Platform Notes

**JVM Only:** Clay requires the JVM and is not compatible with Babashka or ClojureScript compilation. It renders ClojureScript components (Reagent) but runs on the JVM.

**Browser Required:** Visualization output is served via local HTTP server and displayed in browser.

## Resources

- **Documentation:** https://scicloj.github.io/clay/
- **GitHub:** https://github.com/scicloj/clay
- **Kindly Spec:** https://scicloj.github.io/kindly-noted/
- **Zulip:** Clojurians #clay-dev channel

## License

Copyright © Scicloj
Distributed under the EPL-1.0 (same as Clojure)

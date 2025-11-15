#!/usr/bin/env clojure

;; Selmer Examples
;; NOTE: Selmer requires Clojure (JVM) and is NOT compatible with Babashka
;; Run with: clojure -M examples.clj
;; Or add deps.edn with selmer dependency

(comment
  "To run these examples, create a deps.edn file:"
  {:deps {selmer/selmer {:mvn/version "1.12.65"}}}
  "Then run: clojure -M examples.clj")

(require '[selmer.parser :as parser])
(require '[selmer.filters :as filters])

(println "=== Selmer Templating Examples ===\n")

;; ============================================================================
;; Basic Rendering
;; ============================================================================

(println "1. Basic Variable Rendering")
(println (parser/render "Hello {{name}}!" {:name "World"}))
(println (parser/render "{{greeting}}, {{name}}!"
                        {:greeting "Hi" :name "Alice"}))
(println)

(println "2. Nested Data Access")
(println (parser/render "{{person.name}} is {{person.age}} years old"
                        {:person {:name "Bob" :age 30}}))
(println (parser/render "First item: {{items.0}}"
                        {:items ["apple" "banana" "cherry"]}))
(println)

;; ============================================================================
;; Filters
;; ============================================================================

(println "3. String Filters")
(println (parser/render "{{name|upper}}" {:name "alice"}))
(println (parser/render "{{name|lower}}" {:name "ALICE"}))
(println (parser/render "{{text|capitalize}}" {:text "hello world"}))
(println (parser/render "{{text|title}}" {:text "hello world"}))
(println)

(println "4. Filter Chaining")
(println (parser/render "{{text|upper|take:5}}" {:text "hello world"}))
(println (parser/render "{{items|join:\", \"|upper}}"
                        {:items ["a" "b" "c"]}))
(println)

(println "5. Collection Filters")
(println (parser/render "Count: {{items|count}}"
                        {:items [1 2 3 4 5]}))
(println (parser/render "First: {{items|first}}, Last: {{items|last}}"
                        {:items [10 20 30]}))
(println (parser/render "Joined: {{items|join:\", \"}}"
                        {:items ["red" "green" "blue"]}))
(println (parser/render "Reversed: {{items|reverse|join:\"-\"}}"
                        {:items [1 2 3]}))
(println)

(println "6. Utility Filters")
(println (parser/render "Value: {{val|default:\"N/A\"}}" {:val nil}))
(println (parser/render "Value: {{val|default:\"N/A\"}}" {:val "Present"}))
(println (parser/render "Length: {{text|length}}" {:text "hello"}))
(println)

;; ============================================================================
;; Control Flow
;; ============================================================================

(println "7. If/Else Tags")
(println (parser/render "{% if user %}Hello {{user}}!{% else %}Please log in{% endif %}"
                        {:user "Alice"}))
(println (parser/render "{% if user %}Hello {{user}}!{% else %}Please log in{% endif %}"
                        {}))
(println)

(println "8. If with Comparisons")
(println (parser/render "{% if count > 10 %}Many{% elif count > 0 %}Few{% else %}None{% endif %}"
                        {:count 15}))
(println (parser/render "{% if count > 10 %}Many{% elif count > 0 %}Few{% else %}None{% endif %}"
                        {:count 5}))
(println (parser/render "{% if count > 10 %}Many{% elif count > 0 %}Few{% else %}None{% endif %}"
                        {:count 0}))
(println)

(println "9. Ifequal/Ifunequal")
(println (parser/render "{% ifequal role \"admin\" %}Admin panel{% endifequal %}"
                        {:role "admin"}))
(println (parser/render "{% ifunequal status \"active\" %}Inactive{% endifunequal %}"
                        {:status "pending"}))
(println)

;; ============================================================================
;; Loops
;; ============================================================================

(println "10. For Loops")
(def for-template
  "{% for item in items %}{{forloop.counter}}. {{item}}\n{% endfor %}")
(println (parser/render for-template {:items ["apple" "banana" "cherry"]}))

(println "11. For Loop Variables")
(def loop-vars-template
  "{% for item in items %}{{item}}{% if not forloop.last %}, {% endif %}{% endfor %}")
(println (parser/render loop-vars-template {:items [1 2 3 4 5]}))
(println)

(println "12. For with Empty")
(def for-empty-template
  "{% for item in items %}{{item}}\n{% empty %}No items found{% endfor %}")
(println (parser/render for-empty-template {:items []}))
(println (parser/render for-empty-template {:items ["present"]}))
(println)

(println "13. Destructuring in For Loops")
(def destructure-template
  "{% for [k v] in pairs %}{{k}}: {{v}}\n{% endfor %}")
(println (parser/render destructure-template
                        {:pairs [["name" "Alice"]
                                 ["age" "30"]
                                 ["city" "NYC"]]}))

;; ============================================================================
;; Template Includes (Inline)
;; ============================================================================

(println "14. Template Composition with Render")
(def header-template "=== {{title}} ===")
(def content-template "{{header}}\n{{body}}")
(let [header (parser/render header-template {:title "Welcome"})
      result (parser/render content-template
                           {:header header
                            :body "This is the content"})]
  (println result))
(println)

;; ============================================================================
;; Custom Filters
;; ============================================================================

(println "15. Custom Filters")
(filters/add-filter! :shout
  (fn [s] (str (clojure.string/upper-case s) "!!!")))

(println (parser/render "{{msg|shout}}" {:msg "hello"}))

(filters/add-filter! :repeat
  (fn [s n] (apply str (repeat (Integer/parseInt n) s))))

(println (parser/render "{{x|repeat:3}}" {:x "ha"}))
(println)

;; ============================================================================
;; Custom Tags
;; ============================================================================

(println "16. Custom Tags")
(parser/add-tag! :uppercase
  (fn [args context-map]
    (clojure.string/upper-case (first args))))

(println (parser/render "{% uppercase \"hello world\" %}" {}))
(println)

;; ============================================================================
;; Escaping
;; ============================================================================

(println "17. HTML Escaping (Default)")
(println (parser/render "{{html}}" {:html "<b>Bold</b>"}))

(println "\n18. Safe Filter (No Escaping)")
(println (parser/render "{{html|safe}}" {:html "<b>Bold</b>"}))
(println)

;; ============================================================================
;; Variable Introspection
;; ============================================================================

(println "19. Known Variables")
(println "Variables in template '{{x}} {{y.z}}':")
(println (parser/known-variables "{{x}} {{y.z}}"))
(println)

;; ============================================================================
;; Practical Examples
;; ============================================================================

(println "20. Email Template Example")
(def email-template
  "Dear {{name}},

Thank you for registering! Your account details:

Username: {{username}}
Email: {{email}}
Registered: {% now \"yyyy-MM-dd HH:mm\" %}

{% if premium %}
Premium features are now available!
{% else %}
Upgrade to premium for additional features.
{% endif %}

Best regards,
The Team")

(println (parser/render email-template
                        {:name "Alice Johnson"
                         :username "alice"
                         :email "alice@example.com"
                         :premium true}))
(println)

(println "21. HTML List Example")
(def list-template
  "<ul>
{% for item in items %}
  <li class=\"{% cycle 'odd' 'even' %}\">
    {{item.name}} - ${{item.price|double-format:\"%.2f\"}}
  </li>
{% endfor %}
</ul>")

(println (parser/render list-template
                        {:items [{:name "Widget" :price 19.99}
                                 {:name "Gadget" :price 29.50}
                                 {:name "Doohickey" :price 9.99}]}))
(println)

(println "22. Conditional Navigation")
(def nav-template
  "<nav>
  {% if user %}
    Welcome, {{user.name}}!
    {% ifequal user.role \"admin\" %}
      <a href=\"/admin\">Admin Panel</a>
    {% endifequal %}
    <a href=\"/logout\">Logout</a>
  {% else %}
    <a href=\"/login\">Login</a>
    <a href=\"/register\">Register</a>
  {% endif %}
</nav>")

(println "Admin user:")
(println (parser/render nav-template
                        {:user {:name "Admin" :role "admin"}}))

(println "\nRegular user:")
(println (parser/render nav-template
                        {:user {:name "Bob" :role "user"}}))

(println "\nNot logged in:")
(println (parser/render nav-template {}))
(println)

(println "23. Data Table Example")
(def table-template
  "<table>
  <thead>
    <tr><th>#</th><th>Name</th><th>Status</th></tr>
  </thead>
  <tbody>
  {% for user in users %}
    <tr>
      <td>{{forloop.counter}}</td>
      <td>{{user.name|title}}</td>
      <td>{% if user.active %}Active{% else %}Inactive{% endif %}</td>
    </tr>
  {% empty %}
    <tr><td colspan=\"3\">No users found</td></tr>
  {% endfor %}
  </tbody>
</table>")

(println (parser/render table-template
                        {:users [{:name "alice" :active true}
                                 {:name "bob" :active false}
                                 {:name "charlie" :active true}]}))

(println "\n=== Examples Complete ===")

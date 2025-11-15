---
name: selmer
description: Django-inspired HTML templating system for Clojure with filters, tags, and template inheritance
---

# Selmer

Django-inspired HTML templating system for Clojure providing a fast, productive templating experience with filters, tags, template inheritance, and extensive customization options.

## Overview

Selmer is a pure Clojure template engine inspired by Django's template syntax. It compiles templates at runtime, supports template inheritance and includes, provides extensive built-in filters and tags, and allows custom extensions. Designed for server-side rendering in web applications, email generation, reports, and any text-based output.

**Key Features:**
- Django-compatible template syntax
- Variable interpolation with nested data access
- 50+ built-in filters for data transformation
- Control flow tags (if, for, with)
- Template inheritance (extends, block, include)
- Custom filter and tag creation
- Template caching for performance
- Auto-escaping with override control
- Validation and error reporting
- Middleware support

**Installation:**

Leiningen:
```clojure
[selmer "1.12.65"]
```

deps.edn:
```clojure
{selmer/selmer {:mvn/version "1.12.65"}}
```

**Quick Start:**
```clojure
(require '[selmer.parser :refer [render render-file]])

;; Render string
(render "Hello {{name}}!" {:name "World"})
;=> "Hello World!"

;; Render file
(render-file "templates/home.html" {:user "Alice"})
```

## Core Concepts

### Variables

Variables use `{{variable}}` syntax and are replaced with values from the context map.

```clojure
(render "{{greeting}} {{name}}" {:greeting "Hello" :name "Bob"})
;=> "Hello Bob"
```

**Nested Access:**
```clojure
(render "{{person.name}}" {:person {:name "Alice"}})
;=> "Alice"

(render "{{items.0.title}}" {:items [{:title "First"}]})
;=> "First"
```

**Missing Values:**
```clojure
(render "{{missing}}" {})
;=> "" (empty string by default)
```

### Filters

Filters transform variable values using the pipe `|` operator.

```clojure
(render "{{name|upper}}" {:name "alice"})
;=> "ALICE"

;; Chain filters
(render "{{text|upper|take:5}}" {:text "hello world"})
;=> "HELLO"
```

### Tags

Tags use `{% tag %}` syntax for control flow and template structure.

```clojure
{% if user %}
  Welcome {{user}}!
{% else %}
  Please log in.
{% endif %}
```

### Template Inheritance

Parent template (`base.html`):
```html
<html>
  <head>{% block head %}Default Title{% endblock %}</head>
  <body>{% block content %}{% endblock %}</body>
</html>
```

Child template:
```html
{% extends "base.html" %}
{% block head %}Custom Title{% endblock %}
{% block content %}<h1>Hello!</h1>{% endblock %}
```

## API Reference

### Rendering Functions

#### `render`
Render a template string with context.

```clojure
(render template-string context-map)
(render template-string context-map options)

;; Examples
(render "{{x}}" {:x 42})
;=> "42"

(render "[% x %]" {:x 42}
        {:tag-open "[%" :tag-close "%]"})
;=> "42"
```

**Parameters:**
- `template-string` - Template as string
- `context-map` - Data map for template
- `options` - Optional map with `:tag-open`, `:tag-close`, `:filter-open`, `:filter-close`

#### `render-file`
Render a template file from classpath or resource path.

```clojure
(render-file filename context-map)
(render-file filename context-map options)

;; Examples
(render-file "templates/email.html" {:name "Alice"})

(render-file "custom.tpl" {:x 1}
             {:tag-open "<%%" :tag-close "%%>"})
```

**File Resolution:**
1. Checks configured resource path
2. Falls back to classpath
3. Caches compiled template

### Caching

#### `cache-on!`
Enable template caching (default).

```clojure
(require '[selmer.parser :refer [cache-on!]])

(cache-on!)
```

Templates are compiled once and cached. Use in production.

#### `cache-off!`
Disable template caching for development.

```clojure
(require '[selmer.parser :refer [cache-off!]])

(cache-off!)
```

Templates recompile on each render. Use during development.

### Configuration

#### `set-resource-path!`
Configure base path for template files.

```clojure
(require '[selmer.parser :refer [set-resource-path!]])

(set-resource-path! "/var/html/templates/")
(set-resource-path! nil) ; Reset to classpath
```

#### `set-missing-value-formatter!`
Configure how missing values are rendered.

```clojure
(require '[selmer.parser :refer [set-missing-value-formatter!]])

(set-missing-value-formatter!
  (fn [tag context-map]
    (str "MISSING: " tag)))

(render "{{missing}}" {})
;=> "MISSING: missing"
```

### Introspection

#### `known-variables`
Extract all variables from a template.

```clojure
(require '[selmer.parser :refer [known-variables]])

(known-variables "{{x}} {{y.z}}")
;=> #{:x :y.z}
```

Useful for validation and documentation.

### Validation

#### `validate-on!` / `validate-off!`
Control template validation.

```clojure
(require '[selmer.validator :refer [validate-on! validate-off!]])

(validate-on!)  ; Default - validates templates
(validate-off!) ; Skip validation for performance
```

Validation catches undefined filters, malformed tags, and syntax errors.

### Custom Filters

#### `add-filter!`
Register a custom filter.

```clojure
(require '[selmer.filters :refer [add-filter!]])

(add-filter! :shout
  (fn [s] (str (clojure.string/upper-case s) "!!!")))

(render "{{msg|shout}}" {:msg "hello"})
;=> "HELLO!!!"

;; With arguments
(add-filter! :repeat
  (fn [s n] (apply str (repeat (Integer/parseInt n) s))))

(render "{{x|repeat:3}}" {:x "ha"})
;=> "hahaha"
```

#### `remove-filter!`
Remove a filter.

```clojure
(require '[selmer.filters :refer [remove-filter!]])

(remove-filter! :shout)
```

### Custom Tags

#### `add-tag!`
Register a custom tag.

```clojure
(require '[selmer.parser :refer [add-tag!]])

(add-tag! :uppercase
  (fn [args context-map]
    (clojure.string/upper-case (first args))))

;; In template: {% uppercase "hello" %}
```

**Block Tags:**
```clojure
(add-tag! :bold
  (fn [args context-map content]
    (str "<b>" (get-in content [:bold :content]) "</b>"))
  :bold :endbold)

;; In template:
;; {% bold %}text here{% endbold %}
```

#### `remove-tag!`
Remove a tag.

```clojure
(require '[selmer.parser :refer [remove-tag!]])

(remove-tag! :uppercase)
```

### Error Handling

#### `wrap-error-page`
Middleware to display template errors with context.

```clojure
(require '[selmer.middleware :refer [wrap-error-page]])

(def app
  (wrap-error-page handler))
```

Shows error message, line number, and template snippet.

### Escaping Control

#### `without-escaping`
Render template without HTML escaping.

```clojure
(require '[selmer.util :refer [without-escaping]])

(render "{{html}}" {:html "<b>Bold</b>"})
;=> "&lt;b&gt;Bold&lt;/b&gt;"

(without-escaping
  (render "{{html}}" {:html "<b>Bold</b>"}))
;=> "<b>Bold</b>"
```

## Built-in Filters

### String Filters

**upper** - Convert to uppercase
```clojure
{{name|upper}} ; "alice" → "ALICE"
```

**lower** - Convert to lowercase
```clojure
{{NAME|lower}} ; "ALICE" → "alice"
```

**capitalize** - Capitalize first letter
```clojure
{{word|capitalize}} ; "hello" → "Hello"
```

**title** - Title case
```clojure
{{phrase|title}} ; "hello world" → "Hello World"
```

**addslashes** - Escape quotes
```clojure
{{text|addslashes}} ; "I'm" → "I\'m"
```

**remove-tags** - Strip HTML tags
```clojure
{{html|remove-tags}} ; "<b>text</b>" → "text"
```

**safe** - Mark as safe (no escaping)
```clojure
{{html|safe}} ; Renders HTML without escaping
```

**replace** - Replace substring
```clojure
{{text|replace:"old":"new"}}
```

**subs** - Substring
```clojure
{{text|subs:0:5}} ; First 5 characters
```

**abbreviate** - Truncate with ellipsis
```clojure
{{text|abbreviate:10}} ; "Long text..." (max 10 chars)
```

### Formatting Filters

**date** - Format date
```clojure
{{timestamp|date:"yyyy-MM-dd"}}
{{timestamp|date:"MMM dd, yyyy"}}
```

**currency-format** - Format currency
```clojure
{{amount|currency-format}} ; 1234.5 → "$1,234.50"
```

**double-format** - Format decimal
```clojure
{{number|double-format:"%.2f"}} ; 3.14159 → "3.14"
```

**pluralize** - Pluralize noun
```clojure
{{count}} item{{count|pluralize}}
; 1 item, 2 items

{{count}} box{{count|pluralize:"es"}}
; 1 box, 2 boxes
```

### Collection Filters

**count** - Get collection size
```clojure
{{items|count}} ; [1 2 3] → "3"
```

**first** - First element
```clojure
{{items|first}} ; [1 2 3] → "1"
```

**last** - Last element
```clojure
{{items|last}} ; [1 2 3] → "3"
```

**join** - Join with separator
```clojure
{{items|join:", "}} ; [1 2 3] → "1, 2, 3"
```

**sort** - Sort collection
```clojure
{{items|sort}} ; [3 1 2] → [1 2 3]
```

**sort-by** - Sort by key
```clojure
{{people|sort-by:"age"}}
```

**reverse** - Reverse collection
```clojure
{{items|reverse}} ; [1 2 3] → [3 2 1]
```

**take** - Take first N
```clojure
{{items|take:2}} ; [1 2 3] → [1 2]
```

**drop** - Drop first N
```clojure
{{items|drop:1}} ; [1 2 3] → [2 3]
```

### Utility Filters

**default** - Default if falsy
```clojure
{{value|default:"N/A"}}
```

**default-if-empty** - Default if empty
```clojure
{{text|default-if-empty:"None"}}
```

**hash** - Compute hash
```clojure
{{text|hash:"md5"}}
{{text|hash:"sha256"}}
```

**json** - Convert to JSON
```clojure
{{data|json}} ; {:x 1} → "{\"x\":1}"
```

**length** - String/collection length
```clojure
{{text|length}} ; "hello" → "5"
```

## Built-in Tags

### Control Flow

#### `if` / `else` / `endif`
Conditional rendering.

```clojure
{% if user %}
  Hello {{user}}!
{% else %}
  Please log in.
{% endif %}
```

**With operators:**
```clojure
{% if count > 10 %}
  Many items
{% elif count > 0 %}
  Few items
{% else %}
  No items
{% endif %}
```

**Operators:** `=`, `!=`, `<`, `>`, `<=`, `>=`, `and`, `or`, `not`

#### `ifequal` / `ifunequal`
Compare two values.

```clojure
{% ifequal user.role "admin" %}
  Admin panel
{% endifequal %}

{% ifunequal status "active" %}
  Inactive
{% endifunequal %}
```

#### `firstof`
Render first truthy value.

```clojure
{% firstof user.nickname user.name "Guest" %}
```

### Loops

#### `for`
Iterate over collections.

```clojure
{% for item in items %}
  {{forloop.counter}}. {{item}}
{% endfor %}
```

**Loop Variables:**
- `forloop.counter` - 1-indexed position
- `forloop.counter0` - 0-indexed position
- `forloop.first` - True on first iteration
- `forloop.last` - True on last iteration
- `forloop.length` - Total items

**With empty:**
```clojure
{% for item in items %}
  {{item}}
{% empty %}
  No items found
{% endfor %}
```

**Destructuring:**
```clojure
{% for [k v] in pairs %}
  {{k}}: {{v}}
{% endfor %}
```

#### `cycle`
Cycle through values in a loop.

```clojure
{% for item in items %}
  <tr class="{% cycle 'odd' 'even' %}">{{item}}</tr>
{% endfor %}
```

### Template Structure

#### `extends`
Inherit from parent template.

```clojure
{% extends "base.html" %}
```

Must be first tag in template.

#### `block`
Define overridable section.

Parent template:
```html
{% block content %}Default content{% endblock %}
```

Child template:
```html
{% block content %}Custom content{% endblock %}
```

**Block super:**
```html
{% block content %}
  {{block.super}} Additional content
{% endblock %}
```

#### `include`
Insert another template.

```clojure
{% include "header.html" %}
```

**With context:**
```clojure
{% include "item.html" with item=product %}
```

### Other Tags

#### `comment`
Template comments (not rendered).

```clojure
{% comment %}
  This won't appear in output
{% endcomment %}
```

#### `now`
Render current timestamp.

```clojure
{% now "yyyy-MM-dd HH:mm" %}
```

#### `with`
Create local variables.

```clojure
{% with total=items|count %}
  Total: {{total}}
{% endwith %}
```

#### `verbatim`
Render content without processing.

```clojure
{% verbatim %}
  {{this}} won't be processed
{% endverbatim %}
```

Useful for client-side templates.

#### `script` / `style`
Include script/style blocks without escaping.

```clojure
{% script %}
  var x = {{data|json}};
{% endscript %}

{% style %}
  .class { color: {{color}}; }
{% endstyle %}
```

#### `debug`
Output context map for debugging.

```clojure
{% debug %}
```

## Common Patterns

### Email Templates

```clojure
(defn send-welcome-email [user]
  (let [html (render-file "emails/welcome.html"
                          {:name (:name user)
                           :activation-link (generate-link user)})]
    (send-email {:to (:email user)
                 :subject "Welcome!"
                 :body html})))
```

### Web Page Rendering

```clojure
(defn home-handler [request]
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body (render-file "pages/home.html"
                      {:user (:user request)
                       :posts (fetch-recent-posts)})})
```

### Template Fragments

```clojure
;; Reusable components
(render-file "components/button.html"
             {:text "Click me"
              :action "/submit"})
```

### Dynamic Form Generation

```clojure
(render-file "forms/user-form.html"
             {:fields [{:name "username" :type "text"}
                       {:name "email" :type "email"}
                       {:name "password" :type "password"}]
              :action "/register"})
```

### Report Generation

```clojure
(defn generate-report [data]
  (render-file "reports/monthly.html"
               {:period (format-period)
                :totals (calculate-totals data)
                :items data
                :generated-at (java.time.LocalDateTime/now)}))
```

### Template Composition

```clojure
;; Base layout
{% extends "layouts/main.html" %}

;; Page-specific
{% block title %}Dashboard{% endblock %}

{% block content %}
  {% include "components/stats.html" %}
  {% include "components/chart.html" %}
{% endblock %}
```

### Custom Marker Syntax

```clojure
;; Compatible with client-side frameworks
(render-file "spa.html" data
             {:tag-open "[%"
              :tag-close "%]"
              :filter-open "[["
              :filter-close "]]"})
```

### Validation and Error Handling

```clojure
(require '[selmer.parser :refer [render-file known-variables]])
(require '[selmer.validator :refer [validate-on!]])

(validate-on!)

(defn safe-render [template-name data]
  (try
    (let [required (known-variables
                     (slurp (io/resource template-name)))]
      (when-not (every? #(contains? data %) required)
        (throw (ex-info "Missing template variables"
                        {:required required :provided (keys data)})))
      (render-file template-name data))
    (catch Exception e
      (log/error e "Template rendering failed")
      "Error rendering template")))
```

## Error Handling

### Common Errors

**Missing Template:**
```clojure
(render-file "nonexistent.html" {})
;=> Exception: resource nonexistent.html not found
```

**Solution:** Verify file exists in classpath or resource path.

**Undefined Filter:**
```clojure
(render "{{x|badfilter}}" {:x 1})
;=> Exception: filter badfilter not found
```

**Solution:** Check filter name or define custom filter.

**Malformed Tag:**
```clojure
(render "{% if %}" {})
;=> Exception: malformed if tag
```

**Solution:** Ensure tag syntax is correct.

### Error Middleware

```clojure
(require '[selmer.middleware :refer [wrap-error-page]])

(def app
  (-> handler
      wrap-error-page
      wrap-other-middleware))
```

Displays detailed error page with:
- Error message
- Line number
- Template excerpt
- Context data

### Validation

```clojure
(require '[selmer.validator :refer [validate-on!]])

(validate-on!)

(render "{% unknown-tag %}" {})
;=> Validation error with details
```

Catches errors at compile time rather than runtime.

## Performance Considerations

### Template Caching

**Enable in production:**
```clojure
(cache-on!)
```

Templates compile once, cache compiled version. Significant performance improvement.

**Disable in development:**
```clojure
(cache-off!)
```

Recompiles on each render. See changes immediately.

### Resource Path Configuration

```clojure
(set-resource-path! "/var/templates/")
```

Reduces classpath scanning overhead.

### Filter Performance

**Expensive operations:**
```clojure
;; Avoid in loops
{% for item in items %}
  {{item.data|json|hash:"sha256"}}
{% endfor %}

;; Better: preprocess in Clojure
(render-file "template.html"
             {:items (map #(assoc % :hash (compute-hash %))
                          items)})
```

### Validation Overhead

```clojure
;; Development
(validate-on!)

;; Production (after testing)
(validate-off!)
```

Validation adds minimal overhead but can be disabled if templates are thoroughly tested.

### Template Inheritance

Shallow inheritance trees perform better than deep nesting.

**Good:**
```
base.html → page.html
```

**Slower:**
```
base.html → layout.html → section.html → page.html
```

## Best Practices

1. **Use template caching in production**
2. **Keep templates in dedicated directory** (`resources/templates/`)
3. **Validate templates in development**
4. **Preprocess complex data in Clojure** rather than in templates
5. **Use includes for reusable components**
6. **Leverage template inheritance** for consistent layouts
7. **Escape user content** (default behavior) unless explicitly safe
8. **Name templates descriptively** (`user-profile.html`, not `page1.html`)
9. **Document custom filters and tags**
10. **Test templates with various data** to catch edge cases

## Platform Notes

**Clojure:** Full support, production-ready.

**ClojureScript:** Not supported. Selmer is JVM-only due to template compilation requiring Java classes.

**Babashka:** Not supported. Selmer requires classes and compilation not available in Babashka.

**Alternatives for ClojureScript:**
- Reagent (Hiccup-style)
- Rum
- UIx

**Alternatives for Babashka:**
- Hiccup
- String templates with `format`

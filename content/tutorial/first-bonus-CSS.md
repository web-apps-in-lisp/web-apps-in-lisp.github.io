+++
title = "Bonus: pimp your CSS"
weight = 1000
+++

## Bonus: pimp your CSS

Don't ask a web developer to help you with the look and feel of the
app, they will bring in hundreds of megabytes of Nodejs dependencies
:S We suggest a (nearly) one-liner to get a decent CSS with no
efforts: by using a class-less CSS, such as
[Pico](https://picocss.com/docs).

We only need to load it in a `<head>` tag of our app.

Optionally, we may write one `<div>` with a `class="container"` attribute, to have better margins.

So, for instance:

```html
(defparameter *template-root* "

<html>
<head>
  <link
  rel=\"stylesheet\"
  href=\"https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css\">
</head>

<body class=\"container\">
 <form action=\"/\" method=\"GET\">
  <div>
    <label for=\"query\">What do you search for?</label>
    <input name=\"query\" id=\"query\" placeholder=\"Search…\" />
  </div>
  <div>
    <button>Search</button>
  </div>
</form>

{% if query %}
<div> query is: {{ query }} </div>

<ul>
  {% for product in results %}
    <li>
      <a href=\"/product/{{ product.0 }}\">{{ product.1 }} - {{ product.2 }}</a>
    </li>
  {% endfor %}
</ul>
{% endif %}
</body>
</html>
")
```

Note how our root template is benefiting from the CSS, and not the
product page. The two pages should inherit from a base template. It's
about time we setup our templates in their own directory.

Refresh [http://localhost:8899/?query=one](http://localhost:8899/?query=one). Do you enjoy the difference?!

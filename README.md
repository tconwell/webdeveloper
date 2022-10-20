
# webdeveloper

<!-- badges: start -->
<!-- badges: end -->

webdeveloper is a high level R web framework that makes it easy to build web 
applications with R, especially when used with the html5 and bsTools packages. 
webdeveloper provides a simple approach for serving both 
static and dynamic content, makes multi-page applications easier to develop, 
and supports async HTTP responses. This is an alternative to Shiny and hopes to 
offer the same level of productivity as Python's Django or Ruby on Rails. Can be 
used to create websites and REST APIs.

This package requires familiarity with HTML and encourages using the html5 and 
bsTools packages for quickly creating your HTML documents. 

Organizational framework for web development in R including functions to  
serve static and dynamic content via HTTP methods, includes the html5 package to
create HTML pages, and offers other utility functions for common tasks related 
to web development.

## Installation

You can install the package webdeveloper from [CRAN](https://cran.r-project.org/) with:

``` r
install.packages("webdeveloper")
```

You can install the development version of webdeveloper from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tconwell/webdeveloper")
```

## Example: Serving a static HTML file

``` r
library(webdeveloper)
# Run the code and go to http://127.0.0.1:5001/ in a web browser
html_file <- doctype(
  html(
    head(),
    body(
      h1("Hello"),
      p("Here is a static HTML file")
    )
  )
)
tmp <- tempfile(fileext = ".html")
writeLines(html_file, con = tmp)

s <- serveHTTP(
  static = list(
    "/" = tmp
  )
)
# stopAllServers()
```

## Example: Serving static and dynamic content. 
Run the code and go to http://127.0.0.1:5001/ in a web browser to see the static page.
Then go to http://127.0.0.1:5001/dynamic-page in a web browser to see the dynamic page.
``` r
library(webdeveloper)
# Run the code and go to http://127.0.0.1:5001/ in a web browser to see the static page
# Then go to http://127.0.0.1:5001/dynamic-page in a web browser to see the dynamic page
html_file <- doctype(
  html(
    head(),
    body(
      h1("Hello"),
      p("Here is a static HTML file")
    )
  )
)
tmp <- tempfile(fileext = ".html")
writeLines(html_file, con = tmp)

# req is the environment containing all the request information such as 
# headers, query string, form inputs, etc. 
get_dynamic_example <- function(
    req, 
    query_string_parameters = parseQueryString(req[["QUERY_STRING"]])
){
  html_doc <- doctype(
    html(
      head(),
      body(
        h1("Hello"),
        p("Here is a dynamic HTML file"),
        h3("Content Generated At"),
        p(Sys.time()),
        h3("Any Query String Parameters?"),
        if(length(query_string_parameters) > 0){
          p(paste0(names(query_string_parameters), " = ", query_string_parameters))
        }else{
          p("No. Try adding '?var1=test&var2=ok' to the end of your URL in the browser")
        }
      )
    )
  )
  return(
    list(
      status = 200L,
      headers = list('Content-Type' = 'text/html'),
      body = html_doc
    )
  )
}

s <- serveHTTP(
  dynamic = alist(
    "/dynamic-page" = c(
      "GET" = get_dynamic_example(req)
    )
  ),
  static = list(
    "/" = tmp
  )
)
# stopAllServers()
```

## Example: Using templates to serve dynamic content
This will most likely improve performance.
Run the code and go to http://127.0.0.1:5001/ in a web browser.
``` r
library(webdeveloper)
# Run the code and go to http://127.0.0.1:5001/ in a web browser
html_file <-  doctype(
  html(
    head(),
    body(
      h1("Hello"),
      p("Here is a dynamic HTML file"),
      h3("Content Generated At"),
      p(templateVar("curr_time")),
      h3("Any Query String Parameters?"),
      p(templateVar("query_string_message"))
    )
  )
)

get_dynamic_example <- function(
    req, 
    query_string_parameters = parseQueryString(req[["QUERY_STRING"]])
){
  curr_time <- as.character(Sys.time())
  if(length(query_string_parameters) > 0){
    query_string_message <- p(paste0(names(query_string_parameters), " = ", query_string_parameters))
  }else{
    query_string_message <- p("No. Try adding '?var1=test&var2=ok' to the end of your URL in the browser")
  }
  replacements <- c(
    "curr_time" = curr_time,
    "query_string_message" = query_string_message
  )
  names(replacements) <- templateVar(names(replacements))
  html_doc <- dynamicTemplate(
    x = html_file,
    replacements = replacements
  )
  return(
    list(
      status = 200L,
      headers = list('Content-Type' = 'text/html'),
      body = html_doc
    )
  )
}

s <- serveHTTP(
  dynamic = alist(
    "/" = c(
      "GET" = get_dynamic_example(req)
    )
  )
)
# stopAllServers()
```

## Example: Using bsTools to create Bootstrap styled pages

``` r
library(webdeveloper)
library(bsTools)
# Run the code and go to http://127.0.0.1:5001/ in a web browser
html_file <- bs_doc(
  body = div(
    bs_navbar(
      brand = list(
        "/" = "webdeveloper"
      ),
      from_left = list(
        "#" = "Link 1",
        "#" = "Link 2",
        "#" = "Link 3"
      )
    ),
    bs_container(
      bs_row(
        bs_col(
          col_attr = c(class = "col-3"),
          div()
        ),
        bs_col(
          col_attr = c(class = "col-6"),
          h1("Bootstrap Styled Pages"),
          p("You can use the bsTools package to easily create pages with Bootstrap classes."),
          bs_accordion(
            id = "acc1",
            items = list(
              "Panel 1" = div(
                p("Panel 1 content")
              ),
              "Panel 2" = div(
                p("Panel 2 content")
              ),
              "Panel 3" = div(
                p("Panel 3 content")
              )
            )
          )
        ),
        bs_col(
          col_attr = c(class = "col-3"),
          div()
        )
      )
    )
  )
)
tmp <- tempfile(fileext = ".html")
writeLines(html_file, con = tmp)

s <- serveHTTP(
  static = list(
    "/" = tmp
  )
)
# stopAllServers()
```

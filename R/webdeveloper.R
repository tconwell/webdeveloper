#' Parse the content type header string to return the content type and boundary
#'
#' @param x A string containing the content type header.
#' @return A named list with "content_type" and "boundary" if boundary is present.
#' @examples
#' parseContentTypeHeader("application/x-www-form-urlencoded")
parseContentTypeHeader <- function(x){
  x <- strsplit(x, "; ", fixed = TRUE)[[1]]
  return(
    if(length(x) == 1){
      list(
        "content_type" = x
      )
    }else{
      list(
        "content_type" = x[1],
        "boundary" = strsplit(x[2], "=", fixed = TRUE)[[1]][2]
      )
    }
  )
}

#' Parse a query string
#'
#' @param x A string containing the query string.
#' @param split A string, the character to split by.
#' @param consolidate TRUE/FALSE, if TRUE, consolidates items with the same name.
#' @return A named list.
#' @examples
#' parseQueryString("?form_id=example&col_name=Test+String")
parseQueryString <- function(
  x,
  split = "&",
  consolidate = TRUE
){
  x <- base::sub(pattern = "^[?]", replacement = "", x)
  x <- chartr("+", " ", x)
  x <- strsplit(x, split, fixed = TRUE)[[1]]
  args <- lapply(x, function(y) {
    httpuv::decodeURIComponent(strsplit(y, "=", fixed = TRUE)[[1]])
  })
  values <- lapply(args, `[`, 2)
  names(values) <- vapply(args, `[`, character(1), 1)
  if(consolidate){
    value_names <- names(values)
    uvalue_names <- unique(value_names)
    if(length(uvalue_names) < length(value_names)){
      values <- lapply(uvalue_names, function(y){
        return(unlist(values[names(values) %in% y]))
      })
      names(values) <- uvalue_names
    }
  }
  return(
    values
  )
}

#' Helper function for parseMultiPartFormData
#'
#' @param x A vector, a chunk of multi-part form data to parse.
#' @return A named list.
#' @examples
#' parseMultiPartFormParams(c("Content-Disposition: form-data; name=\"form_name\"", "", "Example"))
parseMultiPartFormParams <- function(
  x
){
  meta_chunk <- strsplit(x[1], "; ", fixed = TRUE)[[1]]
  p_list <- list()
  if(meta_chunk[1] == "Content-Disposition: form-data"){
    p_name <- strsplit(meta_chunk[2], "=")[[1]][2]
    p_name <- gsub('\"', "", p_name, fixed = TRUE)
    if(grepl("Content-Type", x[2], fixed = TRUE)){
      filename <- unlist(strsplit(meta_chunk[3], "="))[2]
      filename <- gsub('\"', "", filename)
      x_data <- x[4:length(x)]
      p_list <- list(
        list(
          "filename" = filename,
          "data" = x_data
        )
      )
      names(p_list) <- p_name
    }else{
      x_data <- x[3:length(x)]
      p_list <- list(
        x_data
      )
      names(p_list) <- p_name
    }
  }
  return(p_list)
}

#' Parse multi-part form data
#'
#' @param x A vector.
#' @param boundary A string, the boundary used for the multi-part form data
#' @return A named list.
#' @examples
#' parseMultiPartFormData(
#' x = c(
#'   "------WebKitFormBoundaryfBloeH49iOmYtO5A",
#'   "Content-Disposition: form-data; name=\"form_name\"",
#'   "",
#'   "Example",
#'   "------WebKitFormBoundaryfBloeH49iOmYtO5A",
#'   "Content-Disposition: form-data; name=\"form_id\"",
#'   "",
#'   "test",
#'   "------WebKitFormBoundaryfBloeH49iOmYtO5A",
#'   "Content-Disposition: form-data; name=\"desktop_file\"; filename=\"limit_type.csv\"",
#'   "Content-Type: text/csv",
#'   "",
#'   "limit_type",
#'   "Aggregate",
#'   "Occurrence",
#'   "------WebKitFormBoundaryfBloeH49iOmYtO5A--"
#' ),
#' boundary = parseContentTypeHeader(
#' "multipart/form-data; boundary=----WebKitFormBoundaryfBloeH49iOmYtO5A")[['boundary']]
#' )
parseMultiPartFormData <- function(
  x,
  boundary
){
  boundary <- gsub("-", "", boundary)
  boundaries <- which(grepl(boundary, x, fixed = TRUE))
  params <- list()
  for(i in 1:length(boundaries)){
    if(boundaries[i] == length(x)){

    }else{
      start <- boundaries[i] + 1
      end <- if(i == length(boundaries)){length(x)}else{boundaries[i+1] - 1}
      chunk <- x[start:end]
      chunk_params <- parseMultiPartFormParams(chunk)
      params <- c(params, chunk_params)
    }
  }
  return(params)
}

#' Parse a HTTP request
#'
#' @param x The body of the HTTP request
#' @param content_type_header A string containing the content type header.
#' @param consolidate TRUE/FALSE, if TRUE, consolidates items with the same name.
#' @return A named list.
#' @examples
#' parseHTTP("?form_id=example&col_name=Test+String", "application/x-www-form-urlencoded")
parseHTTP <- function(
  x,
  content_type_header = NULL,
  consolidate = TRUE
){
  return(
    if(is.null(content_type_header)){
      parseQueryString(x, consolidate)
    }else{
      c_type <- parseContentTypeHeader(content_type_header)
      if(c_type[["content_type"]] == "application/x-www-form-urlencoded"){
        parseQueryString(x, consolidate = consolidate)
      }else if(c_type[["content_type"]] == "multipart/form-data"){
        parseMultiPartFormData(x, boundary = c_type[["boundary"]])
      }
    }
  )
}

#' Add a prefix to an id
#'
#' @param id A string to add a prefix to.
#' @param prefix A string, the prefix to add.
#' @param sep A string, the separator to use.
#' @return A string.
#' @examples
#' idAddSuffix("example", 1)
idAddPrefix <- function(prefix, id, sep = "X"){
  return(
    paste0(prefix, sep, id)
  )
}

#' Remove a prefix from an id
#'
#' @param id A string to remove a prefix from.
#' @param split A string, the separator to use for splitting the id.
#' @param position A integer vector, the position of the split string to return.
#' @return A vector.
#' @examples
#' idParsePrefix(idAddPrefix("example", 1))
idParsePrefix <- function(id, split = "X", position = 2){
  return(
    strsplit(id, split = split, fixed = TRUE)[[1]][position]
  )
}

#' Add a suffix to an id
#'
#' @param id A string to add a suffix to.
#' @param suffix A string, the suffix to add.
#' @param sep A string, the separator to use.
#' @return A string.
#' @examples
#' idAddSuffix("example", 1)
idAddSuffix <- function(id, suffix, sep = "-"){
  return(
    paste0(id, sep, suffix)
  )
}

#' Remove a suffix from an id
#'
#' @param id A string to remove a suffix from.
#' @param split A string, the separator to use for splitting the id.
#' @param position A integer vector, the position of the split string to return.
#' @return A vector.
#' @examples
#' idParseSuffix(idAddSuffix("example", 1))
idParseSuffix <- function(id, split = "-", position = 1){
  return(
    strsplit(id, split = split, fixed = TRUE)[[1]][position]
  )
}

#' Add a prefix and suffix to an id
#'
#' @param prefix A string, the prefix to add.
#' @param id A string to add a prefix and suffix to.
#' @param suffix A string, the suffix to add.
#' @param prefix_sep A string, the prefix separator to use.
#' This should be different than suffix_sep.
#' @param suffix_sep A string, the suffix separator to use.
#' This should be different than prefix_sep.
#' @return A string.
#' @examples
#' idAddAffixes("group1", "example", 1)
idAddAffixes <- function(prefix, id, suffix, prefix_sep = "X", suffix_sep = "-"){
  return(
    paste0(prefix, prefix_sep, id, suffix_sep, suffix)
  )
}

#' Remove a prefix and suffix from an id
#'
#' @param id A string to remove a prefix and suffix from.
#' @param split A regular expression to use for splitting the prefix and suffix from the id.
#' @return A named vector, with prefix, id, and suffix returned in that order.
#' @examples
#' idParseAffixes(idAddAffixes("group1", "example", 1))
idParseAffixes <- function(id, split = "X|-"){
  x <- strsplit(id, split = split)[[1]]
  names(x) <- c("prefix", "id", "suffix")
  return(
    x
  )
}

################################################################################

#' Creates HTML option tags for each position of a list of values and labels by calling HTML5::option(),
#' returning a string of HTML to pass to a select tag through HTML5::select().
#'
#' @param x A vector which will become the value/label for each option. If named, names become values.
#' @param selected A value in the vector passed to mark as the initially selected option in the select tag.
#' @param add_blank Boolean, If TRUE, adds a blank option to the top of x.
#' @return A string, with an option tag each row of x.
#' @examples
#' create_options(
#' x = c("New York", "Los Angeles", "Chicago"),
#' selected = "Chicago"
#' )
create_options <- function(x, selected = c(), add_blank = FALSE){
  name_vals <- names(x)
  if(add_blank == TRUE){
    x <- c("", x)
    if(length(name_vals) > 0){
      name_vals <- c("", name_vals)
    }
  }
  x <- unname(x)
  selected <- x %in% selected
  if(length(x) > 1){
    return(
      if(length(name_vals) > 0){
        option(
          attr = list(
            "value" = name_vals,
            "label" = x,
            "selected" = selected
          ),
          separate = TRUE
        )
      }else{
        option(
          attr = list(
            "value" = x,
            "label" = x,
            "selected" = selected
          ),
          separate = TRUE
        )
      }
    )
  }else if(length(x) == 1){
    return(
      if(length(name_vals) > 0){
        option(
          attr = list(
            "value" = name_vals,
            "label" = x,
            "selected" = selected
          )
        )
      }else{
        option(
          attr = list(
            "value" = x,
            "label" = x,
            "selected" = selected
          )
        )
      }
    )
  }else{
    return(
      option(value = "", label = "")
    )
  }
}

#' Conveniently create HTTP server using httpuv::startServer() or httpuv::runServer().
#'
#' @param host A string that is a valid IPv4 or IPv6 address that is owned by this server, which the application will listen on.
#' "0.0.0.0" represents all IPv4 addresses and "::/0" represents all IPv6 addresses. Refer to host parameter of httpuv::startServer() for more details.
#' @param port The port number to listen on. Refer to port parameter of httpuv::startServer() for more details.
#' @param persistent TRUE/FALSE. If FALSE, calls httpuv::startServer(), which returns back to the R session
#' (and would therefore not work with launching a persistent server through a system service as the R session would continue and likely exit/end).
#' If TRUE, calls httpuv::runServer(), which does not return to the R session unless an error or
#' interruption occurs and is suitable for use with system services to start or stop a server.
#' @param async TRUE/FALSE, if TRUE, dynamic path requests will be served asynchronously using multicore evaluation, if possible. This is an
#' advanced option and might make it more confusing to debug your app.
#' @param static A named list, names should be URL paths, values should be paths to the files to be served statically (such as a HTML file saved somewhere)
#' or staticPath objects if lapply_staticPath is FALSE.
#' @param dynamic A named list, names should be URL paths, values should be named alists (use alist instead of list) with alist names equaling a
#' HTTP method (such as "GET" or "POST") and the values being expressions that when evaluated return a named list with valid entries
#' for status, headers, and body as specified by httpuv::startServer(). Refer to httpuv::startServer() for more details on what can be returned
#' as the response.
#' ex. list("/" = alist("GET" = get_function(req), "POST" = post_function(req)))
#' @param lapply_staticPath TRUE/FALSE, if TRUE, httpuv::staticPath will be applied to each element of static to create staticPath objects.
#' @param static_path_options A named list, passed to httpuv::staticPathOptions.
#' @return A HTTP web server on the specified host and port.
#' @details serveHTTP is a convenient way to start a HTTP server that works for both static and dynamically created pages.
#' It offers a simplified and organized interface to httpuv::startServer()/httpuv::runServer() that makes serving static and
#' dynamic pages easier. For dynamic pages, the expression evaluated when a browser requests a dynamically served path should
#' likely be an expression/function that has "req" as a parameter. Per the Rook specification implemented by httpuv, "req" is
#' the R environment in which browser request information is collected. Therefore, to access HTTP request headers, inputs, etc. in a function
#' served by a dynamic path, "req" should be a parameter of that function. For the dynamic parameter of serveHTTP,
#' list("/" = alist("GET" = get_homepage(req))) would be a suitable way to call the function get_homepage(req) when the root path of a
#' website is requested with the GET method. The req environment has the following variables:
#' request_method = req$REQUEST_METHOD,
#' script_name = req$SCRIPT_NAME,
#' path_info = req$PATH_INFO,
#' query_string = req$QUERY_STRING,
#' server_name = req$SERVER_NAME,
#' server_port = req$SERVER_PORT,
#' headers = req$HEADERS,
#' rook_input = req[["rook.input"]]$read_lines(),
#' rook_version = req[["rook.version"]]$read_lines(),
#' rook_url_scheme = req[["rook.url_scheme"]]$read_lines(),
#' rook_error_stream = req[["rook.errors"]]$read_lines()
#'
#' @examples
#' # Run both functions and go to http://127.0.0.1:5001/ in a web browser
#' get_example <- function(req){
#'
#' html <- doctype(
#' html(
#' head(),
#' body(
#' h1("Hello"),
#' p("Here is a list of some of the variables included in the req environment
#' that were associated with this request:"),
#' ul(
#' li(paste0("req$REQUEST_METHOD = ", req$REQUEST_METHOD)),
#' li(paste0("req$SCRIPT_NAME = ", req$SCRIPT_NAME)),
#' li(paste0("req$PATH_INFO = ", req$PATH_INFO)),
#' li(paste0("req$QUERY_STRING = ", req$QUERY_STRING)),
#' li(paste0("req$SERVER_NAME = ", req$SERVER_NAME)),
#' li(paste0("req$SERVER_PORT = ", req$SERVER_PORT))
#' ),
#' p("You can use parseQueryString to deal with inputs passed through query strings as
#' well as passed through the input stream."),
#' p("params <- parseQueryString(req[[\"rook.input\"]]$read_lines()) will give you a
#' named list of parameters. See also parseHTTP.")
#' )
#' )
#' )
#' return(
#' list(
#' status = 200L,
#' headers = list('Content-Type' = 'text/html'),
#' body = html
#' )
#' )
#' }
#'
#' serveHTTP(
#' host = "127.0.0.1",
#' port = 5001,
#' persistent = FALSE,
#' static = list(),
#' dynamic = list(
#' "/" = alist(
#' "GET" = get_example(req)
#' )
#' )
#' )
serveHTTP <- function(
  host = "127.0.0.1",
  port = 5001,
  persistent = FALSE,
  async = FALSE,
  static = list(),
  dynamic = list(),
  lapply_staticPath = TRUE,
  static_path_options = list(
    indexhtml = TRUE,
    fallthrough = FALSE,
    html_charset = "utf-8",
    headers = list(),
    validation = character(0),
    exclude = FALSE
  )
){
  if(length(static) > 0 & lapply_staticPath){
    static <- lapply(static, staticPath)
  }
  if(length(dynamic) > 0){
    for(i in names(dynamic)){
      static[[i]] <- excludeStaticPath()
    }
  }
  valid_dynamic_paths <- names(dynamic)
  if(persistent == TRUE){
    if(async){
      future::plan(future::multicore)
      return(
        runServer(
          host,
          port,
          app = list(
            call = function(req) {
              if(req$PATH_INFO %in% valid_dynamic_paths){
                promises::as.promise(
                  future::future({
                    eval(dynamic[[req$PATH_INFO]][[req$REQUEST_METHOD]])
                  })
                )
              }else{
                list(
                  status = 404,
                  headers = list(
                    'Content-Type' = 'text/html'
                  ),
                  body = "404. Page not found."
                )
              }
            },
            staticPaths = static,
            staticPathOptions = do.call(staticPathOptions, static_path_options)
          )
        )
      )
    }else{
      return(
        runServer(
          host,
          port,
          app = list(
            call = function(req) {
              if(req$PATH_INFO %in% valid_dynamic_paths){
                eval(dynamic[[req$PATH_INFO]][[req$REQUEST_METHOD]])
              }else{
                list(
                  status = 404,
                  headers = list(
                    'Content-Type' = 'text/html'
                  ),
                  body = "404. Page not found."
                )
              }
            },
            staticPaths = static,
            staticPathOptions = do.call(staticPathOptions, static_path_options)
          )
        )
      )
    }
  }else{
    if(async){
      future::plan(future::multicore)
      return(
        startServer(
          host,
          port,
          app = list(
            call = function(req) {
              if(req$PATH_INFO %in% valid_dynamic_paths){
                promises::as.promise(
                  future::future({
                    eval(dynamic[[req$PATH_INFO]][[req$REQUEST_METHOD]])
                  })
                )
              }else{
                list(
                  status = 404,
                  headers = list(
                    'Content-Type' = 'text/html'
                  ),
                  body = "404. Page not found."
                )
              }
            },
            staticPaths = static,
            staticPathOptions = do.call(staticPathOptions, static_path_options)
          )
        )
      )
    }else{
      return(
        startServer(
          host,
          port,
          app = list(
            call = function(req) {
              if(req$PATH_INFO %in% valid_dynamic_paths){
                eval(dynamic[[req$PATH_INFO]][[req$REQUEST_METHOD]])
              }else{
                list(
                  status = 404,
                  headers = list(
                    'Content-Type' = 'text/html'
                  ),
                  body = "404. Page not found."
                )
              }
            },
            staticPaths = static,
            staticPathOptions = do.call(staticPathOptions, static_path_options)
          )
        )
      )
    }
  }
}

#' Stop HTTP server(s) by calling httpuv::stopServer() or httpuv::stopAllServers().
#'
#' @param x A server object that was previously returned from serveHTTP.
#' @param all TRUE/FALSE, if TRUE, calls httpuv::stopAllServers.
#' @return Nothing.
#' @examples
#' endServer(all = TRUE)
endServer <- function(x = NULL, all = FALSE){
  if(all == TRUE){
    return(stopAllServers())
  }else{
    return(stopServer(x))
  }
}

#' Create a string to use as a placeholder variable in a HTML document.
#'
#' @param x Name of placeholder.
#' @return A string.
#' @examples
#' templateVar("my_dynamic_var")
templateVar <- function(x){
  return(paste0("%%rvar-", x, "%%"))
}

#' Find the names of any placeholder variables that exist in a HTML document string.
#'
#' @param x HTML string to check for placeholder.
#' @return A vector of the names of template vars found in the HTML string.
#' @examples
#' findTemplateVars(x = html(body(templateVar("body_var"))))
findTemplateVars <- function(x){
  x <- strsplit(x, "%%")[[1]]
  vars <- x[grepl("rvar-", x, fixed = TRUE)]
  vars <- unlist(lapply(vars, function(x){
    return(
      strsplit(x, "rvar-", fixed = TRUE)[[1]][2]
    )
  }), use.names = FALSE)
  return(vars)
}

#' Replace placeholder variables in a HTML document string.
#'
#' @param x HTML string with placeholder variables that need to be replaced.
#' @param replacements A named vector or named list. Names should match a template variable acting as a placeholder in a HTML document string
#' and values should be the text to replace the placeholders with.
#' @return A string of HTML with placeholder values replaced.
#' @examples
#' dynamicTemplate(
#' x = html(body(templateVar("body_var"))),
#' replacements = c("%%rvar-body_var%%" = div(p("body replacement")))
#' )
dynamicTemplate <- function(x, replacements = c()){
  return(
    if(length(replacements) > 0){
      stringi::stri_replace_all_fixed(x, pattern = names(replacements), replacement = replacements, vectorize_all = FALSE)
    }else{
      x
    }
  )
}

#' Replace placeholder variables in a HTML document string, after reading the file into R.
#'
#' @param file Filepath of the HTML file with placeholder variables that need to be replaced.
#' @param replacements A named vector or named list. Names should match a template variable acting as a placeholder in a HTML document string
#' and values should be the text to replace the placeholders with.
#' @return A string of HTML with placeholder values replaced.
#' @examples
#' tmp <- tempfile()
#' writeLines(html(body(templateVar("body_var"))), con = tmp)
#' dynamicTemplate2(file = tmp, replacements = c("%%rvar-body_var%%" = div(p("body replacement"))))
dynamicTemplate2 <- function(file, replacements = c()){
  x <- readr::read_file(file)
  return(
    if(length(replacements) > 0){
      stringi::stri_replace_all_fixed(x, pattern = names(replacements), replacement = replacements, vectorize_all = FALSE)
    }else{
      x
    }
  )
}


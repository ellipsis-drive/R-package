#library(httr)

baseUrl <- 'https://api.ellipsis-drive.com/v3'

all_names <- function(list) {
  return(length(list) == sum(names(list) != "", na.rm=TRUE))
}


filterNULL <- function(body)
{
  if (is.null(body))
    return(body)

  params <- list()
  for (name in names(body))
  {
    if (!is.null(body[[name]]))
      params[[name]] <- body[[name]]
  }
  return(params)
}

apiManager_post <- function(url, body, token=NULL)
{
  r <- apiManager_call(method="POST", url = url, body = body, token = token)
  return(r)
}

apiManager_get <- function(url, body = NULL, token = NULL, crash = TRUE)
{
  if (is.null(body))
    body <- list("token" = token)
  else
    body[["token"]] <- token

  body <- filterNULL(body)
  for (key in names(body))
    if (!isSingleString(body))
      body[[key]] <- jsonDumps(body[[key]])

  body <- URLencode(body)

  url <- uPaste(uPaste(url, '?'), body)

  r <- apiManager_call(method = "GET", url = url, body = NULL, token = token, crash = crash)
}

apiManager_call <- function(method, url, body = NULL, token = NULL, crash = TRUE)
{
  # Forward declaration
  res <- NULL
  body <- filterNULL(body)
  if (!is.null(body) & (!is.list(body) | (!all_names(body) & sum(names(list) != "", na.rm=TRUE) == 0)))
    stop("ValueError: body of an API call must be of type dict or NULL")

  if (!is.null(token) & !isSingleString(token))
    stop("ValueError: Token must be of type string or NULL")

  if (is.null(token))
  {
    if (method == "POST")
    {
      res <- httr::POST(url = uPaste(baseUrl, url), body = body, encode = "form")
      r <- httr::content(res)
    }
    else if (method == "GET")
    {
      res <- httr::GET(url = uPaste(baseUrl, url), body)
      r <- httr::content(res)
    }
  }
  else
  {
    if (!grepl("Bearer", token, fixed = TRUE))
      token <- paste("bearer", token)
    if (method == "POST")
    {
      res <- httr::POST(url = uPaste(baseUrl, url), body = body, httr::add_headers(Authorization = token), encode = "form")
      r <- httr::content(res)
    }
    else if (method == "GET")
    {
      res <- httr::GET(url = uPaste(baseUrl, url), body, httr::add_headers(Authorization = token))
      r <- httr::content(res)
    }
  }

  if (crash == TRUE)
  {
    errorMessage <- httr::content(res, as="text")
    if (httr::status_code(res) != 200)
      stop(glue::glue("ValueError: {errorMessage}"))
    r <- tryCatch(
      {
        # If something fails, replace text with parsed
        r <- httr::content(res, as="parsed")
      },
      error=function(cond)
      {
        r <- httr::content(res, as="text")
      },
      warning=function(cond)
      {
        r <- httr::content(res, as="text")
      },
      finally=
      {

      }
    )
    return(r)
  }
  return(r)
}

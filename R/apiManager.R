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
  r <- apiManager_call(method = httr::POST, url = url, body = body, token = token)
  return(r)
}

apiManager_put <- function(url, body, token = NULL)
{
  r <- apiManager_call(method = httr::PUT, url = url, body = body, token = token)
  return(r)
}

apiManager_patch <- function(url, body, token = NULL)
{
  r <- apiManager_call(method = httr::PATCH, url = url, body = body, token = token)
  return(r)
}

apiManager_delete <- function(url, body, token = NULL)
{
  r <- apiManager_call(method = httr::DELETE, url = url, body = body, token = token)
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
  {
    if (!isSingleString(body[[key]]))
    {
      body[[key]] <- jsonDumps(body[[key]])
    }
  }
  body_string <- ""
  # There is no good way to encode urls like urllib.parse.urlencode in R
  for (key in names(body))
  {
    val <- body[[key]]
    body_string <- uPaste(body_string, glue::glue("{key}={val}&"))
  }
  # Remove dangling '&'
  body_string <- substring(body_string, 1, nchar(body_string)-1)
  url <- uPaste(uPaste(url, '?'), body_string)
  r <- apiManager_call(method = httr::GET, url = url, body = NULL, token = token, crash = crash)
  return(r)
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
    res <- tryCatch(
      {
        res <- method(url = uPaste(baseUrl, url), body = jsonlite::toJSON(body, auto_unbox=TRUE), httr::add_headers("Content-Type" = "application/json"), encode = "json")
      },
      error = function(cond)
      {
        res <- method(url = uPaste(baseUrl, url), body = body, encode = "form")
      }
    )
  }
  else
  {
    if (!grepl("Bearer", token, fixed = TRUE))
      token <- paste("bearer", token)
    res <- tryCatch(
      {
        res <- method(url = uPaste(baseUrl, url), body = jsonlite::toJSON(body, auto_unbox=TRUE), httr::add_headers("Content-Type" = "application/json", "Authorization" = token), encode = "json")
      },
      error = function(cond)
      {
        res <- method(url = uPaste(baseUrl, url), body = body, httr::add_headers("Authorization" = token), encode = "form")
      }
    )
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
    return(res)
  }
  return(res)
}

apiManager_upload <- function(url, filePath, body, token, key = "data")
{
  body <- filterNULL(body)

  fileName <- basename(filePath)

  for (k in names(body))
  {
    body[[k]] <- toString(body[[k]])
  }
  body[["name"]] <- fileName
  body[["filedata"]] <- httr::upload_file(filePath)
  print(body[["filedata"]])

  token <- paste("bearer", token)
  print(uPaste(baseUrl, url))
  stop("123")
  res <- httr::POST(uPaste(baseUrl, url), body = body, httr::add_headers("Content-Type" = "multipart/form-data", Authorization = token))
  errorMessage <- httr::content(res, as="text")
  if (httr::status_code(res) != 200)
    stop(glue::glue("ValueError: {errorMessage}"))

  return(httr::content(res))
}

apiManager_download <- function(url, filePath, token)
{
  token <- uPaste("Bearer ", token)

  res <- tryCatch(
    {
      res <- httr::GET(uPaste(baseUrl, url), httr::add_headers(Authorization = token))
      tryCatch(
        {
          bin <- httr::content(res, "raw")
          writeBin(bin, filePath)
        },
        error=function(cond)
        {
          print("Something went wrong...")
        },
        finally=function(cond)
        {

        }
      )
    },
    error=function(cond)
    {
      r <- httr::content(res, as="text")
    }
  )
}

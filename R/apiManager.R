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
    if (!isSingleString(body))
      body[[key]] <- purrr::quietly(jsonDumps)(body[[key]])$result

  body <- URLencode(body)

  url <- uPaste(uPaste(url, '?'), body)
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
    r <- httr::content(res)
  }
  else
  {
    if (!grepl("Bearer", token, fixed = TRUE))
      token <- paste("bearer", token)
    res <- tryCatch(
      {
        res <- method(url = uPaste(baseUrl, url), body = jsonlite::toJSON(body, auto_unbox=TRUE), httr::add_headers("Content-Type" = "application/json", Authorization = token), encode = "json")
      },
      error = function(cond)
      {
        res <- method(url = uPaste(baseUrl, url), body = body, httr::add_headers(Authorization = token), encode = "form")
      }
    )
    r <- httr::content(res)
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
  print(httr::upload_file(filePath))
  token <- paste("bearer", token)
  u2 = evaluate::evaluate({
    function()
      {
        res <- httr::POST(uPaste(baseUrl, url), body = body, httr::add_headers("Content-Type" = "multipart/form-data", Authorization = token), httr::verbose(info = TRUE))
      }
    })
  print(u2)
  errorMessage <- httr::content(res, as="text")
  fileConn<-file("output.txt")
  print(res)
  lapply(u[[2]], write, "test.txt", append=TRUE, ncolumns=1000)
  close(fileConn)
  #print(httr::content(res))
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

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

apiManager_call <- function(method, url, body = NULL, token = NULL, crash = TRUE)
{
  body <- filterNULL(body)
  if (!is.null(body) & (!is.list(body) | (!all_names(body) & sum(names(list) != "", na.rm=TRUE) == 0)))
    stop("ValueError: body of an API call must be of type dict or NULL")

  if (!is.null(token) & !isSingleString(token))
    stop("ValueError: Token must be of type string or NULL")

  if (is.null(token))
  {
    if (method == "POST")
    {
      print(gsub(" ", "", paste(baseUrl, url)))
      res <- httr::POST(url = gsub(" ", "", paste(baseUrl, url)), body = body, encode = "form")
      r <- httr::content(res)
    }
  }
}

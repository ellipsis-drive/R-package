#source("R/sanitize.R")
#source("R/apiManager.R")

#' Login a user
#'
#' This function logs in a user, and returns a token the user can then use to
#' call restricted methods. It takes the strings username and password as input
#' and returns a string token as output. The user can provide an optional argument
#' validFor to indicate the time validity of the token.
#'
#' @param username a string representing the username
#' @param password a string representing the password
#' @param validFor an integer indicating how long the token is valid
#' @return A string representing the login token
#' @export
logIn <- function(username, password, validFor = NULL)
{
  username <- validString("username", username, TRUE)
  password <- validString("password", password, TRUE)
  validFor <- validInt("validFor", validFor, FALSE)
  json <- list("username" = username, "password" = password, "validFor" = validFor)
  r <- apiManager_post("/account/login", json)
  token <- r$"token"
  return(token)
}

listRoot <- function(rootName, token, pathTypes = NULL, pageStart = NULL, listAll = TRUE)
{
  token = validString("token", token, TRUE)
  rootName = validString("rootName", rootName, TRUE)
}

#source("R/sanitize.R")
#source("R/apiManager.R")

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

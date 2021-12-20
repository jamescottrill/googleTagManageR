#'Auth for GTM Specific Properties
#'
#' @importFrom googleAuthR gar_auth
#' @family login functions
#' 
#' @param token An existing Google Auth Token
#' @param email The email address for the Google Account
#' 
#' @description 
#' This function authenticates the user with Google Tag Manager
#' To use your own GTM API credentials, either ensure that you have set the
#' path to your OAuth2 API Credentials file in your 
#' 'GAR_CLIENT_JSON' variable in your .Renviron file and then restart your 
#' R session or set 
#'  
#'
#' @examples 
#'  
#'  \dontrun{
#'  # To use the included (shared) credentials
#'  library(googleTagManageR)
#'  gtm_auth()
#'  
#'  # To useyour own GCP credentials
#'  Sys.setenv("GAR_CLIENT_JSON" = "location/of/file.json")
#'  library(googleTagManageR)
#'  gtm_auth()
#'  
#'  # Reauthentication - if you've already logged in
#'  gtm_auth(email="me@mycompany.co.uk")
#'  }
#'  
#' @return Invisibly, the token that has been saved to the session
#' @export
gtm_auth<-function(email = NULL, token = NULL, sa_json=NULL){

  default_project_message()
  if(!is.null(sa_json)){
    email <- jsonlite::fromJSON(sa_json)$client_email
    myMessage(sprintf("Authenticating Using Service Account %s", email), level=3)
    out <- googleAuthR::gar_auth_service(sa_json)
  } else {  
    out<-googleAuthR::gar_auth(email = email,
                             token = token,
                             package = "googleTagManageR")
    myMessage("Authenticated", level = 3)
}
  invisible(out)
}

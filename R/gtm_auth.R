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
#'  gtm_auth()
#'  
#'  gtm_auth(email="me@mycompany.co.uk")
#'  }
#'  
#' @return Invisibly, the token that has been saved to the session
#' @export
gtm_auth<-function(email = NULL, token = NULL){

  default_project_message()
  out<-googleAuthR::gar_auth(email = email,
                             token = token,
                             package = "googleTagManageR")
  myMessage("Authenticated", level = 3)
  invisible(out)
}

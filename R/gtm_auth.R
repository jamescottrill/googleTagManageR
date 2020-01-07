#'Auth for GTM Specific Properties
#'
#' @importFrom googleAuthR gar_auth
#' @family login functions
#' @export

gtm_auth<-function(token = NULL, new_user = FALSE, no_auto = FALSE){
  
  if(!is.null(token)){
    return(googleAuthR::gar_auth(token = token))
  }
  
  needed <- c("https://www.googleapis.com/auth/tagmanager.readonly",
              "https://www.googleapis.com/auth/tagmanager.edit.containers",
              "https://www.googleapis.com/auth/tagmanager.delete.containers",
              "https://www.googleapis.com/auth/tagmanager.edit.containerversions",
              "https://www.googleapis.com/auth/tagmanager.publish",
              "https://www.googleapis.com/auth/tagmanager.manage.users",
              "https://www.googleapis.com/auth/tagmanager.manage.accounts")
  options(googleAuthR.scopes.selected = needed)
  options(googleAuthR.client_id = "301891352590-qnhjsr3nblisijpb2jssbnclujqepuci.apps.googleusercontent.com")
  options(googleAuthR.client_secret = "tUIVWipYKkSljLhbbPA-ot4r")
  # default_project_message()
  out<-googleAuthR::gar_auth(needed)
  myMessage("Authenticated", level = 3)
  invisible(out)
}

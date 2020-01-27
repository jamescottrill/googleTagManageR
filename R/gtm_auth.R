#'Auth for GTM Specific Properties
#'
#' @importFrom googleAuthR gar_auth
#' @family login functions
#' @export

gtm_auth<-function(token = NULL){
  
  if(!is.null(token)){
    return(googleAuthR::gar_auth(token = token))
  }
  
  default_project_message()
  out<-googleAuthR::gar_auth(token = token,
                             package = "googleTagManageR")
  myMessage("Authenticated", level = 3)
  invisible(out)
}

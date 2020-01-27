.onLoad <- function(libname, pkgname) {
  
  op <- options()
  op.googleTagManageR <- list(
    ## default Google project
    googleAuthR.client_id = "301891352590-39fbtji45hu1vapamuu4cc8jgi9vapr4.apps.googleusercontent.com",
    googleAuthR.client_secret = "VOHy0JTxmfumVUJ7w3k9-jIr",
    googleAuthR.webapp.client_id = "301891352590-qnhjsr3nblisijpb2jssbnclujqepuci.apps.googleusercontent.com",
    googleAuthR.webapp.client_secret = "tUIVWipYKkSljLhbbPA-ot4r",
    googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/tagmanager.readonly",
                                    "https://www.googleapis.com/auth/tagmanager.edit.containers",
                                    "https://www.googleapis.com/auth/tagmanager.delete.containers",
                                    "https://www.googleapis.com/auth/tagmanager.edit.containerversions",
                                    "https://www.googleapis.com/auth/tagmanager.publish",
                                    "https://www.googleapis.com/auth/tagmanager.manage.users",
                                    "https://www.googleapis.com/auth/tagmanager.manage.accounts"),
    op.googleTagManageR = "gtm.oauth",
    googleAuthR.quotaUser = Sys.info()[["user"]]
  )
  
  toset <- !(names(op.googleTagManageR) %in% names(op))
  ## only set those not set already
  if(any(toset)) options(op.googleTagManageR[toset])
  
  invisible()
  
}

.onAttach <- function(libname, pkgname){
  
  if(Sys.getenv("GAR_CLIENT_JSON") != ""){
    googleAuthR::gar_set_client(json = Sys.getenv("GAR_CLIENT_JSON"))
  }
  
  needed <- c("https://www.googleapis.com/auth/tagmanager.readonly",
              "https://www.googleapis.com/auth/tagmanager.edit.containers",
              "https://www.googleapis.com/auth/tagmanager.delete.containers",
              "https://www.googleapis.com/auth/tagmanager.edit.containerversions",
              "https://www.googleapis.com/auth/tagmanager.publish",
              "https://www.googleapis.com/auth/tagmanager.manage.users",
              "https://www.googleapis.com/auth/tagmanager.manage.accounts")
  
  googleAuthR::gar_attach_auto_auth(needed, 
                                    environment_var = "GARGLE_EMAIL")
  # for json files
  googleAuthR::gar_attach_auto_auth(needed, 
                                    environment_var = "GA_AUTH_FILE")
  
  invisible()
  
}
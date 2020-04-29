cancelled <- function() {
  stop("Deletion cancelled")
}
#'Main GTM API function
#'
#' @importFrom googleAuthR gar_api_generator
#' @family api functions
#' @description 
#' This creates the API request to be called.
#' @noRd
.gtm_api<-function(type=c("POST","PUT","GET","DELETE"), path_args, pars_args = NULL) {
  type <- match.arg(type)
  url <- 'https://www.googleapis.com/tagmanager/v2/'
  api <- googleAuthR::gar_api_generator(url,
                         type, 
                         path_args = path_args, 
                         pars_args = pars_args,
                         data_parse_function = function(x) x
                         )
  return(api)
  
}


#'GTM List Function
#'
#' @importFrom googleAuthR gar_api_page
#' @importFrom dplyr bind_rows
#' @family api functions
#' @description 
#' This is called by functions that use a list endpoint
#' @noRd
gtm_list <- function(path_args, pars_args = NULL, type) {
  list <- .gtm_api(type = "GET", path_args = path_args)
  res <- googleAuthR::gar_api_page(list, page_f = get_attr_nextLink)
  res <- Reduce(dplyr::bind_rows, res)[[type]]
  return(res)
}

#'GTM Get Function
#'
#' @importFrom googleAuthR gar_api_page
#' @importFrom dplyr bind_rows
#' @family api functions
#' @description 
#' This is called by functions that use a get endpoint
#' @noRd
gtm_get <- function(path_args) {
  get <- .gtm_api(type = "GET", path_args = path_args)
  res <- googleAuthR::gar_api_page(get, page_f = get_attr_nextLink)
  res <- Reduce(dplyr::bind_rows, res)
  return(res)
}

#'GTM Create Function
#'
#' @family api functions
#' @description 
#' This is called by functions that use a create endpoint
#' @noRd
gtm_create <- function(path_args, pars_args = NULL, body = NULL) {
  create <- .gtm_api(type = "POST", path_args = path_args, pars_args = pars_args)
  res <- create(the_body = body)
  return(res)
}

#'GTM update Function
#'
#' @family api functions
#' @description 
#' This is called by functions that use an update endpoint
#' @noRd
gtm_update <- function(path_args, body) {
  update <- .gtm_api(type = "PUT", path_args = path_args)   
  res <- update(the_body = body)
  return(res)
}

#'GTM delete Function
#'
#' @family api functions
#' @description 
#' This is called by functions that use a delete endpoint
#' @noRd
gtm_delete <- function(path_args, pars_args = NULL) {
    delete <- .gtm_api(type = "DELETE", path_args = path_args, pars_args = pars_args)
    res <- delete()
    return(res)
}

#'GTM Action Function
#'
#' @family api functions
#' @description 
#' This is called by functions that use an action attached to a specific endpoint
#' @noRd
gtm_action <- function(path_args, pars_args = NULL, action, body = NULL) {
  path_args[length(path_args)] <- paste0(path_args[length(path_args)],":",action)
  action_call <- .gtm_api(type = "POST", path_args = path_args, pars_args = pars_args)
  res <- action_call(the_body = body )
  return(res)
}

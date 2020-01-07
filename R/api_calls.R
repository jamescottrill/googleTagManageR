#' General functions responsible for the different GTM API calls
#'
#' @importFrom googleAuthR gar_api_generator gar_api_page
#' @importFrom dplyr bind_rows
#' @family tag functions
#' @export
#' 

cancelled <- function() {
  stop("Deletion cancelled")
}

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

gtm_list <- function(path_args, pars_args = NULL, type) {
  list <- .gtm_api(type = "GET", path_args = path_args)
  res <- googleAuthR::gar_api_page(list, page_f = get_attr_nextLink)
  res <- Reduce(dplyr::bind_rows, res)[[type]]
  return(res)
}

gtm_get <- function(path_args) {
  get <- .gtm_api(type = "GET", path_args = path_args)
  res <- googleAuthR::gar_api_page(get, page_f = get_attr_nextLink)
  res <- Reduce(dplyr::bind_rows, res)
  return(res)
}

gtm_create <- function(path_args, pars_args = NULL, body = NULL) {
  create <- .gtm_api(type = "POST", path_args = path_args, pars_args = pars_args)
  res <- create(the_body = body)
  return(res)
}

gtm_update <- function(path_args, body) {
  update <- .gtm_api(type = "PUT", path_args = path_args)   
  res <- update(the_body = body)
  return(res)
}


gtm_delete <- function(path_args, pars_args = NULL) {
    delete <- .gtm_api(type = "DELETE", path_args = path_args, pars_args = pars_args)
    res <- delete()
    return(res)
}

gtm_action <- function(path_args, pars_args = NULL, action, body = NULL) {
  path_args[length(path_args)] <- paste0(path_args[length(path_args)],":",action)
  action_call <- .gtm_api(type = "POST", path_args = path_args, pars_args = pars_args)
  res <- action_call(the_body = body )
  return(res)
}

#' List all container versions of a GTM container
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/version_headers/list}
#' @family version header functions
#' 
#' @description
#'
#' This will return a data frame all container versions.
#' If you want to get the information for the most recent version, use \code{gtm_headers_latest}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param includeDeleted Include deleted version - Default False
#' 
#' @export
gtm_headers_list <- function(account_id, container_id, includeDeleted=c("TRUE","FALSE")) {
  
  if (any(missing(account_id),
         missing(container_id)
  )) {
    stop("Account Id and Container Id are both required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    version_headers = ""
  )
  
  if (missing(includeDeleted)) includeDeleted <- "FALSE"
  includeDeleted <- as.character(includeDeleted)
  includeDeleted <- match.arg(includeDeleted)
  
  pars_args <- list(
    includeDeleted = includeDeleted
  )
  
  res <- gtm_list(path_args = path_args, pars_args = pars_args, type = "containerVersionHeader")
  return(res)
}

#'Gets the latest container version header
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/version_headers/latest}
#' @family version header functions
#' 
#' @description
#'
#' This will return the latest container version header
#' If you want to get the information for the most all container versions, use \code{gtm_headers_list}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' 
#' @export
gtm_headers_latest <- function(account_id, container_id) {
  
  if (any(missing(account_id),
         missing(container_id)
  )) {
    stop("Account Id and Container Id are both required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id
  )
  path_args['version_headers:latest'] <- ""
  
  res <- gtm_get(path_args = path_args)
  
  return(res)
}

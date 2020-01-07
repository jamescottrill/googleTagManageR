#' Manage version headers
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/version_headers
#' @family container version functions
#' @export
#

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

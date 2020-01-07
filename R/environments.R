#' Manage Environments within GTM
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/environments
#' @family container environment functions
#' @export

gtm_environments_list <- function(account_id,container_id) {
  
  if (any(missing(account_id),
          missing(container_id)
  )) {
    stop("Account Id and Container Id are required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    environments = ""
  )
  res <- gtm_list(path_args = path_args, type = "environment")
  return(res)
}

gtm_environments_get <- function(account_id,container_id,environment_id){
  
  if (any(missing(account_id),
         missing(container_id),
         missing(environment_id)
  )) {
    stop("Account Id, Container Id and Environment Id are required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    environments = environment_id
  )
  res <- gtm_get(path_args = path_args)
  return(res)
}

gtm_environments_create <- function(account_id, container_id, name, description = NULL, debug=c("TRUE","FALSE"), url = NULL) {
  
  if (any(missing(account_id),
         missing(container_id)
  )) {
    stop("Account Id and Container Id are both required for this function")
  }
  
  if (missing(name)) stop("An environment name is required for this function")
  if (missing(debug)) debug <- "FALSE"
  
  debug<-match.arg(debug)
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    environments = ""
  )
  
  body <- list(
    name = name,
    description = description,
    debug = debug,
    url = url
  )
  
  res <- gtm_create(path_args = path_args, body = body)
  return(res)
}
 
gtm_environments_update <- function(account_id, container_id ,environment_id, name, description = NULL, debug=c("TRUE","FALSE"), url = NULL){
  
  if (any(missing(account_id),
         missing(container_id),
         missing(environment_id)
  )) {
    stop("Account Id, Container Id and Environment Id are required for this function")
  }
  
  if (missing(name)) stop("An environment name is required for this function")
  if (missing(debug)) debug <- "FALSE"
  
  debug <- match.arg(debug)
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    environments = environment_id
  )
  
  body <- list(
    name = name,
    description = description,
    debug = debug,
    url = url
  )
  
  res <- gtm_update(path_args = path_args, body = body)
  
  return(res)
}

gtm_environments_delete <- function(account_id, container_id, environment_id, force = c("TRUE","FALSE")) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(environment_id)
  )) {
    stop("Account Id, Container Id and Environment Id are required for this function")
  }
  
  if (missing(force)) force <- "FALSE"
  force <- as.character(force)
  force <- match.arg(force)
  force <- as.logical(force)
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    environments = environment_id
  )
  
  if (force) {
    res <- gtm_delete(path_args = path_args)
    if (length(res) == 0) {
      myMessage(sprintf("Environment %s has been deleted", environment_id), level = 3)
    }
  } else {
    switch(
      menu(c("Yes", "No"),
           title = sprintf("This will delete environment %s. Are you sure you want to continue?", environment_id)), {
             res <- gtm_delete(path_args = path_args)
             }, {
               cancelled()
               }
    )
    }
  if (length(res) == 0) {
    myMessage(sprintf("Environment %s has been deleted", environment_id), level = 3)
  }
}

gtm_environments_reauthorize<-function(account_id, container_id, environment_id, environment){
  if (any(missing(account_id),
         missing(container_id),
         missing(environment_id)
  )) {
    stop("Account Id, Container Id and Environment Id are required for this function")
  }
  
  if (missing(environment)) stop("An environment object is required for this function")
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    environments = environment_id
  )
  
  res <- gtm_action(path_args = path_args, action = "reauthorize", body = environment)
  return(res)
}

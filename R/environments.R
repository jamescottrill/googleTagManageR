#' List the containers
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/environments/list}
#' @family environment functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#'
#' @description
#'
#' This lists the environments in a specified container.
#' 
#'  @export
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

#' This gets the metadata for a single GTM Environment
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/environments/get}
#' @family environment functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param environment_id Environment Id
#'
#' @description
#'
#' This gets the metadata for a single GTM Environment
#' 
#'  @export
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

#' Create an environment
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/environments/update}
#' @family environment functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' 
#' @param name The environment display name. Can be set or changed only on USER type environments.
#' @param description The environment description. Can be set or changed only on USER type environments.
#' @param debug Whether or not to enable debug by default for the environment.
#' @param url Default preview page url for the environment.
#'
#' @description
#'
#' This creates a new GTM environment
#' 
#'  @export
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

#' Update an environment
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/environments/update}
#' @family environment functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param environment_id Environment Id
#' @param name The environment display name. Can be set or changed only on USER type environments.
#' @param description The environment description. Can be set or changed only on USER type environments.
#' @param debug Whether or not to enable debug by default for the environment.
#' @param url Default preview page url for the environment.
#'
#' @description
#'
#' This deletes a GTM environment
#' 
#'  @export
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

#' Delete an environment
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/environments/reauthorize}
#' @family environment functions
#' @importFrom utils menu
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param environment_id Environment Id
#' @param force This will bypass the user confirmation. Useful for scripted functions to avoid user input
#'
#' @description
#'
#' This deletes a GTM environment
#' 
#'  @export
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


#' This reauthorises an existing GTM environment
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/environments/reauthorize}
#' @family environment functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param environment_id Environment Id
#' @param environment An Environment object
#'
#' @description
#'
#' 
#'  @export
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

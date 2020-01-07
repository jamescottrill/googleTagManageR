#' Manage Versions in GTM
#'
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/versions
#' @family container version functions
#' @export

gtm_versions_get <- function(account_id,container_id,version_id) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(version_id)
  )){
    stop("Account Id, Container Id and Version Id are all required for this function")
  }
  
  account_id<-as.character(account_id)
  container_id<-as.character(container_id)
  version_id<-as.character(version_id)
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    versions = version_id
  )
  
  res <- gtm_get(path_args = path_args)
  return(res)
}

gtm_versions_update <- function(account_id,container_id,version_id, name = NULL, description = NULL) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(version_id)
  )){
    stop("Account Id, Container Id and Version Id are all required for this function")
  }
  
  if (all(missing(name),
         missing(description)
  )){
    stop("Either a name or description are required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    versions = version_id
  )
  
  body <- list(
    name = name,
    description = description
  )
  
  res<-gtm_update(path_args = path_args, body = body)
  
  if (res$versionId) {
    myMessage(sprintf("Version %s (%s) has been updated", res$name, res$versionId), level = 3)
  } else {
    myMessage("An Error has occured, please check your inputs and try again", level = 3)
  }
  
  return(res)
}

gtm_versions_delete <- function(account_id, container_id, version_id, force = c("TRUE","FALSE")) {
    if(any(missing(account_id),
           missing(container_id),
           missing(version_id)
    )) {
      stop("Account Id, Container Id and Version Id are all required for this function")
    }
    
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = version_id
    )
    
    if (missing(force)) force <- "FALSE"
    force <- as.character(force)
    force <- match.arg(force)
    force <- as.logical(force)
    
    if (force) {
      res <- gtm_delete(path_args = path_args)
    } else {
      switch(
        menu(c("Yes", "No"),
             title = sprintf("This will delete Version %s. Are you sure you want to continue?", version_id)),
        {
          res <- gtm_delete(path_args = path_args)
        },
        {
          cancelled()
        }
      )
    }
    if (length(res) == 0) {
      myMessage(sprintf("Version %s has been deleted", version_id), level = 3)
    }
  }

gtm_versions_setlatest <- function(account_id,container_id,version_id) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(version_id)
  )) {
    stop("Account Id, Container Id and Version Id are all required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    versions = version_id
  )
  res <- gtm_action(path_args = path_args, action = "set_latest")
  myMessage(sprintf("Version %s has been set to the latest version", res$containerVersion$containerVersionId), level=3)
  return(res)
}

gtm_versions_undelete <- function(account_id, container_id, version_id) {

  if(any(missing(account_id),
         missing(container_id),
         missing(version_id)
  )) {
    stop("Account Id, Container Id and Version Id are all required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    versions = version_id
  )
  
  res <- gtm_action(path_args = path_args, action = "undelete")
  myMessage(sprintf("Version %s has been undeleted",res$containerVersion$containerVersionId),level=3)
  return(res)
}
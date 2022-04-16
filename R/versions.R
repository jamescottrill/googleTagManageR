#' List a single version in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/versions/get}
#' @family version functions
#' 
#' @description
#'
#' This returns a single workspace version
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param version_id Version Id
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' versionId <- 6
#' 
#' version <- gtm_versions_get(accountId, containerId, versionId)
#' 
#' }
#' 
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

#' Update a container version
#'
#' @seealso \url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/versions/update}
#' @family version functions
#' 
#' @description
#'
#' Updates a container Version
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param version_id Version Id
#' @param name Version Name
#' @param description Version Description
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' versionId <- 6
#' newName <- 'My Cool New Version'
#' newDescription <- 'My new version does something cool'
#' version <- gtm_versions_update(accountId, containerId, versionId, newName, newDescription)
#' 
#' }
#' 
#' @export
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
#' Delete a container version
#'
#' @seealso \url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/versions/delete}
#' @family version functions
#' 
#' @description
#'
#' Deletes a container version
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param version_id Version Id
#' @param force Force deletion without user input
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' versionId <- 6
#' 
#' gtm_versions_delete(accountId, containerId, versionId)
#' 
#' # This will delete Version %s. Are you sure you want to continue?
#' 
#' #  1: Yes
#' #  2: No
#'
#' # Selection: 1
#' 
#' # Version 6 has been deleted.
#' 
#' tagId = 7
#' 
#' gtm_versions_delete(accountId, containerId, versionId, "TRUE")
#' 
#' # Version 7 has been deleted.
#' 
#' }
#' 
#' @export
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


#' Sets a container version as the latest version
#' 
#' @seealso \url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/versions/set_latest}
#' @family versions functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param version_id Version Id
#'
#' @description
#' Sets the given container version as the latest version
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' versionId <- 6
#' 
#' version <- gtm_versions_setlatest(accountId, containerId, versionId)
#' 
#' }
#' 
#' 
#' @export
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
  myMessage(sprintf("Version %s has been set to the latest version", res$containerVersionId), level=3)
  invisible(res)
}

#' Undeletes a container version
#' 
#' @seealso \url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/versions/undelete}
#' @family versions functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param version_id Version Id
#'
#' @description
#' Undeletes a container version
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' versionId <- 6
#' 
#' version <- gtm_versions_undelete(accountId, containerId, versionId)
#' 
#' }
#' 
#' 
#' @export
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

#' Publish a new container version
#' 
#' @seealso \url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/versions/publish}
#' @family versions functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param version_id Version Id
#'
#' @description
#' Publishes a container version to be live
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' versionId <- 6
#' 
#' version <- gtm_versions_publish(accountId, containerId, versionId)
#' 
#' }
#' 
#' 
#' @export
gtm_versions_publish <- function(account_id, container_id, version_id) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(version_id)
  )) {
    stop("Account Id, Container Id and Version Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    versions = version_id
  )
  res <- gtm_action(path_args = path_args, action = "publish")
  myMessage(sprintf("Version %s has been published as Live in %s",res$containerVersion$containerVersionId, res$containerVersion$container$name),level=3)
  return(res)
}
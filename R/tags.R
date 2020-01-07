#' Manage Tags in GTM
#'
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/tags
#' @family tag functions
#' @export

gtm_tags_list <- function(account_id, container_id, workspace_id) {
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function")
  }
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    tags = ""
  )
  res <- gtm_list(path_args = path_args, type = "tag")
  return(res)
}

gtm_tags_get <- function(account_id,container_id,workspace_id,tag_id) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(tag_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Tag Id are all required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    tags = tag_id
  )
  
  res <- gtm_get(path_args)
  return(res)
}

gtm_tags_create <- function(account_id,container_id,workspace_id,tag_object) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Tag Id are all required for this function")
  }
  
  if (missing(tag_object)) stop("A Tag Object is required for this function")
    
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    tags = ""
  )
  res <- gtm_create(path_args = path_args, body = tag_object)
  
  myMessage(sprintf("Tag %s (%s) has been created",res$name,res$tagId),level=3)
  
  return(res)
}


gtm_tags_update <- function(account_id, container_id, workspace_id, tag_id, tag_object) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(tag_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Tag Id are all required for this function")
  }
  
  if (missing(tag_object)) stop("A Tag Object is required for this function")
  
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      tags = tag_id
    )
    res <- gtm_update(path_args = path_args, body = tag_object)
    myMessage(sprintf("Tag %s (%s) has been updated", res$name, res$tagId),
              level = 3)
    return(res)
  }

gtm_tags_delete <-function(account_id, container_id, workspace_id, tag_id, force = c("TRUE","FALSE")) {

    if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(tag_id)
    )){
      stop("Account Id, Container Id, Workspace Id and Tag Id are all required for this function")
    }
    
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      tags = tag_id
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
             title = sprintf("This will delete tag %s. Are you sure you want to continue?", tag_id)),
        {
          res <- gtm_delete(path_args = path_args)
        },
        {
          cancelled()
        }
      )
    }
    if (length(res) == 0) {
      myMessage(sprintf("Tag %s has been deleted", tag_id), level = 3)
    }
    return()
  }

gtm_tags_revert <- function(account_id, container_id, workspace_id, tag_id) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(tag_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Tag Id are all required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    tags = tag_id
  )
  
  res <- gtm_action(path_args = path_args, action = "revert")
  myMessage(sprintf("Changes to tag %s have been reverted", res$tag$tagId), level = 3)
  return(res)
}
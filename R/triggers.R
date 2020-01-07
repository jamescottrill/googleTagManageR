#' Manage Triggers in GTM
#'
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers
#' @family trigger functions
#' @export


gtm_triggers_list <- function(account_id, container_id, workspace_id) {
  
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
    triggers = ""
  )
  res <- gtm_list(path_args = path_args, type = "trigger")
  return(res)
}

gtm_triggers_get <- function(account_id, container_id, workspace_id, trigger_id) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(trigger_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Trigger Id are all required for this function")
  }
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    triggers = trigger_id
  )
  res <- gtm_get(path_args = path_args)
  return(res)
}

gtm_triggers_create <- function(account_id, container_id, workspace_id, trigger_object) {

  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function")
  }
  
  if (missing(trigger_object)) stop("A Trigger Object is required for this function")
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    triggers = ""
  )
  
  res <- gtm_create(path_args = path_args, body = trigger_object)
  myMessage(sprintf('Trigger "%s" (%s) has been created', res$name,res$triggerId) ,level=3)
  return(res)
}


gtm_triggers_update <- function(account_id, container_id, workspace_id, trigger_id, trigger_object) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(trigger_id)
    )) {
    stop("Account Id, Container Id, Workspace Id and Trigger Id are all required for this function")
  }
  
  if (missing(trigger_object)) stop("A Trigger Object is required for this function")
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    triggers = trigger_id
  )
  res <- gtm_update(path_args = path_args, body = trigger_object)
  myMessage(sprintf('Trigger %s - "%s" has been updated',res$triggerId, res$name) ,level=3)
  return(res)
}

gtm_triggers_delete <- function(account_id, container_id, workspace_id, trigger_id, force = c(TRUE,FALSE)) {

      if(any(missing(account_id),
           missing(container_id),
           missing(workspace_id),
           missing(trigger_id)
      )) {
      stop("Account Id, Container Id, Workspace Id and Trigger Id are all required for this function")
    }
  
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      triggers = trigger_id
    )
    
    if (missing(force)) force <- "FALSE"
    force <- as.character(force)
    force <- match.arg(force)
    force <- as.logical(force)
    
    if (force) {
      res<-gtm_delete(path_args = path_args)
    } else {
      switch(
        menu(c("Yes", "No"),
             title = sprintf("This will delete Trigger %s. Are you sure you want to continue?", trigger_id)),
        {
          res<-gtm_delete(path_args = path_args)
        },
        {
          cancelled()
        }
      )
    }
    if (length(res) == 0) {
      myMessage(sprintf("Trigger %s has been deleted", trigger_id), level = 3)
    }
  }

gtm_triggers_revert <- function(account_id, container_id, workspace_id, trigger_id) {
    
  if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(trigger_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Trigger Id are all required for this function")
  }
  
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      triggers = trigger_id
    )
    
    res <- gtm_action(path_args = path_args, action = "revert")
    myMessage(sprintf("Changes to trigger %s (%s) have been reverted", res$trigger$name, res$zone$triggerId), level = 3)
    return(res$trigger)
  }
#' List all triggers in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers/list}
#' @family trigger structure functions
#' 
#' @description
#'
#' This will return a data frame all your available triggers in a given workspace
#' If you want to get the information for a single trigger, use \code{gtm_triggers_get}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' 
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

#' List a single trigger in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers/get}
#' @family trigger structure functions
#' 
#' @description
#'
#' This will return a list with the settings of a single trigger.
#' If you want to get the information for all triggers, use \code{gtm_triggers_list}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param trigger_id trigger Id
#' 
#' @export
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

#' Create a new trigger
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers/create}
#' @family trigger structure functions
#' 
#' @description
#'
#' This takes a trigger resource and creates a new trigger in GTM.
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param trigger_object trigger Object
#' 
#' @export
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


#' Update an existing trigger
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers/update}
#' @family trigger structure functions
#' 
#' @description
#'
#' This takes a trigger resource and overwrites the existing trigger in GTM
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param trigger_id trigger Id
#' @param trigger_object trigger Object
#' 
#' @export
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

#' Delete a trigger
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers/delete}
#' @family trigger structure functions
#' 
#' @description
#'
#' This deletes a trigger in a GTM workspace
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param trigger_id trigger Id
#' @param force Force deletion without user input
#' 
#' @export
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

#' Reverts a trigger to its original state
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers/revert}
#' @family trigger structure functions
#' 
#' @description
#'
#' This reverts a trigger back to its original, unmodified state.
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param trigger_id Trigger Id
#' 
#' @export
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
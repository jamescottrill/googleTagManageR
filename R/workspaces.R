#' Manage Workspaces in GTM
#'
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces
#' @family workspaces functions
#' @export

gtm_workspaces_list <- function(account_id, container_id) {
  
  if (any(missing(account_id),
         missing(container_id)
  )) {
    stop("Account Id and Container Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = ""
  )
  res <- gtm_list(path_args = path_args, type="workspace")
  return(res)
}

gtm_workspaces_get <- function(account_id, container_id, workspace_id) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id
  )
  
  res <- gtm_get(path_args = path_args)
  return(res)
}

gtm_workspaces_create <- function(account_id, container_id, name, description = NULL) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(name)
  )) {
    stop("Account Id, Container Id and Name are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = ""
  )
  
  body <- list(
    name = name,
    description = description
  )
  
  res <- gtm_create(path_args = path_args, body = body)
  myMessage(sprintf('Workspace "%s" created in Container %s with workspaceId %s',res$name,res$containerId,res$workspaceId),level = 3)
  return(res)
}

gtm_workspaces_update <- function(account_id, container_id, workspace_id, name, description = NULL) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(name)
    )) {
    stop("Account Id, Container Id, Workspace Id and Name are all required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id
  )
  
  body <- list(
    name = name,
    description = description
  ) 
  
  res <- gtm_update(path_args = path_args, body = body)
  
  if (res$workspaceId) {
    myMessage(sprintf("Workspace %s (%s) has been updated", res$name, res$workspaceId), level = 3)
  } else {
    myMessage("An Error has occured, please check your inputs and try again", level = 3)
  }
  
  return(res)
}

gtm_workspaces_delete <- function(account_id, container_id, workspace_id, force = c("TRUE","FALSE")) {
    
    if (any(missing(account_id),
           missing(container_id),
           missing(workspace_id)
      )) {
      stop("Account, Container and Workspace Ids are required for this function")
    }
    
  if (missing(force)) force = "FALSE"
  force <- as.character(force)
  force <- match.arg(force)
  force <- as.logical(force)
    
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id
    )
    
    
    if (force) {
      res <- gtm_delete(path_args = path_args)
    } else {
      switch(
        menu(c("Yes", "No"),
             title = sprintf("This will delete Workspace %s. Are you sure you want to continue?", workspace_id)),
        {
          res <- gtm_delete(path_args = path_args)
        },
        {
          cancelled()
        }
      )
    }
    if (length(res) == 0) {
      myMessage(sprintf("Workspace %s has been deleted", workspace_id), level = 3)
    }
  }


gtm_workspaces_status <- function(account_id,container_id,workspace_id) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )){
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    status = ''
  )
  status <- gtm_get(path_args = path_args)
  return(status)
}


gtm_workspaces_submit <- function(account_id, container_id, workspace_id, name = NULL, notes = NULL) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
    )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  

  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id
  )
  
  body <- list(
    name = name,
    notes = notes
  )
  res <- gtm_action(path_args = path_args, action = "create_version")
  
  if (!is.null(res$syncStatus$mergeConflict)) {
    myMessage(sprintf("There is a conflict in container %s, you will need to fix this before you can submit the workspace.",container_id), level = 3)
    return(res$syncStatus)
  }
  else if (res$containerVersion$containerVersionId==0) {
    myMessage(sprintf("There is an error in Container, please check your changes and try again"), level = 3)
  } else {
  myMessage(sprintf("Version %s has been created in container %s", res$containerVersion$containerVersionId, container_id), level=3)
  }
  return(res$containerVersion)
}

gtm_workspaces_publish <- function(account_id, container_id, version_id) {
  
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

gtm_workspaces_preview<-function(account_id,container_id,workspace_id){
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id
  )
  
  res <- gtm_action(path_args = path_args, action = "quick_preview")
  
  if (res$compilerError) {
    myMessage("There was an error compiling the container, that's all we know.", level = 3)
    return (res)
    }
  if (res$syncStatus$mergeConflict) {
    myMessage("The sync was unsuccessful, please check your workspace and try again.", level = 3)
    return(res)
    }
  if (res$syncStatus$mergeConflict) { 
    myMessage("There is a conflict in the workspace. This will need to be resolved before the workspace can be merged.", level = 3)
    return(res$mergeConflict)
    }
  return(res$containerVersion)
  
}

gtm_workspaces_sync <- function(account_id,container_id,workspace_id) {
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id
  )
  
  res <- gtm_action(path_args = path_args, action = "sync")
  if (res$syncStatus$mergeConflict) myMessage("The sync was unsuccessful, please check your workspace and try again.", level = 3)
  if (res$syncStatus$mergeConflict) myMessage("There is a conflict in the workspace. This will need to be resolved before the workspace can be merged.", level = 3)
  
  return(res)
}

gtm_workspaces_resolve <- function(account_id,
                                   container_id,
                                   workspace_id, 
                                   tag = NULL, 
                                   trigger = NULL, 
                                   variable = NULL, 
                                   folder = NULL, 
                                   changeStatus = c('added','changeStatusUnspecified','deleted','none','updated') 
                                   ) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  
  if (all(missing(tag),
         missing(trigger),
         missing(variable),
         missing(folder)
    )) {
    stop("You need to include the entity that you want to resolve")
  }
  
  if (missing(changeStatus)) stop("The change status of the entity must be included in the request")
  
  changeStatus <- match.arg(changeStatus)
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id
  )
  
  body <- list(
    changeStatus = changeStatus,
    tag = tag,
    trigger = trigger,
    variable = variable,
    folder = folder
  )
  
  res <- gtm_action(path_args = path_args, action = "resolve", body = body)
  
  return(res)
}


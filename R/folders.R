#' Manage Folders within GTM
#' @seealso https://developers.google.com/folder-manager/api/v2/reference/accounts/containers/workspaces/folders
#' @family workspace folder functions
#' @export
#' 

gtm_folders_list <- function(account_id, container_id, workspace_id) {
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    folders = ""
  )
  
  res <- gtm_list(path_args = path_args, type = 'folder')
  return(res)
}

gtm_folders_get <- function(account_id,container_id,workspace_id,folder_id) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(folder_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Folder Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    folders = folder_id
  )
  
  res <- gtm_get(path_args)
  return(res)
}

gtm_folders_create<-function(account_id, container_id, workspace_id, folder_object){
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(folder_object)
  )) {
    stop("Account Id, Container Id, Workspace Id and Folder Object are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    folders = ""
  )
  
  res <- gtm_create(path_args, body = folder_object)
  myMessage(sprintf("folder %s (%s) has been created", res$name, res$folderId),level=3)
  return(res)
}


gtm_folders_update <- function(account_id, container_id, workspace_id, folder_id, folder_object) {
    
    if (any(missing(account_id),
           missing(container_id),
           missing(workspace_id),
           missing(folder_id)
    )) {
      stop("Account Id, Container Id, Workspace Id and Folder Id are all required for this function.")
    }
    
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      folders = folder_id
    )
    res <- gtm_update(path_args = path_args, body = folder_object)
    myMessage(sprintf("folder %s (%s) has been updated", res$name, res$folderId), level = 3)
    return(res)
  }

gtm_folders_delete <-function(account_id, container_id, workspace_id, folder_id, force = c("TRUE","FALSE")) {
    
    if (any(missing(account_id),
           missing(container_id),
           missing(workspace_id),
           missing(folder_id)
    )) {
      stop("Account, Container, Workspace and Folder Ids are required for this function")
    }
    
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      folders = folder_id
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
             title = sprintf("This will delete folder %s. Are you sure you want to continue?", folder_id)), {
               res <- gtm_delete(path_args = path_args)
               }, {
                 cancelled()
                 }
      )
      }
    if (length(res) == 0) {
      myMessage(sprintf("Folder %s has been deleted", folder_id), level = 3)
    }
  }

gtm_folders_revert <- function(account_id, container_id, workspace_id, folder_id) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(folder_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Folder Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    folders = folder_id
  )
  res <- gtm_action(path_args = path_args, action = "revert")
  myMessage(sprintf("Changes to folder %s have been reverted", res$folder$folderId), level = 3)
  return(res)
}

gtm_folders_entities<-function(account_id, container_id, workspace_id, folder_id){
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(folder_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Folder Id are all required for this function.")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    folders = folder_id
  )
  res <- gtm_action(path_args = path_args, action = "entities")
  return(res)
}

gtm_folders_move<-function(account_id, container_id, workspace_id, folder_id, tags = NULL, triggers = NULL, variables = NULL, folder){
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(folder_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Folder Id are all required for this function.")
  }
  
  if (all(missing(tags),
         missing(triggers),
         missing(variables)
  )) {
    stop("You need to include at least one entity that you're moving")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    folders = folder_id
  )
  
  pars_args <- list(
    tags = tags,
    triggers = triggers,
    variables = variables
  )
  
  res <- gtm_action(path_args = path_args, pars_args = pars_args, action = "move_entities_to_folder", body = folder)
  if (length(res)==0) myMessage(sprintf("Entities were successfully moved to the folder %s", folder$name), level = 3)
  return()
}


#' List all containers in an account
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/list}
#' @family folder functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#'
#' @description
#'
#' This returns a dataframe containing all the folder in a workspace
#' If you want to get the information for a single folder, use \code{gtm_folders_get}
#' 
#' @export
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

#' Gets a GTM Folder
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/get}
#' @family folder functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param folder_id Folder Id
#'
#' @description
#'
#' This returns a list containing all the metadata for a single folder in a workspace
#' If you want to get the information for all folders, use \code{gtm_folders_list}
#' 
#' @export
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

#' Creates a GTM Folder
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/get}
#' @family folder functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param name Folder display name
#' @param notes User notes on how to apply this folder in the container.
#'
#' @description
#'
#' This creates a new folder in the specified workspaces

#' 
#' @export
gtm_folders_create<-function(account_id, container_id, workspace_id, name, notes = NULL){
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(name)
  )) {
    stop("Account Id, Container Id, Workspace Id and Folder Name are all required for this function.")
  }
  
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    folders = ""
  )
  
  folder <-list(
    name = name,
    notes = notes
  ) 
  
  res <- gtm_create(path_args, body = folder)
  myMessage(sprintf("folder %s (%s) has been created", res$name, res$folderId),level=3)
  return(res)
}

#' Updates a GTM Folder
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/update}
#' @family folder functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param folder_id Folder Id
#' @param name Folder display name
#' @param notes User notes on how to apply this folder in the container.
#'
#' @description
#'
#' This updates a folder in the specified workspaces
#' 
#' @export
gtm_folders_update <- function(account_id, container_id, workspace_id, folder_id, name = NULL, notes = NULL) {
    
    if (any(missing(account_id),
           missing(container_id),
           missing(workspace_id),
           missing(folder_id)
    )) {
      stop("Account Id, Container Id, Workspace Id and Folder Id are all required for this function.")
    }
  if(all(missing(name),
         missing(notes)
  )) {
    stop("Either a new name or new notes are required for this function..")
  }
  
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      folders = folder_id
    )
  
    folder_object <- list(
      name = name,
      nones = notes
    )
    
    res <- gtm_update(path_args = path_args, body = folder_object)
    myMessage(sprintf("folder %s (%s) has been updated", res$name, res$folderId), level = 3)
    return(res)
  }

#' Deletes a GTM Folder
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/delete}
#' @family folder functions
#' @importFrom utils menu
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param folder_id Folder Id
#' @param force Force deletion without user input
#'
#' @description
#'
#' This updates a folder in the specified workspaces
#' 
#' @export
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


#' Reverts a GTM Folder
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/revert}
#' @family folder functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param folder_id Folder Id
#'
#' @description
#'
#' This reverts any changes to a folder in the current workspace
#' 
#' @export
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

#' List all entities in a GTM Folder.
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/entities}
#' @family folder functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param folder_id Folder Id
#'
#' @description
#'
#' This lists all entities in a GTM Folder.
#' 
#' @export
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

#' Move entities to a new folder
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/folders/move_entities_to_folder}
#' @family folder functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param folder_id Folder Id
#' @param tags The tags to be moved to the folder. This can either be an individual tag Id or a list of tag Ids
#' @param triggers The triggers to be moved to the folder. This can either be an individual trigger Id or a list of trigger Ids
#' @param variables The variables to be moved to the folder. This can either be an individual variable Id or a list of variable Ids
#'
#' @description
#'
#' This moves entities to a new folder.
#' 
#' @export
gtm_folders_move<-function(account_id, container_id, workspace_id, folder_id, tags = NULL, triggers = NULL, variables = NULL){
  
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
  
  pars_args <- list()
  
  if(length(tags) > 1) pars_args <- convert('tagId', tags, pars_args)
  if(length(triggers) > 1) pars_args <- convert('triggerId', triggers, pars_args)
  if(length(variables)>1) pars_args <- convert('variableId', variables, pars_args)
  if(length(tags) == 1) pars_args['tagId'] <- tags
  if(length(triggers) == 1) pars_args['triggerId'] <- triggers
  if(length(variables) == 1) pars_args['variableId'] <- variables
  
  folder <- list(
    accountId = account_id,
    containerId = container_id,
    workspaceId = workspace_id
  )
  
  res <- gtm_action(path_args = path_args, pars_args = pars_args, action = "move_entities_to_folder", body = folder)
  if (length(res)==0) myMessage(sprintf("Entities were successfully moved to the folder %s", folder$name), level = 3)
  return()
}


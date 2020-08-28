#' List all workspaces a user has accesss to 
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/list}
#' @family workspace functions
#' 
#' @description
#'
#' This will return a data frame all your available workspaces in a given container.
#' If you want to get the information for a single workspace, use \code{gtm_workspaces_get}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' 
#' workspaces <- gtm_workspaces_list(accountId, containerId)
#' 
#' }
#' 
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

#' Get workspace metadata
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/get}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#'
#' @description
#'
#' This returns a list containing the metadata about a single Workspace
#' If you want to get the information for all workspaces, use \code{gtm_workspaces_list}
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 5
#' 
#' workspace <- gtm_workspaces_get(accountId, containerId, workspaceId)
#' 
#' }
#' 
#' @export
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

#' Create a new workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/create}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param name Workspace Name
#' @param description Workspace Description
#'
#' @description
#'
#' This creates a new workspace in an esisting container.
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' name <- 'New Analytics Tags'
#' description <- 'Adding element visibility tags'
#' 
#' workspace <- gtm_workspaces_create(accountId, containerId, name, description)
#' 
#' }
#' 
#' @export
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

#' Update a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/update}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param name Workspace Name
#' @param description Workspace Description
#'
#' @description
#'
#' This updates the name or description of an existing workspace.
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 6
#' name <- 'New Analytics Tags'
#' description <- 'Adding element visibility tags and scroll tracking'
#' 
#' workspace <- gtm_workspaces_update(accountId, containerId, workspaceId, name, description)
#' 
#' }
#' 
#' @export
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

#' Delete a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/delete}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param force Force deletion without user input
#'
#' @description
#'
#' This irrevocably deletes a workspace and any work in it.
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 84
#' 
#' gtm_workspaces_delete(accountId, containerId, workspaceId)
#' 
#' # This will delete Workspace %s. Are you sure you want to continue?
#' 
#' #  1: Yes
#' #  2: No
#'
#' # Selection: 1
#' 
#' # Workspace 84 has been deleted.
#' 
#' workspaceId = 85
#' 
#' gtm_workspaces_delete(accountId, containerId, workspaceId, "TRUE")
#' 
#' # Workspace 85 has been deleted.
#' 
#' }
#' 
#' @export
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


#' See workspace changes
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/getStatus}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#'
#' @description
#'
#' Finds conflicting and modified entities in the workspace.
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 6
#' 
#' workspace <- gtm_workspaces_status(accountId, containerId, workspaceId)
#' 
#' }
#' 
#' 
#' @export
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

#' Create a new container version
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/create_version}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param name Version Name
#' @param notes Version Notes
#'
#' @description
#'
#' This creates a new version of the GTM container, but does not publish it
#' as a live version. The name and notes will be permanently visible for the container version.
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 6
#' name <- 'New Analytics Tags'
#' notes <- 'Adding element visibility tags and scroll tracking'
#' 
#' workspace <- gtm_workspaces_update(accountId, containerId, workspaceId, name, notes)
#' 
#' }
#' 
#' 
#' @export
gtm_workspaces_submit <- function(account_id, container_id, workspace_id, name, notes = NULL) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
    )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function.")
  }
  
  if(missing(name)) stop("New versions need a name, please provide one.")
  

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
  
  if (!is.null(res$syncStatus$syncError)) {
    myMessage(sprintf("There is an error in container %s, you will need to fix this before you can submit the workspace.",container_id), level = 3)
    return(res$syncStatus)
  }
  else if (res$containerVersion$containerVersionId==0) {
    myMessage(sprintf("There is an error in Container, please check your changes and try again"), level = 3)
  } else {
  myMessage(sprintf("Version %s has been created in container %s", res$containerVersion$containerVersionId, container_id), level=3)
  }
  return(res$containerVersion)
}

#' Preview the compilation of a container version
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/quick_preview}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#'
#' @description
#' Quick previews a workspace by creating a fake container version from all entities in the 
#' provided workspace. It returns a container object, and if there are any conflicts, sync errors
#' or compiler errors in the workspace
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 6
#' 
#' preview <- gtm_workspaces_preview(accountId, containerId, workspaceId)
#' 
#' }
#' 
#' @return Invisibly, the server response. If successful this is list with one element.
#' @export
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
  
  if (!is.null(res$compilerError)) {
    myMessage("There was an error compiling the container, that's all we know.", level = 3)
    return (res)
    }
  if (!is.null(res$syncStatus$mergeConflict)) {
    myMessage("The sync was unsuccessful, please check your workspace and try again.", level = 3)
    return(res)
    }
  if (!is.null(res$syncStatus$mergeConflict)) { 
    myMessage("There is a conflict in the workspace. This will need to be resolved before the workspace can be merged.", level = 3)
    return(res$mergeConflict)
    }
  return(res$containerVersion)
  
}

#' Bring a workspace in line with the latest version
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/sync}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#'
#' @description
#' Syncs a workspace to the latest container version by updating all unmodified 
#' workspace entities and displaying conflicts for modified entities
#' 
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 6
#' 
#' syncResult <- gtm_workspaces_sync(accountId, containerId, workspaceId)
#' 
#' }
#' 
#' 
#' @export
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
  if (!is.null(res$syncStatus$syncError) && res$syncStatus$syncError) myMessage("There was a sync error in your workspace. This usually means you've created an object with a name that's already in use. 
                                                                                The API doesn't expose this information, so either compare your base and new workspaces manually or try and sync on the
                                                                                UI to see the result.", level = 3)
  if (!is.null(res$syncStatus$mergeConflict) && res$syncStatus$mergeConflict) myMessage(sprintf("There was a merge conflict in your workspace, please check your workspace and try again. There are %s conflicts to resolve", length(res$mergeConflict$entityInBaseVersion)), level = 3)
  invisible(res)
}

#' Resolve workspace conflicts
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/resolve_conflict}
#' @family workspace functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param tag A Tag Object
#' @param trigger A Trigger Object
#' @param variable A Variable Object
#' @param folder A Folder Object
#' @param changeStatus Represents how the entity has been changed in the workspace.
#'
#' @description
#' Resolves a merge conflict for a workspace entity by updating it to the resolved 
#' entity passed in the request. This is one of the more complex functions, however if you
#' are using this package for automation you will most probably want to keep the version from
#' the current workspace not the base version, which is shown in hte example.
#' @examples
#' 
#' \dontrun{
#' 
#'  # Comlpexities arise converting the list created in R into the correct JSON for GTM, specifically for the 
#'  # Monitoring Metadata in Tags, so this is brought out, converted separately and then included in the tag 
#'  # before it is resolved.
#' 
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' 
#' mergeConflicts <- gtm_workspaces_sync(accountId, containerId, workspaceId)
#' 
#' for(i in 1:nrow(mergeConflicts$mergeConflict$entityInWorkspace)){
#'     tag <- NULL
#'     trigger <- NULL
#'     variable <- NULL
#'     folder <- NULL
#'     obj <- mergeConflicts$mergeConflict$entityInWorkspace[i,]
#'     if(!is.na(obj$tag$path)){
#'        tag <- obj$tag
#'        if(is.null(tag$monitoringMetadata$map)){
#'        tag <- tag %>% select(-monitoringMetadata)
#'        tag <- as.list(tag) %>% lapply(function(x){if(is.null(x[[1]])){x<-NA}else{x<-x}}) %>% lapply(function(x) x[!is.na(x)])
#'        } else{
#'        metadataMap <- tag$monitoringMetadata$map[[1]]
#'        metadata <- list(
#'            type = 'map',
#'            map = metadataMap
#'        )
#'        tag <- as.list(tag) %>% lapply(function(x){if(is.null(x[[1]])){x<-NA}else{x<-x}}) %>% lapply(function(x) x[!is.na(x)])
#'        tag$monitoringMetadata <- metadata
#'        }
#'        
#'     }
#'     if(!is.na(obj$trigger$path)){
#'        trigger <- obj$trigger
#'        trigger<-as.list(trigger) %>% lapply(function(x){if(is.null(x[[1]])){x<-NA}else{x<-x}}) %>% lapply(function(x) x[!is.na(x)])
#'     }
#'     if(!is.na(obj$variable$path)){
#'        variable <- obj$variable
#'        variable<-as.list(variable) %>% lapply(function(x){if(is.null(x[[1]])){x<-NA}else{x<-x}}) %>% lapply(function(x) x[!is.na(x)])
#'     }
#'     if(!is.na(obj$folder$path)){
#'        folder <- obj$folder
#'        folder<-as.list(folder) %>% lapply(function(x){if(is.null(x[[1]])){x<-NA}else{x<-x}}) %>% lapply(function(x) x[!is.na(x)])
#'     }
#'     changeStatus <- obj$changeStatus
#'     
#'     resolve <- gtm_workspaces_resolve(accountId, containerId, workspaceId, tag, trigger, variable, folder, changeStatus)
#'   }
#' }
#' 
#' @export
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
  
  res <- gtm_action(path_args = path_args, action = "resolve_conflict", body = body)
  
  invisible(res)
}
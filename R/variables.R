#' Manage Variables in GTM
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables
#' @family account structure functions
#' @export

gtm_variables_list <- function(account_id,container_id,workspace_id) {
  
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
    variables = ''
  )
  res <- gtm_list(path_args = path_args, type = "variable")
  return(res)
}

gtm_variables_get <- function(account_id,container_id,workspace_id,variable_id) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(variable_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Variable Id are all required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    variables = variable_id
  )
  variables <- gtm_get(path_args = path_args)
  return(variables)
}

gtm_variables_create <- function(account_id, container_id, workspace_id, variable_object) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function")
  }
  
  if (missing(variable_object)) stop("A Variable Object is required for this function")
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    variables = ''
  )
  res <- gtm_create(path_args = path_args, body = variable_object)
  myMessage(sprintf("Variable %s (%s) has been created", res$name, res$variableId), level=3)
  return(res)
}

gtm_variables_update <- function(account_id,container_id,workspace_id,variable_id,variable_object) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function")
  }
  
  if (missing(variable_object)) stop("A Variable Object is required for this function")
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    variables = variable_id
  )
  res <- gtm_update(path_args = path_args, body = variable_object)
  myMessage(sprintf("Variable %s (%s) has been updated",res$name,res$variableId),level=3)  
  return(res)
}


gtm_variables_delete <- function(account_id, container_id, workspace_id, variable_id, force = c(TRUE,FALSE)) {
  
  if(any(missing(account_id),
           missing(container_id),
           missing(workspace_id),
           missing(variable_id)
    )){ 
    stop("Account, Container, Workspace and Variable Ids are required for this function")
  }
    
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      variables = variable_id
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
             title = sprintf("This will delete variable %s. Are you sure you want to continue?", variable_id)),
        {
          res<-gtm_delete(path_args = path_args)
        },
        {
          cancelled()
        }
      )
    }
    if (length(res) == 0) {
      myMessage(sprintf("variable %s has been deleted", variable_id), level = 3)
    }
  }


gtm_variables_revert <- function(account_id, container_id, workspace_id, variable_id) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(variable_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Variable Id are all required for this function")
  }
  
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      variables = variable_id
    )
    res <- gtm_action(path_args = path_args, action = "revert")
    myMessage(sprintf("Changes to variable %s (%s) have been reverted", res$variable$name, res$zone$variableId), level = 3)
    return(res$variable)
  }

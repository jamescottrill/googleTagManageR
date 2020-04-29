#' List all variables in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables/list}
#' @family variable structure functions
#' 
#' @description
#'
#' This will return a data frame all your available variables in a given workspace
#' If you want to get the information for a single variable, use \code{gtm_variables_get}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' 
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

#' List all variables in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables/get}
#' @family variable structure functions
#' 
#' @description
#'
#' This will return a list with the settings of a single variable.
#' If you want to get the information for all variables, use \code{gtm_variables_list}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable_id Variable Id
#' 
#' @export
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


#' Create a new variable
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables/create}
#' @family variable structure functions
#' 
#' @description
#'
#' This takes a variable resource and creates a new variable in GTM.
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable_object Variable Object
#' 
#' @export
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

#' Update an existing variable
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables/update}
#' @family variable structure functions
#' 
#' @description
#'
#' This takes a variable resource and overwrites the existing variable in GTM
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable_id Variable Id
#' @param variable_object Variable Object
#' 
#' @export
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

#' Delete a variable
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables/delete}
#' @family variable structure functions
#' @importFrom utils menu
#' 
#' @description
#'
#' This deletes a variable in a GTM workspace
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable_id Variable Id
#' @param force Force deletion without user input
#' 
#' @export
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

#' Reverts a variable to its original state
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables/revert}
#' @family variable structure functions
#' 
#' @description
#'
#' This reverts a variable back to its original, unmodified state.
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable_id Variable Id
#' 
#' @export
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

#' List all variables in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables/list}
#' @family variable functions
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
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' 
#' variables <- gtm_variables_list(accountId, containerId, workspaceId)
#' 
#' }
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
#' @family variable functions
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
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' variableId = 100
#' 
#' variable <- gtm_variables_get(accountId, containerId, workspaceId, variableId)
#' 
#' }
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
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/variables#resource-representations}
#' @family variable functions
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
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 3
#' 
#' variable <- list(
#'   name = 'Custom JS Variable',
#'   type = 'jsm',
#'   parameter = list(
#'     list(
#'       type = 'template',
#'       key = 'javascript',
#'       value = 'function(){\n  return document.querySelector(\"form\").id;\n}'
#'     )
#'   )
#' )
#' 
#' cjsVariable <- gtm_variables_create(accountId, containerId, workspaceId, variable)
#' 
#' variable2 <- list(
#' name = 'Lookup Table',
#' type = 'smm',
#' parameter = list(
#'   list(
#'         type = 'boolean',
#'         key = 'setDefaultValue',
#'         value = 'false'
#'     ),
#'     list(
#'         type = 'template',
#'         key = 'input',
#'         value = '{{Page Hostname}}'
#'     ),
#'     list(
#'         type = 'list',
#'         key = 'map',
#'         list = list(
#'             list(
#'             type = 'map',
#'             map = list(list(
#'                 type = 'template',
#'                 key = 'key',
#'                 value = 'google.co.uk'
#'             ),
#'             list(
#'                 type = 'template',
#'                 key = 'value',
#'                 value = 'UA-123456-1'
#'             )
#'        )
#'     ),
#'     list(
#'             type = 'map',
#'             map = list(list(
#'                 type = 'template',
#'                 key = 'key',
#'                 value = 'bing.com'
#'             ),
#'             list(
#'                 type = 'template',
#'                 key = 'value',
#'                 value = 'UA-123456-2'
#'             )
#'        )
#'     )
#'     )
#'    )
#'   )
#'  )
#' 
#' lookupTable <- gtm_variables_create(accountId, containerId, workspaceId, variable2)
#' 
#' }
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
#' @family variable functions
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
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 3
#' 
#' 
#' updatedVariable <- list(
#' name = 'Lookup Table',
#' type = 'smm',
#' parameter = list(
#'   list(
#'         type = 'boolean',
#'         key = 'setDefaultValue',
#'         value = 'false'
#'     ),
#'     list(
#'         type = 'template',
#'         key = 'input',
#'         value = '{{Page Hostname}}'
#'     ),
#'     list(
#'         type = 'list',
#'         key = 'map',
#'         list = list(
#'             list(
#'             type = 'map',
#'             map = list(list(
#'                 type = 'template',
#'                 key = 'key',
#'                 value = 'google.co.uk'
#'             ),
#'             list(
#'                 type = 'template',
#'                 key = 'value',
#'                 value = 'UA-123456-1'
#'             )
#'        )
#'     ),
#'     list(
#'             type = 'map',
#'             map = list(list(
#'                 type = 'template',
#'                 key = 'key',
#'                 value = 'bing.com'
#'             ),
#'             list(
#'                 type = 'template',
#'                 key = 'value',
#'                 value = 'UA-123456-2'
#'             )
#'        )
#'     ),
#'     list(
#'             type = 'map',
#'             map = list(list(
#'                 type = 'template',
#'                 key = 'key',
#'                 value = 'yahoo.com'
#'             ),
#'             list(
#'                 type = 'template',
#'                 key = 'value',
#'                 value = 'UA-123456-3'
#'             )
#'        )
#'     )
#'     )
#'    )
#'   )
#'  )
#' 
#' newLookupTable <- gtm_variables_create(accountId, containerId, workspaceId, updatedVariable)
#' 
#' }
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
#' @family variable functions
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
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' variableId <- 30
#' 
#' gtm_variables_delete(accountId, containerId, workspaceId, variableId)
#' 
#' # This will delete tag %s. Are you sure you want to continue?
#' 
#' #  1: Yes
#' #  2: No
#'
#' # Selection: 1
#' 
#' # Variable 30 has been deleted.
#' 
#' variableId = 31
#' 
#' gtm_variables_delete(accountId, containerId, workspaceId, variableId, "TRUE")
#' 
#' # Variable 31 has been deleted.
#' 
#' }
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
#' @family variable functions
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
#' @examples
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' variableId <- 102
#' 
#' variable <- gtm_variables_revert(accountId, containerId, workspaceId, variableId)
#' 
#' # Changes to variable 22 have been reverted
#' }
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

#' List all zones in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/zones/list}
#' @family zone functions
#' 
#' @description
#'
#' This will return a data frame all your available zones in a given workspace
#' If you want to get the information for a single zone, use \code{gtm_zones_get}
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
#' workspaceId <- 11
#' 
#' zones <- gtm_zones_list(accountId, containerId, workspaceId)
#' }
#' 
#' @export
gtm_zones_list <- function(account_id, container_id, workspace_id) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function")
  }
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    zones = ""
  )
  res <- gtm_list(path_args = path_args, type = 'zone')
  return(res)
}

#' List a single zone in a workspace
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/zones/get}
#' @family zone functions
#' 
#' @description
#'
#' This will return a list with the settings of a single zone.
#' If you want to get the information for all zones, use \code{gtm_zones_list}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param zone_id Zone Id
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 11
#' zoneId <- 12
#' 
#' zone <- gtm_zones_get(accountId, containerId, workspaceId, zoneId)
#' }
#' 
#' @export
gtm_zones_get <- function(account_id,container_id,workspace_id,zone_id) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(zone_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Zone Id are all required for this function")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    zones = zone_id
  )
  
  res <- gtm_get(path_args = path_args)
  return(res)
}


#' Create a new zone
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/zones/create}
#' @family zone functions
#' 
#' @description
#'
#' This takes a zone resource and creates a new zone in GTM.
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param zone_object Zone Object
#' 
#' @examples 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' 
#' 
#'  
#' zone <- list(
#'     accountId=accountId,
#'     containerId=containerId,
#'     workspaceId=workspaceId,
#'     name="Marketing Zone",
#'     childContainer=list(
#'       list(
#'         publicId="GTM-ABCDEF",
#'         nickname="My Marketing Container"
#'      )
#'    )
#' )
#'  
#' new_zone <- gtm_zones_create(accountId, containerId, workspaceId, zone)
#'  
#' }
#' @export
gtm_zones_create <- function(account_id,container_id,workspace_id,zone_object) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function")
  }
  
  if (missing(zone_object)) stop("A Zone object is required for this function")
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    zones = ""
  )
  res <- gtm_create(path_args = path_args, body = zone_object)
  myMessage(sprintf("Zone %s (%s) has been created",res$name,res$zoneId),level=3)
  return(res)
}

#' Update an existing zone
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/zones/update}
#' @family zone functions
#' 
#' @description
#'
#' This takes a zone resource and overwrites the existing zone in GTM
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param zone_id Zone Id
#' @param zone_object Zone Object
#' 
#' @export
gtm_zones_update <- function(account_id,container_id, workspace_id,zone_id,zone_object) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(zone_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Zone Id are all required for this function")
  }
  
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      zones = zone_id
    )
    res <- gtm_update(path_args = path_args, body = zone_object)
    myMessage(sprintf("Zone %s (%s) has been updated", res$name, res$zoneId), level = 3)
    return(res)
  }

#' Delete a zone
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/zones/delete}
#' @family zone functions
#' @importFrom utils menu
#' 
#' @description
#'
#' This deletes a zone in a GTM workspace
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param zone_id Zone Id
#' @param force Force deletion without user input
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 11
#' zoneId <- 3
#' 
#' gtm_zones_delete(accountId, containerId, workspaceId, zoneId, "TRUE")
#' 
#' }
#' 
#' @export
gtm_zones_delete <- function(account_id, container_id, workspace_id, zone_id, force = c("TRUE","FALSE")) {

  if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(zone_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Zone Id are all required for this function")
  }
    
    path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      zones = zone_id
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
             title = sprintf("This will delete Zone %s. Are you sure you want to continue?", zone_id)),
        {
          res <- gtm_delete(path_args = path_args)
        },
        {
          cancelled()
        }
      )
    }
    if (length(res) == 0) {
      myMessage(sprintf("Zone %s has been deleted", zone_id), level = 3)
    }
    return()
  }

#' Reverts a zone to its original state
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/zones/revert}
#' @family zone functions
#' 
#' @description
#'
#' This reverts a zone back to its original, unmodified state.
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param zone_id Zone Id
#' 
#' @examples 
#' 
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 11
#' zoneId <- 5
#' 
#' zone <- gtm_zones_revert(accountId, containerId, workspaceId, zoneId)
#' }
#' 
#' @export
gtm_zones_revert <- function(account_id, container_id, workspace_id,zone_id) {
  
  if(any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(zone_id)
  )) {
    stop("Account Id, Container Id, Workspace Id and Zone Id are all required for this function")
  }
    
  path_args <- list(
      accounts = account_id,
      containers = container_id,
      workspaces = workspace_id,
      zones = zone_id
    )
    res <- gtm_action(path_args = path_args, action = "revert")
    myMessage(sprintf("Changes to zone %s (%s) have been reverted", res$zone$name, res$zone$zoneId), level = 3)
    return(res$zone)
  }

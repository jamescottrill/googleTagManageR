#' Manage Zones in GTM
#'
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/zones
#' @family zone functions
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

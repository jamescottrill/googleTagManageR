#' List all containers in an account
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/list}
#' @family container functions
#' 
#' @param account_id Account Id
#'
#' @description
#'
#' This returns a dataframe containing all the containers in an account
#' If you want to get the information for a single container, use \code{gtm_containers_get}
#' 
#' @export
gtm_containers_list <- function(account_id) {
  
  path_args <- list(
    accounts = account_id,
    containers = ""
    )
  
  res <- gtm_list(path_args = path_args, type = "container")
  return(res)
}

#' Gets the metadata for a single container in GTM
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/get}
#' @family container functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#'
#' @description
#'
#' This returns a list containing all the metadata about a single container.
#' If you want to get the information for all containers, use \code{gtm_containers_list}
#' 
#'  @export
gtm_containers_get <- function(account_id, container_id) {
  
  path_args <- list(
    accounts = account_id,
    containers = container_id
    )
  
  res <- gtm_get(path_args = path_args)
  return(res)
}

#' Creates a new container in an acount
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/create}
#' @family container functions
#' 
#' @param account_id Account Id
#' @param name Container Name
#' @param type Container Use Type
#' @param domain_name List of domain names associated with the Container.
#' @param notes Container Notes.
#'
#' @description
#'
#' This creates a new container in the specified account.
#' 
#'  @export
gtm_containers_create <- function(account_id, 
                                  name, 
                                  type = c('web', 'iosSdk5', 'androidSdk5', 'amp'), 
                                  domain_name = NULL, 
                                  notes = NULL) {
    if (any(missing(account_id),
            missing(name),
            missing(type)
            )) {
      stop("Account Id, Name and Container type are required for container creation")
    }
    
    account_id <- as.character(account_id)
    type <- match.arg(type)
    
    path_args <- list(
      accounts = account_id,
      containers = ''
      )
    
    body <- list(
      name = name,
      usageContext = list(type),
      domain_name = domain_name,
      notes = notes
      )
    
   res <- gtm_create(path_args = path_args, body = body)
    
   myMessage(sprintf("Container %s (%s) created. The public container id is %s", res$name, res$containerId, res$publicId), level = 3)
   
   return(res)
  }

#' Updates a GTM Container
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/update}
#' @family container functions
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param name Container Name
#' @param type Container Use Type
#' @param domain_name List of domain names associated with the Container.
#' @param notes Container Notes.
#'
#' @description
#'
#' This updates a GTM Container.
#' Although you can't change the type of container, it is still required for this function to work.
#' 
#'  @export
gtm_containers_update <-function(account_id,
                                 container_id,
                                 name,
                                 type = c("amp", "androidSdk5", "iosSdk5", "web"),
                                 domain_name = "",
                                 notes = "") {
    if (missing(type)) {
      stop("Container type is required to update the container. You can't change the container type, but you need to include it anyway so go figure...")
    }
  
    if (any(missing(account_id),
            missing(container_id)
    )) {
      stop(
        "Account Id and Contaienr Id are required for this function"
      )
    }
    
    type <- match.arg(type)
    path_args <- list(
      accounts = account_id,
      containers = container_id
      )
    
    body <- list(
      name = name,
      typeContext = list(type)
      )
    
    res <- gtm_update(path_args = path_args, body = body)
    myMessage(sprintf("Container %s (%s) has been updated", res$name, res$id),
              level = 3)
    return(res)
  }

#' Deletes an existing GTM Container.
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/delete}
#' @family container functions
#' @importFrom utils menu
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param force Force deletion without user input
#'
#' @description
#'
#' This deletes a GTM container from an account. This is an irreversible process, so it's recommended that you first back up the container.
#' 
#'  @export
gtm_containers_delete <- function(account_id, container_id, force = c("TRUE","FALSE")) {
    
  if (any(missing(account_id),
           missing(container_id)
    )) {
    stop("Account and Container Ids are required for this function")
  } 
  
  path_args <- list(
    accounts = account_id,
    containers = container_id
  )
  
  if (missing(force)) force <- "FALSE"
  force <- as.character(force)
  force <- match.arg(force)
  force <- as.logical(force)
    
    if (force) {
      res <- gtm_delete(path_args = path_args)
      if (length(res) == 0) {
        myMessage(sprintf("Container %s has been deleted", container_id), level = 3)
        }
      } else {
        switch(
          menu(c("Yes", "No"),
               title = paste0("!!WARNING!! This command will delete your container.\n",
                              "This operation is irrevocable.\n",
                              "It is strongly recommended that you create an export of your container before you delete it, just in case you ever want it again.\n",
                              "Are you sure you want to continue?")), {
                 switch(menu(c("Yes", "No"), title = "Are You really sure you want to delete this container?"), {
                   res <- gtm_delete(path_args = path_args)
                   if (length(res) == 0) {
                     myMessage(sprintf("Container %s has been deleted", container_id), level = 3)
                     }
                   }, {
                     cancelled()
                     } 
                   )
                 }, {
                   cancelled()
                   } 
        )
      }
  }


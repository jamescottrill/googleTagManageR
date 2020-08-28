#' List all accounts a user has accesss to 
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/list}
#' @family account functions
#' 
#' @description
#'
#' This will return a data frame all your available accounts, returning the path, account Id and account name.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' accounts <- gtm_accounts_list()
#' 
#' }
#' 
#' @export
gtm_accounts_list <- function() {
  path_args <- list(
    accounts = ""
  )
  res <- gtm_list(path_args = path_args, type = "account")
  return(res)
}

#' List account metadata
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/get}
#' @family account functions
#' 
#' @param account_id Account Id
#'
#' @description
#'
#' This returns a list containing the metadata about a single account.
#' If you want to get the information for all accounts, use \code{gtm_accounts_list}
#' 
#' @examples 
#' \dontrun{
#' 
#' account_id <- 12345678
#' account <- gtm_accounts_get(account_id)
#'
#' }
#' 
#' @export
gtm_accounts_get <- function(account_id) {
  path_args <- list(
    accounts = account_id
  )
  res <- gtm_get(path_args = path_args)
  return(res)
}

#' Updates account metadata
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/update}
#' @family account functions
#' 
#' @param account_id Account Id
#' @param name Account Display Name
#' @param shareData Whether the account shares data anonymously with Google and others. This flag enables benchmarking by sharing your data in an anonymous form. Google will remove all identifiable information about your website, combine the data with hundreds of other anonymous sites and report aggregate trends in the benchmarking service.
#'
#' @description
#'
#' This enables you to update the account metadata. You can update the account name and whether or not data is shared with Google.
#' 
#' @examples 
#' \dontrun{
#' 
#' account_id <- 12345678
#' new_account <- gtm_accounts_update(account_id, name = "New Container Name")
#'
#' }
#' @export
gtm_accounts_update <- function(account_id, name = NULL, shareData=c("TRUE","FALSE")) {
  
  if (missing(shareData)) {  
    shareData <- NULL
  } else {
    shareData <- as.character(shareData)
    shareData <- match.arg(shareData)
  }
  
  name <- as.character(name)
  path_args <- list(
    accounts = account_id
  )
  
  body <- list(
    name = name,
    shareData = shareData
    )
  
  res <- gtm_update(path_args = path_args, body = body)
  
  myMessage(sprintf("Account %s updated",res$name),level=3)
  return(res)
}

#' See all the accounts that you have access to in GTM, view specific accounts within GTM and update account
#' details
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts
#' @family account functions
#' @export


gtm_accounts_list <- function() {
  path_args <- list(
    accounts = ""
  )
  res <- gtm_list(path_args = path_args, type = "account")
  return(res)
}

gtm_accounts_get <- function(account_id) {
  path_args <- list(
    accounts = account_id
  )
  res <- gtm_get(path_args = path_args)
  return(res)
}

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

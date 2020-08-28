#' List all built-in variables
#' 
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/built_in_variables/list}
#' @family built in variable functions
#' 
#' @description
#'
#' This will return a data frame all enabled built-in variables in a given workspace
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' 
#' @examples 
#' \dontrun{
#' 
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' 
#' builtinVaribles<- gtm_builtin_list(accountId, containerId, workspaceId)
#' 
#' }
#' 
#' @export
gtm_builtin_list <- function(account_id, container_id, workspace_id) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id)
  )) {
    stop("Account Id, Container Id and Workspace Id are all required for this function")
  }
  
  path_args <- list(
    accounts =account_id,
    containers = container_id,
    workspaces = workspace_id,
    built_in_variables = ''
  )
  
  variables <- gtm_list(path_args = path_args, type = 'builtInVariable')
  return(variables)
}

#' Enable a built-in variable
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/built_in_variables/create}
#' @family built in variable functions
#' 
#' @description
#'
#' This enables one of the built-in variables in GTM.  It is the inverse of \code{\link{gtm_builtin_delete}}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable A Builtin Variable or list of built in variables. Valid values are here \code{\link{variables_list}}
#' 
#' @examples 
#' \dontrun{
#' 
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' variable <- 'clickId'
#' 
#' builtinVaribles<- gtm_builtin_create(accountId, containerId, workspaceId, variable)
#' 
#' listOfVaribales <- c('clickClasses', 'clickElement')
#' 
#' newEnabledVars <- gtm_builtin_create(accountId, containerId, workspaceId, listOfVaribales)
#' 
#' }
#' 
#' 
#' @export
gtm_builtin_create <- function(account_id, container_id, workspace_id, variable){
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(variable)
  )) {
    stop("Account Id, Container Id, Workspace Id and the variable you want to enable are all required for this function")
  }

  if (!typeof(variable)=='list') variable <- as.list(variable) 
  
  if(!all(variable %in% .builtin_variables)){
    stop("One or More of your variables is invalid. Please ensure that all variables are valid. Chek the Manual page (variable_list) for the full list of accepted variables")
  }
  
  path_args <- list(
    accounts=account_id,
    containers = container_id,
    workspaces = workspace_id,
    built_in_variables = ''
  )
  
  pars_args <- convert('type', variable)
  
  res <- gtm_create(path_args = path_args, pars_args = pars_args)
  myMessage(sprintf("The Variable '%s' has been enabled in workspace %s",res$name,res$workspaceId),level=3)
  return(res)
}

#' Disable a built-in variable
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/built_in_variables/delete}
#' @family built in variable functions
#' 
#' @description
#'
#' This disables one or more of the built-in variables in GTM. It is the inverse of \code{\link{gtm_builtin_create}}
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable A built-in Variable or list of built-in variables. Valid values are here \code{\link{variables_list}}
#' 
#' @examples 
#' \dontrun{
#' 
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' variable <- 'clickId'
#' 
#' builtinVaribles<- gtm_builtin_delete(accountId, containerId, workspaceId, variable)
#' 
#' listOfVaribales <- c('clickClasses', 'clickElement')
#' 
#' newEnabledVars <- gtm_builtin_delete(accountId, containerId, workspaceId, listOfVaribales)
#' 
#' }
#' 
#' @export
gtm_builtin_delete <- function(account_id, container_id, workspace_id, variable) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(variable)
  )) {
    stop("Account Id, Container Id, Workspace Id and the variable you want to disable are all required for this function")
  }
  
  if (!typeof(variable)=='list') variable <- as.list(variable) 
  
  if(!all(variable %in% .builtin_variables)){
    stop("One or More of your variables is invalid. Please ensure that all variables are valid. Chek the Manual page (variable_list) for the full list of accepted variables")
  }
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    built_in_variables = ''
  )
  
  pars_args <- convert('type', variable)
  
  res <- gtm_delete(path_args = path_args, pars_args = pars_args)
  myMessage(sprintf("The Variable '%s' has been disabled in workspace %s", res$name, res$workspaceId), level=3)
  invisible(res)
}


#' Reverts changes to a GTM Built-In Variables in a GTM Workspace
#'
#' @seealso \url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/built_in_variables/revert}
#' @family built in variable functions
#' 
#' @description
#'
#' This reeverts any changes to a GTM Built-In Variables in a GTM Workspace.
#' 
#' @param account_id Account Id
#' @param container_id Container Id
#' @param workspace_id Workspace Id
#' @param variable A built-in Variable or list of built-in variables. Valid values are here \code{\link{variables_list}}
#' 
#' @examples
#' \dontrun{
#' accountId <- 1234567
#' containerId <- 7654321
#' workspaceId <- 10
#' variable <- 'clickId'
#' 
#' variable <- gtm_variables_revert(accountId, containerId, workspaceId, variable)
#' 
#' # Changes to variable 22 have been reverted
#' }
#' 
#' @export
gtm_builtin_revert <- function(account_id,container_id,workspace_id,variable) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(variable)
  )) {
    stop("Account Id, Container Id, Workspace Id and the variable you want to revert are all required for this function")
  }
  
  if (!typeof(variable)=='list') variable <- as.list(variable) 
  
  if(!all(variable %in% .builtin_variables)){
    stop("One or More of your variables is invalid. Please ensure that all variables are valid. Chek the Manual page (variable_list) for the full list of accepted variables")
  }

  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = paste0(workspace_id, '/built_in_variables')
  )
  
  # pars_args <- convert('type', variable)
  for (el in variable){
    tryCatch({
      res <- gtm_action(path_args = path_args, pars_args = list(type = el), action = "revert")
      myMessage(sprintf("The Variable '%s' has been reverted.", el), level=3)
    },
    error=function(err){
      handle_error(err)
      myMessage(sprintf("The Variable '%s' has not been reverted.", el), level=3)
    }
    )
  }
  return()
}


#' Built-In Variable Keys
#' 
#' @family built in variable functions
#' @name variables_list
#' 
#' @param advertiserId Advertiser Id - Firebase
#' @param advertisingTrackingEnabled Advertiser Tracking Enabled - Firebase
#' @param ampBrowserLanguage Browser Language - AMP
#' @param ampCanonicalHost Cannonical Host - AMP
#' @param ampCanonicalPath Canonical Path - AMP
#' @param ampCanonicalUrl Canonical URL - AMP
#' @param ampClientId Page Client ID - AMP
#' @param ampClientMaxScrollX Scroll Width - AMP
#' @param ampClientMaxScrollY Scroll Height - AMP
#' @param ampClientScreenHeight Screen Height - AMP
#' @param ampClientScreenWidth Screen Width - AMP
#' @param ampClientScrollX Scroll Left - AMP
#' @param ampClientScrollY Scroll Top - AMP
#' @param ampClientTimestamp Client Timestamp - AMP
#' @param ampClientTimezone Client Timezone - AMP
#' @param ampGtmEvent AMP Event - AMP
#' @param ampPageDownloadTime Page Downlad Time - AMP
#' @param ampPageLoadTime Page Load Time - AMP
#' @param ampPageViewId Page View Id  - AMP
#' @param ampReferrer Document Referrer - AMP
#' @param ampTitle Document Title - AMP
#' @param ampTotalEngagedTime Total Engaged Time - AMP
#' @param appId App Id - Firebase
#' @param appName App Name- Firebase
#' @param appVersionCode App Version Code - Firebase
#' @param appVersionName App Version Name - Firebase
#' @param builtInVariableTypeUnspecified Undefined
#' @param clickClasses Click Classes
#' @param clickElement Click Element
#' @param clickId Click Id
#' @param clickTarget Click Target
#' @param clickText Click Text
#' @param clickUrl Click URL
#' @param containerId Container ID
#' @param containerVersion Container Version
#' @param debugMode Debug Mode
#' @param deviceName Device Name
#' @param elementVisibilityFirstTime Element First Visible
#' @param elementVisibilityRatio Percent Visible
#' @param elementVisibilityRecentTime Element Last Visible
#' @param elementVisibilityTime On Screen Duration
#' @param environmentName Environment Name
#' @param errorLine Error Line
#' @param errorMessage Error Message
#' @param errorUrl Error URL
#' @param event Event
#' @param eventName Event Name
#' @param firebaseEventParameterCampaign Campaign  Name - Firebase
#' @param firebaseEventParameterCampaignAclid Click Id (aclid) - Firebase
#' @param firebaseEventParameterCampaignAnid Ad Network ID (anid) - Firebase
#' @param firebaseEventParameterCampaignClickTimestamp Campaign Click Timestamp - Firebase
#' @param firebaseEventParameterCampaignContent Campaign Content - Firebase
#' @param firebaseEventParameterCampaignCp1 Campaign Custom Parameter 1 - Firebase
#' @param firebaseEventParameterCampaignGclid  Google Click Id (gclid) - Firebase
#' @param firebaseEventParameterCampaignSource Campaign Source - Firebase
#' @param firebaseEventParameterCampaignTerm Campaign Search Term  - Firebase
#' @param firebaseEventParameterCurrency In-app Purchase Currency - Firebase
#' @param firebaseEventParameterDynamicLinkAcceptTime Dynamic Link Accept Time - Firebase
#' @param firebaseEventParameterDynamicLinkLinkid Dynamic Link ID - Firebase
#' @param firebaseEventParameterNotificationMessageDeviceTime Notification Device Time - Firebase
#' @param firebaseEventParameterNotificationMessageId Notification Message Id - Firebase
#' @param firebaseEventParameterNotificationMessageName Notification Message Name - Firebase
#' @param firebaseEventParameterNotificationMessageTime Notification Message Time - Firebase
#' @param firebaseEventParameterNotificationTopic Notification Message Topic - Firebase
#' @param firebaseEventParameterPreviousAppVersion Previous Application Version - Firebase
#' @param firebaseEventParameterPreviousOsVersion Previous OS Version - Firebase
#' @param firebaseEventParameterPrice In-app Purchase Price - Firebase
#' @param firebaseEventParameterProductId In-app Purchase Product ID - Firebase
#' @param firebaseEventParameterQuantity In-app Purchase Quantity - Firebase
#' @param firebaseEventParameterValue In-app Purchase Value - Firebase
#' @param formClasses Form Classes
#' @param formElement Form Element
#' @param formId Form Id
#' @param formTarget Form Target
#' @param formText Form Text
#' @param formUrl Form URL
#' @param historySource History Source
#' @param htmlId HTML ID
#' @param language Language
#' @param newHistoryFragment New History Fragment
#' @param newHistoryState New History State
#' @param newHistoryUrl New History URL
#' @param oldHistoryFragment Old History Fragment
#' @param oldHistoryState Old History Stat
#' @param oldHistoryUrl Old History URL
#' @param osVersion OS Version
#' @param pageHostname Page Hostname
#' @param pagePath Page Path
#' @param pageUrl Page URL
#' @param platform Platform
#' @param randomNumber Random Number
#' @param referrer Referrer
#' @param resolution Screen Resolution
#' @param scrollDepthDirection Scroll Depth Direction
#' @param scrollDepthThreshold Scroll Depth Threshold
#' @param scrollDepthUnits Scroll Depth Units
#' @param sdkVersion SDK Version - Firebase
#' @param videoCurrentTime Video Current Time
#' @param videoDuration Video Duration
#' @param videoPercent Video Percent
#' @param videoProvider Video Provider
#' @param videoStatus Video Status
#' @param videoTitle Video Title
#' @param videoUrl Video URL
#' @param videoVisible Video Visible
#' 
#' @description 
#' These are the variable names accepted when enabling/disabling Built-In Variables
NULL

#' Built-in Variables
#' 
#' @description 
#' A list of built in variables for comparison
#' 
#' @noRd
.builtin_variables<-c("advertiserId",
                     "advertisingTrackingEnabled",
                     "ampBrowserLanguage",
                     "ampCanonicalHost",
                     "ampCanonicalPath",
                     "ampCanonicalUrl",
                     "ampClientId",
                     "ampClientMaxScrollX",
                     "ampClientMaxScrollY",
                     "ampClientScreenHeight",
                     "ampClientScreenWidth",
                     "ampClientScrollX",
                     "ampClientScrollY",
                     "ampClientTimestamp",
                     "ampClientTimezone",
                     "ampGtmEvent",
                     "ampPageDownloadTime",
                     "ampPageLoadTime",
                     "ampPageViewId",
                     "ampReferrer",
                     "ampTitle",
                     "ampTotalEngagedTime",
                     "appId",
                     "appName",
                     "appVersionCode",
                     "appVersionName",
                     "builtInVariableTypeUnspecified",
                     "clickClasses",
                     "clickElement",
                     "clickId",
                     "clickTarget",
                     "clickText",
                     "clickUrl",
                     "containerId",
                     "containerVersion",
                     "debugMode",
                     "deviceName",
                     "elementVisibilityFirstTime",
                     "elementVisibilityRatio",
                     "elementVisibilityRecentTime",
                     "elementVisibilityTime",
                     "environmentName",
                     "errorLine",
                     "errorMessage",
                     "errorUrl",
                     "event",
                     "eventName",
                     "firebaseEventParameterCampaign",
                     "firebaseEventParameterCampaignAclid",
                     "firebaseEventParameterCampaignAnid",
                     "firebaseEventParameterCampaignClickTimestamp",
                     "firebaseEventParameterCampaignContent",
                     "firebaseEventParameterCampaignCp1",
                     "firebaseEventParameterCampaignGclid",
                     "firebaseEventParameterCampaignSource",
                     "firebaseEventParameterCampaignTerm",
                     "firebaseEventParameterCurrency",
                     "firebaseEventParameterDynamicLinkAcceptTime",
                     "firebaseEventParameterDynamicLinkLinkid",
                     "firebaseEventParameterNotificationMessageDeviceTime",
                     "firebaseEventParameterNotificationMessageId",
                     "firebaseEventParameterNotificationMessageName",
                     "firebaseEventParameterNotificationMessageTime",
                     "firebaseEventParameterNotificationTopic",
                     "firebaseEventParameterPreviousAppVersion",
                     "firebaseEventParameterPreviousOsVersion",
                     "firebaseEventParameterPrice",
                     "firebaseEventParameterProductId",
                     "firebaseEventParameterQuantity",
                     "firebaseEventParameterValue",
                     "formClasses",
                     "formElement",
                     "formId",
                     "formTarget",
                     "formText",
                     "formUrl",
                     "historySource",
                     "htmlId",
                     "language",
                     "newHistoryFragment",
                     "newHistoryState",
                     "newHistoryUrl",
                     "oldHistoryFragment",
                     "oldHistoryState",
                     "oldHistoryUrl",
                     "osVersion",
                     "pageHostname",
                     "pagePath",
                     "pageUrl",
                     "platform",
                     "randomNumber",
                     "referrer",
                     "resolution",
                     "scrollDepthDirection",
                     "scrollDepthThreshold",
                     "scrollDepthUnits",
                     "sdkVersion",
                     "videoCurrentTime",
                     "videoDuration",
                     "videoPercent",
                     "videoProvider",
                     "videoStatus",
                     "videoTitle",
                     "videoUrl",
                     "videoVisible")


#' Manage Built-In variables within GTM
#' @seealso https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/built_in_variables
#' @family account structure functions
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


gtm_builtin_create <- function(account_id, container_id, workspace_id, variable = .builtin_variables){
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(variable)
  )) {
    stop("Account Id, Container Id, Workspace Id and the variable you want to enable are all required for this function")
  }

  if (!typeof(variable)=='list') variable <- list(variable)
  
  path_args <- list(
    accounts=account_id,
    containers = container_id,
    workspaces = workspace_id,
    built_in_variables = ''
  )
  
  pars_args <- list(
    type = variable
  )
  
  res <- gtm_create(path_args = path_args, pars_args = pars_args)
  myMessage(sprintf("The Variable '%s' has been enabled in workspace %s",res$name,res$workspaceId),level=3)
  return(res)
}

gtm_builtin_delete <- function(account_id, container_id, workspace_id, variable = .builtin_variables) {
  
  if (any(missing(account_id),
          missing(container_id),
          missing(workspace_id),
          missing(variable)
  )) {
    stop("Account Id, Container Id, Workspace Id and the variable you want to disable are all required for this function")
  }
  
  if (!typeof(variable)=='list') variable <- list(variable)
  
  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id,
    built_in_variables = ''
  )
  
  pars_args <- list(
    type = variable
  )
  
  res <- gtm_delete(path_args = path_args, pars_args = pars_args)
  myMessage(sprintf("The Variable '%s' has been deleted in workspace %s", res$name, res$workspaceId), level=3)
  return(res)
}

gtm_builtin_revert <- function(account_id,container_id,workspace_id,variable) {
  
  if (any(missing(account_id),
         missing(container_id),
         missing(workspace_id),
         missing(variable)
  )) {
    stop("Account Id, Container Id, Workspace Id and the variable you want to revert are all required for this function")
  }
  if (!typeof(variable)=='list') variable <- list(variable) 

  path_args <- list(
    accounts = account_id,
    containers = container_id,
    workspaces = workspace_id
  )
  
  pars_args <- list(
    type = variable
  )
  res <- gtm_action(path_args = path_args, pars_args = pars_args, action = "revert")
  myMessage(sprintf("The Variable '%s' has been reverted. Current status - Enabled: %s ", variable, res$enabled), level=3)
  return(res)
}

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


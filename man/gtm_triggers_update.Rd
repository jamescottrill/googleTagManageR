% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/triggers.R
\name{gtm_triggers_update}
\alias{gtm_triggers_update}
\title{Update an existing trigger}
\usage{
gtm_triggers_update(
  account_id,
  container_id,
  workspace_id,
  trigger_id,
  trigger_object
)
}
\arguments{
\item{account_id}{Account Id}

\item{container_id}{Container Id}

\item{workspace_id}{Workspace Id}

\item{trigger_id}{trigger Id}

\item{trigger_object}{trigger Object}
}
\description{
This takes a trigger resource and overwrites the existing trigger in GTM
}
\examples{

\dontrun{
accountId <- 1234567
containerId <- 7654321
workspaceId <- 3
triggerId <- 8


updatedTrigger <- list(
name = 'More Complex Trigger - Updated',
type = 'pageview',
filter = list(
  list(
    type = 'contains',
    parameter = list(
     list(
       type='template',
       key = 'arg0',
       value='{{Lookup Table}}'
     ),
     list(
       type='template',
       key = 'arg1',
       value='New Lookup Value'
     )
    )
   )
  )
 )

newTrigger2 <- gtm_triggers_update(accountId, containerId, workspaceId, triggerId, updatedTrigger)

}

}
\seealso{
\url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/workspaces/triggers/update}

Other trigger functions: 
\code{\link{gtm_triggers_create}()},
\code{\link{gtm_triggers_delete}()},
\code{\link{gtm_triggers_get}()},
\code{\link{gtm_triggers_list}()},
\code{\link{gtm_triggers_revert}()}
}
\concept{trigger functions}

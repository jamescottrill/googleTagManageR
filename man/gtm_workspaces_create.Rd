% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspaces.R
\name{gtm_workspaces_create}
\alias{gtm_workspaces_create}
\title{Create a new workspace}
\usage{
gtm_workspaces_create(account_id, container_id, name, description = NULL)
}
\arguments{
\item{account_id}{Account Id}

\item{container_id}{Container Id}

\item{name}{Workspace Name}

\item{description}{Workspace Description}
}
\description{
This creates a new workspace in an esisting container.
}
\examples{

\dontrun{
accountId <- 1234567
containerId <- 7654321
name <- 'New Analytics Tags'
description <- 'Adding element visibility tags'

workspace <- gtm_workspaces_create(accountId, containerId, name, description)

}

}
\seealso{
\url{https://developers.google.com/tag-platform/tag-manager/api/v2/reference/accounts/containers/workspaces/create}

Other workspace functions: 
\code{\link{gtm_workspaces_delete}()},
\code{\link{gtm_workspaces_get}()},
\code{\link{gtm_workspaces_list}()},
\code{\link{gtm_workspaces_preview}()},
\code{\link{gtm_workspaces_resolve}()},
\code{\link{gtm_workspaces_status}()},
\code{\link{gtm_workspaces_submit}()},
\code{\link{gtm_workspaces_sync}()},
\code{\link{gtm_workspaces_update}()}
}
\concept{workspace functions}

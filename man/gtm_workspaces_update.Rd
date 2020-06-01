% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspaces.R
\name{gtm_workspaces_update}
\alias{gtm_workspaces_update}
\title{Update a workspace}
\usage{
gtm_workspaces_update(
  account_id,
  container_id,
  workspace_id,
  name,
  description = NULL
)
}
\arguments{
\item{account_id}{Account Id}

\item{container_id}{Container Id}

\item{workspace_id}{Workspace Id}

\item{name}{Workspace Name}

\item{description}{Workspace Description}
}
\description{
This updates the name or description of an existing workspace.
}
\seealso{
\url{https://developers.google.com/tag-manager/api/v2/reference/accounts/containers/workspaces/update}

Other workspace functions: 
\code{\link{gtm_workspaces_create}()},
\code{\link{gtm_workspaces_delete}()},
\code{\link{gtm_workspaces_get}()},
\code{\link{gtm_workspaces_list}()},
\code{\link{gtm_workspaces_preview}()},
\code{\link{gtm_workspaces_resolve}()},
\code{\link{gtm_workspaces_status}()},
\code{\link{gtm_workspaces_submit}()},
\code{\link{gtm_workspaces_sync}()}
}
\concept{workspace functions}
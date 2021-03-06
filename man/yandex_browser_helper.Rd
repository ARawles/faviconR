% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{yandex_browser_helper}
\alias{yandex_browser_helper}
\title{Create Yandex favicon parameters}
\usage{
yandex_browser_helper(
  background_color = NULL,
  manifest = list(show_title = TRUE, version = 1, existing_manifest = NULL,
    error_on_override = FALSE)
)
}
\arguments{
\item{background_color}{character; hex color string - background of icon}

\item{manifest}{list; list of manifest parameters. Should contain 'show_title'; whether to show the title of the page
in the bookmark, 'version'; the manifest version (default 1), 'existing_manifest'; existing manigest can be provided
 here, 'error_on_override'; whether to raise an error if the manifests conflict}
}
\description{
This function helps with the creation of the list structure required to provide Yandex-specific favicon parameters
to the API.
Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
}

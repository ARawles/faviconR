% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{firefox_app_helper}
\alias{firefox_app_helper}
\title{Create Firefox favicon parameters}
\usage{
firefox_app_helper(
  picture_aspect = c("no_change", "circle", "rounded_square", "square"),
  background_colour = NULL,
  margin = NULL,
  keep_picture_in_circle = TRUE,
  circle_inner_margin = NULL,
  overlay = TRUE,
  manifest = list(app_name = NULL, app_description = NULL, developer_name = NULL,
    developer_url = NULL, existing_manifest = NULL, on_conflict = c("raise_error",
    "override", "keep_existing"))
)
}
\arguments{
\item{picture_aspect}{character; one of 'no_change', 'circle', 'rounded_square' or 'square'. If 'circle' is chosen, background_color,
margin, keep_picture_in_circle, circle_inner_margin and overlay parameters can all be used.}

\item{manifest}{list; list of manifest attributes. Should contain app_name; the app's name, app_description; a description of the app,
developer_name; the developer name, developer_url; the developer url, existing_manifest; an existing manifest file that can be passed,
and on_conflict; what to do if existing manifest conflicts with created manifest}
}
\description{
This function helps with the creation of the list structure required to provide Firefox-specific favicon parameters
to the API.
Further details can be found in the \href{https://realfavicongenerator.net/api/non_interactive_api}{realfavicongenerator.net API documentation}
}

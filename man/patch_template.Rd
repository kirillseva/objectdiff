% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/patch.R
\name{patch_template}
\alias{patch_template}
\title{Create a patch from environment injected objects and body.}
\usage{
patch_template(provides, body)
}
\arguments{
\item{provides}{list. Objects to inject into the
patch's environment.}
}
\value{
A bodiless patch with parent base environment.
}
\description{
Create a patch from environment injected objects and body.
}
\examples{
p <- objectdiff:::patch_template(list(a = 1), { a + object })
# function(object) { a + object }
# with environment containing a = 1
stopifnot(p(1) == 2)
}


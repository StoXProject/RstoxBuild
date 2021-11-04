#' Package for Building All Rstox Packages
#'
#' Building the Rstox packages (Rstox, RstoxFramework, RstoxData, RstoxFDA, RstoxSurveyPlanner, RstoxTempdoc, and even RstoxBuild), and semi-automated testing of Rstox though test projects.
#'
#' The package defines titles, descriptions, dependencies, authors, install instructions and other info for all the packages. All changes to authors, descriptions, suggests and other outputs of the function \code{packageSpecs} should be changed in this package, and not in the individual packages. The package also contains functionality for semi-automated testing of Rstox on a set of test projects.
#' @docType package
#' @name RstoxBuild
#'
"_PACKAGE"

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL


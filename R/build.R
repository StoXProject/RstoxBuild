########## Attempt to write a general build method which takes package name as input, and has different README, .onLoad, .onAttach and other stuff specified in separate functions for the available packages: ##########

# 0.	Rstox
# 1.	RstoxFramework
# 2.	RstoxData
# 3.	RstoxFDA
# 4.	RstoxSurveyPlanner
# 5a.	RstoxTempdoc
# 5b.	RstoxBuild


# Get paths to GitHub:
githubPaths <- function(element = c("github", "api")) {
	element <- match.arg(element)
	out <- list(
		github = "https://github.com", 
		api = "https://api.github.com/repos"
	)
	return(out[[element]])
}

getGithubURL <- function(packageName, accountName = "StoXProject") {
	paste(githubPaths("github"), accountName, packageName, sep = "/")
}

getGithubAPI <- function(packageName, accountName = "StoXProject") {
	paste(githubPaths("api"), accountName, packageName, "releases", sep = "/")
}


strwrap_file <- function(file, ...) {
    l <- readLines(file)
    l <- strwrap(l, ...)
    writeLines(l, file)
}

### # Function to get a list of release names of a package:
### getReleaseNames <- function(packageName, accountName = "StoXProject") {
### 	# Build the URL to list the releases:
### 	URL <- getGithubAPI(packageName = packageName, accountName = accountName)
### 	# Download the list of releases to a temporary file:
### 	tmp <- tempfile()
### 	utils::download.file(URL, tmp)
### 	json <- jsonlite::read_json(tmp)
### 	releases <- sapply(json, "[[", "tag_name")
### 	
### 	return(releases)
### }
### 
### # Get latest release version
### getLatestReleaseVersion <- function(packageName, accountName = "StoXProject", default = "0.0.1") {
### 	releaseNames <- getReleaseNames(
### 		packageName = packageName, 
### 		accountName = accountName
### 	)
### 	lastVersion <- sort(sub("^\\D+(\\d)", "\\1", releaseNames), decreasing = TRUE)[1]
### 	if(is.na(lastVersion)) {
### 		lastVersion <- default
### 	}
### 	return(lastVersion)
### }



### # Function to get the current version from the file RstoxVersions.R in the Rstox_utils repository:
### getCurrentVersion <- function(packageName) {
### 	
### 	versionFile <- "https://raw.githubusercontent.com/Sea2Data/Rstox_utils/master/Build/RstoxVersions.R"
### 	# Try sourcing the list of versions from GitHub:
### 	temp <- tryCatch(
### 		{
### 			source(versionFile)
### 			TRUE
### 		}, 
### 		error = function(err) {
### 			FALSE
### 		}
### 	)
### 	
### 	if(!temp) {
### 		warning("Internet connection failed, or for some other reason the file '", versionFile, "' could not be reached. Specify the ### version manually using 'version' in RstoxBuild::buildRstoxPackage().")
### 	}	
### 	
### 	version <- RstoxVersions[[packageName]]
### 	if(length(version) == 0) {
### 		stop("The package ", packageName, " is not present in the file ", versionFile, ".")
### 	}
### 	
### 	return(version)
### }



##############################################
##############################################
#' Function for building Rstox packages
#'
#' \code{buildRstoxPackage} is used in the continous development of Rstox, writing .onLoad, .onAttach, pkgname, DESCRIPTION and README files and adding dependencies to the NAMEPACE file.\cr \cr
#' \code{packageSpecs} gets all specifications of the package from separate funcitons for each package into a list.\cr \cr
#'
#' @param dir	The directory holding the package structure.
#' @param check	Logical: If TRUE run devtools::check() on the package.\code{\link[RstoxBase]{StratumArea}}
#' 
#' @export
#' @rdname buildRstoxPackage
# 
buildRstoxPackage <- function(
	packageName, 
	accountName = "StoXProject", 
	version = "current", 
	Rversion = "3.5", 
	imports = NULL, 
	suggests = NULL, 
	linkingto = NULL, 
	remotes = NULL, 
	importToNamespace = NULL, 
	internal.dependencies = NULL, 
	additional_repositories = NULL, 
	onCran = FALSE, 
	license = "LGPL-3", 
	rootDir = NULL, 
	title = NULL, 
	description = NULL, 
	details = NULL, 
	.onLoad = NULL, 
	.onUnload = NULL, 
	.onAttach = NULL, 
	misc = NULL, 
	authors = NULL, 
	check = FALSE, 
	noRcpp = FALSE, 
	addManual = FALSE, 
	addIndividualManuals = FALSE, 
	globalVariables = NULL, 
	type = c("patch", "minor", "major")
) {
	
    # Get the specifications of the package:
	spec <- packageSpecs(
		packageName = packageName, 
		accountName = accountName, 
		version = version, 
		Rversion = Rversion, 
		imports = imports, 
		suggests = suggests, 
		linkingto = linkingto, 
		remotes = remotes, 
		importToNamespace = importToNamespace, 
		internal.dependencies = internal.dependencies, 
		additional_repositories = additional_repositories, 
		onCran = onCran, 
		license = license, 
		rootDir = rootDir, 
		title = title, 
		description = description, 
		details = details, 
		.onLoad = .onLoad, 
		.onUnload = .onUnload, 
		.onAttach = .onAttach, 
		misc = misc, 
		authors = authors, 
		type = type
	)
	
	# Set the path to the package for usethis:
	usethis::proj_set(spec$dir)
	
	# If there is a src folder present, temporary rename it to src_ to document without, and then re-document with afterwards:
	if(spec$useCpp) {
		#file.rename(spec$src, spec$src_)
	}
	
	# Make sure the folder is recognised as a package with a NAMESPACE file:
	#usethis::create_package(spec$dir)
	NAMESPACEFile <- file.path(spec$dir, "NAMESPACE")
	if(!file.exists(NAMESPACEFile)) {
		cat("", file=NAMESPACEFile)
	}
	
	
	# Clear the installed package:
	try(lapply(.libPaths(), function(x) utils::remove.packages(spec$packageName, x)), silent = TRUE)
	
	##### Write the pkgname.R file: #####
	#pkgnameFile <- file.path(spec$dir, "R", "pkgname.R")
	pkgnameFile <- file.path(spec$dir, "R", paste0(packageName, "-package.R"))
	write(c(spec$pkgname, ""), pkgnameFile)
	##########
	
	##### Write the onAttach.R to the pkgname file: #####
	if(length(globalVariables)) {
	    globalVariablesText <- c(
	        "# Global variables", 
	        "utils::globalVariables(c(", 
	        paste(
	            "\t", 
	            strwrap(
	                paste0(
	                    paste0(
	                        "\"", sort(globalVariables), "\"", 
	                        collapse = ", "
	                    ), 
	                    "))"
	                ), 
	                100 - 4
	            )
	        )
	    )
	    write(c(globalVariablesText, ""), pkgnameFile, append = TRUE)
	}
	##########
	
	##### Write the onLoad.R to the pkgname file: #####
	if(length(spec$.onLoad)) {
	    write(c(spec$.onLoad, ""), pkgnameFile, append = TRUE)
	}
	##########
	
	##### Write the onLoad.R to the pkgname file: #####
	if(length(spec$.onUnload)) {
	    write(c(spec$.onUnload, ""), pkgnameFile, append = TRUE)
	}
	##########
	
	##### Write the onAttach.R to the pkgname file: #####
	if(length(spec$.onAttach)) {
		write(c(spec$.onAttach, ""), pkgnameFile, append = TRUE)
	}
	##########
	
	##### Write the importToNamespace to the to the pkgname file: #####
	if(length(spec$importToNamespace)) {
	    write(c(
	        "# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)", 
	        paste("#' @import", spec$importToNamespace), 
	        "NULL", 
	        ""
	   ), pkgnameFile, append = TRUE)
	}
	##########
	
	
	##### Write the DESCRIPTION file: #####
	DESCRIPTION <- getDESCRIPTION(spec)
	DESCRIPTIONFile <- file.path(spec$dir, "DESCRIPTION")
	write(DESCRIPTION, DESCRIPTIONFile)
	
	# Alter the DESCRIPTION file to contain the imports listed in the NAMESPACE file. This must take place AFTER creating the documentation:
	addImportsToDESCRIPTION(spec)
	##########
	

	##### Create documentation: #####
	# Remove current documentation and NAMESPACE, and then re-document without C++:
	unlink(file.path(spec$dir, "man"), recursive = TRUE, force = TRUE)
	unlink(NAMESPACEFile, force = TRUE)
	devtools::document(spec$dir)
	
	# Build individual function PDFs for use by the GUI:
	if(addIndividualManuals) {
		writeFunctionPDFs(spec$dir)
	}
	
	# Get and write an rds file with function argument descriptions for use by the GUI:
	getFunctionArgumentDescriptions(spec$dir)
	
	# Add linkedTo: Rcpp in the DESCRIPTION:
	if(spec$useCpp) {
	    if(!noRcpp) {
	        #usethis::use_rcpp()
			# Add the C++ specifics to the pkgnameFile:
			write(c(spec$Rcpp, ""), pkgnameFile, append = TRUE)
		}
		# Also delete shared objects for safety:
		sharedObjects <- list.files(spec$src, pattern = "\\.o|so$", full.names = TRUE)
		unlink(sharedObjects, recursive = TRUE, force = TRUE)
	}
	
	##### Run R cmd check with devtools: #####
	if(check) {
		devtools::check(pkg=spec$dir)
		# args = "--no-examples"
	}
	##########
	
	# 2020-11-06: We skip the automatic README, and keep this fixed instead.
	#### Generate the README file: ###
	#README <- getREADME(spec)
	#READMEFile <- file.path(spec$dir, "README")
	#write(README, READMEFile)
	###########
	
	##### Unload the package: #####
	# devtools::unload(spec$packageName)
	packageString <- paste("package", spec$packageName, sep=":")
	if(packageString %in% search()) {
		detach(packageString, unload = TRUE, character.only = TRUE)
	}
	##########
	
	##### Install the package: #####
	utils::install.packages(spec$dir, repos = NULL, type = "source", lib = .libPaths()[1])
	
	# Build and open documentation pdf:
	if(addManual) {
		pkg <- file.path(.libPaths()[1], spec$packageName)
		path <- file.path(pkg, "extdata", "manual")
		dir.create(path, recursive = TRUE)
		temp <- devtools::build_manual(pkg = pkg, path = path)
		
		# Open the PDF:
		pdfFile <- list.files(path, full.names = TRUE)[1]
		system(paste0("open \"", pdfFile, "\""))
	}
	##########
	
	# Load the package:
	library(spec$packageName, character.only = TRUE)
	
	#path
}
#' 
#' @export
#' @rdname buildRstoxPackage
# 
packageSpecs <- function(
	packageName, 
	accountName = "StoXProject", 
	version = "current", 
	Rversion = "3.5", 
	imports = NULL, 
	suggests = NULL, 
	linkingto = NULL, 
	remotes = NULL, 
	importToNamespace = importToNamespace, 
	internal.dependencies = NULL, 
	additional_repositories = NULL, 
	onCran = FALSE, 
	license = "LGPL-3", 
	rootDir = NULL, 
	title = NULL,
	description = NULL,
	details = NULL,
	authors = NULL,
	.onLoad = NULL,
	.onUnload = NULL,
	.onAttach = NULL,
	misc = NULL,
	type = c("patch", "minor", "major")
) {
	
	# If the packageName is a string with no slashes and does not exist as a directory, locate the directories of the developers defined in the 
	if(is.character(packageName) && !file.exists(packageName) && !grepl('\\\\|/', packageName)) {
		if(length(rootDir) == 0) {
			user <- Sys.info()["user"]
			rootDir <- data.table::fread(system.file("extdata", "rootDir.txt", package="RstoxBuild"), sep = ";")
			rootDir <- rootDir$rootDir[rootDir$user == user]
		}
		# The path to the package source code folder should contain a folder named by the package name, containing various optional files and folders like "test" or "temp", and a sub folder also named by the oackage name, which is the folder containing the package source code with DESCRIPTION, NAMESPACE, sub folder "R" etc.
		packageName <- file.path(rootDir, packageName, packageName)
	}
	
	# Extract the package name from the path to the package source code folder:
	dir <- packageName
	packageName <- basename(packageName)
	
	# Get version:
	if(identical(version, "current")) {
		#version <- getCurrentVersion(packageName)
		version <- incrementHighestRelease(packageName, type = type, sting.out = TRUE, accountName = accountName)
	}
	
	## Get NEWS file and NEWS;
	#NEWSfile <- file.path(dir, "NEWS")
	#if(startsWith(readLines(NEWSfile, 1), "#")) {
	#	NEWS <- getNews_old(NEWSfile, version = version)
	#}
	#else{
	#	NEWS <- getNews(NEWSfile, version = version)
	#}
	
	# Assure that imports, suggests and linkingto are named lists:
	imports <- asNamedList(imports)
	suggests <- asNamedList(suggests)
	linkingto <- asNamedList(linkingto)
	
	if(length(remotes) && length(internal.dependencies)) {
	    stop("remotes and internal.dependencies cannot be set at the same time. Use either remotes, which specifies to install from the GitHub account, or internal.dependencies, which uses the additional_repositories.")
	}
	
	# Get remotes version:
	remotes_versions <- NULL
	remotes_strings <- NULL
	if(length(remotes)) {
	    remotes_versions <- lapply(remotes, getHighestRelease)
	    names(remotes_versions) <- remotes
	    
	    # Add the remotes to the imports:
	    imports <- c(
	        imports, 
	        remotes_versions
	    )
	    
	    # Also get the remotes strings (for use in the Remotes field in DESCRIPTION):
	    remotes_strings <- paste0(
	        remotes, 
	        "@", 
	        getRstoxPackageVersionString(
	            packageName = remotes, 
	            version = unlist(remotes_versions)
	        )
	    )
	}
	
	# Get internal.dependencies version:
	internal.dependencies_versions <- NULL
	if(length(internal.dependencies) && length(additional_repositories)) {
	    internal.dependencies_versions <- lapply(internal.dependencies, getHighestRelease)
	    names(internal.dependencies_versions) <- internal.dependencies
	    ## Mark these as exact dependencies by setting them as list:
	    #internal.dependencies_versions <- lapply(internal.dependencies_versions, as.list)
	    
	    # Add the remotes to the imports:
	    imports <- c(
	        imports, 
	        internal.dependencies_versions
	    )
	}
	
	# Construct the output list:
	spec <- list(
		# Paths, names and dependencies:
		dir = dir, 
		packageName = packageName, 
		accountName = accountName, 
		version = version, 
		date = as.character(Sys.Date()), 
		Rversion = Rversion, 
		imports = imports, 
		suggests = suggests, 
		linkingto = linkingto, 
		remotes = remotes, 
		importToNamespace = importToNamespace, 
		additional_repositories = additional_repositories, 
		remotes_versions = remotes_versions, 
		remotes_strings = remotes_strings, 
		# Mandatory objects:
		title = title, 
		description = description, 
		details = details, 
		authors = authors, 
		# Optional objects:
		.onLoad = .onLoad, 
		.onUnload = .onUnload, 
		.onAttach = .onAttach, 
		misc = misc, 
		# Other specs:
		onCran = onCran, 
		license = license#, 
		#NEWSfile = NEWSfile, 
		#NEWS = NEWS
	)
	
	mandatory <- c("title", "description", "details", "authors")
	optional <- c(".onLoad", ".onUnload", ".onAttach", "misc")
	mandatoryORoptional <- c(mandatory, optional)
	
	# Get the missing package documentation objects from memory:
	spec[mandatoryORoptional] <- lapply(mandatoryORoptional, getPackageItem, spec=spec, packageName=packageName, version = version)
	
	# Check for the existence of the mandatory objects:
	empty <- lengths(spec[mandatory]) == 0
	if(any(empty)) {
		stop(paste("The following package documentation objects are mandatory. Add them as parameters or create them as objects named by e.g. title_Rstox as string vectors or functions returning a string vector: ", paste(mandatory[empty], collapse=", ")))
	}
	
	# Get the details shown using help(packageName):
	spec$pkgname <- getPkgname(spec)
	
	# Link to Rcpp:
	spec$useCpp <- FALSE
	spec$src <- file.path(spec$dir, "src")
	if(dir.exists(spec$src)) {
		if(length(list.files(spec$src))) {
			spec$useCpp <- TRUE
		}
	}
	#spec$src_ <- file.path(spec$dir, "src_")
	spec$Rcpp <- c(
	    "## usethis namespace: start", 
		paste0("#' @useDynLib ", spec$packageName, ", .registration = TRUE"), 
		"## usethis namespace: end", 
		"NULL", 
		"", 
		"## usethis namespace: start", 
		"#' @importFrom Rcpp sourceCpp", 
		"## usethis namespace: end", 
		"NULL"
	)
	
	return(spec)
}


##############################################
##############################################
#' Utility functions for getting and adding imports, DESCRIPTION, README and kgname to the package.
#'
#' \code{getDESCRIPTION} gets the DESCRIPTION text to write to the DESCRIPTION file, adding authors, R-dependency, title, description and other info stored in the input \code{spec}. Note that the package imports are not added here, but later in the \code{addImportsToDESCRIPTION} function.\cr \cr
#' \code{getREADME} gets the README file in the Rstox style, which has package and R version in the first two lines, followed by description, install instructions, miscellaneous info, and release notes retrieved from the NEWS file.\cr \cr
#' \code{addImportsToDESCRIPTION} adds the imports to the DESCRIPTION file.\cr \cr
#' \code{getPkgname} gets the pkgname text to write to the pkgname.R file.
#'
#' @param spec	A list of package specifications returned from \code{\link{packageSpecs}}.
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
getDESCRIPTION <- function(spec) {
	# Add R-dependency:
	if(length(spec$Rversion)) {
		Depends = paste0("R (>= ", spec$Rversion, ")")
	}
	else{
		Depends <- NULL
	}
	
	# Add package and bug reports URL:
	URL <- getGithubURL(
		packageName = spec$packageName, 
		accountName = spec$accountName
	)
	#URL = file.path(spec$githubRoot, spec$packageName)
	BugReports <- file.path(URL, "issues")
	
	# Paste and return the info:
	out <- list(
		"Package" = spec$packageName, 
		"Version" = spec$version, 
		"Date" = spec$date, 
		#"Title" = spec$title, 
		"Title" = paste(strwrap(spec$title, width = 80, exdent = 2), collapse = "\n"), 
		"Authors@R" = getAuthors(spec$authors), 
		"Depends" = Depends, 
		#"Description" = spec$description, 
		"Description" = paste(strwrap(spec$description, width = 80, exdent = 2), collapse = "\n"), 
		"URL" = URL, 
		"BugReports" = BugReports, 
		"License" = spec$license, 
		"LazyData" = "true", 
		"Encoding" = "UTF-8"
	)
	out <- paste(names(out), out, sep=": ", collapse="\n")
	# Add a new line ending the file:
	out <- paste0(out, "\n")
	
	return(out)
}
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
getREADME <- function(spec) {
	
	# Add devtools to the imports, since it is used for installing: THIS SEEMS WRONG
	#if(spec$onCran) {
	#	spec$imports <- c(spec$imports, "devtools")
	#}
	
	# Write package and R version in the first two lines. THIS SHOULD NEVER BE CHANGED, SINCE STOX READS THESE TWO LINES TO CHECK VERSIONS:
	header <- c(
		paste0("# ", spec$packageName, " version: ", spec$version, " (latest ", if(isMaster(version=spec$version)) "beta" else "alpha", ", ", format(Sys.time(), "%Y-%m-%d"), ")"), 
		paste0("# R version: ", spec$Rversion)
	)
	
	# If available instruct the user to install from CRAN:
	if(spec$onCran) {
		install <- c(
			paste0("# Install the ", spec$packageName, "package:"), 
			paste0("utils::install.packages(", spec$packageName, ")")
		)
	}
	# Install from GitHub, either with or without first installing dependencies:
	else{
		## Define install of dependencies:
		#if(length(spec$imports)) {
		#	install <- c(
		#		paste0("# Install the packages that ", spec$packageName, " depends on. Note that this updates all the specified packages to the late#st (binary) version:"), 
		#		paste0("dep.pck <- c(\"", paste0(spec$imports, collapse="\", \""), "\")"), 
		#		""
		#	)
		#}
		#else{
		#	install <- NULL
		#}
		
		## Add install of the package:
		#install <- c(
		#	install, 
		#	paste0("# Install ", spec$packageName, " from GitHub using the devtools package:"), 
		#	getGitHub_InstallPath(
		#		packageName = spec$packageName, 
		#		accountName = spec$accountName, 
		#		version = spec$version
		#	), 
		#	""
		#)
		
		# Add the alternative install:
	    install <- getGitHub_InstallPath(
			packageName = spec$packageName, 
			accountName = spec$accountName, 
			version = getRstoxPackageVersionString(
			    packageName = spec$packageName, 
			    version = spec$version
			), 
			additional_repositories = spec$additional_repositories
		)
	}
	
	releaseNotes <- c(
		paste0("# Release notes for ", spec$packageName, "_", spec$version, ":"), 
		spec$NEWS, 
		"", 
		paste0(
			"# For historical release notes see ", 
			getGitHub_NewsLink(
				packageName = spec$packageName, 
				accountName = spec$accountName, 
				version = spec$version
			)
		)
	)
	
	# Combine into a list and add space between each paragraph:
	README <- list(
		header,
		paste("#", spec$description), 
		paste("#", spec$details), 
		install, 
		spec$misc, 
		releaseNotes
	)
	README <- README[lengths(README) > 0]
	README <- lapply(README, append, rep("", 1))
	README <- unlist(README)
	
	return(README)
}
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
addImportsToDESCRIPTION <- function(spec, cpp=FALSE) {
	
	# Get the README and NEWS file paths:
	DESCRIPTIONFile <- file.path(spec$dir, "DESCRIPTION")
	
    # Add imports from 'spec':
	if(length(spec$imports)) {
		use_package_with_min_version(
			spec$imports, 
			type = "Imports"
		)
	}
	# Add also the suggests:
	if(length(spec$suggests)) {
		use_package_with_min_version(
			spec$suggests, 
			type = "Suggests"
		)
	}
	# Add also the linkingto:
	if(length(spec$linkingto)) {
		use_package_with_min_version(
			spec$linkingto, 
			type = "LinkingTo"
		)
		#lapply(spec$linkingto, usethis::use_package, type="LinkingTo")
	}
	# Add the remotes directly:
	if(length(spec$remotes)) {
		# Read the DESCRIPTION file:
		DESCRIPTION <- readLines(DESCRIPTIONFile)
		# Add remotes:
		DESCRIPTION <- c(
			DESCRIPTION, 
			"Remotes: ", 
			paste(
				paste0(
					"\t", 
					spec$accountName, 
					"/", 
					spec$remotes, 
					"@", 
					getRstoxPackageVersionString(
						packageName = spec$remotes, 
						version = unlist(spec$imports[spec$remotes])
				   )
		        ), 
			    collapse = ", \n"
			)
		)
		writeLines(DESCRIPTION, DESCRIPTIONFile)
	}
	# Add the additional_repositories directly:
	if(length(spec$additional_repositories)) {
	    # Read the DESCRIPTION file:
	    DESCRIPTION <- readLines(DESCRIPTIONFile)
	    # Add additional_repositories:
	    DESCRIPTION <- c(
	        DESCRIPTION, 
	        "Additional_repositories: ", 
	        paste0("\t", spec$additional_repositories)
	    )
	    writeLines(DESCRIPTION, DESCRIPTIONFile)
	}
	
	
	
	
	
	return(DESCRIPTIONFile)
}
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
getPkgname <- function(spec) {
	# Paste title, description, details by linespaces and start all lines with the roxygen line header. There is a line separating the title and description:
    out <- c(spec$title, spec$description, spec$details, spec$Rcpp)
	out <- paste0("#' ", out, collapse="\n#'\n")
	# Add the _PACKAGE line:
	out <- paste(
		out, 
		"#' @docType package", 
		paste("#' @name", spec$packageName), 
		"#'\n\"_PACKAGE\"", 
		sep="\n"
	)
	return(out)
}


##### Package specific functions: #####
##### Rstox: #####
title_Rstox <- function(version = "1.0") {
	"Running StoX Functionality Independently in R"
}

description_Rstox <- function(version = "1.0") {
	"R implementation of the functionality of the stock assesment utility StoX, which is an open source application fo acoustic-trawl and swept-area survey estimation."
}

details_Rstox <- function(version = "1.0") {
	c(
		"The core funciton of the package is \\code{\\link{getBaseline}} which runs a StoX project and retrieves the output and input parameters. The functions \\code{\\link{runBootstrap}} and \\code{\\link{imputeByAge}} are used by StoX for estimating the variance of the survey estimate. The functions \\code{\\link{getReports}} and \\code{\\link{getPlots}} are used to run report and plot funcitons relevant for the type of StoX project.",
		"Rstox supports a variety of other uses, such as downloading biotic and acoustic data from the Norwegian Marine Data Center through \\code{\\link{getNMDinfo}} and \\code{\\link{getNMDdata}}. The data are placed in StoX projects, enabling the data to be read using \\code{\\link{getBaseline}}. The function \\code{\\link{readXMLfiles}} can be used to simply read an acoustic or biotic xml file into R memory (via a temporary StoX project). The simpler function \\code{\\link{downloadXML}} reads an arbitrary XML file into a list. It is also possible to write acoustic and biotic XML file in the MND echosounder (version 1.0) and biotic (version 1.4 and 3.0) format.",
		"Rstox also contains functions for generating and reporting parallel or zigzag transect lines for use in a survey through \\code{\\link{surveyPlanner}}.",
		"Soon to be implemented is running the Estimated Catch at Age (ECA) model develped by the Norwegian Computing Center and the Norwegian Institute of Marine Research."
	)
}

authors_Rstox <- function(version = "1.0") {
    list(
        list(given="Arne Johannes", family="Holmin",	role=c("cre", "aut"), email="arnejh@hi.no"), 
        list(given="Edvin",		 family="Fuglebakk", role=c("aut")),
        list(given="Gjert Endre",	   family="Dingsoer",	  role=c("aut")),
        list(given="Aasmund",	   family="Skaalevik", role=c("aut")),
        list(given="Espen",		 family="Johnsen",   role=c("aut"))
     )
}

.onLoad_Rstox <- function(version = "1.0") {
	out <- c(
		".onLoad <- function(libname, pkgname) {", 
		"\tif(Sys.getenv(\"JAVA_HOME\")!=\"\") Sys.setenv(JAVA_HOME=\"\")", 
		"\t# Initiate the Rstox environment:", 
		"\tDefinitions <- initiateRstoxEnv()", 
		"\t# Set the Java memory:", 
		"\tsetJavaMemory(Definitions$JavaMem)", 
		"} "
	)
	out <- paste(out, collapse="\n")
	#function(libname, pkgname) {
	#	if(Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
	#	# Initiate the Rstox environment:
	#	Definitions <- initiateRstoxEnv()
	#	# Set the Java memory:
	#	setJavaMemory(Definitions$JavaMem)
	#} 
	
	return(out)	
}

.onAttach_Rstox <- function(version = "1.0") {
	
	fun <- ".onAttach <- function(libname, pkgname) {"
	hash <- "**********"
	devWarn <- "WARNING: This version of Rstox is an unofficial/developer version and bugs should be expected."
	JavaMem <- "If problems with Java Memory such as java.lang.OutOfMemoryError occurs, see ?setJavaMemory."
	info <- paste(
		c(
			"", 
			hash, 
			if(!isMaster(version)) devWarn else NULL, 
			JavaMem, 
			hash, 
			""
		)
		, collapse = "\n"
	)
	
	
	ver <- paste0("Rstox_", version)
	
	out <- paste(
		fun, 
		paste0("\tpackageStartupMessage(", deparse(paste(ver, info)), ", appendLF=FALSE)"), 
		"}", 
		sep = "\n"
	)
	
	
	return(out)
}

misc_Rstox <- function(version = "1.0") {
	c(
		"# To do this, uncheck the box \"32-bit Files\" when selecting components to install.", 
		"# If you are re-installing an R that has both 32 and 64 bit, you will need to uninstall R first.", 
		"", 
		"# On Windows systems with adminstrator requirements, it is recommended to install R in C:/users/<user>/documents/R.", 
		"# Also if you are using Rstudio, please make sure that you are using the correct R version (in case you have", 
		"# multiple versions installed). The R version can be selected in Tools > Global Options.", 
		"", 
		"# Note that 64 bit Java is required to run Rstox", 
		"", 
		"# On Windows, install Java from this webpage: https://www.java.com/en/download/windows-64bit.jsp,", 
		"# or follow the instructions found on ftp://ftp.imr.no/StoX/Tutorials/", 
		"", 
		"# On Mac, getting Java and Rstox to communicate can be challenging.", 
		"# If you run into problems such as \"Unsupported major.minor version ...\", try the following:", 
		"# Update java, on", 
		"# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", 
		"# If this does not work install first the JDK and then the JRE:", 
		"# \thttp://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html", 
		"# \thttp://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html", 
		"# Rstox sohuld also work with Java 11, presently available only as Development Kit:", 
		"# \thttps://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html", 
		"# You may want to check that the downloaded version is first in the list by running the following in the Terminal:", 
		"# \t/usr/libexec/java_home -V", 
		"# \tjava -version", 
		"# Then run this in the Terminal.app (you will be asked for password, but the password will not show as you type.", 
		"# It is possible to type the password in a text editor first and then paste it into the Terminal.):", 
		"# \tsudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib", 
		"# \tsudo R CMD javareconf", 
		"# Open R (close and then open if already open) and install rJava:", 
		"# \tutils::install.packages('rJava', type='source')", 
		"# \tutils::install.packages('rJava', type=\"binary\")", 
		"# If this fails, try installing from source instead using utils::install.packages('rJava', type='source')", 
		"# Then the installed Rstox should work."
	)
}
##########

##### RstoxFramework: #####
title_RstoxFramework <- function(version = "1.0") {
	"The Engine of StoX"
}

description_RstoxFramework <- function(version = "1.0") {
	"The framwork of StoX >= 3.0."
}

details_RstoxFramework <- function(version = "1.0") {
	c(
		"The RstoxFramework package is the engine of the stock assesment utility StoX, which is an open source application fo acoustic-trawl and swept-area survey estimation.", 
		"The package creates an evironment containing the StoX project(s) in separate environments. Each StoX project consists of a list of data tables holding e.g. biotic and acoustic data, filtered versions of the data, strata system, definitios of primary sampling units, accompanied by a list of specifications of the StoX processes comprising the StoX project. A StoX process is an R function taking as input the project name, the input data and parameters used by the function.",
		"The package replaces the old Java library in StoX versions prior to StoX 4.0."
	)
}

authors_RstoxFramework <- function(version = "1.0") {
	list(
		list(given="Arne Johannes", family="Holmin",	role=c("cre", "aut"), email="arnejh@hi.no"), 
		list(given="Ibrahim",	   family="Umar",	  role=c("aut")),
		list(given="Edvin",		 family="Fuglebakk", role=c("aut")),
		list(given="Aasmund",	   family="Skaalevik", role=c("aut")),
		list(given="Esmael Musema", family="Hassen",	role=c("aut")),
		list(given="Sindre",		family="Vatnehol",  role=c("aut")),
		list(given="Espen",		 family="Johnsen",   role=c("aut")),
		list(given="Atle",		  family="Totland",   role=c("aut")),
		list(given="Mikko Juhani",  family="Vihtakari", role=c("aut")),
		list(given="Norwegian Institute of Marine Research",   role=c("cph", "fnd"))
	)
}

.onLoad_RstoxFramework <- function(version = "1.0") {
	out <- c(
		".onLoad <- function(libname, pkgname) {", 
		"\t# Initiate the RstoxFramework environment:", 
		"\tinitiateRstoxFramework()", 
		"} "
	)
	paste(out, collapse="\n")
}
##########

##### RstoxData: #####
title_RstoxData <- function(version = "1.0") {
    # "Utilities to Read Fisheries Biotic, Acoustic and Landing Data Formats"
	"Utilities to Read and Manipulate Fisheries' Trawl Survey, Acoustic Survey and Commercial Landings Data Formats"
}

description_RstoxData <- function(version = "1.0") {
	#"Tools to fetch and manipulate various data formats for fisheries (mainly geared towards biotic and acoustic data)."
    "Set of tools to read and manipulate various data formats for fisheries. Mainly catered towards scientific trawl survey sampling ('biotic') data, acoustic trawl data, and commercial fishing catch ('landings') data. Among the supported data formats are the data products from the Norwegian Institute Marine Research ('IMR') and the International Council for the Exploration of the Sea (ICES)."
}

details_RstoxData <- function(version = "1.0") {
	"The RstoxData package contains functions for reading, filtering and writing biotic, acoustic and landing data as XML files. Filtering can be done by R syntax such as longitude > 10, or by pre defined functions such as inside(). On computers that return errors when trying to run the Rtools through RStudio (most institutional Windows machines), install the binary directly from https://github.com/StoXProject/RstoxData/releases. Download the newest RstoxData zip file, click the \"Packages\" tab -> \"Install\" -> \"Install from:\" \"Package Archive File\" -> \"Install\". If the installer does not complain, the package is installed correctly."
}

authors_RstoxData <- function(version = "1.0") {
	list(
		list(given="Ibrahim",	   family="Umar",	  role=c("cre", "aut"), email="ibrahim.umar@hi.no"), 
		#list(given="Mikko Juhani",  family="Vihtakari", role=c("aut")),
		list(given="Sindre",		family="Vatnehol",  role=c("aut")),
		list(given="Arne Johannes", family="Holmin",	role=c("aut")),
		list(given="Edvin",		 family="Fuglebakk", role=c("aut")),
		list(given="Espen",		 family="Johnsen",   role=c("aut")),
		list(given="Norwegian Institute of Marine Research",   role=c("cph", "fnd"))
	)
}

.onLoad_RstoxData <- function(version = "1.0") {
    out <- c(
        ".onLoad <- function(libname, pkgname) {", 
        "\t# Initiate the RstoxData environment:", 
        "\tinitiateRstoxData()", 
        "} "
    )
    paste(out, collapse="\n")
}

.onUnload_RstoxData <- function(version = "1.0") {
    out <- c(
        "# Try to unload dynamic library", 
        ".onUnload <- function (libpath) {", 
        "\tlibrary.dynam.unload(\"RstoxData\", libpath)", 
        "} "
    )
    paste(out, collapse="\n")
}
##########

##### RstoxFDA: #####
title_RstoxFDA <- function(version = "1.0") {
	"Fisheries Dependent Analysis with RstoX"
}

description_RstoxFDA <- function(version = "1.0") {
	"Fisheries dependent analysis, including running the Estimated Catch at Age model through the Reca package developed by the Norwegian Computing Center."
}

details_RstoxFDA <- function(version = "1.0") {
	"The estimated catch at age (ECA) model uses the correlation structure in fisheries dependent data to distribute age readings from cathes (samples) onto the total reported landings. The ECA model is described in Hirst, D., Aanes, S., Storvik, G., Huseby, R. B., & Tvete, I. F. (2004). Estimating catch at age from market sampling data by using a Bayesian hierarchical model. Journal of the Royal Statistical Society: Series C (Applied Statistics), 53(1), 1-14."
}

authors_RstoxFDA <- function(version = "1.0") {
	list(
		list(given="Arne Johannes", family="Holmin",	role=c("cre", "aut"), email="arnejh@hi.no"), 
		list(given="Edvin",		 family="Fuglebakk", role=c("aut"))
	)
}
##########

##### RstoxSurveyPlanner: #####
title_RstoxSurveyPlanner <- function(version = "1.0") {
	"Survey Design of Acoustic-trawl and Swept-area Surveys"
}

description_RstoxSurveyPlanner <- function(version = "1.0") {
	"Tools to generate parallel or zig zag transects for use in acoustic-trawl and swept area surveys."
}

details_RstoxSurveyPlanner <- function(version = "1.0") {
	"The RstoxSurveyPlanner package is a tool for generating parallel and zig zag transects for acoustic-trawl and swept-area surveys. Several methods for zig zag survey design are implemented, including the equal space design by Strindberg, S., & Buckland, S. T. (2004). Zigzag survey designs in line transect sampling. Journal of Agricultural, Biological, and Environmental Statistics, 9(4), 443. To garantee equal coverage the package includes the zig zag survey design presented by Harbitz, A. (2019). A zigzag survey design for continuous transect sampling with guaranteed equal coverage probability. Fisheries Research, 213, 151-159."
}

authors_RstoxSurveyPlanner <- function(version = "1.0") {
	list(
		list(given="Arne Johannes", family="Holmin",   role=c("cre", "aut"), email="arnejh@hi.no"), 
		list(given="Sindre",		family="Vatnehol", role=c("cre", "aut"), email="sindre.vatnehol@hi.no"), 
		list(given="Alf",		   family="Harbitz",  role=c("aut")), 
		list(given="Espen",		 family="Johnsen",  role=c("aut"))
	)
}
##########

##### RstoxTempdoc: #####
title_RstoxTempdoc <- function(version = "1.0") {
	"Temporary Package for Documenting the RstoxFramework Package"
}

description_RstoxTempdoc <- function(version = "1.0") {
	"Documenting RstoxFramework."
}

details_RstoxTempdoc <- function(version = "1.0") {
	"This package will be deleted once the development of the RstoxFramework package has reached a version with identical or expectedly differing output as StoX 3.0."
}

authors_RstoxTempdoc <- function(version = "1.0") {
	list(
		list(given="Arne Johannes", family="Holmin",  role=c("cre", "aut"), email="arnejh@hi.no"), 
		list(given="Atle",		  family="Totland", role=c("aut"))
	)
}
##########

##### RstoxBuild: #####
title_RstoxBuild <- function(version = "1.0") {
	"Package for Building All Rstox Packages"
}

description_RstoxBuild <- function(version = "1.0") {
	"Building the Rstox packages (Rstox, RstoxFramework, RstoxData, RstoxFDA, RstoxSurveyPlanner, RstoxTempdoc, and even RstoxBuild), and semi-automated testing of Rstox though test projects."
}

details_RstoxBuild <- function(version = "1.0") {
	"The package defines titles, descriptions, dependencies, authors, install instructions and other info for all the packages. All changes to authors, descriptions, suggests and other outputs of the function \\code{packageSpecs} should be changed in this package, and not in the individual packages. The package also contains functionality for semi-automated testing of Rstox on a set of test projects."
}

authors_RstoxBuild <- function(version = "1.0") {
	list(
		list(given="Arne Johannes", family="Holmin", role=c("cre", "aut"), email="arnejh@hi.no")
	)
}
##########

##### RstoxBase: #####
title_RstoxBase <- function(version = "1.0") {
	"Base StoX Functions"
}

description_RstoxBase <- function(version = "1.0") {
	"Base StoX functions used for survey estimation."
}

details_RstoxBase <- function(version = "1.0") {
	"The StoX functions defined in RstoxBase are those for defining resolution (e.g., PSUs and Layers), assignment, NASC data and StationLengthDistribution data, density, abundance and superindividual abundance"
}

authors_RstoxBase <- function(version = "1.0") {
	list(
		list(given="Arne Johannes", family="Holmin", role=c("cre", "aut"), email="arnejh@hi.no"),
		list(given="Ibrahim",	 family="Umar",	     role=c("aut")), 
		list(given="Sindre",		family="Vatnehol",  role=c("aut")),
		list(given="Edvin",		 family="Fuglebakk", role=c("aut")),
		list(given="Aasmund",	   family="Skaalevik", role=c("aut")),
		list(given="Esmael Musema", family="Hassen",	role=c("aut")),
		list(given="Espen",		 family="Johnsen",   role=c("aut")),
		list(given="Atle",		  family="Totland",   role=c("aut")),
		list(given="Norwegian Institute of Marine Research",   role=c("cph", "fnd"))
	)
}

.onLoad_RstoxBase <- function(version = "1.0") {
	out <- c(
		".onLoad <- function(libname, pkgname) {", 
		"\t# Initiate the RstoxBase environment:", 
		"\tinitiateRstoxBase()", 
		"} "
	)
	paste(out, collapse="\n")
}
##########

##### Rstox: #####
title_RstoxAPI <- function(version = "1.0") {
	"The API to StoX"
}

description_RstoxAPI <- function(version = "1.0") {
	"This package is the interface between RstoxFramework and the StoX GUI. It ensures that the specified packages are installed for a StoX version."
}

details_RstoxAPI <- function(version = "1.0") {
	"The package contains the function runModel() for running a model of a StoX project, runFunction() for accessing a function in RstoxFramework (or another package), and specifies the particular versions of the RstoxFramework and the packages specified as official packages by RstoxFramework."
}

authors_RstoxAPI <- function(version = "1.0") {
	list(
		list(given="Arne Johannes", family="Holmin", role=c("cre", "aut"), email="arnejh@hi.no")
	)
}

.onLoad_RstoxAPI <- function(version = "1.0") {
	out <- c(
		".onLoad <- function(libname, pkgname) {", 
		"\t# Initiate the RstoxAPI environment:", 
		"\tinitiateRstoxAPI()", 
		"} "
	)
	paste(out, collapse="\n")
}
##########


##### Utility functions: #####
# Function to get a package documentation object:
getPackageItem <- function(name, spec, packageName=NULL, version=NULL) {
	# If not given (default) search for the object containing the spec:
	if(length(spec[[name]]) == 0) {
		# The 'packageName' must be given:
		if(length(packageName) == 0) {
			stop("The 'packageName' must be given if the package documentation object is not present in 'spec' and is searched for in memory.")
		}
		
		# Define the object name as the concatination of the 'name' and the package name, separated by underscore:
		objectName <- paste(name, packageName, sep="_")
		if(exists(objectName)) {
			object <- get(objectName)
		}
		else{
			object <- getDefault(name)
		}
		#else{
		#	stop(paste("The package documentation object", objectName, "does not exist. Please define it as a string vector or a function with one parameter 'version' returning a string vector."))
		#}
		
		# If the object is a function of the version, apply the function:
		if(is.function(object) && "version" %in% names(formals(object))) {
			object <- object(version)
		}
		# If the object is still a function, deparse it to print to file:
		if(is.function(object)) {
			object <- paste(name, "<-", paste(deparse(object, control="all"), collapse="\n"))
			
		}
	}
	else{
		object <- spec[[name]]
	}
	
	return(object)
}
# Funciton to get the install path to GitHub:
getGitHub_InstallPath <- function(packageName = "Rstox", accountName = "StoXProject" , version = NULL, ref = NULL, additional_repositories = NULL) {
	# Get the relative GitHub path for the specific release:
	path <- getGithubURL(
		packageName = packageName, 
		accountName = accountName
	)

	#path <- file.path(basename(githubRoot), packageName)
	# Add the release version:
	#if(length(version)) {
	#	path <- paste0(path, "@v", version)
	#}
	#path <- deparse(path)
	
	ref <- version
	
	## Add the ref, which could be the deleop branch:
	#if(length(ref)) {
	#	path <- paste0(path, ", ref = ", deparse(ref))
	#}
	
	# Construct and return the install string:
	if(length(additional_repositories)) {
	    string <- paste0(
	        "# Install the latest binary:\n", 
	        "install.packages(", packageName, ", repo = c(\"https://stoxproject.github.io/repo\", \"https://cloud.r-project.org\"))\n", 
	        "# ... or install the latest version from GitHub:\n", 
	        "# devtools::install_github(", "\"", path, "\"", ", ref = ", deparse(ref), ")"
	    )
	}
	else {
	    string <- paste0(
	        "# Install the latest GitHub release:\n", 
	        "devtools::install_github(", "\"", path, "\"", ", ref = ", deparse(ref), ")"
	    )
	}
	
	string <- c(
        string, 
        paste0(
            "# ... or install the develop version from GitHub:\n", 
            "# devtools::install_github(", "\"", path, "\"", ", ref = \"develop\")"
        )
    )
	
	
	return(string)
}
# Function to get the link to the (online) NEWS file on GitHub depending on the :
getGitHub_NewsLink <- function(packageName = "Rstox", accountName = "StoXProject", version = "1.0") {
	file.path(
		getGithubURL(
			packageName = packageName, 
			accountName = accountName
		), 
		"blob", 
		#if(isMaster(version)) "master" else "alpha", 
		"master", 
		"NEWS"
	)
}
# Function to get the relevant release notes:
getNews_old <- function(NEWSfile, version = "1.0") {
	if(file.exists(NEWSfile)) {
		l <- readLines(NEWSfile, warn=FALSE)
		# Split into vesions:
		atversion <- which(substr(l, 1, 1) == "#")
		# Strip off "#", and extract the version string:
		versionStringInChanges <- l[atversion]
		startversion <- regexpr("Version ", versionStringInChanges)
		startversion <- startversion + attributes(startversion)$match.length
		versionStringInChanges <- substring(versionStringInChanges, startversion)
		endversion <- regexpr(" ", versionStringInChanges) - 1
		versionStringInChanges <- substr(versionStringInChanges, 1, endversion)
		# Split into versions and format to insert into the README file:
		l <- split(l, findInterval(seq_along(l), c(atversion, length(l)+1)))
		names(l) <- versionStringInChanges
		# Remove the version line:
		l <- lapply(l, function(xx) xx[substr(xx, 1, 1) != "#"])
		thisl <- l[[version]]
		hasText <- which(nchar(thisl)>1)
		thisl[hasText] <- paste0("# ", seq_along(hasText), ". ", thisl[hasText])
	}
	else{
		thisl <- NULL
	}
	
	return(thisl)
}
getNews <- function(NEWSfile, version = "1.0", NEWSBullet="*", READMEBullet="#") {
	if(file.exists(NEWSfile)) {
		# Strip off "#", and extract the version string:
		getPos <- function(pattern, string) {
			out <- regexpr(pattern, string)
			out <- out + attributes(out)$match.length
			out
		}
		
		# Read the NEWS:
		l <- readLines(NEWSfile, warn=FALSE)
		# Delete lines with only whitespace:
		l <- l[nchar(gsub("[[:space:]]", "", l)) > 0]
		# Split into vesions:
		atversion <- which(startsWith(l, "==")) - 1
		
		# Get only the version lines:
		versionString <- l[atversion]
		start <- getPos(" v", versionString)
		end <- getPos(" ", substring(versionString, start)) - 3 + start
		versionString <- substring(versionString, start, end)
		
		# Split into versions and format to insert into the README file:
		l <- split(l, findInterval(seq_along(l), c(atversion, length(l)+1)))
		names(l) <- versionString
		
		# Remove the version line:
		thisl <- l[[version]]
		thisl <- trimws(thisl)
		thisl <- thisl[startsWith(thisl, NEWSBullet)]
		thisl <- sub(NEWSBullet, "", thisl, fixed = TRUE)
		thisl <- paste0(READMEBullet, " ", seq_along(thisl), ". ", thisl)
	}
	else{
		thisl <- NULL
	}
	
	return(thisl)
}
# Is the release a master/beta release (judging from the number of dots in the version)?:
isMaster <- function(version = "1.0") {
	length(gregexpr(".", version, fixed = TRUE)[[1]]) == 1
}
# Get one author string:
getAuthor <- function(x) {
	# Deparse and add parameter names:
	out <- lapply(x, deparse)
	out <- paste0(names(out), " = ", out, collapse = ", \n    ")
	# Enclose in a person function:
	out <- paste0(
	    "person(",
	    out, 
	    ")"
	)
	
	return(out)
}
# Get one all author strings:
getAuthors <- function(x) {
	if(is.list(x)) {
		if(!is.list(x[[1]])) {
			x <- list(x)
		}
	}
	#else if(length(x) == 2) {
	#	x <- list(list(given=x[1], family=x[2], role=c("cre", "aut")))
	#}
	else{
		stop("Malformed author. Must be a named list with given and family name, role, and email.")
	}
	
	out <- lapply(x, getAuthor)
	# Create indent:
	out <- paste0("  ", out)
	out <- paste(out, collapse=", \n")
	# Enclose in a c():
	out <- paste0(
		"c(\n",
		out, 
		")"
	)
	return(out)
}
getDefault <- function(name) {
	defaults <- list(
		title = "Title of the package", 
		description = "Description of the package", 
		details = "Details of the package", 
		authors = list(
			given = "Given Name", 
			family = "Family Name", 
			role = c("cre", "aut"), 
			email = "arnejh@hi.no")
	)
	return(defaults[[name]])
}

# Write single function PDF:
writeFunctionPDFFromInstalledPackage <- function(functionName, packageName, pdfDir) {
	# We need to load the package to get the Rd database:
	library(packageName, character.only = TRUE)
	# Get the Rd database:
	db <- tools::Rd_db(packageName)
	# Define a temporary file to write the Rd to
	tmp <- tempfile()
	# Paste the Rd to one string and write to the temporary file:
	thisRd <- as.character(db[[paste0(functionName, ".Rd")]])
	
	if(length(thisRd)) {
		thisRd <- paste(thisRd, collapse = "")
		write(thisRd, tmp)
		# Define the output PDF file:
		pdfFile <- path.expand(file.path(pdfDir , paste0(functionName, ".pdf")))
		# Build the PDF
		msg <- callr::rcmd(
			"Rd2pdf", 
			cmdargs = c(
				"--force", 
				"--no-index", 
				paste0("--title=", packageName, "::", functionName), 
				paste0("--output=", pdfFile), 
				tmp
			)
		)
		# Return the path to the PDF file:
		pdfFile
	}
	else {
		warning("Function ", functionName, " not exported from package ", packageName)
		return(NULL)
	}
}

getFunctionArgumentDescriptionsFromInstalledPackage <- function(functionName, packageName) {
	
	# The package needs to be loaded to get the documentation database:
	library(packageName, character.only = TRUE)
	# Get the Rd database:
	db <- tools::Rd_db(packageName)
	# Define a temporary file to write the Rd to
	tmp <- tempfile()
	# Paste the Rd to one string and write to the temporary file:
	thisRd <- as.character(db[[paste0(functionName, ".Rd")]])
	thisRd <- paste(thisRd, collapse = "")
	write(thisRd, tmp)
	# Read the file back in:
	p <- tools::parse_Rd(tmp)
	
	# Detect the arguments, and get the valid arguments as those which are not line breaks:
	atArguments <- sapply(p, attr, "Rd_tag") == "\\arguments"
	argumentNames <- unlist(lapply(p[atArguments][[1]], head, 1))
	validArgumentNames <- argumentNames != "\n"
	
	# Get and return the argument names and the descriptions:
	argumentNames <- subset(argumentNames, validArgumentNames)
	descriptions <- lapply(p[atArguments][[1]][validArgumentNames], function(x) paste0(unlist(x)[-1], collapse = " "))
	names(descriptions) <- argumentNames
	#list(
	#	argumentName = argumentNames, 
	#	description = descriptions
	#)
	descriptions
}

# Write single function PDF:
writeFunctionPDFs <- function(sourceDir, pdfDir = NULL) {
	
	# Function to convert one documentation file to pdf:
	writeFunctionPDf_one <- function(manFile, pdfDir) {
		# Get the funciton name:
		functionName <- tools::file_path_sans_ext(basename(manFile))
		
		# Define the output PDF file:
		pdfFile <- path.expand(file.path(pdfDir , paste0(functionName, ".pdf")))
		
		# Build the PDF
		msg <- callr::rcmd(
			"Rd2pdf", 
			cmdargs = c(
				"--force", 
				"--no-index", 
				paste0("--title=", functionName), 
				paste0("--output=", pdfFile), 
				manFile
			)
		)
		# Return the path to the PDF file:
		pdfFile
	}
	
	# Get the directories of the documentation files and the 
	manDir <- file.path(sourceDir, "man")
	if(length(pdfDir) == 0) {
		pdfDir <- file.path(sourceDir, "inst", "extdata", "functionPDFs")
	}
	if(file.exists(pdfDir)) {
		unlink(pdfDir, force = TRUE, recursive = TRUE)
	}
	dir.create(pdfDir, showWarnings = FALSE, recursive = TRUE)
	
	
	# List all the documentation files:
	manFiles <- list.files(manDir, full.names = TRUE)
	# Convert all documentation files:
	sapply(manFiles, writeFunctionPDf_one, pdfDir = pdfDir)
}

# Get and write the function argument descriptions of all documentation files:
getFunctionArgumentDescriptions <- function(sourceDir, docDir = NULL) {
	
	# Get the function argument descriptions of one documentation file:
	getFunctionArgumentDescriptions_one <- function(manFile) {
		# Parse the documentation file:
		doc <- tools::parse_Rd(manFile)
		
		# Detect the arguments, and get the valid arguments as those which are not line breaks:
		atArguments <- sapply(doc, attr, "Rd_tag") == "\\arguments"
		if(any(atArguments)) {
			argumentNames <- unlist(lapply(doc[atArguments][[1]], head, 1))
			validArgumentNames <- argumentNames != "\n"
			
			# Get and return the argument names and the descriptions:
			argumentNames <- subset(argumentNames, validArgumentNames)
			descriptions <- lapply(doc[atArguments][[1]][validArgumentNames], function(x) paste0(unlist(x)[-1], collapse = " "))
			names(descriptions) <- argumentNames
			
			descriptions
		}
		else {
			descriptions <- NULL
		}
		descriptions
	}
	
	
	# Get the directories of the documentation files and the 
	manDir <- file.path(sourceDir, "man")
	if(length(docDir) == 0) {
		docDir <- file.path(sourceDir, "inst", "extdata")
	}
	#docFile <- 
	#if(file.exists(docDir)) {
	#	unlink(docDir, force = TRUE, recursive = TRUE)
	#}
	dir.create(docDir, recursive = TRUE, showWarnings = FALSE)
	
	
	# List all the documentation files:
	manFiles <- list.files(manDir, full.names = TRUE)
	
	# Convert all documentation files:
	functionDocumentation <- lapply(manFiles, getFunctionArgumentDescriptions_one)
	
	# Get the funciton name:
	functionNames <- basename(sapply(manFiles, tools::file_path_sans_ext))
	names(functionDocumentation) <- functionNames
	
	
	functionDocumentationFile <- file.path(docDir, "functionArguments.rds")
	saveRDS(functionDocumentation, functionDocumentationFile)
}

# Function to add a package to the DESCRIPTION file, optionally with minimum version:
use_package_with_min_version <- function(packageList, type = "Imports") {
    # Add each package with version if present:
	#mapply(
	#	usethis::use_package, 
	#	package = names(packageList), 
	#	type = type, 
	#	min_version = packageList
	#)
    
    # Get the versions:
    getVersionString <- function(version) {
        if(length(version) == 0) {
            "*"
        }
        else if(is.list(version)) {
            paste("==", unlist(version))
        }
        else {
            paste(">=", version)
        }
    }
    
    versions <- sapply(packageList, getVersionString)
    
    mapply(
        desc::desc_set_dep, 
        package = names(packageList), 
        type = type, 
        version = versions, 
        file = usethis::proj_get()
    )
}

# Function to build a Rstox package version string:
getRstoxPackageVersionString <- function(packageName, version) {
	paste0(packageName, "-v", version)
}





getReleases <- function(packageName, accountName = "StoXProject", all.releases = FALSE) {
    API <- githubPaths("api")
    URL_releases <- paste(API, accountName, packageName, "releases", sep = "/")
    #URL_tags <- paste(API, accountName, packageName, "tags", sep = "/")
    print(URL_releases)
    tryCatch(
        {
            # Access the API using the GITHUB_PAT (personal access token)
            pat <- Sys.getenv("GITHUB_PAT")
            if(nchar(pat)) {
                api_data <- httr::GET(URL_releases, httr::authenticate(Sys.getenv("GITHUB_PAT"), "x-oauth-basic", "basic"))
            }
            else {
                warning("Please create a personal access token with the following procedure: Use `usethis::browse_github_pat()` to go the the GitHub page where you need to log in and click the green 'Generate token' button at the bottom of the page. Then copy this to the clipboard and use `usethis::edit_r_environ()` to open the .Renivron file. Add the token as `GITHUB_PAT=12345678901234567890` (replace with the copied token) and end the file with a line space. Save and close the file, and restart R to make the change effective.")
                api_data <- httr::GET(URL_releases)
            }
            parsed <<- jsonlite::fromJSON(httr::content(api_data, "text"), simplifyVector = FALSE)
        }, 
        error = function(err) {
            parsed <<- NULL
        }
    )
    if(length(parsed) == 0) {
        return(data.table::data.table())
    }
    
    releases <- sapply(parsed, "[[", "tag_name")
    if(all.releases || length(releases) == 0) {
        return(releases)
    }
    else {
        # Get valid release names, which are those starting with the package name:
        valid <- startsWith(releases, packageName)
        validReleases <- releases[valid]
        if(length(validReleases) == 0) {
            stop("No valid releases")
        }
        # Get versions:
        #versions <- sub("^[^-v]*", "", validReleases)
        versions <- sub(".*-v", "", validReleases)
        
        # Get the three individual numbers, which are separated by dot:
        versionsNumeric <- strsplit(versions, ".", fixed = TRUE)
        
        # Extract only three numbers, excluding any additional dots:
        versionsNumeric <- lapply(versionsNumeric, utils::head, 3)
        
        # Trim any characters from punctuation and on:
        versionsNumeric <- lapply(versionsNumeric, function(x) gsub("(.*)[[:punct:]].*", "\\1", x))
        versionsNumeric <- lapply(versionsNumeric, as.numeric)
        
        # Append NAs for those which may have less than 3 numbers:
        versionsNumeric <- lapply(versionsNumeric, function(x) append(x, rep(NA, 3 - length(x))))
        
        # Convert to data.table:
        versionsNumeric <- lapply(versionsNumeric, as.list)
        
        # Rbind into data.table:
        versionsNumeric <- data.table::rbindlist(versionsNumeric)
        
        names(versionsNumeric) <- c("major", "minor", "patch")
        
        return(versionsNumeric)
    }
}


getHighestRelease <- function(packageName, sting.out = TRUE, accountName = "StoXProject") {
    
    # Get release version numbers:
    versionsNumeric <- getReleases(packageName, accountName = accountName, all.releases = FALSE)
    # If there are no versions, return 0.0.1:
    if(length(versionsNumeric) == 0) {
        versionsNumeric <- list(0, 0, 1)
        names(versionsNumeric) <- c("patch", "minor", "major")
        if(sting.out) {
            versionsString <- paste(unlist(versionsNumeric), collapse = ".")
            return(versionsString)
        }
        else {
            return(versionsNumeric)
        }
    }
    
    # Order the versions by first, second and then third number:
    versionOrder <- order(versionsNumeric$major, versionsNumeric$minor, versionsNumeric$patch, decreasing = TRUE)
    
    if(versionOrder[[1]] != 1L) {
        warning("The latest release is not the one with the highest version number")
    }
    
    # Add 1 to the specified version number:
    highestVersion <- versionsNumeric[versionOrder[[1]], ]
    highestVersion <- as.list(highestVersion)
    
    if(sting.out) {
        highestVersion <- paste(unlist(highestVersion), collapse = ".")
    }
    
    return(highestVersion)
}

incrementHighestRelease <- function(packageName, type = c("patch", "minor", "major"), sting.out = TRUE, accountName = "StoXProject") {
    
    # Get the increment type:
    type <- match.arg(type)
    
    # Get the highest release:
    highestRelease <- getHighestRelease(packageName, sting.out = FALSE, accountName = accountName)
    
    nextVersion <- highestRelease
    nextVersion[[type]] <- nextVersion[[type]] + 1
    
    cat("Highest release: ", paste(unlist(highestRelease), collapse = "."), "\n")
    cat("Next release: ", paste(unlist(nextVersion), collapse = "."), "\n")
    
    if(sting.out) {
        nextVersion <- paste(unlist(nextVersion), collapse = ".")
    }
    
    return(nextVersion)
}

asNamedList <- function(x, values) {
    if(!is.list(x)) {
        x <- structure(vector("list", length(x)), names = x)
    }
    return(x)
}
##########


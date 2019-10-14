########## Attempt to write a general build method which takes package name as input, and has different README, .onLoad, .onAttach and other stuff specified in separate functions for the available packages: ##########

# 0.	Rstox
# 1.	RstoxFramework
# 2.	RstoxData
# 3.	RstoxECA
# 4.	RstoxSurveyPlanner
# 5a.	RstoxTempdoc
# 5b.	RstoxBuild

#*********************************************
#*********************************************
#' Function for building Rstox packages
#'
#' \code{buildRstoxPackage} is used in the continous development of Rstox, writing .onLoad, .onAttach, pkgname, DESCRIPTION and README files and adding dependencies to the NAMEPACE file.\cr \cr
#' \code{packageSpecs} gets all specifications of the package from separate funcitons for each package into a list.\cr \cr
#'
#' @param dir	The directory holding the package structure.
#' @param check	Logical: If TRUE run devtools::check() on the package.
#' 
#' @export
#' @rdname buildRstoxPackage
# 
buildRstoxPackage <- function(
	packageName, 
	version = "1.0", 
	Rversion = "3.5", 
	pckversion = NULL, 
	suggests = NULL, 
	githubRoot = "https://github.com/StoXProject", 
	onCran = FALSE, 
	license = "LGPL-3", 
	rootDir = NULL, 
	title = NULL, 
	description = NULL, 
	details = NULL, 
	.onLoad = NULL, 
	.onAttach = NULL, 
	misc = NULL, 
	authors = NULL, 
	check = FALSE){
	
	# Get the specifications of the package:
	spec <- packageSpecs(
		packageName = packageName, 
		version = version, 
		Rversion = Rversion, 
		pckversion = pckversion, 
		suggests = suggests, 
		githubRoot = githubRoot, 
		onCran = onCran, 
		license = license, 
		rootDir = rootDir, 
		title = title, 
		description = description, 
		details = details, 
		.onLoad = .onLoad, 
		.onAttach = .onAttach, 
		misc = misc, 
		authors = authors
	)
	
	# Set the path to the package for usethis:
	usethis::proj_set(spec$dir)
	
	# If there is a src folder present, temporary rename it to src_ to document without, and then re-document with afterwards:
	if(spec$useCpp){
		file.rename(spec$src, spec$src_)
	}
	
	# Make sure the folder is recognised as a package with a NAMESPACE file:
	#usethis::create_package(spec$dir)
	NAMESPACEFile <- file.path(spec$dir, "NAMESPACE")
	if(!file.exists(NAMESPACEFile)){
		cat("", file=NAMESPACEFile)
	}
	
	
	# Clear the installed package:
	try(lapply(.libPaths(), function(x) utils::remove.packages(spec$packageName, x)), silent=TRUE)
	
	##### Write the onLoad.R file: #####
	if(length(spec$.onLoad)){
		onLoadFile <- file.path(spec$dir, "R", "onLoad.R")
		write(spec$.onLoad, onLoadFile)
	}
	##########
	
	##### Write the onAttach.R file: #####
	if(length(spec$.onAttach)){
		onAttachFile <- file.path(spec$dir, "R", "onAttach.R")
		write(spec$.onAttach, onAttachFile)
	}
	##########
	
	##### Write the pkgname.R file: #####
	pkgnameFile <- file.path(spec$dir, "R", "pkgname.R")
	write(spec$pkgname, pkgnameFile)
	##########
	
	##### Write the DESCRIPTION file: #####
	DESCRIPTION <- getDESCRIPTION(spec)
	DESCRIPTIONFile <- file.path(spec$dir, "DESCRIPTION")
	write(DESCRIPTION, DESCRIPTIONFile)
	##########
	

	##### Create documentation: #####
	# Remove current documentation and then re-document without C++:
	unlink(file.path(spec$dir, "man"), recursive=TRUE, force=TRUE)
	devtools::document(spec$dir)
	
	
	# Alter the DESCRIPTION file to contain the imports listed in the NAMESPACE file. This must take place AFTER creating the documentation:
	spec$imports <- getImports(spec)
	addImportsToDESCRIPTION(spec)
	
	
	# If there is a src folder present, temporary rename it to src_ to document without, and then re-document with afterwards:
	if(spec$useCpp){
		file.rename(spec$src_, spec$src)
		usethis::use_rcpp()
		# Add the C++ specifics to the pkgnameFile:
		write(spec$Rcpp, pkgnameFile, append=TRUE)
		
		# Also remove the shared objects from the src directory:
		sharedObjects <- list.files(spec$src, pattern = "\\.o|so$", full.names=TRUE)
		unlink(sharedObjects, recursive=TRUE, force=TRUE)
		
		message(paste(
			"Using devtools::document to re-document the package after C++ code is included. If errors occurred such as 'System command error' try building the package to see specific c++ errors with the following commands:", 
			paste0("sharedObjects <- list.files(\"", spec$src, "\", pattern = \"\\\\.o|so$\", full.names=TRUE)"), 
			paste0("unlink(sharedObjects, recursive=TRUE, force=TRUE)"), 
			paste0("utils::install.packages(\"", spec$dir, "\", repos=NULL, type=\"source\", lib=.libPaths()[1])"), 
			sep = "\n    "
			)
		)
		
		# Re-document to include the C++ code:
		devtools::document(spec$dir)
	}
	
	
	##### Run R cmd check with devtools: #####
	if(check){
		devtools::check(pkg=spec$dir)
		# args = "--no-examples"
	}
	##########
	
	### Generate the README file: ###
	README <- getREADME(spec)
	READMEFile <- file.path(spec$dir, "README")
	write(README, READMEFile)
	##########
	
	##### Unload the package: #####
	# devtools::unload(spec$packageName)
	packageString <- paste("package", spec$packageName, sep=":")
	if(packageString %in% search()){
		detach(packageString, unload=TRUE, character.only=TRUE)
	}
	##########
	
	##### Install the package: #####
	if(spec$useCpp){
		sharedObjects <- list.files(spec$src, pattern = "\\.o|so$", full.names=TRUE)
		unlink(sharedObjects, recursive=TRUE, force=TRUE)
	}
	utils::install.packages(spec$dir, repos=NULL, type="source", lib=.libPaths()[1])
	# Build and open documentation pdf:
	pkg <- file.path(.libPaths()[1], spec$packageName)
	path <- file.path(pkg, "extdata", "manual")
	dir.create(path, recursive=TRUE)
	temp <- devtools::build_manual(pkg=pkg, path=path)
	print(path)
	# Open the PDF:
	pdfFile <- list.files(path, full.names=TRUE)[1]
	system(paste0("open \"", pdfFile, "\""))
	##########
	
	# Load the package:
	library(spec$packageName, character.only=TRUE)
	
	path
}
#' 
#' @export
#' @rdname buildRstoxPackage
# 
packageSpecs <- function(
	packageName, 
	version = "1.0", 
	Rversion = "3.5", 
	pckversion = NULL, 
	suggests = NULL, 
	githubRoot = "https://github.com/StoXProject", 
	onCran = FALSE, 
	license = "LGPL-3", 
	rootDir = NULL, 
	title = NULL,
	description = NULL,
	details = NULL,
	authors = NULL,
	.onLoad = NULL,
	.onAttach = NULL,
	misc = NULL
	){
	
	# If the packageName is a string with no slashes and does not exist as a directory, locate the directories of the developers defined in the 
	if(is.character(packageName) && !file.exists(packageName) && !grepl('\\\\|/', packageName)){
		if(length(rootDir) == 0){
			user <- Sys.info()["user"]
			rootDir <- utils::read.table(system.file("extdata", "rootDir.txt", package="RstoxBuild"), header=TRUE)
			rootDir <- rootDir$rootDir[rootDir$user == user]
		}
		# The path to the package source code folder should contain a folder named by the package name, containing various optional files and folders like "test" or "temp", and a sub folder also named by the oackage name, which is the folder containing the package source code with DESCRIPTION, NAMESPACE, sub folder "R" etc.
		packageName <- file.path(rootDir, packageName, packageName)
	}
	
	# Extract the package name from the path to the package source code folder:
	dir <- packageName
	packageName <- basename(packageName)
	
	# Get NEWS file and NEWS;
	NEWSfile <- file.path(dir, "NEWS")
	if(startsWith(readLines(NEWSfile, 1), "#")){
		NEWS <- getNews_old(NEWSfile, version=version)
	}
	else{
		NEWS <- getNews(NEWSfile, version=version)
	}
	
	
	# Construct the output list:
	spec <- list(
		# Paths, names and dependencies:
		dir = dir, 
		packageName = packageName, 
		version = version, 
		Rversion = Rversion, 
		pckversion = pckversion, 
		suggests = suggests, 
		# Mandatory objects:
		title = title, 
		description = description, 
		details = details, 
		authors = authors, 
		# Optional objects:
		.onLoad = .onLoad, 
		.onAttach = .onAttach, 
		misc = misc, 
		# Other specs:
		onCran = onCran, 
		githubRoot = githubRoot, 
		license = license, 
		NEWSfile = NEWSfile, 
		NEWS = NEWS
	)
	
	mandatory <- c("title", "description", "details", "authors")
	optional <- c(".onLoad", ".onAttach", "misc")
	mandatoryORoptional <- c(mandatory, optional)
	
	# Get the missing package documentation objects from memory:
	spec[mandatoryORoptional] <- lapply(mandatoryORoptional, getPackageItem, spec=spec, packageName=packageName, version=version)
	
	# Check for the existence of the mandatory objects:
	empty <- lengths(spec[mandatory]) == 0
	if(any(empty)){
		stop(paste("The following package documentation objects are mandatory. Add them as parameters or create them as objects named by e.g. title_Rstox as string vectors or functions returning a string vector: ", paste(mandatory[empty], collapse=", ")))
	}
	
	# Get the details shown using help(packageName):
	spec$pkgname <- getPkgname(spec)
	
	# Link to Rcpp:
	spec$useCpp <- FALSE
	spec$src <- file.path(spec$dir, "src")
	if(dir.exists(spec$src)){
		if(length(list.files(spec$src))){
			spec$useCpp <- TRUE
		}
	}
	spec$src_ <- file.path(spec$dir, "src_")
	spec$Rcpp <- c(
		paste0("#' @useDynLib ", spec$packageName, ", .registration = TRUE"), 
		"#' @importFrom Rcpp sourceCpp", 
		"NULL"
	)
	
	
	## src <- file.path(spec$dir, "src")
	## spec$useCpp <- FALSE
	## if(file.exists(src)){
	## 	if(length(list.files(src)) == 0){
	## 		stop("The src folder is empty. Put c++ files in it or remove it. Documentatino craches if not.")
	## 	}
	## 	spec$useCpp <- TRUE
	## 	spec$Rcpp <- c(
	## 		paste0("#' @useDynLib", spec$packageName, ", .registration = TRUE"), 
	## 		"#' @importFrom Rcpp sourceCpp", 
	## 		"NULL"
	## 	)
	## 	### # Do what usethis::use_rcpp() does, adding Rcpp to imports and linkedto:
	## 	### spec$linkingto <- unique(c(spec$linkingto, "Rcpp"))
	## 	### spec$imports <- unique(c(spec$imports, "Rcpp"))
	## }
	
	return(spec)
}


#*********************************************
#*********************************************
#' Utility functions for getting and adding imports, DESCRIPTION, README and kgname to the package.
#'
#' \code{getDESCRIPTION} gets the DESCRIPTION text to write to the DESCRIPTION file, adding authors, R-dependency, title, description and other info stored in the input \code{spec}. Note that the package imports are not added here, but later in the \code{addImportsToDESCRIPTION} function.\cr \cr
#' \code{getREADME} gets the README file in the Rstox style, which has package and R version in the first two lines, followed by description, install instructions, miscellaneous info, and release notes retrieved from the NEWS file.\cr \cr
#' \code{getImports} gets the package dependencies (imports) from the NAMESPACE file. This function must be run (immediately) after devtools::document(dir) in \code{\link{buildRstoxPackage}}.\cr \cr
#' \code{addImportsToDESCRIPTION} adds the imports to the DESCRIPTION file.\cr \cr
#' \code{getPkgname} gets the pkgname text to write to the pkgname.R file.
#'
#' @param spec	A list of package specifications returned from \code{\link{packageSpecs}}.
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
getDESCRIPTION <- function(spec){
	# Add R-dependency:
	if(length(spec$Rversion)){
		Depends = paste0("R (>= ", spec$Rversion, ")")
	}
	else{
		Depends <- NULL
	}
	
	# Add package and bug reports URL:
	URL = file.path(spec$githubRoot, spec$packageName)
	BugReports <- file.path(URL, "issues")
	
	# Paste and return the info:
	out <- list(
		"Package" = spec$packageName, 
		"Title" = spec$title, 
		"Version" = spec$version, 
		"Authors@R" = getAuthors(spec$authors), 
		"Depends" = Depends, 
		"Description" = spec$description, 
		"URL" = URL, 
		"BugReports" = BugReports, 
		"License" = spec$license, 
		"LazyData" = "true"
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
getREADME <- function(spec){
	
	# Add devtools to the imports, since it is used for installing:
	if(spec$onCran){
		spec$imports <- c(spec$imports, "devtools")
	}
	
	# Write package and R version in the first two lines. THIS SHOULD NEVER BE CHANGED, SINCE STOX READS THESE TWO LINES TO CHECK VERSIONS:
	header <- c(
		paste0("# ", spec$packageName, " version: ", spec$version, " (latest ", if(isMaster(version=spec$version)) "beta" else "alpha", ", ", format(Sys.time(), "%Y-%m-%d"), ")"), 
		paste0("# R version: ", spec$Rversion)
	)
	
	# If available instruct the user to install from CRAN:
	if(spec$onCran){
		install <- c(
			paste0("# Install the ", spec$packageName, "package:"), 
			paste0("utils::install.packages(", spec$packageName, ")")
		)
	}
	# Install from GitHub, either with or without first installing dependencies:
	else{
		# Define install of dependencies:
		if(length(spec$imports)){
			install <- c(
				paste0("# Install the packages that ", spec$packageName, " depends on. Note that this updates all the specified packages to the latest (binary) version:"), 
				paste0("dep.pck <- c(\"", paste0(spec$imports, collapse="\", \""), "\")"), 
				""
			)
		}
		else{
			install <- NULL
		}
		
		# Add install of the package:
		install <- c(
			install, 
			paste0("# Install ", spec$packageName, " from GitHub using the devtools package:"), 
			getGitHub_InstallPath(packageName=spec$packageName, version=spec$version, githubRoot=spec$githubRoot), 
			""
		)
		
		# Add the alternative install:
		install <- c(
			"# Install the latest developer version", 
			getGitHub_InstallPath(packageName=spec$packageName, version=NULL, githubRoot=spec$githubRoot)
		)
	}
	
	releaseNotes <- c(
		paste0("# Release notes for ", spec$packageName, "_", spec$version, ":"), 
		spec$NEWS, 
		"", 
		paste0("# For historical release notes see ", getGitHub_NewsLink(packageName=spec$packageName, version=spec$version, githubRoot=spec$githubRoot))
	)
	
	# Combine into a list and add space between each paragraph:
	README <- list(
		header,
		paste("#", spec$description), 
		install, 
		spec$misc, 
		releaseNotes
	)
	README <- README[lengths(README) > 0]
	README <- lapply(README, append, rep("", 2))
	README <- unlist(README)
	
	return(README)
}
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
getImports <- function(spec){
	
	# Functions used for extracting the imports in Rstox, in order to inform about these in the README file. This will not be needed once the package is on CRAN:
	discardBasePackages <- function(x){
		inst <- utils::installed.packages()
		Base <- inst[, "Package"][inst[,"Priority"] %in% c("base", "recommended")]
		sort(setdiff(x, Base))
	}
	
	NAMESPACEFile <- file.path(spec$dir, "NAMESPACE")
	if(file.exists(NAMESPACEFile)){
		NAMESPACE <- readLines(NAMESPACEFile, warn=FALSE)
		atImports <- grep("import", NAMESPACE)
		imports <- NAMESPACE[atImports]
		if(length(atImports)){
			imports <- sapply(strsplit(imports, "(", fixed=TRUE), "[", 2)
			imports <- sapply(strsplit(imports, ")", fixed=TRUE), "[", 1)
			imports <- unique(sapply(strsplit(imports, ",", fixed=TRUE), "[", 1))
			imports <- discardBasePackages(imports)
		}
	}
	
	return(imports)
}
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
addImportsToDESCRIPTION <- function(spec, cpp=FALSE){
	
	# Get the README and NEWS file paths:
	DESCRIPTIONFile <- file.path(spec$dir, "DESCRIPTION")
	
	# Add imports from the NAMESPACE file (via 'spec'):
	if(length(spec$imports)){
		use_package_with_min_version <- function(pck, pckversion){
			min_version <- if(length(pckversion[[pck]])) pckversion[[pck]] else NULL
			usethis::use_package(pck, type="imports", min_version=min_version)
		}
		
		lapply(spec$imports, use_package_with_min_version, pckversion=spec$pckversion)
		
		#cat("Imports:\n		", file=DESCRIPTIONFile, append=TRUE)
		#cat(paste(spec$imports, collapse=",\n		"), file=DESCRIPTIONFile, append=TRUE)
		#cat("", file=DESCRIPTIONFile, append=TRUE)
	}
	# Add also the suggests:
	if(length(spec$suggests)){
		lapply(spec$suggests, usethis::use_package, type="suggests")
	}
	# Add also the linkingto:
	if(length(spec$linkingto)){
		lapply(spec$linkingto, usethis::use_package, type="LinkingTo")
	}
	
	return(DESCRIPTIONFile)
}
#' 
#' @export
#' @rdname getDESCRIPTION
#' 
getPkgname <- function(spec){
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
title_Rstox <- function(version = "1.0"){
	"Running StoX functionality independently in R"
}

description_Rstox <- function(version = "1.0"){
	"The package Rstox contains most of the functionality of the stock assesment utility StoX, which is an open source application fo acoustic-trawl and swept-area survey estimation."
}

details_Rstox <- function(version = "1.0"){
	c(
		"The core funciton of the package is \\code{\\link{getBaseline}} which runs a StoX project and retrieves the output and input parameters. The functions \\code{\\link{runBootstrap}} and \\code{\\link{imputeByAge}} are used by StoX for estimating the variance of the survey estimate. The functions \\code{\\link{getReports}} and \\code{\\link{getPlots}} are used to run report and plot funcitons relevant for the type of StoX project.",
		"Rstox supports a variety of other uses, such as downloading biotic and acoustic data from the Norwegian Marine Data Center through \\code{\\link{getNMDinfo}} and \\code{\\link{getNMDdata}}. The data are placed in StoX projects, enabling the data to be read using \\code{\\link{getBaseline}}. The function \\code{\\link{readXMLfiles}} can be used to simply read an acoustic or biotic xml file into R memory (via a temporary StoX project). The simpler function \\code{\\link{downloadXML}} reads an arbitrary XML file into a list. It is also possible to write acoustic and biotic XML file in the MND echosounder (version 1.0) and biotic (version 1.4 and 3.0) format.",
		"Rstox also contains functions for generating and reporting parallel or zigzag transect lines for use in a survey through \\code{\\link{surveyPlanner}}.",
		"Soon to be implemented is running the Estimated Catch at Age (ECA) model develped by the Norwegian Computing Center and the Norwegian Institute of Marine Research."
	)
}

.onLoad_Rstox <- function(version = "1.0"){
	out <- c(
		".onLoad <- function(libname, pkgname){", 
		"\tif(Sys.getenv(\"JAVA_HOME\")!=\"\") Sys.setenv(JAVA_HOME=\"\")", 
		"\t# Initiate the Rstox envitonment:", 
		"\tDefinitions <- initiateRstoxEnv()", 
		"\t# Set the Java memory:", 
		"\tsetJavaMemory(Definitions$JavaMem)", 
		"} "
	)
	out <- paste(out, collapse="\n")
	#function(libname, pkgname){
	#	if(Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
	#	# Initiate the Rstox envitonment:
	#	Definitions <- initiateRstoxEnv()
	#	# Set the Java memory:
	#	setJavaMemory(Definitions$JavaMem)
	#} 
	
	return(out)	
}

.onAttach_Rstox <- function(version = "1.0"){
	
	fun <- ".onAttach <- function(libname, pkgname){"
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

misc_Rstox <- function(version = "1.0"){
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
title_RstoxFramework <- function(version = "1.0"){
	"The engine of StoX"
}

description_RstoxFramework <- function(version = "1.0"){
	"This package contains all functions and framwork for running a StoX project."
}

details_RstoxFramework <- function(version = "1.0"){
	c(
		"The RstoxFramework package is the engine of the stock assesment utility StoX, which is an open source application fo acoustic-trawl and swept-area survey estimation.", 
		"The package creates an evironment containing the StoX project(s) in separate environments. Each StoX project consists of a list of data tables holding e.g. biotic and acoustic data, filtered versions of the data, strata system, definitios of primary sampling units, accompanied by a list of specifications of the StoX processes comprising the StoX project. A StoX process is an R function taking as input the project name, the input data and parameters used by the function.",
		"The package replaces the old Java library in StoX versions prior to StoX 4.0."
	)
}

authors_RstoxFramework <- function(version = "1.0"){
	list(
		list(given="Arne Johannes", family="Holmin",    role=c("cre", "aut"), email="arnejh@hi.no"), 
		list(given="Ibrahim",       family="Umar",      role=c("aut")),
		list(given="Edvin",         family="Fuglebakk", role=c("aut")),
		list(given="Aasmund",       family="Skaalevik", role=c("aut")),
		list(given="Sindre",        family="Vatnehol",  role=c("aut")),
		list(given="Esmael Musema", family="Hassen",    role=c("aut")),
		list(given="Espen",         family="Johnsen",   role=c("aut")),
		list(given="Atle",          family="Totland",   role=c("aut")),
		list(given="Mikko Juhani",  family="Vihtakari", role=c("aut"))
	)
}

.onLoad_RstoxFramework <- function(version = "1.0"){
    out <- c(
        ".onLoad <- function(libname, pkgname){", 
        "\t# Initiate the Rstox envitonment:", 
        "\tinitiateRstoxFramework()", 
        "} "
    )
    paste(out, collapse="\n")
}
##########

##### RstoxData: #####
title_RstoxData <- function(version = "1.0"){
	"Read, filter and write input/output data for StoX"
}

description_RstoxData <- function(version = "1.0"){
	"This package contains functions for reading, filtering and writing XML files and possibly other files used as input/output to StoX."
}

details_RstoxData <- function(version = "1.0"){
	"The RstoxData package contains functions for reading, filtering and writing biotic, acoustic and landing data as XML files. Filtering can be done by R syntax such as longitude > 10, or by pre defined functions such as inside()."
}

authors_RstoxData <- function(version = "1.0"){
	list(
		list(given="Ibrahim",       family="Umar",      role=c("cre", "aut"), email="ibrahim.umar@hi.no"), 
		list(given="Mikko Juhani",  family="Vihtakari", role=c("aut")),
		list(given="Arne Johannes", family="Holmin",    role=c("aut"))
	)
}
##########

##### RstoxECA: #####
title_RstoxECA <- function(version = "1.0"){
	"Estimated Catch at Age with Rstox"
}

description_RstoxECA <- function(version = "1.0"){
	"This package is used to run the Estimated Catch at Age model through the Reca package developed by the Norwegian Computing Center."
}

details_RstoxECA <- function(version = "1.0"){
	"The estimated catch at age (ECA) model uses the correlation structure in biotic (fishery independent) data to distribute age readings from cathes (landings) onto the biotic data. The ECA model is described in Hirst, D., Aanes, S., Storvik, G., Huseby, R. B., & Tvete, I. F. (2004). Estimating catch at age from market sampling data by using a Bayesian hierarchical model. Journal of the Royal Statistical Society: Series C (Applied Statistics), 53(1), 1-14."
}

authors_RstoxECA <- function(version = "1.0"){
	list(
		list(given="Arne Johannes", family="Holmin",    role=c("cre", "aut"), email="edvin.fuglebakk@hi.no"), 
		list(given="Edvin",         family="Fuglebakk", role=c("aut"))
	)
}
##########

##### RstoxSurveyPlanner: #####
title_RstoxSurveyPlanner <- function(version = "1.0"){
	"Survey design of acoustic-trawl and swept area surveys"
}

description_RstoxSurveyPlanner <- function(version = "1.0"){
	"This package generates parallel or zig zag transects for use in acoustic-trawl and swept area surveys."
}

details_RstoxSurveyPlanner <- function(version = "1.0"){
	"The RstoxSurveyPlanner package is a tool for generating parallel and zig zag transects for acoustic-trawl and swept-area surveys. Several methods for zig zag survey design are implemented, including the equal space design by Strindberg, S., & Buckland, S. T. (2004). Zigzag survey designs in line transect sampling. Journal of Agricultural, Biological, and Environmental Statistics, 9(4), 443. To garantee equal coverage the package includes the zig zag survey design presented by Harbitz, A. (2019). A zigzag survey design for continuous transect sampling with guaranteed equal coverage probability. Fisheries Research, 213, 151-159."
}

authors_RstoxSurveyPlanner <- function(version = "1.0"){
	list(
		list(given="Arne Johannes", family="Holmin",   role=c("cre", "aut"), email="arnejh@hi.no"), 
		list(given="Sindre",        family="Vatnehol", role=c("cre", "aut"), email="sindre.vatnehol@hi.no"), 
		list(given="Alf",           family="Harbitz",  role=c("aut")), 
		list(given="Espen",         family="Johnsen",  role=c("aut"))
	)
}
##########

##### RstoxTempdoc: #####
title_RstoxTempdoc <- function(version = "1.0"){
	"Temporary package for documenting the RstoxFramework package"
}

description_RstoxTempdoc <- function(version = "1.0"){
	"This package is merely for documenting RstoxFramework."
}

details_RstoxTempdoc <- function(version = "1.0"){
	"This package will be deleted once the development of the RstoxFramework package has reached a version with identical or expectedly differing output as StoX 3.0."
}

authors_RstoxTempdoc <- function(version = "1.0"){
	list(
		list(given="Arne Johannes", family="Holmin",  role=c("cre", "aut"), email="arnejh@hi.no"), 
		list(given="Atle",          family="Totland", role=c("aut"))
	)
}
##########

##### RstoxBuild: #####
title_RstoxBuild <- function(version = "1.0"){
	"Package for building all Rstox packages"
}

description_RstoxBuild <- function(version = "1.0"){
	"This package contains functionality for building the Rstox packages (Rstox, RstoxFramework, RstoxData, RstoxECA, RstoxSurveyPlanner, RstoxTempdoc, and even RstoxBuild), and semi-automated testing of Rstox though test projects."
}

details_RstoxBuild <- function(version = "1.0"){
	"The package defines titles, descriptions, dependencies, authors, install instructions and other info for all the packages. All changes to authors, descriptions, suggests and other outputs of the function \\code{packageSpecs} should be changed in this package, and not in the individual packages. The package also contains functionality for semi-automated testing of Rstox on a set of test projects."
}

authors_RstoxBuild <- function(version = "1.0"){
	list(
		list(given="Arne Johannes", family="Holmin", role=c("cre", "aut"), email="arnejh@hi.no")
	)
}
##########


##### Utility functions: #####
# Function to get a package documentation object:
getPackageItem <- function(name, spec, packageName=NULL, version=NULL){
	# If not given (default) search for the object containing the spec:
	if(length(spec[[name]]) == 0){
		# The 'packageName' must be given:
		if(length(packageName) == 0){
			stop("The 'packageName' must be given if the package documentation object is not present in 'spec' and is searched for in memory.")
		}
		
		# Define the object name as the concatination of the 'name' and the package name, separated by underscore:
		objectName <- paste(name, packageName, sep="_")
		if(exists(objectName)){
			object <- get(objectName)
		}
		else{
			object <- getDefault(name)
		}
		#else{
		#	stop(paste("The package documentation object", objectName, "does not exist. Please define it as a string vector or a function with one parameter 'version' returning a string vector."))
		#}
		
		# If the object is a function of the version, apply the function:
		if(is.function(object) && "version" %in% names(formals(object))){
			object <- object(version)
		}
		# If the object is still a function, deparse it to print to file:
		if(is.function(object)){
			object <- paste(name, "<-", paste(deparse(object, control="all"), collapse="\n"))
			
		}
	}
	else{
		object <- spec[[name]]
	}
	
	return(object)
}
# Funciton to get the install path to GitHub:
getGitHub_InstallPath <- function(packageName = "Rstox", version = NULL, githubRoot = "https://github.com/StoXProject", ref = NULL){
	# Get the relative GitHub path for the specific release:
	path <- file.path(basename(githubRoot), packageName)
	# Add the release version:
	if(length(version)){
		path <- paste0(path, "@v", version)
	}
	path <- deparse(path)
	
	# Add the ref, which could be the deleop branch:
	if(length(ref)){
		path <- paste0(path, ", ref = ", deparse(ref))
	}
	
	# Construct and return the install string:
	string <- paste0("devtools::install_github(", path, ")")
	return(string)
}
# Function to get the link to the (online) NEWS file on GitHub depending on the :
getGitHub_NewsLink <- function(packageName = "Rstox", version = "1.0", githubRoot = "https://github.com/StoXProject"){
	file.path(githubRoot, packageName, "blob", if(isMaster(version)) "master" else "alpha", "NEWS")
}
# Function to get the relevant release notes:
getNews_old <- function(NEWSfile, version = "1.0"){
	if(file.exists(NEWSfile)){
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
getNews <- function(NEWSfile, version = "1.0", NEWSBullet="*", READMEBullet="#"){
	if(file.exists(NEWSfile)){
		# Strip off "#", and extract the version string:
		getPos <- function(pattern, string){
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
		thisl <- sub(NEWSBullet, "", thisl, fixed=TRUE)
		thisl <- paste0(READMEBullet, " ", seq_along(thisl), ". ", thisl)
	}
	else{
		thisl <- NULL
	}
	
	return(thisl)
}
# Is the release a master/beta release (judging from the number of dots in the version)?:
isMaster <- function(version = "1.0"){
	length(gregexpr(".", version, fixed=TRUE)[[1]]) == 1
}
# Get one author string:
getAuthor <- function(x){
	# Deparse and add parameter names:
	out <- lapply(x, deparse)
	out <- paste(names(out), out, sep=" = ", collapse=", ")
	# Enclose in a person function:
	out <- paste0(
		"person(",
		out, 
		")"
	)
	return(out)
}
# Get one all author strings:
getAuthors <- function(x){
	if(is.list(x)){
		if(!is.list(x[[1]])){
			x <- list(x)
		}
	}
	#else if(length(x) == 2){
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
getDefault <- function(name){
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
##########

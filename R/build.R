########## Attempt to write a general build method which takes package name as input, and has different README, .onLoad, .onAttach and other stuff specified in separate functions for the available packages: ##########

# 0.	Rstox
# 1.	RstoxFramework
# 2.	RstoxData
# 3.	RstoxFDA
# 4.	RstoxSurveyPlanner
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
	importToNamespace = NULL, 
	internal.dependencies = NULL, 
	internal.suggests = NULL, 
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
	type = c("patch", "minor", "major"), 
	prerelease = FALSE, 
	date = NULL, 
	avoid_compileAttributes_error = FALSE, 
	VignetteBuilder = NULL, 
	debugNAMESPACE = FALSE
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
		importToNamespace = importToNamespace, 
		internal.dependencies = internal.dependencies, 
		internal.suggests = internal.suggests, 
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
		type = type,
		prerelease = prerelease, 
		date = date, 
		avoid_compileAttributes_error = avoid_compileAttributes_error, 
		VignetteBuilder = VignetteBuilder
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
	#try(lapply(.libPaths(), function(x) utils::remove.packages(spec$packageName, x)), silent = TRUE)
	
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
	                        #"\"", sort(globalVariables), "\"", 
	                        "\"", globalVariables, "\"", 
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
	
	##### Write the onUnload to the pkgname file: #####
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
	
	# For RstoxFramework check that the current version is the one used in the OfficialRstoxFrameworkVersions.txt:
	#if(spec$packageName == "RstoxFramework" && !prerelease) {
    if(spec$packageName == "RstoxFramework") {
	        
	    OfficialRstoxFrameworkVersionsFile <- file.path(spec$dir, "inst", "versions", "OfficialRstoxFrameworkVersions.txt")
	    OfficialRstoxFrameworkVersions <- data.table::fread(OfficialRstoxFrameworkVersionsFile)
	    # Find and delete any existing line with the current Framework version:
	    atCurrentRstoxFramework <- which(OfficialRstoxFrameworkVersions$RstoxFramework == spec$version)
	    if(length(atCurrentRstoxFramework)) {
	        OfficialRstoxFrameworkVersions <- OfficialRstoxFrameworkVersions[-atCurrentRstoxFramework]
	    }
	    
	    # Get the order of the depedent Rstox packages, as listed in OfficialRstoxFrameworkVersions.txt:
	    DependentRstoxPackagesOrder <- sub("\\_.*", "", read.csv(textConnection(tail(OfficialRstoxFrameworkVersions, 1)$Dependencies), header = FALSE))
	    DependentRstoxPackages <- subset(spec$imports,  startsWith(names(spec$imports), "Rstox"))
	    DependentRstoxPackages <- DependentRstoxPackages[DependentRstoxPackagesOrder]
	    
	    # Get the order of the depedent Rstox packages, as listed in OfficialRstoxFrameworkVersions.txt:
	    OptionalDependentRstoxPackagesOrder <- sub("\\_.*", "", read.csv(textConnection(tail(OfficialRstoxFrameworkVersions, 1)$OptionalDependencies), header = FALSE))
	    OptionalDependentRstoxPackages <- subset(spec$suggests,  startsWith(names(spec$suggests), "Rstox"))
	    OptionalDependentRstoxPackages <- OptionalDependentRstoxPackages[OptionalDependentRstoxPackagesOrder]
	    
	    # Add the current version, with the latest versions of the dependencies from GitHub
	    newStoXVersion <- packageSpecsGeneral(
	        packageName = "StoX", 
	        accountName = accountName, 
	        version = version, 
	        date = NULL, 
	        rootDir = NULL, 
	        type = type, 
	        prerelease = prerelease
	    )$version
        
	    # Delete the last row if it has the new StoX version:
	    if(utils::tail(OfficialRstoxFrameworkVersions$StoX, 1) == newStoXVersion) {
	        OfficialRstoxFrameworkVersions <- utils::head(OfficialRstoxFrameworkVersions, -1)
	    }
        
	    newRow <- data.table(
	        StoX = newStoXVersion, 
	        RstoxFramework = spec$version, 
	        Dependencies = paste(names(DependentRstoxPackages), DependentRstoxPackages,  collapse = ",",  sep = "_"), 
	        OptionalDependencies = paste(names(OptionalDependentRstoxPackages), OptionalDependentRstoxPackages,  collapse = ",",  sep = "_"), 
	        Official = isOfficial(spec$version), 
	        Date = data.table::as.IDate(spec$date)
	    )
	    
	    OfficialRstoxFrameworkVersions <- rbind(
	        OfficialRstoxFrameworkVersions, 
	        newRow
	    )
	    
	    data.table::fwrite(OfficialRstoxFrameworkVersions, OfficialRstoxFrameworkVersionsFile, sep = "\t")
	    
	}

	##### Create documentation: #####
	# Remove current documentation and NAMESPACE, and then re-document without C++:
	unlink(file.path(spec$dir, "man"), recursive = TRUE, force = TRUE)
	unlink(NAMESPACEFile, force = TRUE)
	
	# If troubles with installation occurs, such as "pkgdir must refer to the directory containing an R package", copy the NAMESPACE file from GitHub before devtools::document:
	if(debugNAMESPACE) {
	    message("If troubles with installation occurs, such as \"pkgdir must refer to the directory containing an R package\", copy the NAMESPACE file from GitHub or from a local file before devtools::document")
	    browser()
	}
	is_online <- function(site = "https://raw.githubusercontent.com/StoXProject/repo/master/README.md") {
	    tryCatch({
	        readLines(site, n = 1)
	        TRUE
	    },
	    warning = function(w) invokeRestart("muffleWarning"),
	    error = function(e) FALSE)
	}
	if(avoid_compileAttributes_error && is_online()) {
	    # Copy the NAMESPACE file from GitHub:
	    GitHubNAMESPACEFile <- file.path(
	        "https://raw.githubusercontent.com", 
	        spec$accountName, 
	        spec$packageName, 
	        "master/NAMESPACE"
	    )
	    localNAMESPACEFile <- file.path(
	        spec$dir, 
	        "NAMESPACE"
	    )
	    suppressWarnings(file.remove(localNAMESPACEFile))
	    download.file(GitHubNAMESPACEFile, localNAMESPACEFile)
	}
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
	        
	        # roxygen2::roxygenize prepares cpp (before we used usethis::use_rcpp(), which is not necessary expecct for the first time the package is created???):
	        roxygen2::roxygenize(spec$dir)
		}
		# Also delete shared objects for safety (run this manually if problems like this occurs:
	    # dyn.load(dll_copy_file) : unable to load shared object (mach-o file, but is an incompatible architecture  'x86_64', need "arm64e" or "arm64"
		sharedObjects <- list.files(spec$src, pattern = "\\.o|so$", full.names = TRUE)
		unlink(sharedObjects, recursive = TRUE, force = TRUE)
	}
	
	##### Run R cmd check with devtools: #####
	if(check) {
		#devtools::check(pkg=spec$dir)
	    rcmdcheck::rcmdcheck(
	        spec$dir, 
	        args = c("--no-manual", "--as-cran"), 
	        error_on = "error", 
	        check_dir = "check", 
	        # Only used to check for circular dependencies, so we use the official StoX repo:
	        repos = c("https://stoxproject.github.io/repo", "https://cloud.r-project.org")
        )
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
	importToNamespace = importToNamespace, 
	internal.dependencies = NULL, 
	internal.suggests = NULL, 
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
	type = c("patch", "minor", "major"), 
	prerelease = FALSE, 
	date = NULL, 
	avoid_compileAttributes_error = FALSE, 
	VignetteBuilder = NULL
) {
	
	
    
    specGeneral <- packageSpecsGeneral(
        packageName = packageName, 
        accountName = accountName, 
        version = version, 
        date = date, 
        rootDir = rootDir, 
        type = type, 
        prerelease = prerelease
    )
	
	# Assure that imports, suggests and linkingto are named lists:
	imports <- asNamedList(imports)
	suggests <- asNamedList(suggests)
	linkingto <- asNamedList(linkingto)
	
	#if(length(remotes) && length(internal.dependencies)) {
	#    stop("remotes and internal.dependencies cannot be set at the same time. Use either remotes, which specifies to install from the GitHub account, or internal.dependencies, which uses the additional_repositories.")
	#}
	#if(length(remotes) && length(internal.suggests)) {
	#    stop("remotes and internal.suggests cannot be set at the same time. Use either remotes, which specifies to install from the GitHub account, or internal.suggests, which uses the additional_repositories.")
	#}
	
	## Get remotes version:
	#remotes_versions <- NULL
	#remotes_strings <- NULL
	#if(length(remotes)) {
	#    remotes_versions <- lapply(remotes, getHighestRelease, includePrerelease = FALSE)
	#    names(remotes_versions) <- remotes
	#    
	#    # Add the remotes to the imports:
	#    imports <- c(
	#        imports, 
	#        remotes_versions
	#    )
	#    
	#    # Also get the remotes strings (for use in the Remotes field in DESCRIPTION):
	#    remotes_strings <- paste0(
	#        remotes, 
	#        "@", 
	#        getRstoxPackageVersionString(
	#            packageName = remotes, 
	#            version = unlist(remotes_versions)
	#        )
	#    )
	#}
	
	
	# Get internal.dependencies version:
	internal.dependencies_versions <- NULL
	#if(length(internal.dependencies) && length(additional_repositories)) {
	if(length(internal.dependencies)) {
	    
	    browser()
	    
	    #internal.dependencies_versions <- lapply(internal.dependencies, getHighestRelease, includePrerelease = prerelease)
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
	# Get internal.suggests version:
	internal.suggests_versions <- NULL
	#if(length(internal.suggests) && length(additional_repositories)) {
	if(length(internal.suggests)) {
	    
	    if(!is.list(internal.suggests)) {
	        #internal.suggests_versions <- lapply(internal.suggests, getHighestRelease, includePrerelease = prerelease)
	        internal.suggests_versions <- lapply(internal.suggests, getHighestRelease)
	        names(internal.suggests_versions) <- internal.suggests
	        ## Mark these as exact dependencies by setting them as list:
	        #internal.dependencies_versions <- lapply(internal.dependencies_versions, as.list)
	    }
	    else {
	        internal.suggests_versions <- internal.suggests
	    }
	    
	    # Add the remotes to the imports:
	    suggests <- c(
	        suggests, 
	        internal.suggests_versions
	    )
	}
	
	
	# Construct the output list:
	spec <- list(
	    # Paths, names and dependencies:
		Rversion = Rversion, 
		imports = imports, 
		suggests = suggests, 
		linkingto = linkingto, 
		#remotes = remotes, 
		importToNamespace = importToNamespace, 
		additional_repositories = additional_repositories, 
		#remotes_versions = remotes_versions, 
		#remotes_strings = remotes_strings, 
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
		license = license, 
		#NEWSfile = NEWSfile, 
		#NEWS = NEWS
		avoid_compileAttributes_error = avoid_compileAttributes_error, 
		VignetteBuilder = VignetteBuilder
	)
	spec <- c(specGeneral, spec)
	
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


packageSpecsGeneral <- function(
    packageName, 
    accountName = "StoXProject", 
    repo = c("https://stoxproject.github.io/testingRepo", "https://stoxproject.github.io/repo"), 
    version = "current", 
    date = NULL, 
    rootDir = NULL, 
    type = c("patch", "minor", "major"), 
    prerelease = FALSE
) {
    
    # If the packageName is a string with no slashes and does not exist as a directory, locate the directories of the developers defined in the 
    if(is.character(packageName) && !file.exists(packageName) && !grepl('\\\\|/', packageName)) {
        if(length(rootDir) == 0) {
            user <- Sys.info()["user"]
            rootDir <- data.table::fread(system.file("extdata", "rootDir.txt", package="RstoxBuild"), sep = ";")
            rootDir <- rootDir$rootDir[rootDir$user == user]
            if(grepl(" ", rootDir, fixed = TRUE)) {
                stop("The directory specified in the file ", system.file("extdata", "rootDir.txt", package="RstoxBuild"), " for the current user (", user, ") contains spaces, which are not allowed for RstoxBuild. Please instruct the developer to change the path in the rootDir.txt file")
            }
        }
        # The path to the package source code folder should contain a folder named by the package name, containing various optional files and folders like "test" or "temp", and a sub folder also named by the oackage name, which is the folder containing the package source code with DESCRIPTION, NAMESPACE, sub folder "R" etc.
        packageName <- file.path(rootDir, packageName, packageName)
    }
    
    # Extract the package name from the path to the package source code folder:
    dir <- packageName
    packageName <- basename(packageName)
    
    # Get version:
    if(identical(version, "current")) {
        # Use the releases for StoX and the testingRepo for the Rstox packages:
        if(packageName == "StoX") {
            version <- incrementHighestRelease_old_using_GitHub_releasaes(packageName, type = type, prerelease = prerelease, accountName = accountName)
        }
        else {
            #version <- getCurrentVersion(packageName)
            version <- incrementHighestRelease(packageName, type = type, prerelease = prerelease, repo = repo)
        }
        
    }
    
    
    spec <- list(
        # Paths, names and dependencies:
        dir = dir, 
        packageName = packageName, 
        accountName = accountName, 
        version = version, 
        date = if(length(date)) date else as.character(Sys.Date())
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
	if(length(spec$VignetteBuilder)) {
        out <- c(
           out, 
            list("VignetteBuilder" = spec$VignetteBuilder)
        )
    }
	
	#if(spec$parallelTest) {
	#    out[["Config/testthat/edition"]] = 3
	#    out[["Config/testthat/parallel"]] = "true"
	#}
	
	out <- paste(names(out), out, sep=": ", collapse="\n")
	# Add a new line ending the file:
	out <- paste0(out, "\n")
	
	return(out)
}
# #' 
# #' @export
# #' @rdname getDESCRIPTION
# #' 
# #getREADME <- function(spec) {
#	
#	# Add devtools to the imports, since it is used for installing: THIS SEEMS WRONG
#	#if(spec$onCran) {
#	#	spec$imports <- c(spec$imports, "devtools")
#	#}
#	
#	# Write package and R version in the first two lines. THIS SHOULD NEVER BE CHANGED, SINCE STOX READS THESE TWO LINES TO CHECK VERSIONS:
#	header <- c(
#		paste0("# ", spec$packageName, " version: ", spec$version, " (latest ", if(isMaster(version=spec$version)) "beta" else "alpha", ", ", format(Sys.time(), "%Y-%m-%d"), ")"), 
#		paste0("# R version: ", spec$Rversion)
#	)
#	
#	# If available instruct the user to install from CRAN:
#	if(spec$onCran) {
#		install <- c(
#			paste0("# Install the ", spec$packageName, "package:"), 
#			paste0("utils::install.packages(", spec$packageName, ")")
#		)
#	}
#	# Install from GitHub, either with or without first installing dependencies:
#	else{
#		## Define install of dependencies:
#		#if(length(spec$imports)) {
#		#	install <- c(
#		#		paste0("# Install the packages that ", spec$packageName, " depends on. Note that this updates all the specified packages to the late#st (binary) version:"), 
#		#		paste0("dep.pck <- c(\"", paste0(spec$imports, collapse="\", \""), "\")"), 
#		#		""
#		#	)
#		#}
#		#else{
#		#	install <- NULL
#		#}
#		
#		## Add install of the package:
#		#install <- c(
#		#	install, 
#		#	paste0("# Install ", spec$packageName, " from GitHub using the devtools package:"), 
#		#	getGitHub_InstallPath(
#		#		packageName = spec$packageName, 
#		#		accountName = spec$accountName, 
#		#		version = spec$version
#		#	), 
#		#	""
#		#)
#		
#		# Add the alternative install:
#	    install <- getGitHub_InstallPath(
#			packageName = spec$packageName, 
#			accountName = spec$accountName, 
#			version = getRstoxPackageVersionString(
#			    packageName = spec$packageName, 
#			    version = spec$version
#			), 
#			additional_repositories = spec$additional_repositories
#		)
#	}
#	
#	releaseNotes <- c(
#		paste0("# Release notes for ", spec$packageName, "_", spec$version, ":"), 
#		spec$NEWS, 
#		"", 
#		paste0(
#			"# For historical release notes see ", 
#			getGitHub_NewsLink(
#				packageName = spec$packageName, 
#				accountName = spec$accountName, 
#				version = spec$version
#			)
#		)
#	)
#	
#	# Combine into a list and add space between each paragraph:
#	README <- list(
#		header,
#		paste("#", spec$description), 
#		paste("#", spec$details), 
#		install, 
#		spec$misc, 
#		releaseNotes
#	)
#	README <- README[lengths(README) > 0]
#	README <- lapply(README, append, rep("", 1))
#	README <- unlist(README)
#	
#	return(README)
#
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
	## Add the remotes directly:
	#if(length(spec$remotes)) {
	#	# Read the DESCRIPTION file:
	#	DESCRIPTION <- readLines(DESCRIPTIONFile)
	#	# Add remotes:
	#	DESCRIPTION <- c(
	#		DESCRIPTION, 
	#		"Remotes: ", 
	#		paste(
	#			paste0(
	#			    "    ", 
	#				spec$accountName, 
	#				"/", 
	#				spec$remotes, 
	#				"@", 
	#				getRstoxPackageVersionString(
	#					packageName = spec$remotes, 
	#					version = unlist(spec$imports[spec$remotes])
	#			   )
	#	        ), 
	#		    collapse = ", \n"
	#		)
	#	)
	#	writeLines(DESCRIPTION, DESCRIPTIONFile)
	#}
	# Add the additional_repositories directly:
	if(length(spec$additional_repositories)) {
	    # Read the DESCRIPTION file:
	    DESCRIPTION <- readLines(DESCRIPTIONFile)
	    # Add additional_repositories:
	    DESCRIPTION <- c(
	        DESCRIPTION, 
	        "Additional_repositories: ", 
	        paste0("    ", spec$additional_repositories)
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
## Funciton to get the install path to GitHub:
#getGitHub_InstallPath <- function(packageName = "Rstox", accountName = "StoXProject" , version = NULL, ref = NULL, additional_repositories = NULL) {
#	# Get the relative GitHub path for the specific release:
#	path <- getGithubURL(
#		packageName = packageName, 
#		accountName = accountName
#	)
#
#	#path <- file.path(basename(githubRoot), packageName)
#	# Add the release version:
#	#if(length(version)) {
#	#	path <- paste0(path, "@v", version)
#	#}
#	#path <- deparse(path)
#	
#	ref <- version
#	
#	## Add the ref, which could be the deleop branch:
#	#if(length(ref)) {
#	#	path <- paste0(path, ", ref = ", deparse(ref))
#	#}
#	
#	# Construct and return the install string:
#	if(length(additional_repositories)) {
#	    string <- paste0(
#	        "# Install the latest binary:\n", 
#	        "install.packages(", packageName, ", repo = c(\"https://stoxproject.github.io/repo\", \"https://cloud.r-project.org\"))\n", 
#	        "# ... or install the latest version from GitHub:\n", 
#	        "# devtools::install_github(", "\"", path, "\"", ", ref = ", deparse(ref), ")"
#	    )
#	}
#	else {
#	    string <- paste0(
#	        "# Install the latest GitHub release:\n", 
#	        "devtools::install_github(", "\"", path, "\"", ", ref = ", deparse(ref), ")"
#	    )
#	}
#	
#	string <- c(
#        string, 
#        paste0(
#            "# ... or install the develop version from GitHub:\n", 
#            "# devtools::install_github(", "\"", path, "\"", ", ref = \"develop\")"
#        )
#    )
#	
#	
#	return(string)
#}
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



getReleases_old_using_GitHub_releasaes <- function(packageName, accountName = "StoXProject", all.releases = FALSE) {
    
    API <- githubPaths("api")
    URL_releases <- paste(API, accountName, packageName, "releases", sep = "/")
    #URL_tags <- paste(API, accountName, packageName, "tags", sep = "/")
    print(URL_releases)
    tryCatch(
        {
            ### # Access the API using the GITHUB_PAT (personal access token)
            ### pat <- Sys.getenv("GITHUB_PAT")
            ### if(nchar(pat)) {
            ###     api_data <- httr::GET(URL_releases, httr::authenticate(Sys.getenv("GITHUB_PAT"), "x-oauth-basic", "basic"))
            ### }
            ### else {
            ###     api_data <- httr::GET(URL_releases)
            ###     warning("Please create a personal access token with the following procedure: Use `usethis::create_github_token()` to go the the GitHub page where you need to log in and click the green 'Generate token' button at the bottom of the page, but be sure to create personalized note in the required field (you will be reminded of this if not). Then copy this to the clipboard and use `usethis::edit_r_environ()` to open the .Renivron file. Add the token as `GITHUB_PAT=12345678901234567890` (replace with the copied token) and end the file with a line space. Save and close the file, and restart R to make the change effective.")
            ### }
            api_data <- httr::GET(URL_releases, httr::authenticate("x-oauth-basic", "basic"))
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
        #valid <- startsWith(releases, packageName)
        #validReleases <- releases[valid]
        #if(length(validReleases) == 0) {
        #    stop("No valid releases")
        #}
        # Get versions:
        #versions <- sub("^[^-v]*", "", validReleases)
        versions <- sub(".*-v", "", releases)
        
        # Also try to remove the package name:
        versions <- sub(paste0(packageName, "-v"), "", versions)
        versions <- sub(paste0(packageName, "v"), "", versions)
        versions <- sub("^v", "", versions)
        
        # Get the three individual numbers, which are separated by dot:
        versionsNumeric <- strsplit(versions, ".", fixed = TRUE)
        
        # Split also the last element by "-" to identify prerelease:
        versionsNumeric <- lapply(versionsNumeric, function(x) {x[length(x)] <- strsplit(x[length(x)], "-", fixed = TRUE); return(unlist(x))})
        
        ## Extract only three numbers, excluding any additional dots:
        #versionsNumeric <- lapply(versionsNumeric, utils::head, 3)
        
        # Trim any characters from punctuation and on:
        versionsNumeric <- lapply(versionsNumeric, function(x) gsub("(.*)[[:punct:]].*", "\\1", x))
        versionsNumeric <- lapply(versionsNumeric, as.numeric)
        
        # Append NAs for those which may have less than 4 numbers ("major", "minor", "patch", "prerelease"):
        versionsNumeric <- lapply(versionsNumeric, function(x) append(x, rep(NA, 4 - length(x))))
        
        # Convert to data.table:
        versionsNumeric <- lapply(versionsNumeric, as.list)
        
        # Rbind into data.table:
        versionsNumeric <- data.table::rbindlist(versionsNumeric)
        
        names(versionsNumeric) <- c("major", "minor", "patch", "prerelease")
        
        return(versionsNumeric)
    }
}


getHighestRelease_old_using_GitHub_releasaes <- function(packageName, accountName = "StoXProject", includePrerelease = TRUE) {
    
    # Get release version numbers:
    versionsNumeric <- getReleases_old_using_GitHub_releasaes(packageName, accountName = accountName, all.releases = FALSE)
    # If there are no versions, return 0.0.1:
    if(length(versionsNumeric) == 0) {
        versionsString <- "0.0.1"
        return(versionsString)
    }
    
    # Get the semantic versions:
    versions <- versionsNumeric[, paste(paste(major, minor, patch, sep = "."), prerelease, sep = "-")]
    versions <- sub("-NA", "", versions)
    versions <- semver::parse_version(versions)
    
    versionsSorted <- sort(versions)
    
    if(!includePrerelease) {
        hasPrerelease <- sapply(versionsSorted, function(x) nchar(semver::render_version(x)$prerelease) > 0)
        highestVersion <- max(versionsSorted[!hasPrerelease])
    }
    else {
        highestVersion <- max(versionsSorted)
    }
    
    highestVersion <- as.character(highestVersion)
    
    
    
    #highestVersion <- max(versions)
    #highestVersion <- semver::render_version(highestVersion)[c("major", "minor", "patch", "prerelease")]
    #
    #if(!includePrerelease) {
    #    if(nchar(highestVersion$prerelease)) {
    #        
    #    }
    #    highestVersion <- paste(semver::render_version(highestVersion)[c("major", "minor", "patch")], collapse = ".")
    #}
    #else {
    #    highestVersion <- as.character(highestVersion)
    #}
    
    return(highestVersion)
}

incrementHighestRelease_old_using_GitHub_releasaes <- function(packageName, type = c("patch", "minor", "major"), prerelease = FALSE, accountName = "StoXProject", includePrerelease = TRUE) {
    
    # Get the increment type:
    type <- match.arg(type)
    
    # Get the highest release:
    highestRelease <- getHighestRelease_old_using_GitHub_releasaes(packageName, accountName = accountName, includePrerelease = includePrerelease)
    
    nextVersion <- incrementVersion(version = highestRelease, type = type, prerelease = prerelease)
    
    ## Use the semver package to define the svptr object and the rendered list of version numbers:
    #highestRelease <- semver::parse_version(highestRelease)[[1]]
    #highestReleaseList <- semver::render_version(highestRelease)
    #
    ## If the highest release is a prerelease, increment 0:
    #hasPrerelease <- highestReleaseList$prerelease != ""
    #increment <- 1L - as.integer(hasPrerelease)
    #
    ## Increment the Rstox preselease number:
    #if(prerelease) {
    #    nextVersion <- semver::increment_version(highestRelease, field = type, value = increment)
    #    nextVersion <- semver::reset_version(nextVersion, field = "prerelease", value = incrementPrerelease(highestReleaseList$prerelease))
    #}
    #else {
    #    nextVersion <- semver::increment_version(highestRelease, field = type, value = increment)
    #}
    #
    #message("Highest release: ", as.character(highestRelease))
    #message("Next release: ", as.character(nextVersion))
    #
    #nextVersion <- as.character(nextVersion)
    
    return(nextVersion)
}


getHighestRelease <- function(packageName, repo = c("https://stoxproject.github.io/testingRepo", "https://stoxproject.github.io/repo")) {
    
    # Get the versions across repos:
    versions <- sapply(repo, getHighestReleaseOneRepo, packageName)
    
    # Get the highest:
    versions_semver <- semver::parse_version(versions)
    highestRelease <- as.character(max(versions_semver))
    
    
    return(highestRelease)
}

getHighestReleaseOneRepo <- function(repo, packageName) {
    
    # Get release version numbers:
    a <- available.packages(repos = repo)
    # If there are no versions, return 0.0.1:
    if(NROW(a) == 0) {
        return("0.0.1")
    }
    version <- a[a[, "Package"] == packageName, "Version"]
    
    return(version)
}

incrementHighestRelease <- function(packageName, type = c("patch", "minor", "major"), prerelease = FALSE, repo = c("https://stoxproject.github.io/testingRepo", "https://stoxproject.github.io/repo")) {
    
    
    # Get the increment type:
    type <- match.arg(type)
    
    # Get the highest release:
    highestRelease <- getHighestRelease(packageName, repo = repo)
    
    nextVersion <- incrementVersion(version = highestRelease, type = type, prerelease = prerelease)
    
    ## Use the semver package to define the svptr object and the rendered list of version numbers:
    #highestRelease <- semver::parse_version(highestRelease)[[1]]
    #highestReleaseList <- semver::render_version(highestRelease)
    #
    ## If the highest release is a prerelease, increment 0:
    #hasPrerelease <- highestReleaseList$prerelease != ""
    #increment <- 1L - as.integer(hasPrerelease)
    #
    ## Increment the Rstox preselease number:
    #if(prerelease) {
    #    nextVersion <- semver::increment_version(highestRelease, field = type, value = increment)
    #    nextVersion <- semver::reset_version(nextVersion, field = "prerelease", value = incrementPrerelease(highestReleaseList$prerelease))
    #}
    #else {
    #    nextVersion <- semver::increment_version(highestRelease, field = type, value = increment)
    #}
    #
    #message("Highest release: ", as.character(highestRelease))
    #message("Next release: ", as.character(nextVersion))
    #
    #nextVersion <- as.character(nextVersion)
    
    return(nextVersion)
}




getSemverType <- function(version) {
    if(is.character(version)) {
        version <- semver::parse_version(version)[[1]]
    }
    versionList <- semver::render_version(version)
    are0 <- unlist(versionList[c("major", "minor", "patch")]) == 0
    
    
    if(are0[3] && !are0[2]) {
        type <- "minor"
    }
    else if(are0[3] && are0[2]) {
        type <- "major"
    }
    else{
        type <- "patch"
    }
    
    return(type)
}


incrementVersion <- function(version, type = c("patch", "minor", "major"), prerelease = FALSE) {
    
    # Get the increment type:
    type <- match.arg(type)
    # First identify the type of the existing version:
    currentType <- getSemverType(version)
    
    # Use the semver package to define the svptr object and the rendered list of version numbers:
    version <- semver::parse_version(version)[[1]]
    versionList <- semver::render_version(version)
    
    # 1. If moving from a pre-release to release, increment with 0, and simply skip the pre-release number
    # 2. If moving from a pre-release to a pre-release, increment with 0, and increment the pre-release number
    # 3. If moving from a release to pre-release, increment with 1, and increment the pre-release number (from "" to "9001")
    # 4: Otherwise increment with 1
    
    # Condensed, this is the following rules:
    # a: If the current is a pre-release, increment with 0
    
    increment <- ifelse(versionList$prerelease != "", 0L, 1L)
    
    # Increment to next version:
    nextVersion <- semver::increment_version(version, field = type, value = increment)
    
    # Increment the pre-release number if the type is equal:
    if(prerelease) {
        if(currentType == type) {
            prereleaseString <- semver::render_version(version)$prerelease
        }
        else {
            prereleaseString <- semver::render_version(nextVersion)$prerelease
        }
        #nextVersionList <- semver::render_version(nextVersion)
        #nextVersion <- semver::reset_version(nextVersionList, field = "prerelease", value = incrementPrerelease(prereleaseString))
        nextVersion <- semver::reset_version(nextVersion, field = "prerelease", value = incrementPrerelease(prereleaseString))
    }
    
    
    
    
    
    
    #versionList <- semver::render_version(version)
    
    # Skipping this and rather in increment by one always, which also solves the problem that R does no respect semanic versioning:
    ## If the highest release is a prerelease, increment 0:
    #hasPrerelease <- versionList$prerelease != ""
    #increment <- 1L - as.integer(hasPrerelease)
    
    
    
    
    
    ### # Increment the Rstox preselease number:
    ### increment <- 1L
    ### if(prerelease) {
    ###     # First identify the type of the existing version:
    ###     currentType <- getSemverType(version)
    ###     
    ###     #if(currentType == type) {
    ###     #    increment <- 0L
    ###     #}
    ###     
    ###     # Do not increment the release if we are in a pre-release, as the function incrementPrerelease() takes care of that:
    ###     versionList <- semver::render_version(version)
    ###     if(versionList$prerelease != "") {
    ###         increment <- 0L
    ###     }
    ###     
    ###     # Increment to next version:
    ###     nextVersion <- semver::increment_version(version, field = type, value = increment)
    ###     
    ###     # Increment the pre-release number if the type is equal:
    ###     if(currentType == type) {
    ###         prereleaseString <- semver::render_version(version)$prerelease
    ###     }
    ###     else {
    ###         prereleaseString <- semver::render_version(nextVersion)$prerelease
    ###     }
    ###     
    ###     nextVersionList <- semver::render_version(nextVersion)
    ###     nextVersion <- semver::reset_version(nextVersionList, field = "prerelease", value = incrementPrerelease(prereleaseString))
    ### }
    ### else {
    ###     nextVersion <- semver::increment_version(version, field = type, value = increment)
    ### }
    
    message("Highest release: ", as.character(version))
    message("Next release: ", as.character(nextVersion))
    
    nextVersion <- as.character(nextVersion)
    
    return(nextVersion)
}





incrementPrerelease_string <-  function(x, prereleaseString = "alpha", prereleaseSeparator = ".", increment = 1L) {
    if(x == "") {
        newnum <- 1
    }
    else {
        num <- as.numeric(sub(paste0(".*", prereleaseString, prereleaseSeparator), '', x))
        newnum <- num + increment
    }
    
    paste0(prereleaseString, prereleaseSeparator, newnum)
}
incrementPrerelease <-  function(x, increment = 1L) {
    if(x == "") {
        newnum <- 9001
    }
    else {
        num <- as.numeric(x)
        newnum <- num + increment
    }
    
    return(as.character(newnum))
}



asNamedList <- function(x, values) {
    if(!is.list(x)) {
        x <- structure(vector("list", length(x)), names = x)
    }
    return(x)
}
##########




















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
#'
prepareStoX <- function(
    accountName = "StoXProject", 
    version = "current", 
    date = NULL, 
    rootDir = NULL, 
    type = c("patch", "minor", "major"), 
    prerelease = FALSE
) {
    
    # Get the specs such as ersion and date:
    specGeneral <- packageSpecsGeneral(
        packageName = "StoX", 
        accountName = accountName, 
        version = version, 
        date = date, 
        rootDir = rootDir, 
        type = type, 
        prerelease = prerelease
    )
    
    # Add the currently installed R version in the message in R connection:
    RConnectionDlg.html_path <- file.path(specGeneral$dir, "frontend", "src", "app", "dlg", "RConnectionDlg.html")
    addInstalledRToRConnectionDlg(RConnectionDlg.html_path)
    
    
    # Read package.json and change the version:
    package.json_path <- file.path(specGeneral$dir, "package.json")
    updateVersionInPackage.json(package.json_path, specGeneral$version) 
    # Read frontend/package.json and change the version:
    package.json_path <- file.path(specGeneral$dir, "frontend", "package.json")
    updateVersionInPackage.json(package.json_path, specGeneral$version) 
    
    
    # Copy and check OfficialRstoxFrameworkVersions.txt from the develop branch:
    OfficialRstoxFrameworkVersionsFilePath <- file.path(specGeneral$dir, "srv/OfficialRstoxFrameworkVersions.txt")
    download.file(
        "https://raw.githubusercontent.com/StoXProject/RstoxFramework/develop/inst/versions/OfficialRstoxFrameworkVersions.txt", 
        OfficialRstoxFrameworkVersionsFilePath
    )
    #}
    
    
    # Check the StoX version in the OfficialRstoxFrameworkVersions.txt:
    OfficialRstoxFrameworkVersions <- data.table::fread(OfficialRstoxFrameworkVersionsFilePath)
    vesionInOfficialRstoxFrameworkVersions <- utils::tail(OfficialRstoxFrameworkVersions$StoX, 1)
    if(vesionInOfficialRstoxFrameworkVersions != specGeneral$version) {
        warning("The last row of OfficialRstoxFrameworkVersions.txt must contain the current StoX version (was ", vesionInOfficialRstoxFrameworkVersions, ", should be ", specGeneral$version, ").")
    }
    
    
    latestOfficialVesion <- utils::tail(subset(OfficialRstoxFrameworkVersions, Official)$StoX, 1)
    
    # Check the StoX version in the Official_StoX_versions.md:
    Official_StoX_versionsMDFilePath <- file.path(specGeneral$dir, "Official_StoX_versions.md")
    # Check if the latest version is included:
    if(! grepl(latestOfficialVesion, readChar(Official_StoX_versionsMDFilePath, 1e6)) ) {
        stop("The file ", Official_StoX_versionsMDFilePath, " does not contain the latest official version. Please add that.")
    }
    
    # Check the StoX version in the Official_StoX_versions.txt:
    Official_StoX_versionsTXTFilePath <- file.path(specGeneral$dir, "srv/Official_StoX_versions.txt")
    # Check if the latest version is included:
    if(! grepl(latestOfficialVesion, readChar(Official_StoX_versionsTXTFilePath, 1e6)) ) {
        stop("The file ", Official_StoX_versionsTXTFilePath, " does not contain the latest official version. Please add that.")
    }
    
    
    # Check that the NEWS has been updated for an official release:
    NEWS.md_path <- file.path(specGeneral$dir, "NEWS.md")
    NEWS.md <- readLines(NEWS.md_path)
    atVersions <- startsWith(tolower(trimws(NEWS.md)), "# stox v")
    versions <- gsub(".*# stox v(.+) \\(.*", "\\1", tolower(NEWS.md[atVersions]))
    dates <- gsub(".*\\((.+)\\).*", "\\1", NEWS.md[atVersions])
    if(! specGeneral$version %in% versions) {
        stop("The file ", NEWS.md_path, " must be updated for all releases. Contained ",paste(versions, collapse = ", "), ". Needed ", specGeneral$version, ".")
    }
    
    # Set the last official as the new version if not a pre-release:
    if(prerelease) {
        lastOfficialVersion <- getHighestRelease_old_using_GitHub_releasaes(packageName = "StoX", accountName = accountName, includePrerelease = FALSE)
    }
    else {
        lastOfficialVersion <- specGeneral$version
    }
    newsDate <- dates[versions == lastOfficialVersion]
    
    # Read README.md and change the version:
    README.md_path <- file.path(specGeneral$dir, "README.md")
    README.md <- readLines(README.md_path)
    atSee <- startsWith(README.md, "See [release")
    if(sum(atSee) != 1) {
        stop("The file ", README.md_path, " does not contain a line of the following form: See [release notes for StoX 3.1.0](https://github.com/StoXProject/StoX/blob/master/NEWS.md#Stox-v310-2021-06-18).")
    }
    
    
    
    stringToreplace <- gsub(".*release notes for StoX (.+)\\]\\(.*", "\\1", README.md[atSee])
    README.md[atSee] <- sub(stringToreplace, lastOfficialVersion, README.md[atSee])
    
    # Change version in link to NEWS:
    versionDateString <- paste0(gsub("\\.", "", lastOfficialVersion),  "-",  newsDate)
    stringToreplace <- gsub(".*/NEWS.md\\#stox-v(.+)\\).*", "\\1", README.md[atSee])
    README.md[atSee] <- gsub(stringToreplace, versionDateString, README.md[atSee])
    
    
    # Change version in link to release:
    atDownload <- startsWith(README.md, "Download StoX from (https")
    if(sum(atDownload) != 1) {
        stop("The file ", README.md_path, " does not contain a line of the following form (starting with): Download StoX from (https://github.com/StoXProject/StoX/releases/tag/v3.1.0)")
    }
    
    stringToreplace <- gsub(".*releases/tag/v(.+)\\)\\. For.*", "\\1", README.md[atDownload])
    README.md[atDownload] <- gsub(stringToreplace, lastOfficialVersion, README.md[atDownload])
    
    
    # Write the changes:
    tmp <- tempdir()
    file.copy(README.md_path, tmp)
    message("File ",  README.md_path, "backed up to ", tmp)
    writeLines(README.md, README.md_path)
}


addInstalledRToRConnectionDlg <- function(RConnectionDlg.html_path) {
    RConnectionDlg.html <- readLines(RConnectionDlg.html_path)
    Rver <- strsplit(base::version[['version.string']], ' ')[[1]][3]
    RConnectionDlg.html <- sub("(?:(?:\\d)[.-]){2,}(?:\\d)", Rver, RConnectionDlg.html)
    writeLines(RConnectionDlg.html, RConnectionDlg.html_path)
}

updateVersionInPackage.json <- function(package.json_path, version) {
    package.json <- readLines(package.json_path)
    # Find the line starting with '"version": ':
    atVersionKey <- "\"version\": "
    atVersion <- startsWith(trimws(package.json),  atVersionKey)
    if(sum(atVersion) != 1) {
        stop("The file ", package.json_path, " does not contain a line starting with ", atVersionKey, ".")
    }
    package.json[atVersion] <- paste0("  ", atVersionKey, "\"",  version, "\",")
    
    # Write the changes:
    tmp <- tempdir()
    file.copy(package.json_path, tmp)
    message("File ",  package.json_path, "backed up to ", tmp)
    writeLines(package.json, package.json_path)
}

isOfficial <- function(version) {
    #endsWith(version, ".0.0") | endsWith(version, ".0")
    !grepl("-9", version)
}


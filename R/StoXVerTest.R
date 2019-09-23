#*********************************************
#*********************************************
#' Get the platform ID of an operating system
#'
#' @param var	The element of Sys.info() used as identifyer of the platform.
#'
#' @export
#'
getPlatformID <- function(){
	#getPlatformID <- function(var="release"){
	# 2019-02-05: Removed the 'release' to make file paths shorter:
	paste(.Platform$OS.type, paste(strsplit(Sys.info()["release"], " ", fixed=TRUE)[[1]], collapse="_"), sep="_")
}

#*********************************************
#*********************************************
#' Small funciton to get a list of the file paths to the sub folders of the test directory (ProjOrig, Proj, Output, Diff)
#'
#' @param x	The test directory
#'
#' @export
#'
getTestFolderStructure <- function(x){
	
	# Accept the platform specific directory:
	if("ProjOrig" %in% list.dirs(x, recursive=FALSE, full.names=FALSE)){
		x <- dirname(x)
	}
	
	platformFolderName <- getPlatformID()
	
	list(
		StagedProjOrig = file.path(x, "StagedProjOrig"), 
		#Notes = file.path(x, platformFolderName, "Notes"), 
		ProjOrig = file.path(x, platformFolderName, "ProjOrig"), 
		ProjOrig1 = file.path(x, platformFolderName, "ProjOrig", "Rstox_1.0_StoXLib_1.0"), 
		Proj = file.path(x, platformFolderName, "Proj"), 
		Output = file.path(x, platformFolderName, "Output"), 
		Diff = file.path(x, platformFolderName, "Diff"))
}

#*********************************************
#*********************************************
#' Get the latest directory of 
#'
#' @param dir	The directory of subdirectories named with the Rstox and stox-lib versions.
#' @param op	The operator used to get the latest. Do not mess with this unless you know what you are doing.
#' @param n		The number of directories to return, as used in utils::tail().
#'
#' @export
#' @noRd
#'
getLatestDir <- function(dir, op="<", n=1){
	
	if(length(dir)==0){
		return(NULL)
	}
	
	
	# Get the Rstox and stox-lib versions:
	current <- sapply(getRstoxVersion(), as.character)
	current <- data.frame(package=names(current), version=unlist(current), stringsAsFactors=FALSE)
	currentString <- paste(current$version, sep="_", collapse="_")
	currentNumeric <- version2numeric(current)
	current <- versionScaled(currentNumeric)
	
	# List all directories:
	All <- list.dirs(dir, recursive=FALSE)
	All <- All[grep("Rstox", All)]
	if(length(All)==0){
		warning(paste0("No projects in the test folder '", dir, "'"))
	}
	
	
	# Get the Rstox and stox-lib versions encoded in the folder names:
	versions <- strsplit(basename(All), "_")
	# Pick out every other element, which are the version strings:
	versionStrings <- lapply(versions, extractVersionstrings)
	versionStrings <- lapply(versionStrings, version2numeric)
	versions <- sapply(versionStrings, versionScaled)
	
	# Order All and versions in increasing order by the scale:
	o <- order(versions)
	versions <- versions[o]
	All <- All[o]
	
	# There has to be at least one previous version:
	latest <- do.call(op, list(versions, current))
	if(!any(is.na(latest)) && any(latest)){
		#return(All[max(which(latest))])
		# Allow for specifying e.g. the third latest:
		if(is.integer(n)){
			atlatest <- sort(which(latest))
			out <- All[atlatest[length(atlatest) - n + 1]]
		}
		else{
			out <- All[tail(sort(which(latest)), n)]
		}
		return(out)
	}
	else{
		warning(paste0("No directories with Rstox version before Rstox_StoXLib version \"", currentString, "\" in the directory \"", dir, "\""))
		return(NULL)
	}
}

# Function for converting the Rstox version to a numeric value suitable for sorting, by multiplying each digit in the version number by scaling factor which are largest for the first digits (e.g., Rstox_1.10.3 gives 1 * 1e4 + 10 * 1e2 + 3 = 11003):
extractVersionstrings <- function(x){
	# Remove duplicated, since the diff paths have e.g. Rstox twice in the folder name:
	packageAndVersion <- data.frame(
		package = x[seq(1, length(x), by=2)], 
		version = x[seq(2, length(x), by=2)], 
		stringsAsFactors = FALSE
	)
	
	
	dup <- duplicated(packageAndVersion$package)
	packageAndVersion <- packageAndVersion[!dup, ]
	
	# packageAndVersion$version
	packageAndVersion
}
version2numeric <- function(x){
	if(is.data.frame(x)){
		temp <- lapply(strsplit(x$version, ".", fixed=TRUE), as.numeric)
		x$version <- sapply(temp, function(y) sum(y * 10^(6 - 2 * seq_along(y))))
	}
	else{
		temp <- lapply(strsplit(x, ".", fixed=TRUE), as.numeric)
		x <- sapply(temp, function(y) sum(y * 10^(6 - 2 * seq_along(y))))
	}
	x
}
versionScaled <- function(x){
	power <- list(
		Rstox = 3, 
		StoXLib = 2, 
		eca = 1
	)
	
	scale <- unlist(power[x$package])
	
	scale <- 10^(7 * scale)
	
	sum(x$version * scale)
}

#*********************************************
#*********************************************
#' Copy the 'n' latest directories of the folders 'toCopy' from the local 'from' to the central 'to' directory.
#'
#' @param from		The local directory holding the version testing.
#' @param to		The central directory holding the version testing.
#' @param toCopy	The sub folders to copy files from.
#' @param overwrite	Logical: If TRUE, overwrite the files on the central directory.
#' @param msg		Logical: If TRUE, print progress to the console.
#' @param op,n		See \code{\link{getLatestDir}}.
#'
#' @export
#' @rdname copyLatestToServer
#' @noRd
#' @keywords internal
#'
copyLatestToServer <- function(local, server, toCopy=c("Diff", "Output", "ProjOrig"), overwrite=TRUE, msg=FALSE, op="<", n=1){
	
	# Function for copying from one subdirectory:
	copyLatestOne <- function(folder, local, server, overwrite=TRUE, msg=FALSE, op="<", n=1){
		local <- getLatestDir(local[[folder]], op=op, n=n)
		if(length(local)){
			# Check for the existence of the folder (as opposed to using 'overwrite' in file.copy(), which copies all files which do not exist in the destination).
			if(file.exists(server[[folder]]) && !overwrite){
				warning(paste0("The folder ", server[[folder]], " already exists and was not overwritten. Use overwrite=TRUE to overwrite from ", local))
			}
			else{
				temp <- file.copy(local, server[[folder]], recursive=TRUE, overwrite=overwrite)
				if(msg){
					cat("Copied", local, "to", server[[folder]], "\n")
				}
			}
		}
	}
	

	# Get the folder structure of the local and central directory:
	local <- getTestFolderStructure(path.expand(local))
	#server <- getTestFolderStructure(path.expand(server))
	# Quick fix of duplicated consecutive platform ID:
	server <- getTestFolderStructure(path.expand(dirname(server)))
	lapply(server, dir.create, recursive=TRUE, showWarnings=FALSE)
	
	# Copy for all specified subdirectories:
	invisible(lapply(toCopy, copyLatestOne, local, server, overwrite=overwrite, msg=msg, op=op, n=n))
}
#'
#' @export
#' @rdname copyLatestToServer
#' @noRd
#' @keywords internal
#'
copyStagedProjOrigFromServer <- function(server, local, overwrite=TRUE, op="<", n=1){
    localStagedProjOrig <- getTestFolderStructure(path.expand(local))$StagedProjOrig
    localStagedProjOrigList <- list.dirs(localStagedProjOrig, recursive=FALSE)
	serverStagedProjOrig <- getTestFolderStructure(path.expand(server))$StagedProjOrig
	serverStagedProjOrigList <- list.dirs(serverStagedProjOrig, recursive=FALSE)
	
	# Copy the folders missing in the local StagedProjOrig:
	missingStagedProjOrig <- serverStagedProjOrigList[!basename(serverStagedProjOrigList) %in% basename(localStagedProjOrigList)]
	
	if(length(missingStagedProjOrig)){
	    message("Copying the following folders from the server to the local system:\n\t", paste(missingStagedProjOrig, collapse="\n\t"))
	    
	    file.copy(missingStagedProjOrig, localStagedProjOrig, recursive=TRUE, overwrite=overwrite)
	}
	else{
	    message("No staged original projects copied from the server to the local system")
	}
}
#'
#' @export
#' @rdname copyLatestToServer
#' @noRd
#' @keywords internal
#'
copyStagedProjOrigLocal <- function(dir){
    # Get the 
    StagedProjOrig <- getTestFolderStructure(path.expand(dir))$StagedProjOrig
    ProjOrig <- getTestFolderStructure(path.expand(dir))$ProjOrig
   
    
     localStagedProjOrigList <- list.dirs(localStagedProjOrig, recursive=FALSE)
    serverStagedProjOrig <- getTestFolderStructure(path.expand(server))$StagedProjOrig
    serverStagedProjOrigList <- list.dirs(serverStagedProjOrig, recursive=FALSE)
    
    # Copy the folders missing in the local StagedProjOrig:
    missingStagedProjOrig <- serverStagedProjOrigList[!basename(serverStagedProjOrigList) %in% basename(localStagedProjOrigList)]
    
    if(length(missingStagedProjOrig)){
        message("Copying the following folders from the server to the local system:\n\t", paste(missingStagedProjOrig, collapse="\n\t"))
        
        file.copy(missingStagedProjOrig, localStagedProjOrig, recursive=TRUE, overwrite=overwrite)
    }
    else{
        message("No staged original projects copied from the server to the local system")
    }
}

#*********************************************
#*********************************************
#' Get the path to the server, depending on the local platform (Mac, Windows).
#'
#' @param root	A list of specifyers for the root directory to the central server.
#' @param path	The relative path from the root.
#'
#' @export
#' @keywords internal
#'
getServerPath <- function(root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoXVerTest"){
	root <- root[[.Platform$OS.type]]
	if(length(root)==0){
		stop(paste0("The OS.type ", .Platform$OS.type, " does not match any of the names of 'root' (", paste(names(root), collapse=", "), ")"))
	}
	# There should be one directory per system, named by the output of getPlatformID():
	server <- file.path(root, path, getPlatformID())
	if(!file.exists(server)){
		warning(paste0("The server location ", server, " does not exist. Please create it manually for the given platform ID (obtained by getPlatformID()): ", getPlatformID()))
	}
	server
}

#*********************************************
#*********************************************
#' Copy the test run to the central srever.
#'
#' @param root		A list of specifyers for the root directory to the central server.
#' @param path		The relative path from the root.
#' @param toCopy	The sub folders to copy files from.
#' @param overwrite	Logical: If TRUE, overwrite the files on the central directory.
#' @param msg		Logical: If TRUE, print progress to the console.
#' @param n			The number of runs (one runfor each version tested) to copy.
#'
#' @export
#' @keywords internal
#'
copyCurrentToServer <- function(root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoXVerTest", toCopy=c("Diff", "Output", "ProjOrig"), overwrite=FALSE, msg=FALSE, n=1){
	server <- getServerPath(root=root, path=path)
	
	dir <- getProjectPaths()$projectRoot
	dir <- file.path(dir, "StoXVerTest")
	
	copyLatestToServer(dir, server, toCopy=toCopy, overwrite=overwrite, msg=msg, op="<=", n=n)
}

#*********************************************
#*********************************************
#' Function for running the r scripts of a project and copying the relevant output files to the "Output" directory:
#'
#' @param projectName		The path to the project.
#' @param progressFile		The paht to the progress file.
#' @param outputDir			The path to the directory in which to put the output files to be compared for the project (everything in "output" except trash files, baseline and baseline report output, and the project.xml file).
#'
#' @export
#' @keywords internal
#'
runProject <- function(projectName, progressFile, outputDir, ind=NULL){
	
	requireNamespace("Rstox")
	
	RstoxVersion <- getRstoxVersion()
	
	# Run the scripts and print info to the progress file:
	write(paste0(now(TRUE), "Starting project", paste0(" ", ind), ": ", projectName), progressFile, append=TRUE)
	
	
	cat(paste0("\n\n------------------------------------------------------------\nRunning project", paste0(" ", ind), ": ", projectName, ":\n"))
	
	# Run the baseline and baseline report (the latter with input=NULL):
	# The parameter 'modelType', enabling reading Baseline Report, was introduced in 1.8.1:
	# 2018-04-19 Added saveProject() since we wish to pick up changes in the project.xml files:
	#if(RstoxVersion$Rstox > "1.8"){
	if(version2numeric(getRstoxVersion()$Rstox) > version2numeric("1.8")){
		write(paste0(now(TRUE), "Running Baseline and Baseline Report"), progressFile, append=TRUE)
		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="baseline", input=c("par", "proc"), drop=FALSE)
		saveProject(projectName)
		baselineReportOutput <- getBaseline(projectName, exportCSV=TRUE, modelType="report", input=c("par", "proc"), drop=FALSE)
		saveProject(projectName)
	}
	else{
		write(paste0(now(TRUE), "Running Baseline"), progressFile, append=TRUE)
		baselineOutput <- getBaseline(projectName, exportCSV=TRUE, input=c("par", "proc"), drop=FALSE)
		saveProject(projectName)
	}
	
	# Get the path to the scripts to run:
	r_script <- file.path(projectName, "output", "R", "r.R")
	rreport_script <- file.path(projectName, "output", "R", "r-report.R")
	# Generate the r scripts:
	generateRScripts(projectName)

	write(paste0(now(TRUE), "Running r.R"), progressFile, append=TRUE)
	if(file.exists(r_script)){
		source(r_script)
	}
	write(paste0(now(TRUE), "Running r-report.R"), progressFile, append=TRUE)
	if(file.exists(rreport_script)){
		source(rreport_script)
	}
	write(paste0(now(TRUE), "Ending project", paste0(" ", ind), ": ", projectName), progressFile, append=TRUE)
	write("", progressFile, append=TRUE)
	closeProject(projectName)
	
	# Copy output files to the output directory:
	unlink(outputDir, recursive=TRUE, force=TRUE)
	suppressWarnings(dir.create(outputDir, recursive=TRUE))
	output <- file.path(projectName, "output")
	file.copy(output, outputDir, recursive=TRUE)
	
	# Delete trash:
	trash <- list.dirs(outputDir)
	trash <- trash[grep("trash", trash)]
	unlink(trash, recursive=TRUE, force=TRUE)
	
	# Save also the output from baseline and baseline report to an RData file:
	save(baselineOutput, file=file.path(outputDir, "baselineOutput.RData"))
	#if(RstoxVersion$Rstox > "1.8"){
	if(version2numeric(getRstoxVersion()$Rstox) > version2numeric("1.8")){
		save(baselineReportOutput, file=file.path(outputDir, "baselineReportOutput.RData"))
	}
	
	# Copy the project.xml file:
	from <- getProjectPaths(projectName)$projectXML
	to <- file.path(outputDir, "project.xml")
	file.copy(from=from, to=to, overwrite=TRUE)
	
	cat("\n")
}

# Convenience function for getting the Rstox version string, which is a possible output from getRstoxVersion() as of approximately Rstox_1.9:
getRstoxVersionString <- function(){
	#if(getRstoxVersion()$Rstox < "1.8.1"){
	if(version2numeric(getRstoxVersion()$Rstox) < version2numeric("1.8.1")){
		RstoxVersionString <- getRstoxVersion()
		RstoxVersionString <- paste(names(RstoxVersionString), unlist(lapply(RstoxVersionString, as.character)), sep="_", collapse="_")
	}
	else{
		RstoxVersionString <- getRstoxVersion("string")
	}
	RstoxVersionString
}

# On Windows 10 the file.path() using .Platform$file.sep is "/", but cmd fc only accepts "\\". Thus we hach the file.path funciton to accommodate this:
file.path_Windows10 <- function(...){
	release <- Sys.info()["release"]
	if(tolower(.Platform$OS.type) == "windows" && startsWith(release, "10")){
		paste(..., sep="\\")
	}
	else{
		file.path(...)
	}
}

# Function for deleting all output files of a project:
deleteOutput <- function(x){
	requireNamespace("Rstox")
	if(length(x)==1 && !isProject(x[1])){
		x <- list.dirs(x, recursive=FALSE)
	}
	output <- file.path(x, "output")
	files <- list.files(output, recursive=TRUE, full.names=TRUE)
	unlink(files)
	invisible(files)
}

all.equal2 <- function(target, current){
	out <- all.equal(target, current)
	out
}


#*********************************************
#*********************************************
#' Function for running all test projects and comparing outputs with previous outputs.
#'
#' @param root				A list of specifyers for the root directory to the central server.
#' @param path				The relative path from the root.
#' @param copyFromServer	Logical: If TRUE, copy the latest original projects, outputs and diffs in the server to the local directory.
#' @param process			Which steps to run in the testing used mostly to reduce processing time for development and bug fixing.
#' @param diffs				Which diffs to include, also used to reduce processing time.
#' @param nlines			The number of lines to display for diffs between text files.
#'
#' @export
#' @keywords internal
#'
automatedRstoxTest <- function(root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoXVerTest", copyFromServer=TRUE, process=c("run", "diff"),  diffs=c("Rdata", "images", "text", "baseline"), projectInd=NULL, nlines=50, mem.size=16e9, nwarnings=10000, n=1L, skipError=FALSE){
#automatedRstoxTest <- function(dir, copyFromServer=TRUE, process=c("run", "diff"),  nlines=-1L, root=list(windows="\\\\delphi", unix="/Volumes"), path="pc_prog/S2D/stox/StoXAutoTest"){
	
	# Load image packages:
	library(Rstox)
	library(png)
	library(jpeg)
	library(tiff)
	# Load utilities packages:
	library(tools)
	library(R.utils)
	
	setJavaMemory(mem.size)
	oldnwarnings <- options()$nwarnings
	options(nwarnings=nwarnings)  
	
	# The function readBaselineFiles() was introduced in Rstox 1.8.1:
	#if(getRstoxVersion()$Rstox <= "1.8"){
	if(version2numeric(getRstoxVersion()$Rstox) <= version2numeric("1.8")){
		readBaselineFiles <- function(x){
			# Return NULL if no files are given:
			if(length(x)==0){
				return(NULL)
			}
			
			# Function for converting string to logical:
			string2logical <- function(y){
				string2logicalOne <- function(z){
					if(length(z)>0 && any(z %in% c("true", "false"))){
					 	z <- as.logical(z)
					}
					z
				}
				as.data.frame(lapply(y, string2logicalOne), stringsAsFactors=FALSE)
			}

			# Read one text connection:
			if("textConnection" %in% class(x)){
				out <- read.csv(x, sep="\t", stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8", quote=NULL)
				out <- string2logical(out)
			}
			# Read the files:
			else{
				out <- lapply(x, function(y) read.csv(y, sep="\t", stringsAsFactors=FALSE, na.strings="-", encoding="UTF-8", quote=NULL))
				for(i in seq_along(out)){
					out[[i]] <- string2logical(out[[i]])
				}

				# Get the names of the processes and data frames:
				x_split <- strsplit(basename(x), "_")
				#dataFrameNames <- sapply(lapply(x_split, "[", -1), paste, collapse="_")
				#processNames <- sapply(x_split, "[", 2)
				dataFrameNames <- sapply(lapply(x_split, "[", -1), paste, collapse="_")
				processNames <- sapply(x_split, function(y) paste(y[seq(2, length(y)-2)], sep="_"))
				
				# Set the names of the data frames:
				names(out) <- dataFrameNames
				out <- split(out, processNames)
				out <- lapply(out, function(y) if(length(y)==1) y[[1]] else y)
			}
			out
		}
	}

	browser()
	# Set the directory of the test projects:
	server <- getServerPath(root=root, path=path)
	#root <- root[[.Platform$OS.type]]
	#if(length(root)==0){
	#	stop(paste0("The OS.type ", .Platform$OS.type, " does not match any of the names of 'root' (", paste(names(root), collapse=", "), ")"))
	#}
	##root <- ifelse(.Platform$OS.type == "windows", "\\\\delphi", "/Volumes")
	## There should be one directory per system, named by the output of getPlatformID():
	#server <- file.path(root, path, getPlatformID())
	
	# Make sure the paths are expanded:
	server <- path.expand(server)
	# Changed from using a 'dir' parameter to using the default workspace:
	#dir <- path.expand(dir)
	dir <- getProjectPaths()$projectRoot
	dir <- file.path(dir, "StoXVerTest")
	#suppressWarnings(dir.create(dir))
	
	dirList <- getTestFolderStructure(dir)
	# Create the folder structure if missing:
	lapply(dirList, dir.create, recursive=TRUE, showWarnings=FALSE)
	
	# Name the folder for the output files by the time and Rstox version:
	RstoxVersionString <- getRstoxVersionString()
	#folderName <- paste(names(RstoxVersion), unlist(lapply(RstoxVersion, as.character)), sep="_", collapse="_")
	
	# 1. Copy the latest original projects, outputs and diffs on the server to the local directory:
	#if("run" %in% process && copyFromServer){
	if(copyFromServer){
		#cat("Copying original projects from \"", server, "\" to ", dir, "\n", sep="")
		#message("Copying original projects from \"", server, "\" to \"", dir, "\"\n")
		copyStagedProjOrigFromServer(dirname(server), dir)
	}
	
	# Get the latest projects:
	ProjectsDir_original <- getLatestDir(dirList$ProjOrig, n=1)
	Previous_ProjectsList_original <- list.dirs(ProjectsDir_original, recursive=FALSE)
	newProjectsDir_original <- file.path(dirname(ProjectsDir_original), RstoxVersionString)
	# Get paths to the original projects and previous output folders:
	#ProjectsList_original <- list.dirs(ProjectsDir_original, recursive=FALSE)
	ProjectsDir <- dirList$Proj
	Staged_ProjectsDir_original <- file.path(dirList$StagedProjOrig, RstoxVersionString)
	Staged_ProjectsList_original <- list.dirs(Staged_ProjectsDir_original, recursive=FALSE)
	
	# First copy all files from ProjectsDir_original to ProjectsDir
	if("run" %in% process){
		# Delete the Proj directory and the current ProjOrig directory (which will be filled by the contents of the previous ProjOrig directory and that of the current Staged_Projects directory):
		unlink(ProjectsDir, recursive=TRUE, force=TRUE)
		suppressWarnings(dir.create(ProjectsDir))
		unlink(newProjectsDir_original, recursive=TRUE, force=TRUE)
		suppressWarnings(dir.create(newProjectsDir_original))
		
		
		# Copy first projects from last run:
		if(length(Previous_ProjectsList_original)){
			message("Copying projects from \n\t\"", ProjectsDir_original, "\"\n to \n\t", newProjectsDir_original, "\n")
			lapply(Previous_ProjectsList_original, file.copy, newProjectsDir_original, overwrite=TRUE, recursive=TRUE)
		}
		
		# Then copy staged projects:
		if(length(Staged_ProjectsList_original)){
			message("Copying projects from \n\t\"", Staged_ProjectsDir_original, "\"\n to \n\t", newProjectsDir_original, "\n")
			lapply(Staged_ProjectsList_original, file.copy, newProjectsDir_original, overwrite=TRUE, recursive=TRUE)
		}
		
		# List files in the ProjOrig:
		ProjectsList_original <- list.dirs(newProjectsDir_original, recursive=FALSE)
		message("Copying projects from \n\t\"", newProjectsDir_original, "\"\n to \n\t", ProjectsDir, "\n")
		lapply(ProjectsList_original, file.copy, ProjectsDir, overwrite=TRUE, recursive=TRUE)
		
		# Then delete all output files for safety:
		lapply(list.dirs(ProjectsDir, recursive=FALSE), deleteOutput)
	}
	
	# Get all project paths:
	projectPaths <- list.dirs(ProjectsDir, recursive=FALSE)
	
	# Get the outputs directory and the sub directory of the new outputs:
	Output <- dirList$Output
	newOutput <- file.path(Output, RstoxVersionString)
	
	
	# List all projects in the latest and new output directory:
	newOutputList <- file.path(newOutput, basename(projectPaths))
	
	# Then run through all projects, printing progress to a file:
	suppressWarnings(dir.create(dirList$Diff))
	progressFile <- file.path(dirList$Diff, "progress.R")
	unlink(progressFile)
	
	if("run" %in% process){
		if(length(projectPaths)==0){
			stop("'Proj' folder empty or invalid")
		}
		
		if(length(projectInd) == 0){
			projectInd <- seq_along(projectPaths)
		}
		else if(is.function(projectInd)){
			projectInd <- projectInd(seq_along(projectPaths))
		}
		else{
			projectInd <- subset(projectInd, projectInd >= 1 & projectInd <= length(projectPaths))
		}
		
		for(i in projectInd){
			
			if(skipError){
			    tryCatch({
			      runProject(projectName=projectPaths[i], progressFile=progressFile, outputDir=newOutputList[i], ind=i)
			    },
			    error = function(e) {
			    },
			    finally = function(e) {
			    })
			
			}
			else{
				runProject(projectName=projectPaths[i], progressFile=progressFile, outputDir=newOutputList[i], ind=i)
			}
			
			
			#runProject(projectName=projectPaths[i], progressFile=progressFile, outputDir=newOutputList[i], ind=i)
		}
	}
	
	# Copy the projects that were run to a new folder in the ProjOrig, but first delete any output:
	if("run" %in% process){
		suppressWarnings(dir.create(newProjectsDir_original))
		ProjectsList <- list.dirs(ProjectsDir, recursive=FALSE)
		
		# Delete output:
		lapply(ProjectsList, deleteOutput)
		
		lapply(ProjectsList, file.copy, newProjectsDir_original, overwrite=TRUE, recursive=TRUE)
		
		# Also delete the projects in "Proj":
		#unlink(ProjectsDir, recursive=TRUE, force=TRUE)
	}
	
	# Get the lastest sub directory of the previously generated outputs:
	latestOutput <- getLatestDir(dirList$Output, n=1)
	
	if("diff" %in% process && length(latestOutput)){
		VersionComparisonString <- paste(basename(newOutput), basename(latestOutput), sep="_")
	
		#diffdir <- path.expand(file.path(dir, "Diff", paste("Diff", basename(newOutput), basename(latestOutput), sep="_")))
		diffdir <- path.expand(file.path(dirList$Diff, VersionComparisonString))
		setSlashes(diffdir)
		unlink(diffdir, recursive=TRUE, force=TRUE)
		suppressWarnings(dir.create(diffdir))
		
		# Get all files common and different between the old and new run, separated into file types RData, image and text:
		printHeader("1. Common and differing projects and files", progressFile)
		write("{", file=progressFile, append=TRUE)
		
		message("Comparing\n", newOutput, "\nand\n", latestOutput, "\n")
		
		allFiles <- getAllFiles(newOutput, latestOutput, progressFile)
		write("}", file=progressFile, append=TRUE)
		
		
		# Special diff of RData files:
		if("Rdata" %in% diffs){
			printHeader("2. Comparing RData files", progressFile)
			write("\n{", file=progressFile, append=TRUE)
			lapply(allFiles$RDataFiles, diffRData , progressFile=progressFile)
			write("}", file=progressFile, append=TRUE)
		}
		
		# Special diff of images:
		if("images" %in% diffs){
			printHeader("3. Comparing image files", progressFile)
			write("{", file=progressFile, append=TRUE)
			lapply(allFiles$imageFiles, diffImages, progressFile=progressFile, diffdir=diffdir)
			write("}", file=progressFile, append=TRUE)
		}
		
		# Diff text files:
		if("text" %in% diffs){
			printHeader("4. Comparing text files", progressFile)
			write("{", file=progressFile, append=TRUE)
			lapply(allFiles$textFiles, diffTextFiles, progressFile=progressFile, diffdir=diffdir, nlines=nlines)
			write("}", file=progressFile, append=TRUE)
		}
		
		# Diff also the baseline output and the files written by baseline:
		if("baseline" %in% diffs){
			printHeader("5. Comparing Rstox and StoX baseline output", progressFile)
			write("{", file=progressFile, append=TRUE)
			lapply(newOutputList, diffBaseline, progressFile=progressFile)
			write("}", file=progressFile, append=TRUE)
		}
		
		
		write("\nPlease also run the example script on ftp://ftp.imr.no/StoX/Download/Rstox/Examples\n", file=progressFile, append=TRUE)
	
		# Add indentation corresponding to the curly brackets:
		l <- readLines(progressFile)
		n <- double(length(l))
		atStart <- which(startsWith(l, "{")) + 1
		atEnd <- which(startsWith(l, "}"))
		n[atStart] <- n[atStart]  + 1
		n[atEnd] <- n[atEnd] -1
		tabs <- strrep("\t", cumsum(n))
		l <- paste0(tabs, l)
		writeLines(l, progressFile)
		
		# Add warnings():
		d <- warnings()
		d <- paste0("[", formatC(seq_along(d), width=nchar(length(d)), format="d", flag="0"), "]", "\t", d, names(d))
		d <- c(paste("THERE WERE", length(d), " WARNINGS:"), d)
		write(d, progressFile, append=TRUE)
		
		# Copy the progress file to the current diff directory:
		finalProgressFile <- file.path(diffdir, paste0("PROGRESS_", VersionComparisonString, ".R"))
		# The notesFile is a copy of the progress file, in which the reviewer should input comments to each diff. This will be made more automatic in later versions, where the diff will be saved in a list of strings which wil be numbered and each diff must be approved or resolved.
		#finalNotesFile <- file.path(dirList$Notes, paste0("NOTES_", VersionComparisonString, ".R"))
		
		file.copy(progressFile, finalProgressFile, overwrite=TRUE)
		#file.copy(progressFile, finalNotesFile, overwrite=TRUE)
		unlink(progressFile, force=TRUE)
	}
	
	options(nwarnings=oldnwarnings)
}

# Order the sub data frames:
sortByName <- function(x){
	if(length(x)){
		x[order(names(x))]
	}
	else{
		x
	}
}


pasteWithLineShift <- function(...){
	out <- paste0("# ", c(...))
	out <- paste0(out, collapse="\n")
	out
}
	
# Convert to all forward- or all backslashed:
# Forwardslash should work on all systems, so the default is back=FALSE:
setSlashes <- function(x, back=FALSE, platform=NULL){
	if(identical(platform, "windows")){
		back <- TRUE
	}
	if(back){
		gsub("/", "\\", x, fixed=TRUE)
	}
	else{
		gsub("\\", "/", x, fixed=TRUE)
	}
}

# Function for getting the common files:
#' @importFrom tools file_ext
getFilesByExt <- function(dir1, dir2, ext=NULL, recursive=TRUE, ignore.case=TRUE){
	# Function for getting all image files in a vector of files (returning a list with names corresponding to the file extensions):
	getFilesByExtOne <- function(x, ext=NULL){
		if(length(ext)){
			fileext <- tools::file_ext(x)
			if(ignore.case){
				x <- x[tolower(fileext) %in% tolower(ext)]
			}
			else{
				x <- x[fileext %in% ext]
			}
		}
		x
	}
	getMatches <- function(files1, files2, dir1, dir2){
		commonFiles <- intersect(files1, files2)
		# This was an error, the correct would be to use the commonFiles as basename:
		#commonPaths1 <- file.path(dir1, files1)
		#commonPaths2 <- file.path(dir2, files2)
		commonPaths1 <- file.path(dir1, commonFiles)
		commonPaths2 <- file.path(dir2, commonFiles)
		onlyInFirst <- setdiff(files1, files2)
		onlyInSecond <- setdiff(files2, files1)
		list(commonFiles=commonFiles, commonPaths1=commonPaths1, commonPaths2=commonPaths2, onlyInFirst=onlyInFirst, onlyInSecond=onlyInSecond)
	}
	
	
	
	# Get matching and differing files:
	files1 <- getFilesByExtOne(list.files(dir1, recursive=recursive, full.names=FALSE), ext=ext)
	files2 <- getFilesByExtOne(list.files(dir2, recursive=recursive, full.names=FALSE), ext=ext)
	out <- getMatches(files1, files2, dir1, dir2)
	# Add the input directories:
	out$dir1 <- dir1
	out$dir2 <- dir2
	out$projectName <- basename(dir2)
	
	# Set all slashes to the appropriate direction:
	out <- lapply(out, setSlashes, platform = .Platform$OS.type)
	#out <- lapply(out, setSlashes)
	
	out
}

printProjectName <- function(x, progressFile){
	cat(x$projectName, "...", "\n", sep="")
	toWrite <- paste0("\n##### PROJECT: ", x$projectName, ": #####")
	write(toWrite, file=progressFile, append=TRUE)
}

reportFilesIntersects <- function(x, progressFile, type="Projects", addProjectName=FALSE){
	if(addProjectName){
		printProjectName(x, progressFile)
		#toWrite <- paste0("##### ", x$projectName, ": #####")
		#write(toWrite, file=progressFile, append=TRUE)
	}
	
	# (1) Common files:
	toWrite <- paste0("# ", type, " common for both directories")
	# Add the common files, ot the string "NONE":
	if(length(x$commonFiles)){
		toWrite <- paste0(c(
			toWrite, 
			x$dir1, 
			"# and", 
			paste0(x$dir2, ":\n"), 
			paste0("\t", x$commonFiles, collapse="\n"), 
			""), collapse="\n"
		)
	}
	else{
		toWrite  <- paste0(c(
			toWrite, 
			"\tNONE", 
			""), collapse="\n"
		)
	}
	write(toWrite, file=progressFile, append=TRUE)

	# (2) Files only in the first directory:
	toWrite <- paste0(c(
		paste0("# ", type, " present only in the later directory: "), 
		paste0(x$dir1, ":\n"))
	)
	# Add the common files, ot the string "NONE":
	if(length(x$onlyInFirst)){
		toWrite <- paste0(c(
			toWrite, 
			paste0("\t", x$onlyInFirst, collapse="\n"), 
			""), collapse="\n"
		)
	}
	else{
		toWrite  <- paste0(c(
			toWrite, 
			"\tNONE", 
			""), collapse="\n"
		)
	}
	write(toWrite, file=progressFile, append=TRUE)

	# (3) Files only in the second directory:
	toWrite <- paste0(c(
		paste0("# ", type, " present only in the former directory:"), 
		paste0(x$dir2, ":\n"))
	)
	# Add the common files, ot the string "NONE":
	if(length(x$onlyInSecond)){
		toWrite <- paste0(c(
			toWrite, 
			paste0("\t", x$onlyInSecond, collapse="\n"), 
			""), collapse="\n"
		)
	}
	else{
		toWrite  <- paste0(c(
			toWrite, 
			"\tNONE", 
			""), collapse="\n"
		)
	}
	write(toWrite, file=progressFile, append=TRUE)

	
	
	
	## Files only in the first directory:
	#if(length(x$onlyInFirst)){
	#	#toWrite <- paste0("# ", type, " present only in the directory\n# ", x$dir1, ":\n", paste("\t", x$onlyInFirst, collapse="\n"), "\n")
	#	toWrite <- paste0(c(
	#		paste0("# ", type, " present only in the directory"), 
	#		paste0(x$dir1, ":\n"), 
	#		paste0("\t", x$onlyInFirst, collapse="\n"), 
	#		""), collapse="\n"
	#	)
	#}
	#else{
	#	toWrite  <- "NONE"
	#}
	#write(toWrite, file=progressFile, append=TRUE)
	#
	## Files only in the second directory:
	#if(length(x$onlyInSecond)){
	#	#toWrite <- paste0("# ", type, " present only in the directory\n# ", x$dir2, ":\n", paste("\t", x$onlyInSecond, collapse="\n"), "\n")
	#	toWrite <- paste0(c(
	#		paste0("# ", type, " present only in the directory"), 
	#		paste0(x$dir2, ":\n"), 
	#		paste0("\t", x$onlyInSecond, collapse="\n"), 
	#		""), collapse="\n"
	#	)
	#}
	#else{
	#	toWrite  <- "NONE"
	#}
	#write(toWrite, file=progressFile, append=TRUE)
	
}

printHeader <- function(header, progressFile, w=60){
	# Print to console
	cat("\n", header, "...", "\n", sep="")
	
	# Prepare for printing to file:
	ncharHeader <- nchar(header)
	nstars <- (w - ncharHeader - 2) / 2
	hash <- paste(rep("#", w), collapse="")
	hash1 <- paste(rep("#", ceiling(nstars)), collapse="")
	hash2 <- paste(rep("#", floor(nstars)), collapse="")
	header <- paste(hash1, header, hash2)
	header <- paste("", "", hash, header, hash, "", sep="\n")
	# Print to file:
	write(header, file=progressFile, append=TRUE)
}

getAllFiles <- function(dir1, dir2, progressFile){
	# Get the projects of the first and second directory (including common and different projects):
	projects <- getFilesByExt(dir1, dir2, recursive=FALSE)
	
	printHeader("Projects", progressFile, w=30)
	reportFilesIntersects(projects, progressFile=progressFile, type="Projects")
	
	# Get the different files per project, in a list, for clarity:
	RDataFiles <- lapply(seq_along(projects$commonFiles), function(i) getFilesByExt(dir1=projects$commonPaths1[i], dir2=projects$commonPaths2[i], ext="RData"))
	imageFiles <- lapply(seq_along(projects$commonFiles), function(i) getFilesByExt(dir1=projects$commonPaths1[i], dir2=projects$commonPaths2[i], ext=c("png", "jpg", "jpeg", "tif", "tiff")))
	textFiles <- lapply(seq_along(projects$commonFiles), function(i) getFilesByExt(dir1=projects$commonPaths1[i], dir2=projects$commonPaths2[i], ext=c("txt", "xml")))
	
	printHeader("RData files", progressFile, w=30)
	lapply(RDataFiles, reportFilesIntersects, progressFile=progressFile, type="RData files", addProjectName=TRUE)
	printHeader("Image files", progressFile, w=30)
	lapply(imageFiles, reportFilesIntersects, progressFile=progressFile, type="Image files", addProjectName=TRUE)
	printHeader("Text files", progressFile, w=30)
	lapply(textFiles, reportFilesIntersects, progressFile=progressFile, type="Text files", addProjectName=TRUE)
	
	list(RDataFiles=RDataFiles, imageFiles=imageFiles, textFiles=textFiles)
}

# Get diffs between RData files:
diffRData  <- function(files, progressFile){
	diffRData <- function(i, files, progressFile){
		all.equalOne <- function(name, progressFile){
			#write(paste0("\tObject: ", name), file=progressFile, append=TRUE)
			all.equal2(tempenvironment1[[name]], tempenvironment2[[name]])
		}
		
		file <- files$commonFiles[i]
		dir1 <- files$dir1
		dir2 <- files$dir2
		#write(paste0("File: ", file), file=progressFile, append=TRUE)
		# Read the files and diff:
		file1 <- file.path(dir1, file)
		file2 <- file.path(dir2, file)
		assign("tempenvironment1", new.env(), envir=.GlobalEnv)
		assign("tempenvironment2", new.env(), envir=.GlobalEnv)
		x1 <- load(file1, envir=tempenvironment1)
		x2 <- load(file2, envir=tempenvironment2)
		
		diffs <- lapply(x1, all.equalOne, progressFile=progressFile)
		nodiff <- unlist(lapply(diffs, isTRUE))
		
		
		# Print info also for no differences:
		write("{", file=progressFile, append=TRUE)
		out <- paste0(c("# (Code 0) No differences in the following RData files: ", file1, "# and", file2), collapse="\n")
		
		# Print info about different names:
		#if(!identical(x1, x2)){
		if(!isTRUE(all.equal(x1, x2))){
			objectList1 <- paste0("# OBJECTS: ", paste0(x1, collapse=", "), ":")
			objectList2 <- paste0("# OBJECTS: ", paste0(x2, collapse=", "), ":")
			out <- paste0(c("# (Code 1) Non-identical object NAMES in the following RData files: ", file1, objectList1, "# and", file2, objectList2), collapse="\n")
			#out <- paste("# ", c("Non-identical object NAMES in files", file1, objectList1, "and", file2, objectList2))
			#out <- paste(out, collapse="\n# ")
		}
		
		# Print info about different objects:
		if(!all(nodiff)){
			objectList <- paste0("# OBJECTS: ", paste0(x1[!nodiff], collapse=", "), ":")
			
			out <- paste0(c("# (Code 2) Non-identical objects in the following RData files: ", file1, "# and", file2), collapse="\n")
			
			howToInspect <- c(
				paste0("file1 <- \"", setSlashes(file1), "\""),
				paste0("file2 <- \"", setSlashes(file2), "\""),
				"assign(\"tempenvironment1\", new.env(), envir=.GlobalEnv)",
				"assign(\"tempenvironment2\", new.env(), envir=.GlobalEnv)",
				"x1 <- load(file1, envir=tempenvironment1)",
				"x2 <- load(file2, envir=tempenvironment2)", 
				"str(tempenvironment1[[x1]])", 
				"str(tempenvironment2[[x2]])", 
				"all.equal(tempenvironment1[[x1]], tempenvironment2[[x2]])"
				)
			
			out <- paste(out, "# Inspect the differences by using the following code:", sep="\n")
			out <- paste(out, paste0("\t", c(howToInspect), collapse="\n"), sep="\n")
		
			out <- paste(out, paste0(c(objectList), collapse="\n"), sep="\n")
			out <- paste(out, unlist(lapply(diffs[!nodiff], function(x) paste("\t", x, collapse="\n"))), sep="\n")
		}
		write(unlist(out), file=progressFile, append=TRUE)
		write("}\n", file=progressFile, append=TRUE)
	}

	# Compare images:
	printProjectName(files, progressFile)
	#write("\n\n********************", file=progressFile, append=TRUE)
	#write(paste0("***** ", files$projectName, " *****"), file=progressFile, append=TRUE)
	
	write("{", file=progressFile, append=TRUE)
	out <- lapply(seq_along(files$commonFiles), diffRData, files=files, progressFile=progressFile)
	write("}", file=progressFile, append=TRUE)
}

# Get diffs between images:
#' @importFrom tools file_ext
#' @importFrom png readPNG writePNG
#' @importFrom jpeg readJPEG writeJPEG
#' @importFrom tiff readTIFF writeTIFF
diffImages <- function(files, progressFile, diffdir){
	diffImagesOne <- function(file, dir1, dir2, progressFile, diffdir){
		
		if(length(file)==0){
			write("No images", progressFile, append=TRUE)
		}
		
		# Get the read and write functions:
		validExt <- list(png="png", jpeg=c("jpg", "jpeg"), tiff=c("tif", "tiff"))
		ext <- tools::file_ext(file)
		if(ext %in% validExt$png){
			readFun <- png::readPNG
			writeFun <- png::writePNG
		}
		else if(ext %in% validExt$jpeg){
			readFun <- jpeg::readJPEG
			writeFun <- jpeg::writeJPEG
		}
		else if(ext %in% validExt$tiff){
			readFun <- tiff::readTIFF
			writeFun <- tiff::writeTIFF
		}
	
		# Read the files and diff:
		file1 <- file.path(dir1, file)
		file2 <- file.path(dir2, file)
		x1 <- readFun(file1)
		x2 <- readFun(file2)
			
		write("{", file=progressFile, append=TRUE)
		if(!all(x1==x2)){
			x12 <- x1 - x2
			# Modify the diff image to fit the [0, 1] range, and set all identical values to 0:
			#x12 <- (x12 + 1) / 2
			# Take the absolute difference and invert:
			x12 <- 1 - abs(x12)
			#x12[x1 == x2] <- 1
		
			# Paste together the first the diff and the second image:
			dimx <- dim(x12)
			out <- array(double(3 * prod(dimx)), dim=c(dimx[1], 3 * dimx[2], dimx[3]))
			out[, seq_len(dimx[2]),] <- x1
			out[, seq_len(dimx[2]) + dimx[2],] <- x12
			out[, seq_len(dimx[2]) + 2 * dimx[2],] <- x2
			# Reset the alpha-channel, if present, assuming constant alpha throughout the image:
			if(dimx[3]==4){
				out[,,4] <- x1[1,1,4]
			}
			
			# Write to file if differing:
			thisdiffdir <- file.path(diffdir, basename(dir1))
			outfile <- file.path(thisdiffdir, basename(file))
			suppressWarnings(dir.create(thisdiffdir))
			writeFun(out, outfile)
			# Write a log to the progressFile:
			
			
			out <- paste0(c("# (Code 2) Differences in the following images", file1, "# and", file2, paste0("# See an image with the current to the left, the diff in the middle, and the previous image to the right in the file \"", outfile, "\"")), collapse="\n")
			write(out, progressFile, append=TRUE)
			
			
			#write(paste0("(Code 2) Images\n\t", file1, " and\n\t", file2, "\ndiffer. See an image with the current to the left, the diff in the middle, and the previous image to the right in the file \n\t", outfile, "\n"), progressFile, append=TRUE)
		}
		else{
			out <- paste0(c("# (Code 0) No differences in the following images", file1, "# and", file2), collapse="\n")
			write(out, progressFile, append=TRUE)
		}
		write("}\n", file=progressFile, append=TRUE)
	}
	
	printProjectName(files, progressFile)
	
	write("{", file=progressFile, append=TRUE)
	out <- lapply(files$commonFiles, diffImagesOne, dir1=files$dir1, dir2=files$dir2, progressFile=progressFile, diffdir=diffdir)
	write("}", file=progressFile, append=TRUE)
}

# Get diff between the previous and new output files:
# 'files' is a list as returned from the function getFilesByExt(), which uses getMatches() to return the following elements: 'commonFiles', 'commonPaths1', 'commonPaths2', 'onlyInFirst', 'onlyInSecond':
diffTextFiles <- function(files, progressFile, diffdir, nlines=50){
	
	diffTextFilesOne <- function(file, dir1, dir2, progressFile, diffdir, nlines){
		file1 <- setSlashes(file.path(dir1, file), platform = .Platform$OS.type)
		file2 <- setSlashes(file.path(dir2, file), platform = .Platform$OS.type)
		nlinesFile1 <- R.utils::countLines(file1)
		nlinesFile2 <- R.utils::countLines(file2)
		# Write tepmorary files with less lines:
		
		
		
		#tempfile1 <- file.path(tempdir(), "tempfile1")
		#tempfile2 <- file.path(tempdir(), "tempfile2")
		tempfile1 <- file.path_Windows10(tempdir(), "tempfile1")
		tempfile2 <- file.path_Windows10(tempdir(), "tempfile2")
		if(nlinesFile1 > nlines){
			temp <- readLines(file1, n=nlines)
			writeLines(temp, tempfile1)
		}
		else{
			tempfile1 <- file1
		}
		if(nlinesFile2 > nlines){
			temp <- readLines(file2, n=nlines)
			writeLines(temp, tempfile2)
		}
		else{
			tempfile2 <- file2
		}
		
		tempdiff <- file.path(path.expand(diffdir), "tempdiff.txt")
		
		# 2018-04-03 Added by Johnsen: New function to be used with sink(). This is the only way of getting output from FC to file on Windows:
		system1 = function(...) cat(base::system(..., intern = TRUE), sep = "\n")
		
		# Platform dependent diff commands:
		if(.Platform$OS.type == "windows"){
			
			# 2018-04-03 Added by Johnsen: Do not redirect the output to a file, but rather use sink() below instead:
			cmd <- paste0(
				"fc /LB",
				nlines, 
				" ",
				shQuote(tempfile1, type="cmd2"), 
				" ",
				shQuote(tempfile2, type="cmd2")
			)
			
			inspect <- paste0(
				"fc /LB",
				nlines, 
				" ",
				shQuote(file1, type="cmd2"), 
				" ",
				shQuote(file2, type="cmd2")
			)
		}
		else if(.Platform$OS.type == "unix"){
			cmd <- paste0(
				"diff ", 
				shQuote(tempfile1), 
				" ", 
				shQuote(tempfile2)
			)
			
			inspect <- paste0(
				"diff ", 
				shQuote(file1), 
				" ", 
				shQuote(file2)
			)
			
			
		}
		else{
			stop("Unknown system. Must be one of UNIX or Windows")
		}
		
		
		
		# 2018-04-03 Added by Johnsen: Use sink() to get the output from the diff/fc:
		#sink(file=tempdiff, append=TRUE) ## Not sure if all messages should go to the same file. Arne Johannes to decide
		sink(file=tempdiff, append=FALSE) ## Not sure if all messages should go to the same file. Arne Johannes to decide
		system1(cmd)
		# End writing to file
		sink(type = "message")
		sink()
		
		
		### # Run the diff as a system call and print to the temp file:
		### system(cmd)
		### # -x '*.bmp' -x '*.jpeg' -x '*.png' -x '*.tiff' -x '*.RData'
		
		# Read the tempdiff file and append to the progress file:
		#diffinfo <- readLines(tempdiff, n=nlines)
		diffinfo <- readLines(tempdiff)
		# Determine whether the tempdiff reported any diffs
		noDiffUnix <- sum(nchar(diffinfo)) <= 1
		noDiffWindows <- length(grep("no differences encountered", diffinfo))
		
		write("{", file=progressFile, append=TRUE)
		if(noDiffUnix || noDiffWindows){
			out <- paste0(c("# (Code 0) No differences in the following text files", file1, "# and", file2), collapse="\n")
			write(out, progressFile, append=TRUE)
		}
		else{
			out <- paste0(c("# (Code 2) Differences in the following text files", file1, "# and", file2), collapse="\n")
			out <- c(out, paste0("\t", diffinfo))
			
			if(any(c(nlinesFile1, nlinesFile2) > nlines)){
				out <- c(
					out, 
					"\t...", 
					paste0("\tNumber of lines exceeding 'nlines' (", nlines, ") in the files ", file1, " (", nlinesFile1, ") and/or ", file2, " (", nlinesFile2, ")"), 
					paste0(
						"\tInspect the full diff by the following command in the ", 
						if(.Platform$OS.type == "windows") " CMD app on Windows, increasing the number immediately following fc /LB (which \"Sets the maximum consecutive mismatches to the specified
        number of lines\"): " else " Terminal on Mac: ", 
						inspect
					)
				)
			}
			
			
			write(out, progressFile, append=TRUE)
		}
		write("}\n", file=progressFile, append=TRUE)
		
		
		if(file.exists(tempdiff)){
			unlink(tempdiff)
		}
	}

	# Compare text files:
	printProjectName(files, progressFile)
	
	write("{", file=progressFile, append=TRUE)
	out <- lapply(files$commonFiles, diffTextFilesOne, dir1=files$dir1, dir2=files$dir2, progressFile=progressFile, diffdir=diffdir, nlines=nlines)
	write("}", file=progressFile, append=TRUE)
	
	
	return(NULL)
}

#' Get diff between baseline outputs:
diffBaseline <- function(dir, progressFile){
	
	RstoxVersion <- getRstoxVersion()
	
	all.equalRstoxStoX <- function(Rstox, StoX, name, progressFile){
		write_all.equal <- function(name, x, y, progressFile){
			d <- all.equal2(x[[name]], y[[name]])
			Code <- 0
			write("{", file=progressFile, append=TRUE)
			if(!isTRUE(d)){
				Code <- 2
				write(paste0("# (Code 2) Differences in output from process ", name, " from Rstox and StoX"), file=progressFile, append=TRUE)
				write(paste("\t", d), file=progressFile, append=TRUE)
			}
			else{
				write(paste0("# (Code 0) No differences in output from process ", name, " from Rstox and StoX"), file=progressFile, append=TRUE)
			}
			write("}\n", file=progressFile, append=TRUE)
			
			Code
		}
		
		
		namesRstox <- names(Rstox)
		namesStoX <- names(StoX)
		commonDF <- intersect(namesRstox, namesStoX)
		onlyInRstox <- setdiff(namesRstox, namesStoX)
		onlyInStoX <- setdiff(namesStoX, namesRstox)
		
		write(paste0("\n### MODEL TYPE: ", name, "\n"), file=progressFile, append=TRUE)
		
		# Inform files present only in one or the other:
		if(length(commonDF)){
			write("# Data frames common for Rstox and StoX: ", file=progressFile, append=TRUE)
			lapply(paste("\t", commonDF), write, file=progressFile, append=TRUE)
		}
		if(length(onlyInRstox)){
			write("# Data frames only in Rstox: ", file=progressFile, append=TRUE)
			lapply(paste("\t", onlyInRstox), write, file=progressFile, append=TRUE)
		}
		if(length(onlyInStoX)){
			write("# Data frames only in StoX: ", file=progressFile, append=TRUE)
			lapply(paste("\t", onlyInStoX), write, file=progressFile, append=TRUE)
		}
		
		# Compare each data frame og the project:
		unlist(lapply(commonDF, write_all.equal, x=Rstox, y=StoX, progressFile=progressFile))
	}
	
	# Get the baseline and baseline report saved in RData files:
	baselineOutputFiles <- file.path(dir, c("baselineOutput.RData", "baselineReportOutput.RData"))
	present <- file.exists(baselineOutputFiles)
	if(!any(present)){
		return(NULL)
	}
	baselineOutputFiles <- baselineOutputFiles[present]
	
	# Load the data to a list:
	### readBaselineOutputFiles <- function(x){
	### 	mget(load(x))$outputData
	### }
	
	dataFromRstox <- unlist(lapply(baselineOutputFiles, function(x) mget(load(x))), recursive=FALSE)
	dataFromRstox <- lapply(dataFromRstox, "[[", "outputData")
	# Due to a fundamental problem of interpreting the process name from the baseline and baseline report output csv files (ProcessName_OutputType_Level.txt), where _Level may be missing, and any user introduced "_" in the process names will make the interpretation ambigous, we group the processes according to the first element of the process name after separating by underscore. This is done to allow comparison between Rstox and StoX:
	
	#cropProcessName <- function(x){
	#	if(length(x)==0){
	#		return(x)
	#	}
	#	x_names <- names(x)
	#	x_names <- strsplit(x_names, "_")
	#	x_names <- sapply(x_names, head, 1)
	#	#names(x) <- x_names
	#	x <- split(x, x_names)
	#	x <- lapply(x, function(y) if(length(y)==1) y[[1]] else y)
	#	x
	#	#x <- lapply(x, "[[", 1)
	#	#x <- lapply(x, function(y) if(is.list(y) && !is.data.frame(y)) unlist(y, recursive=FALSE) else y)
	#}
	#dataFromRstox <- lapply(dataFromRstox, cropProcessName)
	
	
	# Read also the txt-files from baseline and baseline report:
	baselineDirs <- file.path(dir, "output", "baseline", c("data", "report"))
	baselineFiles <- lapply(baselineDirs, list.files, recursive=TRUE, full.names=TRUE)
	names(baselineFiles) <- c("baselineOutput", "baselineReportOutput")
	
	# Read the data to a list:
	dataFromStoX <- lapply(baselineFiles, readBaselineFiles)
	
	# Keep only the modelType present in the Rstox output file:
	dataFromStoX <- dataFromStoX[names(dataFromRstox)]
	
	# Sort the data by name
	dataFromRstox <- lapply(dataFromRstox, function(x) lapply(x, sortByName))
	dataFromStoX <- lapply(dataFromStoX, function(x) lapply(x, sortByName))
	
	#write("\n", file=progressFile, append=TRUE)
	printProjectName(list(projectName=basename(dir)), progressFile)
	
	write("{", file=progressFile, append=TRUE)
	Code <- unlist(lapply(names(dataFromRstox), function(x) all.equalRstoxStoX(Rstox=dataFromRstox[[x]], StoX=dataFromStoX[[x]], name=x, progressFile=progressFile)))
	
	#if(any(Code==2) && RstoxVersion$Rstox > "1.8"){
	if(any(Code==2) && version2numeric(getRstoxVersion()$Rstox) > version2numeric("1.8")){
		howToInspect <- c(
			paste0("dataFromRstox <- unlist(lapply(c(", paste0("\"", baselineOutputFiles, "\"", collapse=", "), "), function(x) mget(load(x))), recursive=FALSE)"),
			"dataFromRstox <- lapply(dataFromRstox, \"[[\", \"outputData\")", 
			paste0("baselineDirs <- file.path(\"", dir, "\", \"output\", \"baseline\", c(\"data\", \"report\"))"),
			"baselineFiles <- lapply(baselineDirs, list.files, recursive=TRUE, full.names=TRUE)",
			"names(baselineFiles) <- c(\"baselineOutput\", \"baselineReportOutput\")", 
			"dataFromStoX <- lapply(baselineFiles, readBaselineFiles)", 
			"ls.str(dataFromRstox)", 
			"ls.str(dataFromStoX)"
		)
		write("# Inspect the differences by using the following code:", file=progressFile, append=TRUE)
		write(paste0("\t", c(howToInspect), collapse="\n"), file=progressFile, append=TRUE)
	}
	
	write("}", file=progressFile, append=TRUE)
	#write("\n", file=progressFile, append=TRUE)
}


# Function for getting a string with the current time: 
now <- function(brackets=FALSE){
	out <- format(Sys.time(),tz="UTC", "%Y-%m-%d_%H.%M.%S")
	if(brackets){
		out <- paste0("[", out, "] ")
	}
	out
}





#*********************************************
#*********************************************
#' Get the Rstox version and the version of the Java library used by Rstox, on which StoX is built.
#'
#' @param out	The type of object to return.
#'
#' @export
#' @importFrom rJava J
#' @keywords internal
#'
getRstoxVersion <- function(out=c("list", "string"), dependencies=getRstoxDef("internal_dependencies")){
	Rstox.init()
	ver <- list(
		Rstox = as.character(packageVersion("Rstox")), 
		StoXLib = rJava::J("no.imr.stox.model.Project")$RESOURCE_VERSION
	)
	installed <- installed.packages()[, "Package"]
	if(any(dependencies %in% installed)){
		installed <- intersect(dependencies, installed)
		add <- lapply(installed, function(x) as.character(packageVersion(x)))
		names(add) <- installed
		ver <- c(ver, add)
	}
	if(out[1]=="string"){
		ver <- paste(names(ver), unlist(ver), sep="_", collapse="_")
	}
	ver
}


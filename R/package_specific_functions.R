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
        list(given="Anthony",  family="Hennessey", role=c("aut")),
        list(given="Espen",		 family="Johnsen",   role=c("aut")),
        list(given="Sindre",		family="Vatnehol",  role=c("aut")),
        list(given="Edvin",		 family="Fuglebakk", role=c("aut")),
        list(given="Ibrahim",	   family="Umar",	  role=c("aut")),
        list(given="Aasmund",	   family="Skaalevik", role=c("aut")),
        list(given="Esmael Musema", family="Hassen",	role=c("aut")),
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

#.onAttach_RstoxFramework <- function(version = "1.0") {
#    out <- c(
#        ".onAttach <- function(libname, pkgname) {", 
#        "\trequireNamespace(\"data.table\")", 
#        "} "
#    )
#    paste(out, collapse="\n")
#}


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
        list(given="Anthony",  family="Hennessey", role=c("aut")),
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


##### RstoxData: #####
title_RstoxData <- function(version = "1.0") {
    # "Utilities to Read Fisheries Biotic, Acoustic and Landing Data Formats"
    #"Utilities to Read and Manipulate Fisheries' Trawl Survey, Acoustic Survey and Commercial Landings Data Formats"
    "Tools to Read and Manipulate Fisheries Data"
}

description_RstoxData <- function(version = "1.0") {
    #"Tools to fetch and manipulate various data formats for fisheries (mainly geared towards biotic and acoustic data)."
    "Set of tools to read and manipulate various data formats for fisheries. Mainly catered towards scientific trawl survey sampling ('biotic') data, acoustic trawl data, and commercial fishing catch ('landings') data. Among the supported data formats are the data products from the Norwegian Institute Marine Research (IMR) and the International Council for the Exploration of the Sea (ICES)."
}

details_RstoxData <- function(version = "1.0") {
    "The RstoxData package contains functions for reading, filtering and writing biotic, acoustic and landing data as XML files. Filtering can be done by R syntax such as longitude > 10, or by pre defined functions such as inside(). On computers that return errors when trying to run the Rtools through RStudio (most institutional Windows machines), install the binary directly from https://github.com/StoXProject/RstoxData/releases. Download the newest RstoxData zip file, click the \"Packages\" tab -> \"Install\" -> \"Install from:\" \"Package Archive File\" -> \"Install\". If the installer does not complain, the package is installed correctly."
}

authors_RstoxData <- function(version = "1.0") {
    list(
        list(given="Edvin",		 family="Fuglebakk", role=c("cre", "aut"), email="edvin.fuglebakk@hi.no"),
        list(given="Arne Johannes", family="Holmin",	role=c("aut")),
        list(given="Ibrahim",	   family="Umar",	  role=c("aut")), 
        #list(given="Mikko Juhani",  family="Vihtakari", role=c("aut")),
        list(given="Sindre",		family="Vatnehol",  role=c("aut")),
        list(given="Anthony",  family="Hennessey", role=c("aut")),
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
        list(given="Edvin",		 family="Fuglebakk", role=c("cre", "aut"), email="arnejh@hi.no"), 
        list(given="Arne Johannes", family="Holmin", role=c("aut"))
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
        list(given="Sindre",		family="Vatnehol", role=c("aut"), email="sindre.vatnehol@hi.no"), 
        list(given="Alf",		   family="Harbitz",  role=c("aut")), 
        list(given="Espen",		 family="Johnsen",  role=c("aut"))
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

#.onLoad_RstoxBuild <- function(version = "1.0") {
#    out <- c(
#        ".onLoad <- function(libname, pkgname) {", 
#        "\t# Initiate the RstoxBuild environment:", 
#        "\tlibrary(data.table)", 
#        "} "
#    )
#    paste(out, collapse="\n")
#}
##########



##### RstoxTesting: #####
title_RstoxTesting <- function(version = "1.0") {
    "Utilities to Test StoX Projects"
}

description_RstoxTesting <- function(version = "1.0") {
    "A set of tools for comparing the output of a StoX project with the expected ouput as stored in the output files."
}

details_RstoxTesting <- function(version = "1.0") {
    "The main function in the package is compareProjectToStoredOutputFiles()."
}

authors_RstoxTesting <- function(version = "1.0") {
    list(
        list(given="Arne Johannes", family="Holmin", role=c("cre", "aut"), email="arnejh@hi.no"), 
        list(given="Anthony",  family="Hennessey", role=c("aut"))
    )
}


##########
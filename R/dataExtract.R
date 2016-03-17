#' This extracts the history features for each person in the cohort with the specific age/gender
#' @description This function connects to the CDM and constructs the data used to do the clustering
#' - this is either condition_concept_ids that are recorded during the defined time period relative
#' to the cohort start date for each person in the cohort or covariate concept_sets that are
#' specified by using the 'default' grouping or inputing a dataframe with columns: definition and
#' concept_id specifiying the concept_ids that make up each covariate definition, see examples below.
#' @param dbconnection class:connectionDetails - the database connection details requires Library(DatabaseConnector)
#' @param cdmDatabaseSchema  class:character - database schema containing cdm tables
#' @param cohortDatabaseSchema  class:character - database schema containing cohort
#' @param workDatabaseSchema  class:character -database you writing tables to
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param minAge  class:numeric default(NULL)- the minimum age a person in the cohort must be to be included in the data
#' @param maxAge  class:numeric default(NULL)- the maximum age a person in the cohort must be to be included in the data
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param type  class:character - features used by clustering (condition i.e. all condition_concept_ids or group i.e. concept sets),
#' @param groupDef class:dataframe - a dataframe containing covariate concept_sets - must have the columns definition and concept_id
#' @param historyStart class:numeric days prior to index to start searching person records for features
#' @param historyEnd class:numeric days prior to index to stop searching person records for features
#' @param ffloc   class:character - specifies the directory where the ff files are stored
#' @param debug   class:character - default(NULL) otherise specifies the directory where the main SQL for extraction is written to for debugging
#' @return clusterData class:clusterData - a list containing:
#' \item{strata}{an ffdf containing the age/gender/row_id for each person in the cohort}
#' \item{covariates}{an ffdf containing the covariates for each person in the cohort}
#' \item{covariateRef}{ an ffdf containing details about the covariates}
#' \item{metadata}{a list containing details about the data extraction}
#' @seealso DatabaseConnector, OhdsiRTools, SqlRender, ggplot2, reshape2, dplyr, plyr
#' @keywords OHDSI, clustering
#' @export
#' @examples
#'
#' # to extract the males ages between 30 and 45 in cdm_test.dbo.cohort with id 21
#' # and find whether they have the default concept definitions 1 to 60 days prior
#' # to cohort start:
#' dbconnection <- DatabaseConnector::createConnectionDetails(dbms = dbms,server = server,
#' user = user,password = pw,port = port,schema = cdmDatabaseSchema)
#'
#' data <- dataExtract(dbconnection, cdmDatabaseSchema='cdm_test.dbo',
#'                     cohortDatabaseSchema='cdm_test.dbo', cohort_id=21,
#'                     minAge = 30, maxAge=45, gender=8507,
#'                     type='group', groupDef='default',
#'                     historyStart=1,historyEnd=60,
#'                     ffloc='C:fftemps')
#'
#' # to extract the cluster data using user specified concept sets:
#' # where definition 1 contains concept_ids: 101,32011,1 and 63434
#' #       definition 2 contains concept_ids: 12,13
#' #       definition 3 contains concept_ids: 450453,21435324,232,3424,4534435 and 3453
#' groupDef <- data.frame(covariate=c(1,1,1,1,2,2,3,3,3,3,3,3),
#'                        concept_id =c(c(101,32011,1,63434), c(12,13),
#'                                      c(450453,21435324,232,3424,4534435,3453))
#' data <- dataExtract(dbconnection, cdmDatabaseSchema='cdm_test.dbo',
#'                     cohortDatabaseSchema='cdm_test.dbo', cohort_id=21,
#'                     type='group', groupDef=groupDef,
#'                     historyStart=1,historyEnd=180,
#'                     ffloc='C:fftemps')
#'
dataExtract <- function(dbconnection=NULL, cdmDatabaseSchema=NULL, cohortDatabaseSchema=NULL,
                        workDatabaseSchema=NULL,
                        cohortid=100, ageMin=NULL, ageMax=NULL, gender=NULL,
                        type='group', groupDef = 'default',
                        historyStart=1,historyEnd=180,
                        ffloc=NULL, debug=NULL, ...)
{

  if(is.null(ffloc)){
    warning('Please enter a directory with read/write access to store the ff files')
    return()
  }
  options(fftempdir = ffloc)

  if(!is.null(dbconnection) & !is.null(cdmDatabaseSchema) & !is.null(cohortDatabaseSchema) ){

    cdmDatabase <- strsplit(cdmDatabaseSchema,'\\.')[[1]][1]
    cdm_version <- 5
    sqlType <- dbconnection$dbms
    conn <- DatabaseConnector::connect(dbconnection)
    #sql.loc <- file.path(.libPaths(), 'patientCluster','sql','sql_server')[1]
    sql.loc <- file.path(system.file(package='patientCluster'), 'sql','sql_server')[1]

    # do checks for input
    writeLines('Checking input...')
    test <- !is.null(dbconnection)&class(dbconnection)=="connectionDetails" &
      !is.null(cdmDatabaseSchema) & class(cdmDatabaseSchema)=="character" &
      !is.null(cohortid) & class(cohortid)%in%c("numeric","character") &
      !is.null(type) & class(type)=="character"
    if(!test){writeLines('Input error...')}

    if(type=='group')
      if(groupDef!='default' & class(groupDef)!='data.frame')
        return('No group settings')
    if(type=='group'){
    # extract definitions of interest into table: rows of definition, concept_id
      definitions <- groupDef
      if(groupDef=='default'){
        definitions <-read.table(file.path(system.file(package='patientCluster'),'feat','definitions.txt'),sep=':')
        definitions <- cbind(strsplit(paste(apply(definitions, 1, function(x)
                             paste(rep(x[1],length(strsplit(as.character(x[2]),',')[[1]])), collapse=':')),collapse=':'),':')[[1]]
                             ,strsplit(paste(as.character(definitions[,2]), collapse=','),',')[[1]])
        colnames(definitions) <- c('covariate','concept_id')
      }

    # load this table into the sql table named covariates in outputdatabase
    DatabaseConnector::insertTable(conn, 'covariateGroups', definitions, dropTableIfExists = TRUE,
                createTable = TRUE, tempTable = T, oracleTempSchema = NULL)
    }

    # extracting strata and covariates
    ageLower <- 0
    ageUpper <- 200
    agegroup <- !is.null(ageMin) | !is.null(ageMax)
    if(!is.null(ageMin))
      ageLower <- ageMin
    if(!is.null(ageMax))
      ageUpper <- ageMax
    writeLines('Extracting data...')
    sql <- SqlRender::readSql(file.path(sql.loc,paste(type,'cohortCluster.sql', sep='')))
    sql <- SqlRender::renderSql(sql,
                                cdm_database = cdmDatabase,
                                workDatabase = workDatabaseSchema,
                                cohort_database_schema=cohortDatabaseSchema,
                                cohort_ids = cohortid,
                                cdm_version = '5',
                                cohort_definition_id = 'cohort_definition_id',
                                start = historyStart, end=historyEnd,
                                use_age= !is.null(agegroup), agelower=ageLower, ageupper=ageUpper,
                                use_gender= !is.null(gender), gender=gender)$sql
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    if(!is.null(debug))
      SqlRender::writeSql(sql, file.path(debug, paste("rendered_",type,"_extraction.sql", sep="")))
    DatabaseConnector::executeSql(conn, sql)

    # output results and progress to user
    sql <- 'select * from #sub_cohort'
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    strata <- DatabaseConnector::querySql.ffdf(conn, sql)
    writeLines(paste('Found ',as.double(nrow(strata)),' cohort people in cohort_id ',cohortid, sep=''))
    writeLines(ifelse(as.double(nrow(strata))>1, 'Enough people... Extracting covariate data', 'not enough people... Terminating') )
    if(nrow(strata)==0){return()}

    # check the data isnt too big or return warning:
    if(type=='condition')
      if(nrow(strata)>100000)
        warning("Extracted dara for clusting has more than 100000 people please reduce the number of features by rerunning data extract and using type='group' or use covariatesToInclude in clusterPeople to select a smaller number of covariates ")

    sql <- 'select * from #covariates'
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    covariates <- DatabaseConnector::querySql.ffdf(conn, sql)

    sql <- paste0('select b.concept_id, b.concept_name, b.',
                  ifelse(cdm_version==5,'concept_class_id','concept_class'),
            ' from #counts a inner join concept b on a.concept_id=b.concept_id')
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    covariateRef <- DatabaseConnector::querySql.ffdf(conn, sql)

    writeLines('Data extracted ...')

   metaData <- list(cohortId = cohortid,
                     database = cdmDatabaseSchema,
                     agegroup=agegroup,
                     type=type,
                     call = match.call())

    result <- list(strata = strata,
                   covariates = covariates,
                   covariateRef = covariateRef,
                   metaData = metaData)
    class(result) <- 'clusterData'
    RJDBC::dbDisconnect(conn)
    return(result)
  }
}


#' Save the clustering data to folder
#'
#' @description
#' \code{saveClusterData} saves an object of type clusterData to folder.
#'
#' @param cData   An object of type \code{clusterData} as generated using \code{dataExtract}.
#' @param file      The name of the folder where the data will be written. The folder should not yet
#'                  exist.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @examples
#' # todo
#'
#' @export
saveClusterData <- function(cData, file,overwrite=F){
  if (missing(cData))
    stop("Must specify cData")
  if (missing(file))
    stop("Must specify directory")
  if (class(cData) != "clusterData")
    stop("Data not of class clusterData")

  strata <- cData$strata
  covariates <- cData$covariates
  covariateRef <- cData$covariateRef
  ffbase::save.ffdf(strata, covariates, covariateRef, dir = file, overwrite=overwrite)

  metaData <- cData$metaData
  save(metaData, file = file.path(file, "metaData.Rdata"))
}

#' Load the cluster data from a folder
#'
#' @description
#' \code{loadClusterData} loads an object of type \code{clusterData} from a folder in the file system.
#'
#' @param file       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class clusterData
#'
#' @examples
#' # todo
#'
#' @export
loadClusterData <- function(file, readOnly = FALSE) {
  if (!file.exists(file))
    stop(paste("Cannot find directory:", file))
  if (!file.info(file)$isdir)
    stop(paste("Not a folder", file))

  temp <- setwd(file)
  absolutePath <- setwd(temp)

  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(strata = get("strata", envir = e),
                 covariates = get("covariates", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 metaData = mget("metaData",
                                 envir = e,
                                 ifnotfound = list(NULL))[[1]]  #For backwards compatibility
  )

  # Open all ffdfs to prevent annoying messages later:
  open(result$strata, readonly = readOnly)
  open(result$covariates, readonly = readOnly)
  open(result$covariateRef, readonly = readOnly)

  class(result) <- "clusterData"
  rm(e)
  return(result)
}

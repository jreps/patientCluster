#' This extracts the history features for each person in the cohort with the specific age/gender
#'
#' @param dbconnection class:connectionDetails - the database connection details requires Library(DatabaseConnector)
#' @param cdmDatabaseSchema  class:character - database schema containing cdm tables
#' @param cohortDatabaseSchema  class:character - database schema containing cohort
#' @param workDatabaseSchema  class:character -database you writing tables to
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param agegroup  class:numeric - 1=0-20 year olds; 2=20-40 year olds; 3=40-60 year olds; 4=60-80 year olds;5=80-100 year olds
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param type  class:character - features used by clustering (currently only history),
#' @param historyStart ....
#' @param historyEnd ....
#' @param sqlType ...
#' @param loc   class:character - directory where the results of the clustering are saved
#' @return clusterData class:clusterData - a list containing: strata- an ffdf containing the age/gender/row_id for each person in the cohort,
#'                                                            covariates - an ffdf containing the covariates for each person in the cohort
#'                                                            covariateRef - an ffdf containing details about the covariates
#'                                                            metadata - a list containing details about the data extraction
#' @seealso DatabaseConnector, OhdsiRTools, SqlRender, ggplot2, reshape2, dplyr, plyr
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' dataExtract(gender=8507)
dataExtract <- function(dbconnection=NULL, cdmDatabaseSchema=NULL, cohortDatabaseSchema=NULL,
                        workDatabaseSchema=NULL,
                        cohortid=100, agegroup=NULL, gender=NULL,
                        type='condition', groupDef = 'default',
                        historyStart=1,historyEnd=180, loc=NULL)
{
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

    if(type=='group' & (groupDef!='default' & class(groupDef)!='data.frame'))
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
    writeLines('Extracting data...')
    sql <- SqlRender::readSql(file.path(sql.loc,paste(type,'cohortCluster.sql', sep='')))
    sql <- SqlRender::renderSql(sql,
                                cdm_database = cdmDatabase,
                                workDatabase = workDatabaseSchema,
                                cohort_database_schema=cohortDatabaseSchema,
                                cohort_ids = cohortid,
                                cdm_version = '5',
                                cohort_definition_id = 'cohort_definition_id',
                                start = historyStart, end=historyEnd)$sql
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    SqlRender::writeSql(sql, file.path(loc, paste("rendered_",type,"_extraction.sql", sep="")))
    DatabaseConnector::executeSql(conn, sql)

    # output results and progress to user
    sql <- 'select * from #sub_cohort'
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    strata <- DatabaseConnector::querySql.ffdf(conn, sql)
    writeLines(paste('Found ',as.double(nrow(strata)),' cohort people in cohort_id ',cohortid, sep=''))
    writeLines(ifelse(as.double(nrow(strata))>1, 'Enough people... Extracting covariate data', 'not enough people... Terminating') )
    if(nrow(strata)==0){return()}

    sql <- 'select * from #covariates'
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    covariates <- DatabaseConnector::querySql.ffdf(conn, sql)
    ##clust.data <- DatabaseConnector::querySql(conn, sql)

    sql <- paste0('select b.concept_id, b.concept_name, b.',
                  ifelse(cdm_version==5,'concept_class_id','concept_class'),
            ' from #counts a inner join concept b on a.concept_id=b.concept_id')
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    covariateRef <- DatabaseConnector::querySql.ffdf(conn, sql)

    writeLines('Data extracted ...')

    # save the data to output folder
    if(!dir.exists(file.path(loc,cohortid,'extractedData',type))){
      dir.create(file.path(loc,cohortid,'extractedData',type), recursive = T)}


    ##save.ffdf(clust.data, dir=file.path(loc,cohortid,'extractedData',type, paste(agegroup,'_',gender, sep='')))
    ##saveRDS(clust.data, file.path(loc,cohortid,'extractedData',type, paste(agegroup,'_',gender,'.rds', sep='')))
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
    #saveClusterData(result, file=file.path(loc,cohortid,'extractedData',type, paste(agegroup,'_',gender, sep='')))
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
saveClusterData <- function(cData, file){
  if (missing(cData))
    stop("Must specify cData")
  if (missing(file))
    stop("Must specify directory")
  if (class(cData) != "clusterData")
    stop("Data not of class clusterData")

  strata <- cData$strata
  covariates <- cData$covariates
  covariateRef <- cData$covariateRef
  ffbase::save.ffdf(strata, strataRef, covariates, covariateRef, dir = file)

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

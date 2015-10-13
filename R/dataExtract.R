#' This extracts the history features for each person in the cohort with the specific age/gender
#'
#' @param dbconnection class:connectionDetails - the database connection details requires Library(DatabaseConnector)
#' @param cdmDatabaseSchema  class:character - database schema containing database
#' @param workDatabaseSchema  class:character -database you writing tables to
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param agegroup  class:numeric - 1=0-20 year olds; 2=20-40 year olds; 3=40-60 year olds; 4=60-80 year olds;5=80-100 year olds
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param type  class:character - features used by clustering (currently only history),
#' @return NULL (writes results to output folder)
#' @seealso DatabaseConnector, OhdsiRTools, SqlRender, ggplot2, reshape2, dplyr, plyr
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' dataExtract()
dataExtract <- function(dbconnection=NULL, cdmDatabaseSchema=NULL, workDatabaseSchema=NULL,
                        cohortid=100, agegroup=3, gender=8507,type='history')
{
  if(!is.null(dbconnection) & !is.null(cdmDatabaseSchema) & !is.null(workDatabaseSchema) ){

    loc <- file.path(.libPaths(), 'patientCluster','sql','sql_server')
    #loc <- file.path(getwd(), 'src')

    # age values
    age <- matrix(c(0,20,20,40,40,60,60,80,80,100), ncol=2, byrow=T)

    # do checks for input
    writeLines('Checking input...')
    test <- !is.null(dbconnection)&class(dbconnection)=="connectionDetails" &
      !is.null(cdmDatabaseSchema) & class(cdmDatabaseSchema)=="character" &
      !is.null(cohortid) & class(cohortid)%in%c("numeric","character") &
      !is.null(agegroup) & class(agegroup)%in%c("numeric","character") &
      !is.null(gender) & gender %in% c(8507,8532) &
      !is.null(type) & class(type)=="character"
    if(!test){writeLines('Input error...')}

    # extract the data into ffdf
    writeLines('Extracting data...')
    conn <- DatabaseConnector::connect(dbconnection)
    sql <- SqlRender::readSql(file.path(loc,'count_extractor.sql'))
    sql <- SqlRender::renderSql(sql,
                     database = cdmDatabaseSchema,
                     outDatabase = workDatabaseSchema,gender=gender, ageupper=age[agegroup,2], agelower=age[agegroup,1],
                     cohortID = cohortid)$sql
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    SqlRender::writeSql(sql, file.path(getwd(),"rendered_count_extractor.sql"))
    result <- DatabaseConnector::querySql(conn, sql)
    RJDBC::dbDisconnect(conn)
    if(length(result)<1){result <- 0}

    # output results and progress to user
    writeLines(paste('Found ',as.double(result[1]),' distinct patients in cohort_id ',cohortid,' in age and gender stratum', sep=''))
    writeLines(ifelse(as.double(result[1])>1, 'Enough patients... Extracting Data', 'not Enough patients... Terminating') )
    if(result[1]==0){return()}

    # extract definitions of interest into table: rows of definition, concept_id
    load(file.path(.libPaths(),'data','definitions.RData'))
    definitions <- cbind(strsplit(paste(apply(definitions, 1, function(x)
        paste(rep(x[1],length(strsplit(as.character(x[2]),',')[[1]])), collapse=':')),collapse=':'),':')[[1]]
      ,strsplit(paste(as.character(definitions[,2]), collapse=','),',')[[1]])
    colnames(definitions) <- c('covariate','concept_id')

    # load this table into the sql table named covariates in outputdatabase
    dbconnection2 <- dbconnection
    dbconnection2$schema <- workDatabaseSchema
    conn2 <- DatabaseConnector::connect(dbconnection2)
    DatabaseConnector::insertTable(conn2, paste(workDatabaseSchema,'JMR_covariates',sep='.'), definitions, dropTableIfExists = TRUE,
                createTable = TRUE, tempTable = FALSE, oracleTempSchema = NULL)
    RJDBC::dbDisconnect(conn2)

    # extracting features
    conn <- DatabaseConnector::connect(dbconnection)
    finalSelect <- paste("select person_id, ",
                         paste("sum(case when covariate = '",unique(definitions[,1]),"' then  1 else 0 end) [",
                               unique(definitions[,1]),"]", sep="", collapse=","),
                         " from history group by person_id")

    sql <- SqlRender::readSql(file.path(loc,paste(type,'cohortCluster.sql', sep='')))
    sql <- SqlRender::renderSql(sql,
                     database = cdmDatabaseSchema, outDatabase = workDatabaseSchema,
                     gender=gender, ageupper=age[agegroup,2], agelower=age[agegroup,1],
                     cohortID = cohortid,
                     history = history,
                     finalSelect=finalSelect)$sql
    sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    SqlRender::writeSql(sql, file.path(getwd(), paste("rendered_",type,"_extraction.sql")))
    ##clust.data <- as.ffdf(querySql(conn, sql))   - caused crashing issues
    clust.data <- DatabaseConnector::querySql(conn, sql)
    writeLines('Data extracted ...')

    # save the data to output folder
    if(!dir.exists(file.path(getwd(),cohortid))){
      dir.create(file.path(getwd(),cohortid))}
    if(!dir.exists(file.path(getwd(),cohortid,'extractedData'))){
      dir.create(file.path(getwd(),cohortid,'extractedData'))}
    if(!dir.exists(file.path(getwd(),cohortid,'extractedData',type))){
      dir.create(file.path(getwd(),cohortid,'extractedData',type))}


    ##save.ffdf(clust.data, dir=file.path(getwd(),cohortid,'extractedData',type, paste(agegroup,'_',gender, sep='')))
    saveRDS(clust.data, file.path(getwd(),cohortid,'extractedData',type, paste(agegroup,'_',gender,'.rds', sep='')))
    RJDBC::dbDisconnect(conn)
  }
}




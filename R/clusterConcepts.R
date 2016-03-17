#'  clusterConcepts
#' @description Create topics by clusting condition_concept_ids based on ingredience counts
#' @param dbconnection  using DatabaseConnector - connect to cdm database
#' @param cdmDatabaseSchema - cdm schema used to extract data from
#' @param method  class:character - method used to do clustering (currently only supports kmeans)
#' @param clusterSize class:numeric - number of clusters returned,
#' @param topicSize class:numeric - number of topics in glrm
#' @param scale  class:boolean - whether to use ingredience percentage scale for clustering
#' @param covariatesToInclude  class:character vector - features to include: default NULL
#' @param indications  class:boolean   extract drug indicator features;Default TRUE
#' @param dayStart  class:integer number of days relative to condition_concept_code to start looking for drugs
#' @param dayEnd  class:integer number of days relative to condition_concept_code to stop looking for drugs
#' @param use_min_obs class:boolean whether to remove ingredient features that are rare
#' @param min_obs clkass:integer threshold used when use_min_obs is TRUE to determine what is rare
#' @return list contining definition data.frame containing columes for concept_id, covariate (cluster id)
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' clusterConcepts()

# age: age
clusterConcepts <- function(dbconnection,cdmDatabaseSchema=NULL,workDatabaseSchema=NULL,
                       method='kmeans', clusterSize=10, topicSize=NULL, scale=T,
                       covariatesToInclude=NULL,
                       indications=T, dayStart=1,dayEnd=30,
                       use_min_obs=TRUE, min_obs=100, extraparameters=NULL,
                       updateProgress=NULL,...)
{
  # shiny app output
  if (is.function(updateProgress)) {
    updateProgress(detail = "\n Initiating H2o...")
  }
  h2o::h2o.init(nthreads=-1)
  use_before = indications
  cdmDatabase <- strsplit(cdmDatabaseSchema,'\\.')[[1]][1]
  sql.loc <- file.path(system.file(package='patientCluster'), 'sql','sql_server')[1]

  # create the inclusion/exclusion tables to use in the sql
  use_include <- !is.null(covariatesToInclude)
  if(!is.null(covariatesToInclude)){
    writeLines('Creating inclusion table')
    include <- data.frame(concept_id=covariatesToInclude, include=rep(T, length(covariatesToInclude)))
    dbconnection2 <- dbconnection
    dbconnection2$schema <- workDatabaseSchema
    conn2 <- DatabaseConnector::connect(dbconnection2)
    DatabaseConnector::insertTable(conn2, 'include', include, dropTableIfExists = TRUE,
                                   createTable = TRUE, tempTable = T, oracleTempSchema = NULL)
    RJDBC::dbDisconnect(conn2)
  }

  if (is.function(updateProgress)) {
    updateProgress(detail = "\n Extracting data...")
  }
  conn <- DatabaseConnector::connect(dbconnection)
  writeLines('Extracting covariate cluster data...')
  start <- Sys.time()
  sql <- SqlRender::readSql(file.path(sql.loc,'covariateCluster.sql'))
  sql <- SqlRender::renderSql(sql,
                              cdm_database = cdmDatabase,
                              cdm_version = '5',
                              use_before = use_before,
                              use_include = is.null(covariatesToInclude),
                              start = dayStart, end=dayEnd,
                              use_min_obs=use_min_obs  ,
                              min_obs=min_obs,
                              use_include=use_include)$sql
  sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = dbconnection$dbms)$sql
  DatabaseConnector::executeSql(conn, sql)

  if (is.function(updateProgress)) {
    updateProgress(detail = "\n Extracting additional info...")
  }
  # select ingredient values:
  sql.out <- "select condition_concept_id, ingredience_concept_id, value from #covariateClust"
  sql.out <- SqlRender::translateSql(sql = sql.out, sourceDialect = "sql server", targetDialect = dbconnection$dbms)$sql
  covData <- DatabaseConnector::querySql.ffdf(conn, sql.out)
  total <- Sys.time() - start
  writeLines(paste0('Extracting data took:', format(total, digits=4)))

  # extract concept and ingredient information:
  sql.out <- "select concept_id, concept_name from concept a inner join
  (select distinct condition_concept_id from #covariateClust) b
  on b.condition_concept_id=a.concept_id"
  sql.out <- SqlRender::translateSql(sql = sql.out, sourceDialect = "sql server", targetDialect = dbconnection$dbms)$sql
  conceptUsed <- DatabaseConnector::querySql.ffdf(conn, sql.out)

  sql.out <- "select concept_id, concept_name from concept a inner join
  (select distinct ingredience_concept_id from #covariateClust) b
  on b.ingredience_concept_id=a.concept_id"
  sql.out <- SqlRender::translateSql(sql = sql.out, sourceDialect = "sql server", targetDialect = dbconnection$dbms)$sql
  ingUsed <- DatabaseConnector::querySql.ffdf(conn, sql.out)

  if (is.function(updateProgress)) {
    updateProgress(detail = "\n Creating clustering matrix...")
  }
  # reshape data into matrix:
  writeLines('Casting sparse data into matrix form')
  start <- Sys.time()
  covMat <- reshape2::dcast(ff::as.ram(covData), CONDITION_CONCEPT_ID ~ INGREDIENCE_CONCEPT_ID, fill=0)
  total <- Sys.time() - start
  writeLines(paste0('Casting took:', format(total, digits=4)))

  covMat.data <- covMat[,-1]
  covMat.names <- covMat[,1]
  rm(covMat)
  #center the data?
  if(scale==T){
    writeLines('Scaling data')
    start <- Sys.time()
    covMat.data <- covMat.data/matrix(rep(apply(covMat.data, 1, sum), ncol(covMat.data)), byrow=F, ncol=ncol(covMat.data))
    total <- Sys.time() - start
    writeLines(paste0('Scaling took:', format(total, digits=4)))
  }


  # perform the clustering
  if (is.function(updateProgress)) {
    updateProgress(detail = "\n Clustering data...")
  }
  # set training frame:
  if(is.null(topicSize))
    topicSize <- 100

  training_frame <- h2o::as.h2o(covMat.data)
  param <- list(training_frame = training_frame,
                regularization_x = 'NonNegative',
                normalise = FALSE,
                rowIds = covMat.names,
                colIds = colnames(covMat.data),
                clusterSize = clusterSize,
                glrmFeat=topicSize)
  if(!is.null(extraparameters))
    param <- c(param,extraparameters)
   clust.result <- do.call(paste0('pc.',method), param)

   if (is.function(updateProgress)) {
     updateProgress(detail = "\n Formatting...")
   }
  metaData <- list(method=method,
                   size=clusterSize,
                   database = cdmDatabaseSchema,
                   scale=scale,
                   covariatesToInclude=covariatesToInclude,
                   indications=indications,
                   start=dayStart,
                   end=dayEnd,
                   use_min_obs=use_min_obs, min_obs=min_obs,
                   call= match.call(),
                   sql=sql
  )
  defs <- NULL
  if(!is.null(clust.result$newData))
    defs <- merge(clust.result$clusters, clust.result$newData, by='rowId')
  if(is.null(clust.result$features))
    clust.result$features <-  clust.result$centers
  result <- list(data= covMat.data,
                 datanames=covMat.names,
                 ingredientsUsed = ingUsed,
                 conceptDescriptions = conceptUsed,
                 definitions= defs,
                 topics =clust.result$features,
                 metaData = metaData)


  return(result)
}

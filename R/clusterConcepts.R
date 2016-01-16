#'  clusterConcepts
#' @description Create topics by clusting condition_concept_ids based on ingredience counts
#' @param dbconnection  using DatabaseConnector - connect to cdm database
#' @param cdmDatabaseSchema - cdm schema used to extract data from
#' @param method  class:character - method used to do clustering (currently only supports kmeans)
#' @param clusterSize class:numeric - number of clusters returned,
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
clusterConcepts <- function(dbconnection,cdmDatabaseSchema=NULL,
                       method='kmeans', clusterSize=10, scale=T,
                       covariatesToInclude=NULL,
                       indications=T, dayStart=1,dayEnd=30,
                       use_min_obs=TRUE, min_obs=100)
{
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
  sql <- SqlRender::translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
  DatabaseConnector::executeSql(conn, sql)


  # select ingredient values:
  sql.out <- "select condition_concept_id, ingredience_concept_id, value from #covariateClust"
  sql.out <- SqlRender::translateSql(sql = sql.out, sourceDialect = "sql server", targetDialect = sqlType)$sql
  covData <- DatabaseConnector::querySql.ffdf(conn, sql.out)
  total <- Sys.time() - start
  writeLines(paste0('Extracting data took:', format(total, digits=4)))

  # extract concept and ingredient information:
  sql.out <- "select concept_id, concept_name from concept a inner join
  (select distinct condition_concept_id from #covariateClust) b
  on b.condition_concept_id=a.concept_id"
  sql.out <- SqlRender::translateSql(sql = sql.out, sourceDialect = "sql server", targetDialect = sqlType)$sql
  conceptUsed <- DatabaseConnector::querySql.ffdf(conn, sql.out)

  sql.out <- "select concept_id, concept_name from concept a inner join
  (select distinct ingredience_concept_id from #covariateClust) b
  on b.ingredience_concept_id=a.concept_id"
  sql.out <- SqlRender::translateSql(sql = sql.out, sourceDialect = "sql server", targetDialect = sqlType)$sql
  ingUsed <- DatabaseConnector::querySql.ffdf(conn, sql.out)

  # reshape data into matrix:
  writeLines('Casting sparse data into matrix form')
  start <- Sys.time()
  covMat <- reshape2::dcast(as.ram(covData), CONDITION_CONCEPT_ID ~ INGREDIENCE_CONCEPT_ID, fill=0)
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
  if(method=='kmeans'){
    writeLines('Performing kmeans')
    start <- Sys.time()
    training_frame <- h2o::as.h2o(covMat.data)
    #clusters <- kmeans(covMat.data, center= clusterSize, iter.max=100, nstart=10)
    res.kmeans <- h2o::h2o.kmeans(training_frame, k=clusterSize, max_iterations = 1000,
               standardize = FALSE, init = "PlusPlus", seed=1)
    total <- Sys.time() - start
    writeLines(paste0('Kmeans took:', format(total, digits=4)))
#res.kmeans@model$run_time
    cluster <- as.data.frame(predict(res.kmeans, training_frame))
    colnames(cluster)[colnames(cluster)=='predict'] <- 'covariate'
    definitions <- data.frame(covariate = cluster,
                              concept_id=covMat.names)
    topics <- as.data.frame(res.kmeans@model$centers)
  }


  if(method=='glrm'){
    training_frame <- h2o::as.h2o(covMat.data)
    res.glrm <- h2o::h2o.glrm(training_frame, k=clusterSize, loading_name='basis',
                              ignore_const_cols=T, transform = "NONE", regularization_y = "NonNegative",
                              regularization_x =  "NonNegative", gamma_x = 0.5, gamma_y = 0.5,
                              max_iterations = 1000, init_step_size = 1, min_step_size = 0.0000001,
                              init = "SVD",#"PlusPlus",
                              impute_original = FALSE,  seed=1)
    plot(res.glrm)
    y <- res.glrm@model$archetypes
    x <- h2o.getFrame(res.glrm@model$representation_name)

    res.kmeans <- h2o::h2o.kmeans(x, k=clusterSize)
    cluster <- as.data.frame(predict(res.kmeans, x))
    colnames(cluster)[colnames(cluster)=='predict'] <- 'covariate'
    definitions <- data.frame(covariate = cluster,arch=as.data.frame(x),
                              concept_id=covMat.names)
    topics <-  data.frame(t(y), concept_id=colnames(covMat.data))
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

  result <- list(data= covMat.data,
                 datanames=covMat.names,
                 ingredientsUsed = ingUsed,
                 conceptDescriptions = conceptUsed,
                 definitions= definitions,
                 topics =topics,
                 metaData = metaData)


  return(result)
}

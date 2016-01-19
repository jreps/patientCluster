#' Summarise the differences between the clusters
#' @description This function simply calculates summaries of each cluster and returns these
#' as a list.
#' @param clusterResult  A list of class 'clusterResult' return by running clusterPeople()
#' @keywords OHDSI, clustering
#' @details This function only has one input, the clusterResults obtained by applying clusterPeople
#' @export
#' @return A list containing:
#' \item{clusterMeans}{ A data frame containing the mean of each feature per cluster}
#' \item{clusterSds}{ A data frame containing the standard deviation of each feature value per cluster}
#' \item{clusterFrac}{ A data frame containing the fraction of each cluster with non-zero values for the feature}
#' @examples
#' # set database connection
#' dbconnection <- DatabaseConnector::createConnectionDetails(dbms = dbms,server = server,
#' user = user,password = pw,port = port,schema = cdmDatabaseSchema)
#'
#' # then extract the data - in thie example using default groups
#' clusterData <- dataExtract(dbconnection, cdmDatabaseSchema,
#' cohortDatabaseSchema=cdmDatabaseSchema,
#' workDatabaseSchema='scratch.dbo',
#' cohortid=2000006292, agegroup=NULL, gender=NULL,
#' type='group', groupDef = 'default',
#' historyStart=1,historyEnd=365,  loc=getwd())
#'
#' # initialise the h2o cluster
#' h2o.init(nthreads=-1, max_mem_size = '50g')
#'
#' # cluster the males aged between 30 and 50 into 15 clusters
#' clusterPeople <- clusterRun(clusterData, ageSpan=c(30,50), gender=8507,
#'                          method='kmeans', clusterSize=15,
#'                          normalise=F, binary=F,fraction=T)
#'
#' # get the summary details of each cluster:
#' clusterSum <- clusterEval(clusterResult)
#'

clusterEval <- function(clusterResult)
{
  # check input
  writeLines('Checking input...')
  test <- class(clusterResult)== 'clusterResult'
  if(!test){writeLines('Incorrect input...')
    return('error')}

  if(clusterResult$metadata$method=='glrm'){writeLines('Not suitable for glrm results...')
    return('error')}

  writeLines('Evaluating cluster data...')
  clusters <- as.ram(clusterResult$clusters)
  covariates <- ff::clone(clusterResult$covariates)

  covariates <- as.ram(covariates)
  covariates$value <- 1
  covMat <- reshape2::dcast(covariates[,c('ROW_ID','COVARIATE','value')], ROW_ID~COVARIATE)

  allData <- merge(covMat, clusters, by.x='ROW_ID', by.y='rowId')

  predict <- allData$predict
  # calculate total counts per cluster predict
  mean <- aggregate(. ~ predict, allData[,!colnames(allData)%in%c('ROW_ID','predict')], mean)
  sd <- aggregate(. ~ predict, allData[,!colnames(allData)%in%c('ROW_ID','predict')], stats::sd)
  frac <- aggregate(. ~ predict, allData[,!colnames(allData)%in%c('ROW_ID','predict')], function(x) sum(x>0)/length(x))

  result <- list(clusterMeans=mean,
                 clusterSds =sd,
                 clusterFrac = frac)
  class(result) <- 'clusterEval'
  return(result)

}


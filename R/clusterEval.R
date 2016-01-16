#' Summarises the differences between the clusters
#'
#' @param clusterResult  ...
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' clusterEval()
#'

clusterEval <- function(clusterResult)
{
  # check input
  writeLines('Checking input...')
  test <- class(clusterResult)== 'clusterResult'
  if(!test){writeLines('Incorrect input...')
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


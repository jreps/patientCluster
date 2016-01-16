#' runs kmeans on the data
#'
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param ageSpan  class:numeric - 1=0-20 year olds; 2=20-40 year olds; 3=40-60 year olds; 4=60-80 year olds;5=80-100 year olds
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param method  class:character - method used to do clustering (currently only supports kmeans)
#' @param clusterSize class:numeric - number of clusters returned,
#' @param normalise  class:boolean - whether to center the data prior to clustering
#' @param covariatesToInclude  class:character vector - features to include: default NULL
#' @param covariatesToExclude  class:character  vector - features to exclude;Default NULL
#' @param covariatesGroups  class:covariatecluster   result of clusterCovariate();Default NULL
#' @param loc   class:character - directory where the results of the clustering are saved
#' @return ...
#' @seealso DatabaseConnector, OhdsiRTools, SqlRender, ggplot2, reshape2, dplyr, plyr
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' clusterRun()
clusterPeople <- function(clusterData, ageSpan=c(0,100), gender=8507,
                       method='kmeans', clusterSize=10, normalise=T,
                       covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL,
                       loc=loc)
{
  # chect input
  test <- !is.null(method) & method %in%c('kmeans','glrm') &
    class(clusterData) == 'clusterData' &
    (is.null(ageSpan) | class(ageSpan)=="numeric" & length(ageSpan)==2) &
    (is.null(gender) | gender %in% c(8507,8532))

  if(!test){writeLines('Incorrect input...')}

  if(test){

    strata <- ff::clone(clusterData$strata)
    covariates <- ff::clone(clusterData$covariates)
    covariateRef <- ff::clone(clusterData$covariateRef)

    if(!is.null(ageSpan)){
      writeLines('Filtering by age span')
      t <- strata$AGE >= ageSpan[1] & strata$AGE <= ageSpan[2]
      ppl.include <- strata$ROW_ID[ffbase::ffwhich(t,t==T)]

      ind <- ffbase::ffmatch(strata$ROW_ID, ppl.include)
      strata <- strata[ffbase::ffwhich(ind, !is.na(ind)),]

      ind <- ffbase::ffmatch(covariates$ROW_ID, ppl.include)
      covariates <- covariates[ffbase::ffwhich(ind, !is.na(ind)),]

    }

    if(!is.null(gender)){
      writeLines('Filtering by gender')
      t <- strata$GENDER == gender
      ppl.include <- strata$ROW_ID[ffbase::ffwhich(t,t==T)]

      ind <- ffbase::ffmatch(strata$ROW_ID, ppl.include)
      strata <- strata[ffbase::ffwhich(ind, !is.na(ind)),]

      ind <- ffbase::ffmatch(covariates$ROW_ID, ppl.include)
      covariates <- covariates[ffbase::ffwhich(ind, !is.na(ind)),]

    }

    if(!is.null(covariatesToInclude)){
      writeLines('Removing/Including covariates as specified...')
      ind <- fbase::ffmatch(covariates$COVARIATE, table==ff::as.ff(covariatesToInclude))
      covariates <- covariates[ffbase::ffwhich(ind, !is.na(ind)),]
    }
    if(!is.null(covariatesToExclude)){
      writeLines('Removing/Including covariates as specified...')
      ind <- fbase::ffmatch(covariates$COVARIATE, table==ff::as.ff(covariatesToExclude))
      covariates <- covariates[ffbase::ffwhich(ind, is.na(ind)),]
      }

    # set groupings if included:
    if(!is.null(covariatesGroups)){
      covariates <- ff::as.ffdf(merge(covariateGroups, as.ram(covariates), by.x='conceptId', by.y='CONCEPT_ID'))
    }

    # tranform into matrix and h2o object
    covariates <- as.ram(covariates)
    covariates$value <- 1
    covMat <- reshape2::dcast(covariates[,c('ROW_ID','COVARIATE','value')], ROW_ID~COVARIATE)
    covMat.data <- covMat[,-1]

    # cluster using kmeans or glrm
    if(method=='kmeans'){
      writeLines('Performing kmeans')
      start <- Sys.time()
      training_frame <- h2o::as.h2o(covMat.data)
      #clusters <- kmeans(covMat.data, center= clusterSize, iter.max=100, nstart=10)
      res.kmeans <- h2o::h2o.kmeans(training_frame, k=clusterSize, max_iterations = 1000,
                                    standardize = T, init = "PlusPlus", seed=1)
      total <- Sys.time() - start
      writeLines(paste0('Kmeans took:', format(total, digits=4)))
      cluster <- as.data.frame(predict(res.kmeans, training_frame))
      clusters <- data.frame(cluster = cluster,
                                rowId=as.character(covMat[,1]))
      centers <- as.data.frame(res.kmeans@model$centers)
    }

    if(method=='glrm'){
      writeLines('Performing generalised low rank model')
      start <- Sys.time()
      training_frame <- h2o::as.h2o(covMat.data)
      res.glrm <- h2o::h2o.glrm(training_frame, k=clusterSize, loading_name='basis',
                                ignore_const_cols=T, transform = "NONE", regularization_y = "NonNegative",
                                regularization_x =  "L1", gamma_x = 0.5, gamma_y = 0.5,
                                max_iterations = 1000, init_step_size = 1, min_step_size = 0.0000001,
                                init = "SVD",#"PlusPlus",
                                impute_original = FALSE,  seed=1)
      total <- Sys.time() - start
      writeLines(paste0('Generalised low rank model took:', format(total, digits=4)))
      cluster = as.data.frame(h2o.getFrame(res.glrm@model$representation_name))
      clusters <- data.frame(cluster = cluster,
                             rowId=as.character(covMat[,1]))

      centers <- t(res.glrm@model$archetypes)


    }
    metaData <- c(list(size=clusterSize, method=method), clusterData$metaData)
    result <- list(strata=strata,
                   covariates=as.ffdf(covariates),
                   covariateRef=covariateRef,
                   clusters = clusters,
                   centers= centers,
                   metadata = metaData)
    class(result) <- 'clusterResult'
  }

return(result)
}



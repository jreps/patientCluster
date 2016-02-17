#' Runs kmeans or generalised low rank models on the cluster data
#' @description This function clusters patients into subgroups based on covariates corresponding
#' to sets of concept_ids or concept_ids.  It is recommended to use generalised low rank models to
#' preprocess the data when clustering patients using individual concept_ids and reduce the dimensionality
#' before applying k-means.  When the data extraction used covariate groups kmeans can be run directly.
#'
#' @details  This function performs kmeans clustering or general low rank model clustering
#' on clusterData extraced from the CDM using dataExtract().  The user can specify a subset of the
#' data based on ageSpan=c(lowerAgeLimit, upperAgeLimit) and gender=gender_concept_id and then
#' the clustering method 'kmeans' or 'glrm' and the required cluster size: clusterSize=10.
#'
#' When method 'kmeans' is chosen, the people are clustered using kmeans from the h2o package
#' into clusterSize number of groups. When method 'glrm' is chosen, a glrm is run on the data to reduce the dimensionality to glrmFeat
#' number of features and then kmeans is run on the reduced dimensionality data to cluster the
#' people into clusterSize number of groups.
#'
#' The data can be pre-processed using the normalise, binary and fraction variables.  When normalise
#' is TRUE then the data have the feature means subtracted and the result is divided by the
#' feature standard deviation.  When binary is TRUE, each feature for a person is set to 1 if the
#' patient has the feature in the covariate list and 0 otherwise.  When binary is set to FALSE the
#' feature value is set to the number of concepts in the feature set that the patient has in the
#' covariates list (e.g. if feature 1 consists of three concept_ids, 12, 1 and 304 and patient 1 has
#' none of these concept_ids in the covariate list, he will have 0 in the feature 1 column, whereas
#' if patient 2 has concept_id 12 and 304, she will have 2 in the feature 1 column).  When fraction is
#' TRUE then the features for each patient are scaled by dividing by the total sum of the patient's
#' feature values (e.g. if patient 1 has value 3 for feature 5, value 1 for feature 10 and 0 for all
#' other features then if fraction =TRUE this will be scale to 3/4 for feature 5 and 1/4 for feature 10).
#'
#' The user can also specify covariates to include/exclude from the clustering by specifying the
#' covariate_ids in a vector, for example setthing covariatesToInclude=c(1,3,10,45) will cluster
#' the data using only the four specified covariates whereas setting covariatesToExclude=c(1,3,10,45)
#' will exclude the specified covariates from the clustering.
#' @usage clusterPeople()
#'
#' ## Default S3 method:
#' clusterPeople(clusterData, ageSpan=c(0,100), gender=8507, method='kmeans',
#'               clusterSize=10, glrmFeat=NULL,normalise=T, binary=T,
#'               fraction=F, covariatesToInclude=NULL,covariatesToExclude=NULL,
#'               covariatesGroups=NULL, loc=loc)
#'
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param ageSpan  class:numeric - 1=0-20 year olds; 2=20-40 year olds; 3=40-60 year olds; 4=60-80 year olds;5=80-100 year olds
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param method  class:character - method used to do clustering (currently only supports kmeans)
#' @param clusterSize class:numeric - number of clusters returned,
#' @param glrmFeat class:numeric - number of features engineered by generalised low rank model
#' @param normalise  class:boolean - whether to center the data prior to clustering
#' @param binary class:boolean - whether to treat features as binary
#' @param fraction class:boolean - whether to treat features as fraction of total records
#' @param covariatesToInclude  class:character vector - features to include: default NULL
#' @param covariatesToExclude  class:character  vector - features to exclude;Default NULL
#' @param covariatesGroups  class:covariatecluster   result of clusterCovariate();Default NULL
#' @param extraparameters - a list of parameters that can be used when adding a non default cluster method
#' @return A list is returned of class 'clusterResult' containing:
#'
#' \item{strata}{An ffdf containing the row_id (unique reference of the person), their age and gender}
#'
#'  \item{covariates}{  An ffdf containing the covariates each person has in sparse format}
#' \item{covariateRef}{  An ffdf containing the description of each covariate}
#'  \item{clusters}{  A data frame containing the cluster allocated for each row_id}
#'  \item{centers}{   A data frame containing the cluster centers returned by the kmeans algorithm}
#'  \item{metadata}{  A list containing the information about the paramaters set to extract the data
#'                and do the clustering}
#'  \item{newData}{  An ffdf containing the reduced dimensionality data returned when glrm pre-processing is done}
#'  \item{features}{ An ffdf containing the clustering of the original covariates by glrm}
#'
#' @keywords OHDSI, CDM, clustering
#' @author Jenna Reps
#' @references todo...
#' @export
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
clusterPeople <- function(clusterData, ageSpan=c(0,100), gender=NULL,
                       method='kmeans', clusterSize=10, glrmFeat=NULL,
                       normalise=T, binary=F, fraction=F,
                       covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL,
                       extraparameters=NULL
                       ){
  # chect input
  test <- !is.null(method) & method %in%c('kmeans','glrm', 'concensus') &
    class(clusterData) == 'clusterData' &
    (is.null(ageSpan) | ifelse(!is.null(ageSpan), class(ageSpan), 'none')=="numeric" &
       ifelse(!is.null(ageSpan),length(ageSpan), 0)==2) &
    (is.null(gender) | ifelse(!is.null(gender),gender, 1) %in% c(8507,8532))

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

      writeLines(paste0(length(unique(strata$ROW_ID)),' people remaining'))
    }

    if(!is.null(gender)){
      writeLines('Filtering by gender')
      t <- strata$GENDER == gender
      ppl.include <- strata$ROW_ID[ffbase::ffwhich(t,t==T)]

      ind <- ffbase::ffmatch(strata$ROW_ID, ppl.include)
      strata <- strata[ffbase::ffwhich(ind, !is.na(ind)),]

      ind <- ffbase::ffmatch(covariates$ROW_ID, ppl.include)
      covariates <- covariates[ffbase::ffwhich(ind, !is.na(ind)),]

      writeLines(paste0(length(unique(strata$ROW_ID)),' people remaining'))

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
      covariates <- ff::as.ffdf(merge(covariateGroups, ff::as.ram(covariates), by.x='conceptId', by.y='CONCEPT_ID'))
    }

    # tranform into matrix and h2o object
    covariates <- ff::as.ram(covariates)
    covariates$value <- 1
    if(binary==T){
    covMat <- reshape2::dcast(covariates[,c('ROW_ID','COVARIATE','value')], ROW_ID~COVARIATE, fill=0,
                              fun.aggregate = max)
    }
    if(binary==F){
      covMat <- reshape2::dcast(covariates[,c('ROW_ID','COVARIATE','value')], ROW_ID~COVARIATE, fill=0,
                                fun.aggregate = sum)
    }
    covMat.data <- covMat[,-1]
    writeLines(paste0(nrow(covMat.data),' rows in full matrix'))

    if(fraction==T){
      covMat.data <- covMat.data/matrix(rep(apply(covMat.data,1,sum), ncol=ncol(covMat.data)), byrow=F,
                                         ncol=ncol(covMat.data), nrow=nrow(covMat.data))
    }

    # convert to h20
    training_frame <- h2o::as.h2o(covMat.data)

    param <- list(clusterSize=clusterSize, glrmFeat=glrmFeat,
                  normalise=normalise,training_frame=training_frame )
    if(!is.null(extraparameters))
      param <- c(param,extraparameters)
    param$rowIds <- as.character(covMat[,1])
    param$colIds <- as.character(colnames(covMat.data))
    clust.result <- do.call(paste0('pc.',method), param)


    metaData <- c(list(size=clusterSize, method=method), clusterData$metaData)
    result <- list(strata=strata,
                   covariates=ff::as.ffdf(covariates),
                   covariateRef=covariateRef,
                   metadata = metaData)
    result <- c(result, clust.result)

    class(result) <- 'clusterResult'
  }

return(result)
}



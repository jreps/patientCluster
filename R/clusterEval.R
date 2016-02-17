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
  clusters <- ff::as.ram(clusterResult$clusters)
  covariates <- ff::clone(clusterResult$covariates)

  covariates <- ff::as.ram(covariates)
  covariates$value <- 1
  covMat <- reshape2::dcast(covariates[,c('ROW_ID','COVARIATE','value')], ROW_ID~COVARIATE,fill=0,fun.aggregate=length)

  allData <- merge(covMat, clusters, by.x='ROW_ID', by.y='rowId')

  predict <- allData$predict
  # calculate total counts per cluster predict
  means <- aggregate(. ~ predict, allData[,!colnames(allData)%in%c('ROW_ID','predict')], FUN=mean)
  sds <- aggregate(. ~ predict, allData[,!colnames(allData)%in%c('ROW_ID','predict')], stats::sd)
  fracs <- aggregate(. ~ predict, allData[,!colnames(allData)%in%c('ROW_ID','predict')], function(x) sum(x>0)/length(x))

  covmat.stand <- t(t(allData[,!colnames(allData)%in%c('ROW_ID','predict')])-apply(allData[,!colnames(allData)%in%c('ROW_ID','predict')],2, mean))
  rel <- t(t(covmat.stand)/apply(covmat.stand,2, function(x) max(x)))
  rel[covmat.stand<0] <- t(t(covmat.stand)/apply(covmat.stand,2, function(x) abs(max(-x))))[covmat.stand<0]
  rel <- as.data.frame(rel)
  relative <- aggregate(. ~ predict, rel, mean)



  strataLabs <- merge(ff::as.ram(clusterResult$strata), clusters, by.x='ROW_ID', by.y='rowId')
  strataLabs$value <- 1
  ageGen <- aggregate(strataLabs[,c('value')], by=list( predict=strataLabs$predict,
                                                        age=strataLabs$AGE,gender=strataLabs$GENDER), sum)
  #ageGen <- reshape2::dcast(ageGen, predict+gender~age,fill=0)
  genders <- data.frame(gender_id=c(8507,8532), genderName=c('Male','Female'))
  ageGen <- merge(ageGen, genders, by.x='gender', by.y='gender_id')

  # each cluster
  param <- min(allData$predict):max(allData$predict)
  proces <- function(data, clust){
    colnames(data)[colnames(data)=='x'] <- 'value'
    return(list(male=data[data$predict==clust&data$genderName=='Male',c('age','value')],
                female=data[data$predict==clust&data$genderName=='Female',c('age','value')])
    )
  }
  ageGen<- lapply(param, function(x) proces(ageGen,x))


  # calculate min/ range per topic, calculate variability of topic across clusters

  result <- list(clusterMeans=means,
                 clusterSds =sds,
                 clusterFrac = fracs,
                 clusterRelative=relative,
                 ageGender=ageGen)
  class(result) <- 'clusterEval'
  return(result)

}


# eval topics:
topicEval <- function(clust.res, threshold=NULL){
  def <- merge(clust.res$definitions, clust.res$conceptDescriptions, by.x='rowId',by.y='CONCEPT_ID')
  topics <- merge(clust.res$topics, clust.res$ingredientsUsed, by.x='covariate_id',by.y='CONCEPT_ID')
  topicRanks <- def[,c(-1,-2)]

  ranker <- function(i){
    val <- c()
    if(sum(topicRanks[,i]>0)!=0){
      val <- data.frame(concept=topicRanks$CONCEPT_NAME,value=topicRanks[,i])
      if(!is.null(threshold))
        val <- val[val$value>=threshold,]
      val <- val[order(-val$value),]
    }
    return(val)
  }

  topiclist <- lapply( 1:(ncol(topicRanks)-1), ranker)

  topicsMax <- data.frame(concept=topicRanks$CONCEPT_NAME,
                          cluster= apply(topicRanks, 1, function(x) which.max(x[-ncol(topicRanks)])))
  topicsMax <- lapply(min(topicsMax$cluster):max(topicsMax$cluster),
                      function(x) topicsMax[topicsMax$cluster==x,] )

  topicsKmeans <- data.frame(concept=def$CONCEPT_NAME,
                             cluster= def$predict )
  topicsKmeans <- lapply(min(topicsKmeans$cluster):max(topicsKmeans$cluster),
                         function(x) topicsKmeans[topicsKmeans$cluster==x,] )

  result <- list(
    topicsOrdered = topiclist,
    topicsMax = topicsMax ,
    topicsKmeans =topicsKmeans
  )
  return(result)
}


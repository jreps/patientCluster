#' runs kmeans on the data
#'
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param agegroup  class:numeric - 1=0-20 year olds; 2=20-40 year olds; 3=40-60 year olds; 4=60-80 year olds;5=80-100 year olds
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param type  class:character - features used by clustering (currently only history),
#' @param method  class:character - method used to do clustering (currently only supports kmeans)
#' @param clusterSize class:numeric - number of clusters returned,
#' @param centerVal  class:boolean - whether to center the data prior to clustering
#' @param covariatesToInclude  class:character vector - features to include: default NULL
#' @param covariatesToExclude  class:character  vector - features to exclude;Default NULL
#' @return NULL (writes results to output folder)
#' @seealso DatabaseConnector, OhdsiRTools, SqlRender, ggplot2, reshape2, dplyr, plyr
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' clusterRun()
#'
clusterRun <- function(cohortid=1, agegroup=3, gender=8507,type='history',
                           method='kmeans', clusterSize=10, centerVal=T,
                           covariatesToInclude=NULL,covariatesToExclude=NULL)
{
  # chect input
  test <- !is.null(cohortid) & class(cohortid)%in%c("numeric","character") &
    !is.null(agegroup) & class(agegroup)%in%c("numeric","character") &
    !is.null(gender) & gender %in% c(8507,8532) &
    !is.null(type) & class(type)=="character"
  if(!test){writeLines('Incorrect input...')}
  # check if cohort data already extracted (if not call extractData())
  dataExists <- file.exists(file.path(getwd(),cohortid,'extractedData',type, paste(agegroup,'_',gender,'.rds', sep='')))

  # if the data does not exist
  if(!dataExists){
    writeLines("No data to run clustering...")
  }

  if(dataExists){
    # check for saved ffdf and load if exists:
    ##load.ffdf(file.path(getwd(),cohortid,'extractedData',type, paste(agegroup,'_',gender, sep='')))
    clust.data <- readRDS(file.path(getwd(),cohortid,'extractedData',type, paste(agegroup,'_',gender,'.rds', sep='')))

    if(!is.null(covariatesToInclude)){clust.data <- clust.data[,covariatesToInclude]}
    clust.data <- clust.data[,!colnames(clust.data)%in%covariatesToExclude]

    clust.data[is.na(clust.data)] <- 0

    # remove and empyt features:
    clust.data <- clust.data[, colSums(clust.data != 0) > 0]

    # perform clustering
    if(centerVal==T){datoi <- scale(clust.data[,-1])}
    if(centerVal==F){datoi <- clust.data[,-1]}
    clust.kmeans <- kmeans(datoi, centers=clusterSize, nstart=10, iter.max = 100)
    # create dir
    if(!dir.exists(file.path(getwd(),cohortid,'kmeans',type))){dir.create(file.path(getwd(),cohortid,'kmeans',type),recursive=T)}
    saveRDS(clust.kmeans, file.path(getwd(),cohortid,'kmeans',type, paste(agegroup,'_',gender,'.rds', sep='')))
    writeLines('Clustering completed... saving results')
    # append cluster results to data
    ##clust.res <- do.call("ffdf", c(physical(cohort.data), physical(as.ffdf(cluster.res$x))))
    ##colnames(clust.res) <- c(colnames(cohort.data), 'Cluster')
    clust.res <- data.frame(clust.data,cluster=clust.kmeans$cluster)

    # save results to folder
    #save.ffdf(clust.res, file.path(getwd(), outputFolder,cohortid,'clusterResults',paste(agegroup,'_',gender, sep='')))
    # create dir
    if(!dir.exists(file.path(getwd(),cohortid,'clusterResults',type))){dir.create(file.path(getwd(),cohortid,'clusterResults',type),recursive=T)}
    saveRDS(clust.res, file.path(getwd(),cohortid,'clusterResults',type, paste(agegroup,'_',gender,'.rds', sep='')))
  }

}



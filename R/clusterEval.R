#' Summarises the differences between the clusters
#'
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param agegroup  class:numeric - 1=0-20 year olds; 2=20-40 year olds; 3=40-60 year olds; 4=60-80 year olds;5=80-100 year olds
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param type  class:character - features used by clustering (currently only history),
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' clusterEval()
#'

clusterEval <- function(cohortid=1, agegroup=3, gender=8507, type='history')
{
  # check input
  writeLines('Checking input...')
  test <- !is.null(cohortid) & class(cohortid)%in%c("numeric","character") &
    !is.null(agegroup) & class(agegroup)%in%c("numeric","character") &
    !is.null(gender) & gender %in% c(8507,8532) &
    !is.null(type) & class(type)=="character"

  if(!test){writeLines('Incorrect input...')}

  test <- file.exists(file.path(getwd(), cohortid,'clusterResults',type,paste(agegroup,'_',gender,'.rds', sep='')))
  if(!test){writeLines('No cluster data...')}

  # load data:
  writeLines('Loading cluster data...')
  ##load.ffdf(file.path(getwd(), outputFolder,cohortid,'clusterResults',paste(agegroup,'_',gender, sep='')))
  if(file.exists(file.path(getwd(),cohortid,'clusterResults',type, paste(agegroup,'_',gender,'.rds', sep='')))){
    clust.res <- readRDS(file.path(getwd(),cohortid,'clusterResults',type, paste(agegroup,'_',gender,'.rds', sep='')))


    #use ddlpy to count number of patients/ number of recordings of each covariates per cluster
    writeLines('Summarising cluster data...')
    library(dplyr)
    evalResults.sum <- group_by(clust.res[,-1], cluster) %>% summarise_each(funs(sum))

    evalResults.mean <- group_by(clust.res[,-1], cluster) %>% summarise_each(funs(mean))

    evalResults.median<- group_by(clust.res[,-1], cluster) %>% summarise_each(funs(median))

    writeLines('Saving results to directory...')
    if(!dir.exists(file.path(getwd(), cohortid,'clusterSummary'))){dir.create(file.path(getwd(), cohortid,'clusterSummary'))}
    if(!dir.exists(file.path(getwd(), cohortid,'clusterSummary',type))){dir.create(file.path(getwd(), cohortid,'clusterSummary',type))}

    write.csv(evalResults.mean, file.path(getwd(), cohortid,'clusterSummary',type,paste('mean',agegroup,'_',gender,'.csv', sep='')),
              row.names=FALSE)
    write.csv(evalResults.median, file.path(getwd(), cohortid,'clusterSummary',type,paste('median',agegroup,'_',gender,'.csv', sep='')),
              row.names=FALSE)
    write.csv(evalResults.sum, file.path(getwd(), cohortid,'clusterSummary',type,paste('sum',agegroup,'_',gender,'.csv', sep='')),
              row.names=FALSE)
  }


}


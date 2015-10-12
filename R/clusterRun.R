# create clusters using hisorical data
createClusters <- function(cohortid, agegroup, gender,
                           outputfolder, method=c('kmeans',...), clusterSize=10,
                           type=c('history', 'stats'),
                           covariatesToInclude,covariatesToExclude)
{
  # chect input
  test <- .checkInput(cohortid, clusterSize=10, outputfolder, method)
  if(test){
    writeLines('Incorrect input...')
    return()}
  # check if cohort data already extracted (if not call extractData())
  dataExists <- .dataExists(dbconnection, cohortid, agegroup, gender, type)

  # if the data does not exist
  if(!dataExists){
    writeLines("No data...Extracting cohort's agegroup/gender history data")
    extractData(dbconnection, outputfolder, cohortid, agegroup, gender, method,
                type)
  }

  # check for saved ffdf and load if exists:
  load.ffdf(file.path(getwd(), outputFolder,cohortid,'extractedData', paste(agegroup,'_',gender, sep='')))

  # perform clustering
  clust.res <- kmeans(clust.data, centers=clusterSize)

  # append cluster results to data
  clust.res <- do.call("ffdf", c(physical(cohort.data), physical(as.ffdf(cluster.res$x))))
  colnames(clust.res) <- c(colnames(cohort.data), 'Cluster')

  # save results to folder
  save.ffdf(clust.res, file.path(getwd(), outputFolder,cohortid,'clusterResults',paste(agegroup,'_',gender, sep='')))


}

.checkinput<- function(){
  test <- class(cohortid)=='' & class(clusterSize)=='' &
    class(outputfolder)=='' & class(method)==''
  return(test)
}

.dataExists<- function(){
  return(dir.exists(file.path(getwd(), outputFolder,'extractedData', paste(cohortid,'_',agegroup,'_',gender, sep=''))))
}

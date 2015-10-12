# clusterEval

clusterEval <- function(dbconnection, cohortid, agegroup, gender,
                        outputFolder, covariatesToInclude,covariatesToExclude)
{
  # check input
  writeLines('Checking input...')
  test <- .checkInput(dbconnection, cohortid, agegroup, gender,
                      outputFolder, covariatesToInclude,covariatesToExclude)
  if(test){
    writeLines('Incorrect input...')
    return()}

  # check cohortid, age, gender in local datafile
  test <- dir.exists(file.path(getwd(), outputFolder,cohortid,'clusterResults',paste(agegroup,'_',gender, sep='')))
  if(!test){
    writeLines('No cluster data...')
    return()}

  # load data:
  writeLines('Loading cluster data...')
  load.ffdf(file.path(getwd(), outputFolder,cohortid,'clusterResults',paste(agegroup,'_',gender, sep='')))

  #use ddlpy to count number of patients/ number of recordings of each covariates per cluster
  writeLines('Summarising cluster data...')
  evalResults <- ddply(clust.res, ...)

  writeLines('Saving results to directory...')
  write.csv(evalResults, file.path(getwd(), outputFolder,cohortid,'clusterSummary',paste(agegroup,'_',gender.csv, sep='')))


}

.checkinput<- function(){
  test <- class(dbconnection)=='' & class(cohortid)=='' & class(agegroup)=='' &
    class(outputfolder)=='' & class(covariatesToInclude)=='' & class(gender)==''
  & class(covariatesToExclude)==''
  return(test)
}

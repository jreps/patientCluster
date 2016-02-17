jsonFormat <- function(clusterEval,inc=NULL){

  if(!is.null(inc))
    ind <- inc
  if(is.null(inc))
    inc <- 1:nrow(clusterEval$clusterMeans)

  proces <- function(i){
    cen <- reshape2::melt(clusterEval$clusterFrac[i,-1])
    colnames(cen)[1] <- 'axis'
    return(list('centers'=cen, 'counts'=clusterEval$ageGender[[i]]))
  }

  allD <- lapply(inc,proces)
  allD <- jsonlite::toJSON(allD)

return(allD)
}

topicJsonFormat <- function(topicEval, type='topicsKmeans', loc=NULL){
  singles <- (1:length(topicEval[[type]]))[unlist(lapply(topicEval[[type]], nrow))<=1]

  # put all singles into new list at end
  newList <- list()
  length(newList) <- length(topicEval[[type]])-length(singles)+1
  count <- 0
  for(i in 1:length(topicEval[[type]])){
    if(!i%in%singles){
      count <- count+1
      newList[[count]] <- topicEval[[type]][[i]]
    }
    if(i%in%singles)
      newList[[length(newList)]] <- rbind(newList[[length(newList)]], topicEval[[type]][[i]])
  }

  #Name topics
  names(newList) <- c(paste0('topic ',1:(length(newList)-1)), 'Unclustered')

  # convert to json:
  exportJson <- jsonlite::toJSON(newList)

  # sAVE RESULT AS JSON+ DRUG LIST
  if(!is.null(loc))
    write(exportJson, file=file.path(loc,"exportTopics.JSON"))

  return(exportJson)

}

pc.kmeans <- function(clusterSize,normalise,training_frame,rowIds,...){
  writeLines('Performing kmeans')
  start <- Sys.time()
  #clusters <- kmeans(covMat.data, center= clusterSize, iter.max=100, nstart=10)
  res.kmeans <- h2o::h2o.kmeans(training_frame, k=clusterSize, max_iterations = 1000,
                                standardize = normalise, init = "PlusPlus", seed=1)
  total <- Sys.time() - start
  writeLines(paste0('Kmeans took:', format(total, digits=4)))
  cluster <- as.data.frame(h2o::h2o.predict(res.kmeans, training_frame))
  clusters <- data.frame(cluster = cluster,
                         rowId=rowIds)

  centers <- as.data.frame(res.kmeans@model$centers)
  clustMat <- function(x){
    vec.x <- rep(0, clusterSize)
    vec.x[x] <- 1
    return(vec.x)
  }

  newData <- t(sapply(cluster[,1], clustMat))
  colnames(newData) <- paste0('cluster', 1:clusterSize)
  newData <- data.frame(rowId=rowIds, as.data.frame(newData))

  features = NULL

  result <- list(clusters = clusters,
                 centers= centers,
                 newData= newData,
                 features = features)
  return(result)
}

pc.glrm <- function(glrmFeat,clusterSize,normalise,training_frame,rowIds,colIds,regularization_x="L1",...){
  writeLines('Performing generalised low rank model')
  start <- Sys.time()
  res.glrm <- h2o::h2o.glrm(training_frame, k=glrmFeat, loading_name='basis',
                            ignore_const_cols=T, transform = "NONE", regularization_y = "NonNegative",
                            regularization_x =  regularization_x, gamma_x = 0.5, gamma_y = 0.5,
                            max_iterations = 1000, init_step_size = 1, min_step_size = 0.0000001,
                            init = "SVD",#"PlusPlus",
                            impute_original = FALSE,  seed=1)
  total <- Sys.time() - start
  writeLines(paste0('Generalised low rank model took:', format(total, digits=4)))
  transData = h2o::h2o.getFrame(res.glrm@model$representation_name)

  writeLines(paste0(class(res.glrm@model$archetypes)))
  writeLines(paste0(dim(res.glrm@model$archetypes)))
  features <- data.frame(t(res.glrm@model$archetypes), covariate_id=colIds)#ff::as.ffdf()
  # now do kmeans on reduced dim data:

  ##newData <- ff::as.ffdf(as.h2o(transData))
  res.kmeans <- h2o::h2o.kmeans(transData, k=clusterSize, max_iterations = 1000,
                                standardize = normalise, init = "PlusPlus", seed=1)
  cluster <- as.data.frame(h2o::h2o.predict(res.kmeans, transData))
  clusters <- data.frame(predict = cluster,
                         rowId=rowIds) #definitions
  centers <- as.data.frame(res.kmeans@model$centers)

  newData <- data.frame(transData =  as.data.frame(transData),
                        rowId=rowIds)
  #writeLines(paste(colnames(newData), collapse='-'))

  result <- list(clusters = clusters,
                 centers= centers,
                 newData= ff::as.ffdf(newData),
                 features = features)
  return(result)

}


# method of consensus kmeans using hclust - algo heirarchal
#...
pc.concensus<- function(clusterSize,normalise,training_frame,rowIds,colIds, csample=0.5, rsample=0.8, repeats=20 ,...){
  writeLines('Performing concensusus clustering')
  start <- Sys.time()

  eachiter <- function(...){
  # sample from the data:
  colSamp <- sort(sample(ncol(training_frame), ncol(training_frame)*csample))
  rowSamp <- sort(sample(nrow(training_frame), nrow(training_frame)*rsample))

  #clusters <- kmeans(covMat.data, center= clusterSize, iter.max=100, nstart=10)
  res.kmeans <- h2o::h2o.kmeans(training_frame[rowSamp,colSamp], k=clusterSize, max_iterations = 1000,
                                standardize = normalise, init = "PlusPlus", seed=1)

  return(as.data.frame(predict(res.kmeans, training_frame)))
  }
  cluster <- lapply(1:repeats, eachiter)
  cluster <-do.call(cbind, cluster)
  writeLines('Finished multiple clustering...')

  colnames(cluster) <- paste0('predict',1:ncol(cluster))
  #writeLines(paste(rowIds[1:10], collapse=' - '))
  cluster$rowId <- rowIds
  #writeLines(paste(cluster$rowId[1:10], collapse=' - '))
  writeLines(paste0(colnames(cluster)))
  melted <- reshape2::melt(cluster, id.vars='rowId', value.name='clust')
  #colnames(melted)[colnames(melted)=='value'] <- 'clust'
  #melted$value <- 1
  writeLines(paste0(colnames(melted)))
  writeLines(paste0(dim(melted)))
  casted <- reshape2::dcast(melted, rowId~variable+clust, fill=0)

  data.concensus <- h2o::as.h2o(casted)

  # now do glrm
  mod <- h2o::h2o.glrm(data.concensus[,-1], k=clusterSize, loading_name='basis',
            ignore_const_cols=T, transform = "NONE", regularization_y = "NonNegative",
            regularization_x =  "L1", gamma_x = 0.5, gamma_y = 0.5,
            max_iterations = 1000, init_step_size = 1, min_step_size = 0.0000001,
            init = "SVD",#"PlusPlus",
            impute_original = FALSE,  seed=1)

  writeLines('Finished concensus - now generating summary details...')
  # cluster groupings:
  groups <- ff::as.ffdf(as.data.frame(t(mod@model$archetypes)))


  # TO FINISH
  transData = as.data.frame(h2o::h2o.getFrame(mod@model$representation_name))
  cluster = apply(transData, 1, which.max)

  # calculate centers:
  training_frame$cluster <- h2o::as.h2o(cluster)
  centers <- as.data.frame(h2o::h2o.ddply(training_frame, "cluster", FUN=mean)[,-(ncol(training_frame)+1)])
  colnames(centers)[-1] <- colnames(training_frame)[-ncol(training_frame)]


  # original attempt of heirachal:
  ##i <- 1
  #maxLen <- nrow(cluster)
  # this works but is too slow
  #clust.dis <- lapply(1:maxLen, function(i) apply(cluster[i+1:nrow(cluster),], 1, function(x) maxLen-sum(x==cluster[i,]))
  #)
  # ?try merging on predict_iter and group by rowIds to get counts the cast into matrix?
  # now to do heirarchal clustering
  #h1 <- flashClust::hclust(overlap(cluster), method= "average");


  total <- Sys.time() - start
  writeLines(paste0('Concensus clustering took:', format(total, digits=4)))

  clusters <- data.frame(predict = cluster,
                         rowId=rowIds)
  newData=NULL
  features = NULL

  result <- list(clusters = clusters,
                 centers= centers,
                 newData= newData,
                 features = groups)
  return(result)
}

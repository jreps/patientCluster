#' Plots the different cluster visulisations
#'
#' @param clusterresult  output from applying clusterPeople()
#' @param saveLoc   class:character - directory where the results of the clustering are saved
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' clusterVisual()
#'
clusterVisual <-function(clusterResult, saveLoc=getwd()){

  # first get the summaries of the clusters:
  clusterSummary <- clusterEval(clusterResult)
  clusterCenter <- clusterResult$centers # centroid is the cluster name

  if(!dir.exists(file.path(saveLoc, 'plots'))){dir.create(file.path(saveLoc, 'plots'))}
  plot_loc <- file.path(saveLoc, 'plots')

  temp1 <- reshape2::melt(clusterSummary$clusterMeans, id.vars="predict")
  colnames(temp1)[colnames(temp1)=='value'] <- 'mean'
  temp2 <- reshape2::melt(clusterSummary$clusterSds, id.vars="predict")
  colnames(temp2)[colnames(temp2)=='value'] <- 'sd'
  temp_all <- merge(temp1, temp2, all=T, by=c('predict', 'variable'))

  pdf(file = file.path(plot_loc, paste('barPlot.pdf', sep='')),
      paper='a4r')
  variables <- unique(temp_all$variable)
  for(i in 1:floor(length(variables/4))){
    temp <- temp_all[temp_all$variable%in%variables[(1+(i-1)*4):min((i*4), length(variables))],]
  print(

    ggplot2::ggplot(temp, ggplot2::aes(x = factor(predict), y = mean, fill=factor(predict))) +
      ggplot2::geom_bar(stat = "identity", position=ggplot2::position_dodge()) +
      ggplot2::geom_text(ggplot2::aes(y=mean, ymax=mean, label=round(mean,2)), position= ggplot2::position_dodge(width=0.9), vjust=-.5, color="black") +
      ggplot2::scale_x_discrete("Cluster") +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=mean-sd, ymax=mean+sd), width=.3) +
      ggplot2::facet_grid(variable ~ .)

  )}
  dev.off()




}

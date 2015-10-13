#' Plots the different cluster visulisations
#'
#' @param cohortid  class:numeric - id of cohort in cohort table
#' @param agegroup  class:numeric - 1=0-20 year olds; 2=20-40 year olds; 3=40-60 year olds; 4=60-80 year olds;5=80-100 year olds
#' @param gender  class:numeric - gender concept_id (8507- male; 8532-female)
#' @param type  class:character - features used by clustering (currently only history),
#' @keywords OHDSI, clustering
#' @export
#' @examples
#' clusterVisual()
#'
clusterVisual <-function(cohortid, agegroup, gender, type){
  if(!dir.exisits(file.path(getwd(), cohortid,'plots'))){dir.create(file.path(getwd(), cohortid,'plots'))}
 plot_loc <- file.path(getwd(), cohortid,'plots')

  # check input
  # load the cluster results
  if(type=='history'){
    evalResults.mean <- read.csv(file.path(getwd(), cohortid,'clusterSummary',type,paste('mean',agegroup,'_',gender,'.csv', sep='')),
              row.names=FALSE)
    evalResults.median <- write.csv(file.path(getwd(), cohortid,'clusterSummary',type,paste('median',agegroup,'_',gender,'.csv', sep='')),
              row.names=FALSE)
    evalResults.sum <- write.csv(file.path(getwd(), cohortid,'clusterSummary',type,paste('sum',agegroup,'_',gender,'.csv', sep='')),
                                    row.names=FALSE)
    evalResults.sum <- as.data.frame(evalResults.sum)
    evalResults.mean <- as.data.frame(evalResults.mean)
    evalResults.median <- as.data.frame(evalResults.median)

    if('pie'%in%plot_choice){
      cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                   "#999999", "#66CC99")
      sum.melt <- melt(evalResults.sum, id=c("cluster"))
      pdf(file = file.path(plot_loc, paste(type,'_pie_raw_',agegroup,'_',gender,'.pdf', sep='')),
          paper='a4r' )
      ggplot(sum.melt, aes(x=factor(1), y=value, fill=as.factor(cluster)))+
        geom_bar(width = 1, stat = "identity",position = 'fill') +
        coord_polar(theta = "y") +
        facet_wrap(~variable) +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        labs(fill='Cluster') + scale_fill_manual(values=cbPalette)

      dev.off()

      # now when normalised:
      mean.melt <- melt(evalResults.mean, id=c("cluster"))
      pdf(file = file.path(plot_loc, paste(type,'_pie_norm_',agegroup,'_',gender,'.pdf', sep='')),
          paper='a4r')
      ggplot(mean.melt, aes(x=factor(1), y=value, fill=as.factor(cluster)))+
          geom_bar(width = 1, stat = "identity",position = 'fill') +
          coord_polar(theta = "y") +
          facet_wrap(~variable) +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid  = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
         labs(fill='Cluster') + scale_fill_manual(values=cbPalette)
      dev.off()

    }

    if('bar'%in%plot_choice){
      cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                     "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                     "#999999", "#66CC99")

      # total - not normalised
      sum.melt <- melt(evalResults.sum, id=c("cluster"))

      pdf(file = file.path(plot_loc, paste(type,'_bar_raw_',agegroup,'_',gender,'.pdf', sep='')),
          paper='a4r')
        ggplot(sum.melt, aes(x=factor(1), y=value, fill=as.factor(cluster)))+
          geom_bar(width = 1, stat = "identity",position="dodge") +
          facet_wrap(~variable,scales = "free") +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid  = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
          labs(fill='Cluster') + scale_fill_manual(values=cbPalette)
      dev.off()

      # now when normalised:
      mean.melt <- melt(evalResults.mean, id=c("cluster"))
      pdf(file = file.path(plot_loc, paste(type,'_bar_norm_',agegroup,'_',gender,'.pdf', sep='')),
          paper='a4r')
        ggplot(mean.melt, aes(x=factor(1), y=value, fill=as.factor(cluster)))+
          geom_bar(width = 1, stat = "identity",position="dodge") +
          facet_wrap(~variable,scales = "free") +
          theme(axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid  = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
          labs(fill='Cluster') + scale_fill_manual(values=cbPalette)
      dev.off()
    }
  }

  if(type=='stats'){
    evalResults.mean <- read.csv(file.path(getwd(), cohortid,'clusterSummary',type,paste('mean',agegroup,'_',gender,'.csv', sep='')),
                                 row.names=FALSE)
    evalResults.mean <- as.data.frame(evalResults.mean)
    mean.melt <- melt(evalResults.mean, id=c("cluster"))
    mean.melt$category <- apply(mean.melt, 1, function(x) strsplit(x[2],'_')[[1]][1])
    mean.melt$time <- as.double(apply(mean.melt, 1, function(x)
      strsplit(x[2],'_')[[1]][2]))
    mean.melt$cluster <- factor(mean.melt$cluster)

    pdf(file = file.path(plot_loc, paste(type,'_stats_norm_',agegroup,'_',gender,'.pdf', sep='')),
        paper='a4r')
    ggplot(mean.melt, aes(x=time, y=value,color=cluster, group=cluster))+
      geom_line(aes(linetype=cluster)) +
      geom_point(aes(shape=cluster), size = 2.5) +
      facet_wrap(~category,scales = "free") +
      scale_shape_manual(values = c(0, 5, 6, 15,3,9,20,2,11,18)) +
      labs(x = "months prior to index", y='Number of events') +
      scale_x_continuous(breaks=seq(0, 12, 1))
    dev.off()

  }

}

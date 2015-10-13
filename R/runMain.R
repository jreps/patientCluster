#' cluster patients in your OHDSi cohort based on their medical history
#'
#' @param dbconnection class:connectionDetails - the database connection details requires Library(DatabaseConnector)
#' @param cdmDatabaseSchema  class:character - database schema containing database
#' @param workDatabaseSchema  class:character -database you writing tables to
#' @param outputFolder class:character - directory output will be written to
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
#' runMain()
#'
runMain <- function(dbconnection=NULL, cdmDatabaseSchema=NULL, workDatabaseSchema=NULL,
                 outputFolder=NULL,
                 cohortid=100, agegroup=3, gender=8507,type='history',
                 method='kmeans', clusterSize=10, centerVal=T,
                 covariatesToInclude=NULL,covariatesToExclude=NULL){

  if(!is.null(dbconnection) & !is.null(cdmDatabaseSchema) & !is.null(workDatabaseSchema)
     & !is.null(outputFolder)){


    require(DatabaseConnector)
    require(OhdsiRTools)
    require(SqlRender)
    require(ggplot2)
    require(reshape2)
    require(dplyr)
    require(plyr)


    writeLines(paste('Performing clustering for: ',cohortid, sep=''))
    # SET WORKING DIRECTORY
    ifelse(!dir.exists(file.path(outputFolder )),
           dir.create(file.path(outputFolder)), FALSE)

    setwd(file.path(outputFolder))
    ifelse(!dir.exists(file.path(getwd(), cohortid  )),
           dir.create(file.path(getwd(), cohortid)), FALSE)

    # extract data - extractdata
    dataExtract(dbconnection, cdmDatabaseSchema, workDatabaseSchema,
                cohortid=cohortid, agegroup=agegroup, gender=gender,type=type)

    # perform clustering createclusters
    clusterRun(cohortid=cohortid, agegroup=agegroup, gender=gender,type=type,
               method=method, clusterSize=clusterSize, centerVal=centerVal,
               covariatesToInclude=covariatesToInclude,covariatesToExclude=covariatesToExclude)

    # evaluate clustering - clustereval
    clusterEval(cohortid, agegroup, gender, type)

    # visualise clusters - clusterVisual
    clusterVisual(cohortid, agegroup, gender, type)

    writeLines('Clustering completed...')
  }
}

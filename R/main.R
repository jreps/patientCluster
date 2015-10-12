# main file
require(DatabaseConnector)
require(OhdsiRTools)
require(SqlRender)
require(ffbase)

main <- function(){

  # SET WORKING DIRECTORY
  setwd(file.path(getwd(), outputFolder))
  ifelse(!dir.exists(file.path(getwd(), cohortID  )),
         dir.create(file.path(getwd(), cohortID)), FALSE)

  # DATABASE CONNECTION:
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port,
                                                                  schema = cdmDatabaseSchema)



  # extract data - extractdata
  extractdata()
  # perform clustering createclusters
  createHistoryClusters()
  createObsClusters()

  # evaluate clustering - clustereval
  clusterEval()

  # visualise clusters - clusterVisual
  clusterVisual()
}

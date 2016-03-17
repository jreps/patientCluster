.testCode <- function() {
  # load library
  library(patientCluster)
  library(h2o)
  #options(fftempdir = "s:/FFtemp")
  options(ffmaxbytes = min(getOption("ffmaxbytes"),.Machine$integer.max * 12))

  # set connection details
  pw <- NULL
  user <-  NULL
  dbms <- "pdw" #"sql server"
  server <- "server name"
  cdmDatabaseSchema <- 'CDM.dbo'
  workDatabaseSchema <- "scratch.dbo"
  port <- "port number"
  sqlType <- 'pdw'

  # initiate h2o (in this example using all cores and 50GB or RAM)
  h2o.init(nthreads=-1, max_mem_size = '50g')

  outputFolder <- 'S:/temp/patientClusters'
  dbconnection <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                             server = server,
                                                             user = user,
                                                             password = pw,
                                                             port = port,
                                                             schema = cdmDatabaseSchema)

  # get the cohort data for cohort 2000006292 using default concept_groups and
  # using records that are recorded 1-365 days prior to the cohort index to construct features
  d1 <- dataExtract(dbconnection, cdmDatabaseSchema,
                    cohortDatabaseSchema=cdmDatabaseSchema,
                    workDatabaseSchema='scratch.dbo',
                    cohortid=2000006292,  gender=NULL,
                    type='group', groupDef = 'default',
                    historyStart=1,historyEnd=365,  ffloc="s:/FFtemp")


  # cluster the data that used predefined groups as features using kmeans into 10 clusters
  # with normalised scaling prior to clustering.  This only clusters males age between 0 and 100
  c1 <- clusterPeople(d1, ageSpan=c(0,110), gender=NULL,
                       method='kmeans', clusterSize=100, normalise=T, fraction=F, binary=F,
                       covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL)

  c1.concensus <- clusterPeople(d1, ageSpan=c(0,100), gender=NULL,
                                method='concensus', clusterSize=100, glrmFeat=NULL,
                                normalise=F, fraction=T, binary=T,
                                covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL,
                                extraparameters = list(csample=0.8, rsample=0.9, repeats=10)
  )



  # get summary stats for each cluster
  c1.eval <- clusterEval(c1)
  c1c.eval <- clusterEval(c1.concensus)

  # output data for javascript visual
  coi <- (1:100)[table(c1$clusters$predict)>280]
  jsonFormat(c1.eval, coi)

  # produce R bar char plots for each cluster
  clusterVisual(c1)




  # get the cohort data for cohort 2000006292 using all condition_concept_ids and
  # using records that are recorded 1-180 days prior to the cohort index to construct features
  d2 <-   dataExtract(dbconnection=dbconnection, cdmDatabaseSchema=cdmDatabaseSchema,
                      cohortDatabaseSchema=cdmDatabaseSchema,
                      workDatabaseSchema='scratch.dbo',
                      cohortid=2000006292, agegroup=NULL, gender=NULL,
                      type='condition', groupDef = NULL,
                      historyStart=1,historyEnd=180,  loc=getwd())


  # cluster the data that used all condition_concept_ids as features using glrm to reduce the
  # dimensionality to 50 data-driven topics and then apply kmeans to obtain 10 clusters
  # with fraction scaling prior to clustering.  This only clusters people age between 20 and 50
  c2 <- clusterPeople(d2, ageSpan=c(20,50), gender=NULL,
                       method='glrm', glrmFeat=50, clusterSize=50, normalise=F, fraction=T,
                       covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL)


}

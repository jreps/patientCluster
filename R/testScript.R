.testCode <- function() {
  # load library
  library(patientCluster)

  # set connection details
  pw <- NULL
  user <-  NULL
  dbms <- "pdw" #"sql server"
  server <- 'JRDUSAPSCTL01'
  cdmDatabaseSchema <- 'CDM_CPRD_v5.dbo' #'CDM_CPRD.dbo' #"CDM_CPRD_V5" #"cdm_truven_mdc[dr].dbo"
  workDatabaseSchema <- "scratch.dbo"
  port <- 17001 #NULL #17001
  sqlType <- 'pdw' #'sql server'#'pdw'

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
                    cohortid=2000006292, agegroup=NULL, gender=NULL,
                    type='group', groupDef = 'default',
                    historyStart=1,historyEnd=365,  loc=getwd())

  # get the cohort data for cohort 2000006292 using all condition_concept_ids and
  # using records that are recorded 1-180 days prior to the cohort index to construct features
  d2 <-   dataExtract(dbconnection=dbconnection, cdmDatabaseSchema=cdmDatabaseSchema,
                      cohortDatabaseSchema=cdmDatabaseSchema,
                      workDatabaseSchema='scratch.dbo',
                      cohortid=2000006292, agegroup=NULL, gender=NULL,
                      type='condition', groupDef = NULL,
                      historyStart=1,historyEnd=180,  loc=getwd())


  # cluster the data that used predefined groups as features using kmeans into 10 clusters
  # with normalised scaling prior to clustering.  This only clusters males age between 0 and 100
  c1 <- clusterPeople1(d1, ageSpan=c(0,100), gender=8507,
                       method='kmeans', clusterSize=10, normalise=T,
                       covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL,
                       loc=getwd())

  # cluster the data that used all condition_concept_ids as features using glrm to reduce the
  # dimensionality to 50 data-driven topics and then apply kmeans to obtain 10 clusters
  # with fraction scaling prior to clustering.  This only clusters people age between 20 and 50
  c2 <- clusterPeople1(d2, ageSpan=c(20,50), gender=NULL,
                       method='glrm', glrmfeat=50, clusterSize=10, normalise=F, fraction=T,
                       covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL,
                       loc=getwd())

  # get summary stats for each cluster
  clustSum <- clusterEval(c1)

  # produce bar char plots for each cluster
  clusterVisual(c1)
}

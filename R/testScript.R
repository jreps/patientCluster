.testCode <- function() {
  library(patientCluster)
  pw <- NULL
  user <-  NULL

  dbms <- "pdw" #"sql server"
  server <- 'JRDUSAPSCTL01' #'RNDUSRDHIT05' #'JRDUSAPSCTL01'# "RNDUSRDHIT07.jnj.com"
  cdmDatabaseSchema <- 'CDM_CPRD_v5.dbo' #'CDM_CPRD.dbo' #"CDM_CPRD_V5" #"cdm_truven_mdc[dr].dbo"
  workDatabaseSchema <- "scratch.dbo"
  port <- 17001 #NULL #17001
  sqlType <- 'pdw' #'sql server'#'pdw'

  h2o.init(nthreads=-1, max_mem_size = '50g')

  outputFolder <- 'S:/temp/patientClusters'
  dbconnection <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                             server = server,
                                                             user = user,
                                                             password = pw,
                                                             port = port,
                                                             schema = cdmDatabaseSchema)

  d1 <- dataExtract(dbconnection, cdmDatabaseSchema,
                    cohortDatabaseSchema=cdmDatabaseSchema,
                    workDatabaseSchema='scratch.dbo',
                    cohortid=2000006292, agegroup=NULL, gender=NULL,
                    type='group', groupDef = 'default',
                    historyStart=1,historyEnd=365,  loc=getwd())

  d2 <-   dataExtract(dbconnection=dbconnection, cdmDatabaseSchema=cdmDatabaseSchema,
                      cohortDatabaseSchema=cdmDatabaseSchema,
                      workDatabaseSchema='scratch.dbo',
                      cohortid=2000006292, agegroup=NULL, gender=NULL,
                      type='condition', groupDef = 'default',
                      historyStart=1,historyEnd=365,  loc=getwd())


  c1 <- clusterPeople1(d1, ageSpan=c(0,100), gender=8507,
                       method='kmeans', clusterSize=10, normalise=T,
                       covariatesToInclude=NULL,covariatesToExclude=NULL,covariatesGroups=NULL,
                       loc=getwd())

  clustSum <- clusterEval(c1)

  clusterVisual(c1)
}

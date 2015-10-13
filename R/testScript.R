pw <- NULL
user <-  NULL

dbms <- "pdw" #"sql server"
server <- 'JRDUSAPSCTL01' #'RNDUSRDHIT05' #'JRDUSAPSCTL01'# "RNDUSRDHIT07.jnj.com"
cdmDatabaseSchema <- 'CDM_CPRD_v5.dbo' #'CDM_CPRD.dbo' #"CDM_CPRD_V5" #"cdm_truven_mdc[dr].dbo"
workDatabaseSchema <- "scratch.dbo"
port <- 17001 #NULL #17001
sqlType <- 'pdw' #'sql server'#'pdw'

outputFolder <- 'C:/Users/jreps/Documents/test'
library(DatabaseConnector)
dbconnection <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                           server = server,
                                                           user = user,
                                                           password = pw,
                                                           port = port,
                                                           schema = cdmDatabaseSchema)



library(patientCluster)
runMain(dbconnection, cdmDatabaseSchema, workDatabaseSchema,
                    outputFolder,
                    cohortid=413, agegroup=3, gender=8507,type='history',
                    method='kmeans', clusterSize=10, centerVal=T,
                    covariatesToInclude=NULL,covariatesToExclude=NULL)

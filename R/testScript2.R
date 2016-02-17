.testCode2 <- function() {
  # load library
  library(patientCluster)
  library(h2o)
  options(fftempdir = "s:/FFtemp")
  options(ffmaxbytes = min(getOption("ffmaxbytes"),.Machine$integer.max * 12))

# set connection details
pw <- NULL
user <-  NULL
dbms <- "pdw" #"sql server"
server <- "server name"
cdmDatabaseSchema <- "cdm.dbo"
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



test <- clusterConcepts(dbconnection,cdmDatabaseSchema,
                method='glrm', clusterSize=150,topicSize=100, scale=T,
                covariatesToInclude=NULL,
                indications=T, dayStart=1,dayEnd=30,
                use_min_obs=TRUE, min_obs=365, extraparameters=NULL)

dd.defs <- topicEval(test)

topicJsonFormat(dd.defs, loc=getwd())



}









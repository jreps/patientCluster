# extracts the necessary data using the cohortid

extractData <- function(dbconnection, cdmDatabaseSchema, workDatabaseSchema,
                        cohortid, agegroup, gender,
                        type=c('history', 'stats'), method='kmeans')
{
  #loc <- file.path(.libPaths(), 'patientCluster','libs','x64')
  loc <- file.path(getwd(), 'src')

  # do checks for input
  writeLines('Checking input...')
  # .checkData()
  # if fails write error message

  # extract the data into ffdf
  writeLines('Extracting data...')
  conn <- connect(dbconnection)
  sql <- readSql(file.path(loc,'count_extractor.sql'))
  sql <- renderSql(sql,
                   database = cdmDatabaseSchema,
                   outDatabase = workDatabaseSchema,gender=gender, ageupper=age[agegroup,2], agelower=age[agegroup,1],
                   cohortID = cohortID)$sql
  sql <- translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
  writeSql(sql, file.path(outputfolder,"rendered_count_extractor.sql"))
  result <- querySql(conn, sql)
  dbDisconnect(conn)
  if(length(result)<1){result <- 0}

  # output results and progress to user
  writeLines(paste('Found ',as.double(result[1]),' distinct patients in cohort_id ',cohortID,' in age and gender stratum', sep=''))
  writeLines(ifelse(as.double(result[1])>1, 'Enough patients... Extracting Data', 'not Enough patients... Terminating') )
  if(result[1]==0){return()}

  # extract definitions of interest into table: rows of definition, concept_id
  definitions <- read.table(file.path(loc,'definitions.txt'), sep=':')
  definitions <- cbind(strsplit(paste(apply(definitions, 1, function(x)
      paste(rep(x[1],length(strsplit(as.character(x[2]),',')[[1]])), collapse=':')),collapse=':'),':')[[1]]
    ,strsplit(paste(as.character(definitions[,2]), collapse=','),',')[[1]])
  colnames(definitions) <- c('covariate','concept_id')

  # load this table into the sql table named covariates in outputdatabase
  dbconnection2 <- dbconnection
  dbconnection2$schema <- workDatabaseSchema
  conn2 <- connect(dbconnection2)
  insertTable(conn2, tableName, data, dropTableIfExists = TRUE,
              createTable = TRUE, tempTable = FALSE, oracleTempSchema = NULL)
  dbDisconnect(conn2)

    # extracting features
    conn <- connect(dbconnection)
    sql <- readSql(file.path(loc,paste(type,'cohortCluster.sql', sep='')))
    sql <- renderSql(sql,
                     database = cdmDatabaseSchema, outDatabase = workDatabaseSchema,
                     gender=gender, ageupper=age[agegroup,2], agelower=age[agegroup,1],
                     cohortID = cohortID, definitions=...,
                     history = history,
                     definitionNames= paste('[',unique(definitions[,1]),']', sep='',collapse=','))$sql
    sql <- translateSql(sql = sql, sourceDialect = "sql server", targetDialect = sqlType)$sql
    writeSql(sql, file.path(input.loc, paste("rendered_",type,"_extraction.sql")))
    clust.data <- as.ffdf(querySql(conn, sql))
    writeLines('Data extracted ...')

    # save the data to output folder
    if(!dir.path(getwd(),cohortid,'extractedData',type)){dir.create(dir.path(getwd(),cohortid,'extractedData',type))}
    save.ffdf(clust.data, file.path(getwd(),cohortid,'extractedData',type, paste(agegroup,'_',gender, sep='')))
    dbDisconnect(conn)
}



connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port,
                                                                schema = cdmDatabaseSchema)

extractData()


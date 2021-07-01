#====================================
#ODBC function to connect to database
#====================================
odbc <- function(accdb.name, db.dir,dsn, sqtable, fields)
{
  package.already.there <- any(search()=="package:RODBC")
  if(!package.already.there)
    require(RODBC,quietly=TRUE)
  #require(odbc)
  #require(DBI)
  my_DB_path <- file.path(db.dir,accdb.name)
  # The driver name (can find these in your ODBC connections)
  #driver <- "{Microsoft Access Driver (*.mdb, *.accdb)}"
  # Paste together the connection string
  #con_str <- paste0("Driver=",driver,";", "Dbq=", my_DB_path,";")
  #connect
  #con <- DBI::dbConnect(odbc::odbc(),.connection_string=con_str )
  channel <- odbcConnectAccess2007(my_DB_path) 
  query <- paste("SELECT ", fields, " FROM [", sqtable, "]", sep="")
  imported <- sqlQuery(channel, query)
  odbcClose(channel)
  if(!package.already.there)
    detach("package:RODBC")
  return(imported)
}

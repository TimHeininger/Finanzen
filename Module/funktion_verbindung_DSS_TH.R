


database_connection_DSS_TH<-function(database_name,database_pwd,prod_test ="PROD"){
  
  if(prod_test=="PROD"){
    con<-dbConnect(
      odbc::odbc(),
      "Greenplum DSSPROD",
      uid = database_name,
      pwd = database_pwd,
      encoding = "windows-1252"
    )
  }else if(prod_test=="TEST"){
    con<-dbConnect(
      odbc::odbc(),
      "Greenplum DSSTEST",
      uid = database_name,
      pwd = database_pwd,
      encoding = "windows-1252"
    )
  }
  
  
  return(con)
  
}













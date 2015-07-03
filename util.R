testdata <<- sqlFetch(conn,"test2")
count <- nrow(testdata)

while(TRUE){
  for(i in 1:20){
    if(count == nrow(sqlFetch(conn,"test2"))){
      cat("nothings changed...")
      next 
    }else{
      testdata <<- sqlFetch(conn,"test2")
      count <- nrow(testdata)
      cat("things dun changed...")
    }
  }
  if(i == 5){
    break
  }
  Sys.sleep(15000)
}
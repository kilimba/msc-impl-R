library(RODBC)
library(lubridate)

# connUR <- odbcConnect("unifiedReports")
conn <- odbcConnect("postgres")

language <- sqlFetch(conn,"language")
hhvariables <- sqlFetch(conn,"hhvariable")

for(i in language$language){
  hhvariables[,i] <- as.character(hhvariables[,i])
} 

hhresponse <- sqlFetch(conn,"hhresponse")

# Change to lowercase all language responses
for(i in language$language){
  hhresponse[,i] <- tolower(hhresponse[,i])
}

indvariables <- sqlFetch(conn,"indvariable")

for(i in language$language){
  indvariables[,i] <- as.character(indvariables[,i])
} 



getHSEData <- function(){
  if(file.exists("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.rds")){
    return(readRDS("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.rds"))
  }else{
    data <- read.csv("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.csv")
    saveRDS(data,"//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Households/RD06-99 ACDIS HSE-H All.rds")
    return(data)
  }
  return(read.csv())
}

getIndividualData <- function(){
  if(file.exists("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.rds")){
    return(readRDS("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.rds"))
  }else{
    data <- read.csv("//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.csv")
    saveRDS(data,"//acs-fs02/Shared/RDM/Datasets/ACDIS/201501/Modules/HSE/Individuals/RD07-99 ACDIS HSE-I All.rds")
    return(data)
  }
  
}

hsedata <- getHSEData()

hhCounts <- sqlQuery(conn,"select  DATEPART(YEAR,VisitDate) as Year,COUNT(HHIntId)as Count from vacHSE_Households
GROUP BY DATEPART(YEAR,VisitDate)
ORDER BY Year")

# test <- sqlQuery(conn,"select count(*) as Count,IsElectrified,DATEPART(YEAR,VisitDate) as Year
# from vacHSE_Households
# GROUP BY DATEPART(YEAR,VisitDate),IsElectrified")

addYearCol <- function(data) {
  data$Year <- year(data$VisitDate)
  return(data)
}

getTable <- function(data, variable){  
  return(table(data[,"Year"],data[,variable]))
}

getYearlyTotals <- function(table){
  Total <- margin.table(table,1)
  table <- cbind(table,Total)
  total <- as.data.frame(table[,"Total"])
  names(total) <- "Total"
  yearlyTotals <<- total
} 

getDataFrame <- function(table){
  return(as.data.frame(table,stringsAsFactors = F))
}

getData <- function(variable){
  #browser()
  responsevar <- variable
  data <- hsedata
  validresponse <- subset(hhresponse,hhresponse$variable == responsevar)
  data <- addYearCol(data)
  table <- getTable(data,variable)
  getYearlyTotals(table)
  df <- getDataFrame(table)
  df$total <- yearlyTotals[df$Var1,]
  df$rate <- (df$Freq/df$total)*100
  df <- merge(df,validresponse,by.x = "Var2",by.y = "validresponse")
  df <- df[order(df$Var2,df$Var1),]
  return(df)
}


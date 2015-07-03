require(sqldf)


dsYearOfDeath <- sqlQuery(conn,"select distinct
                          vrd.YearOfDeath
                          from vacvaUnifiedReportDeaths vrd
                          order by vrd.YearOfDeath desc")


#vacvaUnifiedReportDeaths <- sqlFetch(conn,"vacvaUnifiedReportDeaths")

getvacvaUnifiedReportDeaths <- function(conn){
  sqlFetch(conn,"vacvaUnifiedReportDeaths")
}

getVAData <- function(vacUnifiedReports, vacvaUnifiedReportDeaths){
  data <- subset(vacUnifiedReports, vacUnifiedReports$RoundType == 3)
  data <- sqldf("select * from data where Acronym like 'VAN%'")
  deaths <- subset(vacvaUnifiedReportDeaths, vacvaUnifiedReportDeaths$YearOfDeath %in% dsYearOfDeath$YearOfDeath)
  merged <- merge(data, deaths , by.x = "QuestionnaireAllocation", by.y = "questionnaireAllocation")
  
  merged <- merged[c('YearOfDeath','MonthOfDeath','DeathMonth','Section','DocumentStatus')]
  
 return(merged)
}


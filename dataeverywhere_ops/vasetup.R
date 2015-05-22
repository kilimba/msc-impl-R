dsYearOfDeath <- sqlQuery(conn,"select distinct
                          vrd.YearOfDeath
                          from dbo.vacvaUnifiedReportDeaths vrd
                          order by vrd.YearOfDeath desc")

getVARoundData <- function(round){
  
}
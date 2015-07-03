library(RODBC)
conn <- odbcConnect("unifiedReports")

vacUnifiedReports <- sqlFetch(conn, "vacUnifiedReports")

dsDSRoundA <- sqlQuery(conn,"SELECT ExtID, RIGHT(Name, 3) AS Name
                       FROM DSRounds AS d
                       WHERE (RoundType = 1) AND (ExtID < 97)
                       order by d.EndEvent desc, d.StartEvent asc, d.ExtID ASC")

#rounds <- sqlFetch(conn, "_DSRounds")

dsSurveyA <- sqlQuery(conn, "SELECT     
                      CASE 
                      WHEN q.Acronym LIKE 'BSB' THEN 'Normal' 
                      WHEN q.Acronym IN ('QAB', 'QBB') THEN 'QC' 
                      WHEN q.Acronym = 'BVB' THEN 'BrokenDown' 
                      WHEN q.Acronym IN ('AQB', 'BQB') THEN 'Adhoc' 
                      WHEN q.Acronym = 'BTB' THEN 'Resident' 
                      WHEN q.Acronym = 'NRB' THEN 'Non-Resident' 
                      WHEN q.Acronym = 'RVB' THEN 'Refusal Verification'
                      WHEN q.Acronym LIKE 'VA%' THEN 'Verbal Autopsy' 
                      END AS Survey
                      FROM Questionnaires AS q
                      WHERE (IsBundle = 'Y') 
                      AND (RoundType = 1) --For type A only
                      AND (Acronym IN ('BSB', 'QAB', 'QBB', 'BVB', 'AQB', 'BQB', 'BTB', 'NRB', 'RVB', 'VAB'))")

dsWeekA <- sqlQuery(conn, "SELECT DISTINCT Week AS WeekBlock
                    FROM NewWeekBlocks AS nwb
                    WHERE (RoundType = 1)
                    UNION
                    SELECT 0 AS WeekBlock
                    ORDER BY WeekBlock  ")

dsSectionA <- sqlQuery(conn, "SELECT DISTINCT(
                       CASE 
                       WHEN DocumentStatus = 0 THEN '00-Registry - Allocated but not yet printed'
                       WHEN DocumentStatus = 10 THEN '10-Registry: Printed but not yet distributed'
                       WHEN DocumentStatus = 19 THEN '19-Registry - Document Management'
                       WHEN DocumentStatus = 20 THEN '20-Field' 
                       WHEN DocumentStatus = 30 THEN '30-QC'
                       WHEN DocumentStatus = 31 THEN '31-CLO'
                       WHEN DocumentStatus = 32 THEN '32-GIS'
                       WHEN DocumentStatus = 33 THEN '33-Migration'
                       WHEN DocumentStatus = 34 THEN '34-Special Task'
                       WHEN DocumentStatus = 35 THEN '35-Data Cleaning'
                       WHEN DocumentStatus = 36 THEN '36-DQA'
                       WHEN DocumentStatus = 39 THEN '39-SQA'
                       WHEN (DocumentStatus = 40 
                       OR DocumentStatus = 50 
                       OR DocumentStatus = 51) THEN '40-DCP'
                       --WHEN DocumentStatus = 50 THEN '50-First Capture'	--Now Type A, B and C
                       --WHEN DocumentStatus = 51 THEN '51-Second Capture'	--Now Type A, B and C
                       --WHEN DocumentStatus = 55 THEN '55-Validate'		--Only Type C
                       WHEN DocumentStatus = 60 THEN '60-Archived'
                       WHEN DocumentStatus = 91 THEN '91-Lent Out'
                       WHEN DocumentStatus = 95 THEN '95-Recycle Bin'
                       WHEN DocumentStatus = 100 THEN '100-Scanned'
                       ELSE '91-Lent Out'
                       END) Section
                       FROM DocumentStatuses")

dsSupervisor <- sqlQuery(conn, "SELECT DISTINCT
                         nwb.Supervisor
                         FROM dbo.NewWeekBlocks nwb
                         WHERE nwb.RoundType = 1 --Type A
                         UNION
                         SELECT 'X' Supervisor
                         ORDER BY Supervisor ASC")

dsFieldworkerA <- sqlQuery(conn, "SELECT DISTINCT
                           nwb.Fieldworker
                           FROM dbo.NewWeekBlocks nwb
                           WHERE nwb.RoundType = 1
                           UNION
                           SELECT 'X' Fieldworker
                           ORDER BY Fieldworker ASC")

dsBundleAFunction <- function(round){
  
  query <- paste("SELECT
                 q.Acronym
                 , CONVERT(Varchar(55)
                 , q.Acronym + ' - ' + q.Name) AS Name
                 FROM Questionnaires AS q 
                 JOIN QuestionnaireAssignments AS qa ON qa.Questionnaire = q.Questionnaire 
                 JOIN StudyAssignments AS sa ON sa.Questionnaire = q.Questionnaire
                 WHERE (q.RoundType = 1) 
                 AND (qa.DSRound =", round,")
                 AND (q.Acronym LIKE 'BSUv%' OR q.Acronym LIKE 'BSRv%' OR q.Acronym LIKE 'BSVv%' 
                 OR q.Acronym LIKE 'BSU-QCv%' OR q.Acronym LIKE 'RVFv%' OR q.Acronym = 'BSU-R')
                 ORDER BY sa.Bundle, sa.Sequence, q.IsBundle DESC") 
  
  
  
  result <-  sqlQuery(conn, query)
  return(result)
  
}



dsDocumentStatus <- sqlQuery(conn,"SELECT 
                             ds.DocumentStatus
                             , ds.StatusDescription
                             FROM DocumentStatuses ds
                             ORDER BY DS.DocumentStatus ASC")

getRoundData <- function(round){
  #browser()
  rnd <- as.character(round)
  bundle <- dsBundleAFunction(rnd) 
  data <- subset(vacUnifiedReports,
                 vacUnifiedReports$DSRound == rnd)
  data <- subset(data, data$Survey %in% dsSurveyA$Survey)
  data <- subset(data, data$Week %in% dsWeekA$WeekBlock)
  data <- subset(data, data$Section %in% dsSectionA$Section)
  data <- subset(data, data$Supervisor %in% dsSupervisor$Supervisor)
  data <- subset(data, data$FieldWork %in% dsFieldworkerA$Fieldworker)
  data <- subset(data, data$DocumentStatus %in% dsDocumentStatus$DocumentStatus)
  data <- subset(data, data$Acronym %in% bundle$Acronym)
  data <- data[c('DSRound','Survey','Week','Section',
                 'Supervisor','FieldWork','DocumentStatus',
                 'Acronym')]
  
  
  return(data)
}

getRoundDataPerWeek <- function(data,week){
  weekdata <- subset(data, data$Week == week)
  return(weekdata)
}

getContingencyTable <- function(data){
  #browser()
  table <- table(data$DocumentStatus,data$Survey)  
  Total <- margin.table(table,1)
  table <- cbind(table,Total)
  Total <- as.vector(margin.table(table,2))
  table <- rbind(table,Total)
  # 60 represents a document status of "Archived"
  # Wrapped in tryCatch() function to handle cases where this data is not available
  totalArchived <<- tryCatch(
    {
      table["60","Total"]
    },
     error = function(cond) {       
       # Choose a return value in case of error
       return(0)
     }
    
    )
  
  
  # 20 represents a document status of "Collated for Distribution"
  totalField <<-  tryCatch(
    {
      table["20","Total"]
    },
  error = function(cond) {       
    # Choose a return value in case of error
    return(0)
  }

  )
  # 50 represents a document status of "Captured"
  totalCaptured <<- tryCatch(
    {
      table["50","Total"]
    },
    error = function(cond) {       
    # Choose a return value in case of error
    return(0)
    }

  )
  
  totalDocs <<- table["Total","Total"]
  
  table <- as.data.frame.matrix(table)
  table <- table[rowSums(table)!=0, ] 
  table <- table[,colSums(table)!=0 ] 
  return(table)
}

getTotalArchived <- function(){
  return(totalArchived)
}

getTotalDocs <- function(){
  return(totalDocs)
}

getTotalField <- function(){
  return(totalField)
}

getTotalCaptured <- function(){
  return(totalCaptured)
}


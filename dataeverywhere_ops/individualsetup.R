#library(RODBC)
#conn <- odbcConnect("unifiedReports")

dsDSRoundB <- sqlQuery(conn,"SELECT
              d.ExtID
              , RIGHT(Name, 3) + '  ('+ CAST(d.ExtID AS VARCHAR) +')' AS NAME
              FROM DSRounds AS d
              WHERE (RoundType = 2) 
              --ORDER BY ExtID DESC
              ORDER BY d.EndEvent DESC, d.ExtID ASC")

dsDSRoundBCurrent <- sqlQuery(conn,"SELECT TOP 1
                              ExtID
                              , RIGHT(Name, 3) AS Name
                              FROM DSRounds AS d
                              WHERE (RoundType = 2) 
                              ORDER BY d.EndEvent DESC, ExtID ASC")

getCurrentRound <- function(){  
  return(dsDSRoundBCurrent[,"ExtID"])  
}

dsSurveyB <- sqlQuery(conn,"SELECT     
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
                    	AND (RoundType = 2) --For type B only
                    	AND (Acronym IN ('BSB', 'QAB', 'QBB', 'BVB', 'AQB','BQB', 'BTB', 'NRB', 'RVB', 'VAB'))")

dsWeekB <- sqlQuery(conn,"SELECT DISTINCT 
                    nwb.Week AS WeekBlock
                    FROM NewWeekBlocks AS nwb
                    WHERE (nwb.RoundType = 2)
                    UNION
                    SELECT
                    0 AS WeekBlock
                    ORDER BY WeekBlock")

dsSectionB <- sqlQuery(conn,"SELECT DISTINCT(
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
                      FROM DocumentStatuses
                      ")

dsSupervisorB <- sqlQuery(conn,"SELECT DISTINCT
                          nwb.Supervisor
                          FROM dbo.NewWeekBlocks nwb
                          WHERE nwb.RoundType = 2 --Type B
                          UNION
                          SELECT 'X' Supervisor
                          ORDER BY Supervisor ASC")

dsFieldworkerB <- sqlQuery(conn,"SELECT DISTINCT
                          nwb.Fieldworker
                          FROM dbo.NewWeekBlocks nwb
                          WHERE nwb.RoundType = 2
                          UNION
                          SELECT 'X' Fieldworker
                          ORDER BY Fieldworker ASC")

dsBundleB <- sqlQuery(conn,"SELECT
                      q.Acronym
                      , CONVERT(Varchar(55)
                      , q.Acronym + ' - ' + q.Name) AS Name
                      FROM Questionnaires AS q 
                      JOIN QuestionnaireAssignments AS qa ON qa.Questionnaire = q.Questionnaire 
                      JOIN StudyAssignments AS sa ON sa.Questionnaire = q.Questionnaire
                      WHERE (q.RoundType = 2) 
                      AND (qa.DSRound = @dsDSRound) 
                      AND (q.Acronym LIKE 'IHO%'  --for 2013 
                      	or q.Acronym LIKE 'CF%' --for 2012
                      	OR q.Acronym LIKE '%GH-QCv%') --CF%
                      ORDER BY sa.Bundle, sa.Sequence, q.IsBundle DESC")

dsBundleBFunction <- function(round){
  
  query <- paste("SELECT
                q.Acronym
                , CONVERT(Varchar(55)
                , q.Acronym + ' - ' + q.Name) AS Name
                FROM Questionnaires AS q 
                JOIN QuestionnaireAssignments AS qa ON qa.Questionnaire = q.Questionnaire 
                JOIN StudyAssignments AS sa ON sa.Questionnaire = q.Questionnaire
                WHERE (q.RoundType = 2) 
                AND (qa.DSRound =", round,")
                AND (q.Acronym LIKE 'IHO%'  --for 2013 
                	or q.Acronym LIKE 'CF%' --for 2012
                	OR q.Acronym LIKE '%GH-QCv%') --CF%
                ORDER BY sa.Bundle, sa.Sequence, q.IsBundle DESC")
  
  result <-  sqlQuery(conn, query)
  return(result)
}

dsDocumentStatusB <- sqlQuery(conn,"SELECT 
ds.DocumentStatus
, ds.StatusDescription
FROM DocumentStatuses ds
ORDER BY DS.DocumentStatus ASC")

getIndividualRoundData <- function(round){
  
  rnd <- as.character(round)
  bundle <- dsBundleBFunction(rnd)
  
  data <- subset(vacUnifiedReports,
                 vacUnifiedReports$DSRound == rnd)
  data <- subset(data, data$InSample == "Y")
  data <- subset(data, data$Survey %in% dsSurveyB$Survey)
  data <- subset(data, data$Week %in% dsWeekB$WeekBlock)
  data <- subset(data, data$Section %in% dsSectionB$Section)
  data <- subset(data, data$Supervisor %in% dsSupervisorB$Supervisor)
  data <- subset(data, data$FieldWork %in% dsFieldworkerB$Fieldworker)
  data <- subset(data, data$Acronym %in% bundle$Acronym)
  data <- subset(data, data$DocumentStatus %in% dsDocumentStatusB$DocumentStatus)  
  data <- data[c('DSRound','Survey','Week','Section',
                 'Supervisor','FieldWork','DocumentStatus',
                 'Acronym')]
  
  return(data)
}
  
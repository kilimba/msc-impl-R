library(RODBC)
conn <- odbcConnect("unifiedReports")

#vacUnifiedReports <<- sqlFetch(conn, "vacUnifiedReports")

getUnifiedReports <- function(){
  #browser()
  return(sqlFetch(conn, "vacUnifiedReports"))
}

dsDSRoundA <- sqlQuery(conn,"SELECT ExtID, RIGHT(Name, 3) AS Name
                       FROM DSRounds AS d
                       WHERE (RoundType = 1) AND (ExtID < 97)
                       order by d.EndEvent desc, d.StartEvent asc, d.ExtID ASC")

consentData <<- sqlQuery(conn,"SELECT * FROM [ETL_Staging].[dbo].[DE01-HIV Consent Data]")
consentData$HIVRefused <- ifelse(consentData$HIVRefused=='Y','Refused','Consented')

# hhRefusalData <<- sqlQuery(conn,"SELECT RefusalRegisteredIndividual,RefusalHousehold,
#                           RefusalWholeBS,AvoidIndividual,AvoidBoundedStructure from Refusals")

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

getRoundData <- function(round, vacUnifiedReports){
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
  # Get unique weeks. Used to populate dropdown menu
  weeks <<- sort(unique(data$Week))
  data <- data[c('DSRound','Survey','Week','Section',
                 'Supervisor','FieldWork','DocumentStatus',
                 'Acronym')]
  
  
  return(data)
}

getWeeks <- function(){
  return(weeks)
}

getRoundDataPerWeek <- function(data,week){
  weekdata <- subset(data, data$Week == week)
  return(weekdata)
}

getContingencyTable <- function(data,type,period){
  #browser()
  if(type == "va"){
    # Cross Tab of DocumentStatus and Year
    table <- table(data$DocumentStatus,data$YearOfDeath) 
    # Cross Tab of Section and Year
    tab2 <- table(data$Section,data$YearOfDeath)
  }else if(type == "individual"){ 
    #browser()
    data <- subset(data, data$Survey == "Resident")
    # Cross Tab of DocumentStatus and Survey
    table <- table(data$DocumentStatus,data$Survey)
    # Cross Tab of Section and Survey
    tab2 <- table(data$Section,data$Survey)
  }else{
    table <- table(data$DocumentStatus,data$Survey)
    tab2 <- table(data$Section,data$Survey)
  }
  
  #browser()
  
  Total <- margin.table(table,1)
  table <- cbind(table,Total)
  
  Total <- margin.table(tab2,1)
  tab2 <- cbind(tab2,Total)
  
  Total <- as.vector(margin.table(table,2))
  table <- rbind(table,Total)
  
  Total <- as.vector(margin.table(tab2,2))
  tab2 <- rbind(tab2,Total)
  
  table <- as.data.frame(table)
  tab2 <- as.data.frame(tab2)
  
  # 60 represents a document status of "Archived"
  # Wrapped in tryCatch() function to handle cases where this data is not available
  totalArchived <<- tryCatch(
    {
      archived <- c("60","90","91","92","95","97","99","100")
      sum(table[archived,]["Total"],na.rm = TRUE)
    },
     error = function(err) {   
       print(paste("MY_ERROR:  ",err))
       # Choose a return value in case of error
       return(0)
     }
    
    )
  
  # 20 represents a document status of "Collated for Distribution"
  totalField <-  tryCatch(
    {
      # The cumulation of docs currently in field and having passed through field
      field = c("20","21","25","30","31","32","33","34","35","36","39","40",
                "50","51","52","55","56","60","90","91","92","95","97","99","100")
      sum(table[field,]["Total"],na.rm = TRUE)
      
    },
  error = function(cond) {       
    # Choose a return value in case of error
    return(0)
  }

  ) 
  
  # 50 represents a document status of "Captured"
  totalCaptured <- tryCatch(
    {
      captured <- c("50","51","52","55","56","60","90","91","92","95","97","99","100")
      sum(table[captured,]["Total"],na.rm = TRUE)
    },
    error = function(cond) {       
    # Choose a return value in case of error
    return(0)
    }

  )

  totalDocs <- table["Total","Total"]
  
  if(type == "household"){
    totalDocsHousehold <<- totalDocs
    totalCapturedHousehold <<- totalCaptured
    totalFieldHousehold <<- totalField
    totalArchivedHousehold <<- totalArchived
    
  }else if(type == "individual"){
    totalDocsIndividual <<- totalDocs
    totalCapturedIndividual <<- totalCaptured
    totalFieldIndividual <<- totalField
    totalArchivedIndividual <<- totalArchived
    
  }else if(type == "va"){
    totalDocsVa <<- totalDocs
    totalCapturedVa <<- totalCaptured
    totalFieldVa <<- totalField
    totalArchivedVa <<- totalArchived
    
  }
  
  
  table <- table[rowSums(table)!=0, ] 
  table <- table[,colSums(table)!=0 ]
  
  tab2 <- tab2[rowSums(tab2)!=0, ]
  tab2 <- tab2[,colSums(tab2)!=0 ]
  
  weekData4Viz <<- list()
  roundData4Viz <<- list()
  
  if(type == "household"){
    if(period == "round"){
      roundData4Viz$household <<- table
    }else if(period == "week"){
      weekData4Viz$household <<- tab2
    }    
  }else if(type == "individual"){
    if(period == "round"){
      roundData4Viz$individual <<- table
    }else if(period == "week"){
      weekData4Viz$individual <<- tab2
    }
  }else if(type == "va"){
    if(period == "round"){
      roundData4Viz$va <<- table
    }else if(period == "week"){
      weekData4Viz$va <<- tab2
    }
  }
  
  return(table)
}

getTotalArchived <- function(type){
  if(type == "individual" ){
    return(totalArchivedIndividual)
  }else if(type == "household"){
    return(totalArchivedHousehold)
  }else if(type == "va"){
    return(totalArchivedVa)
  }   
  
}

getTotalDocs <- function(type){
  if(type == "individual" ){
    return(totalDocsIndividual)
  }else if(type == "household"){
    return(totalDocsHousehold)
  }else if(type == "va"){
    return(totalDocsVa)
  }       
}

getTotalField <- function(type){
  if(type == "individual" ){
    return(totalFieldIndividual)
  }else if(type == "household"){
    return(totalFieldHousehold)
  }else if(type == "va"){
    return(totalFieldVa)
  }   
}

getTotalCaptured <- function(type){
  if(type == "individual" ){
    return(totalCapturedIndividual)
  }else if(type == "household"){
    return(totalCapturedHousehold)
  }else if(type == "va"){
    return(totalCapturedVa)
  }   
}


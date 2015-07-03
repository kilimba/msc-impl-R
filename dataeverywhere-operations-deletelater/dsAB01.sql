--dsAB01
SELECT 
vur.DSRound
, vur.Survey
, vur.[Week]
, vur.Section
, vur.Supervisor
, vur.FieldWork
, vur.DocumentStatus
, vur.Acronym
, COUNT(*) Qty
FROM dbo.vacUnifiedReports vur
WHERE 
vur.DSRound IN (@pRound)
AND vur.Survey IN (@pSurvey)
AND vur.[Week] IN (@pWeek)
AND vur.Section IN (@pSection)
AND vur.Supervisor IN (@pSupervisor)
AND vur.FieldWork IN (@pFieldWorker)
AND vur.Acronym IN (@pBundle)
AND vur.DocumentStatus IN (@pDocStatus)
GROUP BY vur.DSRound, vur.Survey, vur.[Week], vur.Section, vur.Supervisor, vur.FieldWork, vur.DocumentStatus, vur.Acronym

--dsDSRoundA
SELECT     ExtID, RIGHT(Name, 3) AS Name
FROM         DSRounds AS d
WHERE     (RoundType = 1) AND (ExtID < 97)
order by d.EndEvent desc, d.StartEvent asc, d.ExtID ASC

--dsSurveyA
SELECT     
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
	AND (Acronym IN ('BSB', 'QAB', 'QBB', 'BVB', 'AQB', 'BQB', 'BTB', 'NRB', 'RVB', 'VAB')) --All bundles
	
--dsWeekA
SELECT DISTINCT Week AS WeekBlock
FROM         NewWeekBlocks AS nwb
WHERE     (RoundType = 1)
UNION
SELECT     0 AS WeekBlock
ORDER BY WeekBlock	

--dsSectionA
SELECT DISTINCT(
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

--dsSupervisor
SELECT DISTINCT
nwb.Supervisor
FROM dbo.NewWeekBlocks nwb
WHERE nwb.RoundType = 1 --Type A
UNION
SELECT 'X' Supervisor
ORDER BY Supervisor ASC

--dsFieldworkerA
SELECT DISTINCT
nwb.Fieldworker
FROM dbo.NewWeekBlocks nwb
WHERE nwb.RoundType = 1
UNION
SELECT 'X' Fieldworker
ORDER BY Fieldworker ASC

--dsBundleA
SELECT
q.Acronym
, CONVERT(Varchar(55)
, q.Acronym + ' - ' + q.Name) AS Name
FROM Questionnaires AS q 
JOIN QuestionnaireAssignments AS qa ON qa.Questionnaire = q.Questionnaire 
JOIN StudyAssignments AS sa ON sa.Questionnaire = q.Questionnaire
WHERE (q.RoundType = 1) 
AND (qa.DSRound = @dsDSRound) 
AND (q.Acronym LIKE 'BSUv%' OR q.Acronym LIKE 'BSRv%' OR q.Acronym LIKE 'BSVv%' 
		OR q.Acronym LIKE 'BSU-QCv%' OR q.Acronym LIKE 'RVFv%' OR q.Acronym = 'BSU-R')
ORDER BY sa.Bundle, sa.Sequence, q.IsBundle DESC

--dsDocumetStatus
SELECT 
ds.DocumentStatus
, ds.StatusDescription
FROM DocumentStatuses ds
ORDER BY DS.DocumentStatus ASC

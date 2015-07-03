----VB04_DoD_Bundle_Dashboard
select 
vd.YearOfDeath
, vd.MonthOfDeath
, vd.DeathMonth
--, vr.DSRound
--, vr.Survey 
, vr.Section 
--, vr.Acronym
--, vr.DocumentStatus
, COUNT(*) AS Qty
from dbo.vacUnifiedReports vr
join dbo.vacvaUnifiedReportDeaths vd on vd.questionnaireAllocation = vr.QuestionnaireAllocation
where vr.RoundType = 3
and vr.Acronym like 'VAN%'
and vd.YearOfDeath in (@YearOfDeath)
--and vd.DeathMonth in (@DeathMonth)
--and vr.Section in (@Section)
group by
vd.YearOfDeath, vd.MonthOfDeath, vd.DeathMonth
, vr.DSRound
, vr.Survey 
, vr.Section 
, vr.Acronym
, vr.DocumentStatus
order by vd.YearOfDeath asc, vd.MonthOfDeath asc, vd.DeathMonth
, vr.DSRound
, vr.Survey

----dsYearOfDeath
select distinct
vrd.YearOfDeath
from dbo.vacvaUnifiedReportDeaths vrd
order by vrd.YearOfDeath desc

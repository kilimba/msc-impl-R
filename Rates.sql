--region Rates
--
SELECT
  *
FROM dbo.ACDISDemographyAgeGrps
-- Migration rates
--
SELECT
  ExpYear,SUM(ExpDays) Days,
  SUM(CASE WHEN AgeGrp BETWEEN 9 AND 15 AND Sex='FEM' THEN ExpDays ELSE 0 END) WRADays,
  SUM(InMigrEx) InMigrations,
  SUM(OutMigrEx) OutMigrations
FROM dbo.vacACDISDemographyResident
GROUP BY ExpYear
ORDER BY ExpYear
--
SELECT
  ExpYear,SUM(ExpDays) Days,
  SUM(CASE WHEN AgeGrp BETWEEN 9 AND 15 AND Sex='FEM' THEN ExpDays ELSE 0 END) WRADays,
  SUM(InMigrEx) InMigrations,
  SUM(OutMigrEx) OutMigrations
FROM dbo.ACDISDemographyYr
GROUP BY ExpYear
ORDER BY ExpYear
--
-- MidYear Population
--
SELECT
  ExpYear,COUNT(*) n
FROM dbo.vacACDISDemographyResident
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND EpisodeType=1
GROUP BY ExpYear
ORDER BY ExpYear
--
-- MidYear Population (All)
--
SELECT
  ExpYear,COUNT(*) n
FROM dbo.ACDISDemography
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
GROUP BY ExpYear
ORDER BY ExpYear
--
-- MidYear Population by Sex
--
SELECT
  ExpYear,Sex,COUNT(*) n
FROM dbo.vacACDISDemographyResident
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND EpisodeType=1
  AND Sex<>'MIS'
GROUP BY ExpYear,Sex
ORDER BY ExpYear,Sex
--
-- MidYear Population in 1 Year Age Groups
--
SELECT
  ExpYear,AgeGrp,COUNT(*) n
FROM dbo.ACDISDemographyYr
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=1
  AND EpisodeType=1
GROUP BY ExpYear,AgeGrp
ORDER BY ExpYear,AgeGrp
--
-- Birth rate (residents)
--
SELECT
  A.ExpYear,COUNT(*) Births
FROM dbo.vacACDISDemographyResident A
WHERE A.Episode=1
  AND A.AgedIn=1
GROUP BY A.ExpYear
ORDER BY A.ExpYear
--
-- Deaths rate (residents)
--
SELECT
  A.ExpYear,COUNT(*) Deaths
FROM dbo.vacACDISDemographyResident A
WHERE A.Episode=A.Episodes
  AND A.Died=1
  --AND NOT (A.ExpYear=2009 AND A.ExpSem=2) -- Remove for mid year analytical database
  AND A.EpisodeType=1
GROUP BY A.ExpYear
ORDER BY A.ExpYear
--
-- Lost rate (residents)
--
SELECT
  A.ExpYear,COUNT(*) Lost
FROM dbo.vacACDISDemographyResident A
WHERE A.Episode=A.Episodes
  AND A.Lost=1
  AND A.EpisodeType=1
  --AND NOT (A.ExpYear=2009 AND A.ExpSem=2) -- Remove for mid year analytical database
GROUP BY A.ExpYear
ORDER BY A.ExpYear
--
-- Number of occupied BoundedStructures at mid-year
--
SELECT
  ExpYear,COUNT(DISTINCT BSIntID) n
FROM dbo.ACDISDemography
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND NOT BSIntID IS NULL
GROUP BY ExpYear
ORDER BY ExpYear
--endregion
--region Balance Sheet
--
--region Balance Sheet All Members
--
-- Under surveillance at start of year
SELECT
  ExpYear,
  SUM(CASE
        WHEN PeriodStart=0
          AND (AgedIn=0 OR (AgedIn=1 AND AgeGrp>1))
          AND MemberStart=0
          AND InMigrEx=0
        THEN 1
        ELSE 0
      END) UnderSurveillance
FROM dbo.ACDISDemography
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=1
GROUP BY ExpYear
ORDER BY ExpYear
--
-- Under surveillance at start of 2013 1nd Semester
SELECT
  COUNT(DISTINCT IIntID)
FROM dbo.ACDISDemography
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=1
  AND ExpYear=2014
--
SELECT
  COUNT(DISTINCT IIntID)
FROM dbo.ACDISDemography
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=1 --1
  AND ExpYear=2012
--
-- Inflow per year
SELECT
  ExpYear,
  SUM(PeriodStart) SurveillanceStart,
  SUM(AgedIn) Born,
  SUM(MemberStart) MemberStart,
  SUM(InMigrEx) InMigration
FROM dbo.ACDISDemography A
WHERE Episode=1
--  AND NOT (A.ExpYear=2012 AND A.ExpSem=2) -- Remove for mid year analytical database
GROUP BY ExpYear
ORDER BY ExpYear
--
-- Exposure at unknown locations for residents
--
SELECT
  ExpYear,
  SUM(ExpDays) TotalExp,
  SUM(CASE WHEN BSIntID IS NULL THEN ExpDays ELSE 0 END) UnkExp
FROM dbo.ACDISDemography
WHERE EpisodeType=1
GROUP BY ExpYear
ORDER BY ExpYear
--
-- As individuals
--
SELECT
  ExpYear,
  COUNT(DISTINCT IIntID) Residents
FROM dbo.ACDISDemography
WHERE EpisodeType=1
GROUP BY ExpYear
ORDER BY ExpYear
--
SELECT
  ExpYear,
  COUNT(DISTINCT IIntID) ResUnk
FROM dbo.ACDISDemography
WHERE EpisodeType=1
  AND BSIntID IS NULL
GROUP BY ExpYear
ORDER BY ExpYear
--
--
-- Outflow per year
SELECT
  ExpYear,
  SUM(Died) Died,
  SUM(MemberEnd) MemberEnd,
  SUM(OutMigrEx) OutMigration,
  SUM(Lost) Lost,
  SUM(PeriodEnd) PeriodEnd
FROM dbo.ACDISDemography A
WHERE Episode=Episodes
--  AND NOT (A.ExpYear=2012 AND A.ExpSem=2) -- Remove for mid year analytical database
GROUP BY ExpYear
ORDER BY ExpYear
--endregion
--
--region Balance Sheet Residents
--
-- Under surveillance at start of year
SELECT
  ExpYear,
  SUM(CASE
        WHEN PeriodStart=0
          AND (AgedIn=0 OR (AgedIn=1 AND AgeGrp>1))
          AND MemberStart=0
          AND InMigrEx=0
        THEN 1
        ELSE 0
      END) UnderSurveillance
FROM dbo.vacACDISDemographyResident
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=1
GROUP BY ExpYear
ORDER BY ExpYear
--
-- Under surveillance at start of 2010
SELECT
  COUNT(DISTINCT IIntID)
FROM dbo.vacACDISDemographyResident
WHERE PeriodEnd=1
  AND ExpYear=2013
--
SELECT
  COUNT(DISTINCT IIntID)
FROM dbo.vacACDISDemographyResident
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7 --1
  AND ExpYear=2010
--
-- Inflow per year
SELECT
  ExpYear,
  SUM(PeriodStart) SurveillanceStart,
  SUM(AgedIn) Born,
  SUM(MemberStart) MemberStart,
  SUM(InMigrEx) InMigration
FROM dbo.vacACDISDemographyResident A
WHERE Episode=1
--  AND NOT (A.ExpYear=2012 AND A.ExpSem IN (1,2)) -- Remove for mid year analytical database
GROUP BY ExpYear
ORDER BY ExpYear
--
-- Outflow per year
SELECT
  ExpYear,
  SUM(Died) Died,
  SUM(MemberEnd) MemberEnd,
  SUM(OutMigrEx) OutMigration,
  SUM(Lost) Lost,
  SUM(PeriodEnd) PeriodEnd
FROM dbo.vacACDISDemographyResident A
WHERE Episode=Episodes
--  AND NOT (A.ExpYear=2012 AND A.ExpSem IN (1,2)) -- Remove for mid year analytical database
GROUP BY ExpYear
ORDER BY ExpYear

--endregion
--
--endregion
--
--region Mid Year population by age grp
--
-- MidYear Population
--
SELECT
  AgeDescr,ExpYear,Sex,COUNT(*) n
FROM dbo.ACDISDemography D
  JOIN dbo.ACDISDemographyAgeGrps A ON (D.AgeGrp=A.AgeGrp)
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND EpisodeType=1
  AND Sex<>'MIS'
GROUP BY AgeDescr,ExpYear,Sex
ORDER BY AgeDescr,ExpYear,Sex
--
-- Mid-year population everyone under surveillance
--
SELECT
  AgeDescr,ExpYear,Sex,COUNT(*) n
FROM dbo.ACDISDemography D
  JOIN dbo.ACDISDemographyAgeGrps A ON (D.AgeGrp=A.AgeGrp)
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND Sex<>'MIS'
GROUP BY AgeDescr,ExpYear,Sex
ORDER BY AgeDescr,ExpYear,Sex
--
SELECT
  ExpYear,COUNT(*) n
FROM dbo.ACDISDemography D
  JOIN dbo.ACDISDemographyAgeGrps A ON (D.AgeGrp=A.AgeGrp)
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND Sex<>'MIS'
GROUP BY ExpYear
ORDER BY ExpYear
--
SELECT
  ExpYear,COUNT(*) n
FROM dbo.ACDISDemography D
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND Sex<>'MIS'
  AND AgeGrp>=9
GROUP BY ExpYear
ORDER BY ExpYear
--
-- Mid-year population everyone excluding type 3 episodes
--
SELECT
  AgeDescr,ExpYear,COUNT(*) n
FROM dbo.ACDISDemography D
  JOIN dbo.ACDISDemographyAgeGrps A ON (D.AgeGrp=A.AgeGrp)
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND EpisodeType<>3--1
  AND Sex<>'MIS'
GROUP BY AgeDescr,ExpYear
ORDER BY AgeDescr,ExpYear
--
SELECT
  ExpYear,COUNT(*) n
FROM dbo.ACDISDemography D
  JOIN dbo.ACDISDemographyAgeGrps A ON (D.AgeGrp=A.AgeGrp)
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND EpisodeType<>3--1
  AND Sex<>'MIS'
GROUP BY ExpYear
ORDER BY ExpYear
--endregion
--
--region Under 5 migrants
--
SELECT 
  ExpYear,
  SUM(OutMigrEx) OutMigrants,
  SUM(InMigrEx) InMigrants
FROM dbo.ACDISDemography
WHERE AgeGrp BETWEEN 1 AND 6
GROUP BY ExpYear
ORDER BY ExpYear
--
SELECT
  ExpYear,A.AgeDescr,
  SUM(ExpDays) Days,
  SUM(InMigrEx) InMigrations,
  SUM(OutMigrEx) OutMigrations
FROM dbo.vacACDISDemographyResident D
  LEFT JOIN dbo.ACDISDemographyAgeGrps A ON (D.AgeGrp=A.AgeGrp)
GROUP BY ExpYear,A.AgeDescr
ORDER BY ExpYear,A.AgeDescr
--endregion
--
--region Total Fertility Rates
--
-- Residents
SELECT
  ExpYear,AgeGrp,
  SUM(ExpDays) Days,
  SUM(LBCnt) Births
FROM dbo.ACDISDemography
WHERE EpisodeType=1
  AND AgeGrp BETWEEN 9 AND 15 
  AND Sex='FEM'
GROUP BY ExpYear,AgeGrp
ORDER BY ExpYear,AgeGrp
--
-- Residents & Non-residents
SELECT
  ExpYear,AgeGrp,
  SUM(ExpDays) Days,
  SUM(LBCnt) Births
FROM dbo.ACDISDemography
WHERE EpisodeType<>3
  AND AgeGrp BETWEEN 9 AND 15
  AND Sex='FEM'
GROUP BY ExpYear,AgeGrp
ORDER BY ExpYear,AgeGrp
--endregion
--
--region Deaths for INDEPTH
SELECT
  AgeDescr,ExpYear,
  SUM(Died) Deaths,
  SUM(CASE WHEN Died=1 AND NOT(D.ImmediateCause IS NULL AND D.UnderlyingCause IS NULL) THEN 1 ELSE 0 END) Coded
FROM dbo.ACDISDemography A
  JOIN dbo.ACDISDemographyAgeGrps AG ON (A.AgeGrp=AG.AgeGrp)
  JOIN dbo.RegisteredIndividuals I ON (A.IIntID=I.IntID)
  JOIN dbo.Deaths D ON (I.EndEvent=D.IntID)
GROUP BY AgeDescr,ExpYear
ORDER BY AgeDescr,ExpYear
--endregion
--
--region Mid year population in 5-year age groups
--
SELECT
  CASE
    WHEN AgeGrp BETWEEN 1 AND 6 THEN 6
    --WHEN AgeGrp>18 THEN 18
    ELSE AgeGrp
  END AgeGroup,
  ExpYear,Sex,
  SUM(CASE WHEN EpisodeType=1 THEN 1 ELSE 0 END) n,
  COUNT(*) Total
FROM dbo.ACDISDemography D
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  --AND EpisodeType=1
  AND Sex<>'MIS'
GROUP BY   CASE
    WHEN AgeGrp BETWEEN 1 AND 6 THEN 6
    --WHEN AgeGrp>18 THEN 18
    ELSE AgeGrp
  END,ExpYear,Sex
ORDER BY AgeGroup,ExpYear,Sex
--
SELECT
  AgeGrp,ExpYear,Sex,COUNT(*) n
FROM dbo.ACDISDemographyYr D
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND EpisodeType=1
  AND Sex<>'MIS'
GROUP BY AgeGrp,ExpYear,Sex
ORDER BY AgeGrp,ExpYear,Sex
-- Loss rate
SELECT
  CASE
    WHEN AgeGrp BETWEEN 1 AND 6 THEN 6
    WHEN AgeGrp>18 THEN 18
    ELSE AgeGrp
  END AgeGroup,
  A.ExpYear,
  SUM(A.Lost) Loss,
  COUNT(DISTINCT IIntID) n
FROM dbo.ACDISDemography A
WHERE A.EpisodeType=1
GROUP BY  CASE
    WHEN AgeGrp BETWEEN 1 AND 6 THEN 6
    WHEN AgeGrp>18 THEN 18
    ELSE AgeGrp
  END ,A.ExpYear
ORDER BY AgeGroup,A.ExpYear
--
SELECT
  *
FROM dbo.ACDISDemographyAgeGrps


--endregion
--
--region Mid Year Resident Population by AgeGrp & Sex
SELECT
  CASE
    WHEN AgeGrp BETWEEN 1 AND 6 THEN 6
    ELSE AgeGrp
  END AgeGroup,
  ExpYear,Sex,COUNT(*) n
FROM dbo.ACDISDemography D
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
  AND EpisodeType=1
  AND Sex<>'MIS'
GROUP BY   CASE
    WHEN AgeGrp BETWEEN 1 AND 6 THEN 6
    ELSE AgeGrp
  END,ExpYear,Sex
ORDER BY AgeGroup,ExpYear,Sex
--endregion
--region Cause specific death rates
SELECT
  ExpYear,
  SUM(ExpDays) ExposureDays,
  SUM(Died) Deaths,
  SUM(C_Unknown) Unknown,
  SUM(C_CMPN) CMPN,
  SUM(C_AIDS_TB) HIVrelated,
  SUM(C_NonComm) NonComm,
  SUM(C_Injuries) Injuries
FROM dbo.ACDISDemographyYr
GROUP BY ExpYear
ORDER BY ExpYear
--endregion
--
SELECT
  *
FROm dbo.DeathsDxStatuses
--
SELECT
  YEAR(ObservationStart) Yr,
  COUNT(DISTINCT IIntID) n
FROM dbo.ACDISDemography D
WHERE DAY(ObservationStart)=1
  AND MONTH(ObservationStart)=7
GROUP BY YEAR(ObservationStart)
ORDER BY YEAR(ObservationStart)
--
SELECT
  COUNT(DISTINCT IIntId) n
FROM dbo.ACDISDemography
WHERE ExpYear BETWEEN 2000 AND 2011
  AND EpisodeType<3
--
SELECT
  SUM(Died) Deaths
FROM dbo.ACDISDemography
WHERE ExpYear BETWEEN 2000 AND 2009
--
SELECT
  *
FROM dbo.ACDISDemographyAgeGrps
--
SELECT
  COUNT(DISTINCT IIntId)
FROM dbo.ACDISDemographyYr
--
SELECT
  SUM(CASE WHEN NOT P.CivilianID IS NULL THEN 1 ELSE 0 END) HasId,
  COUNT(*) n
FROM dbo.ProtectedIndividuals P
  JOIN dbo.RegisteredIndividuals I ON (P.IntId=I.IntId)
  JOIN dbo.Events EE ON (EE.IntId=I.EndEvent)
  JOIN dbo.Events SE ON (SE.IntId=I.Delivery)
WHERE EE.Type='VIS'
  AND dbo.fnacAgeYears(SE.AssignedDate,EE.AssignedDate)<18
--
WITH CurrentResidents AS (
  SELECT
    IIntId
  FROM dbo.ACDISDemographyYr
  WHERE Episodes=Episode
    AND EpisodeType=1
    AND ExpYear=2013
)
SELECT
  SUM(CASE WHEN NOT P.CivilianID IS NULL THEN 1 ELSE 0 END) HasId,
  COUNT(*) n
FROM dbo.ProtectedIndividuals P
  JOIN CurrentResidents R ON P.IntId=R.IIntId
  JOIN dbo.RegisteredIndividuals I ON (P.IntId=I.IntId)
  JOIN dbo.Events EE ON (EE.IntId=I.EndEvent)
  JOIN dbo.Events SE ON (SE.IntId=I.Delivery)
WHERE EE.Type='VIS'
  AND dbo.fnacAgeYears(SE.AssignedDate,EE.AssignedDate)<18
--
SELECT
  COUNT(DISTINCT IIntID)
FROM dbo.ACDISDemography

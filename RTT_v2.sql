-- !preview conn=DBI::dbConnect(RSQLite::SQLite())


/*=================================================================================================
CREATE RTT Output Table
=================================================================================================*/
DROP TABLE IF EXISTS #BBCS_RRT_PlanningData

CREATE TABLE #BBCS_RRT_PlanningData

(			[Row_Id]					BIGINT IDENTITY(1,1) PRIMARY KEY
,			[Metric]					VARCHAR (20)
,			[DataView]					VARCHAR (20)
,			[Commissioner_Code]			VARCHAR (5)
,			[Org_Code]					VARCHAR (5)
,			[Org_Name]					VARCHAR (255)
,			[YearMonth]					INT
,			[TFC]						VARCHAR (5)
,			[TFC_Desc]					VARCHAR (255)
,			[RTTPartShortName]			VARCHAR(10)

,			[Total_All]					DECIMAL (20,2)
,			[00-18_Weeks]				DECIMAL (20,2)
,			[18-52_Weeks]				DECIMAL (20,2)

,			[18+_Weeks]					DECIMAL (20,2)
,			[52+_Weeks]					DECIMAL (20,2)
,			[Date_Last_Refreshed]		DATETIME

)

--TRUNCATE TABLE ##BSol_RRT_PlanningData

/*=================================================================================================
INSERT BSol ICB RRT Data View
=================================================================================================*/

INSERT INTO		#BBCS_RRT_PlanningData

(			[DataView]
,			[Commissioner_Code]
,			[Org_Code]
,			[Org_Name]
,			[YearMonth]
,			[TFC]
,			[TFC_Desc]
,			[RTTPartShortName]

,			[Total_All]
,			[00-18_Weeks]
,			[18-52_Weeks]

,			[18+_Weeks]
,			[52+_Weeks]
,			[Date_Last_Refreshed]

)

(
SELECT		'ICB'					[DataView]
,			[CommissionerCode]			[Commissioner_Code]
,			[CommissionerCode]			[Org_Code]
,			[CommissionerName]			[Org_Name]
,			[YYYYMM]					[YearMonth]
,			[TreatmentFunctionCode]		[TFC]
,			[TreatmentFunctionName]		[TFC_Desc]
,			[RTTPartShortName]			[RTTPartShortName]

,			SUM([TotalAll])				[Total_All]
,			SUM([0to18Weeks])			[00-18_Weeks]
,			SUM([18to52Weeks])			[18-52_Weeks]

,			SUM([Over18Weeks])			[18+_Weeks]
,			SUM([52WeeksPlus])			[52+_Weeks]
,			GETDATE() 					[Date_Last_Refreshed]

FROM		[AnalystGlobal].[Performance].[ReferralToTreatmentStandardised]


WHERE		(
				[YYYYMM] BETWEEN 202404 AND 202609
				--OR
				-- [YYYYMM] BETWEEN 202604 AND 202608
			)
--AND			[TreatmentFunctionCode] <> '999' -- exclude totals
AND			[CommissionerCode] IN ( SELECT Code FROM [Cluster_BBCS].[Filters].[Commissioner] )

GROUP BY	[CommissionerCode]
,			[CommissionerName]
,			[YYYYMM]
,			[TreatmentFunctionCode]
,			[TreatmentFunctionName]
,			[RTTPartShortName]

)


/*=================================================================================================
INSERT BSol In-System Providers RRT Data View
=================================================================================================*/

INSERT INTO		#BBCS_RRT_PlanningData

(			[DataView]
,			[Commissioner_Code]
,			[Org_Code]
,			[Org_Name]
,			[YearMonth]
,			[TFC]
,			[TFC_Desc]
,			[RTTPartShortName]

,			[Total_All]
,			[00-18_Weeks]
,			[18-52_Weeks]

,			[18+_Weeks]
,			[52+_Weeks]
,			[Date_Last_Refreshed]

)

(
SELECT		'Providers'					[DataView]
,			[CommissionerCode]			[Commissioner_Code]
,										[Org_Code]
,										[Org_Name]
,			[YYYYMM]					[YearMonth]
,			[TreatmentFunctionCode]		[TFC]
,			[TreatmentFunctionName]		[TFC_Desc]
,			[RTTPartShortName]			[RTTPartShortName]

,			SUM([TotalAll])				[Total_All]
,			SUM([0to18Weeks])			[00-18_Weeks]
,			SUM([18to52Weeks])			[18-52_Weeks]

,			SUM([Over18Weeks])			[18+_Weeks]
,			SUM([52WeeksPlus])			[52+_Weeks]
,			GETDATE() 					[Date_Last_Refreshed]

FROM		(
			Select *,
			case when [CommissionerCode] = '15E' and [ProviderCode] in ('RRK', 'RQ3', 'RRJ', 'RYW') THEN [ProviderCode]
				 when [CommissionerCode] = 'D2P2L' and [ProviderCode] in ('RBK', 'RL4', 'RNA', 'RXK') THEN [ProviderCode]
				 ELSE 'Other' END AS [Org_Code],
			case when [CommissionerCode] = '15E' and [ProviderCode] in ('RRK', 'RQ3', 'RRJ', 'RYW') THEN [ProviderName]
				 when [CommissionerCode] = 'D2P2L' and [ProviderCode] in ('RBK', 'RL4', 'RNA', 'RXK') THEN [ProviderName]
				 ELSE 'Other' END AS [Org_Name]
			FROM [AnalystGlobal].[Performance].[ReferralToTreatmentStandardised]
			) sub


WHERE		(
				[YYYYMM] BETWEEN 202404 AND 202609
				--OR
				--[YYYYMM] BETWEEN 202604 AND 202608
			)
--AND			[TreatmentFunctionCode] <> '999' -- exclude totals
--AND			ProviderCode IN ( )	--BSol In-System Providers
AND			[CommissionerCode] IN ( SELECT Code FROM [Cluster_BBCS].[Filters].[Commissioner] )



GROUP BY	[CommissionerCode]
,			[Org_Code]
,			[Org_Name]
,			[YYYYMM]
,			[TreatmentFunctionCode]
,			[TreatmentFunctionName]
,			[RTTPartShortName]

)


/*=================================================================================================
UPDATE Metric Descriptions
=================================================================================================*/

UPDATE		#BBCS_RRT_PlanningData
SET			[Metric] ='Waiters'
WHERE		RTTPartShortName= 'IU'


UPDATE		#BBCS_RRT_PlanningData
SET			[Metric] ='Clock Stops'
WHERE		RTTPartShortName IN ('AU','NU')


UPDATE		#BBCS_RRT_PlanningData
SET			[Metric] ='Clock Starts'
WHERE		RTTPartShortName IN ('NP')






/*=================================================================================================
UFinal RRT Output Extract
=================================================================================================*/


Select starts.Commissioner_Code
, starts.Org_Code
, starts.Org_Name
, starts.TFC as Specialty
, convert(date, cast(starts.YearMonth as char(6)) + '01',112) as  start_date
, dateadd(day, -1, dateadd(month, 1, convert(date, cast(starts.YearMonth as char(6)) + '01',112))) as  end_date
, sum(starts.Total_All) as Referrals
, sum(stops.Total_All) as Removals
, sum(Waiters.Total_All) as Waiting_List_Size
from (
		Select Commissioner_Code, Org_Code, Org_Name, TFC, YearMonth, sum(Total_All) Total_All
		from #BBCS_RRT_PlanningData
		Where Metric = 'Waiters'
		group by Commissioner_Code, Org_Code, Org_Name, TFC, YearMonth
	) waiters inner join
	(
		Select Commissioner_Code, Org_Code, Org_Name, TFC, YearMonth, sum(Total_All) Total_All
		from #BBCS_RRT_PlanningData
		Where Metric = 'Clock Stops'
		group by Commissioner_Code, Org_Code, Org_Name, TFC, YearMonth
	) stops ON waiters.Commissioner_Code= stops.Commissioner_Code and waiters.Org_Code = stops.Org_Code and waiters.TFC = stops.TFC and waiters.YearMonth = stops.YearMonth
	inner join
	(
		Select Commissioner_Code, Org_Code, Org_Name, TFC, YearMonth,  sum(Total_All) Total_All
		from #BBCS_RRT_PlanningData
		Where Metric = 'Clock Starts'
		group by Commissioner_Code, Org_Code, Org_Name, TFC, YearMonth
	) starts ON waiters.Commissioner_Code = starts.Commissioner_Code and waiters.Org_Code = starts.Org_Code and waiters.TFC = starts.TFC and waiters.YearMonth = starts.YearMonth

--Where starts.TFC = '999'
group by starts.Commissioner_Code
, starts.Org_Code
, starts.Org_Name
, starts.TFC
, starts.YearMonth




--Select Sum(Total_All)
--from #BBCS_RRT_PlanningData
--Where TFC = '999'
--and YearMonth = 202606
--and RTTPartShortName = 'IU'
--and Commissioner_Code = '15E'
--and Org_c


--Select *
--from [AnalystGlobal].[Performance].[ReferralToTreatmentStandardised]


--WHERE		[YYYYMM] = 202606
--AND			[TreatmentFunctionCode] = '999' -- exclude totals
--AND			[CommissionerCode] IN ( SELECT Code FROM [Cluster_BBCS].[Filters].[Commissioner] )




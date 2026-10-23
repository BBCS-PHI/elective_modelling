
/*=================================================================================================
CREATE RTT Output Table
=================================================================================================*/
DROP TABLE IF EXISTS #BSol_RRT_PlanningData

CREATE TABLE #BSol_RRT_PlanningData

(
    [Row_Id] BIGINT IDENTITY(1,1) PRIMARY KEY
,
    [Metric] VARCHAR (20)
,
    [DataView] VARCHAR (20)
,
    [Org_Code] VARCHAR (5)
,
    [Org_Name] VARCHAR (255)
,
    [YearMonth] INT
,
    [TFC] VARCHAR (5)
,
    [TFC_Desc] VARCHAR (255)
,
    [RTTPartShortName] VARCHAR(10)

,
    [Total_All] DECIMAL (20,2)
,
    [00-18_Weeks] DECIMAL (20,2)
,
    [18-52_Weeks] DECIMAL (20,2)

,
    [18+_Weeks] DECIMAL (20,2)
,
    [52+_Weeks] DECIMAL (20,2)
,
    [Date_Last_Refreshed] DATETIME

)

--TRUNCATE TABLE ##BSol_RRT_PlanningData



/*=================================================================================================
INSERT BSol ICB RRT Data View
=================================================================================================*/

INSERT INTO		#BSol_RRT_PlanningData

    ( [DataView]
    , [Org_Code]
    , [Org_Name]
    , [YearMonth]
    , [TFC]
    , [TFC_Desc]
    , [RTTPartShortName]

    , [Total_All]
    , [00-18_Weeks]
    , [18-52_Weeks]

    , [18+_Weeks]
    , [52+_Weeks]
    , [Date_Last_Refreshed]

    )

(
SELECT 'BSol ICB'					[DataView]
, [CommissionerCode]			[Org_Code]
, [CommissionerName]			[Org_Name]
, [YYYYMM]					[YearMonth]
, [TreatmentFunctionCode]		[TFC]
, [TreatmentFunctionName]		[TFC_Desc]
, [RTTPartShortName]			[RTTPartShortName]

, SUM([TotalAll])				[Total_All]
, SUM([0to18Weeks])			[00-18_Weeks]
, SUM([18to52Weeks])			[18-52_Weeks]

, SUM([Over18Weeks])			[18+_Weeks]
, SUM([52WeeksPlus])			[52+_Weeks]
, GETDATE() 					[Date_Last_Refreshed]

FROM [AnalystGlobal].[Performance].[ReferralToTreatmentStandardised]


WHERE		[YYYYMM] BETWEEN 202404 AND 202602
    AND [TreatmentFunctionCode] = '999' -- exclude totals
    AND [CommissionerCode] IN ( SELECT Commissioner_Code
    FROM EAT_Reporting_BSOL.Reference.BSOL_COMMISSIONER_CODES )
-- BSol ICB

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

INSERT INTO		#BSol_RRT_PlanningData

    ( [DataView]
    , [Org_Code]
    , [Org_Name]
    , [YearMonth]
    , [TFC]
    , [TFC_Desc]
    , [RTTPartShortName]

    , [Total_All]
    , [00-18_Weeks]
    , [18-52_Weeks]

    , [18+_Weeks]
    , [52+_Weeks]
    , [Date_Last_Refreshed]

    )

(
SELECT 'BSol Providers'			[DataView]
, ProviderCode				[Org_Code]
, ProviderCode				[Org_Name]
, [YYYYMM]					[YearMonth]
, [TreatmentFunctionCode]		[TFC]
, [TreatmentFunctionName]		[TFC_Desc]
, [RTTPartShortName]			[RTTPartShortName]

, SUM([TotalAll])				[Total_All]
, SUM([0to18Weeks])			[00-18_Weeks]
, SUM([18to52Weeks])			[18-52_Weeks]

, SUM([Over18Weeks])			[18+_Weeks]
, SUM([52WeeksPlus])			[52+_Weeks]
, GETDATE() 					[Date_Last_Refreshed]

FROM [AnalystGlobal].[Performance].[ReferralToTreatmentStandardised]


WHERE		[YYYYMM] BETWEEN 202404 AND 202602
    AND [TreatmentFunctionCode] = '999' -- exclude totals
    AND ProviderCode IN ( SELECT Provider_Code
    FROM EAT_Reporting_BSOL.Reference.BSOL_NHS_PROVIDERS ) --BSol In-System Providers
    and [CommissionerCode] IN ( SELECT Commissioner_Code
    FROM EAT_Reporting_BSOL.Reference.BSOL_COMMISSIONER_CODES )
-- BSol ICB

GROUP BY
			ProviderCode
,			ProviderName
,			[YYYYMM]
,			[TreatmentFunctionCode]
,			[TreatmentFunctionName]
,			[RTTPartShortName]

)

/*=================================================================================================
INSERT BSol In-System Providers RRT Data View
=================================================================================================*/

INSERT INTO		#BSol_RRT_PlanningData

    ( [DataView]
    , [Org_Code]
    , [Org_Name]
    , [YearMonth]
    , [TFC]
    , [TFC_Desc]
    , [RTTPartShortName]

    , [Total_All]
    , [00-18_Weeks]
    , [18-52_Weeks]

    , [18+_Weeks]
    , [52+_Weeks]
    , [Date_Last_Refreshed]

    )

(
SELECT 'BSol Providers'			[DataView]
, 'other'						[Org_Code]
, 'other'						[Org_Name]
, [YYYYMM]					[YearMonth]
, [TreatmentFunctionCode]		[TFC]
, [TreatmentFunctionName]		[TFC_Desc]
, [RTTPartShortName]			[RTTPartShortName]

, SUM([TotalAll])				[Total_All]
, SUM([0to18Weeks])			[00-18_Weeks]
, SUM([18to52Weeks])			[18-52_Weeks]

, SUM([Over18Weeks])			[18+_Weeks]
, SUM([52WeeksPlus])			[52+_Weeks]
, GETDATE() 					[Date_Last_Refreshed]

FROM [AnalystGlobal].[Performance].[ReferralToTreatmentStandardised]


WHERE		[YYYYMM] BETWEEN 202404 AND 202602
    AND [TreatmentFunctionCode] = '999' -- exclude totals
    AND ProviderCode NOT IN ( SELECT Provider_Code
    FROM EAT_Reporting_BSOL.Reference.BSOL_NHS_PROVIDERS ) --BSol In-System Providers
    and [CommissionerCode] IN ( SELECT Commissioner_Code
    FROM EAT_Reporting_BSOL.Reference.BSOL_COMMISSIONER_CODES )
-- BSol ICB

GROUP BY

			[YYYYMM]
,			[TreatmentFunctionCode]
,			[TreatmentFunctionName]
,			[RTTPartShortName]

)


/*=================================================================================================
UPDATE Metric Descriptions
=================================================================================================*/

UPDATE		#BSol_RRT_PlanningData
SET			[Metric] ='Waiters'
WHERE		RTTPartShortName= 'IU'


UPDATE		#BSol_RRT_PlanningData
SET			[Metric] ='Clock Stops'
WHERE		RTTPartShortName IN ('AU','NU')


UPDATE		#BSol_RRT_PlanningData
SET			[Metric] ='Clock Starts'
WHERE		RTTPartShortName IN ('NP')





/*=================================================================================================
UFinal RRT Output Extract
=================================================================================================*/

SELECT [Metric]
, [DataView]
, [Org_Code]
, [Org_Name]
, [YearMonth]
, [TFC]
, [TFC_Desc]

, [Total_All]
, [00-18_Weeks]
, [18-52_Weeks]

, [18+_Weeks]
, [52+_Weeks]
, [Date_Last_Refreshed]

FROM #BSol_RRT_PlanningData
WHERE		Metric IS NOT NULL

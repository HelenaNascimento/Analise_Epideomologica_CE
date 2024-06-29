USE [PROD_2023]
GO

/****** Object:  Table [dbo].[DW_DATAS]    Script Date: 28/06/2024 10:05:58 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE TABLE [dbo].[R_DATAS](
	[DateKey] [int] NOT NULL,
	[Date] [datetime] NULL,
	[FullDateBR] [char](10) NULL,
	[FullDateISO] [char](10) NULL,
	[DayOfMonth] [varchar](2) NULL,
	[DaySuffix] [varchar](4) NULL,
	[DayName] [varchar](20) NULL,
	[DayOfWeekUSA] [char](1) NULL,
	[DayOfWeekUK] [char](1) NULL,
	[DayOfWeekInMonth] [varchar](2) NULL,
	[DayOfWeekInYear] [varchar](2) NULL,
	[DayOfQuarter] [varchar](3) NULL,
	[DayOfYear] [varchar](3) NULL,
	[WeekOfMonth] [varchar](1) NULL,
	[WeekOfQuarter] [varchar](2) NULL,
	[WeekOfYear] [varchar](2) NULL,
	[Mes] [varchar](2) NULL,
	[MonthName] [varchar](9) NULL,
	[MonthOfQuarter] [varchar](2) NULL,
	[Quarter] [char](1) NULL,
	[QuarterName] [varchar](9) NULL,
	[Ano] [char](4) NULL,
	[YearName] [char](7) NULL,
	[MonthYear] [char](10) NULL,
	[MMYYYY] [char](6) NULL,
	[FirstDayOfMonth] [date] NULL,
	[LastDayOfMonth] [date] NULL,
	[FirstDayOfQuarter] [date] NULL,
	[LastDayOfQuarter] [date] NULL,
	[FirstDayOfYear] [date] NULL,
	[LastDayOfYear] [date] NULL,
	[IsHolidayUSA] [bit] NULL,
	[IsWeekday] [bit] NULL,
	[HolidayUSA] [varchar](50) NULL,
	[IsHolidayUK] [bit] NULL,
	[HolidayUK] [varchar](50) NULL,
	[IsHolidayBR] [bit] NULL,
	[HolidayBR] [varchar](50) NULL,
 CONSTRAINT [PK__R_DATAS__40DF45E30B4C655B] PRIMARY KEY CLUSTERED 
(
	[DateKey] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO



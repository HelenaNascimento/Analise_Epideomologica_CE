USE [DW_PROD]
GO

/****** Object:  View [dbo].[V_HSPRC_FAKE_VIEW]    Script Date: 11/04/2024 17:20:02 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE view [dbo].[V_HSPRC_FAKE_VIEW] as
select * from DW_PROD.DBO.FAKE_HSPRC
GO



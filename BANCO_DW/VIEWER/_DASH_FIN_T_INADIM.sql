USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_FIN_T_INADIM]    Script Date: 27/06/2024 13:25:36 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

  CREATE VIEW [dbo].[DASH_FIN_T_INADIM] AS 
  
	SELECT 
		Distinct
		year(C_B.Dat_Vencimento) as ANO,
		sum(P_T.Parcela_T) as Parcela_T,
		SUM(P_T.Vlr_T) as Vlr_T,
		sum(P_A.Parcela_A) as Parcela_A,
		SUM(P_A.Vlr_A) as Vlr_A,
		sum(P_B.Parcela_B) as Parcela_B,
		SUM(P_B.Vlr_B) as Vlr_B,
		sum(P_C.Parcela_C) as Parcela_C,
		SUM(P_C.Vlr_C) as Vlr_C,
		sum(P_X.[Parcela_D_Em_Diante]) as [Parcela_D_Em_Diante],
		SUM(P_X.Vlr_D) as Vlr_X
	FROM [VW_FIN_BOL_A_] C_B
		left join  (SELECT 
						Dat_Vencimento as ANO,
						count(Par_Documento) as 'Parcela_T',
						sum([Vlr_Documento]) as 'Vlr_T'
					FROM [VW_FIN_BOL_A_]
					group by Dat_Vencimento) P_T on C_B.Dat_Vencimento = P_T.ANO
		left join  (SELECT 
						Dat_Vencimento as ANO,
						count(Par_Documento) as 'Parcela_A',
						sum([Vlr_Documento]) as 'Vlr_A'
					FROM [VW_FIN_BOL_A_]
					WHERE Par_Documento = 'A'
					group by Dat_Vencimento) P_A on C_B.Dat_Vencimento = P_A.ANO
		left join  (SELECT 
						Dat_Vencimento,
						count(Par_Documento) as 'Parcela_B',
						sum([Vlr_Documento]) as 'Vlr_B'
			FROM [VW_FIN_BOL_A_]
			WHERE Par_Documento = 'B'
			group by Dat_Vencimento) P_B on C_B.Dat_Vencimento = P_B.Dat_Vencimento
		left join  (SELECT 
						Dat_Vencimento,
						count(Par_Documento) as 'Parcela_C',
						sum([Vlr_Documento]) as 'Vlr_C'
			FROM [VW_FIN_BOL_A_]
			WHERE Par_Documento = 'C'
			group by Dat_Vencimento) P_C on C_B.Dat_Vencimento = P_C.Dat_Vencimento
		left join  (SELECT 
						Dat_Vencimento,
						count(Par_Documento) as 'Parcela_D_Em_Diante',
						sum([Vlr_Documento]) as 'Vlr_D'
			FROM [VW_FIN_BOL_A_]
			WHERE Par_Documento not in ('A', 'B', 'C')
			group by Dat_Vencimento) P_X on C_B.Dat_Vencimento = P_X.Dat_Vencimento
	group by 
	year(C_B.Dat_Vencimento)
GO



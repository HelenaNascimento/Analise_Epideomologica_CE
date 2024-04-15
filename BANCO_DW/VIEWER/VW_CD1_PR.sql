USE [DW_PROD]
GO

/****** Object:  View [dbo].[VW_CD1_PR]    Script Date: 15/04/2024 17:26:54 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO



CREATE VIEW [dbo].[VW_CD1_PR] AS 
	SELECT
		distinct
		[Cod_EAN]
		,pr.[CODIGO] as Cod_Prod
		,[Descricao]
		,[Dat_Cadastro]
		,fb.Fantasia
		,Ativo = 
			case 
				when [Flag_ImprClassif1] = 'N' then 'SIM'
				else 'NÃO'
			end
		,Tipo = 
			case 
				when [Tipo] = 'R' then 'REVENDA'
				when [Tipo] = 'U' then 'USO CONSUMO'
				else 'OUTROS'
			end
		   ,Controle_Preco=
			case 
				when [Ctrl_Preco] = 'L' then 'Liberado'
				when [Ctrl_Preco] = 'R' then 'Liberado c\ Repasse'
				when [Ctrl_Preco] = 'C' then 'Monitorado'
			end
		  ,Controle_Vendas = 
			case 
				when [Ctrl_Venda] = 'E' then 'Entorpecente'
				when [Ctrl_Venda] = 'P' then 'Psicotropico'
				when [Ctrl_Venda] = 'L' then 'Livre'
				when [Ctrl_Venda] = 'C' then 'Controlado'
				when [Ctrl_Venda] = 'M' then 'Antimecrobiano'
			end
		  ,Grp_Preco =
			case
				when [Cod_GrpPrc] = 'E' then 'Ético'
				when [Cod_GrpPrc] = 'X' then 'Outro'
				when [Cod_GrpPrc] = 'S' then 'Similar'
				when [Cod_GrpPrc] = 'P' then 'Perfumaria'
				when [Cod_GrpPrc] = 'G' then 'Genérico'
				else ''
			end
		  ,[Tip_Por344] as Portaria_344
		  ,Lista = 
			case 
				when [Tip_LisPis] = 'O' then 'Outras'
				when [Tip_LisPis] = 'N' then 'Negativa'
				when [Tip_LisPis] = 'X' then 'Neutra'
				when [Tip_LisPis] = 'P' then 'Positiva'
			end
		  ,format (IsNull([Prc_Fabric20], 0), 'c', 'pt-br') as [Prc_Fabric20]
		  ,format (IsNull([Prc_MaxCon20], 0), 'c', 'pt-br') as [Prc_MaxCon20]
		  ,format ([Prc_CusLiqEnt], 'c', 'pt-br') as [Prc_CusLiqEnt]
		  ,format ([Prc_CusLiqEntDep], 'c', 'pt-br') as [Prc_CusLiqEntDep]
		  ,format ([Prc_CusMed], 'c', 'pt-br') as [Prc_CusMed]
		  ,format ([Prc_CusMedCom], 'c', 'pt-br') as [Prc_CusMedCom]
		  ,format ([Prc_Venda], 'c', 'pt-br') as [Prc_Venda]
		  ,format ([Prc_Fabric], 'c', 'pt-br') as [Prc_Fabric]
		  ,format ([Prc_UltEnt], 'c', 'pt-br') as [Prc_UltEnt]
		  ,[Qtd_UltEnt]
		  ,[Qtd_Fisico]
		  ,[Qtd_Avaria]
		  ,[Qtd_Quaren]
		  ,[Qtd_Reserv]
		  ,[Qtd_Solici]
		  ,[Qtd_Transi]
		  ,[Qtd_Dispon]

	FROM [DW_PROD].[dbo].[VW_PRODUTOS] pr
			INNER JOIN  [DW_PROD].[dbo].VW_FABR fb on pr.Cod_Fabricante = fb.Codigo
GO



USE [BD_DW]
GO

/****** Object:  View [dbo].[VW_PRODUTOS]    Script Date: 13/04/2024 16:47:26 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[VW_PRODUTOS] AS
SELECT 
	PR.CODIGO,
	PR.Descricao,
	PR.Cod_EAN,
	PR.Dat_Cadastro,
	PR.Cod_Fabricante,
	PR.Flag_ImprClassif1,
	Ctrl_Preco,
	Ctrl_Venda,
	Cod_GrpPrc,
	Tip_Por344,
	Tip_LisPis,
	Cod_ClaFis,
	PR.Prc_Fabric20,
	PR.Prc_MaxCon20,
	Tipo,
	ES.Prc_CusLiqEnt,
	ES.Prc_CusLiqEntDep,
	ES.Prc_CusMed,
	ES.Prc_CusMedCom,
	ES.Prc_Venda,
	ES.Prc_Fabric,
	ES.Prc_UltEnt,
	ES.Qtd_UltEnt,
	ES.Qtd_Fisico,
	ES.Qtd_Avaria,
	es.Qtd_Quaren,
	ES.Qtd_Reserv,
	ES.Qtd_Solici,
	Es.Qtd_Transi,
	Es.Qtd_Dispon
	FROM PROD_2023.dbo.PRODU PR
	INNER JOIN PROD_2023.dbo.PRXES ES ON PR.Codigo = ES.Cod_Produt
WHERE 
	ES.Cod_Estabe = 1
GO



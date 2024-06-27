USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_VPROD_POL]    Script Date: 27/06/2024 13:32:54 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO




CREATE VIEW [dbo].[DASH_VPROD_POL] AS 


SELECT 
pr.Cod_Fabricante, 
fb.Fantasia,
pr.cod_ean,
pr.codigo,
pr.descricao,
ForadeLinha = 
	case 
		when pr.Flag_ImprClassif1 <> 'N' then 'Não'
		when pr.Flag_ImprClassif1 = 'N' then 'Sim'
	end,
pc.Cod_PolCom,
QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
'C/RES' =  Sum(it.Vlr_LiqItem-it.Vlr_RecSbt),
'S/RES' = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0)),
month(cb.dat_emissao) Mes,
year(cb.dat_emissao) Ano
FROM PROD_2023.dbo.NFSCB cb 
	INNER JOIN PROD_2023.dbo.NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
                                                            (cb.Ser_Nota = it.Ser_Nota) AND 
                                                            (cb.Num_Nota = it.Num_Nota)) 
   INNER JOIN PROD_2023.dbo.PRODU pr on it.Cod_Produto = pr.Codigo
   left join PROD_2023.dbo.POCOM PC on it.Id_PolCom = pc.Id_PolCom
   left join PROD_2023.dbo.FABRI FB on pr.Cod_Fabricante = fb.codigo
WHERE cb.Cod_Estabe = 1
AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
AND cb.Dat_Emissao >= '20230101' 
Group by 
pr.Cod_Fabricante, 
fb.Fantasia,
pr.cod_ean,
pr.codigo,
pr.descricao,
pr.Flag_ImprClassif1,
pc.Cod_PolCom,
cb.dat_emissao



GO



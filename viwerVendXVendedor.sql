CREATE VIEW vwVENDASxVENDEDOR AS

SELECT 
	distinct
	SCB.Cod_Vendedor,
	vd.Nome_Guerra,
	COUNT(SCB.NUM_NOTA) 'Qtd_nota_Saídas',
	SUM(Qtd_Pra) AS 'Qtd_Produto_Saída',
	format (SUM(SIT.Vlr_BruItem), 'c', 'pt-br') AS '( Vlr_BruItem',
	format (SUM(SIT.Vlr_DescItem), 'c', 'pt-br') AS 'Vlr_DescItem )',
	format (SUM(SIT.Vlr_TotItem), 'c', 'pt-br') AS 'Saída por Vendas'
	FROM NFSCB SCB
		INNER JOIN NFSIT SIT ON SCB.Cod_Estabe = SIT.Cod_Estabe AND SCB.Ser_Nota = SIT.Ser_Nota AND SCB.Num_Nota = SIT.Num_Nota
		INNER JOIN VENDE VD on SCB.Cod_Vendedor = VD.Codigo

where SCB.Cod_Estabe = 1 
	and SCB.Dat_Emissao > '20230731'
	and SCB.Dat_Emissao < '20230901'
	and SCB.Ser_Nota = '1'
	and SCB.Ret_CStat = 100
	and SCB.Status = 'F'
	and SCB.Tip_Saida = 'V'
GROUP BY SCB.Cod_Vendedor, vd.Nome_Guerra
/*
SELECT * FROM vwVENDASxVENDEDOR
Select
	Vlr_LiqItens,
    Vlr_DescontoCom ,
    Vlr_DscBon ,
    Vlr_RepIcms,
    Vlr_SubsTrib ,
    Vlr_SbtRes ,
    Vlr_RecSbt,
    Vlr_DscTri,
    Vlr_Ipi,
    Vlr_Frete,
    Vlr_Seguro,
    Vlr_OutDsp,
    Vlr_DspExt,
    Vlr_TotalNota
from NFSCB
Where Cod_Estabe = 1
And Ser_Nota = '1'
And Num_Nota = 423888 

select 
	Qtd_Pra,
	format (Prc_UniImpFat, 'c', 'pt-br'),
	format (Vlr_BruItem,'c', 'pt-br'),
	format (Vlr_DescItem, 'c', 'pt-br'),
	format (vlr_TotItem, 'c', 'pt-br')
	FROM NFSIT
Where Cod_Estabe = 1
And Ser_Nota = '1'
And Num_Nota = 423888 


select 
	cb.NUM_NOTA,
	SUM(Qtd_Pra) AS Qtd_Pra,
	format (SUM(Prc_UniImpFat), 'c', 'pt-br') AS Prc_UniImpFat,
	format (SUM(Vlr_BruItem), 'c', 'pt-br') AS Vlr_BruItem,
	format (SUM(Vlr_DescItem), 'c', 'pt-br') AS Vlr_DescItem,
	format (SUM(Vlr_TotItem), 'c', 'pt-br') AS Vlr_TotItem
	FROM NFSIT IT
		INNER JOIN NFSCB cb on 
				IT.Cod_Estabe = CB.Cod_Estabe 
			AND IT.Ser_Nota = CB.Ser_Nota 
			AND IT.Num_Nota = CB.Num_Nota
Where IT.Cod_Estabe = 1
AND CB.Dat_Emissao > '20230731'
And IT.Ser_Nota = '1'
And CB.Cod_Vendedor = 464
--And IT.Num_Nota = 423888
GROUP BY CB.NUM_NOTA


select * from NFSCB
where Cod_Vendedor = 464 
	--and CB.Num_Nota = 424205
	and Dat_Emissao > '20230731'
	and Ser_Nota = '1'
	and Num_Nota in (423888)
	and Ret_CStat = 100
	and Status = 'F'
	and Tip_Saida = 'V'


select * from NFSIT
where Cod_Estabe = 1 and Ser_Nota = '1' and Num_Nota in (423888)

--, 424325)


Qtd_Produto,
Prc_Unitario,
Per_Desconto,
Vlr_DescItem,
Vlr_CusIte,
Alq_ICMS,
Vlr_IcmsNor,
Vlr_PrdSubTri,
Vlr_TotItem,
Vlr_LiqItem,
Vlr_BruItem,
Vlr_Comissao,
Vlr_ComTlmkt,
Vlr_DespRateada,
Vlr_DescRateado,


SELECT 
	distinct
	Cod_Vendedor,
	COUNT(CB.NUM_NOTA) as Qtd_Nota,
	format (sum(VlrBruItens), 'c', 'pt-br') as 'Vlr_Bruto_Itens',
	format (sum(Vlr_DespRateada), 'c', 'pt-br') as '(+)Despesas',
	format (sum(cb.Vlr_IcmsTri), 'c', 'pt-br') as Vlr_IcmsTri,
	format (sum(cb.Vlr_SbtRes), 'c', 'pt-br') as '(+)Vlr_SbtRes',
	format (sum(cb.Vlr_RepIcms), 'c', 'pt-br') as '(-)Vlr_RepIcms',
	format (sum(IT.Vlr_DescItem), 'c', 'pt-br') as '(-)Vlr_DesconItem',
	format (sum(Vlr_TotalNota), 'c', 'pt-br') as '(=)NF_Vlr_Total'
	FROM NFSCB CB
		inner join NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota
where CB.cod_estabe = 1
	and Dat_Emissao > '20230731'
	and CB.Ser_Nota = '1'
	and Ret_CStat = 100
	and Status = 'F'
	and Tip_Saida = 'V'
group by Cod_Vendedor


*/
select grouping(Cod_Estabe), Cod_Estabe, sum(Vlr_Total) from PDECB
where Dat_LeiPed >='20241031'
group by rollup(Cod_Estabe)


select grouping(Cod_Estabe), Cod_Estabe, sum(Vlr_Total) from PDECB
where Dat_LeiPed >='20241031'
group by cube(Cod_Estabe)

SELECT 
	DISTINCT
	grouping(CS.Descricao),
	CS.Descricao,
	SUM(IT.Vlr_LiqItem)
FROM PRODU PR
	JOIN CLASS CS ON PR.Cod_Classif = CS.Codigo
	JOIN NFSIT IT ON PR.Codigo = IT.Cod_Produto
	JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND  IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
WHERE 
	IT.Cod_Estabe = 1
AND CB.Dat_Emissao >= '20241001'
AND CB.Dat_Emissao <= '20241031'
AND CB.Ret_CStat = 100
AND cb.Tip_Saida = 'V'
GROUP BY ROLLUP (CS.Descricao)


SELECT 
--GROUPING(Cod_OrigemPdv),
Cod_OrigemPdv,
SUM(C_VlrPedido) VLR_PEDI
FROM PDVCB  CB
	JOIN VENDE VE on CB.Cod_Vendedor = VE.codigo
	JOIN NFSCB NF ON CB.Cod_Estabe = NF.Cod_Estabe AND CB.Numero = NF.Cod_Pedido
WHERE CB.Cod_Estabe = 1
AND Dat_Pedido >= '20241001'
AND Dat_Pedido <= '20241031'
AND NF.Ret_CStat = 100
AND VE.Codigo NOT IN (464, 472)
AND CB.ID_POLCOM = 3015
GROUP BY rollup (Cod_OrigemPdv)
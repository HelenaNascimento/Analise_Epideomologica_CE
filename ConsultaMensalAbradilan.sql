--Inadiplência por período

SELECT 
  sum(IsNull(((ct.Vlr_Documento - ct.Vlr_DescConced) + Isnull(bx.Vlr_Juros, 0)), 0))
          
from CTREC ct
	left outer join CLIEN cl on ct.Cod_Cliente = cl.Codigo
	left outer join (select 
						Cod_Estabe, 
						Cod_Documento,
						Vlr_Juros,
						Qtd_DiasAtraso
					from bxrec where cod_Estabe = 1) bx on ct.cod_estabe = bx.Cod_Estabe and ct.cod_Documento = bx.cod_Documento
where ct.cod_estabe = 1
    and ct.Dat_Vencimento >='20241101'
	and ct.Dat_Vencimento <='20241130'
	and  IsNull(bx.Qtd_DiasAtraso, 0) > 0 
    and ct.[Status] <> 'C'


-- Qtd (UND) Faturado

SELECT 
	Sum(it.Qtd_Produto+it.Qtd_Bonificacao)
	FROM NFSIT IT
		JOIN NFSCB CB ON IT.COD_ESTABE = CB.COD_ESTABE AND IT.SER_NOTA = CB.SER_NOTA AND IT.NUM_NOTA = CB.NUM_NOTA
WHERE cb.Cod_Estabe = 1
    AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
    AND cb.Dat_Emissao >= '20241101'
    AND cb.Dat_Emissao <= '20241130'


--Consulta Origem de Pedido

SELECT 
Cod_OrigemPdv = 
	CASE  
		when Cod_OrigemPdv = 'AL' then 'Ativo'
		when Cod_OrigemPdv = 'ML' then 'Móvel'
		when Cod_OrigemPdv = 'TL' then 'OL'
		ELSE 'TOTAL'
	END,

format(SUM(C_VlrPedido), 'c', 'pt-br') as VLR_PEDI
FROM PDVCB  CB
	JOIN VENDE VE on CB.Cod_Vendedor = VE.codigo
	JOIN NFSCB NF ON CB.Cod_Estabe = NF.Cod_Estabe AND CB.Numero = NF.Cod_Pedido
	JOIN NFSIT IT ON NF.Cod_Estabe = IT.Cod_Estabe AND NF.Ser_Nota = IT.Ser_Nota AND NF.Num_Nota = IT.Num_Nota
WHERE CB.Cod_Estabe = 1
    AND Dat_Pedido >= '20241101'
    AND Dat_Pedido <= '20241130'
    AND (NF.Status = 'F' and NF.Tip_Saida = 'V') 
    AND NF.Ret_CStat = 100
    AND VE.Codigo NOT IN (464, 472)
GROUP BY rollup (Cod_OrigemPdv)

--Por categoria

SELECT 
	CL.Descricao,
	FORMAT(SUM(IT.Vlr_LiqItem), 'c', 'pt-br') AS Vlr_LiqItem
FROM  NFSCB CB 
	JOIN NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe 
		AND CB.Ser_Nota = IT.Ser_Nota 
		AND CB.Num_Nota = IT.Num_Nota
	JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
	JOIN CLASS CL ON PR.Cod_Classif = CL.Codigo
WHERE CB.Cod_Estabe = 1
    AND CB.Dat_Emissao >= '20241101'
    AND CB.Dat_Emissao <= '20241130'
    AND (CB.Status = 'F' and CB.Tip_Saida = 'V') 
    AND CB.Ret_CStat = 100
GROUP BY rollup (CL.Descricao)
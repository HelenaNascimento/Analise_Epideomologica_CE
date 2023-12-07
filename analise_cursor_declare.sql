
declare
@codEstab int = 1,
@DatIn smalldatetime = '20230101',
@DatFim smalldatetime= '20231130',
@CFABR int = 33

SELECT
PRD.Cod_Fabricante,
FB.Fantasia,
PRD.CODIGO,
PRD.Cod_EAN,
PRD.Descri,
Prc_UltEnt,
eit2.Qtd_PedFat as Qtd_Ent_Comp,
--eit0.Prc_UniFat as VlrUltEnt,
eit1.Qtd_PedFat as Qtd_Ent_BONI,
Prc_Fabric,
Prc_Venda,
Prc_CusMedCom
	FROM PRODU PRD
		INNER JOIN PRXES PES ON PRD.Codigo = PES.Cod_Produt
		INNER JOIN FABRI FB ON PRD.Cod_Fabricante = FB.Codigo
		/*LEFT  JOIN (SELECT top 1 Prc_UniFat, IT.Cod_Estabe, IT.Protocolo, it.Cod_Produto, Dat_Emissao
						from NFEIT IT
							inner join NFECB cb0 on it.cod_estabe = cb0.Cod_Estabe and it.Protocolo = cb0.Protocolo
										where 
											it.Cod_Estabe = @codEstab
											and Tip_NF <> 'D' 
											and cb0.Dat_Entrada >= @DatIn
											and cb0.Dat_Entrada <= @DatFim
											--and it.Cod_Produto = @CodProd
											and status not in ('A', 'C') 
										order by Dat_Emissao desc) eit0 on PES.Cod_Estabe = eit0.Cod_Estabe and PES.Cod_Produt = eit0.Cod_Produto*/
		LEFT  JOIN (SELECT 
						Cod_Produto, 
						sum(Qtd_PedFat) as Qtd_PedFat, 
						IT.Cod_Estabe  
						FROM NFEIT IT
							inner join NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
					WHERE IT.Cod_Estabe = @codEstab
					and cb.Dat_Entrada >= @DatIn
					and cb.Dat_Entrada <= @DatFim
					--and it.Cod_Produto = @CodProd
					and IT.Cod_Cfo in (1910, 2910) 
					group by Cod_Produto, IT.Cod_Estabe  ) eit1 on PES.Cod_Estabe = eit1.Cod_Estabe and PES.Cod_Produt = eit1.Cod_Produto
					
		FULL OUTER JOIN (SELECT 
						Cod_Produto, 
						sum(Qtd_PedFat) as Qtd_PedFat, 
						IT.Cod_Estabe  
						FROM NFEIT IT
							inner join NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
					WHERE IT.Cod_Estabe = @codEstab
					and cb.Dat_Entrada >= @DatIn
					and cb.Dat_Entrada <= @DatFim
					--and it.Cod_Produto = @CodProd
					and IT.Cod_Cfo in (1404, 2404)
					group by Cod_Produto, IT.Cod_Estabe) eit2 on PES.Cod_Estabe = eit2.Cod_Estabe and PES.Cod_Produt = eit2.Cod_Produto
where 
pes.Cod_Estabe = @codEstab
and PRD.Cod_Fabricante = @CFABR
--and PRD.CODIGO = @CodProd
order by 3

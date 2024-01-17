DECLARE 
	@codEstab int = 1,
	@DatIn smalldatetime = '20230101',
	@DatFim smalldatetime= '20231231'



SELECT 
	DISTINCT
	PR.CODIGO,
	Prc_Fabric,
	Prc_UltEnt,
	--IsNull(FAT.Qtd_PedFat, 0) as Qtd_PedFat
	--IsNull(BON.Qtd_BON, 0) as Qtd_BON
FROM PRODU PR
	INNER JOIN PRXES ES ON PR.CODIGO = ES.COD_PRODUT
	/*LEFT JOIN (SELECT 
				distinct
				CB.Cod_Estabe,
				IT.Cod_Produto,
				sum(Qtd_PedFat) as Qtd_PedFat			
					FROM NFEIT IT
						inner join NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
				WHERE
					IT.Cod_Estabe = @codEstab
				and cb.Dat_Entrada >= @DatIn
				and cb.Dat_Entrada <= @DatFim
				and IT.Cod_Cfo in (2102, 2403, 2404)
				Group by
					CB.Cod_Estabe,
					IT.Cod_Produto) FAT ON ES.Cod_Estabe = FAT.Cod_Estabe AND PR.CODIGO = FAT.Cod_Produto*/
	LEFT JOIN (SELECT 
			distinct
			CB.Cod_Estabe,
			IT.Cod_Produto,
			sum(Qtd_PedFat) as Qtd_BON			
				FROM NFEIT IT
					inner join NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Protocolo = CB.Protocolo
			WHERE
				IT.Cod_Estabe = @codEstab
			and cb.Dat_Entrada >= @DatIn
			and cb.Dat_Entrada <= @DatFim
			and IT.Cod_Cfo in (1910, 2910)
			Group by
					CB.Cod_Estabe,
					IT.Cod_Produto) BON ON ES.Cod_Estabe = BON.Cod_Estabe AND PR.CODIGO = BON.Cod_Produto
WHERE ES.Cod_Estabe = 1 
AND COD_FABRICANTE = 158
order by 1
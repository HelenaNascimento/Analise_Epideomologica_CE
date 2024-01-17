select 
	DISTINCT
    Fabricante = FB.Fantasia,
	PR.CODIGO,
    Qtd_Produto = count(IT.Qtd)
    from
        PRODU PR 
			INNER JOIN (       SELECT 
                                Cod_Produto, 
                                SUM(Qtd_ImpFat) AS Qtd, 
                                Cod_Estabe, 
                                ser_nota, 
                                num_nota 
                            FROM NFSIT 
                            WHERE 
                                cod_estabe = 1 
                                AND cod_cfo IN (5102, 5405)
                            GROUP BY 
                                Cod_Produto,
                                Cod_Estabe, 
                                ser_nota, 
                                num_nota 
                                ) IT ON PR.CODIGO = IT.COD_PRODUTO
            INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
            Left join FABRI FB ON PR.Cod_Fabricante = FB.Codigo
    where it.cod_estabe = 1
    and year(dat_emissao)= '2024'                           
    and Status = 'F'
    and Ret_CStat = 100

    group by FB.Fantasia, PR.CODIGO, dat_emissao
    order by 3 desc;

SELECT 
	DISTINCT
	FB.Fantasia,
	SUM(IT.Qtd_Produto) AS Qtd_Produto,
	CONVERT(DECIMAL(20,2) ,SUM(IT.Vlr_LiqItem)) AS Vlr_LiqItem,
	Mes=
	CASE
		WHEN MONTH(cb.Dat_Emissao) = '01' THEN 'Jan'
		WHEN MONTH(cb.Dat_Emissao) = '02' THEN 'Fev'
		WHEN MONTH(cb.Dat_Emissao) = '03' THEN 'Mar'
		WHEN MONTH(cb.Dat_Emissao) = '04' THEN 'Abr'
		WHEN MONTH(cb.Dat_Emissao) = '05' THEN 'Mai'
		WHEN MONTH(cb.Dat_Emissao) = '06' THEN 'Jun'
		WHEN MONTH(cb.Dat_Emissao) = '07' THEN 'Jul'
		WHEN MONTH(cb.Dat_Emissao) = '08' THEN 'Ago'
		WHEN MONTH(cb.Dat_Emissao) = '09' THEN 'Set'
		WHEN MONTH(cb.Dat_Emissao) = '10' THEN 'Out'
		WHEN MONTH(cb.Dat_Emissao) = '11' THEN 'Nov'
		WHEN MONTH(cb.Dat_Emissao) = '12' THEN 'Dez'
	END,
	ANO = 
		CASE 
			WHEN YEAR(cb.Dat_Emissao) = '2023' THEN '2023'
			WHEN YEAR(cb.Dat_Emissao) = '2024' THEN '2024'
		END
	FROM NFSIT IT
		INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
		LEFT JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
		LEFT JOIN FABRI FB ON PR.Cod_Fabricante = FB.Codigo
WHERE 
	IT.Cod_Estabe = 1
AND IT.Cod_Cfo = 5949
AND CB.Ser_Nota = '1'
AND CB.Cod_Cliente = 12976
AND CB.Dat_Emissao >= '20230101'
GROUP BY Dat_Emissao, FB.Fantasia
ORDER BY 3 DESC

SELECT 
	DISTINCT
	FB.Fantasia,
	SUM(IT.Qtd_Produto) AS Qtd_Produto,
	CONVERT(DECIMAL(20,2) ,SUM(IT.Vlr_LiqItem)) AS Vlr_LiqItem,
	CASE
		WHEN MONTH(cb.Dat_Emissao) = '01' THEN 'Jan'
	END
	FROM NFSIT IT
		INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
		LEFT JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
		LEFT JOIN FABRI FB ON PR.Cod_Fabricante = FB.Codigo
WHERE 
	IT.Cod_Estabe = 1
AND IT.Cod_Cfo = 5949
AND CB.Ser_Nota = '1'
AND CB.Cod_Cliente = 12976
AND CB.Dat_Emissao >= '20240101'
AND CB.Dat_Emissao <= '20241231'
GROUP BY FB.Fantasia
ORDER BY 3 DESC
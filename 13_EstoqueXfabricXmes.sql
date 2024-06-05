declare 
	@ANO varchar(4) = '2024',
	@MES varchar(2) = '05',
	@FB int = 1022,
	@CEst int = 1,
	@OP int = 1
if @OP = 1 (
			SELECT
				PR.CODIGO,
				SUM(IT.Qtd_PedFat) AS QTD_Compra
				FROM NFEIT IT
					INNER JOIN NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
					INNER JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
			WHERE 
			IT.Cod_Estabe = @CEst
			AND Cod_Fabricante = @FB
			AND year(CB.DAT_ENTRADA) = @ANO
			AND month(CB.DAT_ENTRADA) = @MES
			AND IT.Cod_Cfo in (2102, 2403, 2404) 

			GROUP BY
				PR.CODIGO);
if @OP = 2 (
			SELECT 
				PR.CODIGO,
				SUM(IT.Qtd_PedFat) AS QTD_BONI
				FROM NFEIT IT
					INNER JOIN NFECB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.PROTOCOLO = CB.PROTOCOLO 
					INNER JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
			WHERE 
						IT.Cod_Estabe = @CEst
						AND Cod_Fabricante = @FB
						AND year(CB.DAT_ENTRADA) = @ANO
						AND month(CB.DAT_ENTRADA) = @MES
			AND IT.Cod_Cfo in (1910, 2910)

			GROUP BY
				PR.CODIGO);
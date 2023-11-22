DECLARE 
	@Prod int,
	@CodEan varchar(13), 
	@Codig int, 
	@Descr varchar(50), 
	@QtdPed int, 
	@PrcAtu decimal(20,2), 
	@VlrCom decimal(20,2), 
	@VlrMed decimal(20,2)
Declare CursorProd CURSOR FOR 
SELECT DISTINCT Codigo 
FROM PRODU
WHERE Cod_Fabricante = 158

OPEN CursorProd

FETCH NEXT FROM CursorProd INTO @Prod
WHILE @@FETCH_STATUS = 0
BEGIN
	DECLARE ENT_EURO cursor for
	select 
	cod_ean,
	codigo,
	descricao,
	SUM(it.Qtd_Pedido) AS Qtd_Vendas,
	convert(decimal (20,2),(ES.prc_venda)) as Prc_Venda_Atual,
	convert(decimal (20,2), (SUM(prc_unitario) * SUM(it.Qtd_Pedido))) as Valor_Total_Compra,
	convert(decimal (20,2),((SUM(it.prc_unitario) / count(cb.protocolo)))) AS Vlr_Médio_Compra
	from PRODU PR
		Inner join PRXES ES ON PR.CODIGO = ES.COD_PRODUT
		inner join NFEIT IT ON PR.CODIGO = IT.Cod_Produto AND ES.COD_ESTABE = IT.COD_ESTABE 
		inner join NFECB CB ON IT.COD_ESTABE = CB.COD_ESTABE  AND IT.PROTOCOLO = CB.PROTOCOLO
	 WHERE ES.Cod_Estabe = 1
	AND CB.SERIE = '1'
	--AND CB.Tip_NF = 'C'
	AND pr.Cod_Fabricante = 158
	AND PR.Codigo = @Prod
	GROUP BY
		cod_ean,
		codigo,
		descricao,
		ES.prc_venda
	order by 2

	OPEN ENT_EURO;
	FETCH NEXT FROM ENT_EURO INTO @CodEan, @Codig, @Descr, @QtdPed, @PrcAtu, @VlrCom, @VlrMed;
    WHILE @@FETCH_STATUS = 0
    BEGIN
     INSERT INTO EURO_ENT (COD_EAN, COD_PROD, DESCRICAO, QTD_PEDIDO, PRC_ATU, VLR_COMP, VLR_MED, Cust_Med)
	   select 
	   	CAST(@CodEan AS varchar(13)), 
		CAST(@Codig AS INT), 
		CAST(@Descr AS VARCHAR(255)),
		CAST(@QtdPed AS int),
		CAST(@PrcAtu AS decimal(20,2)),
		 CAST(@VlrCom AS decimal(20,2)),
		 CAST(@VlrMed AS decimal(20,2)),
		 Custo = case 
						when pr.Prc_CusMed > 0 then convert (decimal(20,2), pr.Prc_CusMed) 
						when pr.Prc_CusMed = 0 then convert (decimal(20,2), Prc_CusLiqEnt)
				end
		 from PRXES pr
		  where
			Cod_Estabe = 1 and
			Cod_Produt = @Codig
	FETCH NEXT FROM ENT_EURO INTO @CodEan, @Codig, @Descr, @QtdPed, @PrcAtu, @VlrCom, @VlrMed;

	END;
	CLOSE ENT_EURO
	DEALLOCATE ENT_EURO

FETCH NEXT FROM CursorProd INTO @Prod
END

CLOSE CursorProd
DEALLOCATE CursorProd
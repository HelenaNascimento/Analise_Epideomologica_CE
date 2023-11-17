DECLARE 
	@Prod int,
	@CodEan NVARCHAR(15), 
	@Codig int, 
	@Descr varchar(50), 
	@PrUni  varchar(12),
	@QtdPed int, 
	@PrcAtu varchar(12), 
	@VlrCom varchar(12), 
	@VlrMed varchar(12)
Declare CursorProd CURSOR FOR 
SELECT DISTINCT Codigo 
FROM PRODU
WHERE Cod_Fabricante = 158

OPEN CursorProd

FETCH NEXT FROM CursorProd INTO @Prod
WHILE @@FETCH_STATUS = 0
BEGIN
	DECLARE ENT_EURO cursor for
	select top 1
	cod_ean,
	codigo,
	descricao,
	format(prc_unitario, 'c', 'pt-br') as prc_unitario,
	SUM(it.Qtd_Pedido) AS Qtd_Pedido,
	format((ES.prc_venda), 'c', 'pt-br') as Prc_Venda_Atual,
	Format((SUM(prc_unitario) * SUM(it.Qtd_Pedido)), 'c', 'pt-br') as Valor_Total_Compra,
	format(((SUM(it.prc_unitario) / count(cb.protocolo))), 'c', 'pt-br') AS Vlr_Médio_Compra
	from PRODU PR
		Inner join PRXES ES ON PR.CODIGO = ES.COD_PRODUT
		inner join NFEIT IT ON PR.CODIGO = IT.Cod_Produto AND ES.COD_ESTABE = IT.COD_ESTABE 
		inner join NFECB CB ON IT.COD_ESTABE = CB.COD_ESTABE  AND IT.PROTOCOLO = CB.PROTOCOLO
	 WHERE ES.Cod_Estabe = 1
	AND CB.SERIE = '1'
	AND pr.Cod_Fabricante = 158
	AND PR.Codigo = @Prod
	AND DAT_ENTRADA >= '20210701'
	AND DAT_ENTRADA <= '20230930'
	GROUP BY
		cod_ean,
		codigo,
		descricao,
		prc_unitario,
		ES.prc_venda
	order by 2 desc 

	OPEN ENT_EURO;
	FETCH NEXT FROM ENT_EURO INTO @CodEan, @Codig, @Descr, @PrUni, @QtdPed, @PrcAtu, @VlrCom, @VlrMed;
    WHILE @@FETCH_STATUS = 0
    BEGIN

	PRINT CAST(@PrUni AS NVARCHAR(255));
	FETCH NEXT FROM ENT_EURO INTO @CodEan, @Codig, @Descr, @PrUni, @QtdPed, @PrcAtu, @VlrCom, @VlrMed;

	END;
	CLOSE ENT_EURO
	DEALLOCATE ENT_EURO

FETCH NEXT FROM CursorProd INTO @Prod
END

CLOSE CursorProd
DEALLOCATE CursorProd
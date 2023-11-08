DECLARE 
	@cest int = 1,
	@nest varchar(10),
	@cfabr int = 123, 
	@nfabr varchar(20), 
	@cforn int, 
	@nforn varchar(20),
	@trepos int,
	@dsupr int,
	@desc decimal(10,2),
	@praz int

Declare  Cabecalho CURSOR for

SELECT 
	top 1
	FS.COD_ESTABE,
	ES.DES_ESTABE,
	FF.COD_FABRIC,
	FB.Fantasia,
	COD_FORNEC,
	FR.Fantasia,
	Tempo_Reposicao,
	Dias_Suprimento,
	Desconto_Comercial,
	Prazo_Medio
	FROM FORNE FR
		INNER JOIN FRXFB FF ON FR.CODIGO = FF.COD_FORNEC
		INNER JOIN FABRI FB ON FF.COD_FABRIC = FB.CODIGO
		INNER JOIN FBXES FS ON FB.CODIGO = FS.COD_FABRIC
		INNER JOIN ESTAB ES ON FS.COD_ESTABE = ES.Cod_Estabe
WHERE FS.COD_ESTABE = @cest  
		AND FF.COD_FABRIC = @cfabr 

OPEN Cabecalho;

FETCH NEXT FROM Cabecalho 
	INTO 
		@cest, 
		@nest, 
		@cfabr, 
		@nfabr, 
		@cforn, 
		@nforn, 
		@trepos, 
		@dsupr, 
		@desc, 
		@praz;

WHILE @@FETCH_STATUS = 0

BEGIN 
	PRINT 'MAPA DE VENDAS - ' + @nfabr
	PRINT 'Estabs: ' + concat(@cest, '-', @nest) 
	PRINT 'Fabricante: ' + concat( @cfabr ,'-', @nfabr) + ' /Fornecedor: '+ concat ( @cforn,'-', @nforn) + '* Reposicao: ' + CAST(@trepos AS VARCHAR(4)) + 'dd * Suprimento: ' +  CAST(@dsupr AS VARCHAR(4)) + 'dd * Desc.Comercial: ' + CAST(@desc AS VARCHAR(14)) +'% * Prazo: ' + CAST(@praz AS VARCHAR(4)) + 'dd * Curva ABC por Fabric'
	PRINT 'Emissao: ' + format(getdate(),'d', 'en-gb')

	FETCH NEXT FROM Cabecalho 
	INTO 
		@cest, 
		@nest, 
		@cfabr, 
		@nfabr, 
		@cforn, 
		@nforn, 
		@trepos, 
		@dsupr, 
		@desc, 
		@praz;
END
Close Cabecalho;
Deallocate Cabecalho;

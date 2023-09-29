SELECT 
	codigo AS 'Código',
	Nome_Guerra AS 'Nome_Guerra',
	Nome_Completo AS 'Nome_Completo',
	email as EMAIL,
	TML = 'TELEVENDAS'
	FROM VENDE VD
		INNER JOIN VEXES ES ON VD.codigo = ES.COD_VENDED
where es.cod_estabe = 1
	and vd.Cod_TipVenBas = 'TLM'
	and bloqueado = 0
	and nome_guerra like 'CE-%'
	and codigo in (456, 551, 559, 562, 581, 587, 591, 637, 649, 650)


SELECT 
	DISTINCT
	Codigo,
	Descricao,
	Cod_EAN,
	EIT.Vlr_BruItem,
	max(ECB.Dat_Entrada),
	sum(SIT.Qtd_Produto),
	sum(SIT.Vlr_TotItem)
FROM PRODU PRO
		INNER JOIN PRXES XES ON PRO.Codigo = XES.Cod_Produt
		INNER JOIN NFEIT EIT ON PRO.Codigo = EIT.Cod_Produto AND XES.Cod_Estabe = EIT.Cod_Estabe
		INNER JOIN NFECB ECB ON EIT.Cod_Estabe = ECB.Cod_Estabe and EIT.Protocolo = ECB.Protocolo
		INNER JOIN NFSIT SIT ON XES.Cod_Produt = SIT.Cod_Produto AND XES.Cod_Estabe = SIT.Cod_Estabe
		INNER JOIN NFSCB SCB ON SIT.Cod_Estabe = SCB.Cod_Estabe AND SIT.Ser_Nota = SCB.Ser_Nota AND SIT.Num_Nota = SCB.Num_Nota
WHERE XES.Cod_Estabe = 1
	AND PRO.Cod_Fabricante = 123
	AND SCB.Dat_Emissao >= '20230101'
	AND SCB.Dat_Emissao <= '20230201'
group by
		Codigo,
		Descricao,
		Cod_EAN,
		EIT.Vlr_BruItem,
		SIT.Qtd_Produto,
		SIT.Vlr_TotItem,
		ECB.Dat_Entrada,
		SCB.Dat_Emissao
having(EIT.Vlr_BruItem) <= SCB.Dat_Emissao

----------------------------------------------------------------------------------------------

SELECT 
	Codigo,
	Descricao,
	Cod_EAN,
	Prc_UltEnt = (select Prc_UltEnt from NFEIT) ,
	IT.Qtd_Produto,
	IT.Vlr_TotItem
FROM PRODU PR
		INNER JOIN PRXES ES ON PR.Codigo = ES.Cod_Produt
		INNER JOIN NFSIT IT ON ES.Cod_Produt = IT.Cod_Produto AND ES.Cod_Estabe = IT.Cod_Estabe
		INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
WHERE ES.Cod_Estabe = 1
	AND PR.Cod_Fabricante = 123
	AND CB.Dat_Emissao >= '20231231'
	AND CB.Dat_Emissao <= '20230102'

----------------------------------------------------------------------------------------------


DECLARE @codProd int, @Descr VARCHAR(80), @EAN VARCHAR(13)
 
-- Cursor para percorrer os registros
DECLARE cursor1 CURSOR FOR
SELECT
	Codigo,
	Descricao,
	Cod_EAN
FROM PRODU PR
		INNER JOIN PRXES ES ON PR.Codigo = ES.Cod_Produt
WHERE Cod_Estabe = 1
	AND Cod_Fabricante = 123
	AND Dat_UltVenda BETWEEN '20230101' AND '20230131'


FETCH NEXT FROM cursor1 INTO @codProd, @Descr, @EAN

Declare cursor2 CURSOR FOR
SELECT 
	IT.Qtd_Produto,
	IT.Vlr_TotItem
FROM NFSIT IT 
	INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
WHERE Cod_Produto = @codProd 



--Abrindo Cursor
OPEN cursor1
 
-- Lendo a próxima linha
FETCH NEXT FROM cursor1 INTO @codcliente, @primeironome, @sobrenome
 
-- Percorrendo linhas do cursor (enquanto houverem)
WHILE @@FETCH_STATUS = 0
BEGIN
 
-- Executando as rotinas desejadas manipulando o registro
update clientes set nomecompleto = @primeironome + ' ' + @sobrenome where codcliente = @codcliente
 
-- Lendo a próxima linha
FETCH NEXT FROM cursor1 INTO @codcliente, @primeironome, @sobrenome
END
 
-- Fechando Cursor para leitura
CLOSE cursor1
 
-- Finalizado o cursor
DEALLOCATE cursor1
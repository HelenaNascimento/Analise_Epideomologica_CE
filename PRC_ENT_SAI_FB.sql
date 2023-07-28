Declare @CodEst int = 1 , @DatIn date = '20230101', @DatFi date = '20230131', @Fabr NUMERIC = 832



SELECT 
     PR.CODIGO,
     PR.DESCRICAO,
     Format(IT.Prc_Unitario, 'c', 'pt-br') as Prc_Uni_Entrada,
     IT.Qtd_PedFat,
     IT.Qtd_BonFat,
     Format(CB.Dat_Entrada, 'd', 'en-gb') as Data     
     FROM PRODU PR
        INNER JOIN NFEIT IT ON PR.CODIGO = IT.Cod_Produto
        INNER JOIN NFECB CB ON IT.COD_ESTABE = CB.COD_ESTABE AND IT.PROTOCOLO =  CB.PROTOCOLO 
WHERE IT.COD_ESTABE = @CodEst  
    AND CB.Dat_Entrada >= @DatIn
    AND CB.Dat_Entrada <= @DatFi
    AND  PR.cod_fabricante = @Fabr
order by Codigo, Dat_Entrada

select 
	cod_produt as Cod_Produto, 
	pr.Descri as Descricao,
	'Saída',
	format(scb.Dat_Emissao, 'd', 'en-gb'),
	Qtd_Venda = (sit.Qtd_Produto - sit.Qtd_Bonificacao),
	format (sit.Prc_Unitario, 'c', 'pr-br')Prc_Unitario,
	cast(sit.Per_Descon as decimal(10,2)) as Per_Descon,
	format (sit.Vlr_LiqItem, 'c', 'pr-br' )as Vlr_LiqUnit,
	'/' as '/',
	format((sit.Vlr_LiqItem/(sit.Qtd_Produto - sit.Qtd_Bonificacao)), 'c', 'pt-br'),
	sit.Qtd_Bonificacao
	from PRODU pr 
		inner join PRXES es on pr.codigo = es.Cod_Produt 
		inner join NFSIT sit on es.cod_estabe = sit.cod_estabe and es.cod_produt = sit.cod_produto
		inner join NFSCB scb on sit.cod_estabe = scb.cod_estabe and sit.Ser_Nota = scb.ser_nota and sit.Num_Nota = scb.Num_Nota
where
	es.cod_estabe= 1 and
	pr.cod_fabricante= 832 and
	scb.Dat_Emissao >= '20230101' and
	scb.Dat_Emissao <= '20230131'

group by 	
	cod_produt, 
	pr.Descri,
	scb.Dat_Emissao,
	sit.Qtd_Produto,
	sit.Qtd_Bonificacao,
	sit.Prc_Unitario,
	sit.Per_Descon,
	Vlr_LiqItem

Order by cod_produt, scb.Dat_Emissao
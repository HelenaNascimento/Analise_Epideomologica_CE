select distinct
	cod_produt as Cod_Produto, 
	pr.Descri as Descricao,
	Ent_Sai = 'Entrada',
	format(ecb.Dat_Entrada, 'd', 'en-gb') as Dat_Movi,
	sum(eit.Qtd_Pedido) as Qtd_Pedido,
	format (eit.Prc_UniFat, 'c', 'pt-br') as Prc_Uni_Ent,
	cast(eit.Per_DescItem as decimal(8,2)) as Desconto,
	Format (eit.Vlr_TotItem, 'c', 'pt-br') as Vlr_Total,
	'/' as '/',
	Vlr_UntLiq =  '--------------',
--	LucroApr = '--------------',
	Qtd_Bonif = '000'
	from  PRODU pr
		inner join PRXES es on pr.codigo = es.Cod_Produt 
		inner join NFEIT eit on es.cod_estabe = eit.cod_estabe and es.cod_produt = eit.cod_produto
		inner join NFECB ecb on eit.cod_estabe = ecb.cod_estabe and eit.protocolo = ecb.protocolo
where
	es.cod_estabe= 1 and
	pr.cod_fabricante = 832 and
	ecb.dat_entrada >= '20230101' and
	ecb.dat_entrada <= '20230131'

group by 	
	cod_produt, 
	pr.Descri,
	es.Qtd_Dispon,
	ecb.Dat_Entrada,
	eit.Prc_UniFat,
	eit.Per_DescItem,
	eit.Vlr_TotItem,
	eit.Qtd_Pedido


union all 

select distinct
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

Order by cod_produt, Dat_Movi, Ent_Sai
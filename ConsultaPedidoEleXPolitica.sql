
select	
	pc.id_polcom, 
	ppr.cod_produt, 
	pit.Prc_Unitario,
	pre.cod_ean,
	Sld_Disp = (xes.Qtd_fisico - (xes.Qtd_Solici + xes.Qtd_Quaren)),
	pit.Qtd_Solicitado,
	pit.Qtd_Pra,
	pit.Des_MtvRej,
	ppr.per_descon as per_descon_pedido,
	outi.per_descon as per_descon_outi
	from POCOM pc 
		inner join PCXES pes on pc.id_polcom = pes.id_polcom
		inner join PCXPR ppr on pes.id_polcom = ppr.id_polcom
		inner join PDVIT pit on ppr.cod_produt  = pit.cod_produto and ppr.id_polcom = pit.id_polcom
		left outer join (select cod_produt, id_polcom, per_descon from PCXPR where id_polcom = 3014) as outi on ppr.cod_produt = outi.cod_produt and ppr.Id_PolCom <> outi.id_polcom
		left outer join PRXES xes on pes.cod_estabe = xes.cod_estabe and ppr.cod_produt = xes.cod_produt
		left join PREAN pre on xes.cod_produt = pre.cod_produt and pre.cod_ean like '7%'
where pes.cod_estabe = 1
	and pit.cod_pedido = '2184251'
	and pc.id_polcom = 2719


union all

select	
	pc.id_polcom, 
	ppr.cod_produt,
	pit.Prc_Unitario,
	pre.cod_ean,
	Sld_Disp = (xes.Qtd_fisico - (xes.Qtd_Solici + xes.Qtd_Quaren)),
	pit.Qtd_Solicitado,
	pit.Qtd_Pra,
	pit.Des_MtvRej,
	ppr.per_descon as per_descon_pedido,
	outi.per_descon as per_descon_outi
	from POCOM pc 
		inner join PCXES pes on pc.id_polcom = pes.id_polcom
		inner join PCXPR ppr on pes.id_polcom = ppr.id_polcom
		inner join PDVIT pit on ppr.cod_produt  = pit.cod_produto and ppr.id_polcom = pit.id_polcom
		left outer join (select cod_produt, id_polcom, per_descon from PCXPR where id_polcom = 2719) as outi on ppr.cod_produt = outi.cod_produt and ppr.Id_PolCom <> outi.id_polcom
		left outer join PRXES xes on pes.cod_estabe = xes.cod_estabe and ppr.cod_produt = xes.cod_produt
		left join PREAN pre on xes.cod_produt = pre.cod_produt and pre.cod_ean like '7%'
where pes.cod_estabe = 1
	and pit.cod_pedido = '2184251'
	and pc.id_polcom = 3014

order by id_polcom, cod_produt


--1463582
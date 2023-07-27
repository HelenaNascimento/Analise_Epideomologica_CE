select 
		vd.codigo,
		vd.nome_completo AS Respresentante,
		su.Nome_Completo as SUPERVISOR,
		cd.Cod_estado,
		cd.Descricao,
		count(cl.codigo) as Qtd_Cliente

	from ENXES es 
		inner join VENDE vd on es.cod_vendedor = vd.codigo
		inner join super su on vd.cod_supervisor = su.codigo
		inner join clien cl on es.cod_client = cl.codigo 
		inner join CIDAD as cd on cl.cod_cidade = cd.codigo and cl.Cod_Estado = cd.Cod_Estado		


where
	es.cod_estabe = 1 and 
	cl.bloqueado = 0 and
	vd.nome_guerra like 'CE%' and
	vd.Bloqueado = 0 and
	vd.cod_tipvenbas = 'EXT' and
	vd.flg_export = 1 and
	vd.codigo <> 464 and
	es.cod_client > 0
group by 
		vd.codigo,
		vd.nome_completo,
		su.Nome_Completo,
		cd.cod_estado,
		cd.Descricao
order by Respresentante, cd.Descricao

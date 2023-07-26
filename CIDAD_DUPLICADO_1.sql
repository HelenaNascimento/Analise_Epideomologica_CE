select cid.Codigo, ci.Cod_Estado, Descricao--, count(cl.codigo) 
	from CIDAD ci     
		inner join (select Cod_Estado ,codigo from CIDAD) cid on ci.Cod_Estado <> cid.Cod_Estado and ci.Codigo = cid.Codigo
		--inner join CLIEN cl on ci.Codigo = cl.Cod_Cidade and ci.Cod_Estado = cl.Cod_Estado
group by cid.Codigo, ci.Cod_Estado, Descricao, ci.Codigo
order by ci.Codigo
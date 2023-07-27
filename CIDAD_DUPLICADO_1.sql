select cid.Codigo, ci.Cod_Estado, Descricao
	from CIDAD ci     
		inner join (select Cod_Estado ,codigo from CIDAD) cid on ci.Cod_Estado <> cid.Cod_Estado and ci.Codigo = cid.Codigo
group by cid.Codigo, ci.Cod_Estado, Descricao, ci.Codigo
order by ci.Codigo
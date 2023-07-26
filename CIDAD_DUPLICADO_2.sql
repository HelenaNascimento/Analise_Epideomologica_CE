select Cidade = 280,  Estado = 'MG', count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 280 and Cod_Estado = 'MG'

select Cidade = 285, Estado = 'MG', count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 285 and Cod_Estado = 'MG'

select Cidade = 289, Estado = 'CE', cl.codigo, count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 289 and Cod_Estado = 'CE'
group by cl.codigo

select Cidade = 291, Estado = 'PI', count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 291 and Cod_Estado = 'PI'

select  Cidade = 294, Estado = 'PE', cl.codigo, count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 294 and Cod_Estado = 'PE'
group by cl.codigo

select Cidade = 302, Estado = 'CE', cl.codigo, count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade =302 and Cod_Estado = 'CE'
group by cl.codigo

select Cidade = 307, Estado = 'RN', count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 307 and Cod_Estado = 'RN'

select Cidade = 309, Estado = 'PB', count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 309 and Cod_Estado = 'PB'

select Cidade = 312, Estado = 'PE', cl.codigo, count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 312 and Cod_Estado = 'PE'
group by cl.codigo

select Cidade = 322, Estado = 'PI',count(num_nota) from CLIEN cl
		inner join NFSCB cb on cl.Codigo = cb.Cod_Cliente 
where cl.Cod_Cidade = 322 and Cod_Estado = 'PI'



--311
--335
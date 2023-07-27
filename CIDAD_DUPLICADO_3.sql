--select cid.Codigo, ci.Cod_Estado, Descricao
--	from CIDAD ci     
--		inner join (select Cod_Estado ,codigo from CIDAD) cid on ci.Cod_Estado <> cid.Cod_Estado and ci.Codigo = cid.Codigo
--order by ci.Codigo

-- PASSO 1
CREATE TABLE ##CIDADREPETE ( Cod_Estado CHAR(2),
	Codigo INT,
	Descricao VARCHAR(25),
	Cod_CidIbge Varchar(7),
	Qtd_Cliente INT,
	Cod_Estabe INT)

INSERT INTO ##CIDADREPETE
select 
	
	cd.Cod_Estado,
	cd.codigo,
	cd.Descricao,
	cd.Cod_CidIbge,
	count(cl.codigo) as Qtd_Cliente,
	es.Cod_Estabe
	from  CIDAD cd 
		inner join (select Codigo, Cod_Estado from cidad) as cd1 on cd.Codigo = cd1.Codigo and cd.Cod_Estado <> cd1.Cod_Estado
		left join CLIEN cl on  cl.Cod_Cidade = cd.Codigo and cl.Cod_Estado = cd.Cod_Estado
		left join ENXES es on cl.codigo = es.Cod_Client
	where es.Cod_Estabe is null
group by 	
	
	cd.Cod_Estado,
	cd.Codigo,
	cd.Descricao,
	es.Cod_Estabe,	
	cd.Cod_CidIbge
order by cd.Codigo

--PASSO 2

declare @cod int = (select max(codigo) from cidad)

begin tran
UPDATE cd
SET cd.Codigo = (@cod + 1)
	from CIDAD cd
		inner join ##CIDADREPETE cd1 on cd.Codigo = cd1.Codigo and cd.Cod_Estado = cd1.Cod_Estado and cd.Cod_CidIbge = cd.Cod_CidIbge

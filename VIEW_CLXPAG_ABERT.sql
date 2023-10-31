
CREATE VIEW CLXPAG_ABERT AS

select 
	Dias_Atraso = cast(((getdate()-1) - Dat_Vencimento)as int),
	format(Dat_Vencimento, 'd') as Dat_Vencimento, 
	format(Dat_Emissao, 'd') as Dat_Emissao,
	convert(decimal(10,2), Vlr_Documento) as Vlr_Documento,
	convert(decimal(10,2), ct.Per_Juros) as Per_Juros,
	convert(decimal(10,2), ((Vlr_Documento * (Per_Juros / 100)))) as Ao_Dia,
	convert(decimal(10,2), ((Vlr_Documento * (Per_Juros / 100)) * cast(((getdate() - 1) - Dat_Vencimento)as int))) as 'Vlr_Jrs+Mult',
	convert(decimal(10,2), ((Vlr_Documento * (Per_Juros / 100)) * cast(((getdate() - 1) - Dat_Vencimento)as int)) + Vlr_Documento) as Vlr_Atual,
	Status as Status_Documento,
	Num_Documento,
	Cod_Documento,
	ct.Par_Documento,
	cl.codigo,
	cl.Cgc_Cpf,
	cl.Razao_Social,
	cd.Descricao as Cidade,
	ve.Nome_Guerra as Vendedor,
	Cod_Agente,
	Cod_EstOri
from CTREC ct
	inner join CLIEN cl on ct.Cod_Cliente = cl.Codigo
	inner join CIDAD cd on cl.Cod_Cidade = cd.Codigo
	inner join VENDE ve on ct.Cod_Vendedor = ve.Codigo
where Cod_Estabe = 1
	and ct.Status = 'A'
	and cast(((getdate()-1) - Dat_Vencimento)as int) > 0
--	and Cod_Documento = 666376

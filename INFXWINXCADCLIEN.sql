/*
Cadastro de Cliente:
Código / Cliente / Fantasia / Tipo Pessoa / CNPJ/CPF / Insc. Est. / Atividade
Telefone / Email / Cons Final / Contribu / Data Cadastro / Data ult Altera
CEP / End Comercial / Numero / Bairro / Cidade IBGE / Cod País / Praça (Grupo de Cliente)
End Cobrança / CEP / Duplicata (Padrão: Não) / Permitir Agrupar Títulos (Padrão: Sim)
RCA (Vendedor) / Cliente Passível de Protesto (Padrão: Não) / Calcular ST (Padrão: Não)
Calcular PIS/Cofins (Padrão: Não) / Situação no e-commerce Unileve (Padrão: Não)
*/
select  top 100 
	Código = cl1.Codigo, 
	Cliente = cl1.razao_social, 
	Fantasia = Fantasia, 
	Tipo_Pessoa = pessoa, 
	CNPJ_CPF = cgc_cpf, 
	Insc_Est = cl1.Cgf, 
	Atividade = cod_RamoAtividade, 
	Telefone = Fone1,
	Email = cl1.Email, 
	Cons_Final = Case
			when Tipo_Consumidor = 'N' then 'NÃO'
			when Tipo_Consumidor = 'F' then 'SIM'
		end,
	Contribu = Case
		when Tipo_Consumidor  = 'N' and (cl1.Cgf like '00000%' or cl1.Cgf = 'ISENTO') then 'NÃO'
		when Tipo_Consumidor  = 'F' and (cl1.Cgf like '00000%' or cl1.Cgf = 'ISENTO') then 'NÃO'
		when Tipo_Consumidor  = 'N' and cl1.Cgf >= '0' then 'SIM'
		when Tipo_Consumidor  = 'F' and cl1.Cgf >= '0' then 'SIM'
		end,
	Data_Cadastro,
	Data_UltAltera = '',
	cl1.CEP,
	cl1.Endereco,
	CL1.Numero,
	Cod_Bairro,
	Cod_Cidade,
	Id_Pais,
	Praca= '',
	End_Cob =
		case 
			when Endereco_Cob is null and cl1.codigo = cl1.Cod_CliPag then cl1.Endereco
			when Endereco_Cob is null and cl1.codigo <> cl1.Cod_CliPag then ''
			when Endereco_Cob is not null and cl1.codigo = cl1.Cod_CliPag then Endereco_Cob
			when Endereco_Cob is not null and cl1.codigo <> cl1.Cod_CliPag then cl1.Endereco_Cob
		end,
	Cep_Cobr =
		case 
			when Cep_Cob is null and cl1.codigo = cl1.Cod_CliPag then cl1.Cep
			when Cep_Cob is null and cl1.codigo <> cl1.Cod_CliPag then ''
			when Cep_Cob is not null and cl1.codigo = cl1.Cod_CliPag then Cep_Cob
			when Cep_Cob is not null and cl1.codigo <> cl1.Cod_CliPag then cl1.Cep_Cob
		end,
	Duplicata = '',
	RCAxEstab0 =
		case 
			when es.Cod_Estabe = 0 and cod_client > 0 and  es.Cod_Vendedor > 0  then vd.Nome_completo
		end,
	RCAxEstab1 =
		case 
			when es.Cod_Estabe = 1 and cod_client > 0 and  es.Cod_Vendedor > 0  then vd.Nome_completo
		end,
	RCAxEstab3 =
		case 
			when es.Cod_Estabe = 3 and cod_client > 0 and  es.Cod_Vendedor > 0  then vd.Nome_completo
		end,
	RCAxEstab4 =
		case 
			when es.Cod_Estabe = 4 and cod_client > 0 and  es.Cod_Vendedor > 0 then vd.Nome_completo
		end,
	ClPasProt =
		case 
			when Flg_BlqProtes = 1 then 'SIM'
			when Flg_BlqProtes = 0 then 'NÃO'
		end,
	CalPisCofins ='NÃO',
	Calc_ST = 'NÃO',
	Unileve = 'NÃO'
	from CLIEN cl1
		inner join ENXES es on cl1.codigo = es.Cod_Client
		inner join vende vd on es.cod_vendedor = vd.codigo
where cl1.bloqueado=0
group by 
	cl1.Codigo,
	cl1.razao_social, 
	Fantasia, 
	pessoa,  
	Cod_Vendedor, 
	Cod_Estabe, 
	cgc_cpf, 
	cl1.Cgf, 
	cod_RamoAtividade, 
	Fone1, 
	cl1.email, 
	Tipo_Consumidor, 
	Data_Cadastro,
	cl1.cep,
	cl1.endereco,
	numero,
	Cod_Bairro,
	Cod_Cidade,
	id_pais,
	Endereco_Cob,
	Cep_Cob,
	Cod_CliPag,
	Flg_BlqProtes,
	cod_client,
	vd.codigo,
	vd.Nome_completo
order by cl1.Codigo

select 
	cb.Cod_Cliente,
	cl.Razao_Social,
	cd.Descricao,
	ct.Num_Documento,
	cb.num_nota,
	FORMAT(cb.Dat_Emissao, 'd') as Dat_Emissao,
	FORMAT(ct.dat_vencimento, 'd') as Dat_vencimento,
	IsNull(Format(ct.dat_quitacao, 'd'), 'NaoPago') as dat_quitacao,
	Dias_Atraso =  case
		when cast((dat_quitacao - Dat_vencimento)as int) is not null then cast((dat_quitacao - Dat_vencimento)as int)
		when cast((dat_quitacao - Dat_vencimento)as int) is null then cast((getdate() - Dat_vencimento)as int)
	end,
	count(num_nota) as qtd_num_nota,
	FORMAT(sum(Vlr_TotalNota), 'c', 'pt-br') as Vlr_TotalNota
	from NFSCB CB
		inner join CTREC CT ON cb.Cod_Estabe = ct.Cod_Estabe and cb.Cod_Pedido = ct.Cod_Pedido  
		inner join CLIEN CL ON cb.Cod_Cliente = cl.codigo and ct.Cod_Cliente = cl.Codigo
		left join CIDAD CD ON cl.Cod_Cidade = cd.Codigo
where cb.Cod_Estabe = 1 
	and Ser_Nota = '1'
	and Tip_Saida = 'V'
	and Ret_CStat = 100


group by 
	ct.Num_Documento,
	cb.num_nota,
	cb.Cod_Cliente,
	cl.Razao_Social,
	cd.Descricao,
	cb.Dat_Emissao,
	ct.dat_vencimento,
	ct.dat_quitacao


--Dias_Atraso = cast(((getdate()-1) - Dat_Vencimento)as int)


--SELECT TOP 5 * FROM CTREC
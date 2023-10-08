CREATE VIEW SAI_VENDAS AS
select 
	FORMAT(Dat_Emissao, 'd') as Dat_Emissao,
	count(num_nota) as qtd_num_nota,
	sum(Vlr_TotalNota) as Vlr_TotalNota
	from NFSCB
where Cod_Estabe = 1 
	and Ser_Nota = '1'
	and Tip_Saida = 'V'
	and Ret_CStat = 100
group by Dat_Emissao
GO
select 
	cod_produt,
	Dat_Movime, 
	sum (Qtd_EntCom + Qtd_EntOut + Qtd_EntDev)as Qtd_Entrada,
	(sum(qtd_saiven + Qtd_Saiout) * -1) as Qtd_Saida 
	from PRSLD
where cod_estabe = 1 
	and cod_produt in (select codigo from produ where cod_fabricante = 832) 
	and dat_movime between '20230101' and '20230228'
group by	
	cod_produt,
	Dat_Movime
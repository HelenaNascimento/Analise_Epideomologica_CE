select 
	lot.Cod_Estabe, 
	lot.Cod_Produt, 
	lot.Qtd_Fisico,
	xes.Qtd_Fisico
	from PRLOT LOT 
		INNER JOIN PRXES XES ON LOT.Cod_Estabe = XES.Cod_Estabe and lot.Cod_Produt = XES.Cod_Produt
where LOT.Qtd_Fisico > xes.Qtd_Fisico and  xes.Qtd_Fisico = 0
group by 	
	lot.Cod_Estabe, 
	lot.Cod_Produt, 
	lot.Qtd_Fisico, 
	xes.Qtd_Fisico 

order by lot.Cod_Estabe, lot.Cod_Produt


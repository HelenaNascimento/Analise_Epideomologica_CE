select distinct 
	tb.cod_estabe, 
	tb.Cod_Layout, 
	cb.Layout, 
	tb.Des_Grupo  
	from TBLPG tb
		inner join PDECB cb on tb.Cod_Estabe = cb.Cod_Estabe and tb.Cod_Layout = cb.Cod_Layout 
where Des_Grupo like '%natulab%'
order by tb.cod_estabe, tb.Cod_Layout
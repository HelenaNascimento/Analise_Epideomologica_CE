SELECT
	CB.Numero as 'Nº Pedido',
	Status_Pedido = 
		CASE
			WHEN Status1 = 'P' and Status2 = 'A' THEN 'Aberto'
			WHEN Status1 = 'P' and Status2 = 'F' THEN 'Em Preparação'
			WHEN Status1 = 'D' THEN 'Fechado'
			WHEN Status1 = 'C' THEN 'Cancelado'
		END,
	cast(dat_pedido as date)  as Data_Pedido, 
	cast(Hor_Entrada as time(7)) as Hora,
	Cl.codigo as Cod_Cliente,
	CL.Razao_Social as 'Razao Social',
	Cod_Vendedor,
	VD1.Nome_Guerra as Vendedor,
	Cod_VendTlmkt,
	VD2.Nome_Guerra as Telemarketing,
	Origem_Pedido=
		CASE
			WHEN Cod_OrigemPdv = 'AL' THEN 'Ativo'
			WHEN Cod_OrigemPdv = 'TL' THEN 'Eletronico'
			WHEN Cod_OrigemPdv = 'ML' THEN 'Móvel'
		END,
	format(C_VlrPedido, 'c', 'pt-br') as Vlr_Pedido
	FROM PDVCB CB
		inner join CLIEN CL on CB.cod_Cliente = CL.codigo
		left join VENDE VD1 on CB.cod_vendedor = VD1.codigo
		left join VENDE VD2 on CB.cod_vendedor = VD2.codigo
WHERE cod_estabe = 1   
and TIP_Faturamento = 'FAT'
and Cod_OrigemPdv in ('AL', 'TL', 'ML')
and cast(Hor_Entrada as time(7)) > '18:00:00' 
and year(dat_pedido) = '2024'
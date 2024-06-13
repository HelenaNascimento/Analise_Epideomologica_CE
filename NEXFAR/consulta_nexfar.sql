SELECT 
	Estabelecimento = case
		when cod_estabe = 0 then 'NOVAPE'
		when cod_estabe = 1 then 'NOVACE'
		when cod_estabe = 3 then 'MULT'
		when cod_estabe = 4 then 'NOVABA'
	end,  
	Status = 
	case
		when Status1 = 'P' then 'Pendente'
		when Status1 = 'D' then 'Depachado'
	end, 
	cod_cliente, 
	bloqueio, 
	msg_bloqueio,
	Cod_PedCmpCli,
	dat_pedido
FROM PDVCB
where Cod_PedCmpCli in ('139793388195160'
,'139869688925570'
,'139871938813949'
,'139872013628842'
,'139873653292569'
,'140019935246991'
,'140096392108784'
,'140125444704629'
,'140126054275686'
,'140130332700728'
,'140132017492526'
,'140132962689137'
,'140133014404978'
,'140135376827341'
,'140135734066514'
,'140136803577425'
,'140137346610992'
,'140140303123826')
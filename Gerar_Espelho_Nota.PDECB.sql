update PDECB
set
Sta_Pedido=1
where Cod_Estabe=1 and Cod_Layout=22
and Dat_LeiPed between '20240901' and '20241003' and Num_PedVen > 0


select * from PDECB where Cod_PedCli='2024090400000510721E' and Num_PedVen=50425140 and Cod_Estabe=0
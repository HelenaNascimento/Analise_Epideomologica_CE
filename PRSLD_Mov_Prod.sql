SELECT * from PRSLD
WHERE Cod_Estabe = 1 
and dat_movime >='20240213'
and dat_movime <='20240213'


Select Cod_Produt, Cod_Lote, Num_SeqBal, Cod_MovEst, Qtd_Produt, Tip_Movim, Qtd_SldEst, Num_SeqBal from BALIT
where cod_estabe = 1 
and Num_SeqBal in (46, 47 ,48 ,49, 50, 51 )
order by Cod_Produt, Cod_Lote, Num_SeqBal
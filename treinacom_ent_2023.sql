SELECT 
distinct
Cod_Produt,
Mes = 
	case
		when month(Dat_Movime) = 01 then '01_JAN'
		when month(Dat_Movime) = 02 then '02_FEV'
		when month(Dat_Movime) = 03 then '03_MAR'
		when month(Dat_Movime) = 04 then '04_ABR'
		when month(Dat_Movime) = 05 then '05_MAI'
		when month(Dat_Movime) = 06 then '06_JUN'
		when month(Dat_Movime) = 07 then '07_JUL'
		when month(Dat_Movime) = 08 then '08_AGO'
		when month(Dat_Movime) = 09 then '09_SET'
		when month(Dat_Movime) = 10 then '10_OUT'
		when month(Dat_Movime) = 11 then '11_NOV'
		when month(Dat_Movime) = 12 then '12_DEZ'
	end,
SUM(Qtd_EntCom + Qtd_EntDev + Qtd_EntOut + Qtd_EntTrf + Qtd_InfEntInv) as Soma_Entrada
--SUM(Qtd_SaiVen + Qtd_SaiDev + Qtd_SaiOut + Qtd_SaiTrf + Qtd_InfSaiInv) as Soma_Saida
FROM PRSLD
WHERE Cod_Estabe = 1
AND Dat_Movime >= '20230101'
AND Dat_Movime <= '20231231'
AND Cod_Produt IN (1815 ,20987 ,1740,22207,12354,15227,22206,4299,22209,5756,11584,10611,10126,8749,13602,1056,16718,397,6887,14104,12340,14061,20917
,8835,19921,14654,10615,1035,5036,12514,9354,21046,10566,10177,15392,6411,13124,6888,882,19256,15229,21015,10545,19038,10739,11741,10543,12631,12345
,10472,10715,10695,5067,10666,13578,21581,10084,13013,21018,10853,16351,4858,10697,6651,15309,13601,10001,19039,12625,2253,21147,20986,10435,20246,10161
,10745,7594,14485,14582,5237,925,10040,13694,6892)
GROUP BY 
	Cod_Produt,
	Dat_Movime
ORDER BY 1,2
--having(Soma_Entrada > 0)
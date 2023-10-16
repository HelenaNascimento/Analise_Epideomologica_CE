SELECT DISTINCT 
	PRO.CODIGO,
	PRO.DESCRICAO,
	PRO.COD_EAN,
	format(XES.Prc_UltEnt, 'c', 'pt-br') as Prc_UltEnt, 
	format(max(Dat_UltCompra), 'd', 'en-gd') as Dat_UltCompra,
	format(XES.Prc_EntAnt,'c', 'pt-br') as Prc_EntAnt,
	format(max(DAT_EntAnt), 'd', 'en-gd') as Dat_EntAnt
	FROM PRODU PRO 
		INNER JOIN PRXES XES ON PRO.CODIGO= XES.COD_PRODUT
WHERE --XES.COD_ESTABE = 1
	 COD_FABRICANTE = 321
	AND DAT_EntAnt >='20230101'
	AND DAT_ULTCOMPRA <= '20231016'
group by
	PRO.CODIGO,
	PRO.DESCRICAO,
	PRO.COD_EAN,
	XES.Prc_UltEnt,
	XES.Prc_EntAnt,
	DAT_EntAnt
ORDER BY 1

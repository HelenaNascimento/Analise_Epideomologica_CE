select  
	ct.cod_documento,
	ct.NUM_documento,
	ct.dat_emissao, 
	ct0.dat_quitacao,
	ct.transacao,
	ct.status as Status_Serv_Atual,
	ct0.status as Status_Serv_bkp,
	BX.Cod_Rec
from  RemoteServerName.[DMD].[dbo].[ctrec] ct
	JOIN PROD_2023.dbo.ctrec ct0 
		on ct.Cod_Estabe = ct0.Cod_Estabe 
			and ct.cod_documento = ct0.cod_documento
	JOIN PROD_2023.dbo.BXREC  BX ON CT0.Cod_Estabe = BX.Cod_Estabe AND CT0.Cod_Documento = BX.Cod_Documento
where ct.Cod_Estabe = 1
and ct.status = 'A'
AND ct0.status = 'Q'
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

SELECT 
	bx.Cod_Estabe,
	ct.Cod_Documento,
	Par_Documento
	--count(ct.Cod_Documento) as Qtd
	FROM BXREC bx
		JOIN CTREC ct on bx.Cod_Estabe = ct.Cod_Estabe and  bx.Cod_Documento = ct.Cod_Documento
		JOIN RECEB rc on bx.Cod_Estabe = rc.Cod_Estabe and bx.Cod_Rec = rc.Cod_rec
WHERE 
bx.Cod_rec = 178421
and	ct.Status = 'A'
and bx.Status = 'Q'
and rc.Data >= '20260112'
order by 1

select COUNT(*) from RECEB
WHERE Cod_Estabe = 4 AND Cod_rec = 178421



select ct.cod_estabe,
count(ct.cod_documento)
FROM CTREC CT
INNER JOIN BXREC BX
    ON CT.Cod_Estabe = BX.Cod_Estabe
   AND CT.Cod_Documento = BX.Cod_Documento
WHERE  BX.Status = 'Q'
  AND CT.Status = 'A'
  group by  ct.cod_estabe

  commit

begin tran
UPDATE CT
SET CT.Status = BX.Status
FROM CTREC CT
INNER JOIN BXREC BX
    ON CT.Cod_Estabe = BX.Cod_Estabe
   AND CT.Cod_Documento = BX.Cod_Documento
WHERE BX.Cod_Estabe = 4 --(codigo do estabelecimento a aplicar a correção)
  AND BX.Status = 'Q'
  AND CT.Status = 'A';

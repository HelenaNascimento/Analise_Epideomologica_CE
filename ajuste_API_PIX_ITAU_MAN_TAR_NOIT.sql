

SELECT cr.Cod_Agente, cr.*,                                    
ag.Api_CliIdSec, 
px.Tip_Chave, 
px.Chave 
FROM CTREC cr                                                                         
INNER Join AGCOB ag on cr.Cod_Agente = ag.Codigo                                      
LEFT JOIN TBPIX px on px.Cod_Entid = ag.Codigo                                        
Where ISNULL(ag.Api_CliId,'') <> ''                                            
And cr.Status = 'A'                                                                
AND IsNull(cr.Num_Bloqueto,'') <> ''                                              
AND IsNull(cr.Cod_Barra,'') = ''                                                  
AND ag.Flg_ImpBlo = 1                                                                 
AND ag.Flg_IntApiBan = 1                                                           
AND cr.Cod_ServRem <> ''                                                            
AND ((ISNULL(cr.Dat_Remessa, '') = ''
AND ISNULL(cr.Num_Remessa, '') = '')    
   OR (ISNULL(cr.Dat_Remessa, '') = '' AND ISNULL(cr.Num_Remessa, '') = '0'))







select concat(replicate('0',2-len(day(getdate()-1))), day(getdate()-1),'/',replicate('0',2-len(month(getdate()))), month(getdate()),'/',year(getdate())), * from INIDB_GLOBAL
order by dat_regist desc


--09/07/2026
update INIDB_GLOBAL set Des_Valor = concat(replicate('0',2-len(day(getdate()-1))), day(getdate()-1),'/',replicate('0',2-len(month(getdate()))), month(getdate()),'/',year(getdate()))
where Cod_Regist = 944


update nfecb
set
flg_importado=0, Arquivo=''
where Cod_Estabe = 4
and protocolo=413280



select * from NFECB where Protocolo = 413238
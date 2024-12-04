Set dateformat dmy
select convert(numeric(18,4),(SUBSTRING(cod_barra, 10,10))) as ValorNoCodBarras, convert(numeric(18,4),Vlr_Documento*100) as ValorDocumento, 
 floor(vlr_documento*100), Cod_Estabe, Cod_Barra, Vlr_Documento, Dat_Vencimento,SUBSTRING(cod_barra, 10,10), *
 from CTREC
 where 1=1
 --And Cod_Estabe = 1
 --and Cod_Agente = 344
 And Dat_Vencimento > '22/02/2025'
 And isnumeric(cod_barra) = 1
 And convert(numeric(18,4),(SUBSTRING(cod_barra, 10,10))) <> convert(numeric(18,4),floor(Vlr_Documento*100))


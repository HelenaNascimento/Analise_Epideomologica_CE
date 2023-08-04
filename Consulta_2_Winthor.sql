select cod_estabe, sum(cod_client) as cliXest
    from ENXES es
        inner join CLIEN cl on es.Cod_Client = cl.codigo  
where cl.Bloqueado = 0 and cl.Motivo_Bloqueio = ''
group by cod_estabe

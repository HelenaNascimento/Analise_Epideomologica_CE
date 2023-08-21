
select 
    ve.Cod_Client AS Codigo,
    cl.Razao_Social
    from CLXVE ve
        inner join CLIEN cl on ve.Cod_Client = cl.codigo
        inner join ENXES es on cl.Codigo = es.Cod_Client
where 
    Cod_Estabe = 1 and
    Cod_Vended = 656 
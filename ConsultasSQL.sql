
--Localizar Coluna --

SELECT
    T.name AS Tabela,
    C.name AS Coluna
FROM
    sys.sysobjects    AS T (NOLOCK)
INNER JOIN sys.all_columns AS C (NOLOCK) ON T.id = C.object_id AND T.XTYPE = 'U'
WHERE
    C.NAME LIKE '%Desc%'
ORDER BY
    T.name ASC


select * from V_VENDE
where codigo = 564

select * from Vende
where codigo = 564




SELECT TOP 5 * FROM CLIEN


SELECT
 --   T.name AS Tabela,
    C.name AS Coluna
FROM
    sys.sysobjects    AS T (NOLOCK)
INNER JOIN sys.all_columns AS C (NOLOCK) ON T.id = C.object_id AND T.XTYPE = 'U'
WHERE
    T.Name = 'CLIEN'
ORDER BY
    T.name ASC
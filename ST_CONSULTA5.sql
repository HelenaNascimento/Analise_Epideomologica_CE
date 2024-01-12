DECLARE @CProd INT,
        @CEstab INT = 1,
        @QVend INT,
        @QVAnd INT,
        @PDesc VARCHAR(80)

DECLARE CursorProd CURSOR FOR
    SELECT TOP 1
        COD_PRODUTO,
        Descri,
        MAX(IT.qtd) AS QTD_VENDA
    FROM (
        SELECT 
            Cod_Produto, 
            SUM(Qtd_ImpFat) AS Qtd, 
            Cod_Estabe, 
            ser_nota, 
            num_nota 
        FROM NFSIT 
        WHERE 
            cod_estabe = 1 
            AND cod_cfo IN (5102, 5405)
            AND EXISTS (
                SELECT Cod_Produto 
                FROM NFSIT IT 
                INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe 
                                      AND IT.Ser_Nota = CB.Ser_Nota 
                                      AND IT.Num_Nota = CB.Num_Nota 
                WHERE 
                    IT.Cod_Estabe = 1 
                    AND dat_emissao >= '20230101' 
                    AND dat_emissao <= '20230131'
            )
        GROUP BY 
            Cod_Produto,
            Cod_Estabe, 
            ser_nota, 
            num_nota 
    ) IT
    INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe 
                        AND IT.Ser_Nota = CB.Ser_Nota 
                        AND IT.Num_Nota = CB.Num_Nota
    LEFT JOIN PRODU PR ON IT.COD_PRODUTO = PR.CODIGO
WHERE 
    IT.Cod_Estabe = 1 
    AND dat_emissao >= '20240101' 
    AND dat_emissao <= '20240131'
GROUP BY 
    COD_PRODUTO,
    Descri
ORDER BY 3

OPEN CursorProd
FETCH NEXT FROM CursorProd INTO @CProd, @PDesc, @QVend
WHILE @@FETCH_STATUS = 0
BEGIN
    DECLARE ANOANT CURSOR FOR
        SELECT TOP 1
            COD_PRODUTO,
            Descri,
            MAX(IT.qtd) AS QTD_VENDA
        FROM (
            SELECT 
                Cod_Produto, 
                SUM(Qtd_ImpFat) AS Qtd, 
                Cod_Estabe, 
                ser_nota, 
                num_nota 
            FROM NFSIT 
            WHERE 
                cod_estabe = 1 
                AND Cod_Produto = 22863
                AND cod_cfo IN (5102, 5405)
            GROUP BY 
                Cod_Produto,
                Cod_Estabe, 
                ser_nota, 
                num_nota  
        ) IT
        INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe 
                              AND IT.Ser_Nota = CB.Ser_Nota 
                              AND IT.Num_Nota = CB.Num_Nota
        LEFT JOIN PRODU PR ON IT.COD_PRODUTO = PR.CODIGO
        WHERE 
            IT.Cod_Estabe = 1 
            AND it.Cod_Produto = 22863
            AND dat_emissao >= '20230101' 
            AND dat_emissao <= '20230131' -- Corrigir as datas conforme necessário
        GROUP BY 
            COD_PRODUTO,
            Descri
        ORDER BY 3 

    OPEN ANOANT
    FETCH NEXT FROM ANOANT INTO @CProd, @PDesc, @QVAnd
    WHILE @@FETCH_STATUS = 0
    BEGIN
        -- Faça algo com os resultados
        PRINT @CProd
        PRINT @PDesc
        PRINT @QVend
        PRINT @QVAnd

        FETCH NEXT FROM ANOANT INTO @CProd, @PDesc, @QVAnd
    END;

    CLOSE ANOANT
    DEALLOCATE ANOANT
    FETCH NEXT FROM CursorProd INTO @CProd, @PDesc, @QVend
END
CLOSE CursorProd
DEALLOCATE CursorProd

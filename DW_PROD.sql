Use DW_PROD
go


DECLARE 
    @codEstab INT = 1,
    @CodFab INT = 158,
    @DatIn SMALLDATETIME = '20230101',
    @DatFim SMALLDATETIME = '20231231',
    @CodProd INT,
    @PrcUnit DECIMAL(20, 2),
    @DatComp SMALLDATETIME

DECLARE CursorProd CURSOR FOR 
    SELECT DISTINCT
        Codigo,
        Cod_Fabricante
    FROM PROD_2023.dbo.PRODU
    WHERE Cod_Fabricante = @CodFab

OPEN CursorProd

FETCH NEXT FROM CursorProd INTO @CodProd, @CodFab
WHILE @@FETCH_STATUS = 0
BEGIN
    PRINT 'Processing Product: ' + CAST(@CodProd AS NVARCHAR(10))

    DECLARE Curv_ABC_ENT CURSOR FOR
        SELECT TOP 1 
            IT.Cod_Estabe, 
            IT.Cod_Produto, 
            TRY_CONVERT(DECIMAL(20, 2), Prc_Unitario) AS Prc_Unitario,
            Dat_Entrada
        FROM PROD_2023.dbo.NFEIT IT
        INNER JOIN PROD_2023.dbo.NFECB cb0 ON it.cod_estabe = cb0.Cod_Estabe AND it.Protocolo = cb0.Protocolo
        WHERE 
            it.Cod_Estabe = @codEstab
            AND Tip_NF <> 'D' 
            AND cb0.Dat_Entrada >= @DatIn
            AND cb0.Dat_Entrada <= @DatFim
            AND it.Cod_Produto = @CodProd
            AND status NOT IN ('A', 'C') 
        ORDER BY Dat_Emissao DESC

    OPEN Curv_ABC_ENT;
    FETCH NEXT FROM Curv_ABC_ENT INTO @codEstab, @CodProd, @PrcUnit, @DatComp;

    WHILE @@FETCH_STATUS = 0
    BEGIN
        PRINT 'Inserting into C_ENTRADA'
        PRINT 'Values: ' + CAST(@codEstab AS NVARCHAR(10)) + ', ' + CAST(@CodFab AS NVARCHAR(10)) + ', ' + CAST(@CodProd AS NVARCHAR(10)) + ', ' + CAST(@PrcUnit AS NVARCHAR(20)) + ', ' + CAST(@DatComp AS NVARCHAR(20))

        -- Use o cursor Curv_ABC_ENT para inserir os valores
        INSERT INTO C_ENTRADA (Cod_Estada, Cod_Fabr, Cod_Prod, PrcFab, PrcUltEnt, PrcUnit, DatComp)
        VALUES (@codEstab, @CodFab, @CodProd, 0, 0, @PrcUnit, @DatComp)

        FETCH NEXT FROM Curv_ABC_ENT INTO @codEstab, @CodProd, @PrcUnit, @DatComp;
    END;

    CLOSE Curv_ABC_ENT
    DEALLOCATE Curv_ABC_ENT

    FETCH NEXT FROM CursorProd INTO @CodProd, @CodFab
END

CLOSE CursorProd
DEALLOCATE CursorProd

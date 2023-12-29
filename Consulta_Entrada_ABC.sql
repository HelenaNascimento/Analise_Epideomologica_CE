DECLARE 
    @CESTA INT = 1,
    @CPROD INT,
    @CFABR INT = 1022,
    @CFORN INT = 349,
    @CEAN VARCHAR(14), 
    @DESC VARCHAR(80), 
    @PMED DECIMAL (20,2), 
    @PFABR DECIMAL (20,2),
    @PULENT DECIMAL (20,2),
    @QPED INT,
    @PPED DECIMAL(20,2), 
    @DATENT SMALLDATETIME,
    @QBON INT,
    @PBON DECIMAL(20,2)

DECLARE CAB_PROD CURSOR FOR
SELECT 
    PR.Cod_Fabricante,
    PR.codigo,
    PR.Cod_EAN,
    PR.Descri,
    ES.Prc_Fabric,
    ES.Prc_CusMedCom,
    ES.Prc_UltEnt   
    FROM PROD_2023.dbo.PRODU PR
        INNER JOIN PROD_2023.dbo.PRXES ES ON PR.CODIGO = ES.COD_PRODUT
WHERE 
    ES.COD_ESTABE = @CESTA
AND PR.COD_FABRICANTE = @CFABR
OPEN CAB_PROD;
FETCH NEXT FROM CAB_PROD INTO @CFABR, @CPROD, @CEAN, @DESC, @PFABR, @PMED, @PULENT 
WHILE @@FETCH_STATUS = 0
BEGIN

    DECLARE QTDFAT CURSOR FOR

        SELECT
            sum(it.Qtd_PedFat) as Qtd_PedFat  
            FROM PROD_2023.dbo.NFECB CB
                INNER JOIN PROD_2023.dbo.NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO
        WHERE 
            CB.COD_ESTABE = @CESTA
        AND CB.DAT_ENTRADA >= '20230101'
        AND CB.DAT_ENTRADA <= '20231130'
        AND CB.Cod_EmiFornec = @CFORN
        AND IT.COD_PRODUTO = @CPROD
        AND IT.Cod_Cfo in (2102, 2403, 2404)

        OPEN QTDFAT;
        FETCH NEXT FROM QTDFAT INTO @QPED 
        WHILE @@FETCH_STATUS = 0
        BEGIN
            DECLARE PRCFAT CURSOR FOR
                SELECT
                    top 1
                    Prc_Unitario ,
                    DAT_ENTRADA 
                    FROM PROD_2023.dbo.NFECB CB
                        INNER JOIN PROD_2023.dbo.NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO
                WHERE 
                    CB.COD_ESTABE = @CESTA
                AND CB.DAT_ENTRADA >= '20230101'
                AND CB.DAT_ENTRADA <= '20231130'
                AND CB.Cod_EmiFornec = @CFORN
                AND IT.COD_PRODUTO = @CPROD
                AND IT.Cod_Cfo in (2102, 2403, 2404)
                order by 2 desc
                OPEN PRCFAT;
                FETCH NEXT FROM PRCFAT INTO @PPED, @DATENT 
                WHILE @@FETCH_STATUS = 0
                BEGIN
                    DECLARE QTDBON CURSOR FOR
                        SELECT
                            ISNULL(sum(it.Qtd_PedFat), 0)
                            FROM PROD_2023.dbo.NFECB CB
                                INNER JOIN PROD_2023.dbo.NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO
                        WHERE 
                            CB.COD_ESTABE = @CESTA
                        AND CB.DAT_ENTRADA >= '20230101'
                        AND CB.DAT_ENTRADA <= '20231130'
                        AND CB.Cod_EmiFornec = @CFORN
                        AND IT.COD_PRODUTO = @CPROD
                        AND IT.Cod_Cfo in (2910)

                OPEN QTDBON;
                FETCH NEXT FROM QTDBON INTO @QBON
                WHILE @@FETCH_STATUS = 0
                BEGIN
                    DECLARE PRCBON CURSOR FOR
                        SELECT
                            top 1
                            Prc_Unitario,
                            DAT_ENTRADA  
                            FROM PROD_2023.dbo.NFECB CB
                                INNER JOIN PROD_2023.dbo.NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO
                        WHERE 
                            CB.COD_ESTABE = @CESTA
                        AND CB.DAT_ENTRADA >= '20230101'
                        AND CB.DAT_ENTRADA <= '20231130'
                        AND CB.Cod_EmiFornec = @CFORN
                        AND IT.COD_PRODUTO = @CPROD
                        AND IT.Cod_Cfo in (2910)
                        order by 2 desc
                    OPEN PRCBON;
                    FETCH NEXT FROM PRCBON INTO @PPED, @DATENT 
                    WHILE @@FETCH_STATUS = 0
                    BEGIN
                        /*
                        PRINT   CAST(@CFABR AS NVARCHAR(10)) +';'+ CAST(@CPROD AS NVARCHAR(10)) + ';' + CAST(@CEAN AS VARCHAR(14)) + ';' + CAST(@DESC AS VARCHAR(80)) + ';' + CAST(@PFABR AS NVARCHAR(22)) +';'+
                                CAST(@PMED AS NVARCHAR(22)) + ';'+ CAST(@PULENT AS NVARCHAR(22)) + ';' + CAST(@QPED AS NVARCHAR(22)) + ';' + CAST(@PPED AS NVARCHAR(22)) + ';' + CAST(@QBON AS NVARCHAR(22)) + ';'+
                                CAST(@PPED AS NVARCHAR(22))
                        */

                        SELECT @CFABR, @CPROD, @CEAN, @DESC, @PFABR, @PMED, @PULENT, @QPED, @PPED, @QBON, @PPED
                            
                        FETCH NEXT FROM PRCBON INTO @PPED, @DATENT 
                    END;
                    CLOSE PRCBON;
	                DEALLOCATE PRCBON;

                FETCH NEXT FROM QTDBON INTO @QBON
                END;
                CLOSE QTDBON
	            DEALLOCATE QTDBON

            FETCH NEXT FROM PRCFAT INTO @PPED, @DATENT 
            END;
            CLOSE PRCFAT
            DEALLOCATE PRCFAT
        
        FETCH NEXT FROM QTDFAT INTO @QPED 
        END;
        CLOSE QTDFAT
        DEALLOCATE QTDFAT  

    FETCH NEXT FROM CAB_PROD INTO @CFABR, @CPROD, @CEAN, @DESC, @PFABR, @PMED, @PULENT 
    END;
    CLOSE CAB_PROD
    DEALLOCATE CAB_PROD  
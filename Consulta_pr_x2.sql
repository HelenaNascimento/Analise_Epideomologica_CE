    --código do produto
    --descrição
    --unidade de venda
    --Fabricante
    --prec da ultima entrada
    --Prc Médio
    --Prc Venda
    --Qtd Físico
    --format(((Qtd_Fisico - (Qtd_Quaren + Qtd_Reserv)) * Prc_CusMedPra), 'c', 'pt-br') as  'Total'


    SELECT DISTINCT
        PR.CODIGO,
        PR.DESCRI, 
        PR.UNIDADE_VENDA,
        FB.FANTASIA,
        PRC_UNITARIO = CASE
                        WHEN CONVERT(DECIMAL(10,2), NE.PRC_UNITARIO) > '0.00' THEN format(NE.PRC_UNITARIO, 'c', 'pt-br')
                        WHEN CONVERT(DECIMAL(10,2), NE.PRC_UNITARIO) = '0.00' THEN (SELECT TOP 1 FORMAT(PRC_UNITARIO, 'c', 'pt-br') as PRC_UNITARIO
																							FROM NFEIT 
																						WHERE Dat_Movimento not in (SELECT MAX(Dat_Entrada) FROM NFECB ) 
																						AND COD_ESTABE = 1
																						AND CONVERT(DECIMAL (10,2), Prc_Unitario) > 0.00
                                                                                    ) --format(NE.PRC_UNITARIO, 'c', 'pt-br')
                    END,
        PRC_MEDIO = CASE
                        WHEN convert(decimal(10,2),Prc_CusMedPra) > '0.00' THEN format(Prc_CusMedPra, 'c', 'pt-br') 
                        WHEN convert(decimal(10,2),Prc_CusMedPra) = '0.00' THEN format(NE.Prc_Unitario, 'c', 'pt-br')  
                    END,
        PS.QTD_FISICO,
        Total = CASE
                    WHEN convert (decimal(10,2), Prc_CusMedPra) > '0.00' THEN format((Qtd_Fisico * Prc_CusMedPra), 'c', 'pt-br') 
                    WHEN convert (decimal(10,2), Prc_CusMedPra) = '0.00' THEN format((Qtd_Fisico * NE.Prc_Unitario), 'c', 'pt-br')
                END
--        MAX(DAT_ENTRADA) AS DAT_ENTRADA
        FROM PRODU PR
            INNER JOIN PRXES PS ON PR.CODIGO = PS.COD_PRODUT 
            INNER JOIN FABRI FB ON PR.COD_FABRICANTE = FB.CODIGO
            INNER JOIN NFEIT NE ON PR.CODIGO = NE.COD_PRODUTO
            INNER JOIN NFECB NF ON NE.COD_ESTABE = NF.COD_ESTABE AND NE.PROTOCOLO = NF.PROTOCOLO
    WHERE 
        PS.COD_ESTABE = 1 
    AND PS.QTD_FISICO > 0
    AND PS.FLG_BLOQUEADO = 0
    AND PR.FLAG_IMPRCLASSIF1 <> 'N' 
    AND PR.CODIGO = 15231
    AND PRC_UNITARIO IS NOT NULL 
    AND CONVERT(DECIMAL (10,2), Prc_Unitario) > 0.00

    GROUP BY 
		PS.QTD_FISICO,
        PR.CODIGO,
        PR.DESCRI,
        PR.UNIDADE_VENDA,
        FB.FANTASIA,
        PRC_UNITARIO,
        Prc_CusMedPra,
        DAT_ENTRADA
        
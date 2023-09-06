    --código do produto
    --descrição
    --unidade de venda
    --Fabricante
    --prec da ultima entrada
    --Prc Médio
    --Prc Venda
    --Qtd Físico
    --format(((Qtd_Fisico - (Qtd_Quaren + Qtd_Reserv)) * Prc_CusMedPra), 'c', 'pt-br') as  'Total'


    SELECT 
        PR.CODIGO,
        PR.DESCRI,
        PR.UNIDADE_VENDA,
        FB.FANTASIA,
        PRC_UNITARIO = CASE
                        WHEN CONVERT(DECIMAL(10,2), NE.PRC_UNITARIO) > '0.00' THEN format(NE.PRC_UNITARIO, 'c', 'pt-br')
                        WHEN CONVERT(DECIMAL(10,2), NE.PRC_UNITARIO) = '0.00' THEN (SELECT PRC_UNITARIO 
                                                                                        FROM NFEIT 
                                                                                    WHERE DAT_ENTRADA = (SELECT MAX(DAT_ENTRADA) FROM NFEIT WHERE PRC_UNITARIO > 0 GROUP BY DAT_ENTRADA )
                                                                                    GROUP BY PRC_UNITARIO) --format(NE.PRC_UNITARIO, 'c', 'pt-br')
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

    GROUP BY 
        PR.CODIGO,
        PR.DESCRI,
        PR.UNIDADE_VENDA,
        FB.FANTASIA,
        PRC_UNITARIO,
        Prc_CusMedPra,
        DAT_ENTRADA
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
        pr.Codigo,
        pr.Descri,
        pr.Unidade_Venda as Unidade_Venda,
        fb.Fantasia,
        format(Prc_UltEnt, 'c', 'pt-br') as Prc_Entrada,
        /*
         CASE
                            WHEN convert(decimal (10,2), Prc_UltEnt) > '0.00' THEN format(Prc_UltEnt, 'c', 'pt-br')
                            WHEN convert (decimal (10,2), Prc_UltEnt) = '0.00'THEN 
                                                        CASE 
                                                            WHEN convert(decimal(10,2), EIT.Prc_Unitario) > '0.00' THEN format(EIT.Prc_Unitario, 'c', 'pt-br')
                                                            WHEN convert(decimal(10,2), EIT.Prc_Unitario) = '0.00' THEN (SELECT TOP 1 
                                                                                                                                format (Prc_Unitario, 'c', 'pt-br')
                                                                                                                                FROM NFEIT 
                                                                                                                                    inner join NFECB  on NFEIT.Cod_Estabe = NFECB.Cod_Estabe 
                                                                                                                                        and NFEIT.Protocolo = NFECB.Protocolo                                                                                                                                   
                                                                                                                            WHERE NFECB.COD_ESTABE = 1 
                                                                                                                            AND Dat_Entrada NOT IN (SELECT MAX(Dat_Entrada) 
                                                                                                                                                        FROM NFECB 
                                                                                                                                                    WHERE  Prc_Unitario = 0))
                                                        END
                        END,

        */
        Prc_Medio = CASE
                        WHEN convert(decimal(10,2),Prc_CusMedPra) > '0.00' THEN format(Prc_CusMedPra, 'c', 'pt-br') 
                        WHEN convert(decimal(10,2),Prc_CusMedPra) = '0.00' THEN format(EIT.Prc_Unitario, 'c', 'pt-br')  
                        
                    END,
        PXS.Qtd_Fisico,
        Total = CASE
                    WHEN convert (decimal(10,2), Prc_CusMedPra) > '0.00' THEN format((Qtd_Fisico * Prc_CusMedPra), 'c', 'pt-br') 
                    WHEN convert (decimal(10,2), Prc_CusMedPra) = '0.00' THEN format((Qtd_Fisico * EIT.Prc_Unitario), 'c', 'pt-br')
                END
        FROM PRXES PXS 
            INNER JOIN PRODU PR ON pxs.Cod_Produt = pr.Codigo
            INNER JOIN FABRI FB ON pr.Cod_Fabricante = FB.Codigo
            LEFT JOIN (SELECT top 5
                        CB.COD_ESTABE, 
                        IT.COD_PRODUTO , 
                        max(CB.Dat_Entrada) as Dat_Entrada, 
                        IT.Prc_Unitario,
                        cb.Tip_NF
                        FROM NFECB CB 
                            INNER JOIN NFEIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Protocolo = it.Protocolo
                        where cb.Cod_Estabe = 1 and cb.Tip_NF <> 'D'
                        GROUP BY 
                            CB.COD_ESTABE, 
                            IT.COD_PRODUTO , 
                            IT.Prc_Unitario,
                            cb.Tip_NF
                        ) as EIT on PXS.Cod_Estabe = EIT.COD_ESTABE AND PXS.Cod_Produt = EIT.Cod_Produto
    WHERE
        PXS.Cod_Estabe = 1
    AND PXS.Qtd_Fisico > 0
    AND PXS.Flg_Bloqueado = 0
    AND PR.Flag_ImprClassif1 <> 'N'
    AND PR.CODIGO = 15231

    GROUP BY   
        pr.Codigo,
        pr.Descri,
        pr.Unidade_Venda,
        fb.Fantasia,
        PXS.Qtd_Fisico,
        Prc_UltEnt,
        Prc_Unitario,
        Prc_CusMedPra,
        Qtd_Quaren,
        Qtd_Reserv,
        eit.Dat_Entrada

    ORDER BY PR.Codigo
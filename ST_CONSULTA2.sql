
SELECT * FROM VW_QTDSAIPROD101ANO


SELECT top 1 Descri, max(Atual) as Atual, Antes FROM VW_QTDSAIPROD101ANO group by Descri, Antes order by 2 desc

CREATE VIEW VW_QTDSAIPROD101ANO AS

SELECT 
                                    it.Cod_Produto,
                                    Descri,
                                    MAX(IT.qtd) AS Atual,
                                    Isnull(MAX(ANT.qtd), 0) AS Antes
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
                                LEFT JOIN (        SELECT 
                                                        Cod_Produto, 
                                                        SUM(Qtd_ImpFat) AS Qtd, 
                                                        it.Cod_Estabe, 
                                                        it.ser_nota
                                                    FROM NFSIT it
                                                            INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe 
                                                            AND IT.Ser_Nota = CB.Ser_Nota 
                                                    AND IT.Num_Nota = CB.Num_Nota
                                                    WHERE 
                                                        it.cod_estabe = 1 
														--AND Cod_Produto = 22863
                                                        AND cod_cfo IN (5102, 5405)
                                                        AND dat_emissao >= getdate() - 375 
                                                        AND dat_emissao <= getdate() - 365 
                                                    GROUP BY 
                                                        Cod_Produto,
                                                        it.Cod_Estabe, 
                                                        it.ser_nota
                                        ) ANT on pr.Codigo = ant.Cod_Produto and cb.Cod_Estabe = ant.Cod_Estabe
                            WHERE 
                                IT.Cod_Estabe = 1 
                                AND dat_emissao >= getdate() - 11 
                                AND dat_emissao <= getdate()
                            GROUP BY 
                                it.Cod_Produto,
                                Descri

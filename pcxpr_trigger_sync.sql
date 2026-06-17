CREATE OR ALTER TRIGGER dbo.TRG_PCXPR_SYNC_3015
ON dbo.PCXPR
AFTER INSERT, UPDATE
AS
BEGIN
    SET NOCOUNT ON;

    /*
        Evita loop quando a própria trigger atualizar a política 3015.
    */
    IF TRIGGER_NESTLEVEL() > 1
        RETURN;

    /*
        Só executa se alguma das colunas comerciais for alterada/inserida.
    */
    IF NOT (
           UPDATE(Cod_Produt)
        OR UPDATE(Qtd_Minimo)
        OR UPDATE(Qtd_PrzMax)
        OR UPDATE(Per_Descon)
        OR UPDATE(Per_DscVis)
        OR UPDATE(Qtd_Min2)
        OR UPDATE(Per_Dsc2)
        OR UPDATE(Qtd_Min3)
        OR UPDATE(Per_Dsc3)
        OR UPDATE(Qtd_Min4)
        OR UPDATE(Per_Dsc4)
        OR UPDATE(Qtd_Min5)
        OR UPDATE(Per_Dsc5)
        OR UPDATE(Prc_Promoc)
        OR UPDATE(Per_DscVis2)
        OR UPDATE(Per_DscVis3)
        OR UPDATE(Per_DscVis4)
        OR UPDATE(Per_DscVis5)
        OR UPDATE(Qtd_Maximo)
        OR UPDATE(Qtd_Max2)
        OR UPDATE(Qtd_Max3)
        OR UPDATE(Qtd_Max4)
        OR UPDATE(Qtd_Max5)
        OR UPDATE(Tip_Sai)
        OR UPDATE(Per_Bonifi)
        OR UPDATE(Per_MkpPrdVis)
        OR UPDATE(Per_MkpPrdPrz)
        OR UPDATE(IndEstabeMkp)
        OR UPDATE(IndPrcCusBasMkp)
        OR UPDATE(Per_ComVdr)
        OR UPDATE(Per_ComOpe)
        OR UPDATE(Qtd_Fixo)
        OR UPDATE(Vlr_Minimo)
        OR UPDATE(Vlr_Min2)
        OR UPDATE(Vlr_Min3)
        OR UPDATE(Vlr_Min4)
        OR UPDATE(Vlr_Min5)
        OR UPDATE(Flg_Obrigatorio)
        OR UPDATE(Prc_RecFor)
    )
        RETURN;

    ;WITH ProdutosAlterados AS (
        SELECT
            I.Id_PolCom,
            I.Cod_Produt,

            I.Qtd_Minimo,
            I.Qtd_PrzMax,
            I.Per_Descon,
            I.Per_DscVis,
            I.Qtd_Min2,
            I.Per_Dsc2,
            I.Qtd_Min3,
            I.Per_Dsc3,
            I.Qtd_Min4,
            I.Per_Dsc4,
            I.Qtd_Min5,
            I.Per_Dsc5,
            I.Prc_Promoc,
            I.Per_DscVis2,
            I.Per_DscVis3,
            I.Per_DscVis4,
            I.Per_DscVis5,
            I.Qtd_Maximo,
            I.Qtd_Max2,
            I.Qtd_Max3,
            I.Qtd_Max4,
            I.Qtd_Max5,
            I.Tip_Sai,
            I.Per_Bonifi,
            I.Per_MkpPrdVis,
            I.Per_MkpPrdPrz,
            I.IndEstabeMkp,
            I.IndPrcCusBasMkp,
            I.Per_ComVdr,
            I.Per_ComOpe,
            I.Qtd_Fixo,
            I.Vlr_Minimo,
            I.Vlr_Min2,
            I.Vlr_Min3,
            I.Vlr_Min4,
            I.Vlr_Min5,
            I.Flg_Obrigatorio,
            I.Prc_RecFor,

            Prioridade =
                CASE 
                    WHEN I.Id_PolCom = 3712 THEN 1
                    WHEN I.Id_PolCom = 2662 THEN 2
                    WHEN I.Id_PolCom = 2854 THEN 3
                END
        FROM inserted I
        WHERE I.Id_PolCom IN (3712, 2662, 2854)
    ),

    RegrasAplicaveis AS (
        SELECT
            PA.*
        FROM ProdutosAlterados PA
        WHERE
            PA.Id_PolCom = 3712

            OR (
                PA.Id_PolCom = 2662
                AND NOT EXISTS (
                    SELECT 1
                    FROM dbo.PCXPR P3712
                    WHERE P3712.Id_PolCom = 3712
                      AND P3712.Cod_Produt = PA.Cod_Produt
                )
            )

            OR (
                PA.Id_PolCom = 2854
                AND NOT EXISTS (
                    SELECT 1
                    FROM dbo.PCXPR P3712
                    WHERE P3712.Id_PolCom = 3712
                      AND P3712.Cod_Produt = PA.Cod_Produt
                )
                AND NOT EXISTS (
                    SELECT 1
                    FROM dbo.PCXPR P2662
                    WHERE P2662.Id_PolCom = 2662
                      AND P2662.Cod_Produt = PA.Cod_Produt
                )
            )
    ),

    PoliticaPrioritaria AS (
        SELECT
            RA.*,
            ROW_NUMBER() OVER (
                PARTITION BY RA.Cod_Produt
                ORDER BY RA.Prioridade
            ) AS RN
        FROM RegrasAplicaveis RA
    )

    UPDATE P3015
        SET
            P3015.Qtd_Minimo        = PP.Qtd_Minimo,
            P3015.Qtd_PrzMax        = PP.Qtd_PrzMax,
            P3015.Per_Descon        = PP.Per_Descon,
            P3015.Per_DscVis        = PP.Per_DscVis,
            P3015.Qtd_Min2          = PP.Qtd_Min2,
            P3015.Per_Dsc2          = PP.Per_Dsc2,
            P3015.Qtd_Min3          = PP.Qtd_Min3,
            P3015.Per_Dsc3          = PP.Per_Dsc3,
            P3015.Qtd_Min4          = PP.Qtd_Min4,
            P3015.Per_Dsc4          = PP.Per_Dsc4,
            P3015.Qtd_Min5          = PP.Qtd_Min5,
            P3015.Per_Dsc5          = PP.Per_Dsc5,
            P3015.Prc_Promoc        = PP.Prc_Promoc,
            P3015.Per_DscVis2       = PP.Per_DscVis2,
            P3015.Per_DscVis3       = PP.Per_DscVis3,
            P3015.Per_DscVis4       = PP.Per_DscVis4,
            P3015.Per_DscVis5       = PP.Per_DscVis5,
            P3015.Qtd_Maximo        = PP.Qtd_Maximo,
            P3015.Qtd_Max2          = PP.Qtd_Max2,
            P3015.Qtd_Max3          = PP.Qtd_Max3,
            P3015.Qtd_Max4          = PP.Qtd_Max4,
            P3015.Qtd_Max5          = PP.Qtd_Max5,
            P3015.Tip_Sai           = PP.Tip_Sai,
            P3015.Per_Bonifi        = PP.Per_Bonifi,
            P3015.Per_MkpPrdVis     = PP.Per_MkpPrdVis,
            P3015.Per_MkpPrdPrz     = PP.Per_MkpPrdPrz,
            P3015.IndEstabeMkp      = PP.IndEstabeMkp,
            P3015.IndPrcCusBasMkp   = PP.IndPrcCusBasMkp,
            P3015.Per_ComVdr        = PP.Per_ComVdr,
            P3015.Per_ComOpe        = PP.Per_ComOpe,
            P3015.Qtd_Fixo          = PP.Qtd_Fixo,
            P3015.Vlr_Minimo        = PP.Vlr_Minimo,
            P3015.Vlr_Min2          = PP.Vlr_Min2,
            P3015.Vlr_Min3          = PP.Vlr_Min3,
            P3015.Vlr_Min4          = PP.Vlr_Min4,
            P3015.Vlr_Min5          = PP.Vlr_Min5,
            P3015.Flg_Obrigatorio   = PP.Flg_Obrigatorio,
            P3015.Prc_RecFor        = PP.Prc_RecFor
    FROM dbo.PCXPR P3015
    INNER JOIN PoliticaPrioritaria PP
        ON PP.Cod_Produt = P3015.Cod_Produt
       AND PP.RN = 1
    WHERE P3015.Id_PolCom = 3015;

END;
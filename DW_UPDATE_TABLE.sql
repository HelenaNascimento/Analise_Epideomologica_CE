-- Atualizar dados das Tabelas

--Tabela Cliente

    IF (select top 1 1 from [TESTE].[dbo].[CLIEN] where Transacao > (GETDATE()-30)) = 1
        BEGIN TRAN
            UPDATE dwcl
            SET LicSaude = cl.Licenca_Saude, 
                ValLicSaude = cl.Val_LicSau, 
                Anvisa = cl.Num_Anvisa, 
                ValAnvisa = cl.Val_Anvisa, 
                CRF_CRM = cl.Num_CerReg, 
                ValCRF = cl.Val_CerReg, 
                AlvFunc = cl.Num_AlvFun, 
                ValAlv = cl.Val_AlvFun, 
                Limite_Credito = cl.Limite_Credito,  
                Dat_UltComp = cl.Data_UltimaFatura, 
                Dat_LimCreAtu = cl.Dat_LimCreAtu, 
                Vlr_LimCreAnt = cl.Vlr_LimCreAnt, 
                Total_Debito = cl.Total_Debito
            FROM [TESTE_DW].[dbo].[CLIENTE] dwcl
                INNER JOIN [TESTE].[dbo].[CLIEN] cl ON dwcl.IdCli = cl.Codigo 
            WHERE
                dwcl.IdCli = cl.Codigo AND 
                (dwcl.LicSaude <> cl.Licenca_Saude
                    OR dwcl.ValLicSaude <> cl.Val_LicSau 
                    OR dwcl.Anvisa <> cl.Num_Anvisa
                    OR dwcl.ValAnvisa <> cl.Val_Anvisa 
                    OR dwcl.CRF_CRM <> cl.Num_CerReg
                    OR dwcl.ValCRF <> cl.Val_CerReg
                    OR dwcl.AlvFunc <> cl.Num_AlvFun
                    OR dwcl.ValAlv <> cl.Val_AlvFun
                    OR dwcl.Limite_Credito = cl.Limite_Credito
                    OR dwcl.Dat_UltComp = cl.Data_UltimaFatura 
                    OR dwcl.Dat_LimCreAtu = cl.Dat_LimCreAtu
                    OR dwcl.Vlr_LimCreAnt = cl.Dat_LimCreAtu
                    OR dwcl.Total_Debito = cl.Total_Debito)
        COMMIT TRAN
    GO

--Tabela Fabricante

    IF (select top 1 1 from [TESTE].[dbo].[FABRI] where Transacao > (GETDATE()-30)) = 1

            BEGIN TRAN
                UPDATE dwfb
                SET
                        Sta_ClaAbcVal = FB.Sta_ClaAbcVal,
                        Per_ParticFat =  FB.Per_ParticFat,
                        Qtd_PrzMaxFat = FB.Qtd_PrzMaxFat,
                        Per_DscMaxVis = FB.Per_DscMaxVis,
                        Per_DscMaxPrz = FB.Per_DscMaxPrz,
                        Per_DscBasComNor = FB.Per_DscBasComNor,
                        Per_DscBasTax = FB.Per_DscBasTax,
                        Flg_Desconto = FB.Flg_Desconto,
                        Flg_BlqInfPar = FB.Flg_BlqInfPar,
                        Flg_TransmPro =  ES.Flg_TransmPro,
                        Flg_TransmItx = ES.Flg_TransmItx,
                        Flg_TransmGnx =  ES.Flg_TransmGnx,
                        Flg_TransmNeo = ES.Flg_TransmNeo,
                        Flg_BlqIms =  ES.Flg_BlqIms,
                        Flg_TransmMtr = ES.Flg_TransmMtr,
                        Flg_TransmHyp = ES.Flg_TransmHyp,
                        Flg_BlqCloseUp = ES.Flg_BlqCloseUp,
                        Cod_ForPref = ES.Cod_ForPref
                    FROM [TESTE_DW].[dbo].[FABRIANT] dwfb
                            INNER JOIN [TESTE].[dbo].[FABRI] fb ON dwfb.IdFab = fb.Codigo 
                            INNER JOIN [TESTE].[dbo].[FBXES] ES ON FB.Codigo = ES.Cod_Fabric
                    WHERE ES.Cod_Estabe = 1
                    AND FB.Bloqueado = 0
                    AND  dwfb.IdCli = fb.Codigo
                    AND (dwfb.Sta_ClaAbcVal <> FB.Sta_ClaAbcVal OR
                            dwfb.Per_ParticFat <>  FB.Per_ParticFat OR
                            dwfb.Qtd_PrzMaxFat <> FB.Qtd_PrzMaxFat OR
                            dwfb.Per_DscMaxVis <> FB.Per_DscMaxVis OR
                            dwfb.Per_DscMaxPrz <> FB.Per_DscMaxPrz OR
                            dwfb.Per_DscBasComNor <> FB.Per_DscBasComNor OR
                            dwfb.Per_DscBasTax <> FB.Per_DscBasTax OR
                            dwfb.Flg_Desconto <> FB.Flg_Desconto OR
                            dwfb.Flg_BlqInfPar <> FB.Flg_BlqInfPar OR
                            dwfb.Flg_TransmPro <>  ES.Flg_TransmPro OR
                            dwfb.Flg_TransmItx <> ES.Flg_TransmItx OR
                            dwfb.Flg_TransmGnx <>  ES.Flg_TransmGnx OR
                            dwfb.Flg_TransmNeo <> ES.Flg_TransmNeo OR 
                            dwfb.Flg_BlqIms <>  ES.Flg_BlqIms OR 
                            dwfb.Flg_TransmMtr <> ES.Flg_TransmMtr OR 
                            dwfb.Flg_TransmHyp <> ES.Flg_TransmHyp OR 
                            dwfb.Flg_BlqCloseUp <> ES.Flg_BlqCloseUp OR
                            dwfb.Cod_ForPref <> ES.Cod_ForPref)
            COMMIT TRAN
    GO

--Tabela Vendedor:

    IF (select top 1 1 from [TESTE].[dbo].[VENDE] where Transacao > (GETDATE()-30)) = 1

        BEGIN TRAN
                UPDATE  dwve
                SET Data_Saida = ve.Data_Saida,
                    Area_Atuacao = ve.Area_Atuacao,
                    Bloqueado = ve.Bloqueado,
                    dwve.Cod_TipVenBas = ve.Cod_TipVenBas,
                    Vlr_Obj_vend = ve.Vlr_Objetivo,
                    Isn_CtaFin_vend = ve.Isn_CtaFin,
                    Bairro = ve.Bairro,
                    Cidade = ve.Cidade,
                    Estado = ve.Estado,
                    Flg_Export = ve.Flg_Export,
                    Cod_Supervisor = ve.Cod_Supervisor,
                    Supervisor = su.Nome_Completo ,
                    Cod_Gerencia = ve.Cod_Gerencia,
                    Gerente = ge.Nome_Completo,
                    Vlr_Obj_geren = ge.Vlr_Objetivo,
                    Vlr_ComFixa_geren = ge.Vlr_ComFixa,
                    Isn_CtaFin_geren = ge.Isn_CtaFin,
                    Cod_TabCom = es.Cod_TabCom
                    FROM TESTE_DW.dbo.vendedor dwve
                        INNER JOIN TESTE.dbo.VENDE ve ON dwve.idVen = ve.Codigo
                        INNER JOIN TESTE.dbo.SUPER SU ON VE.Cod_Supervisor = SU.Codigo
                        INNER JOIN TESTE.dbo.GEREN GE ON VE.Cod_Gerencia = GE.Codigo
                        INNER JOIN TESTE.dbo.VEXES es ON ve.Codigo = es.Cod_Vended
                WHERE ES.Cod_Estabe = 1
                    AND (Data_Saida <> ve.Data_Saida OR
                    Area_Atuacao <> ve.Area_Atuacao OR
                    Bloqueado <> ve.Bloqueado OR
                    dwve.Cod_TipVenBas <> ve.Cod_TipVenBas OR 
                    Vlr_Objetivo <> ve.Vlr_Objetivo OR 
                    Bairro <> ve.Bairro OR 
                    Cidade <> ve.Cidade OR 
                    Estado <> ve.Estado OR 
                    Flg_Export <> ve.Flg_Export OR
                    Cod_Gerencia <> ve.Cod_Gerencia OR
                    Cod_Supervisor <> ve.Cod_Supervisor OR
                    Isn_CtaFin <> ve.Isn_CtaFin OR
                    Cod_TabCom <> es.Cod_TabCom)
        COMMIT TRAN
    GO

--Tabela Produto
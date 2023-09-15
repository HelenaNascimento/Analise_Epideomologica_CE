-- INSERIR NOVOS DADOS NO DW

--Tabela Cliente:

IF (SELECT COUNT(*) FROM [TESTE_DW].[dbo].[CLIENTE]) < (SELECT COUNT(*) FROM [TESTE].[dbo].[CLIEN]) 

	BEGIN TRAN
			INSERT INTO TESTE_DW.dbo.CLIENTE (
												IdCli, 
												DesCli, 
												Dat_Cadastro,
												Telefone, 
												UF, 
												Cidade, 
												Bairro, 
												Pessoa, 
												TipCons, 
												LicSaude, 
												ValLicSaude, 
												Anvisa, 
												ValAnvisa, 
												CRF_CRM, 
												ValCRF, 
												AlvFunc, 
												ValAlv, 
												Limite_Credito,  
												Dat_UltComp, 
												Dat_LimCreAtu, 
												Vlr_LimCreAnt, 
												Total_Debito, 
												Suframa, 
												Cod_RamoAtividade)
												SELECT 
													Codigo,
													Razao_Social,
													Data_Cadastro,
													Fone1,
													Cod_Estado,
													Cod_Cidade,
													cod_Bairro,
													Pessoa,
													Tipo_Consumidor,
													Licenca_Saude,
													Val_LicSau,
													Num_Anvisa,
													Val_Anvisa,
													Num_RegCrm,
													Val_CerReg,
													Num_AlvFun,
													Val_AlvFun,
													Limite_Credito,  
													cl.Data_UltimaFatura, 
													Dat_LimCreAtu, 
													Vlr_LimCreAnt, 
													Total_Debito, 
													Inscricao_SUFRAMA,
													Cod_RamoAtividade
													FROM TESTE.DBO.CLIEN cl
														INNER JOIN TESTE.DBO.ENXES es on cl.codigo = es.cod_client
												where bloqueado = 0 
												and Motivo_Bloqueio = ''
												and es.cod_estabe = 1
												and Codigo > (select max(IdCli) from [TESTE_DW].[dbo].[CLIENTE])
	COMMIT TRAN
GO

--Tabela Fabricante:

IF (SELECT COUNT(*) FROM [TESTE_DW].[dbo].[FABRICANT]) < (SELECT COUNT(*) FROM [TESTE].[dbo].[FABRI]) 

    BEGIN TRAN

        INSERT INTO TESTE_DW.dbo.FABRICANT(Codigo, Fantasia, Cgc_Cpf, Sta_ClaAbcVal, Per_ParticFat, Qtd_PrzMaxFat, Per_DscMaxVis, Per_DscMaxPrz, Per_DscBasComNor, Per_DscBasTax, Flg_Desconto, Flg_BlqInfPar,
										Flg_TransmPro, Flg_TransmItx, Flg_TransmGnx, Flg_TransmNeo, Flg_BlqIms, Flg_TransmMtr, Flg_TransmHyp, Flg_BlqCloseUp, Cod_ForPref)
            select 
                DISTINCT
                FB.Codigo,
                FB.Fantasia,
                FB.Cgc_Cpf,
                FB.Sta_ClaAbcVal,
                FB.Per_ParticFat,
                FB.Qtd_PrzMaxFat,
                FB.Per_DscMaxVis,
                FB.Per_DscMaxPrz,
                FB.Per_DscBasComNor,
                FB.Per_DscBasTax,
                FB.Flg_Desconto,
                FB.Flg_BlqInfPar,
                ES.Flg_TransmPro,
                ES.Flg_TransmItx,
                ES.Flg_TransmGnx,
                ES.Flg_TransmNeo,
                ES.Flg_BlqIms,
                ES.Flg_TransmMtr,
                ES.Flg_TransmHyp,
                ES.Flg_BlqCloseUp,
                ES.Cod_ForPref
                from TESTE.dbo.FABRI FB
                    INNER JOIN TESTE.dbo.FBXES ES ON FB.Codigo = ES.Cod_Fabric
            WHERE Cod_Estabe = 1 
                AND Bloqueado = 0
                AND Codigo > (select max(IdCli) from [TESTE_DW].[dbo].[FABRICANT])
            ORDER BY FB.Codigo

    COMMIT
GO

--Tabela Vendedor:

IF (SELECT COUNT(*) FROM [TESTE_DW].[dbo].[VENDEDOR]) < (SELECT COUNT(*) FROM [TESTE].[dbo].[VENDE ]) 
	BEGIN TRAN
			INSERT INTO TESTE_DW.dbo.VENDEDOR (	IdVen,
									Nome_Completo ,
									Nome_Guerra ,
									Data_Admissao ,
									Data_Saida ,
									Area_Atuacao ,
									Bloqueado ,
									Cod_TipVenBas ,
									Vlr_Obj_vend ,
									Isn_CtaFin_vend,
									Bairro ,
									Cidade ,
									UF,
									Flg_Export ,
									Transacao ,
									Cod_Supervisor,
									Supervisor ,
									Cod_Gerencia ,
									Gerente,
									Vlr_Obj_geren,
									Vlr_ComFixa_geren,
									Isn_CtaFin_geren,
									Cod_TabCom )
			select 
					ve.Codigo,
					ve.Nome_Completo ,
					ve.Nome_Guerra ,
					ve.Data_Admissao ,
					ve.Data_Saida ,
					ve.Area_Atuacao ,
					ve.Bloqueado ,
					ve.Cod_TipVenBas,
					ve.Vlr_Objetivo ,
					ve.Isn_CtaFin,
					ve.Bairro ,
					ve.Cidade ,
					ve.Estado,
					ve.Flg_Export ,
					ve.Transacao ,
					ve.Cod_Supervisor,
					su.Nome_Completo ,
					ve.Cod_Gerencia ,
					ge.Nome_Completo,
					ge.Vlr_Objetivo,
					ge.Vlr_ComFixa,
					ge.Isn_CtaFin,
					es.Cod_TabCom 
				from TESTE.dbo.VENDE VE
					inner join TESTE.dbo.SUPER SU ON VE.Cod_Supervisor = SU.Codigo
					inner join TESTE.dbo.GEREN GE	ON VE.Cod_Gerencia = GE.Codigo
					inner join TESTE.dbo.VEXES es on ve.Codigo = es.Cod_Vended
			where es.Cod_Estabe = 1
			    AND Bloqueado = 0
                AND Codigo > (select max(IdVen) from [TESTE_DW].[dbo].[VENDE])
            ORDER BY ve.Codigo


	COMMIT TRAN
GO
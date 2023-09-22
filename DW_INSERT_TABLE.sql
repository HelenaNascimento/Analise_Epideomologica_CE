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

--Tabela Produto:

IF (SELECT COUNT(*) FROM [TESTE_DW].[dbo].[PRODUTO]) < (SELECT COUNT(*) FROM [TESTE].[dbo].[PRODU]) 
	BEGIN TRAN
			INSERT INTO teste_dw.dbo.PRODUTO (
											IdProd,
											Descricao,
											Unidade_Venda,
											Ref_Fabricante,
											Localizacao,
											Ctrl_Preco,
											Ctrl_Venda,
											Cod_Fabricante,
											Cod_EAN,
											Cod_AbcFar,
											Cod_GrpPrc,
											Cod_Promocao,
											Cod_PlaRegTri,
											Cod_PrdExt,
											Cod_Classif,
											Cod_CtrUsu,
											Cod_Estabe,
											Cod_RefPrati,
											Cod_OriMer,
											Validade,
											Dat_Cadastro,
											Dat_UltVenda ,
											Dat_PrcFabAnt,
											Dat_PrcAtual,
											Dat_UltCompra ,
											Dat_PrcFab,
											Dat_UltVen,
											Dat_PrxVctLtl,
											Dat_PrxVctLot,
											Dat_EntAntDep,
											Dat_UltEntDep ,
											Sta_AbcValFat,
											Sta_AbcUniVen,
											Tipo,
											Tip_Prd,
											Tip_Rentab,
											Tip_BlqLuc,
											Tip_Por344,
											Tip_LisPis,
											Alq_Ipi,
											Dias_PlaFinanc,
											Taxa_PlaFinanc,
											Des_PrdDet,
											Des_PrdRef,
											Edi_Pra,
											Edi_Dep,
											Lrg_Emb,
											Alt_Emb,
											Prf_Emb,
											Vol_Emb,
											Und_EmbCmp,
											Lrg_EmbCmp,
											Alt_EmbCmp,
											Prf_EmbCmp, 
											Pes_EmbCmp,
											Flag_ImprClassif1,
											Flg_PlaFatPrcLiq ,
											Flg_PolComercMax ,
											Flg_PrzComercMax,
											Flg_Generico,
											Flg_IncFis,
											Flg_BlqDsc,
											Flg_BlqVen,
											Flg_BlqCmp,
											Flg_BlqInfVen,
											Flg_BlqInfPar,
											Flg_BlqCot,
											Flg_BlqPrp,
											Flg_BlqCfv,
											Flg_Bloqueado,
											Flg_CusMedComRentab,
											Flg_RegSbtEsp
											)
											SELECT
												Codigo,
												Descricao,
												Unidade_Venda,
												Ref_Fabricante,
												Localizacao,
												Ctrl_Preco,
												Ctrl_Venda,
												Cod_Fabricante,
												Cod_EAN,
												Cod_AbcFar,
												Cod_GrpPrc,
												es.Cod_Promocao,
												Cod_PlaRegTri,
												Cod_PrdExt,
												Cod_Classif,
												Cod_CtrUsu,
												Cod_Estabe,
												Cod_RefPrati,
												Cod_OriMer,
												Validade,
												Dat_Cadastro,
												Dat_UltVenda ,
												Dat_PrcFabAnt,
												Dat_PrcAtual,
												Dat_UltCompra ,
												Dat_PrcFab,
												Dat_UltVen,
												Dat_PrxVctLtl,
												Dat_PrxVctLot,
												Dat_EntAntDep,
												Dat_UltEntDep ,
												es.Sta_AbcValFat,
												es.Sta_AbcUniVen,
												Tipo,
												Tip_Prd,
												Tip_Rentab,
												Tip_BlqLuc,
												Tip_Por344,
												Tip_LisPis,
												Alq_Ipi,
												es.Dias_PlaFinanc,
												es.Taxa_PlaFinanc,
												Des_PrdDet,
												Des_PrdRef,
												Edi_Pra,
												Edi_Dep,
												Lrg_Emb,
												Alt_Emb,
												Prf_Emb,
												Vol_Emb,
												Und_EmbCmp,
												Lrg_EmbCmp,
												Alt_EmbCmp,
												Prf_EmbCmp, 
												Pes_EmbCmp,
												Flag_ImprClassif1,
												es.Flg_PlaFatPrcLiq ,
												Flg_PolComercMax ,
												Flg_PrzComercMax,
												Flg_Generico,
												Flg_IncFis,
												Flg_BlqDsc,
												Flg_BlqVen,
												Flg_BlqCmp,
												Flg_BlqInfVen,
												Flg_BlqInfPar,
												Flg_BlqCot,
												Flg_BlqPrp,
												Flg_BlqCfv,
												Flg_Bloqueado,
												Flg_CusMedComRentab,
												Flg_RegSbtEsp
											From teste.dbo.PRODU pr 
												inner join teste.dbo.PRXES es on pr.codigo = es.Cod_Produt
											where Cod_Estabe = 1

	COMMIT TRAN
GO

-- Tabela Movimentação de Estoque

INSERT INTO TESTE_DW.DBO.MOVESTO(
		IdProd,
		Cod_Lote,
		Dat_UltCompra,
		Dat_UltVenda,
		Dat_Vencim,
		Cod_Etique,
		Cod_Dep,
		Num_Rua,
		Num_Col,
		Num_Niv,
		Num_Apt	,
		Qtd_Fisico,
		Qtd_Solici,
		Qtd_Avaria,
		Qtd_Transi,
		Qtd_Reserv,
		Qtd_Quaren,
		Qtd_Embalagem,
		Qtd_SldAntBal,
		Qtd_SldPosBal,
		Qtd_PrmFisico,
		Qtd_PrmDispon,
		Qtd_PrmSolici,
		Qtd_EntAnt,
		Qtd_EstMin,
		Qtd_EstMinCfg,
		Qtd_EstMax,
		Qtd_EstMaxCfg,
		Qtd_UltEnt,
		Qtd_EntAntDep,
		Qtd_UltEntDep,
		Qtd_Pulmao,
		Qtd_EstTraMan,
		Qtd_DiaSupPrd,
		Qtd_CanCompra,
		Qtd_PrzMaxFat,
		Qtd_CngCli,
		Qtd_MesAleRec
)

SELECT	DISTINCT
		Codigo,
		LT.Cod_Lote,
		Dat_UltCompra,
		Dat_UltVenda,
		LT.Dat_Vencim,
		Cod_Etique,
		TL.Cod_Dep,
		Num_Rua,
		Num_Col,
		Num_Niv,
		Num_Apt	,
		LT.Qtd_Fisico,
		Qtd_Solici,
		Qtd_Avaria,
		Qtd_Transi,
		Qtd_Reserv,
		Qtd_Quaren,
		Qtd_Embalagem,
		Qtd_SldAntBal,
		Qtd_SldPosBal,
		Qtd_PrmFisico,
		Qtd_PrmDispon,
		Qtd_PrmSolici,
		Qtd_EntAnt,
		Qtd_EstMin,
		Qtd_EstMinCfg,
		Qtd_EstMax,
		Qtd_EstMaxCfg,
		Qtd_UltEnt,
		Qtd_EntAntDep,
		Qtd_UltEntDep,
		Qtd_Pulmao,
		Qtd_EstTraMan,
		Qtd_DiaSupPrd,
		Qtd_CanCompra,
		Qtd_PrzMaxFat,
		Qtd_CngCli,
		Qtd_MesAleRec
FROM TESTE.DBO.PRODU PR
		INNER JOIN TESTE.DBO.PRXES ES ON PR.Codigo = ES.Cod_Produt
		INNER JOIN TESTE.DBO.PRLOT LT ON ES.Cod_Produt = LT.Cod_Produt AND ES.Cod_Estabe = LT.Cod_Estabe
		INNER JOIN TESTE.DBO.PRLTL TL ON PR.Codigo = TL.Cod_Produt AND ES.Cod_Estabe = TL.Cod_Estabe
WHERE ES.Cod_Estabe = 1 AND Codigo > 0
ORDER BY PR.Codigo
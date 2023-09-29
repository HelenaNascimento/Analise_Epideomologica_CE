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
												AND Codigo > (SELECT max(IdProd) From TESTE_DW.DBO.PRODUTO)

	COMMIT TRAN
GO

-- Tabela Movimentação de Estoque

IF (SELECT COUNT(*) FROM [TESTE_DW].[dbo].[MOVESTO]) < (SELECT COUNT(*) FROM [TESTE].[dbo].[PRXES])

	BEGIN TRAN
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
		WHERE ES.Cod_Estabe = 1 
			AND Codigo > (SELECT max(IdProd) From TESTE_DW.DBO.MOVESTO)

	COMMIT TRAN
GO

-- Tabela Politica X Operador X Vendedor X Supervisor

IF (SELECT TOP 1 1 FROM [teste].[dbo].[PCXOP] POP INNER JOIN[teste].[dbo].[PCXES] PES ON POP.Id_PolCom = PES.Id_PolCom WHERE PES.COD_ESTABE = 1 AND Cod_Vended NOT IN (SELECT CODIGO FROM [TESTE_DW].[DBO].[POLXVEND] WHERE flg_oper =1)) > 0

		INSERT INTO [TESTE_DW].[DBO].[POLXVEND]
		SELECT 
			Cod_Vended,
			POP.Id_PolCom,
			Per_VrbPedAtivo,
			Per_VrbPedReceptivo,
			Per_VrbPedMovel,
			Per_VrbPedEletronico,
			Per_VrbPedWeb,
			Per_VrbPedHospitalar,
			Per_VrbPedCotacao,
			Flg_BlqDebVrbSup = '',
			flg_oper =1,
			flg_vende = 0, 
			flg_sup = 0
		FROM [teste].[dbo].[PCXOP] POP
			INNER JOIN 	[teste].[dbo].[PCXES] PES ON POP.Id_PolCom = PES.Id_PolCom
		where PES.COD_ESTABE = 1 
			AND	Cod_Vended > (select max(codigo) from [TESTE_DW].[DBO].[POLXVEND] where flg_oper =1)

IF (SELECT TOP 1 1 FROM [teste].[dbo].[PCXVE] PCV INNER JOIN[teste].[dbo].[PCXES] PES ON PCV.Id_PolCom = PES.Id_PolCom WHERE PES.COD_ESTABE = 1 AND Cod_Vended NOT IN (SELECT CODIGO FROM [TESTE_DW].[DBO].[POLXVEND] WHERE flg_vende = 1)) > 0

		SELECT 
			Cod_Vended,
			PCV.Id_PolCom,
			Per_VrbPedAtivo,
			Per_VrbPedReceptivo,
			Per_VrbPedMovel,
			Per_VrbPedEletronico,
			Per_VrbPedWeb,
			Per_VrbPedHospitalar,
			Per_VrbPedCotacao,
			'',
			flg_oper = 0,
			flg_vende = 1, 
			flg_sup = 0
		FROM [teste].[dbo].[PCXVE] PCV
			INNER JOIN 	[teste].[dbo].[PCXES] PES ON PCV.Id_PolCom = PES.Id_PolCom
		where PES.COD_ESTABE = 1 
			AND Cod_Vended > (select max(codigo) from [TESTE_DW].[DBO].[POLXVEND] where flg_vende = 1)

IF (SELECT TOP 1 1 FROM [teste].[dbo].[PCXSU] PCU INNER JOIN[teste].[dbo].[PCXES] PES ON PCU.Id_PolCom = PES.Id_PolCom WHERE PES.COD_ESTABE = 1 AND Cod_Super NOT IN (SELECT CODIGO FROM [TESTE_DW].[DBO].[POLXVEND] WHERE flg_sup = 1)) > 0


		SELECT 
			Cod_Super,
			PCU.Id_PolCom,
			Per_VrbPedAtivo,
			Per_VrbPedReceptivo,
			Per_VrbPedMovel,
			Per_VrbPedEletronico,
			Per_VrbPedWeb,
			Per_VrbPedHospitalar,
			Per_VrbPedCotacao,
			Flg_BlqDebVrbSup,
			flg_Oper = 0,
			flg_vende = 0, 
			flg_sup = 1
		FROM [teste].[dbo].[PCXSU] PCU
			INNER JOIN 	[teste].[dbo].[PCXES] PES ON PCU.Id_PolCom = PES.Id_PolCom
		where PES.COD_ESTABE = 1 
			AND Cod_Super > (select max(codigo) from [TESTE_DW].[DBO].[POLXVEND] where flg_sup = 1)
GO

-- Tabela Politica X Cliente

INSERT INTO [TESTE_DW].DBO.PCXCL (Id_PolCom ,
			Cod_Client ,	
			Qtd_PrzMax ,	
			Per_Descon ,	
			Per_DscVis ,	
			CodAnt ,	
			NovoCodigo)

	SELECT distinct 
			pcl.Id_PolCom,
			pcl.Cod_Client,	
			pcl.Qtd_PrzMax,	
			pcl.Per_Descon,	
			pcl.Per_DscVis,	
			pcl.CodAnt,	
			pcl.NovoCodigo
	FROM teste.dbo.PCXCL pcl
			inner join ENXES EES ON PCL.Cod_Client = EES.Cod_Client
			inner join teste.dbo.PCXES pes on pcl.Id_PolCom = pes.Id_PolCom and EES.Cod_Estabe = pes.Cod_Estabe
	WHERE EES.cod_estabe = 1
		and exists (select Id_PolCom from [TESTE_DW].dbo.[POLXVEND])


-- Tabela Politica X Grupo de Cliente

INSERT INTO [TESTE_DW].DBO.PCXGC (Id_PolCom ,
			Cod_Client ,	
			Qtd_PrzMax ,	
			Per_Descon ,	
			Per_DscVis ,	
			CodAnt ,	
			NovoCodigo)

	SELECT distinct 
			pgc.Id_PolCom,
			pgc.Cod_GrpCli,	
			pgc.Qtd_PrzMax,	
			pgc.Per_Descon,	
			pgc.Per_DscVis,	
			pgc.CodAnt,	
			pgc.NovoCodigo
	FROM CLIEN cl
			inner join PCXCL PCL ON CL.Codigo = PCL.Cod_Client
			inner join ENXES EES ON PCL.Cod_Client = EES.Cod_Client
			inner join teste.dbo.PCXGC pgc on pcl.Id_PolCom = pgc.Id_PolCom	AND CL.Cod_GrpCli = pgc.Cod_GrpCli
			inner join teste.dbo.PCXES pes on pcl.Id_PolCom = pes.Id_PolCom and EES.Cod_Estabe = pes.Cod_Estabe
	WHERE EES.cod_estabe = 1
		and exists (select Id_PolCom from [TESTE_DW].dbo.[POLXVEND])


--Tabela Politica X UF

INSERT INTO [TESTE_DW].DBO.PCXUF (Id_PolCom ,
			Cod_Uf ,	
			Qtd_PrzMax ,	
			Per_Descon ,	
			Per_DscVis )

select
	distinct
	puf.Id_PolCom,
	puf.Cod_Uf,
	puf.Qtd_PrzMax,	
	puf.Per_Descon,
	puf.Per_DscVis
	from CLIEN cl 
		inner join PCXCL PCL ON CL.Codigo = PCL.Cod_Client
		inner join ENXES EES ON PCL.Cod_Client = EES.Cod_Client
		inner join teste.dbo.PCXUF puf on pcl.Id_PolCom = puf.Id_PolCom AND CL.Cod_Estado = puf.Cod_Uf	
WHERE EES.Cod_Estabe = 1
	and exists (select Id_PolCom from [TESTE_DW].dbo.[POLXVEND])
order by 1


--Tabela Política X Fabricante

INSERT INTO [TESTE_DW].DBO.PCXFB (Id_PolCom ,
			Cod_Fabric ,	
			Qtd_PrzMax ,	
			Per_Descon ,	
			Per_DscVis )

select 
	distinct
	PFB.Id_PolCom,
	PFB.Cod_Fabric,
	PFB.Qtd_PrzMax,	
	PFB.Per_Descon,
	PFB.Per_DscVis
	from FABRI FB 
		inner join PCXFB PFB ON FB.Codigo = PFB.Cod_Fabric
		inner join FBXES FES ON PFB.Cod_Fabric = FES.Cod_Fabric
		inner join (select Id_PolCom from [TESTE_DW].dbo.[POLXVEND]) DW on pfb.Id_PolCom = dw.Id_PolCom
WHERE FES.Cod_Estabe = 1
order by 1

-- TABELA POLITICA

INSERT INTO [TESTE_DW].DBO.POCOM(Id_PolCom
,	Cod_PolCom
,	Des_Detalhada
,	Dat_Inicio
,	Dat_Termino
,	Bloqueado
,	Qtd_Minimo
,	Vlr_Minimo
,	Per_Desconto
,	Qtd_PrzMax
,	Cod_TipPrz
,	Cod_TabPrc
,	Cod_TabPrz
,	Cod_TabComVdr
,	Cod_TabComOpe
,	Usuario
,	Transacao
,	Flg_Televendas
,	Flg_Cfv
,	Flg_PedEle
,	Flg_Web
,	Qtd_IteMin
,	Per_DscCom
,	Per_DscFin
,	Per_RedComVdr
,	Per_RedComOpe
,	Flg_NegDsc
,	Flg_BlqCli
,	Flg_Balcao
,	Flg_ExcCla
,	Flg_ExcFab
,	Flg_NaoSugDscItePdv
,	Dat_Cadastro
,	Per_DscAutPrc
,	Per_AcrAutPrc
,	Flg_PrcVenPrpPrz
,	Per_CorPrcVenPrpPrz
,	Flg_BlqVerba
,	Flg_BlqPolDifItePdv
,	Flg_BlqPolDifCabPdv
,	Flg_MarkupCusCom
,	Tip_RatBon
,	Per_DscComVis
,	Per_DscFinVis
,	Flg_SugDscGlbPdv
,	Tip_PolCom
,	Flg_DbqUsoVrbPdv
,	Flg_GerVrbPdv
,	Tip_VrbPdv
,	Flg_SugPrzCli
,	Controle_Saldo
,	Flg_AcuVrbPos
,	Flg_AcuVrbNeg
,	Flg_CtrPdvVrbVdr
,	Flg_CtrPdvVrbOpe
,	Per_ComAtvVdr
,	Per_ComPasVdr
,	Per_ComAtvOpe
,	Per_ComPasOpe
,	Obs_PolCom
,	Flg_UsaTabPrcCadCli
,	Dat_Criacao)
SELECT 
	PC.Id_PolCom
,	Cod_PolCom
,	Des_Detalhada
,	Dat_Inicio
,	Dat_Termino
,	Bloqueado
,	Qtd_Minimo
,	Vlr_Minimo
,	Per_Desconto
,	Qtd_PrzMax
,	Cod_TipPrz
,	Cod_TabPrc
,	Cod_TabPrz
,	Cod_TabComVdr
,	Cod_TabComOpe
,	Usuario
,	Transacao
,	Flg_Televendas
,	Flg_Cfv
,	Flg_PedEle
,	Flg_Web
,	Qtd_IteMin
,	Per_DscCom
,	Per_DscFin
,	Per_RedComVdr
,	Per_RedComOpe
,	Flg_NegDsc
,	Flg_BlqCli
,	Flg_Balcao
,	Flg_ExcCla
,	Flg_ExcFab
,	Flg_NaoSugDscItePdv
,	Dat_Cadastro
,	Per_DscAutPrc
,	Per_AcrAutPrc
,	Flg_PrcVenPrpPrz
,	Per_CorPrcVenPrpPrz
,	Flg_BlqVerba
,	Flg_BlqPolDifItePdv
,	Flg_BlqPolDifCabPdv
,	Flg_MarkupCusCom
,	Tip_RatBon
,	Per_DscComVis
,	Per_DscFinVis
,	Flg_SugDscGlbPdv
,	Tip_PolCom
,	Flg_DbqUsoVrbPdv
,	Flg_GerVrbPdv
,	Tip_VrbPdv
,	Flg_SugPrzCli
,	Controle_Saldo
,	Flg_AcuVrbPos
,	Flg_AcuVrbNeg
,	Flg_CtrPdvVrbVdr
,	Flg_CtrPdvVrbOpe
,	Per_ComAtvVdr
,	Per_ComPasVdr
,	Per_ComAtvOpe
,	Per_ComPasOpe
,	Obs_PolCom
,	Flg_UsaTabPrcCadCli
,	Dat_Criacao
FROM [TESTE].DBO.POCOM PC
 INNER JOIN [TESTE].DBO.PCXES PES ON PC.Id_PolCom = PES.Id_PolCom
WHERE PES.Cod_Estabe = 1
	AND EXISTS (select Id_PolCom from [TESTE_DW].dbo.[POLXVEND]) 
ORDER BY 2

--Tabela Politica x Prazo

INSERT INTO TESTE_DW.DBO.PCXPZ (Id_PolCom, Cod_TabPrz, Vlr_MinPdv)
SELECT 
	PPZ.Id_PolCom,
	Cod_TabPrz,
	Vlr_MinPdv
	FROM PCXPZ PPZ
		INNER JOIN PCXES PES ON PPZ.ID_POLCOM = PES.Id_PolCom
WHERE PES.Cod_Estabe = 1 AND 
	EXISTS (SELECT * FROM [TESTE_DW].[DBO].[POCOM])
ORDER BY 2


--Politica X Produto

INSERT INTO TESTE_DW.DBO.PCXPR (
Id_PolCom
,	Cod_Produt
,	Qtd_Minimo
,	Qtd_PrzMax
,	Per_Descon
,	Per_DscVis
,	Qtd_Min2
,	Per_Dsc2
,	Qtd_Min3
,	Per_Dsc3
,	Qtd_Min4
,	Per_Dsc4
,	Qtd_Min5
,	Per_Dsc5
,	Prc_Promoc
,	Per_DscVis2
,	Per_DscVis3
,	Per_DscVis4
,	Per_DscVis5
,	Qtd_Maximo
,	Qtd_Max2
,	Qtd_Max3
,	Qtd_Max4
,	Qtd_Max5
,	Tip_Sai
,	Per_Bonifi
,	Per_MkpPrdVis
,	Per_MkpPrdPrz
,	IndEstabeMkp
,	IndPrcCusBasMkp
)
SELECT 
	PPR.Id_PolCom
,	Cod_Produt
,	Qtd_Minimo
,	Qtd_PrzMax
,	Per_Descon
,	Per_DscVis
,	Qtd_Min2
,	Per_Dsc2
,	Qtd_Min3
,	Per_Dsc3
,	Qtd_Min4
,	Per_Dsc4
,	Qtd_Min5
,	Per_Dsc5
,	Prc_Promoc
,	Per_DscVis2
,	Per_DscVis3
,	Per_DscVis4
,	Per_DscVis5
,	Qtd_Maximo
,	Qtd_Max2
,	Qtd_Max3
,	Qtd_Max4
,	Qtd_Max5
,	Tip_Sai
,	Per_Bonifi
,	Per_MkpPrdVis
,	Per_MkpPrdPrz
,	IndEstabeMkp
,	IndPrcCusBasMkp
	FROM PCXPR PPR
		INNER JOIN PCXES PES ON PPR.ID_POLCOM = PES.Id_PolCom
WHERE PES.Cod_Estabe = 1 AND 
	EXISTS (SELECT Id_PolCom FROM [TESTE_DW].[DBO].[POCOM] WHERE exists (SELECT Id_PolCom FROM [TESTE_DW].[DBO].[PRODUTO]) )
ORDER BY 2, 1


--Tabela PCXPL

INSERT INTO TESTE_DW.DBO.PCXPL ( Id_PolCom, Cod_Produt, Cod_Lote)
SELECT 
	PPL.Id_PolCom, Cod_Produt, Cod_Lote
	FROM PCXPL PPL
WHERE PPL.Cod_Estabe = 1 AND 
	EXISTS (SELECT Id_PolCom FROM [TESTE_DW].[DBO].[POCOM])
	AND EXISTS (SELECT Id_PolCom FROM [TESTE_DW].[DBO].[PRODUTO])
ORDER BY 2, 1

--Tabela PCXCP

INSERT INTO TESTE_DW.DBO.PCXCP (Id_PolCom, Cod_Classi, Qtd_PrzMax,	Per_Descon,	Per_DscVis)
SELECT 
	PCP.Id_PolCom, Cod_Classi, Qtd_PrzMax,	Per_Descon,	Per_DscVis
	FROM PCXCP PCP
		INNER JOIN PCXES PES ON PCP.ID_POLCOM = PES.Id_PolCom
WHERE PES.Cod_Estabe = 1 AND 
	EXISTS (SELECT Id_PolCom FROM [TESTE_DW].[DBO].[POCOM])
ORDER BY 2, 1
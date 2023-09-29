--Criar Tabelas DW

USE TESTE_DW

--Tabela Cliente:
/*
CREATE TABLE CLIENTE (
    IdCli INT PRIMARY KEY,
    DesCli VARCHAR(200),
    Dat_Cadastro DATE,
    Telefone CHAR (20),
    UF VARCHAR(2),
    Cidade VARCHAR(100),
    Bairro VARCHAR(100),
    Pessoa VARCHAR(1),
    TipCons VARCHAR(100),
    LicSaude VARCHAR (50),
    ValLicSaude DATE,
    Anvisa VARCHAR (100),
    ValAnvisa DATE,
    CRF_CRM VARCHAR (100),
    ValCRF DATE,
    AlvFunc VARCHAR (100),
    ValAlv DATE,
    Limite_Credito NUMERIC(9),
    Dat_UltComp DATE,
    Dat_LimCreAtu DATE,
    Vlr_LimCreAnt DECIMAL (10,4),
    Total_Debito DECIMAL(10,4),
    Suframa VARCHAR (9),
    Cod_RamoAtividade VARCHAR (100)
)
*/

--Tabela Fabricante:
/*
Create Table FABRICANT (
		Codigo INT PRIMARY KEY,
		Fantasia VARCHAR(25),
		Cgc_Cpf VARCHAR(14),
		Sta_ClaAbcVal CHAR(1),
		Per_ParticFat NUMERIC (9),
		Qtd_PrzMaxFat INT,
		Per_DscMaxVis NUMERIC (9),
		Per_DscMaxPrz NUMERIC (9),
		Per_DscBasComNor NUMERIC (9),
		Per_DscBasTax NUMERIC (9),
		Flg_Desconto BIT,
		Flg_BlqInfPar BIT,
		Flg_TransmPro BIT,
		Flg_TransmItx BIT,
		Flg_TransmGnx BIT,
		Flg_TransmNeo BIT,
		Flg_BlqIms BIT,
		Flg_TransmMtr BIT,
		Flg_TransmHyp BIT,
		Flg_BlqCloseUp BIT,
		Cod_ForPref INT
)
*/

--Tabela Vendedor:
/*
CREATE TABLE VENDEDOR (
						IdVen int primary key,
						Nome_Completo varchar(60),
						Nome_Guerra varchar (15),
						Data_Admissao smalldatetime,
						Data_Saida smalldatetime,
						Area_Atuacao varchar(30),
						Bloqueado bit,
						Cod_TipVenBas char(3),
						Vlr_Obj_vend numeric(9),
						Isn_CtaFin_vend int,
						Bairro varchar(20),
						Cidade varchar(20),
						UF char(2),
						Flg_Export bit,
						Transacao smalldatetime,
						Cod_Supervisor int,
						Supervisor varchar (30),
						Cod_Gerencia int,
						Gerente varchar(30),
						Vlr_Obj_geren numeric (9),
						Vlr_ComFixa_geren numeric(9),
						Isn_CtaFin_geren int,
						Cod_TabCom int)
*/

--Tabela Produto
/*
		CREATE TABLE PRODUTO (
		IdProd	 int primary key,
		Descricao	varchar(80),
		Unidade_Venda 	char(3),
		Ref_Fabricante	varchar(15),
		Localizacao 	varchar(8),
		Ctrl_Preco 	char(1),
		Ctrl_Venda 	char(1),
		Cod_Fabricante	int,
		Cod_EAN	varchar(13),
		Cod_AbcFar 	int,
		Cod_GrpPrc 	char(1),
		Cod_Promocao	numeric,
		Cod_PlaRegTri 	int,
		Cod_PrdExt	varchar(13),
		Cod_Classif 	varchar(7),
		Cod_CtrUsu 	varchar(3),
		Cod_Estabe	INT,
		Cod_RefPrati 	varchar(14),
		Cod_OriMer 	numeric,
		Validade 	numeric,
		Dat_Cadastro 	smalldatetime,
		Dat_UltVenda 	smalldatetime,
		Dat_PrcFabAnt 	smalldatetime,
		Dat_PrcAtual	smalldatetime,
		Dat_UltCompra 	smalldatetime,
		Dat_PrcFab	smalldatetime,
		Dat_UltVen 	smalldatetime,
		Dat_PrxVctLtl 	smalldatetime,
		Dat_PrxVctLot 	smalldatetime,
		Dat_EntAntDep	smalldatetime,
		Dat_UltEntDep  	smalldatetime,
		Sta_AbcValFat 	CHAR(1),
		Sta_AbcUniVen 	CHAR(1),
		Tipo 	CHAR(1),
		Tip_Prd 	CHAR(1),
		Tip_Rentab 	varchar(1),
		Tip_BlqLuc 	varchar(1),
		Tip_Por344 	varchar(2),
		Tip_LisPis 	char(1),
		Alq_Ipi 	numeric,
		Dias_PlaFinanc 	numeric,
		Taxa_PlaFinanc 	numeric,
		Des_PrdDet 	text,
		Des_PrdRef 	varchar(20),
		Edi_Pra int,
		Edi_Dep int,
		Lrg_Emb 	numeric,
		Alt_Emb	numeric,
		Prf_Emb 	numeric,
		Vol_Emb 	numeric,
		Und_EmbCmp 	char(3),
		Lrg_EmbCmp	 numeric,
		Alt_EmbCmp 	numeric,
		Prf_EmbCmp 	numeric, 
		Pes_EmbCmp 	numeric,
		Flag_ImprClassif1 	CHAR(1),
		Flg_PlaFatPrcLiq 	BIT,
		Flg_PolComercMax 	BIT,
		Flg_PrzComercMax 	BIT,
		Flg_Generico 	BIT,
		Flg_IncFis 	BIT,
		Flg_BlqDsc 	BIT,
		Flg_BlqVen 	BIT,
		Flg_BlqCmp 	BIT,
		Flg_BlqInfVen 	BIT,
		Flg_BlqInfPar 	BIT,
		Flg_BlqCot 	BIT,
		Flg_BlqPrp 	BIT,
		Flg_BlqCfv 	BIT,
		Flg_Bloqueado 	BIT,
		Flg_CusMedComRentab 	BIT,
		Flg_RegSbtEsp 	BIT
		)
	*/

-- Tabela Movimentação de Estoque
/*
	CREATE TABLE MOVESTO(
			IdProd	 int primary key,
			Cod_Lote	varchar(20),
			Dat_UltCompra 	smalldatetime,
			Dat_UltVenda 	smalldatetime,
			Dat_Vencim	smalldatetime,
			Cod_Etique	INT,
			Cod_Dep	smallint,
			Num_Rua	smallint,
			Num_Col	smallint,
			Num_Niv	smallint,
			Num_Apt	smallint,
			Qtd_Fisico	 INT,
			Qtd_Solici 	 INT,
			Qtd_Avaria 	 INT,
			Qtd_Transi 	 INT,
			Qtd_Reserv	 INT,
			Qtd_Quaren 	 INT,
			Qtd_Embalagem 	 INT,
			Qtd_SldAntBal 	 INT,
			Qtd_SldPosBal 	 INT,
			Qtd_PrmFisico 	 INT,
			Qtd_PrmDispon 	 INT,
			Qtd_PrmSolici 	 INT,
			Qtd_EntAnt 	 INT,
			Qtd_EstMin 	 INT,
			Qtd_EstMinCfg 	 INT,
			Qtd_EstMax 	 INT,
			Qtd_EstMaxCfg 	 INT,
			Qtd_UltEnt 	 INT,
			Qtd_EntAntDep 	 INT,
			Qtd_UltEntDep 	 INT,
			Qtd_Pulmao 	 INT,
			Qtd_EstTraMan 	 INT,
			Qtd_DiaSupPrd 	 INT,
			Qtd_CanCompra 	 INT,
			Qtd_PrzMaxFat 	 INT,
			Qtd_CngCli 	INT,
			Qtd_MesAleRec 	INT
			)
*/

-- Tabela Preço de Produto
/*
CREATE TABLE PRPREC (
		IdProd	 int primary key,
		Dat_alteracao	smalldatetime,
		Usuario	varchar(15),
		Prc_Tabela	 numeric,
		Prc_Venda 	 numeric,
		Prc_CusLiqPla 	 numeric,
		Prc_Pmz 	 numeric,
		Prc_FabAnt 	 numeric,
		Prc_MaxConAnt 	 numeric,
		Prc_RefFpb 	 numeric,
		Prc_RefFpbAnt 	 numeric,
		Prc_Fabric12 	 numeric,
		Prc_MaxCon12 	 numeric,
		Prc_Fabric17	 numeric,
		Prc_MaxCon17 	 numeric,
		Prc_Fabric175 	 numeric,
		Prc_MaxCon175 	 numeric,
		Prc_Fabric18 	 numeric,
		Prc_MaxCon18 	 numeric,
		Prc_Fabric19 	 numeric,
		Prc_MaxCon19 	 numeric,
		Prc_Fabric20 	 numeric,
		Prc_MaxCon20 	 numeric,
		Prc_Fabric21 	 numeric,
		Prc_MaxCon21 	 numeric,
		Prc_Fabric22 	 numeric,
		Prc_MaxCon22 	 numeric,
		Prc_Minimo 	 numeric,
		Prc_VenAnt 	 numeric,
		Prc_CusLiqEnt 	 numeric,
		Prc_CusLiqEntDep 	 numeric,
		Prc_CusMed 	 numeric,
		Prc_CusMedPra 	 numeric,
		Prc_CusMedDep 	 numeric,
		Prc_CusMedCom 	 numeric,
		Prc_EntAnt 	 numeric,
		Prc_UltEnt 	 numeric,
		Prc_EntAntDep 	 numeric,
		Prc_UltEntDep 	 numeric,
		Prc_Fabric 	 numeric,
		Prc_MaxCon 	 numeric,
		Per_ParticFat 	 numeric,
		Per_PlaRedCreIcm 	 numeric,
		Per_plarepICm 	 numeric,
		Per_PlaCreIcm 	 numeric,
		Per_PlaDebIcm 	 numeric,
		Per_PlaRebate 	 numeric,
		Per_PlaAgrega 	 numeric,
		Per_PlaDesc1 	 numeric,
		Per_PlaDesc2 	 numeric,
		Per_PlaBonific 	 numeric,
		Per_PlaIpi 	 numeric,
		Per_PlaDesFin 	 numeric,
		Per_PlaCusFre 	 numeric,
		Per_PlaDesOpe 	 numeric,
		Per_PlaDesFre 	 numeric,
		Per_PlaDesCom 	 numeric,
		Per_PlaDesPis 	 numeric,
		Per_PlaDesCof 	 numeric,
		Per_PlaDesIrpj 	 numeric,
		Per_PlaDesConSoc 	 numeric,
		Per_PlaDesIcms 	 numeric,
		Per_PlaMarOpe 	 numeric,
		Per_PlaMarFin 	 numeric,
		Per_ComVnd 	 numeric,
		Per_Markup 	 numeric,
		Per_BonAutOrc 	 numeric,
		Per_DscBasComNor 	 numeric,
		Per_ComEnt 	 numeric,
		Per_DscMaxVis 	 numeric,
		Per_DscMaxPrz 	 numeric,
		Per_DscEntAnt 	 numeric,
		Per_Ipi 	 numeric,
		Per_PlaMarRes 	 numeric,
		Per_BonAut 	 numeric,
		Per_DscUltEnt 	 numeric,
		Per_DscEntAntDep 	 numeric,
		Per_DscUltEntDep 	 numeric,
		Per_MarkupCusCom	 numeric,
		Per_DscAut 	 numeric,
		Per_DscAutOrc 	 numeric,
		Per_Rentab 	 numeric,
		Per_LucMin	 numeric
		)
*/

-- Tabela Politica X Operador X Vendedor X Supervisor

/*

CREATE TABLE POLXVEND (
	IdPvd INT PRIMARY KEY IDENTITY,
	Codigo int,
	Id_PolCom  int,
	Per_VrbPedAtivo decimal (10,2),
	Per_VrbPedReceptivo decimal (10,2),
	Per_VrbPedMovel decimal (10,2),
	Per_VrbPedEletronico decimal (10,2),
	Per_VrbPedWeb decimal (10,2),
	Per_VrbPedHospitalar decimal (10,2),
	Per_VrbPedCotacao decimal (10,2),
	Flg_BlqDebVrbSup bit,
	flg_Oper bit,
	flg_vende bit, 
	flg_sup bit
	)

GO
*/

-- Tabela Politica por Cliente
/*
CREATE TABLE [TESTE_DW].DBO.PCXCL (
			Id_PolCom INT,
			Cod_Client INT,	
			Qtd_PrzMax INT,	
			Per_Descon NUMERIC,	
			Per_DscVis NUMERIC,	
			CodAnt INT,	
			NovoCodigo INT)
GO
*/

--Tabela Politica X Grp de Clientes

/*CREATE TABLE [TESTE_DW].DBO.PCXGC (
			Id_PolCom INT,
			Cod_Client INT,	
			Qtd_PrzMax INT,	
			Per_Descon NUMERIC,	
			Per_DscVis NUMERIC,	
			CodAnt INT,	
			NovoCodigo INT)
GO
*/

--Tabela Politica X UF
/*
CREATE TABLE [TESTE_DW].DBO.PCXUF (
			Id_PolCom INT,
			Cod_Uf char(2),	
			Qtd_PrzMax INT,	
			Per_Descon NUMERIC,	
			Per_DscVis NUMERIC)

GO
*/

--Tabela Política X Fabricante

/*
CREATE TABLE [TESTE_DW].DBO.PCXFB (
			Id_PolCom INT,
			Cod_Fabric INT,	
			Qtd_PrzMax INT,	
			Per_Descon NUMERIC,	
			Per_DscVis NUMERIC)

GO
*/

--Tabela Policita
/*

	CREATE TABLE [TESTE_DW].DBO.POCOM (
		Id_PolCom	int			
	,	Cod_PolCom	varchar	(	25	)
	,	Des_Detalhada	varchar	(	120	)
	,	Dat_Inicio	smalldatetime			
	,	Dat_Termino	smalldatetime			
	,	Bloqueado	bit			
	,	Qtd_Minimo	smallint
	,	Vlr_Minimo	numeric			
	,	Per_Desconto	numeric			
	,	Qtd_PrzMax	smallint
	,	Cod_TipPrz	char	(	1	)
	,	Cod_TabPrc	int			
	,	Cod_TabPrz	int			
	,	Cod_TabComVdr	int			
	,	Cod_TabComOpe	int			
	,	Usuario	varchar	(	35	)
	,	Transacao	smalldatetime			
	,	Flg_Televendas	bit			
	,	Flg_Cfv	bit			
	,	Flg_PedEle	bit			
	,	Flg_Web	bit			
	,	Qtd_IteMin	int			
	,	Per_DscCom	numeric	(	9	)
	,	Per_DscFin	numeric	(	9	)
	,	Per_RedComVdr	numeric	(	9	)
	,	Per_RedComOpe	numeric	(	9	)
	,	Flg_NegDsc	bit			
	,	Flg_BlqCli	bit			
	,	Flg_Balcao	bit			
	,	Flg_ExcCla	bit			
	,	Flg_ExcFab	bit			
	,	Flg_NaoSugDscItePdv	bit			
	,	Dat_Cadastro	smalldatetime			
	,	Per_DscAutPrc	numeric	(	9	)
	,	Per_AcrAutPrc	numeric	(	9	)
	,	Flg_PrcVenPrpPrz	bit			
	,	Per_CorPrcVenPrpPrz	numeric	(	9	)
	,	Flg_BlqVerba	bit			
	,	Flg_BlqPolDifItePdv	bit			
	,	Flg_BlqPolDifCabPdv	bit			
	,	Flg_MarkupCusCom	bit			
	,	Tip_RatBon	varchar	(	1	)
	,	Per_DscComVis	numeric	(	9	)
	,	Per_DscFinVis	numeric	(	9	)
	,	Flg_SugDscGlbPdv	bit			
	,	Tip_PolCom	varchar	(	3	)
	,	Flg_DbqUsoVrbPdv	bit			
	,	Flg_GerVrbPdv	bit			
	,	Tip_VrbPdv	varchar	(	1	)
	,	Flg_SugPrzCli	bit
	,	Controle_Saldo	varchar	(	1	)
	,	Flg_AcuVrbPos	bit			
	,	Flg_AcuVrbNeg	bit			
	,	Flg_CtrPdvVrbVdr	bit			
	,	Flg_CtrPdvVrbOpe	bit			
	,	Per_ComAtvVdr	numeric	(	9	)
	,	Per_ComPasVdr	numeric	(	9	)
	,	Per_ComAtvOpe	numeric	(	9	)
	,	Per_ComPasOpe	numeric	(	9	)
	,	Obs_PolCom	text
	,	Flg_UsaTabPrcCadCli	bit			
	,	Dat_Criacao	datetime			
	)

	GO
*/

--Tabela Politica x Prazo
/*
CREATE TABLE TESTE_DW.DBO.PCXPZ (Id_PolCom INT,
	Cod_TabPrz INT,
	Vlr_MinPdv NUMERIC)
GO
*/

-- Política X Produto
/*

CREATE TABLE TESTE_DW.DBO.PCXPR (
		Id_PolCom	int			
	,	Cod_Produt	int			
	,	Qtd_Minimo	int			
	,	Qtd_PrzMax	int			
	,	Per_Descon	numeric			
	,	Per_DscVis	numeric			
	,	Qtd_Min2	int			
	,	Per_Dsc2	numeric			
	,	Qtd_Min3	int			
	,	Per_Dsc3	numeric			
	,	Qtd_Min4	int			
	,	Per_Dsc4	numeric			
	,	Qtd_Min5	int			
	,	Per_Dsc5	numeric			
	,	Prc_Promoc	numeric			
	,	Per_DscVis2	numeric			
	,	Per_DscVis3	numeric			
	,	Per_DscVis4	numeric			
	,	Per_DscVis5	numeric			
	,	Qtd_Maximo	int			
	,	Qtd_Max2	int			
	,	Qtd_Max3	int			
	,	Qtd_Max4	int			
	,	Qtd_Max5	int			
	,	Tip_Sai	varchar	(1)
	,	Per_Bonifi	numeric			
	,	Per_MkpPrdVis	numeric			
	,	Per_MkpPrdPrz	numeric			
	,	IndEstabeMkp	int			
	,	IndPrcCusBasMkp	int			

	)
GO
*/

-- Tabela PCXPL
/*
CREATE TABLE TESTE_DW.DBO.PCXPL ( Id_PolCom int , Cod_Produt int , Cod_Lote varchar(20) )
GO
*/

--Tabela PCXCP
/*
CREATE TABLE TESTE_DW.DBO.PCXCP ( Id_PolCom INT, Cod_Classi VARCHAR (7), Qtd_PrzMax INT, Per_Descon NUMERIC, Per_DscVis NUMERIC)
GO
*/
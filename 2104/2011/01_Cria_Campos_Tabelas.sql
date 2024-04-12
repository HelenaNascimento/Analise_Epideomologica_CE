/*
  Exclui Campo : AGCOB.Num_UltBlo
*/
Exec PR_ADM_ExcluiCampo 'AGCOB', 'Num_UltBlo'
GO


/*
Campos a excluir: 

PDECB.Nom_Arquiv
PDECB.Sta_PedAnt


Select Nom_Arquiv, Dat_Inicio, Vlr_TotalDscVis, Vlr_TotalDscPrz, Sta_PedAnt
From PDECB
*/


----------------------------------------------------------------------------------------------------------------------------------------
/*
  Versao 20.11
  Novembro de 2022
  Tabela: TPOPE
  Campo : Flg_CalRepInt
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'TPOPE'
                  And Column_Name = 'Flg_CalRepInt')
  ALTER TABLE dbo.TPOPE ADD Flg_CalRepInt bit NULL default 0
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: TPOPE  
  Campo : Flg_CalRepIcmInt
*/
Exec PR_ADM_ExcluiCampo 'TPOPE', 'Flg_CalRepInt'
GO
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'TPOPE'
                  And Column_Name = 'Flg_CalRepIcmInt')
  ALTER TABLE dbo.TPOPE ADD Flg_CalRepIcmInt bit NULL default 0
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: CFVCB: Dados layout CFV 
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[CFVCB]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
  CREATE TABLE [dbo].[CFVCB](
      [Id] [int] NOT NULL,
	  [Dat_Criacao] [datetime] NULL,
	  [Dat_Alteracao] [datetime] NULL,
	  [Dat_Exclusao] [datetime] NULL,
	  [Status] [varchar](50) NULL,
	  [Sta_Mensagem] [ntext] NULL,
	  [Nom_Arquiv] [varchar](200) NULL,
	  [Arquivo] [ntext] NULL,
	  [Nom_Layout] [varchar](50) NULL,
	  [Cod_Estabe] [int] NULL,
	  [Cod_Client] [varchar](20) NULL,
	  [Cod_PedCli] [varchar](20) NULL,
	  [Cod_Vended] [varchar](20) NULL,
	  [Cod_PedVen] [int] NULL	  
  ) ON [PRIMARY]
GO

IF NOT EXISTS (SELECT NAME FROM sysobjects WHERE name = 'PK_CFVCB')
  ALTER TABLE [dbo].[CFVCB] ADD CONSTRAINT [PK_CFVCB] PRIMARY KEY CLUSTERED(
	  [Id] ASC
  ) WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80) ON [PRIMARY]
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: NFSCB
  Campo Nom_UsuFat: Nome do usuário que efetuou o faturamento
*/
GO
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFSCB'
                  And Column_Name = 'Nom_UsuFat')
  ALTER TABLE dbo.NFSCB ADD Nom_UsuFat varchar(15) NULL
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: PDVIT
  Campo Per_LinCot : Percentual de acréscimo / desconto linear realizado na Cotação
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PDVIT'
                  And Column_Name = 'Per_LinCot')
  ALTER TABLE dbo.PDVIT ADD Per_LinCot numeric(8,4) NULL default 0
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PDVCB
  Campo ID_PVMCB: Vínculo com Pedido de Venda Multi-estabelecimento
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PDVCB'
                  And Column_Name = 'ID_PVMCB')
  ALTER TABLE dbo.PDVCB ADD ID_PVMCB int NULL
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PVMCB: Cabeçalho Pedido de Venda Multi-Estabe 
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[PVMCB]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
create table PVMCB (
  ID_PVMCB int Identity(1,1) not null,
  Cod_EstPad int not null,
  Dat_Pedido smalldatetime,
  Sta_Pedido char(1),
  Cod_OrigemPdv varchar(2),
  Tip_Faturamento varchar(3),
  Tip_SaiPdv varchar(1),
  Tip_RatBon varchar(1),
  Tip_GerVrb varchar(1),
  Cod_Cliente int not null,
  Cod_Digitador int not null,
  Cod_VendTlmkt int not null,
  Cod_Vendedor int not null,
  Cod_Supervisor int,
  Cod_Gerencia int,
  Cod_SupOpe int,
  Cod_GerOpe int,
  Cod_Rota int,
  Observacao text,
  ID_PolCom int,
  Cod_Prz int,
  Qtd_PrzMed int,
  Qtd_Parcela int,
  Per_DescontoFin numeric(18, 4),
  Per_DescontoCom numeric(18, 4),
  Per_ComVnd numeric(18, 4),
  Per_ComOpe numeric(18, 4),
  Flg_PriFat bit,
  Flg_CtrUsuPdv bit,
  Flg_CnvDscIteDscGlb bit,
  Obs_NotFis text,
  Obs_IntCtaRec text,
  Flg_Credito int,
  Cod_BlqLic char(1),
  Cod_MtvRej varchar(3),
  Per_Rnt numeric(18, 4),
  Vlr_RepIcms numeric(18, 4),
  Vlr_SubsTrib numeric(18, 4),
  Vlr_SbtRes numeric(18, 4),
  Vlr_DscTri numeric(18, 4),
  Vlr_IcmsDeson numeric(18, 4),
  Vlr_DscBon numeric(18, 4),
  Vlr_Despes numeric(18, 4),
  Vlr_DspExt numeric(18, 4),
  Vlr_Ipi numeric(18, 4),
  Vlr_Verba numeric(18, 4),
  Vlr_BasIrf numeric(18, 4),
  Vlr_Irf numeric(18, 4),
  Qtd_Itens integer,
  Vlr_LiqItens numeric(18, 4),
  Vlr_BasDscCom numeric(18, 4),
  Vlr_BasPar numeric(18, 4),
  Vlr_Total numeric(18, 4),
  Qtd_ItensProm Int,  
  C_VlrDesconto numeric(18, 4),
  C_VlrImposto numeric(18, 4),
  C_VlrPedido numeric(18, 4),
  Tip_Vencimento varchar(1),
  constraint PK_PVMCB primary key (ID_PVMCB));
Go

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PMXPV: Pedido de Venda Multi-Estabe x Pedido de Venda
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[PMXPV]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
create table PMXPV(
  ID_PVMCB int not null,
  Num_PedVen int not null,
  constraint PK_PMXPV primary key (ID_PVMCB, Num_PedVen));
Go

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PVMES: Estabelecimento dos Pedido de Venda Multi-Estabe
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[PVMES]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
create table PVMES (
  ID_PVMCB int not null,
  Cod_Estabe int not null,
  Des_Estabe varchar(25),
  Cod_RegTri int, 
  Des_RegTri varchar(30),
  Cod_Trans int, 
  Des_Trans varchar(80),
  Cod_Rotas int, 
  Des_Rotas varchar(30),
  Cod_Vende int, 
  Des_Vende varchar(15),
  Cod_Opera int, 
  Des_Opera varchar(15),
  Cod_Agente int, 
  Des_Agente varchar(25),
  Transacao smalldatetime not null,
  constraint PK_PVMES primary key (ID_PVMCB, Cod_Estabe),
  constraint FK_PVMES_PVMCB foreign key (ID_PVMCB) references PVMCB (ID_PVMCB) on delete cascade);
Go

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PVMIT: Itens dos Pedido de Venda Multi-Estabe
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[PVMIT]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
create table PVMIT(
  ID_PVMCB int not null,
  Num_SeqDig int Identity(1,1) not null,
  Cod_Estabe int not null,
  Cod_Produto int not null,
  Cod_Lote varchar(20),
  Cod_Fabricante int not null,
  Ean_Produto varchar(20),
  ID_PolCom int not null,
  Tip_Sai varchar(1),
  Qtd_Pedido int,
  Qtd_Bonificacao int,
  Qtd_ImpFat int,
  Qtd_Atendi int,
  Fat_AprPad int,
  Per_DscVis numeric(18, 4),
  Per_Descon numeric(18, 4),
  Per_Desconto numeric (18 ,4),
  Per_DscPolCom	numeric(18, 4),
  Per_DscVisPolCom numeric(18, 4),
  Prc_Unitario numeric(18, 4),
  Prc_TabBru numeric(18, 4),
  Prc_PolCom numeric(18, 4),
  Des_UnvImpFat varchar(3),
  Des_AprImpFat varchar(12),
  Fat_CnvImpFat int,
  Prc_UniImpFat numeric(18, 4),
  Qtd_PrzMax int,
  Flg_Verba bit,
  Vlr_Bruto numeric(18, 4),
  Vlr_Verba numeric(18, 4),
  Vlr_VrbPdv numeric(18, 4),
  Vlr_VrbOpe numeric(18, 4),
  Vlr_VrbVdr numeric(18, 4),
  Vlr_VrbSup numeric(18, 4),
  Vlr_DscBonRat numeric(18, 4),
  Vlr_RepIcms numeric(18, 4),
  Vlr_DscRat numeric(18, 4),
  Vlr_SubsTrib numeric(18, 4),
  Vlr_SbtRes numeric(18, 4),
  Vlr_DscTri numeric(18, 4),
  Vlr_IcmsDeson numeric(18, 4),
  Vlr_Bonificacao numeric(18, 4),
  Vlr_Despes numeric(18, 4),
  Vlr_DspExt numeric(18, 4),
  Vlr_Ipi numeric(18, 4),
  Vlr_BasIrf numeric(18, 4),
  Vlr_Irf numeric(18, 4),
  Loc_Fisica varchar(15),
  Prc_Fabric numeric(18, 4), 
  Prc_MaxCon numeric(18, 4),
  Prc_RefFpb numeric(18, 4),
  Prc_Tabela numeric(18, 4),
  Prc_CusMinComMarkup numeric(18, 4),
  Flg_PrcUniDsc bit,
  Qtd_PrzPolCom int,
  Flg_PolPrm bit,
  Cod_KitPrm int,
  Cod_Promocao int,
  Desconto bit,
  Flg_BlqInfPar bit,
  Per_RntBru numeric(18, 4),
  C_PrcTotal numeric(18, 4),
  C_PrcUni numeric(18, 4),
  C_PerDscIte numeric(18, 4),
  C_VlrDesconto numeric(18, 4),
  constraint PK_PVMIT primary key (ID_PVMCB, Cod_Estabe, Cod_Produto),
  constraint FK_PVMIT_PVMCB foreign key (ID_PVMCB) references PVMCB (ID_PVMCB) on delete cascade,
  constraint FK_PVMIT_PVMES foreign key (ID_PVMCB, Cod_Estabe) references PVMES (ID_PVMCB, Cod_Estabe),
  constraint FK_PVMIT_PRODU foreign key (Cod_Produto) references PRODU (Codigo));
Go

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PVMIT  
  Campo : Cod_Lote
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PVMIT'
                  And Column_Name = 'Cod_Lote')
  ALTER TABLE dbo.PVMIT ADD Cod_Lote varchar(20)
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: TRXUF  
  Campo : Flg_BlqPdvVlrMinFre
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'TRXUF'
                  And Column_Name = 'Flg_BlqPdvVlrMinFre')
  ALTER TABLE dbo.TRXUF ADD Flg_BlqPdvVlrMinFre bit NULL default 0
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PEXFT
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[PEXFT]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
begin
	create table PEXFT(
	  Cod_Estabe int not null,
	  Cod_Pedido int not null,
	  constraint PK_PEXFT primary key (Cod_Estabe, Cod_Pedido));

	create NONCLUSTERED INDEX IX_PK_PEXFT ON dbo.PEXFT(Cod_Estabe, Cod_Pedido);
end
GO



/*
  Versao 20.11
  Novembro de 2022
  Tabela: PRODU
  Campo : Prc_Fabric19 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PRODU'
               And Column_Name = 'Prc_Fabric19')
  ALTER TABLE dbo.PRODU ADD Prc_Fabric19 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: PRODU
  Campo : Prc_MaxCon19 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PRODU'
               And Column_Name = 'Prc_MaxCon19')
  ALTER TABLE dbo.PRODU ADD Prc_MaxCon19 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ALTPR
  Campo : Prc_Fabric19 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ALTPR'
               And Column_Name = 'Prc_Fabric19')
  ALTER TABLE dbo.ALTPR ADD Prc_Fabric19 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ALTPR
  Campo : Prc_MaxCon19 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ALTPR'
               And Column_Name = 'Prc_MaxCon19')
  ALTER TABLE dbo.ALTPR ADD Prc_MaxCon19 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: HSPRC
  Campo : Vlr_PrcFab19 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'HSPRC'
               And Column_Name = 'Vlr_PrcFab19 ')
  ALTER TABLE dbo.HSPRC ADD Vlr_PrcFab19  numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: HSPRC
  Campo : Vlr_PrcMaxCon19
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'HSPRC'
               And Column_Name = 'Vlr_PrcMaxCon19')
  ALTER TABLE dbo.HSPRC ADD Vlr_PrcMaxCon19 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_19 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_19')
  ALTER TABLE dbo.ABCIT ADD PF_19 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_19 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_19')
  ALTER TABLE dbo.ABCIT ADD PMC_19 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_19_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_19_ALC')
  ALTER TABLE dbo.ABCIT ADD PF_19_ALC numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_19_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_19_ALC')
  ALTER TABLE dbo.ABCIT ADD PMC_19_ALC numeric(12,2) NULL default 0
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: PRODU
  Campo : Prc_Fabric21 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PRODU'
               And Column_Name = 'Prc_Fabric21')
  ALTER TABLE dbo.PRODU ADD Prc_Fabric21 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: PRODU
  Campo : Prc_MaxCon21 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PRODU'
               And Column_Name = 'Prc_MaxCon21')
  ALTER TABLE dbo.PRODU ADD Prc_MaxCon21 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ALTPR
  Campo : Prc_Fabric21 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ALTPR'
               And Column_Name = 'Prc_Fabric21')
  ALTER TABLE dbo.ALTPR ADD Prc_Fabric21 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ALTPR
  Campo : Prc_MaxCon21 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ALTPR'
               And Column_Name = 'Prc_MaxCon21')
  ALTER TABLE dbo.ALTPR ADD Prc_MaxCon21 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: HSPRC
  Campo : Vlr_PrcFab21 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'HSPRC'
               And Column_Name = 'Vlr_PrcFab21 ')
  ALTER TABLE dbo.HSPRC ADD Vlr_PrcFab21  numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: HSPRC
  Campo : Vlr_PrcMaxCon21
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'HSPRC'
               And Column_Name = 'Vlr_PrcMaxCon21')
  ALTER TABLE dbo.HSPRC ADD Vlr_PrcMaxCon21 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_21 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_21')
  ALTER TABLE dbo.ABCIT ADD PF_21 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_21 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_21')
  ALTER TABLE dbo.ABCIT ADD PMC_21 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_21_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_21_ALC')
  ALTER TABLE dbo.ABCIT ADD PF_21_ALC numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_21_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_21_ALC')
  ALTER TABLE dbo.ABCIT ADD PMC_21_ALC numeric(12,2) NULL default 0
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: PRODU
  Campo : Prc_Fabric22 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PRODU'
               And Column_Name = 'Prc_Fabric22')
  ALTER TABLE dbo.PRODU ADD Prc_Fabric22 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: PRODU
  Campo : Prc_MaxCon22 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PRODU'
               And Column_Name = 'Prc_MaxCon22')
  ALTER TABLE dbo.PRODU ADD Prc_MaxCon22 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ALTPR
  Campo : Prc_Fabric22 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ALTPR'
               And Column_Name = 'Prc_Fabric22')
  ALTER TABLE dbo.ALTPR ADD Prc_Fabric22 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ALTPR
  Campo : Prc_MaxCon22 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ALTPR'
               And Column_Name = 'Prc_MaxCon22')
  ALTER TABLE dbo.ALTPR ADD Prc_MaxCon22 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: HSPRC
  Campo : Vlr_PrcFab22 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'HSPRC'
               And Column_Name = 'Vlr_PrcFab22 ')
  ALTER TABLE dbo.HSPRC ADD Vlr_PrcFab22  numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: HSPRC
  Campo : Vlr_PrcMaxCon22
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'HSPRC'
               And Column_Name = 'Vlr_PrcMaxCon22')
  ALTER TABLE dbo.HSPRC ADD Vlr_PrcMaxCon22 numeric(18,4) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_22 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_22')
  ALTER TABLE dbo.ABCIT ADD PF_22 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_22 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_22')
  ALTER TABLE dbo.ABCIT ADD PMC_22 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_22_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_22_ALC')
  ALTER TABLE dbo.ABCIT ADD PF_22_ALC numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_22_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_22_ALC')
  ALTER TABLE dbo.ABCIT ADD PMC_22_ALC numeric(12,2) NULL default 0
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_12 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_12')
  ALTER TABLE dbo.ABCIT ADD PF_12 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_12 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_12')
  ALTER TABLE dbo.ABCIT ADD PMC_12 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_12_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_12_ALC')
  ALTER TABLE dbo.ABCIT ADD PF_12_ALC numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_12_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_12_ALC')
  ALTER TABLE dbo.ABCIT ADD PMC_12_ALC numeric(12,2) NULL default 0
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_20 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_20')
  ALTER TABLE dbo.ABCIT ADD PF_20 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_20 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_20')
  ALTER TABLE dbo.ABCIT ADD PMC_20 numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PF_20_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PF_20_ALC')
  ALTER TABLE dbo.ABCIT ADD PF_20_ALC numeric(12,2) NULL default 0
GO
/*
  Versao 20.11
  Novembro de 2022
  Tabela: ABCIT
  Campo : PMC_20_ALC 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ABCIT'
               And Column_Name = 'PMC_20_ALC')
  ALTER TABLE dbo.ABCIT ADD PMC_20_ALC numeric(12,2) NULL default 0
GO

if Exists(Select * from Information_Schema.columns
          Where TABLE_NAME = 'TBPDE'
          And COLUMN_NAME = 'Dat_IniPro'
          And DATA_TYPE = 'smalldatetime')
  ALTER TABLE TBPDE ALTER COLUMN Dat_IniPro datetime null
GO

if Exists(Select * from Information_Schema.columns
          Where TABLE_NAME = 'TBPDE'
          And COLUMN_NAME = 'Dat_FimPro'
          And DATA_TYPE = 'smalldatetime')
  ALTER TABLE TBPDE ALTER COLUMN Dat_FimPro datetime null
GO

/*
  Versao 21.03
  Julho de 2022
  Tabela: PMEML
  Campo Id: Campo chave da tabela
  todo: devido a dependências no infarma email, vamos criar somente o campo mas não defini-lo como chave neste momento.
*/
IF NOT EXISTS(
    SELECT 
	    Column_Name 
	FROM INFORMATION_SCHEMA.COLUMNS
    WHERE Table_Name = 'PMEML'
          AND Column_Name = 'Id'
)
    ALTER TABLE dbo.PMEML ADD Id int NOT NULL DEFAULT 0
GO

/*
  Versao 20.08
  Outubro de 2022
  Tabela: PMSMU: Parâmetros da unidade de gerenciamento de segurança.
*/
IF NOT EXISTS (
    SELECT 1 
	FROM dbo.sysobjects 
	WHERE id = OBJECT_ID(N'dbo.PMSMU') 
	      AND OBJECTPROPERTY(id, N'IsUserTable') = 1
)
	CREATE TABLE PMSMU (
	    Id int NOT NULL, 
        Dat_Criacao	datetime,
        Dat_Alteracao datetime,
        Dat_Exclusao datetime,
		Id_EmlEnvRecSnh int

        CONSTRAINT PK_PMSMU PRIMARY KEY CLUSTERED(
        	  Id ASC
          ) WITH (
              PAD_INDEX = OFF, 
        	  STATISTICS_NORECOMPUTE = OFF, 
        	  IGNORE_DUP_KEY = OFF, 
        	  ALLOW_ROW_LOCKS = ON, 
        	  ALLOW_PAGE_LOCKS = ON, 
        	  FILLFACTOR = 80
        ) 
    );
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PARAM
  Campo : Des_TknTreinacon - Token de integração com a Treinacon
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PARAM'
                  And Column_Name = 'Des_TknTreinacon')
  ALTER TABLE dbo.PARAM ADD Des_TknTreinacon varchar(255) NULL
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: FLTRC - Tabela de Fila de envio de XMLs a Treinacon
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[FLTRC]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
  CREATE TABLE FLTRC(
    Codigo int NOT NULL identity,
    Num_Cnpj varchar(14) NOT NULL,
    Chv_Acesso varchar(44) NULL,
    Arquivo ntext NULL,
    CONSTRAINT [PK_FLTRC] PRIMARY KEY CLUSTERED (Codigo ASC));
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: MOVFP
  Campo : Des_CodOrdPix
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MOVFP'
                 And Column_Name = 'Des_CodOrdPix')
  ALTER TABLE MOVFP ADD Des_CodOrdPix Varchar(100)
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: MOVFP
  Campo : Cod_TaxAplPix
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MOVFP'
                 And Column_Name = 'Cod_TaxAplPix')
  ALTER TABLE dbo.MOVFP ADD Cod_TaxAplPix int
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: MOVFP
  Campo : Flg_ExpApiInfarm
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MOVFP'
                 And Column_Name = 'Flg_ExpApiInfarm')
  ALTER TABLE dbo.MOVFP ADD Flg_ExpApiInfarm bit not null default 0
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: PARAM
  Campo : Des_KeyAceShipay
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'PARAM'
                 And Column_Name = 'Des_KeyAceShipay')
  ALTER TABLE PARAM ADD Des_KeyAceShipay Varchar(100)
GO


/*
  Versao 20.11
  Novembro de 2022
  Tabela: PARAM
  Campo : Des_KeySecShipay
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'PARAM'
                 And Column_Name = 'Des_KeySecShipay')
  ALTER TABLE PARAM ADD Des_KeySecShipay Varchar(120)
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: PARAM
  Campo : Des_KeyPix
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'PARAM'
                 And Column_Name = 'Des_KeyPix')
  ALTER TABLE PARAM ADD Des_KeyPix varchar(80)
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: TXPIX - Tabela de Taxas PIX
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[TXPIX]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
  CREATE TABLE TXPIX (
     Cod_TaxPix int not null default 0,
     Cod_Estabe int NOT NULL,
     Cod_InstitFin int not null default 0,
     Flg_SitTax bit Not null Default 1,
     Val_TaxPix numeric(18,2) not null default 0
     CONSTRAINT [PK_TXPIX] PRIMARY KEY CLUSTERED (Cod_TaxPix ASC));
GO

/*
  Versao 20.11
  Novembro de 2022
  Tabela: MVPIX - Tabela de movimentos PIX
*/
IF NOT EXISTS (SELECT 1 FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[MVPIX]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
  CREATE TABLE MVPIX(
      Cod_Estabe int NOT NULL,
      Cod_MovPix int identity NOT NULL,
      Cod_Movime int NOT NULL,
      Des_CodOrdPix varchar(100) NOT NULL,
      Val_Movime numeric(18, 2) NULL,
      Dat_Movime datetime NOT NULL,
      Des_Situac char(1) NOT NULL    
      CONSTRAINT PK_MVPIX PRIMARY KEY CLUSTERED (Cod_Estabe ASC, Cod_MovPix ASC, Cod_Movime ASC));
GO

/*
  Versao 20.11
  Março de 2023
  Tabela: AGCOB
  Campo : Cod_CliVinc
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'AGCOB'
                 And Column_Name = 'Cod_CliVinc')
  ALTER TABLE dbo.AGCOB ADD Cod_CliVinc int null
GO

/*
  Versao 20.11
  Março de 2023
  Tabela: TBCLP
  Campo : Tip_Priori - Tipo de priridade de processamento
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'TBCLP'
                  And Column_Name = 'Tip_Priori')
  ALTER TABLE dbo.TBCLP ADD Tip_Priori varchar(12) NULL
GO

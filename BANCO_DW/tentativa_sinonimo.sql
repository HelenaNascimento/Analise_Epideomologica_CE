/* Início cria objetos */


/* Apagar tabelas se existirem */
If Exists (Select 0 From dbo.sysobjects Where id = object_id(N'dbo.BI_PDVCB') And OBJECTPROPERTY(id, N'isTable') = 1)
	Drop Table dbo.BI_PDVCB;
Go

Declare @Banco_BI Sysname;
Set @Banco_BI = DB_NAME() + '_BI';

If Not Exists (Select 0 From sys.databases Where name = @Banco_BI)
Begin
	Declare @Diretorio NVarchar(Max);
	Select @Diretorio = Reverse(Substring(Reverse(filename), Charindex('\', Reverse(filename)), Len(filename))) 
	From sysfiles Where fileid = 1;

	Declare @Cmd NVarchar(Max);
	Set @Cmd = 'CREATE DATABASE ['+@Banco_BI+']
	 On  PRIMARY 
	( NAME = N'''+@Banco_BI+'_1'', FILENAME = N'''+@Diretorio+@Banco_BI+'_1.mdf'', SIZE = 100MB , FILEGROWTH = 100MB ),
	( NAME = N'''+@Banco_BI+'_2'', FILENAME = N'''+@Diretorio+@Banco_BI+'_2.ndf'', SIZE = 100MB , FILEGROWTH = 100MB ),
	( NAME = N'''+@Banco_BI+'_3'', FILENAME = N'''+@Diretorio+@Banco_BI+'_3.ndf'', SIZE = 100MB , FILEGROWTH = 100MB ),
	( NAME = N'''+@Banco_BI+'_4'', FILENAME = N'''+@Diretorio+@Banco_BI+'_4.ndf'', SIZE = 100MB , FILEGROWTH = 100MB )
	 LOG On 
	( NAME = N'''+@Banco_BI+'_log'', FILENAME = N'''+@Diretorio+@Banco_BI+'_log.ldf'', SIZE = 512MB , FILEGROWTH = 512MB );
	ALTER DATABASE ['+@Banco_BI+'] Set RECOVERY SIMPLE;
	ALTER DATABASE ['+@Banco_BI+'] Set PAGE_VERIFY CHECKSUM;
	ALTER AUTHORIZATION On DATABASE::['+@Banco_BI+'] TO [sa]
	';
	Exec(@Cmd);
End;
Go

Declare @Banco_BI Sysname;
Set @Banco_BI = DB_NAME() + '_BI';

Declare @Cmd NVarchar(Max);
Set @Cmd = N'Use [' + @Banco_BI + ']
If Not Exists (Select 0 From dbo.sysobjects Where id = object_id(N''[dbo].[BI_PDVCB]'') And OBJECTPROPERTY(id, N''isTable'') = 1)
	CREATE TABLE dbo.BI_PDVCB (
		Numero	int	Not NULL,
		Tip_Pedido	varchar(1),
		Status1	varchar(1),
		Status2	varchar(1),
		Bloqueio varchar(2),
		Flg_JaFechad	bit,
		Flg_Calc	bit	Not NULL,
		Qtd_ImpPnt	int,
		Cod_OrigemPd	varchar(2),
		Cod_PedRem	int,
		Cod_PedCli	varchar(20),
		Flg_ComisNor	bit,
		Per_ComVnd	numeric(18, 8)	 	,
		Flg_TabProgr	bit	 	,
		Tip_Faturame	varchar(3)	 	,
		Tip_Vencimen	varchar(1)	 	,
		Dat_Pedido	smalldatetime	 	,
		Cod_SerNfs	varchar(3)	 	,
		Cod_NumNfsIn	int	 	,
		Cod_NumNfsFi	int	 	,
		Cod_Cliente	int	 	,
		Cod_Funciona	int	 	,
		Num_Lote	int	 	,
		Consumidor	varchar(35)	 	,
		Tip_Consumid	varchar(1)	 	,
		Qtd_PrzMed	numeric(18, 8)	 	,
		Qtd_Parcela	int	 	,
		Qtd_Interval	int	 	,
		Flg_PrzProm	bit	 	,
		Flg_VlrFinan	bit	 	,
		Cod_VendTlmk	int	 	,
		Cod_Vendedor	int	 	,
		Cod_Agente	int	 	,
		Cod_Transpor	int	 	,
		Cod_Rota	int	 	,
		Cod_MicroReg	int	 	,
		Cod_RegTri	int	 	,
		Qtd_Itens	int	 	,
		Qtd_ItensPro	int	 	,
		Vlr_BasDscCo	numeric(18, 8)	 	,
		Vlr_BasPar	numeric(18, 8)	 	,
		Vlr_LiqItens	numeric(18, 8)	 	,
		Vlr_RepIcms	numeric(18, 8)	 	,
		Vlr_SubsTrib	numeric(18, 8)	 	,
		Vlr_DscTri	numeric(18, 8)	 	,
		Tip_DscPdv	varchar(1)	 	,
		Per_Desconto	numeric(18, 8)	 	,
		Per_Desconto	numeric(18, 8)	 	,
		C_VlrDescont	numeric(18, 8)	 	,
		C_VlrPedido	numeric(18, 8)	 	,
		Observacao	text	 	,
		Qtd_Volumes	int	 	,
		Qtd_Peso	numeric(18, 8)	 	,
		Hor_Entrada	smalldatetime	 	,
		Hor_Prenota	smalldatetime	 	,
		Hor_Fatura	smalldatetime	 	,
		Hor_Saida	smalldatetime	 	,
		Cod_FunSepar	int	 	,
		Cod_FunConfe	int	 	,
		Cod_FunEmbal	int	 	,
		Flg_Credito	int	 	,
		Msg_Bloqueio	varchar(30)	 	,
		Msg_NF	varchar(50)	 	,
		Nom_UsuDesbl	varchar(15)	 	,
		Msg_JustDesb	varchar(50)	 	,
		Cod_Propos	int	 	,
		Per_SubFat	numeric(18, 8)	 	,
		Usuario	varchar(15)	 	,
		Transacao	smalldatetime	 	,
		Per_DsdOrc	numeric(18, 0)	 	,
		Cod_Prz	int	 	,
		Hor_ConferIn	smalldatetime	 	,
		Hor_ConferFi	smalldatetime	 	,
		Cod_Digitado	int	 	,
		Cod_GrpCli	int	 	,
		Per_DscPrz	numeric(18, 4)	 	,
		Per_DscVis	numeric(18, 4)	 	,
		Qtd_Prz	int	 	,
		Cod_TabPrc	int	 	,
		Num_Referenc	int	 	,
		Flg_GerArqNf	bit	 	,
		Obs_NotFis	text	 	,
		Cod_TipPrp	varchar(3)	 	,
		Obs_IntCtaRe	text	 	,
		Num_CotPdv	int	 	,
		Per_Rnt	numeric(18, 4)	 	,
		Cod_BlqRnt	char(1)	 	,
		Nom_UsuDesbl	varchar(15)	 	,
		Des_LayoutPa	varchar(16)	 	,
		Flg_WMS	bit	 	,
		Flg_CalCom	bit	 	,
		Vlr_Comissao	numeric(18, 4)	 	,
		Vlr_ComTlmkt	numeric(18, 4)	 	,
		Val_ComTra	numeric(18, 4)	 	,
		Cod_BlqLic	char(1)	 	,
		Nom_UsuDesbl	varchar(15)	 	,
		Num_ConReg	int	 	,
		Est_ConReg	varchar(2)	 	,
		Ide_NotEmp	varchar(20)	 	,
		Per_ComOpe	numeric(18, 4)	 	,
		Qtd_PesBru	numeric(18, 4)	 	,
		Hor_Fechamen	smalldatetime	 	,
		Hor_Liberaca	smalldatetime	 	,
		Hor_DbqFin	smalldatetime	 	,
		Hor_DbqLic	smalldatetime	 	,
		Hor_DbqRnt	smalldatetime	 	,
		Dat_LimEnvPr	smalldatetime	 	,
		Vlr_Frete	numeric(18, 4)	 	,
		Cod_PolCom	varchar(25)	 	,
		Id_PolCom	int	 	,
		Vlr_Verba	numeric(18, 4)	 	,
		Ide_Contra	varchar(40)	 	,
		Cod_Contrato	int	 	,
		Num_Sequenci	int	 	,
		Flg_GerCtrFa	bit	 	,
		Flg_CriIteFe	bit	 	,
		Hor_Cancel	smalldatetime	 	,
		Nom_UsuCance	varchar(15)	 	,
		Msg_JustCanc	varchar(80)	 	,
		Vlr_FrePrv	numeric(18, 4)	 	,
		Flg_GerPdvBo	bit	 	,
		Num_PdvLig	int	 	,
		Cod_MtvRej	varchar(3)	 	,
		Des_MtvRej	varchar(40)	 	,
		Per_FrePrv	numeric(18, 4)	 	,
		Cod_Gerencia	int	 	,
		Per_ComGer	numeric(18, 4)	 	,
		Vlr_ComGer	numeric(18, 4)	 	,
		Cod_Supervis	int	 	,
		Per_ComSup	numeric(18, 4)	 	,
		Vlr_ComSup	numeric(18, 4)	 	,
		Cod_GerOpe	int	 	,
		Per_ComGerOp	numeric(18, 4)	 	,
		Vlr_ComGerOp	numeric(18, 4)	 	,
		Cod_SupOpe	int	 	,
		Per_ComSupOp	numeric(18, 4)	 	,
		Vlr_ComSupOp	numeric(18, 4)	 	,
		Vlr_VrbPar	numeric(18, 4)	 	,
		Vlr_VrbBon	numeric(18, 4)	 	,
		Des_LayoutPd	varchar(25)	 	,
		Flg_GerRetBo	bit	 	,
		Des_Convenio	varchar(30)	 	,
		UF_PlaVei	varchar(2)	 	,
		Cod_PlaVei	varchar(10)	 	,
		Flg_PriFat	bit	 	,
		Cod_PrjPde	varchar(12)	 	,
		Hor_ImpEtq	smalldatetime	 	,
		Num_PrtOri	int	 	,
		Vlr_SbtRes	numeric(18, 8)	 	,
		Tip_SaiPdv	varchar(1)	 	,
		Flg_MarkupCu	bit	 	,
		Vlr_DscBon	numeric(18, 4)	 	,
		Ide_NumPrega	varchar(20)	 	,
		CodAnt	int	 	,
		NovoCodigo	int	 	,
		Cod_PedCmpCl	varchar(20)	 	,
		Flg_CnvDscIt	bit	 	,
		Cod_Estabe	int	Not NULL	,
		Cod_LayoutPd	int	 	,
		Tip_RatBon	varchar(1)	 	,
		Vlr_IcmFcpDe	numeric(18, 4)	 	,
		Vlr_Despes	numeric(18, 4)	 	,
		Vlr_IcmParDe	numeric(18, 4)	 	,
		Vlr_IcmParRe	numeric(18, 4)	 	,
		Flg_UsaPbm	bit	 	,
		Obs_IntFecPd	text	 	,
		Num_PdvPai	int	 	,
		Num_PdvFilho	int	 	,
		Per_MarBru	numeric(18, 4)	 	,
		Per_RntBru	numeric(18, 4)	 	,
		Cod_RamAtv	int	 	,
		Vlr_DscBonDu	numeric(18, 4)	 	,
		Sta_IntWms	varchar(1)	 	,
		Cod_MtvDbq	varchar(6)	 	,
		Flg_CtrUsuPd	bit	 	,
		Inscricao_Su	varchar(9)	 	,
		Vlr_IcmsDeso	numeric(18, 4)	Not NULL	,
		Vlr_DscCalSu	numeric(18, 4)	Not NULL	,
		Obs_Padrao	text	 	,
		Tip_Frete	varchar(1)	 	,
		Tip_GerVrb	varchar(1)	 	,
		Vlr_VrbVdr	numeric(18, 4)	 	,
		Vlr_VrbOpe	numeric(18, 4)	 	,
		Vlr_VrbSup	numeric(18, 4)	 	,
		Sta_SepLot	varchar(1)	 	,
		Flg_BlqDebVr	bit	 	,
		Id_Consig	int	 	,
		Num_SeqDev	smallint	 	,
		Id_IncFis	smallint	 	,
		Vlr_DspExt	numeric(18, 4)	 	,
		Vlr_Ipi	numeric(18, 4)	 	,
		Flg_AltUnvIt	bit	 	,
		Vlr_BasIrf	numeric(18, 4)	 	,
		Vlr_Irf	numeric(18, 4)	 	,
		ID_PVMCB	int	 	,
		Vlr_BasCsl	numeric(18, 4)	 	,
		Vlr_Csl	numeric(18, 4),	 
		CONSTRAINT PK_BI_PDVCB PRIMARY KEY (Numero)
	);

/* Tentar comprimir tabela se for possível */
IF Not Exists (Select 0 From sys.indexes i 
	Join sys.partitions p On p.object_id = i.object_id And p.index_id = i.index_id 
	Where i.name = ''PK_BI_PDVCB'' And Object_name(i.object_id) = ''BI_PDVCB'' And p.data_compression_desc = ''PAGE'')
Begin TRY
	EXEC(''ALTER INDEX PK_BI_PDVCB On dbo.BI_PDVCB REBUILD WITH (DATA_COMPRESSION=PAGE)'');
END TRY
Begin CATCH END CATCH

GRANT INSERT On dbo.BI_PDVCB to public;
GRANT CONNECT TO guest;
GRANT INSERT On dbo.BI_PDVCB to guest;

';
Exec (@Cmd);
Go

Declare @Banco_BI Sysname;
Set @Banco_BI = DB_NAME() + '_BI';
Declare @Cmd Nvarchar(Max);
Set @Cmd = 'Create Synonym BI_PDVCB FOR ['+@Banco_BI+'].dbo.BI_PDVCB';

If Exists (Select 0 From sys.synonyms Where name = 'BI_PDVCB')
	Drop Synonym BI_PDVCB;

Exec(@Cmd);
Go


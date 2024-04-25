/* Início cria objetos */


/* Apagar tabelas se existirem */
If Exists (Select 0 From dbo.sysobjects Where id = object_id(N'[dbo].[TBAUDNEW]') And OBJECTPROPERTY(id, N'isTable') = 1)
	Drop Table dbo.TBAUDNEW;
Go

If Exists (Select 0 From dbo.sysobjects Where id = object_id(N'[dbo].[TBAUDNEW_EXC]') And OBJECTPROPERTY(id, N'isTable') = 1)
	Drop Table dbo.TBAUDNEW_EXC;
Go

Declare @DW_BI Sysname;
Set @DW_BI = DW + '_BI';

If Not Exists (Select 0 From sys.databases Where name = @DW_BI)
Begin
	Declare @Diretorio NVarchar(Max);
	Select @Diretorio = Reverse(Substring(Reverse(filename), Charindex('\', Reverse(filename)), Len(filename))) 
	From sysfiles Where fileid = 1;

	Declare @Cmd NVarchar(Max);
	Set @Cmd = 'CREATE DATABASE ['+@DW_BI+']
	 On  PRIMARY 
	( NAME = N'''+@DW_BI+'_1'', FILENAME = N'''+@Diretorio+@DW_BI+'.mdf'', SIZE = 100MB , FILEGROWTH = 100MB )
	 LOG On 
	( NAME = N'''+@DW_BI+'_log'', FILENAME = N'''+@Diretorio+@DW_BI+'_log.ldf'', SIZE = 512MB , FILEGROWTH = 512MB );
	ALTER DATABASE ['+@DW_BI+'] Set RECOVERY SIMPLE;
	ALTER DATABASE ['+@DW_BI+'] Set PAGE_VERIFY CHECKSUM;
	ALTER AUTHORIZATION On DATABASE::['+@DW_BI+'] TO [sa]
	';
	Exec(@Cmd);
End;
Go

Declare @DW_BI Sysname;
Set @DW_BI = DW + '_BI';

-- Criar as tabelas:

Declare @Cmd NVarchar(Max);
Set @Cmd = N'Use [' + @DW_BI + ']
If Not Exists (Select 0 From dbo.sysobjects Where id = object_id(N''[dbo].[TBAUDNEW]'') And OBJECTPROPERTY(id, N''isTable'') = 1)
	CREATE TABLE dbo.TBAUDNEW (
		Id bigint identity(1,1) Not NULL,
		Des_Tabela Varchar(128) NULL,
		Dat_Evento datetime2(0) Not NULL,
		Chave Varchar(2000) Not NULL,
		Des_Coluna Varchar(128) NULL,
		Des_OldData Varchar(8000) NULL,
		Des_NewData Varchar(8000) NULL,
		Usuario Varchar(128) NULL,
		Des_App Varchar(128) NULL,
		Des_HostName Varchar(128) NULL,
		Operacao char(1) NULL, 
		CONSTRAINT PK_TBAUDNEW PRIMARY KEY (Id)
	);

If Not Exists (Select 0 From dbo.sysobjects Where id = object_id(N''[dbo].[TBAUDNEW_EXC]'') And OBJECTPROPERTY(id, N''isTable'') = 1)
	CREATE TABLE dbo.TBAUDNEW_EXC (
		Id int identity(1,1) Not NULL,
		Des_Tabela Varchar(128) NULL,
		Des_Coluna Varchar(128) NULL,
		CONSTRAINT PK_TBAUDNEW_EXC PRIMARY KEY (Id)
	);

If Not Exists (Select 0 From dbo.sysobjects Where id = object_id(N''[dbo].[TBAUDNEW_SPID]'') And OBJECTPROPERTY(id, N''isTable'') = 1)
Begin
	CREATE TABLE dbo.TBAUDNEW_SPID (
		Id int identity(1,1) Not NULL,
		Usuario Varchar(128) NULL,
		Des_Spid Smallint NULL,
		DataHora Datetime2(0) Not Null Constraint DF_TBAUDNEW_SPID_DataHora Default GETDATE(),
		CONSTRAINT PK_TBAUDNEW_SPID PRIMARY KEY (Id)
	);

	Create Index Idx_TBAUDNEW_SPID_Des_Spid_Usuario
	ON TBAUDNEW_SPID (Des_Spid, Usuario);
End;

/* Tentar comprimir tabela se for possível */
IF Not Exists (Select 0 From sys.indexes i 
	Join sys.partitions p On p.object_id = i.object_id And p.index_id = i.index_id 
	Where i.name = ''PK_TBAUDNEW'' And Object_name(i.object_id) = ''TBAUDNEW'' And p.data_compression_desc = ''PAGE'')
Begin TRY
	EXEC(''ALTER INDEX PK_TBAUDNEW On dbo.TBAUDNEW REBUILD WITH (DATA_COMPRESSION=PAGE)'');
END TRY
Begin CATCH END CATCH

GRANT INSERT On dbo.TBAUDNEW to public;
GRANT SELECT On dbo.TBAUDNEW_EXC to public;
GRANT SELECT On dbo.TBAUDNEW_SPID to public;

GRANT CONNECT TO guest;
GRANT INSERT On dbo.TBAUDNEW to guest;
GRANT SELECT On dbo.TBAUDNEW_EXC to guest;
GRANT SELECT On dbo.TBAUDNEW_SPID to guest;
';

--Alimentar as tabelas:

Exec (@Cmd);
Go

Declare @DW_BI Sysname;
Set @DW_BI = DW + '_BI';
Declare @Cmd Nvarchar(Max);
Set @Cmd = 'Create Synonym TBAUDNEW FOR ['+@DW_BI+'].dbo.TBAUDNEW';

If Exists (Select 0 From sys.synonyms Where name = 'TBAUDNEW')
	Drop Synonym TBAUDNEW;

Exec(@Cmd);
Go


Declare @DW_BI Sysname;
Set @DW_BI = DW + '_BI';
Declare @Cmd Nvarchar(Max);
Set @Cmd = 'Create Synonym TBAUDNEW_EXC FOR ['+@DW_BI+'].dbo.TBAUDNEW_EXC';

If Exists (Select 0 From sys.synonyms Where name = 'TBAUDNEW_EXC')
	Drop Synonym TBAUDNEW_EXC;

Exec(@Cmd);
Go

Declare @DW_BI Sysname;
Set @DW_BI = DW + '_BI';
Declare @Cmd Nvarchar(Max);
Set @Cmd = 'Create Synonym TBAUDNEW_SPID FOR ['+@DW_BI+'].dbo.TBAUDNEW_SPID';

If Exists (Select 0 From sys.synonyms Where name = 'TBAUDNEW_SPID')
	Drop Synonym TBAUDNEW_SPID;

Exec(@Cmd);
Go

If Exists (Select 0 From dbo.sysobjects Where id = object_id(N'[dbo].[PR_BI]') And objectproperty(id, N'IsProcedure') = 1)
	Drop Procedure dbo.PR_CriaTR_BINew
Go

Create Procedure dbo.PR_CriaTR_BINew @Tabela Varchar(128), @ApenasAtualizaExistente Bit = 0
As
Begin
	Set Nocount On;
	Set Xact_Abort On;

	IF Not Exists (Select 0 From dbo.sysobjects Where id = object_id(@Tabela) And objectproperty(id, N'isTable') = 1) 
	Begin
		Raiserror ('A tabela passada como parâmetro não existe.', 17, 1);
	End
	Else
	Begin

		Declare @cmdCriacaoTrigger Varchar(Max) = '';
		Declare @TabelaAudit Varchar(128) = '';
		Declare @CreateTableTipoDados Varchar(Max) = '';
		Declare @CondicaoJuncao Varchar(Max) = '';
		Declare @ColunasSeparadasPorVirgula Varchar(Max) = '';
		Declare @ColunasAtribuidasSeparadasPorVirgulaInsert Varchar(Max) = '';
		Declare @ColunasAtribuidasSeparadasPorVirgulaDelete Varchar(Max) = '';
		Declare @TableName Varchar(250) = (Parsename(@Tabela, 1));
		Declare @SchemaName Varchar(250) = (Isnull(Parsename(@Tabela, 2),'dbo'));

		Set @TabelaAudit = 'dbo.' + Quotename('TBAUDNEW');

		Select 
			@ColunasAtribuidasSeparadasPorVirgulaInsert = Stuff((
				Select '+ ''|'' + Cast(' + 'i.' + c.name + ' AS Varchar(2000))' 
				From sys.index_columns ic 
				Join sys.columns c On (ic.object_id = c.object_id And ic.column_id = c.column_id)
				Join sys.types ty On (c.system_type_id = ty.system_type_id And c.user_type_id = ty.user_type_id)
				Where ic.index_id = i.index_id And ic.object_id = i.object_id 
				Order By ic.column_id
				For XML Path('')), 1, 8, ''
			),
			@ColunasAtribuidasSeparadasPorVirgulaDelete = Stuff((
				Select '+ ''|'' + Cast(' + 'd.' + c.name + ' AS Varchar(2000))' 
				From sys.index_columns ic 
				Join sys.columns c On (ic.object_id = c.object_id And ic.column_id = c.column_id)
				Join sys.types ty On (c.system_type_id = ty.system_type_id And c.user_type_id = ty.user_type_id)
				Where ic.index_id = i.index_id And ic.object_id = i.object_id 
				Order By ic.column_id
				For XML Path('')), 1, 8, ''
			),
			@ColunasSeparadasPorVirgula = Stuff((
				Select ', ' + Quotename(c.name)
				From sys.index_columns ic 
				Join sys.columns c On (ic.object_id = c.object_id And ic.column_id = c.column_id)
				Join sys.types ty On (c.system_type_id = ty.system_type_id And c.user_type_id = ty.user_type_id)
				Where ic.index_id = i.index_id And ic.object_id = i.object_id 
				Order By ic.column_id
				FOR XML PATH('')), 1, 2, ''
			),
			@CondicaoJuncao = Stuff((
				Select ' And ' + 'i.' + c.name + ' = ' + 'd.' + c.name 
				From sys.index_columns ic 
				Join sys.columns c On (ic.object_id = c.object_id And ic.column_id = c.column_id)
				Join sys.types ty On (c.system_type_id = ty.system_type_id And c.user_type_id = ty.user_type_id)
				Where ic.index_id = i.index_id And ic.object_id = i.object_id 
				Order By ic.column_id
				FOR XML PATH('')), 1, 5, ''
			)
		From sys.tables t 
		Join sys.schemas s On (t.schema_id = s.schema_id)
		Join sys.indexes i On (t.object_id = i.object_id) 
		Where  s.name = @SchemaName 
			And t.name = @TableName 
			And i.is_primary_key = 1;

		Set @cmdCriacaoTrigger = ' Trigger TRG_AUDNEW_' + @Tabela + ' On ' + @Tabela + ' After Insert, Update, Delete 
As
Begin
	Set Nocount On;
	Declare @USUARIO Varchar(128);

	If Object_id(''FN_UsuarioInfarma'') Is Not Null
		Exec @Usuario = FN_UsuarioInfarma;

	Set @Usuario = ISNULL(@Usuario, SUSER_NAME());

	If Exists (Select 0 From dbo.TBAUDNEW_SPID Where Des_Spid = @@SPID And Usuario = @Usuario)
		Return;

 	If (Exists (Select 1 From Inserted) And Not Exists (Select 1 From Deleted)) 
	Begin
		Insert Into ' + @TabelaAudit + ' (Des_Tabela, Dat_Evento, Chave, Des_Coluna, Des_OldData, Des_NewData, Usuario, Des_App, Des_HostName, Operacao)
			Select  ''' + @Tabela + ''', GETDATE(), $$COLUNASATRIBUIDASSEPARADASPORVIRGULAINSERT$$, '''', null, null, @Usuario, APP_NAME(), HOST_NAME(), ''I'' From Inserted i;
	End 
	Else If Exists (Select 1 From Deleted)
	Begin
		Insert Into ' + @TabelaAudit + ' (Des_Tabela, Dat_Evento, Chave, Des_Coluna, Des_OldData, Des_NewData, Usuario, Des_App, Des_HostName, Operacao)
		Select ''' + @Tabela + ''', GETDATE(), $$COLUNASATRIBUIDASSEPARADASPORVIRGULADELETE$$, t.Coluna, t.Valor_Antigo, t.Valor_Novo, @Usuario, APP_NAME(), HOST_NAME(),
			CASE WHEN t.Valor_Antigo IS NULL THEN ''I''
							WHEN t.Valor_Novo IS NULL   THEN ''D''
							ELSE ''U''
							END OPERACAO
		From Deleted d 
		Left Join Inserted i On ($$CONDICAO_JUNCAO$$)
		Cross Apply
		(Values
			$$COLUNAS_BIAVEIS$$
		) t (Coluna, Valor_Antigo, Valor_Novo)
		Where Not Exists ((Select Valor_Antigo Intersect Select Valor_Novo))
		And (NullIf(Valor_Antigo, '''') Is Not Null Or NullIf(Valor_Novo, '''') Is Not Null);
	End;
End';
 
	Declare @colunas Varchar(Max) = '';
	Select @colunas += 
		Case When ty.name In ('Varchar','char')
			Then ',(''' + c.name +''',d.' + c.name + ', i.' + c.name + ')' + Char(13) + Char(9) + Char(9) + Char(9)
			Else ',(''' + c.name +''',CAST(d.' + c.name + ' AS Varchar(8000)), CAST(i.' + c.name + ' AS Varchar(8000)))' + Char(13) + Char(9) + Char(9) + Char(9)
		End
	From sys.columns c Join sys.types ty On (c.system_type_id = ty.system_type_id And c.user_type_id = ty.user_type_id)
	Where c.object_id = OBJECT_ID(@Tabela) 
	And ty.name Not IN ('ntext', 'text', 'image') 
	And Not Exists (Select 0 From dbo.TBAUDNEW_EXC Where Object_Id(Des_tabela) = c.object_id And Des_coluna = c.name);

	Set @colunas = Stuff(@colunas, 1, 1, '');
 
    Set @cmdCriacaoTrigger = Replace(@cmdCriacaoTrigger,'$$COLUNASATRIBUIDASSEPARADASPORVIRGULAINSERT$$',@ColunasAtribuidasSeparadasPorVirgulaInsert);
    Set @cmdCriacaoTrigger = Replace(@cmdCriacaoTrigger,'$$COLUNASATRIBUIDASSEPARADASPORVIRGULADELETE$$',@ColunasAtribuidasSeparadasPorVirgulaDelete);
    Set @cmdCriacaoTrigger = Replace(@cmdCriacaoTrigger,'$$CONDICAO_JUNCAO$$',@CondicaoJuncao);
    Set @cmdCriacaoTrigger = Replace(@cmdCriacaoTrigger,'$$COLUNAS_BIAVEIS$$',@colunas);
 
    If @ApenasAtualizaExistente = 1
    Begin
        If Exists (Select 1 From sys.triggers Where name = 'TRG_AUDNEW_' + @Tabela And is_disabled = 0)
        Begin
            Exec('Alter ' + @cmdCriacaoTrigger);
        End
    End
    Else
    Begin 
        If Exists (Select 1 From sys.triggers Where name = 'TRG_AUDNEW_' + @Tabela And is_disabled = 0)
        Begin
            Exec('Alter ' + @cmdCriacaoTrigger);
        End
        Else
        Begin
            Exec('Create ' + @cmdCriacaoTrigger);
        End
    End
	End;
End;
Go

/*
  Versao 20.11
  Fevereiro de 2022
  Tabela: TBAUDNEW_EXC
  Adicionar Campos Exceções a auditoria
*/
if not Exists(Select 1 From TBAUDNEW_EXC Where Des_tabela = 'PDVCB' And Des_coluna = 'Flg_calc')
	Insert Into dbo.TBAUDNEW_EXC (Des_tabela, Des_coluna) values ('PDVCB', 'Flg_calc')

if not Exists(Select 1 From TBAUDNEW_EXC Where Des_tabela = 'USUAR' And Des_coluna = 'Qtd_TenLogInv')
	Insert Into dbo.TBAUDNEW_EXC (Des_tabela, Des_coluna) values ('USUAR', 'Qtd_TenLogInv')

if not Exists(Select 1 From TBAUDNEW_EXC Where Des_tabela = 'PARAM' And Des_coluna = 'DatUltAtuModExt')
	Insert Into dbo.TBAUDNEW_EXC (Des_tabela, Des_coluna) values ('PARAM', 'DatUltAtuModExt')	





/*
  Versao 20.11
  Fevereiro de 2022
  Tabela: TBXAD
  Adicionar Campos Exceções a auditoria
*/
if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'CLIEN')
	Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita) Values ('CLIEN','Clientes',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'CPVCB')
	Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('CPVCB','Conferências de Pedidos - Cabeçalho',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'CPVIT')
	Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('CPVIT','Conferências de Pedidos - Itens',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'CTREC')
	Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('CTREC','Contas a Receber',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'DPXPR')
	Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('DPXPR','Endereço/Local. de Produtos - Picking',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'ESTAB')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('ESTAB','Estabelecimentos',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'NFECB')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('NFECB','Notas Fiscais de Entrada - Cabeçalho',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'NFSCB')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('NFSCB','Notas Fiscais de Saídas - Cabeçalho',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PAGBX')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PAGBX','Baixas de Pagamentos',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PAGCT')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PAGCT','Contas a Pagar',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PDVCB')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PDVCB','Pedidos de Vendas - Cabeçalho',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PRLOT')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PRLOT','Lotes de Produtos - Picking',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PRLTL')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PRLTL','Lotes de Produtos - Depósito(Pulmão)',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PRODU')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PRODU','Produtos',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'FORNE')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('FORNE','Fornecedores',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'CLTRI')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('CLTRI','Classif. Tributárias',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'FABRI')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('FABRI','Fabricantes',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'VENDE')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('VENDE','Vendedores',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'VEXES')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('VEXES','Vendedores por Estabelecimento',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'TBCOM')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('TBCOM','Tabelas de Comissões - Geral',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'TPOPE')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('TPOPE','Tipos de Transações',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'AGCOB')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('AGCOB','Agentes Cobradores',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'ENXES')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('ENXES','Entidades por Estabelecimentos',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'POCOM')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('POCOM','Políticas de Comercialização - Geral',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PCXPR')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PCXPR','Políticas de Comercialização - Produtos',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PCXVE')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PCXVE','Políticas de Comercialização - Vendedores',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'RGTRI')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('RGTRI','Região Tributária',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'RTXCT')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('RTXCT','Regimes de Tributação',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PARAM')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PARAM','Parâmetros Gerais',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PMMES')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PMMES','Parâmetros Mensais',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PMEST')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PMEST','Parâmetros de Estoque',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'TBPRC')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('TBPRC','Tabelas de Preços',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'TPXPR')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('TPXPR','Tab. Preços x Produtos',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'BLQVD')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('BLQVD','Bloqueios de Vendas',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'ACERT')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('ACERT','Ajustes/Trocas Lotes de Estoque',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'LANCB')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('LANCB','Lançamentos Financeiros',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'CTPAR')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('CTPAR','Contas Partidas Financeiras',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'USUAR')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('USUAR','Usuários',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'TBORP')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('TBORP','Origens de Pedidos de Vendas',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'BXREC')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('BXREC','Recebimentos de Títulos',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'TPPAR')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('TPPAR','Tabelas de Preços - Parâmetros',0)

if not Exists(Select 1 From TBXAD Where Nom_Tabela = 'PMEML')
  Insert Into dbo.TBXAD  (Nom_Tabela, Des_Tabela, Flg_Audita)	Values ('PMEML','E-mails - Parâmetros',0)
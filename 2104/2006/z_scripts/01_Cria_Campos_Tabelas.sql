/*
Campos a excluir: 

Select Nom_Arquiv, Des_Motivo, Dat_Inicio, Vlr_TotalDscVis, Vlr_TotalDscPrz, Sta_PedAnt
From PDECB

Select Cod_Client
From PDEIT


Campos criados fora do padrão neste script:
PCXPR.IndEstabeMkp
PCXPR.IndPrcCusBasMkp
*/

/*
  Versao 19.12
  Dezembro de 2021
  Tabela: PARAM
  Campo : FlgEmbDscDesoneItePrcUni: sinalizador para embutir desc. desoneração no preçp unitário
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PARAM'
                  And Column_Name = 'FlgEmbDscDesoneItePrcUni')
  ALTER TABLE dbo.PARAM ADD FlgEmbDscDesoneItePrcUni bit NULL default 0
GO

--------------------------------------------------------------------------------------------------
/*
  Versao 19.12
  Janeiro de 2022
  Tabela: CTREC
  Campo : Per_Projec (% Projeção do Mês)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'CTREC'
               And Column_Name = 'Per_Projec')
  ALTER TABLE dbo.CTREC ADD Per_Projec numeric(18,4) NULL default 0
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: CTREC
  Campo : Vlr_Projec (Valor da Projeção do Mês)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'CTREC'
               And Column_Name = 'Vlr_Projec')
  ALTER TABLE dbo.CTREC ADD Vlr_Projec numeric(18,4) NULL default 0
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: NGTCB
  Campo : Per_Projec (% Projeção do Mês)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NGTCB'
               And Column_Name = 'Per_Projec')
  ALTER TABLE dbo.NGTCB ADD Per_Projec numeric(18,4) NULL default 0
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: NGTIT
  Campo : Per_Projec (% Projeção do Mês)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NGTIT'
               And Column_Name = 'Per_Projec')
  ALTER TABLE dbo.NGTIT ADD Per_Projec numeric(18,4) NULL default 0
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: NGTIT
  Campo : Vlr_Projec (Valor da Projeção do Mês)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NGTIT'
               And Column_Name = 'Vlr_Projec')
  ALTER TABLE dbo.NGTIT ADD Vlr_Projec numeric(18,4) NULL default 0
GO

----------------------------------------------------------------------------------------------------------------------------------------
/*
  Versao 19.12
  Janeiro de 2022
  Tabela: LS_PARAM: Guarda valores de propriedades de objetos
*/
if not exists (select 1 from dbo.sysobjects where id = object_id(N'[dbo].[LS_PARAM]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
  CREATE TABLE [dbo].[LS_PARAM](
		[Grp_Param] [varchar](100) NOT NULL,
		[Obj_Param] [varchar](200) NOT NULL,
		[Nom_Param] [varchar](500) NOT NULL,
		[Val_Param] [ntext] NULL,
		[Des_Param] [varchar](500) NULL,
  ) ON [PRIMARY] 
go
if not Exists (select name from sysobjects where name = 'PK_LS_PARAM')
  ALTER TABLE [dbo].[LS_PARAM] ADD CONSTRAINT [PK_LS_PARAM] PRIMARY KEY CLUSTERED(
    [Grp_Param] ASC,
	  [Obj_Param] ASC,
	  [Nom_Param] ASC
  )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80) ON [PRIMARY]
GO

--------------------------------------------------------------------------------------------------------------------------------------

-- criar indices CTREC, FS_NFXML ----------------
DECLARE @IndexName VARCHAR(256)
      , @NumSeqIdx tinyint
	  , @SqlCmd NVARCHAR(MAX)

-- CTREC
if Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'CTREC'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Ser_NfOrigem, Num_NfOrigem')
  begin
	Set @IndexName = (SELECT i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'CTREC'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Ser_NfOrigem, Num_NfOrigem')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.CTREC') And Name = @IndexName)
      begin
        Select @SqlCmd= 'DROP INDEX '+QUOTENAME(schema_name(t.schema_id))+'.'+QUOTENAME(t.name)+'.'+QUOTENAME(i.name)
          From sys.indexes i INNER JOIN
               sys.tables t ON t.object_id = i.object_id
         Where i.type > 0
           And t.is_ms_shipped = 0 
           And t.name <> 'sysdiagrams'
           And i.name = @IndexName
           And (i.is_primary_key = 0 and i.is_unique_constraint = 0)
        if @@rowcount > 0
          begin
            EXECUTE sp_executesql @SqlCmd
          end
      end

  end

if Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'CTREC'
			 AND i.name not like 'IX_CTREC_%' -- aqui
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Num_NfOrigem, Ser_NfOrigem, Cod_Estabe')
  begin
	Set @IndexName = (SELECT i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'CTREC'
                         AND i.name not like 'IX_CTREC_%' -- aqui
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Num_NfOrigem, Ser_NfOrigem, Cod_Estabe')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.CTREC') And Name = @IndexName)
      begin
        Select @SqlCmd= 'DROP INDEX '+QUOTENAME(schema_name(t.schema_id))+'.'+QUOTENAME(t.name)+'.'+QUOTENAME(i.name)
          From sys.indexes i INNER JOIN
               sys.tables t ON t.object_id = i.object_id
         Where i.type > 0
           And t.is_ms_shipped = 0 
           And t.name <> 'sysdiagrams'
           And i.name = @IndexName
           And (i.is_primary_key = 0 and i.is_unique_constraint = 0)
        if @@rowcount > 0
          begin
            EXECUTE sp_executesql @SqlCmd
          end
      end
  end

if not Exists(SELECT i.name
                FROM sysobjects   o INNER JOIN 
                     sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                     sys.indexes  i ON o.id = i.object_id
               WHERE so.type = 'U'
                 AND o.Name = 'CTREC'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Ser_NfOrigem, Num_NfOrigem')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_CTREC_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_CTREC_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[CTREC] ([Cod_Estabe],[Ser_NfOrigem],[Num_NfOrigem]) '+
                  'INCLUDE ([Vlr_Documento], [Vlr_DescConced], [Vlr_DscBonDup], [Vlr_SbtEmb], [Vlr_ResEmb])'
    EXECUTE sp_executesql @SqlCmd
  end

/*
-- FS_NFXML
if Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'FS_NFXML'
			 AND i.name not like 'IX_FS_NFXML_%' -- aqui
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Seq_Xml')
  begin
	Set @IndexName = (SELECT i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'FS_NFXML'
                         AND i.name not like 'IX_FS_NFXML_%' 
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                          sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Seq_Xml')
             
    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.FS_NFXML') And Name = @IndexName)
      begin
        Select @SqlCmd= 'DROP INDEX '+QUOTENAME(schema_name(t.schema_id))+'.'+QUOTENAME(t.name)+'.'+QUOTENAME(i.name)
          From sys.indexes i INNER JOIN
               sys.tables t ON t.object_id = i.object_id
         Where i.type > 0
           And t.is_ms_shipped = 0 
           And t.name <> 'sysdiagrams'
           And i.name = @IndexName
           And (i.is_primary_key = 0 and i.is_unique_constraint = 0)
        if @@rowcount > 0
          begin
            EXECUTE sp_executesql @SqlCmd
          end
      end
  end

if not Exists(SELECT i.name
                FROM sysobjects   o INNER JOIN 
                     sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                     sys.indexes  i ON o.id = i.object_id
               WHERE so.type = 'U'
                 AND o.Name = 'FS_NFXML'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Seq_Xml')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_FS_NFXML_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_FS_NFXML_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[FS_NFXML] ([Seq_Xml]) '
    EXECUTE sp_executesql @SqlCmd
  end
*/
GO
--------------------------------------------------------------------------------------------------------------------------------------

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: AJICM: criar chave primaria
*/
if not Exists (select name from sysobjects where name = 'PK_AJICM')
  ALTER TABLE [dbo].[AJICM] ADD CONSTRAINT [PK_AJICM] PRIMARY KEY CLUSTERED(
    [Codigo] ASC,
    [Versao] ASC,
    [UF] ASC
  )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80) ON [PRIMARY]
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: AJICM
  Campo: Tip_SubApuIcm 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'AJICM'
               And Column_Name = 'Tip_SubApuIcm')
  ALTER TABLE dbo.AJICM ADD Tip_SubApuIcm char(2) NULL
GO
/*
  Versao 19.12
  Janeiro de 2022
  Tabela: AJICM 
  Campo: Dat_Ini  
*/
IF Not EXISTS(Select Column_Name from Information_Schema.columns
              Where Table_Name = 'AJICM'
              And Column_Name = 'Dat_Ini')
  ALTER TABLE dbo.AJICM ADD Dat_Ini smalldatetime NULL
GO
/*
  Versao 19.12
  Janeiro de 2022
  Tabela: AJICM 
  Campo: Dat_Fim  
*/
IF Not EXISTS(Select Column_Name from Information_Schema.columns
              Where Table_Name = 'AJICM'
              And Column_Name = 'Dat_Fim')
  ALTER TABLE dbo.AJICM ADD Dat_Fim smalldatetime NULL
GO
/*
  Versao 19.12
  Janeiro de 2022
  Tabela: AJICM
  Campo: Id_Lote 
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'AJICM'
               And Column_Name = 'Id_Lote')
  ALTER TABLE dbo.AJICM ADD Id_Lote int NULL
GO

/*
  Versao 19.12
  Dezembro DE 2021
  Tabela: IFXGI
  Campo Cod_AjuSped: cod. ajustes SPED informações adicionais
*/
Exec PR_ADM_ExcluiCampo 'TBIFS', 'Cod_AjuSped'
GO
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'IFXGI'
                  And Column_Name = 'Cod_AjuSped')
  ALTER TABLE dbo.IFXGI ADD Cod_AjuSped varchar(8) NULL
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: PARSP
  Campo : FlgGerC177 (Flag que indica se deve gerar o registro C170/C177)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PARSP'
               And Column_Name = 'FlgGerC177')
  ALTER TABLE dbo.PARSP ADD FlgGerC177 Bit null
GO

/*
  Versao 20.01
  Janeiro de 2022
  Tabela: RPCPF: Relação de Pedidos de Compras a Partir de Faltas de Vendas
*/
if not exists (select 1 from dbo.sysobjects where id = object_id(N'[dbo].[RPCPF]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[RPCPF](
	[ID] [int] IDENTITY(1,1) NOT NULL,
	[Cod_Estabe] [int] NOT NULL,
	[Cod_PedidoVenda] [int] NOT NULL,
	[Cod_PedidoCompra] [int] NOT NULL,
	[Cod_Produto] [int] NOT NULL,
	[Qtd_FalOrigem] [int] NOT NULL,
	[Qtd_PedCompra] [int] NOT NULL,
  ) ON [PRIMARY] 
GO

if not Exists (select name from sysobjects where name = 'PK_RPCPF')
  ALTER TABLE [dbo].[RPCPF] ADD CONSTRAINT [PK_RPCPF] PRIMARY KEY CLUSTERED(
    [ID] ASC
  )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80) ON [PRIMARY]
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: PDVCB
  Campo : Flg_AltUnvItePdv (Flag que indica utiliza unidade minima)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PDVCB'
               And Column_Name = 'Flg_AltUnvItePdv')
  ALTER TABLE dbo.PDVCB ADD Flg_AltUnvItePdv Bit null
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: ENDDP
  Campo: Id_EndDep
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'ENDDP'
               And Column_Name = 'Id_EndDep')
  ALTER TABLE dbo.ENDDP ADD Id_EndDep int IDENTITY(1,1) NOT NULL
GO

-------------------------------------------------------------------------------------------------------------------------------------

-- criar indice ENDDP ----------------
DECLARE @IndexName VARCHAR(256)
      , @NumSeqIdx tinyint
	  , @SqlCmd NVARCHAR(MAX)

 while Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'ENDDP'
			 AND i.name not like 'IX_ENDDP_%' -- aqui
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Id_EndDep')  
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'ENDDP'
                         AND i.name not like 'IX_ENDDP_%' -- aqui
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Id_EndDep')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.ENDDP') And Name = @IndexName)
      begin
        Select @SqlCmd= 'DROP INDEX '+QUOTENAME(schema_name(t.schema_id))+'.'+QUOTENAME(t.name)+'.'+QUOTENAME(i.name)
          From sys.indexes i INNER JOIN
               sys.tables t ON t.object_id = i.object_id
         Where i.type > 0
           And t.is_ms_shipped = 0 
           And t.name <> 'sysdiagrams'
           And i.name = @IndexName
           And (i.is_primary_key = 0 and i.is_unique_constraint = 0)
        if @@rowcount > 0
          begin
            EXECUTE sp_executesql @SqlCmd
          end
      end
  end

if not Exists(SELECT i.name
                FROM sysobjects   o INNER JOIN 
                     sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                     sys.indexes  i ON o.id = i.object_id
               WHERE so.type = 'U'
                 AND o.Name = 'ENDDP'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Id_EndDep')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_ENDDP_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_ENDDP_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[ENDDP] ([Id_EndDep]) '

    EXECUTE sp_executesql @SqlCmd
  end
-------------------------------------------------------------------------------------------------------------------------------------

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: NFECB
  Campo: Id_EndDep
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Id_EndDep')
  ALTER TABLE dbo.NFECB ADD Id_EndDep int NULL default 0
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: PDVIT
  Campo : Per_RepIcms (% Repasse Icms)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PDVIT'
               And Column_Name = 'Per_RepIcms')
  ALTER TABLE dbo.PDVIT ADD Per_RepIcms numeric(7,4) NULL default 0
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: NFSIT
  Campo : Per_RepIcms (% Repasse Icms)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFSIT'
               And Column_Name = 'Per_RepIcms')
  ALTER TABLE dbo.NFSIT ADD Per_RepIcms numeric(7,4) NULL default 0
GO

/*
  Versao 19.12
  Janeiro de 2022
  Tabela: NFEIT
  Campo : Per_RepIcm (% Repasse Icms)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFEIT'
               And Column_Name = 'Per_RepIcm')
  ALTER TABLE dbo.NFEIT ADD Per_RepIcm numeric(7,4) NULL default 0
GO

/*
  Versao 19.12
  Fevereiro de 2022
  Fevereiro: ESTAB 
  Campo: Par_FlgPolComPdvTrm - Trabalhar com Política de Comercialização no TRM e PDV
*/
IF not EXISTS(Select Column_Name from Information_Schema.columns
              Where Table_Name = 'ESTAB'
              And Column_Name = 'Par_FlgPolComPdvTrm')
  ALTER TABLE dbo.ESTAB add Par_FlgPolComPdvTrm bit NULL default 0
GO


-- criar indices  ----------------
DECLARE @IndexName VARCHAR(256)
      , @NumSeqIdx tinyint
	  , @SqlCmd NVARCHAR(MAX)


 -- ABACB
 while Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'ABACB'
			 AND i.name not like 'IX_ABACB_%' -- aqui
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Cod_Pro, Cod_Sta')  
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'ABACB'
                         AND i.name not like 'IX_ABACB_%' -- aqui
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Cod_Pro, Cod_Sta')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.ABACB') And Name = @IndexName)
      begin
        Select @SqlCmd= 'DROP INDEX '+QUOTENAME(schema_name(t.schema_id))+'.'+QUOTENAME(t.name)+'.'+QUOTENAME(i.name)
          From sys.indexes i INNER JOIN
               sys.tables t ON t.object_id = i.object_id
         Where i.type > 0
           And t.is_ms_shipped = 0 
           And t.name <> 'sysdiagrams'
           And i.name = @IndexName
           And (i.is_primary_key = 0 and i.is_unique_constraint = 0)
        if @@rowcount > 0
          begin
            EXECUTE sp_executesql @SqlCmd
          end
      end
  end

if not Exists(SELECT i.name
                FROM sysobjects   o INNER JOIN 
                     sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                     sys.indexes  i ON o.id = i.object_id
               WHERE so.type = 'U'
                 AND o.Name = 'ABACB'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Cod_Pro, Cod_Sta')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_ABACB_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_ABACB_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[ABACB] ([Cod_Estabe], [Cod_Pro], [Cod_Sta]) '

    EXECUTE sp_executesql @SqlCmd
  end
-------------------------------------------------------------------------------------------------------------------------------------
GO

/*
  Versao 19.12
  Março DE 2022
  Tabela: ORCCB
  Campo Id_PolCom
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'ORCCB'
                  And Column_Name = 'Id_PolCom')
  ALTER TABLE dbo.ORCCB ADD Id_PolCom int null default 0
GO

/*
  Versao 19.12
  Dezembro DE 2021
  Tabela: ORCIT
  Campo Id_PolCom
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'ORCIT'
                  And Column_Name = 'Id_PolCom')
  ALTER TABLE dbo.ORCIT ADD Id_PolCom int null default 0
GO


/*
  Versao 19.12
  Dezembro DE 2021
  Tabela: PDVIT/PDEIT
  Campo Des_MtvRej alteração de tamanho 40 para 80
*/
if Exists (select 1 from Information_Schema.columns
            Where Table_Name = 'PDVIT'
              And Column_Name = 'Des_MtvRej'
			  And DATA_TYPE = 'varchar'
              And CHARACTER_MAXIMUM_LENGTH < 80)
  ALTER TABLE dbo.PDVIT ALTER COLUMN Des_MtvRej varchar(80) NULL
GO
if Exists (select 1 from Information_Schema.columns
            Where Table_Name = 'PDEIT'
              And Column_Name = 'Des_MtvRej'
			  And DATA_TYPE = 'varchar'
              And CHARACTER_MAXIMUM_LENGTH < 80)
  ALTER TABLE dbo.PDEIT ALTER COLUMN Des_MtvRej varchar(80) NULL
GO

/*
  Versao 19.12
  Dezembro de 2021
  Tabela: POCOM
  Campo : Flg_UsaTabPrcCadCli: sinalizador para sempre utilizar tabela de preços do cadastro de clientes
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'POCOM'
                  And Column_Name = 'Flg_UsaTabPrcCadCli')
  ALTER TABLE dbo.POCOM ADD Flg_UsaTabPrcCadCli bit NULL default 0
GO


/*
  Versao 20.04
  Abril de 2022
  Tabela: PARAM
  Campo : FlgDbqPrdVenAutEntCmp (Flag que desbloqueia o produto p/ venda automaticamente na entrada por compra)
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'PARAM'
               And Column_Name = 'FlgDbqPrdVenAutEntCmp')
  ALTER TABLE dbo.PARAM ADD FlgDbqPrdVenAutEntCmp Bit null
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: PRSLD
  Campo Qtd_SldCng: quantidade em consignação
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PRSLD'
                  And Column_Name = 'Qtd_SldCng')
  ALTER TABLE dbo.PRSLD ADD Qtd_SldCng int null default 0
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: TPOPE
  Campo Transacao
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'TPOPE'
                  And Column_Name = 'Transacao')
  ALTER TABLE dbo.TPOPE ADD Transacao smalldatetime null
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: OPVDO
  Campo Dat_Operac : cadastro da operação
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'OPVDO'
                  And Column_Name = 'Dat_Operac')
  ALTER TABLE dbo.OPVDO ADD Dat_Operac smalldatetime null
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: PARAM
  Campo : FlgExbLgpdCadCli: sinalizador para 
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PARAM'
                  And Column_Name = 'FlgExbLgpdCadCli')
  ALTER TABLE dbo.PARAM ADD FlgExbLgpdCadCli bit NULL default 0
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: PARAM
  Campo : FlgZerAutMesSldVrbPos: sinalizador para zerar o saldo positivo das verbas no final do mês
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PARAM'
                  And Column_Name = 'FlgZerAutMesSldVrbPos')
  ALTER TABLE dbo.PARAM ADD FlgZerAutMesSldVrbPos bit NULL default 0
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: PARAM
  Campo : FlgZerAutMesSldVrbNeg: sinalizador para zerar o saldo negativo das verbas no final do mês
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PARAM'
                  And Column_Name = 'FlgZerAutMesSldVrbNeg')
  ALTER TABLE dbo.PARAM ADD FlgZerAutMesSldVrbNeg bit NULL default 0
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: NFEIT
  Campo : Vlr_ComSup : comissão supervisor
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFEIT'
               And Column_Name = 'Vlr_ComSup')
  ALTER TABLE dbo.NFEIT ADD Vlr_ComSup numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFEIT'
               And Column_Name = 'Vlr_ComSupOpe')
  ALTER TABLE dbo.NFEIT ADD Vlr_ComSupOpe numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFEIT'
               And Column_Name = 'Vlr_ComGer')
  ALTER TABLE dbo.NFEIT ADD Vlr_ComGer numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFEIT'
               And Column_Name = 'Vlr_ComGerOpe')
  ALTER TABLE dbo.NFEIT ADD Vlr_ComGerOpe numeric(18,4) NULL default 0
GO

/*
  Versao 20.04
  Abril de 2022
  Tabela: NFECB
  Campo : Vlr_ComSup : comissão supervisor
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Vlr_ComSup')
  ALTER TABLE dbo.NFECB ADD Vlr_ComSup numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Vlr_ComSupOpe')
  ALTER TABLE dbo.NFECB ADD Vlr_ComSupOpe numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Vlr_ComGer')
  ALTER TABLE dbo.NFECB ADD Vlr_ComGer numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Vlr_ComGerOpe')
  ALTER TABLE dbo.NFECB ADD Vlr_ComGerOpe numeric(18,4) NULL default 0
GO

if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Vlr_VrbVdr')
  ALTER TABLE dbo.NFECB ADD Vlr_VrbVdr numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Vlr_VrbOpe')
  ALTER TABLE dbo.NFECB ADD Vlr_VrbOpe numeric(18,4) NULL default 0
GO
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'NFECB'
               And Column_Name = 'Vlr_VrbSup')
  ALTER TABLE dbo.NFECB ADD Vlr_VrbSup numeric(18,4) NULL default 0
GO
-----------------------------------------------------------------------------------------------------------

/*
  Versao 20.04
  Abril de 2022
  Tabela: POCOM
  Campo : Tip_PolCom varchar(3) NOT NULL
*/
IF EXISTS (Select Column_Name from Information_Schema.columns
            Where Table_Name = 'POCOM'
              And Column_Name = 'Tip_PolCom '
              And Is_Nullable = 'YES')
BEGIN
  -- cria qrq. temporários comandos Sql
  IF OBJECT_ID('tempdb..#ARQCMD') IS NOT NULL
    DROP TABLE #ARQCMD
  
  CREATE TABLE #ARQCMD
  (
     Chave int identity,
     Ordem varchar(2),
     SqlText  nvarchar(max) 
  ) 
  
  declare @SqlCmd varchar(max)
  declare @SchemaName varchar(100)
  declare @TableName varchar(256)
  declare @IndexName varchar(256)
  declare @ColumnName varchar(100)
  declare @IndexColumns varchar(max)
  declare @IncludedColumns varchar(max)
  
  declare @is_unique varchar(100)
  declare @IndexTypeDesc varchar(100)
  declare @FileGroupName varchar(100)
  declare @is_disabled varchar(100)
  declare @IndexOptions varchar(max)
  declare @IndexColumnId int
  declare @IsDescendingKey int 
  declare @IsIncludedColumn int
  declare @SqlCmdDisableIndex varchar(max)
  
  DECLARE Indices_cr CURSOR LOCAL FAST_FORWARD FOR
    Select schema_name(t.schema_id) [schema_name], t.name, ix.name,
           Case When ix.is_unique = 1 then 'UNIQUE ' Else '' END 
           , ix.type_desc,
           Case When ix.is_padded = 1 then 'PAD_INDEX = ON, ' else 'PAD_INDEX = OFF, ' end
           + case when ix.allow_page_locks=1 then 'ALLOW_PAGE_LOCKS = ON, ' else 'ALLOW_PAGE_LOCKS = OFF, ' end
           + case when ix.allow_row_locks=1 then  'ALLOW_ROW_LOCKS = ON, ' else 'ALLOW_ROW_LOCKS = OFF, ' end
           + case when INDEXPROPERTY(t.object_id, ix.name, 'IsStatistics') = 1 then 'STATISTICS_NORECOMPUTE = ON, ' else 'STATISTICS_NORECOMPUTE = OFF, ' end
           + case when ix.ignore_dup_key=1 then 'IGNORE_DUP_KEY = ON, ' else 'IGNORE_DUP_KEY = OFF, ' end
  --         + 'SORT_IN_TEMPDB = OFF, FILLFACTOR =' + CAST(ix.fill_factor AS VARCHAR(3)) AS IndexOptions
           + 'SORT_IN_TEMPDB = OFF, FILLFACTOR =' + CAST(80 AS VARCHAR(3)) AS IndexOptions
           , ix.is_disabled , FILEGROUP_NAME(ix.data_space_id) FileGroupName
      From sys.tables t 
           inner join sys.indexes ix on t.object_id=ix.object_id
     Where ix.type > 0 
       and ix.is_primary_key = 0 and ix.is_unique_constraint = 0 --and schema_name(tb.schema_id)= @SchemaName and tb.name=@TableName
       and t.is_ms_shipped = 0 and t.name <> 'sysdiagrams'
     order by schema_name(t.schema_id), t.name, ix.name
  
  OPEN Indices_cr
  FETCH NEXT FROM Indices_cr INTO  @SchemaName, @TableName, @IndexName, @is_unique, @IndexTypeDesc, @IndexOptions, @is_disabled, @FileGroupName
  
  WHILE @@fetch_status = 0
  BEGIN
    if (dbo.FN_ADM_ValidaColunaIndice(@TableName,@IndexName,'Tip_PoLCom') = 1) 
      begin
        set @IndexColumns = ''
        set @IncludedColumns = ''
       
        DECLARE IndexColumn_cr CURSOR LOCAL FAST_FORWARD FOR
          Select col.name, ixc.is_descending_key, ixc.is_included_column
            From sys.tables tb 
                 inner join sys.indexes ix on tb.object_id=ix.object_id
                 inner join sys.index_columns ixc on ix.object_id=ixc.object_id and ix.index_id= ixc.index_id
                 inner join sys.columns col on ixc.object_id =col.object_id  and ixc.column_id=col.column_id
           Where ix.type > 0 
             and (ix.is_primary_key = 0 or ix.is_unique_constraint = 0)
             and schema_name(tb.schema_id) = @SchemaName 
             and tb.name = @TableName 
             and ix.name = @IndexName
           Order by ixc.index_column_id
        OPEN IndexColumn_cr 
        FETCH NEXT FROM IndexColumn_cr INTO  @ColumnName, @IsDescendingKey, @IsIncludedColumn
        WHILE (@@fetch_status = 0)
        BEGIN
          if @IsIncludedColumn = 0 
            set @IndexColumns = @IndexColumns + @ColumnName  + case when @IsDescendingKey=1  then ' DESC, ' else  ' ASC, ' end
          else 
            set @IncludedColumns = @IncludedColumns  + @ColumnName  +', ' 
       
          FETCH NEXT FROM IndexColumn_cr INTO  @ColumnName, @IsDescendingKey, @IsIncludedColumn
        END
        CLOSE IndexColumn_cr
        DEALLOCATE IndexColumn_cr
       
        set @IndexColumns = substring(@IndexColumns, 1, len(@IndexColumns)-1)
        set @IncludedColumns = case when len(@IncludedColumns) > 0 then substring(@IncludedColumns, 1, len(@IncludedColumns)-1) else '' end
       
        set @SqlCmd = 'CREATE '+ @is_unique  +@IndexTypeDesc + ' INDEX ' +QUOTENAME(@IndexName)+' ON ' + QUOTENAME(@SchemaName) +'.'+ QUOTENAME(@TableName)+ '('+@IndexColumns+') '+ 
                      case when len(@IncludedColumns)>0 then CHAR(13) +'INCLUDE (' + @IncludedColumns+ ')' else '' end + CHAR(13)+'WITH (' + @IndexOptions+ ') ON ' + QUOTENAME(@FileGroupName) + ';'  
       
        Set @SqlCmd = 'IF NOT EXISTS(SELECT name From sys.indexes WHERE NAME = ''' + @IndexName + ''') ' + @SqlCmd
       
        -- grava na tabela de comandos
        if not Exists(Select 1 From #ARQCMD Where Ordem = 'i6' and SqlText = @SqlCmd)
          begin
            insert into #ARQCMD (Ordem, SqlText) 
                         values ('i6', @SqlCmd)
       	end
       
        set @SqlCmdDisableIndex =''
        if @is_disabled = 1 
          begin
            set @SqlCmdDisableIndex = CHAR(13) +'ALTER INDEX ' +QUOTENAME(@IndexName) + ' ON ' + QUOTENAME(@SchemaName) +'.'+ QUOTENAME(@TableName) + ' DISABLE;' + CHAR(13) 
       
            -- grava na tabela de comandos
            if not Exists(Select 1 From #ARQCMD Where Ordem = 'i6' and SqlText = @SqlCmdDisableIndex)
              if Exists(Select 1 From #ARQCMD Where Ordem = 'i6' and SqlText = @SqlCmd)
                begin
                  insert into #ARQCMD (Ordem, SqlText) 
                               values ('6', @SqlCmdDisableIndex)
       	      end
          end
  
        -- deleta indices
        SET @SqlCmd = 'DROP INDEX '+QUOTENAME(@SchemaName)+ '.' + QUOTENAME(@TableName) + '.' +QUOTENAME(@IndexName)
        Set @SqlCmd = 'IF EXISTS(SELECT name From SYS.INDEXES WHERE NAME = ''' + @IndexName + ''') ' + @SqlCmd
        Exec (@SqlCmd)

      end
   FETCH NEXT FROM Indices_cr INTO  @SchemaName, @TableName, @IndexName, @is_unique, @IndexTypeDesc, @IndexOptions, @is_disabled, @FileGroupName
  END
  CLOSE Indices_cr
  DEALLOCATE Indices_cr 
  

  --  Campo : Tip_PolCom varchar(3) NOT NULL
  Update POCOM 
     Set Tip_PolCom = '' 
   Where Tip_PolCom Is Null

  ALTER TABLE dbo.POCOM ALTER COLUMN Tip_PolCom varchar(3) NOT NULL
  

  -- recria indices
  DECLARE Indices_cr CURSOR LOCAL FAST_FORWARD FOR
    Select SqlText From  #ARQCMD
     Where substring(Ordem,1,1) = 'i'
     Order by Ordem, Chave
  OPEN Indices_cr
  FETCH NEXT FROM Indices_cr INTO @SqlCmd
  WHILE @@fetch_status = 0
  BEGIN
    Exec (@SqlCmd)
    FETCH NEXT FROM Indices_cr INTO @SqlCmd
  END
  CLOSE Indices_cr
  DEALLOCATE Indices_cr 

END  -- IF EXISTS (Select Column_Name from Information_Schema.columns

GO
------------------------------------------------------------------------------------------------------------------------------------------

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFECB
  Campo Cod_Supervisor
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFECB'
                  And Column_Name = 'Cod_Supervisor')
  ALTER TABLE dbo.NFECB ADD Cod_Supervisor int null default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFECB
  Campo Cod_SupOpe
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFECB'
                  And Column_Name = 'Cod_SupOpe')
  ALTER TABLE dbo.NFECB ADD Cod_SupOpe int null default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFECB
  Campo Cod_Gerencia
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFECB'
                  And Column_Name = 'Cod_Gerencia')
  ALTER TABLE dbo.NFECB ADD Cod_Gerencia int null default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFECB
  Campo Cod_GerOpe
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFECB'
                  And Column_Name = 'Cod_GerOpe')
  ALTER TABLE dbo.NFECB ADD Cod_GerOpe int null default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: CNGMV
  Campo : Prc_Unitar
*/
if not Exists (Select Column_Name from Information_Schema.columns
               Where Table_Name = 'CNGMV'
               And Column_Name = 'Prc_Unitar')
  ALTER TABLE dbo.CNGMV ADD Prc_Unitar numeric(18,8) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: RTXCT
  Campo : Alq_IrfIntLoc (Alíquota Imposto Retido na Fonte)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'RTXCT'
                  And Column_Name = 'Alq_IrfIntLoc')
  ALTER TABLE dbo.RTXCT ADD Alq_IrfIntLoc numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PDVCB
  Campo : Vlr_BasIrf (Base de Cálculo para retenção do imposto)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PDVCB'
                  And Column_Name = 'Vlr_BasIrf')
  ALTER TABLE dbo.PDVCB ADD Vlr_BasIrf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PDVCB
  Campo : Vlr_Irf (Valor do imposto retido na fonte)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PDVCB'
                  And Column_Name = 'Vlr_Irf')
  ALTER TABLE dbo.PDVCB ADD Vlr_Irf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PDVIT
  Campo : Vlr_BasIrf (Base de Cálculo para retenção do imposto)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PDVIT'
                  And Column_Name = 'Vlr_BasIrf')
  ALTER TABLE dbo.PDVIT ADD Vlr_BasIrf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PDVIT
  Campo : Alq_Irf (Alíquota usada para retenção do imposto de renda)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PDVIT'
                  And Column_Name = 'Alq_Irf')
  ALTER TABLE dbo.PDVIT ADD Alq_Irf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PDVIT
  Campo : Vlr_Irf (Valor do imposto retido na fonte)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PDVIT'
                  And Column_Name = 'Vlr_Irf')
  ALTER TABLE dbo.PDVIT ADD Vlr_Irf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFSCB
  Campo : Vlr_BasIrf (Base de Cálculo para retenção do imposto)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFSCB'
                  And Column_Name = 'Vlr_BasIrf')
  ALTER TABLE dbo.NFSCB ADD Vlr_BasIrf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFSCB
  Campo : Vlr_Irf (Valor do imposto retido na fonte)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFSCB'
                  And Column_Name = 'Vlr_Irf')
  ALTER TABLE dbo.NFSCB ADD Vlr_Irf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFSIT
  Campo : Vlr_BasIrf (Base de Cálculo para retenção do imposto)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFSIT'
                  And Column_Name = 'Vlr_BasIrf')
  ALTER TABLE dbo.NFSIT ADD Vlr_BasIrf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFSIT
  Campo : Alq_Irf (Alíquota usada para retenção do imposto de renda)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFSIT'
                  And Column_Name = 'Alq_Irf')
  ALTER TABLE dbo.NFSIT ADD Alq_Irf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFSIT
  Campo : Vlr_Irf (Valor do imposto retido na fonte)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'NFSIT'
                  And Column_Name = 'Vlr_Irf')
  ALTER TABLE dbo.NFSIT ADD Vlr_Irf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: CTREC
  Campo : Vlr_Irf (Valor do imposto retido na fonte)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'CTREC'
                  And Column_Name = 'Vlr_Irf')
  ALTER TABLE dbo.CTREC ADD Vlr_Irf numeric(18,4) NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFECB
  Campo : Flg_BlqTrfEstFis: indica se bloqueia transferencia estoque fiscal (acobertamento fiscal)
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'NFECB'
                  And Column_Name = 'Flg_BlqTrfEstFis')
  ALTER TABLE dbo.NFECB ADD Flg_BlqTrfEstFis bit NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFSCB
  Campo : Flg_BlqTrfEstFis: indica se bloqueia transferencia estoque fiscal (acobertamento fiscal)
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'NFSCB'
                  And Column_Name = 'Flg_BlqTrfEstFis')
  ALTER TABLE dbo.NFSCB ADD Flg_BlqTrfEstFis bit NULL default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFECB
  Campo : Flg_OpeVdo
*/
Exec PR_ADM_ExcluiCampo 'NFECB', 'Flg_OpeVdo'
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: NFSCB
  Campo : Flg_OpeVdo
*/
Exec PR_ADM_ExcluiCampo 'NFSCB', 'Flg_OpeVdo'
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PAGCT
  Campo : Observacao null varchar(100)
*/
if Exists (select 1 from Information_Schema.columns
            Where Table_Name = 'PAGCT'
              And Column_Name = 'Observacao'
			  And DATA_TYPE = 'varchar'
              And CHARACTER_MAXIMUM_LENGTH < 100)
  ALTER TABLE dbo.PAGCT ALTER COLUMN Observacao varchar(100) NULL
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PRSLD
  Campo Qtd_InfEntInv : qtd ajustada manualmente em PRXES (inconsistencia com PRLOT+PRLTL)
*/
if Exists(Select 1 from Information_Schema.columns
           Where Table_Name = 'PRSLD'
             and Column_Name = 'Qtd_InfEntSaiInv')
and not Exists(Select 1 from Information_Schema.columns
               Where Table_Name = 'PRSLD'
               and Column_Name = 'Qtd_InfEntInv')
  exec sp_rename 'dbo.PRSLD.Qtd_InfEntSaiInv', 'Qtd_InfEntInv', 'COLUMN'
GO
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PRSLD'
                  And Column_Name = 'Qtd_InfEntInv')
  ALTER TABLE dbo.PRSLD ADD Qtd_InfEntInv int null default 0
GO

/*
  Versao 20.06
  Junho de 2022
  Tabela: PRSLD
  Campo Qtd_InfSaiInv : qtd ajustada manualmente em PRXES (inconsistencia com PRLOT+PRLTL)
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PRSLD'
                  And Column_Name = 'Qtd_InfSaiInv')
  ALTER TABLE dbo.PRSLD ADD Qtd_InfSaiInv int null default 0
GO

/*
  Versao 20.06
  Junho de 2022
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
  Versao 20.06
  Junho de 2022
  Tabela: LOGDIVESTPRDLOT
*/
If not exists (select 1 from dbo.sysobjects where id = object_id(N'[dbo].[LOGDIVESTPRDLOT]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
  CREATE TABLE [dbo].[LOGDIVESTPRDLOT]
  (
	[Cod_Estabe] [int] NOT NULL,
	[Cod_Produt] [int] NOT NULL,
	[Qtd_Difere] [int] NOT NULL,
	[Qtd_FisPrd] [int] NOT NULL,
	[Qtd_FisLot] [int] NOT NULL,
    [Des_Local]  [varchar](80) NULL,
	[Transacao] [datetime]
  ) ON [PRIMARY] 
go

if not Exists (select name from sysobjects where name = 'PK_LOGDIVESTPRDLOT')
  ALTER TABLE [dbo].[LOGDIVESTPRDLOT] ADD CONSTRAINT [PK_LOGDIVESTPRDLOT] PRIMARY KEY CLUSTERED
  (
	[Cod_Estabe] ASC,
	[Cod_Produt] ASC,
	[Qtd_Difere] ASC,
	[Qtd_FisPrd] ASC,
	[Qtd_FisLot] ASC
  )WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, FILLFACTOR = 80) ON [PRIMARY]
GO

/*
  Versao 20.06
  Agosto de 2022
  Tabela: PARAM
  Campo : FlgRegistraLogDivergencia
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PARAM'
                  And Column_Name = 'FlgRegistraLogDivergencia')
  ALTER TABLE dbo.PARAM ADD FlgRegistraLogDivergencia bit NULL default 0
GO

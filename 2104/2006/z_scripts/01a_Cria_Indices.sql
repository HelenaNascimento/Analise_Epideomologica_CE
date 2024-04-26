-- criar indices ----------------
DECLARE @IndexName VARCHAR(256)
      , @NumSeqIdx tinyint
	  , @SqlCmd NVARCHAR(MAX)


While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'BXREC'
			 AND i.name not like 'IX_BXREC_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Dat_FecCom, Status')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'BXREC'
                         AND i.name not like 'IX_BXREC_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Dat_FecCom, Status')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.BXREC') And Name = @IndexName)
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
                 AND o.Name = 'BXREC'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Dat_FecCom, Status') 
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_BXREC_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_BXREC_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[BXREC] ([Cod_Estabe],[Dat_FecCom],[Status]) '+
                  'INCLUDE ([Cod_Documento],[Cod_Lancamento],[Dat_Lancamento],[Dat_Registro],[Dat_Caixa], '+
				  '         [Vlr_Principal],[Vlr_Desconto],[Vlr_Deducoes],[Vlr_DscDev])'
    EXECUTE sp_executesql @SqlCmd
  end
---------------------------------------------------------------------------------------------------------------------------

While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'NFSCB'
			 AND i.name not like 'IX_NFSCB_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Dat_FecCom, Dat_Emissao')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'NFSCB'
                         AND i.name not like 'IX_NFSCB_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Dat_FecCom, Dat_Emissao')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.NFSCB') And Name = @IndexName)
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
                 AND o.Name = 'NFSCB'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Dat_FecCom, Dat_Emissao')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_NFSCB_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_NFSCB_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[NFSCB] ([Cod_Estabe],[Dat_FecCom],[Dat_Emissao]) '+
                  'INCLUDE ([Ser_Nota],[Num_Nota]) '
    EXECUTE sp_executesql @SqlCmd
  end
---------------------------------------------------------------------------------------------------------------------------

While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'NFSCB'
			 AND i.name not like 'IX_NFSCB_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Status, Tip_Saida, Dat_FecCom')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'NFSCB'
                         AND i.name not like 'IX_NFSCB_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Status, Tip_Saida, Dat_FecCom')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.NFSCB') And Name = @IndexName)
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
                 AND o.Name = 'NFSCB'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Status, Tip_Saida, Dat_FecCom')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_NFSCB_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_NFSCB_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[NFSCB] ([Cod_Estabe],[Status],[Tip_Saida],[Dat_FecCom]) '+
                  'INCLUDE ([Ser_Nota],[Num_Nota],[Cod_Vendedor],[Vlr_Comissao]) '
    EXECUTE sp_executesql @SqlCmd
  end
---------------------------------------------------------------------------------------------------------------------------

While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'NFSCB'
			 AND i.name not like 'IX_NFSCB_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Dat_FecComEnt')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'NFSCB'
                         AND i.name not like 'IX_NFSCB_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Dat_FecComEnt')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.NFSCB') And Name = @IndexName)
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
                 AND o.Name = 'NFSCB'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Dat_FecComEnt')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_NFSCB_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_NFSCB_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[NFSCB] ([Cod_Estabe],[Dat_FecComEnt]) '

    EXECUTE sp_executesql @SqlCmd
  end
---------------------------------------------------------------------------------------------------------------------------

While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'VALES'
			 AND i.name not like 'IX_VALES_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Deb_Cre, Cod_CtrLanAut, Num_CtrLanAut')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'VALES'
                         AND i.name not like 'IX_VALES_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Deb_Cre, Cod_CtrLanAut, Num_CtrLanAut')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.VALES') And Name = @IndexName)
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
                 AND o.Name = 'VALES'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Deb_Cre, Cod_CtrLanAut, Num_CtrLanAut')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_VALES_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_VALES_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[VALES] ([Cod_Estabe],[Deb_Cre],[Cod_CtrLanAut],[Num_CtrLanAut]) '

    EXECUTE sp_executesql @SqlCmd
  end

-- ORCCB/ORCIT
if not Exists(SELECT i.name
                FROM sysobjects   o INNER JOIN 
                     sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                     sys.indexes  i ON o.id = i.object_id
               WHERE so.type = 'U'
                 AND o.Name = 'ORCCB'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Sta_Movime')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_ORCCB_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_ORCCB_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[ORCCB] ([Cod_Estabe],[Sta_Movime]) '+
                  'INCLUDE ([Num_Docume], [Cod_OriVen]) '
    EXECUTE sp_executesql @SqlCmd
  end

if not Exists(SELECT i.name
                FROM sysobjects   o INNER JOIN 
                     sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                     sys.indexes  i ON o.id = i.object_id
               WHERE so.type = 'U'
                 AND o.Name = 'ORCIT'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Tip_EntSai, Cod_Produt, Cod_Lote')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_ORCIT_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_ORCIT_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[ORCIT] ([Cod_Estabe],[Tip_EntSai],[Cod_Produt],[Cod_Lote]) '+
                  'INCLUDE ([Qtd_Produt]) '
    EXECUTE sp_executesql @SqlCmd
  end


-- criar indice em PRXES ----------------
While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'PRXES'
			 AND i.name not like 'IX_PRXES_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Produt')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'PRXES'
                         AND i.name not like 'IX_PRXES_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Produt')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.PRXES') And Name = @IndexName)
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
                 AND o.Name = 'PRXES'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Produt') 
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_PRXES_'+FORMAT(@NumSeqIdx,'D3')

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_PRXES_'+FORMAT(@NumSeqIdx,'D3')
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[PRXES] ([Cod_Produt]) '+
                  'INCLUDE ([Qtd_Fisico],[Qtd_Solici],[Qtd_Dispon])'
    EXECUTE sp_executesql @SqlCmd
  end

GO

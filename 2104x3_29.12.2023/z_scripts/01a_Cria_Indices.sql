-- criar indices ----------------
DECLARE @IndexName VARCHAR(256)
      , @NumSeqIdx tinyint
	  , @SqlCmd NVARCHAR(MAX)


While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'PVMIT'
			 AND i.name not like 'IX_PVMIT_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Id_PVMCB, Num_SeqDig, Cod_Produto')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'PVMIT'
                         AND i.name not like 'IX_PVMIT_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Id_PVMCB, Num_SeqDig, Cod_Produto')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.PVMIT') And Name = @IndexName)
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
                 AND o.Name = 'PVMIT'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Id_PVMCB, Num_SeqDig, Cod_Produto')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_PVMIT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_PVMIT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[PVMIT] ([Id_PVMCB],[Num_SeqDig],[Cod_Produto]) '

    EXECUTE sp_executesql @SqlCmd
  end

---

While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'PRLOT'
			 AND i.name not like 'IX_PRLOT_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Cod_Produt, Qtd_Saldo')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'PRLOT'
                         AND i.name not like 'IX_PRLOT_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Cod_Produt, Qtd_Saldo')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.PRLOT') And Name = @IndexName)
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
                 AND o.Name = 'PRLOT'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Cod_Produt, Qtd_Saldo')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_PRLOT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_PRLOT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE INDEX ['+@IndexName+'] ON [dbo].[PRLOT] ([Cod_Estabe],[Cod_Produt],[Qtd_Saldo]) INCLUDE ([Cod_Lote], [Dat_Vencim]) '

    EXECUTE sp_executesql @SqlCmd
  end

---

While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'NFEIT'
			 AND i.name not like 'IX_NFEIT_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Cod_Produto, Cod_Lote')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'NFEIT'
                         AND i.name not like 'IX_NFEIT_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Cod_Produto, Cod_Lote')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.NFEIT') And Name = @IndexName)
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
                 AND o.Name = 'NFEIT'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Cod_Produto, Cod_Lote')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_NFEIT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_NFEIT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE INDEX ['+@IndexName+'] ON [dbo].[NFEIT] ([Cod_Estabe],[Cod_Produto],[Cod_Lote]) '+
	              'INCLUDE ([Qtd_Pedido], [Qtd_Bonificacao], [Vlr_SubsTrib], [Vlr_SbtRes], [Vlr_DifTri]) '

    EXECUTE sp_executesql @SqlCmd
  end

---

While Exists(SELECT i.name
            FROM sysobjects   o INNER JOIN 
                 sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                 sys.indexes  i ON o.id = i.object_id
           WHERE so.type = 'U'
             AND o.Name = 'PDVIT'
			 AND i.name not like 'IX_PDVIT_%'
             AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                          FROM syscolumns sc INNER JOIN 
                              sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                         WHERE sc.id = so.object_id
                           AND ic.index_id = i.index_id
                           AND ic.is_included_column = 0
                         ORDER BY key_ordinal
                           FOR XML PATH('')
                       ), 1, 2, '') = 'Cod_Estabe, Cod_Pedido, Qtd_Pedido')
  begin
	Set @IndexName = (SELECT top 1 i.name
                        FROM sysobjects   o INNER JOIN 
                             sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 INNER JOIN
                             sys.indexes  i ON o.id = i.object_id
                       WHERE so.type = 'U'
                         AND o.Name = 'PDVIT'
                         AND i.name not like 'IX_PDVIT_%'
                         AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                      FROM syscolumns sc INNER JOIN 
                                           sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                     WHERE sc.id = so.object_id
                                       AND ic.index_id = i.index_id
                                       AND ic.is_included_column = 0
                                     ORDER BY key_ordinal
                                       FOR XML PATH('')
                                   ), 1, 2, '') = 'Cod_Estabe, Cod_Pedido, Qtd_Pedido')

    if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.PDVIT') And Name = @IndexName)
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
                 AND o.Name = 'PDVIT'
                 AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                              FROM syscolumns sc INNER JOIN 
                                  sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                             WHERE sc.id = so.object_id
                               AND ic.index_id = i.index_id
                               AND ic.is_included_column = 0
                             ORDER BY key_ordinal
                               FOR XML PATH('')
                           ), 1, 2, '') = 'Cod_Estabe, Cod_Pedido, Qtd_Pedido')
  begin
	Set @NumSeqIdx = 1
    SET @IndexName = 'IX_PDVIT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)

    while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
	begin
      Set @NumSeqIdx = @NumSeqIdx + 1
      SET @IndexName = 'IX_PDVIT_'+dbo.FN_FormataInteiro(@NumSeqIdx,3)
	end

    Set @SqlCmd = 'CREATE INDEX ['+@IndexName+'] ON [dbo].[PDVIT] ([Cod_Estabe],[Cod_Pedido],[Qtd_Pedido]) '+
	              'INCLUDE ([Flg_BlqInfPar],[C_PrcTotal],[Vlr_RepIcms],[Desconto],[Qtd_Bonificacao],[Per_Desconto],[Tip_BasRnt]) '

    EXECUTE sp_executesql @SqlCmd
  end

---


GO




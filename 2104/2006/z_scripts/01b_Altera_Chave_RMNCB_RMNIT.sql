SET NOCOUNT ON

--  Campo _Num_RomaneAnt: armazena numero romaneio original
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'RMNCB'
                  And Column_Name = '_Num_RomaneAnt')
  ALTER TABLE dbo.RMNCB ADD _Num_RomaneAnt int NULL 
GO

Update NUMER
   Set Valor = IsNull((Select max(Numero) From RMNCB),0)
 Where Campo = 'Romaneio'
   And Valor <> (Select max(Numero) From RMNCB)
GO

BEGIN
  Declare @NumRmn int,
          @WChave int,
          @WIte_CodEstabe int,
          @NovoNum int

  Declare @tab1 table(
    Chave int identity(1,1) primary key,
    Num_Romane int,
    Cod_Estabe int 
  )

  Insert into @tab1
    Select distinct cb.Numero, cb.Cod_Estabe 
      From RMNCB cb
           inner join (Select Numero, qtd=count(*)
                         From RMNCB
                        Group by Numero
                       Having count(*) > 1) rm on cb.Numero = rm.Numero
     Order by cb.Numero, cb.Cod_Estabe

  while Exists(Select Num_Romane
                 From @tab1
                Group by Num_Romane
               Having count(*) > 1)
    begin
      Select top 1 @NumRmn = Num_Romane
        From @tab1
       Group by Num_Romane
      Having count(*) > 1
  
      while Exists(Select count(*) 
                     From @tab1
                    Where Num_Romane = @NumRmn
                   Having count(*) > 1)
        begin
          -- seleciona romaneio a ser renumerado
          Select top 1 @WChave = Chave,
      	             @WIte_CodEstabe =Cod_Estabe
            From @tab1
           Where Num_Romane = @NumRmn
           Order by Cod_Estabe desc
      
          Print 'Renumerando Romaneio: '+cast(@WIte_CodEstabe as varchar)+'/'+cast(@NumRmn as varchar)
      
          -- guarda romaneio anterior
          Update RMNCB
             Set _Num_RomaneAnt = Numero
           Where Cod_Estabe = @WIte_CodEstabe
             And Numero = @NumRmn
             And IsNull(_Num_RomaneAnt,0) <> Numero
      
          -- pega o prox numero do romenio
          Exec PR_ProxNumero 'Romaneio', @NovoNum Output
      
          -- renumera RMNCB 
          Update RMNCB
             Set Numero = @NovoNum
           Where Cod_Estabe = @WIte_CodEstabe
             And Numero = @NumRmn
      
          -- atualiza RMNIT
          Update RMNIT
             Set Num_Romaneio = @NovoNum
           Where Cod_Estabe = @WIte_CodEstabe
             And Num_Romaneio = @NumRmn
      
          Delete From @tab1
           Where Chave = @WChave
  
        end  -- while Exists
  
    end  -- while Exists
END
GO

Update NUMER
   Set Valor = IsNull((Select max(Numero) From RMNCB),0)
 Where Campo = 'Romaneio'
   And Valor <> (Select max(Numero) From RMNCB)
GO


-- 
BEGIN
  DECLARE @IndexName VARCHAR(256)
        , @NumSeqIdx smallint
	    , @SqlCmd NVARCHAR(MAX)

  -- remove Cod_Estabe de FK_RMNIT_RMNCB
  if Exists(Select 1
              From sys.foreign_keys ForeignKeys 
                   JOIN sys.foreign_key_columns ForeignKeyRelationships 
                     ON ( ForeignKeys.object_id = 
                          ForeignKeyRelationships.constraint_object_id ) 
                   JOIN sys.tables ForeignKeyTable 
                     ON ForeignKeyRelationships.parent_object_id = ForeignKeyTable.object_id 
                   JOIN sys.tables PrimaryKeyTable 
                     ON ForeignKeyRelationships.referenced_object_id = 
                        PrimaryKeyTable.object_id 
                   JOIN sys.columns PrimaryKeyColumn 
                     ON ( PrimaryKeyTable.object_id = PrimaryKeyColumn.object_id 
                          AND ForeignKeyRelationships.referenced_column_id = 
                              PrimaryKeyColumn.column_id ) 
              Where PrimaryKeyTable.NAME  = 'RMNCB'
                And ForeignKeyTable.NAME  = 'RMNIT'
                And PrimaryKeyColumn.NAME = 'Cod_Estabe')
    begin
      Select @sqlCmd = N'ALTER TABLE dbo.'+OBJECT_NAME(ForeignKeys.Parent_Object_ID)+' DROP CONSTRAINT ['+ForeignKeys.NAME+N']'
        From sys.foreign_keys ForeignKeys 
      
             JOIN sys.foreign_key_columns ForeignKeyRelationships 
               ON ( ForeignKeys.object_id = 
                    ForeignKeyRelationships.constraint_object_id ) 
      
             JOIN sys.tables ForeignKeyTable 
               ON ForeignKeyRelationships.parent_object_id = ForeignKeyTable.object_id 
      
             JOIN sys.tables PrimaryKeyTable 
               ON ForeignKeyRelationships.referenced_object_id = 
                  PrimaryKeyTable.object_id 
      
             JOIN sys.columns PrimaryKeyColumn 
               ON ( PrimaryKeyTable.object_id = PrimaryKeyColumn.object_id 
                    AND ForeignKeyRelationships.referenced_column_id = 
                        PrimaryKeyColumn.column_id ) 
  
        Where PrimaryKeyTable.NAME  = 'RMNCB'
          And ForeignKeyTable.NAME  = 'RMNIT'
          And PrimaryKeyColumn.NAME = 'Cod_Estabe'
  
      Exec (@SqlCmd)
  
      -- elimina FK_RMNIT_RMNCB
      IF EXISTS(Select name from sys.foreign_keys 
                 Where OBJECT_NAME(parent_object_id) = 'RMNIT'
                   And OBJECT_NAME(Referenced_Object_ID) = 'RMNCB'
                   And name = 'FK_RMNIT_RMNCB')
        ALTER TABLE dbo.RMNIT DROP CONSTRAINT [FK_RMNIT_RMNCB]
  
    end          
  ----------------------------------------------------------------------
  
  -- recria PK_RMNCB sem Cod_Estabe
  if Exists(Select ti.COLUMN_NAME
              From INFORMATION_SCHEMA.KEY_COLUMN_USAGE ti
                   Join SYSOBJECTS so on ti.CONSTRAINT_NAME = so.NAME
             Where ti.TABLE_SCHEMA = 'dbo' AND ti.TABLE_NAME = 'RMNCB'
               And ti.TABLE_NAME = OBJECT_NAME(so.Parent_Obj) And so.XTYPE = 'PK'
               And ti.COLUMN_NAME = 'Cod_Estabe') OR
      not Exists(Select * From sys.indexes Where object_id = OBJECT_ID(N'[dbo].[RMNCB]') AND name = N'PK_RMNCB')
    begin
      -- elimina PK existente
      Select @sqlCmd = N'ALTER TABLE dbo.RMNCB DROP CONSTRAINT ['+NAME+N']'
        From dbo.SYSOBJECTS Where OBJECT_NAME(Parent_Obj) = 'RMNCB' And XTYPE = 'PK'    

      Exec (@SqlCmd)

      -- cria nova PK_RMNCB    
      ALTER TABLE dbo.RMNCB WITH NOCHECK 
        ADD CONSTRAINT PK_RMNCB PRIMARY KEY CLUSTERED (Numero) WITH  FILLFACTOR = 80 ON [PRIMARY]
    end
  ---------- fim recria PK_RMNCB sem Cod_Estabe


  -- cria nova FK_RMNIT_RMNCB
  IF not EXISTS(Select name from sys.foreign_keys 
                 Where OBJECT_NAME(parent_object_id) = 'RMNIT'
                   And OBJECT_NAME(Referenced_Object_ID) = 'RMNCB'
                   And name = 'FK_RMNIT_RMNCB')
    begin
      ALTER TABLE [dbo].[RMNIT] WITH NOCHECK 
        ADD CONSTRAINT [FK_RMNIT_RMNCB] FOREIGN KEY([Num_Romaneio]) 
        REFERENCES [dbo].[RMNCB] ([Numero])
        ON UPDATE CASCADE ON DELETE CASCADE NOT FOR REPLICATION 

      ALTER TABLE [dbo].[RMNIT] CHECK CONSTRAINT [FK_RMNIT_RMNCB]
    end
  ---------- fim cria nova FK_RMNIT_RMNCB


  -- remove indice em RMNIT (Cod_Estabe,Num_Romaneio)
  While Exists(SELECT i.name
                 FROM sysobjects o 
                      JOIN sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0 
                      JOIN sys.indexes  i ON o.id = i.object_id
                WHERE so.type = 'U'
                  AND o.Name = 'RMNIT'
                  AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                               FROM syscolumns sc 
                                    JOIN sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                              WHERE sc.id = so.object_id
                                AND ic.index_id = i.index_id
                                AND ic.is_included_column = 0
                              ORDER BY key_ordinal
                                FOR XML PATH('')
                            ), 1, 2, '') = 'Cod_Estabe, Num_Romaneio')
    begin
	  Set @IndexName = (SELECT top 1 i.name
                          FROM sysobjects o
                               JOIN sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0
                               JOIN sys.indexes  i ON o.id = i.object_id
                         WHERE so.type = 'U'
                           AND o.Name = 'RMNIT'
                           AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                        FROM syscolumns sc
                                             JOIN sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                       WHERE sc.id = so.object_id
                                         AND ic.index_id = i.index_id
                                         AND ic.is_included_column = 0
                                       ORDER BY key_ordinal
                                         FOR XML PATH('')
                                     ), 1, 2, '') = 'Cod_Estabe, Num_Romaneio')

      if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.RMNIT') And Name = @IndexName)
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
  --------------- fim remove indice em RMNIT (Cod_Estabe,Num_Romaneio)


  -- remover indice em RMNCB: Cod_Estabe, Numero: diferente de IX_
  While Exists(SELECT i.name
                 FROM sysobjects o  
                      JOIN sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0
                      JOIN sys.indexes  i ON o.id = i.object_id
                WHERE so.type = 'U'
                  AND o.Name = 'RMNCB'
			      AND i.name not like 'IX_RMNCB_%'
                  AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                               FROM syscolumns sc 
                                    JOIN sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                              WHERE sc.id = so.object_id
                                AND ic.index_id = i.index_id
                                AND ic.is_included_column = 0
                              ORDER BY key_ordinal
                                FOR XML PATH('')
                            ), 1, 2, '') = 'Cod_Estabe, Numero')
    begin
	  Set @IndexName = (SELECT top 1 i.name
                          FROM sysobjects o
                               JOIN sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0
                               JOIN sys.indexes  i ON o.id = i.object_id
                         WHERE so.type = 'U'
                           AND o.Name = 'RMNCB'
                           AND i.name not like 'IX_RMNCB_%'
                           AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                        FROM syscolumns sc 
                                             JOIN sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                                       WHERE sc.id = so.object_id
                                         AND ic.index_id = i.index_id
                                         AND ic.is_included_column = 0
                                       ORDER BY key_ordinal
                                         FOR XML PATH('')
                                     ), 1, 2, '') = 'Cod_Estabe, Numero')

      if Exists (Select 1 From Sys.indexes Where Object_id=object_id(N'dbo.RMNCB') And Name = @IndexName)
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
  --------- fim remover indice em RMNCB: Cod_Estabe, Numero: diferente de IX_


  -- criar indice em RMNCB: Cod_Estabe, Numero
  if not Exists(SELECT i.name
                  FROM sysobjects o
                       JOIN sys.objects so ON so.object_id = o.id AND is_ms_shipped = 0
                       JOIN sys.indexes  i ON o.id = i.object_id
                 WHERE so.type = 'U'
                   AND o.Name = 'RMNCB'
                   AND STUFF((SELECT ', ' + sc.NAME + '' AS "text()"
                                FROM syscolumns sc 
                                     JOIN sys.index_columns  ic ON ic.object_id = sc.id AND ic.column_id = sc.colid
                               WHERE sc.id = so.object_id
                                 AND ic.index_id = i.index_id
                                 AND ic.is_included_column = 0
                               ORDER BY key_ordinal
                                 FOR XML PATH('')
                             ), 1, 2, '') = 'Cod_Estabe, Numero')
    begin
	  Set @NumSeqIdx = 1
      SET @IndexName = 'IX_RMNCB_'+FORMAT(@NumSeqIdx,'D3')

      while Exists (Select 1 From Sys.indexes Where Name = @IndexName)
        begin
          Set @NumSeqIdx = @NumSeqIdx + 1
          SET @IndexName = 'IX_RMNCB_'+FORMAT(@NumSeqIdx,'D3')
        end

      Set @SqlCmd = 'CREATE NONCLUSTERED INDEX ['+@IndexName+'] ON [dbo].[RMNCB] ([Cod_Estabe],[Numero]) '

      EXECUTE sp_executesql @SqlCmd
    end
  ---------- fim criar indice em RMNCB: Cod_Estabe, Numero


END
GO
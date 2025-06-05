
USE DMD
GO

DECLARE @NomTab varchar(255)

DECLARE Table_Cr CURSOR FOR
SELECT table_name 
  FROM information_Schema.tables
  WHERE table_type = 'base table'

OPEN Table_Cr

FETCH NEXT FROM Table_Cr INTO @NomTab
WHILE @@FETCH_STATUS = 0
BEGIN
  PRINT '==> Reindexando ' + @NomTab
  DBCC DBREINDEX(@NomTab,'',0)
  FETCH NEXT FROM Table_Cr INTO @NomTab
END

CLOSE Table_Cr
DEALLOCATE Table_Cr

PRINT ''
PRINT 'FIM DE PROCESSAMENTO...'

GO
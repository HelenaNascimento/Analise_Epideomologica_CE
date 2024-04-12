if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[PR_ADM_ExcluiCampo]') and OBJECTPROPERTY(id, N'IsProcedure') = 1)
drop procedure [dbo].[PR_ADM_ExcluiCampo]
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[PR_ADM_ExcluiCampo]
@PNomTab varchar(50),
@PNomCmp varchar(50)

AS

SET NOCOUNT ON

Declare @SqlCmd nvarchar(max)

if Exists (Select Column_Name from Information_Schema.columns
            Where Table_Name  = @PNomTab
              And Column_Name = @PNomCmp)
  begin
    -- elimina estatisticas
    IF EXISTS (SELECT 1 FROM sys.stats WHERE name = @PNomCmp AND object_id = OBJECT_ID(@PNomTab))
      begin
        Set @SqlCmd = 'DROP STATISTICS dbo.'+@PNomTab+'.'+@PNomCmp
        EXECUTE sp_executesql @SqlCmd
      end

    -- elimina indice
    IF EXISTS(SELECT 1
                FROM sys.indexes i INNER JOIN 
                     sys.index_columns ic ON  i.object_id = ic.object_id and i.index_id = ic.index_id INNER JOIN 
                     sys.columns col ON ic.object_id = col.object_id and ic.column_id = col.column_id INNER JOIN 
                     sys.tables t ON i.object_id = t.object_id 
               WHERE i.is_primary_key = 0 
                 AND i.is_unique = 0 
                 AND i.is_unique_constraint = 0 
                 AND t.is_ms_shipped = 0 
                 AND t.name = @PNomTab
                 AND col.name = @PNomCmp)
      begin
        SELECT top 1 @SqlCmd= 'DROP INDEX '+QUOTENAME(schema_name(t.schema_id))+'.'+QUOTENAME(t.name)+'.'+QUOTENAME(i.name)
          FROM sys.indexes i INNER JOIN 
               sys.index_columns ic ON  i.object_id = ic.object_id and i.index_id = ic.index_id INNER JOIN 
               sys.columns col ON ic.object_id = col.object_id and ic.column_id = col.column_id INNER JOIN 
               sys.tables t ON i.object_id = t.object_id 
         WHERE i.is_primary_key = 0 
           AND i.is_unique = 0 
           AND i.is_unique_constraint = 0 
           AND t.is_ms_shipped = 0 
           AND t.name = @PNomTab
           AND col.name = @PNomCmp

        if @@ROWCOUNT > 0
          Exec (@SqlCmd)
      end

    -- elimina FK
    IF EXISTS(SELECT 1
                FROM sys.foreign_keys fk
                     JOIN sys.foreign_key_columns fkr ON ( fk.object_id = fkr.constraint_object_id ) 
                     JOIN sys.tables tb ON fkr.parent_object_id = tb.object_id 
                     JOIN sys.columns col ON ( tb.object_id = col.object_id AND fkr.parent_column_id = col.column_id ) 
               WHERE tb.NAME = @PNomTab
                 AND col.NAME = @PNomCmp)
      begin
        SELECT top 1 @SqlCmd = N'ALTER TABLE ['+tb.NAME+N'] DROP CONSTRAINT ['+fk.NAME+N']'
          FROM sys.foreign_keys fk
               JOIN sys.foreign_key_columns fkr ON ( fk.object_id = fkr.constraint_object_id ) 
               JOIN sys.tables tb ON fkr.parent_object_id = tb.object_id 
               JOIN sys.columns col ON ( tb.object_id = col.object_id AND fkr.parent_column_id = col.column_id ) 
         WHERE tb.NAME = @PNomTab
           AND col.NAME = @PNomCmp

        if @@ROWCOUNT > 0
          Exec (@SqlCmd)
      end


    while Exists(Select top 1 a.name
                   from sys.default_constraints a
                        Join sys.columns b  ON b.column_id = a.parent_column_id and b.object_id = a.parent_object_id
                  Where b.object_id = OBJECT_ID(@PNomTab) 
                    and b.name = @PNomCmp)
      begin
        Select top 1 @SqlCmd = 'ALTER TABLE dbo.'+@PNomTab+' DROP CONSTRAINT ' + a.name
          from sys.default_constraints a
               Join sys.columns b  ON b.column_id = a.parent_column_id and b.object_id = a.parent_object_id
         Where b.object_id = OBJECT_ID(@PNomTab) 
           and b.name = @PNomCmp

        IF @@ROWCOUNT > 0
          EXECUTE sp_executesql @SqlCmd
      end

    Set @SqlCmd = 'ALTER TABLE dbo.'+@PNomTab+' DROP COLUMN '+@PNomCmp
    EXECUTE sp_executesql @SqlCmd

  end
GO
------------------------------------------------------------------------------------------------

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', 'Prc_UltEntLiq'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', 'Prc_UltEntLiqAnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', 'Prc_UltEntLiqIpi'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', 'Prc_UltEntLiqIpiAnt'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Dat_EntAnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Dat_EntAntDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Dat_PrcAnterior'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Dat_PrcAtual'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Dat_PrcFab'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Dat_UltCompra'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Dat_UltEntDep'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Bloqueado'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Cod_ClaTri'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Cod_LocFis'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Cod_Zon'
if Exists(SELECT 1 FROM sys.stats 
           WHERE name = 'Ctrl_Origem' 
             AND object_id = OBJECT_ID('PRODU'))
  DROP STATISTICS dbo.PRODU.Ctrl_Origem
GO
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Ctrl_Origem'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Desconto'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_BlqCfv'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_BlqCmp'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_BlqCot'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_BlqInfPar'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flag_BlqInfVen'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_BlqPrp'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_CesBas'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_RegSbtEsp'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Flg_UsoExcHsp'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_Avariado'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_Disponivel'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EntAnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EntAntDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EstMax'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EstMaxCfg'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EstMaxVrj'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EstMin'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EstMinCfg'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_EstMinVrj'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_Fisico'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_MesAleRec'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_PromDispo'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_PromFisic'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_PromSolic'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_PrzMaxFat'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_Quarentena'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_Reservado'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_Solicitado'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_Transito'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_UltEnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Qtd_UltEntDep'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Num_Apt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Num_Coluna'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Num_Nivel'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Num_Rua'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_BonAut'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_ComEnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscAut'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscAutOrc'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscBasComNor'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscEntAnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscEntAntDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscMaxPrz'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscMaxVis'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscUltEnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_DscUltEntDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Per_MarkupCusCom'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_CusLiqEnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_CusLiqEntDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_CusMedCom'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_CusMedDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_CusMedPra'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_CustoMedio'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_EntAnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_EntAntDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_Fabric'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_MaxCon'

Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_UltEnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_UltEntDep'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_VenAnt'
Exec dbo.PR_ADM_ExcluiCampo 'PRODU', '_Prc_Venda'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Tip_OrdProces'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedInf'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalInf'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotInf'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedWin'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalWin'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotWin'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedCon'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalCon'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotCon'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedCat'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalCat'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotCat'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedCtf'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalCtf'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotCtf'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedEdm'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalEdm'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotEdm'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedMrc'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalMrc'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotMrc'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedTvt'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalTvt'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotTvt'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_PedAcd'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_FalAcd'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_NotAcd'

Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Cod_EmpCat'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_CotCtf'
Exec dbo.PR_ADM_ExcluiCampo 'PMPDE', 'Dir_RetCotCtf'

GO

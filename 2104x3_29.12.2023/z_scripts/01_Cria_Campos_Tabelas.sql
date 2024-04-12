SET NOCOUNT ON;
SET XACT_ABORT ON;

-- versão anterior:
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
-----------------------------------------------------------------------------------------------------


-- criar objeto SEQUENCE "DMDSeq_Pedido_Venda" para substituir campo "Pedido_Venda" em NUMER
-- somente a partir do Sql Server 2012
IF EXISTS (SELECT compatibility_level FROM sys.databases 
            WHERE name = 'master' AND compatibility_level >= 110)
BEGIN
  Declare @SqlCmd nvarchar(max);
  Declare @SeqName Sysname;
  Declare @Campo Varchar(256);
  Declare @Valor Int;
  
  Set @Campo = 'Pedido_Venda';
  Set @SeqName = Quotename('DMDSeq_' + @Campo);

  -- cria objeto SEQUENCE  
  if Object_id(@SeqName, 'SO') Is Null
  begin
    Select @SqlCmd = N'Create Sequence ' + @SeqName + '
                       Start With ' + Cast((IsNull((Select MAX(Numero) From PDVCB),0) + 1) as nvarchar(max)) + '  
                       Increment by 1  
                       As Int ;'
  	
    Execute sp_executesql @SqlCmd=@SqlCmd;

    Delete From NUMER Where Campo = @Campo;
  end
  
  -- atualiza valor sequencial
  if Exists(Select 1 From sys.sequences 
             Where name = 'DMDSeq_Pedido_Venda')
  begin
    if IsNull((Select current_value 
                 From sys.sequences 
                Where name = 'DMDseq_Pedido_Venda'),0) <> IsNull((Select max(Numero) From PDVCB),0)+1
    begin
      Select @SqlCmd = N'ALTER SEQUENCE DMDseq_Pedido_Venda ' + '
                         RESTART WITH ' + Cast((IsNull((Select max(Numero) From PDVCB),0)+1) as nvarchar(max)); 
      Execute sp_executesql @SqlCmd=@SqlCmd;
    end
  end

END
GO
--------------------------------------------------------------------------------------------------------------------------------

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


/*
  Versao 21.03
  Março de 2023
  Tabela: PMEML
  Campo Id: Campo chave da tabela
  todo: devido a dependências no infarma email, vamos criar somente o campo mas não defini-lo como chave neste momento.
*/
IF NOT EXISTS(
    SELECT Column_Name 
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
  Versao 21.03
  Março de 2023
  Tabela: RGTRI
  Campo : Flg_DscSbtOrgPub
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'RGTRI'
                  And Column_Name = 'Flg_DscSbtOrgPub')
  ALTER TABLE dbo.RGTRI ADD Flg_DscSbtOrgPub bit NULL default 0
GO

/*
  Versao 21.03
  Abril de 2023
  Tabela: PARAM
  Campo : FlgAtuPolComMkp
*/
IF NOT EXISTS (
    SELECT 1 FROM Information_Schema.columns
     WHERE Table_Name = 'PARAM'
       AND Column_Name = 'FlgAtuPolComMkp'
)
    ALTER TABLE dbo.PARAM ADD FlgAtuPolComMkp bit NULL DEFAULT 0

GO

/*
   Tabela: PVMIT
   Remove Chave estrangeira: FK_PVMIT_PVMES
*/   
BEGIN TRANSACTION
Declare @sql nvarchar(max)
IF EXISTS(Select name from sys.foreign_keys 
           Where OBJECT_NAME(parent_object_id) = 'PVMIT'
             And name = 'FK_PVMIT_PVMES')
  begin
    while 1 = 1
      begin
        -- elimina FK´s existente
        Select top 1 @sql = N'ALTER TABLE dbo.PVMIT DROP CONSTRAINT ['+NAME+N']'
          From SYS.FOREIGN_KEYS 
          Where OBJECT_NAME(Parent_Object_ID) = 'PVMIT'  
          And OBJECT_NAME(Referenced_Object_ID) = 'PVMES'
    
        if @@ROWCOUNT > 0
		begin
          PRINT @sql
		  Exec (@Sql)
		end	
		BREAK
	      
      end
  end
COMMIT TRANSACTION
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMIT
  Renomear Campo Per_RntBru p/ Per_Rnt
*/
if Exists(Select 1 from Information_Schema.columns
           Where Table_Name = 'PVMIT'
             and Column_Name = 'Per_RntBru')
and not Exists(Select 1 from Information_Schema.columns
               Where Table_Name = 'PVMIT'
               and Column_Name = 'Qtd_InfEntInv')
  exec sp_rename 'dbo.PVMIT.Per_RntBru', 'Per_Rnt', 'COLUMN'
GO


/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campos: Cod_EstPad, Cod_Vendedor, Cod_VendTlmkt
*/
if Exists(Select 1 from Information_Schema.columns
           Where TABLE_NAME = 'PVMCB'
             And COLUMN_NAME = 'Cod_EstPad'
             And IS_NULLABLE = 'NO')
  ALTER TABLE PVMCB ALTER COLUMN Cod_EstPad int null
GO
if Exists(Select 1 from Information_Schema.columns
           Where TABLE_NAME = 'PVMCB'
             And COLUMN_NAME = 'Cod_Vendedor'
             And IS_NULLABLE = 'NO')
  ALTER TABLE PVMCB ALTER COLUMN Cod_Vendedor int null
GO
if Exists(Select 1 from Information_Schema.columns
           Where TABLE_NAME = 'PVMCB'
             And COLUMN_NAME = 'Cod_VendTlmkt'
             And IS_NULLABLE = 'NO')
  ALTER TABLE PVMCB ALTER COLUMN Cod_VendTlmkt int null
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_QtdItens
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_QtdItens')
  ALTER TABLE dbo.PVMCB ADD C_QtdItens int NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrBruto
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrBruto')
  ALTER TABLE dbo.PVMCB ADD C_VlrBruto numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrLiqItens
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrLiqItens')
  ALTER TABLE dbo.PVMCB ADD C_VlrLiqItens numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_PerLuc
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_PerLuc')
  ALTER TABLE dbo.PVMCB ADD C_PerLuc numeric(10,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PDVLT
  Campo : Vlr_SbtRetAnt
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PDVLT'
                  And Column_Name = 'Vlr_SbtRetAnt')
  ALTER TABLE dbo.PDVLT ADD Vlr_SbtRetAnt numeric(18,4) NULL default 0
GO

/*
  Versao.: 21.04
  Criação: Junho de 2023
  Tabela.: PARAM
  Campo..: QtdMesCalcSugCompra
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PARAM'
                  And Column_Name = 'QtdMesCalcSugCompra')
  ALTER TABLE dbo.PARAM ADD QtdMesCalcSugCompra int NULL DEFAULT 0;
GO

/*
  Versao 21.08
  Maio de 2023
  Tabela: PDVIT
  Campo : Vlr_SbtRetAnt 
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PDVIT'
                  And Column_Name = 'Vlr_SbtRetAnt')
  ALTER TABLE dbo.PDVIT ADD Vlr_SbtRetAnt numeric(18,4) NULL default 0
GO

/*
  Versao 21.08
  Maio de 2023
  Tabela: NFSIT
  Campo : Vlr_SbtRetAnt 
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'NFSIT'
                  And Column_Name = 'Vlr_SbtRetAnt')
  ALTER TABLE dbo.NFSIT ADD Vlr_SbtRetAnt numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_QtdUndIte
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_QtdUndIte')
  ALTER TABLE dbo.PVMCB ADD C_QtdUndIte int NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrRepIcm
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrRepIcm')
  ALTER TABLE dbo.PVMCB ADD C_VlrRepIcm numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrSubTri
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrSubTri')
  ALTER TABLE dbo.PVMCB ADD C_VlrSubTri numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrSbtRes
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrSbtRes')
  ALTER TABLE dbo.PVMCB ADD C_VlrSbtRes numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrDscTri
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrDscTri')
  ALTER TABLE dbo.PVMCB ADD C_VlrDscTri numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrDscBon
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrDscBon')
  ALTER TABLE dbo.PVMCB ADD C_VlrDscBon numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrIpi
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrIpi')
  ALTER TABLE dbo.PVMCB ADD C_VlrIpi numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrDscIte
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrDscIte')
  ALTER TABLE dbo.PVMCB ADD C_VlrDscIte numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : C_VlrDespes
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'C_VlrDespes')
  ALTER TABLE dbo.PVMCB ADD C_VlrDespes numeric(18,4) NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PMEST
  Campo : FlgBlqEstTransiNeg: permite negativar pendência em pedido de compra
*/
if Exists (Select 1 from Information_Schema.columns
            Where Table_Name = 'PMEST'
              And Column_Name = 'FlgBlqEstTransiNeg') and
   not Exists(Select 1 from Information_Schema.columns
               Where Table_Name = 'PMEST'
                 and Column_Name = 'FlgQtdPenPedCmpNeg')
  exec sp_rename 'dbo.PMEST.FlgBlqEstTransiNeg', 'FlgQtdPenPedCmpNeg', 'COLUMN'
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PMEST
  Campo : FlgQtdPenPedCmpNeg
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'PMEST'
                  And Column_Name = 'FlgQtdPenPedCmpNeg')
  ALTER TABLE dbo.PMEST ADD FlgQtdPenPedCmpNeg bit NULL default 0
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: PVMCB
  Campo : Obs_PreNot
*/
if not Exists (Select Column_Name from Information_Schema.columns
                Where Table_Name = 'PVMCB'
                  And Column_Name = 'Obs_PreNot')
  ALTER TABLE dbo.PVMCB ADD Obs_PreNot text NULL
GO

/*
  Versao 21.07
  Julho de 2023
  Tabela: ABAIT
  Campo : Flg_Proces: indica se o item já foi processado
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'ABAIT'
                  And Column_Name = 'Flg_Proces')
  ALTER TABLE dbo.ABAIT ADD Flg_Proces bit NULL default 0
GO

/*
Versao 21.04
Julho de 2023
Tabela: ABAIT
Campo : Qtd_Des - Quantidade destino
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'ABAIT'
                  And Column_Name = 'Qtd_Des')
  ALTER TABLE dbo.ABAIT ADD Qtd_Des Int null  default 0
GO

/*
Versao 21.04
Julho de 2023
Tabela: MOVFP
Campo : Cod_InstitPix - Código da Instituição
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MOVFP'
               And Column_Name = 'Cod_InstitPix')
  ALTER TABLE dbo.MOVFP ADD Cod_InstitPix int
GO

/*
Versao 21.04
Julho de 2023
Tabela: MVPIX
Campo : Cod_InstitPix - Código da Instituição
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MVPIX'
               And Column_Name = 'Cod_InstitPix')
  ALTER TABLE dbo.MVPIX ADD Cod_InstitPix int
GO

/*
Versao 21.04
Julho de 2023
Tabela: MVPIX
Campo : Des_CodExtPix - Descrição do Código externo do PIX
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MVPIX'
               And Column_Name = 'Des_CodExtPix')
  ALTER TABLE dbo.MVPIX ADD Des_CodExtPix varchar(80)
GO

/*
Versao 21.04
Julho de 2023
Tabela: MVPIX
Campo : Num_Guid - Número guid da venda
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MVPIX'
               And Column_Name = 'Num_Guid')
  ALTER TABLE dbo.MVPIX ADD Num_Guid varchar(36)
GO

/*
Versao 21.04
Julho de 2023
Tabela: MVPIX
Campo : Des_NsuPix - Número NSU da venda
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MVPIX'
               And Column_Name = 'Des_NsuPix')
  ALTER TABLE dbo.MVPIX ADD Des_NsuPix varchar(80)
GO

/*
Versao 21.04
Julho de 2023
Tabela: MVPIX
Campo : Des_CodCliPix - Descrição do código PIX do cliente
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MVPIX'
               And Column_Name = 'Des_CodCliPix')
  ALTER TABLE dbo.MVPIX ADD Des_CodCliPix varchar(250)
GO

/*
Versao 21.04
Julho de 2023
Tabela: MVPIX
Campo : Des_ChvCanPix - Chave de cancelamento do PIX
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'MVPIX'
               And Column_Name = 'Des_ChvCanPix')
  ALTER TABLE dbo.MVPIX ADD Des_ChvCanPix varchar(80)
GO

/*
Versao 21.04
Julho de 2023
Tabela: PARAM
Campo : Des_CntPix
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'PARAM'
               And Column_Name = 'Des_CntPix')
  ALTER TABLE dbo.PARAM ADD Des_CntPix varchar(100)
GO

/*
Versao 21.04
Julho de 2023
Tabela: FPGCB
Campo : Cod_InstitFin
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'FPGCB'
               And Column_Name = 'Cod_InstitFin')
  ALTER TABLE dbo.FPGCB ADD Cod_InstitFin int
GO 

/*
Versao 21.04
Julho de 2023
Tabela: USUAR
Campo : Cod_OpeCxa
*/
if not Exists (Select 1 from Information_Schema.columns
               Where Table_Name = 'USUAR'
               And Column_Name = 'Cod_OpeCxa')
  ALTER TABLE dbo.USUAR ADD Cod_OpeCxa int
GO


/*
  Versao 21.04
  Agosto de 2023
  Tabela: CN_PEDIT  
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'CN_PEDIT'
                  And Column_Name = 'Qtd_FatMes4')
  ALTER TABLE dbo.CN_PEDIT ADD Qtd_FatMes4 int NULL
GO

if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'CN_PEDIT'
                  And Column_Name = 'Qtd_FatMes5')
  ALTER TABLE dbo.CN_PEDIT ADD Qtd_FatMes5 int NULL
GO

if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'CN_PEDIT'
                  And Column_Name = 'Qtd_FatMes6')
  ALTER TABLE dbo.CN_PEDIT ADD Qtd_FatMes6 int NULL
GO


/*
  Versao 21.04
  Agosto de 2023
  Tabela: AGCOB
  Campo : FlgExibLogoBol
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'AGCOB'
                  And Column_Name = 'FlgExibLogoBol')
  ALTER TABLE dbo.AGCOB ADD FlgExibLogoBol bit NULL default 0
GO

/*
  Versao 21.04
  Agosto de 2023
  Tabela: AGCOB
  Campo : FlgExibDadEstab
*/
if not Exists (Select 1 from Information_Schema.columns
                Where Table_Name = 'AGCOB'
                  And Column_Name = 'FlgExibDadEstab')
  ALTER TABLE dbo.AGCOB ADD FlgExibDadEstab bit NULL default 0
GO

/*
  Versao 21.04
  Agosto de 2023
  Tabela: NFECB
  Campos: Cod_MtvDev
*/
if Exists(Select 1 from Information_Schema.columns
           Where TABLE_NAME = 'NFECB'
             And COLUMN_NAME = 'Cod_MtvDev')
  ALTER TABLE NFECB ALTER COLUMN Cod_MtvDev VARCHAR(5) NULL
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: PRODU
  Campo Prc_Fabric195
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'PRODU'
                   And Column_Name = 'Prc_Fabric195' )
  ALTER TABLE [dbo].[PRODU] ADD [Prc_Fabric195] numeric(18,4) NULL CONSTRAINT [DF_PRODU_Prc_Fabric195] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: PRODU
  Campo Prc_MaxCon195
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'PRODU'
                   And Column_Name = 'Prc_MaxCon195' )
  ALTER TABLE [dbo].[PRODU] ADD [Prc_MaxCon195] numeric(18,4) NULL CONSTRAINT [DF_PRODU_Prc_MaxCon195] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: PRODU
  Campo Prc_Fabric205
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'PRODU'
                   And Column_Name = 'Prc_Fabric205' )
  ALTER TABLE [dbo].[PRODU] ADD [Prc_Fabric205] numeric(18,4) NULL CONSTRAINT [DF_PRODU_Prc_Fabric205] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: PRODU
  Campo Prc_MaxCon205
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'PRODU'
                   And Column_Name = 'Prc_MaxCon205' )
  ALTER TABLE [dbo].[PRODU] ADD [Prc_MaxCon205] numeric(18,4) NULL CONSTRAINT [DF_PRODU_Prc_MaxCon205] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ALTPR
  Campo Prc_Fabric195
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ALTPR'
                   And Column_Name = 'Prc_Fabric195' )
  ALTER TABLE [dbo].[ALTPR] ADD [Prc_Fabric195] numeric(18,4) NULL CONSTRAINT [DF_ALTPR_Prc_Fabric195] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ALTPR
  Campo Prc_MaxCon195
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ALTPR'
                   And Column_Name = 'Prc_MaxCon195' )
  ALTER TABLE [dbo].[ALTPR] ADD [Prc_MaxCon195] numeric(18,4) NULL CONSTRAINT [DF_ALTPR_Prc_MaxCon195] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ALTPR
  Campo Prc_Fabric205
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ALTPR'
                   And Column_Name = 'Prc_Fabric205' )
  ALTER TABLE [dbo].[ALTPR] ADD [Prc_Fabric205] numeric(18,4) NULL CONSTRAINT [DF_ALTPR_Prc_Fabric205] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ALTPR
  Campo Prc_MaxCon205
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ALTPR'
                   And Column_Name = 'Prc_MaxCon205' )
  ALTER TABLE [dbo].[ALTPR] ADD [Prc_MaxCon205] numeric(18,4) NULL CONSTRAINT [DF_ALTPR_Prc_MaxCon205] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: HSPRC
  Campo Vlr_PrcFab195
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'HSPRC'
                   And Column_Name = 'Vlr_PrcFab195' )
  ALTER TABLE [dbo].[HSPRC] ADD [Vlr_PrcFab195] numeric(18,4) NULL CONSTRAINT [DF_HSPRC_Vlr_PrcFab195] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: HSPRC
  Campo Vlr_PrcMaxCon195
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'HSPRC'
                   And Column_Name = 'Vlr_PrcMaxCon195' )
  ALTER TABLE [dbo].[HSPRC] ADD [Vlr_PrcMaxCon195] numeric(18,4) NULL CONSTRAINT [DF_HSPRC_Vlr_PrcMaxCon195] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: HSPRC
  Campo Vlr_PrcFab205
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'HSPRC'
                   And Column_Name = 'Vlr_PrcFab205' )
  ALTER TABLE [dbo].[HSPRC] ADD [Vlr_PrcFab205] numeric(18,4) NULL CONSTRAINT [DF_HSPRC_Vlr_PrcFab205] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: HSPRC
  Campo Vlr_PrcMaxCon205
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'HSPRC'
                   And Column_Name = 'Vlr_PrcMaxCon205' )
  ALTER TABLE [dbo].[HSPRC] ADD [Vlr_PrcMaxCon205] numeric(18,4) NULL CONSTRAINT [DF_HSPRC_Vlr_PrcMaxCon205] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ABCIT
  Campo PF_19_5_ALC
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ABCIT'
                   And Column_Name = 'PF_19_5_ALC' )
  ALTER TABLE [dbo].[ABCIT] ADD [PF_19_5_ALC] numeric(12,2) NULL CONSTRAINT [DF_ABCIT_PF_19_5_ALC] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ABCIT
  Campo PMC_19_5_ALC
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ABCIT'
                   And Column_Name = 'PMC_19_5_ALC' )
  ALTER TABLE [dbo].[ABCIT] ADD [PMC_19_5_ALC] numeric(12,2) NULL CONSTRAINT [DF_ABCIT_PMC_19_5_ALC] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ABCIT
  Campo PF_20_5_ALC
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ABCIT'
                   And Column_Name = 'PF_20_5_ALC' )
  ALTER TABLE [dbo].[ABCIT] ADD [PF_20_5_ALC] numeric(12,2) NULL CONSTRAINT [DF_ABCIT_PF_20_5_ALC] DEFAULT (0)
GO

/*
  Versao 21.04
  Dezembro de 2023
  Tabela: ABCIT
  Campo PMC_20_5_ALC
*/
IF NOT EXISTS ( Select 1 from Information_Schema.columns
                 Where Table_Name = 'ABCIT'
                   And Column_Name = 'PMC_20_5_ALC' )
  ALTER TABLE [dbo].[ABCIT] ADD [PMC_20_5_ALC] numeric(12,2) NULL CONSTRAINT [DF_ABCIT_PMC_20_5_ALC] DEFAULT (0)
GO

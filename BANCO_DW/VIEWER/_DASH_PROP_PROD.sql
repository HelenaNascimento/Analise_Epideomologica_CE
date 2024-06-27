USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_PROP_PROD]    Script Date: 27/06/2024 13:27:59 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [dbo].[DASH_PROP_PROD] AS 

SELECT 
    p.Codigo, 
    p.Descricao as Des_Produt, 
    p.Unidade_Venda, 
    p.Tip_LisPis,       
    fb.Fantasia as Des_Fabric, 
    pr.Cod_ClaTri, 
    p.Ctrl_Preco, 
    p.Cod_Ean,    
    pr.Prc_CusLiqEnt, 
    pr.Prc_CusMed, 
    pr.Prc_Venda, 
    pr.Prc_MaxCon, 
    pr.Prc_UltEnt, 
    pr.Per_DscAut,   
    pr.Qtd_Dispon,        
    pr.Qtd_Reserv, 
    pr.Qtd_Avaria, 
    pr.Qtd_Transi, 
    p.Qtd_FraVen, 
    p.Qtd_Embalagem 
From PROD_2023.dbo.PRXES pr, PROD_2023.dbo.PRODU p, PROD_2023.dbo.FABRI fb 
Where pr.Cod_Estabe = 1
And pr.Cod_Produt = p.Codigo 
And p.Cod_Fabricante = fb.Codigo 
AND p.Flag_ImprClassif1 <> 'N' 
AND ((p.Dat_Cadastro <= getdate()) OR (p.Dat_Cadastro IS NULL) OR (p.Dat_Cadastro = ''))
AND ((pr.Dat_PrcAtual <= getdate()) OR (Pr.Dat_PrcAtual IS NULL) OR (Pr.Dat_PrcAtual = '')) 
GO



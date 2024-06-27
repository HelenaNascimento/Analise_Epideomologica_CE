USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_Vendas_Trimestrais]    Script Date: 27/06/2024 13:32:34 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE view [dbo].[DASH_Vendas_Trimestrais] as

        SELECT 
                sum(QTD_VENDAS) as QTD_VENDAS,
                format(sum(VLR_VENDA), 'c', 'pt-br') as Vlr_Vendas,
				sum(VLR_VENDA) as vlr_sem_form,
				Ano,
				Mes
        FROM 
                [DW_PROD].[dbo].[TOTAL_ANO_MES]
        WHERE 
                ano = year(getdate())
				and mes >= month(getdate()) - 4
		group by 
				Ano,
				Mes

				
GO



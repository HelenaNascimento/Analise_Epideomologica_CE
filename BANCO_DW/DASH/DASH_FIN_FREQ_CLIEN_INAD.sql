USE [DW_PROD]
GO

/****** Object:  View [dbo].[DASH_FIN_FREQ_CLIEN_INAD]    Script Date: 11/04/2024 17:07:13 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





CREATE VIEW [dbo].[DASH_FIN_FREQ_CLIEN_INAD] AS
SELECT 
	'Um' = (select count(distinct codigo) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 1	and  Dias_Atraso < 50 ),
	'Cinquenta' = (select count(distinct codigo) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 50	and  Dias_Atraso < 100 ),
	'Cem' = (select count(distinct codigo) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 100	and  Dias_Atraso < 150 ),
	'Cento_50' = (select count(distinct codigo) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 150	and  Dias_Atraso < 200 ),
	'Duzentos' = (select count(distinct codigo) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 200	and  Dias_Atraso < 250 ),
	'Duzentos_50' = (select count(distinct codigo) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 250)




/*
CREATE VIEW DASH_FIN_FREQ_CLIEN_INAD AS
select 
'1|-29046' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 1	and  Dias_Atraso < 29046 ),
'29046|-- 58092' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 29046	and  Dias_Atraso < 58092),
'58092|-- 87138' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 58092	and  Dias_Atraso < 87138 ),
'87138|-- 29046' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 87138   and  Dias_Atraso < 116184),
'29046|-- 145230' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 116184  and  Dias_Atraso < 145230),
'145230|-- 174276' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 145230  and  Dias_Atraso < 174276),
'174276|-- 203322' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 174276  and  Dias_Atraso < 203322),
'203322|-- 232368' = (select count(Dias_Atraso) from VW_FIN_CLIEN_INAD where mes = '2' and Dias_Atraso >= 203322  and  Dias_Atraso < 232368)
*/


--INSERT INTO DASH_FIN_FREQ_CLIEN_INAD

GO



USE DMD
GO

SELECT DISTINCT
	 PRXES.Cod_ClaTri AS 'productTaxCode'
	,RTXCT.Alq_IcmIntReg AS 'icmsCreditAliquot'
	,RTXCT.Per_RedBasCalIntLoc AS 'icmsCreditAliquotReducer'
	,RTXCT.Alq_DebSbtSai AS 'icmsStDebitAliquot'
	,RTXCT.Per_RedBasCalDebSbtRecSai AS 'icmsStDebitAliquotReducer'
	FROM POCOM
	INNER JOIN PCXPR
	ON POCOM.Id_PolCom = PCXPR.Id_PolCom
	INNER JOIN PRXES
	ON PCXPR.Cod_Produt = PRXES.Cod_Produt
	INNER JOIN RTXCT
	ON PRXES.Cod_ClaTri = RTXCT.Cod_ClaTri
	INNER JOIN ENXES
	ON PCXPR.Cod_Produt = PRXES.Cod_Produt
	WHERE POCOM.Flg_Web = 1
	AND   POCOM.Dat_Termino > GETDATE()
	AND   RTXCT.Alq_IcmIntReg > 17
	AND   RTXCT.Alq_IcmIntReg < 25
	--AND RTXCT.Alq_DebSbtSai > 0
	ORDER BY PRXES.Cod_ClaTri ASC
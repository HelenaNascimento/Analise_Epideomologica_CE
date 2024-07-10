INSERT INTO R_FABRI([Codigo], [Fantasia], [_Cod_ForPref], [Cgc_Cpf], [Sta_ClaAbcVal], [Per_ParticFat], [_Flg_TransmItx], [_Flg_TransmPro], [Email], [Ord_Bloco], [Des_Bloco],
			[Qtd_PrzMaxFat], [Per_DscMaxVis], [Per_DscMaxPrz], [Flg_Desconto], [Flg_BlqInfPar], [Per_DscBasComNor], [Per_DscBasTax], [Per_ComBasTax], [_Cod_DisFab],
			[Usuario], [Transacao], [_Flg_BlqIms], [Flg_Exclusivi], [Cod_Estado], [Flg_InfCnvNfs], [_Flg_TransmGnx], [_Cod_FabGnx], [Bloqueado], [Per_MarkupCusCom],
			[CodAnt], [NovoCodigo], [_Flg_TransmNeo])
SELECT	[Codigo], [Fantasia], [_Cod_ForPref], [Cgc_Cpf], FS.[Sta_ClaAbcVal], FS.[Per_ParticFat], [_Flg_TransmItx], [_Flg_TransmPro], [Email], [Ord_Bloco], [Des_Bloco],
		[Qtd_PrzMaxFat], [Per_DscMaxVis], [Per_DscMaxPrz], [Flg_Desconto], [Flg_BlqInfPar], [Per_DscBasComNor], [Per_DscBasTax], [Per_ComBasTax], [_Cod_DisFab],
		[Usuario], [Transacao], [_Flg_BlqIms], [Flg_Exclusivi], [Cod_Estado], [Flg_InfCnvNfs], [_Flg_TransmGnx], [_Cod_FabGnx], [Bloqueado], [Per_MarkupCusCom],
		[CodAnt], [NovoCodigo], [_Flg_TransmNeo]
FROM PROD_2023.DBO.FABRI FB
		INNER JOIN PROD_2023.DBO.FBXES FS ON FB.Codigo = FS.Cod_Fabric
WHERE Cod_Estabe = 1
AND Codigo <> FB.Codigo

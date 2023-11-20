SELECT DISTINCT 
	CODIGO,
	REF_FABRICANTE,
	COD_EAN AS EAN13,
	convert(decimal(10,2),PESO) as PESO,
	UNIDADE_VENDA,
	QTD_EMBALAGEM,
	LRG_EMB,
	ALT_EMB,
	PRF_EMB,
	convert(decimal(10,2), VOL_EMB) as VOL_EMB,
	UND_EMBCMP,
	convert(decimal(10,2), LRG_EMBCMP) as Lrg_Emb_Compra,
	convert(decimal(10,2), ALT_EMBCMP) as Alt_Emb_Compra,
	convert(decimal(10,2), PRF_EMBCMP) as Prof_Emb_Compra,
	convert(decimal(20,2), PES_EMBCMP) as PES_EMBCMP,
	IsNull(QTD_EMBPALETE, 0) as Latro,
	IsNull(QTD_CAMPALETE, 0) as Camadas,
	COD_EANEMBCMP as DUM14
	FROM PRODU PR
		INNER JOIN PRXES ES ON PR.CODIGO = ES.COD_PRODUT
	WHERE COD_ESTABE = 1
		AND PESO > 1
		AND flag_ImprClassif1 <> 'N'
		AND es.Flg_Bloqueado = 0
		AND es.Flg_BlqCmp = 0
		AND es.Flg_BlqVen = 0
		and ref_fabricante is not null
		and alt_embcmp > 0
	--	and qtd_embpalete > 0

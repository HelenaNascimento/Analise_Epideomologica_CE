SELECT DISTINCT 
    CASE WHEN PRXES.Cod_Estabe = 0 THEN
        CONCAT('85_', PRXES.Cod_ClaTri)
    WHEN PRXES.Cod_Estabe = 1 THEN
        CONCAT('88_', PRXES.Cod_ClaTri)
    WHEN PRXES.Cod_Estabe = 3 THEN
        CONCAT('87_', PRXES.Cod_ClaTri)
    WHEN PRXES.Cod_Estabe = 4 THEN
        CONCAT('80_', PRXES.Cod_ClaTri)
    END 'productTaxCode',
    RTXCT.Alq_AgrDebSai as 'ivaAgregator',
    RTXCT.Per_RedBasCalDebSai as 'debitBaseReducer'
    --maximumLimitPercentageFActorIcmsST
    /*
    minimumLimitPercentageFactorIcmsSt = 
        CASE WHEN Charindex (RTXCT.Tip_PrcBasDebSbt, 'T;V;M;F;P;C;D') > 0 THEN
                                    CASE WHEN (ISNULL (RTXCT.Per_RedBasCalDebSai, 0) > 0)
                                                    AND (RTXCT.Tip_CreSbtSai <> 'F') THEN
            (1 - ISNULL (RTXCT.Per_RedBasCalIntReg,
                    0) / 100) * (1 - ISNULL (RTXCT.Per_RedBasCalDebSai,
                    0) / 100) * (1 + ISNULL (RTXCT.Alq_AgrDebSai,
                    0) / 100) * ISNULL (RTXCT.Per_LimMinIcmSbtSaiRedDeb,
                0)
        ELSE
            0
        END
    ELSE
        0
    END
    */
    --otherDEbitBaseReducers

    /*
select     
    ISNULL (RTXCT.Per_RedBasCalDebSai, 0) as Per_RedBasCalDebSai,
	ISNULL (RTXCT.Alq_AgrDebSai, 0) Alq_AgrDebSai,
	ISNULL (RTXCT.Per_LimMinIcmSbtSaiRedDeb, 0) as Per_LimMinIcmSbtSaiRedDeb,
	(1 - ISNULL (RTXCT.Per_RedBasCalIntReg, 0) / 100) * 
	(1 - ISNULL (RTXCT.Per_RedBasCalDebSai, 0) / 100) *
	(1 + ISNULL (RTXCT.Alq_AgrDebSai, 0) / 100),
	Per_LimMinIcmSbtSai
FROM
    PRXES 
		INNER JOIN RTXCT ON PRXES.Cod_ClaTri = RTXCT.Cod_ClaTri
where 
RTXCT.Tip_PrcBasDebSbt  in ('T', 'V', 'M', 'F', 'P', 'C', 'D') 
and RTXCT.Tip_CreSbtSai <> 'F'
and (Per_LimMinIcmSbtSaiRedDeb > 0 or Alq_AgrDebSai > 0 or Per_RedBasCalDebSai >0 )*/
FROM
    PRXES
    INNER JOIN RTXCT ON PRXES.Cod_ClaTri = RTXCT.Cod_ClaTri

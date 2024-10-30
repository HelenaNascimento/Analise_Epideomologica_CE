SELECT DISTINCT CASE
                  WHEN prxes.cod_estabe = 0 THEN Concat('85_', prxes.cod_clatri)
                  WHEN prxes.cod_estabe = 1 THEN Concat('88_', prxes.cod_clatri)
                  WHEN prxes.cod_estabe = 3 THEN Concat('87_', prxes.cod_clatri)
                  WHEN prxes.cod_estabe = 4 THEN Concat('80_', prxes.cod_clatri)
                END 'productTaxCode',
                minimumLimitPercentageFactorIcmsSt = CASE
                                                       WHEN
                Charindex(rtxct.tip_prcbasdebsbt, 'T;V;M;F;P;C;D') > 0 THEN
                                                         CASE
                WHEN( Isnull(rtxct.per_redbascaldebsai, 0) > 0 )
                    AND ( rtxct.tip_cresbtsai <> 'F' ) THEN ( 1 -
                Isnull(rtxct.per_redbascalintreg, 0) / 100 ) * ( 1 -
                Isnull(rtxct.per_redbascaldebsai, 0) / 100 ) * ( 1 +
                Isnull(rtxct.alq_agrdebsai, 0) / 100 ) *
                Isnull(rtxct.per_limminicmsbtsaireddeb, 0)
                ELSE 0
                END
                ELSE 0
                END
FROM   prxes
       INNER JOIN rtxct
               ON prxes.cod_clatri = rtxct.cod_clatri 
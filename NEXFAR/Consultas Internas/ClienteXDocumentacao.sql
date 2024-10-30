SELECT DISTINCT 
    clien.codigo  AS "clientId",
    clien.cgc_cpf AS "cnpj",
    CASE
      WHEN clien.num_anvisa IS NOT NULL THEN 'ANVISA'
      WHEN clien.num_cerreg IS NOT NULL THEN 'CRF'
      WHEN clien.num_alvfun IS NOT NULL THEN 'VISA'
      WHEN clien.licenca_saude IS NOT NULL THEN 'CONTROLADO'
      ELSE ''
    END           AS "type",
    CASE
      WHEN clien.val_anvisa < Getdate()
            OR clien.val_cerreg < Getdate()
            OR clien.val_alvfun < Getdate()
            OR clien.val_licsau < Getdate() THEN 'VENCIDO'
      WHEN clien.val_anvisa >= Getdate()
            OR clien.val_cerreg >= Getdate()
            OR clien.val_alvfun >= Getdate()
            OR clien.val_licsau >= Getdate() THEN 'REGULAR'
      ELSE ''
    END           AS "status"
FROM   clien
       INNER JOIN pcxcl
               ON pcxcl.cod_client = clien.codigo
       INNER JOIN pocom
               ON pcxcl.id_polcom = pocom.id_polcom
WHERE  pocom.id_polcom IN( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate ()
       AND clien.pessoa = 'J'
       AND clien.cod_estado IN ( 'BA', 'CE', 'PE' )
       AND clien.flg_blqprm = 0
       AND ( clien.num_anvisa IS NOT NULL
              OR clien.num_cerreg IS NOT NULL
              OR clien.num_alvfun IS NOT NULL
              OR clien.licenca_saude IS NOT NULL )
       AND ( clien.val_anvisa IS NOT NULL
              OR clien.val_cerreg IS NOT NULL
              OR clien.val_alvfun IS NOT NULL
              OR clien.val_licsau IS NOT NULL ) 
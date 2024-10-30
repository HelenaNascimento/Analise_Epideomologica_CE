SELECT DISTINCT 
  clien.razao_social                AS "name",
  clien.cgc_cpf                     AS "cnpj",
  clien.codigo                      AS "externalId",
  clien.razao_social                AS "razaoSocial",
  CASE
    WHEN Len(clien.fone1) < 18 THEN
    Concat(clien.cod_ddd_1, ' ', clien.fone1)
  END                               AS "phone",
  clien.cep                         AS "cep",
  Concat('Nova ', clien.cod_estado) AS "address",
  clien.cod_estado                  AS "state",
  CASE
    WHEN clien.bloqueado = 0 THEN 'REGULAR'
    ELSE 'IRREGULAR'
  END                               AS "creditStatus",
  CASE
    WHEN clien.val_licsau < Getdate() THEN 'IRREGULAR'
    WHEN clien.val_cerreg < Getdate() THEN 'IRREGULAR'
    WHEN clien.val_anvisa < Getdate() THEN 'IRREGULAR'
    ELSE 'REGULAR'
  END                               AS "documentationStatus",
  CASE
    WHEN clien.cod_ramoatividade = 11 THEN 1
    ELSE 0
  END                               AS "drugstore",
  CASE
    WHEN clien.flg_blqprm = 0 THEN 'ACTIVE'
    ELSE 'INACTIVE'
  END                               AS "status"
FROM   clien
       INNER JOIN cidad
               ON clien.cod_cidade = cidad.codigo
       INNER JOIN pcxcl
               ON clien.codigo = pcxcl.cod_client
       INNER JOIN pocom
               ON pcxcl.id_polcom = pocom.id_polcom
WHERE  pocom.id_polcom IN ( 3005, 3015, 3004, 3003 )
       AND pocom.dat_termino > Getdate ()
       AND clien.pessoa = 'J'
       AND clien.cod_estado IN ( 'BA', 'CE', 'PE' )
       AND clien.flg_blqprm = 0 
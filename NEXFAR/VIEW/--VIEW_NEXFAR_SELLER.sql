CREATE VIEW VIEW_NEXFAR_SELLER AS 

SELECT DISTINCT VENDE.Nome_Guerra AS "name",
                PCXVE.Cod_Vended AS "externalId",
                0 AS "televendas",
                0 AS "isSupervisor",
                 VENDE.Cpf AS "cpf"
FROM PCXVE
INNER JOIN POCOM ON PCXVE.Id_PolCom = POCOM.Id_PolCom
INNER JOIN VENDE ON PCXVE.Cod_Vended = VENDE.Codigo
INNER JOIN SUPER ON VENDE.Cod_Supervisor = SUPER.Codigo
WHERE POCOM.id_PolCom IN (3005,
                          3015,
                          3004,
                          3003)
  AND POCOM.Dat_Termino > GETDATE ()
  AND VENDE.Bloqueado = 0
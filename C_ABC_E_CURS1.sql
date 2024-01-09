
declare
@codEstab int = 1,
@DatIn smalldatetime = '20230101',
@DatFim smalldatetime= '20231231',
@CFABR int = 164,
@CProd int,
@PrcUlt decimal(20,2),
@Dat_Ent smalldatetime

Declare CursorProd CURSOR FOR 
SELECT DISTINCT Codigo 
FROM PRODU
WHERE Cod_Fabricante = @CFABR 

OPEN CursorProd
FETCH NEXT FROM CursorProd INTO @CProd
WHILE @@FETCH_STATUS = 0
BEGIN
DECLARE Prc_Ult cursor for
	SELECT 
		top 1 
			it.Cod_Produto, 
			Prc_UniFat, 		
			Dat_Entrada
		from NFEIT IT
			inner join NFECB cb0 on it.cod_estabe = cb0.Cod_Estabe and it.Protocolo = cb0.Protocolo
						where 
							it.Cod_Estabe = @codEstab
							and Tip_NF <> 'D' 
							and cb0.Dat_Entrada >= @DatIn
							and cb0.Dat_Entrada <= @DatFim
							--and it.Cod_Fabricante = @CFABR 
							And it.Cod_Produto = @CProd
							and status not in ('A', 'C') 
						order by Dat_Emissao desc
	open Prc_Ult
	FETCH NEXT FROM Prc_Ult INTO @CProd, @PrcUlt, @Dat_Ent 
	WHILE @@FETCH_STATUS = 0
	BEGIN

		PRINT CAST(@CProd AS NVARCHAR(5)) + ';' + CAST(@PrcUlt AS NVARCHAR(25)) + ';' + CAST(@Dat_Ent AS NVARCHAR(8))

		FETCH NEXT FROM Prc_Ult INTO @CProd, @PrcUlt, @Dat_Ent
	END;
	CLOSE Prc_Ult
	DEALLOCATE Prc_Ult
FETCH NEXT FROM CursorProd INTO @CProd
END
CLOSE CursorProd
DEALLOCATE CursorProd
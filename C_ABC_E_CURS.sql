declare 
	@codEstab int = 1,
	@CodFab int = 33,
	@DatIn smalldatetime = '20230101',
	@DatFim smalldatetime = '20231130',
	@CodProd int, -- = 21165,
	@CodEAN varchar(14),
	@Fabri varchar(50),
	@Produ varchar (50), 
	@QtdEntComp int, 
	@QtdEntBoni int, 
	@PrcFab decimal(20,2), 
	@PrcVend decimal(20,2), 
	@PrcCusMed decimal(20,2),
	@PrcComp decimal(20,2),
	@PrcUltComp decimal(20,2),
	@DatComp smalldatetime

	Declare CursorProd CURSOR FOR 
		SELECT DISTINCT Codigo 
		FROM PRODU
		WHERE Cod_Fabricante = @CodFab

	OPEN CursorProd

	FETCH NEXT FROM CursorProd INTO @CodProd
	WHILE @@FETCH_STATUS = 0
	BEGIN
		DECLARE Curv_ABC_ENT cursor for
				SELECT top 1 
					IT.Cod_Estabe, 
					it.Cod_Produto, 
					Prc_Unitar,
					Prc_Unitario,
					Prc_UniFat, 
					Dat_Entrada
				from NFEIT IT
					inner join NFECB cb0 on it.cod_estabe = cb0.Cod_Estabe and it.Protocolo = cb0.Protocolo
					where 
						it.Cod_Estabe = @codEstab
						and Tip_NF <> 'D' 
						and cb0.Dat_Entrada >= @DatIn
						and cb0.Dat_Entrada <= @DatFim
						and it.Cod_Produto = @CodProd
						and status not in ('A', 'C') 
						and Tip_NF <> 'D'
					order by Dat_Emissao desc

	OPEN Curv_ABC_ENT;
	FETCH NEXT FROM Curv_ABC_ENT INTO @codEstab, @CodProd, @PrcUnit, @PrcUnit1, @PrcUnit2, @DatComp;

    WHILE @@FETCH_STATUS = 0
    BEGIN

		PRINT CAST(@codEstab AS NVARCHAR(255)) +';'+ CAST(@CodProd AS NVARCHAR(255)) +';'+ CAST(@PrcUltComp AS NVARCHAR(255)) +';'+ CAST(@DatComp AS VARCHAR(255)) 

	FETCH NEXT FROM Curv_ABC_ENT INTO  @codEstab, @CodProd, @PrcUnit, @PrcUnit1, @PrcUnit2, @DatComp;

	END;
	CLOSE Curv_ABC_ENT
	DEALLOCATE Curv_ABC_ENT

FETCH NEXT FROM CursorProd INTO @CodProd
END

CLOSE CursorProd
DEALLOCATE CursorProd
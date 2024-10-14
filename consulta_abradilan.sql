DECLARE 
	@CodProd int,
	@Dat_Mov smalldatetime,
	@Qtd_Sld int,
	@X int = 0,
	@SldFim int

DECLARE PRODUTO CURSOR FOR
	SELECT Codigo
	FROM PRODU
	ORDER BY 1
OPEN PRODUTO

FETCH NEXT FROM PRODUTO INTO @CodProd
WHILE @@FETCH_STATUS = 0
BEGIN
	-- Cursor para pegar a data de movimentação
	DECLARE DAT_MOV CURSOR FOR
		SELECT cod_produt, MAX(Dat_Movime)
		FROM PRSLD
		WHERE 
			Cod_Produt = @CodProd
			AND Qtd_SldPra > 0
			AND Dat_Movime >= '20210801'
			AND Dat_Movime <= '20210831'
		GROUP BY cod_produt
	OPEN DAT_MOV

	FETCH NEXT FROM DAT_MOV INTO @CodProd, @Dat_Mov
	WHILE @@FETCH_STATUS = 0
	BEGIN
		-- Cursor para pegar a quantidade do saldo
		DECLARE Qtd_Sld CURSOR FOR
			SELECT Qtd_SldPra
			FROM PRSLD
			WHERE 
				Cod_Produt = @CodProd
				AND Dat_Movime = @Dat_Mov
		
		OPEN Qtd_Sld
		FETCH NEXT FROM Qtd_Sld INTO @Qtd_Sld
		WHILE @@FETCH_STATUS = 0
		BEGIN
			-- Soma o saldo
			SET @X += @Qtd_Sld

			FETCH NEXT FROM Qtd_Sld INTO @Qtd_Sld
		END

		CLOSE Qtd_Sld
		DEALLOCATE Qtd_Sld

		FETCH NEXT FROM DAT_MOV INTO @CodProd, @Dat_Mov
	END

	CLOSE DAT_MOV
	DEALLOCATE DAT_MOV

	FETCH NEXT FROM PRODUTO INTO @CodProd
END

CLOSE PRODUTO
DEALLOCATE PRODUTO

-- Resultado final da soma
SELECT @X AS Sld_Total

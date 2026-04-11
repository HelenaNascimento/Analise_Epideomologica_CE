use DMD
GO 


DECLARE 
	@CodProd INT, 
	@Cod_Estabe int = 1,
	@Dat_Ini smalldatetime  = '20230101',
	@Dat_fim smalldatetime = '20260331',
	@Dt_Alt smalldatetime,
	@Prc_Pr varchar (20),
	@CM varchar (20)

Declare Cod_Produ Cursor for 
	SELECT 
		DISTINCT
		PR.Codigo
		FROM PRODU PR
			INNER JOIN PRXES ES on PR.CODIGO = ES.COD_PRODUT  
	WHERE ES.Cod_Estabe = @Cod_Estabe
		AND PR.Cod_Fabricante = 319
	order by 1

OPEN Cod_Produ

	FETCH NEXT FROM Cod_Produ INTO @CodProd
	WHILE @@FETCH_STATUS = 0
	BEGIN
		DECLARE HSPRC CURSOR FOR
			SELECT 
				top 1
				@CodProd,
				replace(Vlr_PrcVen, '.', ',') as Vlr_PrcVen,
				replace(Vlr_CusMedCom, '.', ',') as Vlr_CusMedCom,
				Dat_Alteracao				
			FROM HSPRC HS
					INNER JOIN PRXES ES ON HS.Cod_Estabe = ES.Cod_Estabe AND HS.Cod_Produto = ES.Cod_Produt
			WHERE hs.Cod_Estabe = @Cod_Estabe
			and Dat_Alteracao <= @Dat_fim
			and Cod_Produto = @CodProd
			--GROUP BY 
			--	Vlr_PrcVen,
			--	Vlr_CusMedCom,
			--	Dat_Alteracao
			--HAVING (Vlr_CusMedCom > 0)
			order by 4 desc
		OPEN HSPRC;
		FETCH NEXT FROM HSPRC INTO @CodProd, @Prc_Pr, @CM, @Dt_Alt;

	WHILE @@FETCH_STATUS = 0
		BEGIN
			
			PRINT CAST(@CodProd AS NVARCHAR(10)) + ';' + CAST(@Prc_Pr AS NVARCHAR(20)) + ';' + CAST(@CM AS NVARCHAR(20)) 

			FETCH NEXT FROM HSPRC INTO @CodProd, @Prc_Pr, @CM, @Dt_Alt;

		END;
	CLOSE HSPRC
	DEALLOCATE HSPRC

FETCH NEXT FROM Cod_Produ INTO @CodProd
END;
CLOSE Cod_Produ
DEALLOCATE Cod_Produ

/*
select * from HSPRC
WHERE Cod_Estabe = 1
	AND Cod_Produto = 27
	AND Dat_Alteracao >= '20221225'
ORDER BY 2
*/
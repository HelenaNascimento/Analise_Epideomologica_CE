declare 
	@codEstab int = 1,
	@CPROD INT,
	@CODI INT,
	@DESC VARCHAR (80),
	@UNVEND VARCHAR(2),
	@FANT VARCHAR(50),
	@PCMED numeric (20,2),
	@PRVEND numeric (20,2),
	@PRCULTENT numeric(20,2),
	@Dat_Ini smalldatetime  = '20230101',
	@Dat_fim smalldatetime = '20231231',
	@QTD_SALD INT,
	@DATA smalldatetime

	PRINT 'Código' + ';' + 'Descrição' + ';' + 'Fantasia' + ';' + 'Custo_Médio' + ';' + 'Prc_Venda' + ';' + 'Prc_UltEntrada' + ';' + 'Qtd_Estoque'

	Declare C_Prod CURSOR FOR 

			SELECT 
				p.Codigo, 
				p.Descricao as Des_Produt, 
				p.Unidade_Venda, 
				fb.Fantasia as Des_Fabric
			From PRXES pr, PRODU p, FABRI fb 
			Where pr.Cod_Estabe = 1 
			And pr.Cod_Produt = p.Codigo 
			And p.Cod_Fabricante = fb.Codigo 
			AND p.Flag_ImprClassif1 <> 'N' 
			AND ((p.Dat_Cadastro <= '20231231') OR (p.Dat_Cadastro IS NULL) OR (p.Dat_Cadastro = '')) 
			AND ((pr.Dat_PrcAtual <= '20231231') OR (Pr.Dat_PrcAtual IS NULL) OR (Pr.Dat_PrcAtual = '')) 
			ORDER BY 2
	OPEN C_Prod

	FETCH NEXT FROM C_Prod INTO @CPROD, @DESC, @UNVEND, @FANT
	WHILE @@FETCH_STATUS = 0
	BEGIN 
		DECLARE ESTOQ CURSOR FOR

		SELECT 
		TOP 1
		Cod_Produt,
		Qtd_SldPra,
		Dat_Movime
		FROM PRSLD 
		WHERE Cod_Estabe = 1
		AND Cod_Produt = @CPROD
		--AND Dat_Movime >= @Dat_Ini 
		AND Dat_Movime <= @Dat_fim
		AND Qtd_SldPra > 0
		order by 3 desc

	OPEN ESTOQ;
		FETCH NEXT FROM ESTOQ INTO @CODI, @QTD_SALD, @DATA;
		WHILE @@FETCH_STATUS = 0

	BEGIN
		DECLARE HSPRC CURSOR FOR
			SELECT 
				top 1
				@CODI,
				Vlr_PrcVen,
				Vlr_CusMedCom,
				Dat_Alteracao				
			FROM HSPRC HS
					INNER JOIN PRXES ES ON HS.Cod_Estabe = ES.Cod_Estabe AND HS.Cod_Produto = ES.Cod_Produt
			WHERE hs.Cod_Estabe = 1
			and Dat_Alteracao <= '20231231'
			and Cod_Produto = @CODI
			and Vlr_PrcVen > 0

			order by 4 desc
		OPEN HSPRC;
		FETCH NEXT FROM HSPRC INTO @CODI, @PRVEND, @PCMED, @Dat_fim;
		WHILE @@FETCH_STATUS = 0
		BEGIN
		DECLARE Curv_ABC_ENT cursor for
				SELECT top 1
					IT.Cod_Estabe, 
					it.Cod_Produto, 
					Prc_Unitario,
					Dat_Entrada
				from NFEIT IT
					inner join NFECB cb0 on it.cod_estabe = cb0.Cod_Estabe and it.Protocolo = cb0.Protocolo
					where 
						it.Cod_Estabe = 1
						and Tip_NF <> 'D' 
						and cb0.Dat_Entrada <= '20231231'
						and it.Cod_Produto = @CODI
						and status not in ('A', 'C') 
					order by Dat_Emissao desc

	OPEN Curv_ABC_ENT;
	FETCH NEXT FROM Curv_ABC_ENT INTO @codEstab, @CODI, @PRCULTENT, @Dat_fim;
    WHILE @@FETCH_STATUS = 0
	BEGIN
			
			PRINT CAST(@CODI AS NVARCHAR(10)) + ';' + CAST(@DESC AS VARCHAR(80)) + ';' + CAST(@FANT AS VARCHAR(80)) + ';' + CAST(@PCMED AS NVARCHAR(20)) + ';' + CAST(@PRVEND AS NVARCHAR(20)) + ';' + CAST(@PRCULTENT AS NVARCHAR(20)) + ';' + CAST(@QTD_SALD AS NVARCHAR(20))
			
			FETCH NEXT FROM Curv_ABC_ENT INTO @codEstab, @CODI, @PRCULTENT, @Dat_fim;
			END;
			CLOSE Curv_ABC_ENT
			DEALLOCATE Curv_ABC_ENT	
	
	FETCH NEXT FROM HSPRC INTO @CODI, @PRVEND, @PCMED, @Dat_fim;
	END;
	CLOSE HSPRC
	DEALLOCATE HSPRC



FETCH NEXT FROM ESTOQ INTO @CODI, @QTD_SALD, @DATA;
END;
CLOSE ESTOQ
DEALLOCATE ESTOQ
		

FETCH NEXT FROM C_Prod INTO  @CPROD, @DESC, @UNVEND, @FANT
END;
CLOSE C_Prod
DEALLOCATE C_Prod
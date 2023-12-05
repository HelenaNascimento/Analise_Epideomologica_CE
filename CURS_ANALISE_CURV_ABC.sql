DECLARE 
	@CFabric int,
	@Fabric varchar(50), 
	@CEan varchar(13), 
	@CProd int, 
	@Desc varchar(50), 
	@Dat smalldatetime,
	@PrcUnit decimal(20,2), 
	@QtdComp int, 
	@Prot decimal(20,2),
	@PrcVend decimal(20,2),
	@CusMed decimal(20,2)
Declare CursorProd CURSOR FOR 
SELECT DISTINCT Codigo 
FROM PRODU
WHERE Cod_Fabricante = 588

OPEN CursorProd

FETCH NEXT FROM CursorProd INTO @CProd
WHILE @@FETCH_STATUS = 0
BEGIN
	DECLARE Curv_ABC_ENT cursor for
			select top 1
				pr.Cod_Fabricante,
				FB.Fantasia,
				cod_ean,
				PR.Codigo,
				descricao,
				Dat_Entrada,
				Prc_UniFat,
				SUM(it.Qtd_PedFat) AS Qtd_Comp,
				it.protocolo,
				ES.prc_venda,
				Custo = case 
						when es.Prc_CusMed > 0 then es.Prc_CusMed 
						when es.Prc_CusMed = 0 then Prc_CusLiqEnt
				end
				from PRODU PR
					Inner join PRXES ES ON PR.CODIGO = ES.COD_PRODUT
					inner join NFEIT IT ON PR.CODIGO = IT.Cod_Produto AND ES.COD_ESTABE = IT.COD_ESTABE 
					inner join NFECB CB ON IT.COD_ESTABE = CB.COD_ESTABE  AND IT.PROTOCOLO = CB.PROTOCOLO
					left join FABRI FB  ON PR.Cod_Fabricante = FB.Codigo
				 WHERE ES.Cod_Estabe = 1
				AND CB.status NOT IN ('A', 'C')
				
				AND PR.Codigo = @CProd
				--AND pr.Cod_Fabricante in (158,319,123,321,588,338,33,237,164,1022)
				AND cb.Dat_Entrada >= '20230101'
				AND cb.Dat_Entrada <= '20231031'
				AND Tip_NF <> 'D'
				group by
					pr.Cod_Fabricante,
					FB.Fantasia,
					cod_ean,
					PR.Codigo,
					descricao,
					CB.dat_entrada,
					it.protocolo,
					Prc_UniFat, 
					ES.prc_venda,
					it.cod_lote,
					es.Prc_CusMed,
					Prc_CusLiqEnt
				
				order by Dat_Entrada desc
	OPEN Curv_ABC_ENT;
	FETCH NEXT FROM Curv_ABC_ENT INTO @CFabric, @Fabric, @CEan, @CProd, @Desc, @Dat, @PrcUnit, @QtdComp, @Prot, @PrcVend, @CusMed;
    WHILE @@FETCH_STATUS = 0
    BEGIN

		PRINT CAST(@CFabric AS NVARCHAR(255)) +';'+ CAST(@Fabric AS NVARCHAR(255)) +';'+ CAST(@CEan AS NVARCHAR(255)) +';'+ CAST(@CProd AS VARCHAR(255)) +';'+ CAST(@Desc AS NVARCHAR(255)) +';'+  CAST(@Dat AS NVARCHAR(255)) +';'+ 
		CAST(@PrcUnit AS NVARCHAR(255)) +';'+ CAST(@QtdComp AS NVARCHAR(255)) +';'+  CAST(@Prot AS NVARCHAR(255)) +';'+  CAST(@PrcVend AS NVARCHAR(255)) +';'+  CAST(@CusMed AS NVARCHAR(255))

	FETCH NEXT FROM Curv_ABC_ENT INTO  @CFabric, @Fabric, @CEan, @CProd, @Desc, @Dat, @PrcUnit, @QtdComp, @Prot, @PrcVend, @CusMed;

	END;
	CLOSE Curv_ABC_ENT
	DEALLOCATE Curv_ABC_ENT

FETCH NEXT FROM CursorProd INTO @CProd
END

CLOSE CursorProd
DEALLOCATE CursorProd
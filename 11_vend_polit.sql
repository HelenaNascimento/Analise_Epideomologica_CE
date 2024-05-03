
--Variáveis Globais:
declare 
	@ANO varchar(4) = '2024',
	@MES varchar(2) = '04',
	@FB int = 1022,
	@CEst int = 1,
	@OP int = 1


IF @OP = 1 (
-- Cabeçalho:
	SELECT 
	DISTINCT
		pr.cod_ean,
		pr.codigo,
		pr.descricao
	FROM NFSCB cb 
		INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																(cb.Ser_Nota = it.Ser_Nota) AND 
																(cb.Num_Nota = it.Num_Nota)) 
		INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
		left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
		left join FABRI FB on pr.Cod_Fabricante = fb.codigo
	WHERE cb.Cod_Estabe = @CEst
	AND pr.Cod_Fabricante = @FB
	AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
	AND year(cb.Dat_Emissao) = @ANO
	AND month(cb.Dat_Emissao) = @MES

);

IF @OP = 2 (
--Dados Prod X Poli:

			SELECT 

			DISTINCT
				pr.codigo,
				pr.cod_ean,
				pr.descricao,
				pc.Cod_PolCom,
				--Auxiliar = concat(pc.Cod_PolCom, '-', pr.codigo),
				QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
				VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
			FROM NFSCB cb 
				INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																		(cb.Ser_Nota = it.Ser_Nota) AND 
																		(cb.Num_Nota = it.Num_Nota)) 
				INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
				left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
				left join FABRI FB on pr.Cod_Fabricante = fb.codigo
			WHERE cb.Cod_Estabe = @CEst
			AND pr.Cod_Fabricante = @FB
			AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
			AND year(cb.Dat_Emissao) = @ANO
			AND month(cb.Dat_Emissao) = @MES
			Group by 
			pr.cod_ean,
			pr.codigo,
			pr.descricao,
			pc.Cod_PolCom
			);

IF @OP = 3 (
--Dados Poli x Qtd_Vlr:

			SELECT 
			DISTINCT
				pc.Cod_PolCom,
				QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
				VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
			FROM NFSCB cb 
				INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																		(cb.Ser_Nota = it.Ser_Nota) AND 
																		(cb.Num_Nota = it.Num_Nota)) 
				INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
				left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
				left join FABRI FB on pr.Cod_Fabricante = fb.codigo
			WHERE cb.Cod_Estabe = 1
			AND pr.Cod_Fabricante = @FB
			AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
			AND year(cb.Dat_Emissao) = @ANO
			AND month(cb.Dat_Emissao) = @MES
			Group by 
			pc.Cod_PolCom

			);

IF @OP = 4 (
--Dados Poli x Qtd_Vlr:

			SELECT 
			DISTINCT
				QtdVen = Sum(it.Qtd_Produto+it.Qtd_Bonificacao),    
				VlrBasDscVen = Sum(it.Vlr_LiqItem-it.Vlr_SubsTrib-it.Vlr_SbtRes-it.Vlr_RecSbt-it.Vlr_SubsTribEmb-it.Vlr_DespRateada-IsNull(it.Vlr_DspExt,0))
			FROM NFSCB cb 
				INNER JOIN NFSIT it ON ((cb.Cod_Estabe = it.Cod_Estabe) AND 
																		(cb.Ser_Nota = it.Ser_Nota) AND 
																		(cb.Num_Nota = it.Num_Nota)) 
				INNER JOIN PRODU pr on it.Cod_Produto = pr.Codigo 
				left join POCOM PC on it.Id_PolCom = pc.Id_PolCom
				left join FABRI FB on pr.Cod_Fabricante = fb.codigo
			WHERE cb.Cod_Estabe = 1
			AND pr.Cod_Fabricante = @FB
			AND (cb.Status = 'F' and cb.Tip_Saida = 'V') 
			AND year(cb.Dat_Emissao) = @ANO
			AND month(cb.Dat_Emissao) = @MES
			);

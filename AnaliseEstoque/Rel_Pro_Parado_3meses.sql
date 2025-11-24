select  
	DISTINCT
	PR.Codigo,
	Descri,
	Dat_UltVen,
	Qtd_Dispon,
	FB.Fantasia,
	lt.Cod_Lote,
	LT.Dat_Vencim AS VENC_LOTE
from PRODU PR
JOIN PRXES ES ON PR.Codigo = ES.Cod_Produt
JOIN FABRI FB ON PR.Cod_Fabricante = FB.CODIGO
JOIN PRLOT LT ON PR.Codigo = LT.Cod_Produt AND ES.Cod_Estabe = LT.Cod_Estabe AND LT.Qtd_Fisico > 0
WHERE 
ES.Cod_Estabe = 1
AND Dat_UltVen < GETDATE() - 90
AND Flg_Bloqueado = 0
AND Flag_ImprClassif1 <> 'N'
AND Tipo = '00'
AND Qtd_Dispon > 0
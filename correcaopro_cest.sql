select 
	IT.Cod_Produto,
	PR.Descricao,
	PR.Cod_EAN,
	LISTA = 
		CASE
			WHEN PR.Tip_LisPis = 'O' THEN 'Outra'
			WHEN PR.Tip_LisPis = 'X' THEN 'Neutro'
			WHEN PR.Tip_LisPis = 'P' THEN 'Positiva'
			WHEN PR.Tip_LisPis = 'N' THEN 'Negativa'

		end,
	Cod_Ncm,
	ret_xmotivo

from NFSCB CB
	INNER JOIN NFSIT IT ON CB.Cod_Estabe = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota
	LEFT JOIN PRODU PR ON IT.Cod_Produto = PR.Codigo
WHERE 
	CB.Cod_Estabe = 1 
AND CB.Dat_Emissao >= '20240507'
AND Ret_CStat <> 100
AND (IT.Cod_CEST = '' OR IT.Cod_CEST IS NULL)
order by 1
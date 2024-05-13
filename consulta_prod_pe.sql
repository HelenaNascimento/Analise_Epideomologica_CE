SELECT
    CB.Dat_Emissao,
    CB.Num_Nota,
    RAZAO_SOCIAL,
    CB.Cgc as CNPJ,
    DESCRI as Descricao_Produto,
    IT.Cod_Lote as Lote,
    convert(int, IT.Qtd_ImpFat) as Qtd_Venda
FROM
	NFSCB CB
		INNER JOIN NFSIT IT ON CB.COD_ESTABE = IT.Cod_Estabe AND CB.Ser_Nota = IT.Ser_Nota AND CB.Num_Nota = IT.Num_Nota 
		LEFT JOIN CLIEN CL ON cb.cod_cliente = cl.codigo and CB.Cgc = CL.Cgc_Cpf
		LEFT JOIN PRODU PR ON it.Cod_Produto = PR.codigo
WHERE
	cb.Cod_Estabe = 0 --Codigo do estabelecimento
and	cb.dat_emissao >= '20230801' -- Data início da consulta, formato YYYYMMDD
and it.Cod_Produto = 22500 -- Código do Produto
order by 1
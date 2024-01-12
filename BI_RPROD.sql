/*

select 
     Fabricante = FB.Fantasia,
    Qtd_Produto = count(IT.COD_PRODUTO)
    from
        PRODU PR 
            INNER JOIN NFSIT IT ON PR.CODIGO = IT.COD_PRODUTO
            INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
            Left join FABRI FB ON PR.Cod_Fabricante = FB.Codigo
where it.cod_estabe = 1
    and dat_emissao >= '20240101' 
    and dat_emissao <= '20240131'
    and it.cod_cfo in (5102, 5405)
    and Status = 'F'
    and Ret_CStat = 100

group by FB.Fantasia
order by 2 desc;
 
*/

declare
    @CProd INT,
    @CEstab INT = 1,
    @QVend INT,
    @QVAnd INT,
    @PDesc Varchar(80)

declare CursorProd cursor  for

SELECT
    TOP 1
    COD_PRODUTO,
    Descri,
    MAX(IT.qtd) AS QTD_VENDA
    FROM (select 
                Cod_Produto, 
                sum(Qtd_ImpFat) as Qtd, 
                Cod_Estabe, 
                ser_nota, 
                num_nota 
            FROM NFSIT 
            where cod_estabe = 1 
            and cod_cfo in (5102, 5405)
            group by 
                Cod_Produto,
                Cod_Estabe, 
                ser_nota, 
                num_nota  ) IT
        INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
        left JOIN PRODU PR ON IT.COD_PRODUTO = PR.CODIGO
        
WHERE IT.Cod_Estabe = @CEstab 
    and dat_emissao >= '20240101' 
    and dat_emissao <= '20240131' 
GROUP BY 
    COD_PRODUTO,
    Descri
ORDER BY 3 

OPEN CursorProd
FETCH NEXT FROM CursorProd INTO @CProd, @PDesc, @QVend
WHILE @@FETCH_STATUS = 0
    BEGIN
    DECLARE ANOANT CURSOR FOR
            SELECT
                TOP 1
                COD_PRODUTO,
                Descri,
                MAX(Qtd) AS QTD_VENDA
                    FROM (select 
                                Cod_Produto, 
                                sum(Qtd_ImpFat) as Qtd, 
                                Cod_Estabe, 
                                ser_nota, 
                                num_nota 
                            FROM NFSIT 
                            where cod_estabe = 1 
                            and cod_cfo in (5102, 5405)
                            and Cod_Produto = @CProd
                            group by 
                                Cod_Produto,
                                Cod_Estabe, 
                                ser_nota, 
                                num_nota  ) IT
                    INNER JOIN PRODU PR ON IT.COD_PRODUTO = PR.CODIGO
                    INNER JOIN NFSCB CB ON IT.Cod_Estabe = CB.Cod_Estabe AND IT.Ser_Nota = CB.Ser_Nota AND IT.Num_Nota = CB.Num_Nota
            WHERE IT.Cod_Estabe = @CEstab 
                and dat_emissao >= '20230101' 
                and dat_emissao <= '20230131'
                and it.Cod_Produto = @CProd
            GROUP BY 
                COD_PRODUTO,
                Descri
            ORDER BY 3 
OPEN ANOANT
FETCH NEXT FROM ANOANT INTO @CProd, @PDesc, @QVAnd
WHILE @@FETCH_STATUS = 0
    BEGIN

    SELECT @CProd, @PDesc, @QVend, @QVAnd
    	
        FETCH NEXT FROM ANOANT INTO @CProd, @PDesc, @QVAnd

    END;

CLOSE ANOANT
DEALLOCATE ANOANT
FETCH NEXT FROM CursorProd INTO @CProd, @PDesc, @QVend
END
CLOSE CursorProd
DEALLOCATE CursorProd
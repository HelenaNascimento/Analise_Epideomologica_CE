
/*
Relatório SANDOZ - Período Jan/2023 até Out/2023

/---Lucro---\
Código EAN 
Descrição
DANFE = (Valor da última entrada)
ICMS = (DANFE/100*13,71)
C.Fixo = (Markup / 100 * 7,0864)
C.Venda = (Markup/100 * 4)
Imp.Federal = (P.C.Resc / 100 * 3,25)
Investimento = (Markup/100*1)
C.Total = (Danfe + ICMS + Custo Fixo + Custo venda + ImpFed + Investimento)
Preco Venda = Prc Fábrica
Desconto = (PrcVenda - Markup) / PrcVenda
Markup = (Prc Danfe +( Prc. Danfe * 38,93%))
P.C.Resc (Markup /100* 107,8)
Lucro Liq = (P.C. Resc - C.Total) / P.C.RESC
Markup
P.C. Resc
V.C/C = Markup - Markup
Qtd Mes
Lucro Liq
Est Disponpivel

/---OL---\

Código EAN 
Descrição
id_polcom
Descrição
Quantidade Total de SAÍDA X OL
Venda Total X OL

/---Estoque---\

Código EAN 
Descrição
Mapeamento de Estoque, produto (Lote, vencimento, count - tempo depois a ultima compra...)

*/




select count(codigo) as Qtd_Prod_Sand 
from PRODU
where Cod_Fabricante = 164
    AND Flag_ImprClassif1 <> 'N'

SELECT 
	PC.Id_PolCom,
	COUNT(PR.CODIGO) AS QTD_PRODU
	FROM POCOM PC
		INNER JOIN PCXPR PPC ON PC.Id_PolCom = PPC.Id_PolCom
		INNER JOIN PRODU PR ON PPC.Cod_Produt = PR.Codigo
WHERE PC.Id_PolCom in (2662, 2673, 2854, 2884, 3015)
and pr.Cod_Fabricante = 164
and Flag_ImprClassif1 <> 'N'
group by PC.Id_PolCom


SELECT COUNT(PR.CODIGO)
	FROM PRODU PR
		INNER JOIN PRXES ES ON PR.CODIGO = ES.Cod_Produt
		INNER JOIN PCXPR PC ON PR.CODIGO = PC.Cod_Produt
		INNER JOIN PCXES PS ON PC.ID_POLCOM = PS.ID_POLCOM AND ES.COD_ESTABE = PS.COD_ESTABE
WHERE ES.Cod_Estabe = 1
AND Cod_Fabricante = 164
AND PC.Id_PolCom not in (2662, 2673, 2854, 2884, 3015)
AND Flag_ImprClassif1 <> 'N'


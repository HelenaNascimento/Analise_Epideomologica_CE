select 
cod_fabricante,
cod_ean,
codigo,
descricao,
Cod_PolCom,
sum(QtdVen) as QtdVen,
sum(VlrFatVen) as VlrFatVen,
sum(VlrBasDscVen) as VlrBasDscVen
from AnaSaiFabABC
where Cod_Fabricante = 319
AND dat_emissao >='20231101'
AND dat_emissao <='20231130'
group BY
	cod_fabricante,
	cod_ean,
	codigo,
	descricao,
	Cod_PolCom
order by 3


select 
DISTINCT
--cod_ean,
--codigo,
--descricao
Cod_PolCom
from AnaSaiFabABC
where Cod_Fabricante = 33
order by 1

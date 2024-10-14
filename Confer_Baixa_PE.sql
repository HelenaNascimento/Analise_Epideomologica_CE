select 
format(sum(vlr_principal), 'c', 'pt-br') as vlr_principal,
format(sum(vlr_desconto), 'c', 'pt-br') as vlr_desconto,
format(sum(vlr_deducoes), 'c', 'pt-br') as vlr_deducoes,
format(sum(vlr_juros), 'c', 'pt-br') as vlr_juros,
format(sum(vlr_acrescimos), 'c', 'pt-br') as vlr_acrescimos, 
format(sum(sld_principal), 'c', 'pt-br') as sld_principal,
format(sum(Vlr_JurCalc), 'c', 'pt-br') as Vlr_JurCalc
from BXREC
where Cod_Estabe = 4
and Cod_Rec = 141366

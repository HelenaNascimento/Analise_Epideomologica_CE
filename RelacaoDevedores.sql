select 
	distinct
	ct.Cod_Cliente,
	cl.Razao_Social,
	CT.Cod_Agente,
	CB.Descricao,
	format(sum (ct.Vlr_Documento), 'c', 'pt-br') as vlr_documento
	
from CTREC CT
	JOIN CLIEN CL ON CT.Cod_Cliente = CL.Codigo 
	JOIN ENXES ES ON ct.Cod_Estabe = ES.Cod_Estabe and CL.Codigo = ES.Cod_Client
	LEFT JOIN AGCOB CB ON ct.Cod_Agente = CB.Codigo
where CT.Cod_Estabe = 1
AND Dat_Vencimento < = '20240131'
AND Status = 'A'
group by 
	ct.Cod_Cliente,
	cl.Razao_Social,
	CT.Cod_Agente,
	CB.Descricao
order by 1
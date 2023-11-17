SELECT 
	distinct
	pc.Id_PolCom, 
	Cod_PolCom,
	pr.cod_fabricante,
	fb.fantasia,
	count(ppr.cod_produt) as Qtd_Produ
	FROM POCOM PC
		inner join PCXPR PPR ON pc.id_polcom = ppr.Id_PolCom 
		inner join PRODU PR ON ppr.cod_produt = pr.codigo
		left join FABRI FB ON pr.Cod_Fabricante = fb.codigo 
		inner join PCXFB PFB ON pc.id_polcom = PFB.id_polcom and fb.codigo = pfb.Cod_Fabric
WHERE Cod_PolCom LIKE 'CE%' 
AND (Dat_Termino >= GETDATE() + 2 
 or Dat_Termino is null)
AND pc.Bloqueado = 0
and pc.Id_PolCom = 2884
group by 
	pc.Id_PolCom, 
	Cod_PolCom,
	pr.cod_fabricante,
	fb.fantasia
	 
ORDER BY 2
select distinct 
	Cod_Rec,
	bx.Cod_Documento,
	ct.Num_Documento
	from BXREC bx
		left join (select Num_Documento , cod_documento from CTREC where cod_estabe = 0) ct on bx.Cod_Documento = ct.Cod_Documento
where Cod_Estabe = 0 and Cod_Rec in(134802, 134804, 134807, 134811, 134813, 134815)
-- Modificado para reprocessar todas as contas

Declare @PCodEstabe int,
        @PDatIni datetime,
		@PFlgAtuAll bit,
		@PFlgRec bit,
		@PFlgPag bit

Set DateFormat YMD
-- sugestão data inicial = 1 de janeiro do ano atual
Set @PDatIni = cast(Year(GetDate()) as varchar)+'-01-01 '

-----------------------------------------------------------------------------------------------------------
--Data no formato Ano-Mes-Dia
--Set @PDatIni = '2017-09-01 '

Set @PCodEstabe = 0    -->  -1: atualiza todos os estabelecimentos
Set @PFlgAtuAll = 0    -->   0: atualiza somente contas vazios,  1: atualiza se contas diferentes

Set @PFlgRec = 1       --> 1- atualiza contas a receber
Set	@PFlgPag = 1       --> 1- atualiza contas a pagar

-----------------------------------------------------------------------------------------------------------

-- Está usando as datas abaixo
--CTREC Dat_Emissao
--BXREC Dat_Registro

BEGIN TRANSACTION

SET NOCOUNT ON

-- desabilita triggers
ALTER TABLE PAGBX DISABLE TRIGGER ALL
ALTER TABLE BXREC DISABLE TRIGGER ALL
ALTER TABLE LANIT DISABLE TRIGGER ALL

--======================
-- CONTAS A PAGAR
--======================
if @PFlgPag = 1
  begin
    -- FAVORECIDO/principal
    Update b
      Set b.Cod_CtaDevPri = f.Cod_CtaDevPri,
          b.Cod_LocDevPri = f.Cod_LocDevPri,
          b.Cod_HisPri = f.Cod_HisPri,
          b.Des_CplHisPri = p.Num_Docume
    From PAGBX b 
         Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FAVOR f on (p.Cod_Favore = f.Cod_Favore)
    Where p.Cod_Favore > 0
    And IsNull(b.Val_Princi,0) > 0
    And IsNull(f.Cod_CtaDevPri,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevPri,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevPri,'') <> IsNull(f.Cod_CtaDevPri,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCrePri = f.Cod_CtaCrePri, 
          b.Cod_LocCrePri = f.Cod_LocCrePri, 
          b.Cod_HisPri = f.Cod_HisPri,
          b.Des_CplHisPri = p.Num_Docume
    From PAGBX b 
	     inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag)
         inner Join FAVOR f on p.Cod_Favore = f.Cod_Favore
    Where p.Cod_Favore > 0
    And IsNull(b.Val_Princi,0) > 0
    And IsNull(f.Cod_CtaCrePri,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCrePri,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCrePri,'') <> IsNull(f.Cod_CtaCrePri,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- FAVORECIDO/juros
    Update b
      Set b.Cod_CtaDevJur = f.Cod_CtaDevJur,
          b.Cod_LocDevJur = f.Cod_LocDevJur,
          b.Cod_HisJur = f.Cod_HisJur, 
          b.Des_CplHisJur = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FAVOR f on (p.Cod_Favore = f.Cod_Favore)
    Where p.Cod_Favore > 0
    And IsNull(b.Val_Juros,0)+IsNull(b.Val_OutAcr,0)+IsNull(b.Val_Multa,0) > 0
    And IsNull(f.Cod_CtaDevJur,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevJur,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevJur,'') <> IsNull(f.Cod_CtaDevJur,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCreJur = f.Cod_CtaCreJur,
          b.Cod_LocCreJur = f.Cod_LocCreJur,
          b.Cod_HisJur = f.Cod_HisJur, 
          b.Des_CplHisJur = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FAVOR f on (p.Cod_Favore = f.Cod_Favore)
    Where p.Cod_Favore > 0
    And IsNull(b.Val_Juros,0)+IsNull(b.Val_OutAcr,0)+IsNull(b.Val_Multa,0) > 0
    And IsNull(f.Cod_CtaCreJur,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCreJur,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCreJur,'') <> IsNull(f.Cod_CtaCreJur,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- FAVORECIDO/descontos
    Update b
      Set b.Cod_CtaDevDsc = f.Cod_CtaDevDsc,
          b.Cod_LocDevDsc = f.Cod_LocDevDsc,
          b.Cod_HisDsc = f.Cod_HisDsc, 
          b.Des_CplHisDsc = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FAVOR f on (p.Cod_Favore = f.Cod_Favore)
    Where p.Cod_Favore > 0
    And IsNull(b.Val_Descon,0)+IsNull(b.Val_OutDed,0) > 0
    And IsNull(f.Cod_CtaDevDsc,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevDsc,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevDsc,'') <> IsNull(f.Cod_CtaDevDsc,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCreDsc = f.Cod_CtaCreDsc,
          b.Cod_LocCreDsc = f.Cod_LocCreDsc,
          b.Cod_HisDsc = f.Cod_HisDsc, 
          b.Des_CplHisDsc = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FAVOR f on (p.Cod_Favore = f.Cod_Favore)
    Where p.Cod_Favore > 0
    And IsNull(b.Val_Descon,0)+IsNull(b.Val_OutDed,0) > 0
    And IsNull(f.Cod_CtaCreDsc,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCreDsc,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCreDsc,'') <> IsNull(f.Cod_CtaCreDsc,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- FORNECEDOR/principal
    Update b
      Set b.Cod_CtaDevPri = f.Cod_CtaDevPri,
          b.Cod_LocDevPri = f.Cod_LocDevPri,
          b.Cod_HisPri = f.Cod_HisPri,
          b.Des_CplHisPri = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FORNE f on (p.Cod_Fornec = f.Codigo)
    Where p.Cod_Fornec > 0
    And IsNull(b.Val_Princi,0) > 0
    And IsNull(f.Cod_CtaDevPri,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevPri,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevPri,'') <> IsNull(f.Cod_CtaDevPri,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCrePri = f.Cod_CtaCrePri, 
          b.Cod_LocCrePri = f.Cod_LocCrePri, 
          b.Cod_HisPri = f.Cod_HisPri,
          b.Des_CplHisPri = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and b.Cod_CtaPag = p.Cod_CtaPag 
         Inner Join FORNE f on p.Cod_Fornec = f.Codigo
    Where p.Cod_Fornec > 0
    And IsNull(b.Val_Princi,0) > 0
    And IsNull(f.Cod_CtaCrePri,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCrePri,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCrePri,'') <> IsNull(f.Cod_CtaCrePri,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- FORNECEDOR/juros
    Update b
      Set b.Cod_CtaDevJur = f.Cod_CtaDevJur,
          b.Cod_LocDevJur = f.Cod_LocDevJur,
          b.Cod_HisJur = f.Cod_HisJur, 
          b.Des_CplHisJur = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FORNE f on (p.Cod_Fornec = f.Codigo)
    Where p.Cod_Fornec > 0
    And IsNull(b.Val_Juros,0)+IsNull(b.Val_OutAcr,0)+IsNull(b.Val_Multa,0) > 0
    And IsNull(f.Cod_CtaDevJur,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevJur,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevJur,'') <> IsNull(f.Cod_CtaDevJur,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCreJur = f.Cod_CtaCreJur,
          b.Cod_LocCreJur = f.Cod_LocCreJur,
          b.Cod_HisJur = f.Cod_HisJur, 
          b.Des_CplHisJur = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FORNE f on (p.Cod_Fornec = f.Codigo)
    Where p.Cod_Fornec > 0
    And IsNull(b.Val_Juros,0)+IsNull(b.Val_OutAcr,0)+IsNull(b.Val_Multa,0) > 0
    And IsNull(f.Cod_CtaCreJur,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCreJur,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCreJur,'') <> IsNull(f.Cod_CtaCreJur,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- FORNECEDOR/descontos
    Update b
      Set b.Cod_CtaDevDsc = f.Cod_CtaDevDsc,
          b.Cod_LocDevDsc = f.Cod_LocDevDsc,
          b.Cod_HisDsc = f.Cod_HisDsc, 
          b.Des_CplHisDsc = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FORNE f on (p.Cod_Fornec = f.Codigo)
    Where p.Cod_Fornec > 0
    And IsNull(b.Val_Descon,0)+IsNull(b.Val_OutDed,0) > 0
    And IsNull(f.Cod_CtaDevDsc,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevDsc,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevDsc,'') <> IsNull(f.Cod_CtaDevDsc,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCreDsc = f.Cod_CtaCreDsc,
          b.Cod_LocCreDsc = f.Cod_LocCreDsc,
          b.Cod_HisDsc = f.Cod_HisDsc, 
          b.Des_CplHisDsc = p.Num_Docume
    From PAGBX b 
	     Inner Join PAGCT p on (b.Cod_Estabe = p.Cod_Estabe) and (b.Cod_CtaPag = p.Cod_CtaPag) 
         Inner Join FORNE f on (p.Cod_Fornec = f.Codigo)
    Where p.Cod_Fornec > 0
    And IsNull(b.Val_Descon,0)+IsNull(b.Val_OutDed,0) > 0
    And IsNull(f.Cod_CtaCreDsc,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCreDsc,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCreDsc,'') <> IsNull(f.Cod_CtaCreDsc,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- parametro/principal
    Update b
      Set b.Cod_CtaDevPri = pm.CodCtaDevPriPag,
          b.Cod_LocDevPri = pm.CodLocDevPriPag,
          b.Cod_HisPri = pm.CodHisPriPag,
          b.Des_CplHisPri = p.Num_Docume
    From PAGBX b, PAGCT p, PARFC pm
    Where (b.Cod_Estabe = p.Cod_Estabe) and b.Cod_CtaPag = p.Cod_CtaPag
    And IsNull(b.Val_Princi,0) > 0
    And IsNull(pm.CodCtaDevPriPag,'') <> ''
    And IsNull(b.Cod_CtaDevPri,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCrePri = pm.CodCtaCrePriPag,
          b.Cod_LocCrePri = pm.CodLocCrePriPag,
          b.Cod_HisPri = pm.CodHisPriPag,
          b.Des_CplHisPri = p.Num_Docume
    From PAGBX b, PAGCT p, PARFC pm
    Where (b.Cod_Estabe = p.Cod_Estabe) and b.Cod_CtaPag = p.Cod_CtaPag
    And IsNull(b.Val_Princi,0) > 0
    And IsNull(pm.CodCtaCrePriPag,'') <> ''
    And IsNull(b.Cod_CtaCrePri,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- parametro/juros
    Update b
      Set b.Cod_CtaDevJur = pm.CodCtaDevJurPag,
          b.Cod_LocDevJur = pm.CodLocDevJurPag,
          b.Cod_HisJur = pm.CodHisJurPag,
          b.Des_CplHisJur = p.Num_Docume
    From PAGBX b, PAGCT p, PARFC pm
    Where (b.Cod_Estabe = p.Cod_Estabe) and b.Cod_CtaPag = p.Cod_CtaPag
    And IsNull(b.Val_Juros,0)+IsNull(b.Val_OutAcr,0)+IsNull(b.Val_Multa,0) > 0
    And IsNull(pm.CodCtaDevJurPag,'') <> ''
    And IsNull(b.Cod_CtaDevJur,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCreJur = pm.CodCtaCreJurPag,
          b.Cod_LocCreJur = pm.CodLocCreJurPag,
          b.Cod_HisJur = pm.CodHisJurPag,
          b.Des_CplHisJur = p.Num_Docume
    From PAGBX b, PAGCT p, PARFC pm
    Where (b.Cod_Estabe = p.Cod_Estabe) and b.Cod_CtaPag = p.Cod_CtaPag
    And IsNull(b.Val_Juros,0)+IsNull(b.Val_OutAcr,0)+IsNull(b.Val_Multa,0) > 0
    And IsNull(pm.CodCtaCreJurPag,'') <> ''
    And IsNull(b.Cod_CtaCreJur,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

       
    -- parametro/descontos
    Update b
      Set b.Cod_CtaDevDsc = pm.CodCtaDevDscPag,
          b.Cod_LocDevDsc = pm.CodLocDevDscPag,
          b.Cod_HisDsc = pm.CodHisDscPag,
          b.Des_CplHisDsc = p.Num_Docume
    From PAGBX b, PAGCT p, PARFC pm
    Where (b.Cod_Estabe = p.Cod_Estabe) and b.Cod_CtaPag = p.Cod_CtaPag
    And IsNull(b.Val_Descon,0)+IsNull(b.Val_OutDed,0) > 0
    And IsNull(pm.CodCtaDevDscPag,'') <> ''
    And IsNull(b.Cod_CtaDevDsc,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update b
      Set b.Cod_CtaCreDsc = pm.CodCtaCreDscPag,
          b.Cod_LocCreDsc = pm.CodLocCreDscPag,
          b.Cod_HisDsc = pm.CodHisDscPag,
          b.Des_CplHisDsc = p.Num_Docume
    From PAGBX b, PAGCT p, PARFC pm
    Where (b.Cod_Estabe = p.Cod_Estabe) and b.Cod_CtaPag = p.Cod_CtaPag
    And IsNull(b.Val_Descon,0)+IsNull(b.Val_OutDed,0) > 0
    And IsNull(pm.CodCtaCreDscPag,'') <> ''
    And IsNull(b.Cod_CtaCreDsc,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- LANIT/Principal
    Update l
      Set l.Cod_CtaDevPri = b.Cod_CtaDevPri, 
          l.Cod_LocDevPri = b.Cod_LocDevPri,
          l.Cod_HisPri = b.Cod_HisPri, 
          l.Des_CplHisPri = b.Des_CplHisPri
    From LANIT l 
	     Inner Join PAGBX b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaPag = b.Cod_CtaPag) and (l.Cod_LanBxa = b.Cod_Lancam))
    Where IsNull(l.Val_Princi,0) > 0
    And IsNull(b.Cod_CtaDevPri,'') <> ''
    And IsNull(l.Cod_CtaDevPri,'') <> IsNull(b.Cod_CtaDevPri,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update l
      Set l.Cod_CtaCrePri = b.Cod_CtaCrePri, 
          l.Cod_LocCrePri = b.Cod_LocCrePri,
          l.Cod_HisPri = b.Cod_HisPri, 
          l.Des_CplHisPri = b.Des_CplHisPri
    From LANIT l 
	     Inner Join PAGBX b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaPag = b.Cod_CtaPag) and (l.Cod_LanBxa = b.Cod_Lancam))
    Where IsNull(l.Val_Princi,0) > 0
    And IsNull(b.Cod_CtaCrePri,'') <> ''
    And IsNull(l.Cod_CtaCrePri,'') <> IsNull(b.Cod_CtaCrePri,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- LANIT/juros
    Update l
      Set l.Cod_CtaDevJur = b.Cod_CtaDevJur, 
          l.Cod_LocDevJur = b.Cod_LocDevJur,
          l.Cod_HisJur = b.Cod_HisJur, 
          l.Des_CplHisJur = b.Des_CplHisJur
    From LANIT l 
	     Inner Join PAGBX b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaPag = b.Cod_CtaPag) and (l.Cod_LanBxa = b.Cod_Lancam))
    Where IsNull(l.Val_Juros,0)+IsNull(l.Val_OutAcr,0)+IsNull(l.Val_Multa,0) > 0
    And IsNull(b.Cod_CtaDevJur,'') <> ''
    And IsNull(l.Cod_CtaDevJur,'') <> IsNull(b.Cod_CtaDevJur,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update l
      Set l.Cod_CtaCreJur = b.Cod_CtaCreJur, 
          l.Cod_LocCreJur = b.Cod_LocCreJur,
          l.Cod_HisJur = b.Cod_HisJur, 
          l.Des_CplHisJur = b.Des_CplHisJur
    From LANIT l 
	     Inner Join PAGBX b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaPag = b.Cod_CtaPag) and (l.Cod_LanBxa = b.Cod_Lancam))
    Where IsNull(l.Val_Juros,0)+IsNull(l.Val_OutAcr,0)+IsNull(l.Val_Multa,0) > 0
    And IsNull(b.Cod_CtaCreJur,'') <> ''
    And IsNull(l.Cod_CtaCreJur,'') <> IsNull(b.Cod_CtaCreJur,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni


    -- LANIT/Descontos
    Update l
      Set l.Cod_CtaDevDsc = b.Cod_CtaDevDsc, 
          l.Cod_LocDevDsc = b.Cod_LocDevDsc,
          l.Cod_HisDsc = b.Cod_HisDsc, 
          l.Des_CplHisDsc = b.Des_CplHisDsc
    From LANIT l 
	     Inner Join PAGBX b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaPag = b.Cod_CtaPag) and (l.Cod_LanBxa = b.Cod_Lancam))
    Where IsNull(l.Val_Descon,0)+IsNull(l.Val_OutDed,0) > 0
    And IsNull(b.Cod_CtaDevDsc,'') <> ''
    And IsNull(l.Cod_CtaDevDsc,'') <> IsNull(b.Cod_CtaDevDsc,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

    Update l
      Set l.Cod_CtaCreDsc = b.Cod_CtaCreDsc, 
          l.Cod_LocCreDsc = b.Cod_LocCreDsc,
          l.Cod_HisDsc = b.Cod_HisDsc, 
          l.Des_CplHisDsc = b.Des_CplHisDsc
    From LANIT l 
	     Inner Join PAGBX b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaPag = b.Cod_CtaPag) and (l.Cod_LanBxa = b.Cod_Lancam))
    Where IsNull(l.Val_Descon,0)+IsNull(l.Val_OutDed,0) > 0
    And IsNull(b.Cod_CtaCreDsc,'') <> ''
    And IsNull(l.Cod_CtaCreDsc,'') <> IsNull(b.Cod_CtaCreDsc,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Lancam >= @PDatIni

  end -- if @PFlgPag = 1

--========================================================================================================================

--======================
-- CONTAS A RECEBER
--======================
if @PFlgRec = 1
  begin
    -- AG.Cobrador/principal
    Update b
      Set b.Cod_CtaDevPri = a.Cod_CtaDevPri,
          b.Cod_LocDevPri = Case
                              When IsNull(t.Cod_LocBxaCtb,'') <> '' then t.Cod_LocBxaCtb 
                              Else a.Cod_LocDevPri 
                            End,
          b.Cod_HisPri = a.Cod_HisPri,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b 
         Inner Join CTREC c on (b.Cod_Estabe = c.Cod_Estabe) and (b.Cod_Documento = c.Cod_Documento) 
         Inner Join AGCOB a on (c.Cod_Agente = a.Codigo)
         Left Outer Join TPDOC t on (c.Tip_Documento = t.Cod_Docume)
    Where c.Cod_Agente > 0
    And IsNull(b.Vlr_Principal,0) > 0
    And IsNull(a.Cod_CtaDevPri,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevPri,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevPri,'') <> IsNull(a.Cod_CtaDevPri,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update b
      Set b.Cod_CtaCrePri = a.Cod_CtaCrePri, 
          b.Cod_LocCrePri = Case
                              When IsNull(t.Cod_LocBxaCtb,'') <> '' then t.Cod_LocBxaCtb 
                              Else a.Cod_LocCrePri 
                            End,
          b.Cod_HisPri = a.Cod_HisPri,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b 
         Inner Join CTREC c on (b.Cod_Estabe = c.Cod_Estabe) and (b.Cod_Documento = c.Cod_Documento) 
         Inner Join AGCOB a on (c.Cod_Agente = a.Codigo)
         Left Outer Join TPDOC t on (c.Tip_Documento = t.Cod_Docume)
    Where c.Cod_Agente > 0
    And IsNull(b.Vlr_Principal,0) > 0
    And IsNull(a.Cod_CtaCrePri,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCrePri,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCrePri,'') <> IsNull(a.Cod_CtaCrePri,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni


    -- AG.Cobrador/juros
    Update b
      Set b.Cod_CtaDevJur = a.Cod_CtaDevJur,
          b.Cod_LocDevJur = Case
                              When IsNull(t.Cod_LocBxaCtb,'') <> '' then t.Cod_LocBxaCtb 
                              Else a.Cod_LocDevJur 
                            End,
          b.Cod_HisJur = a.Cod_HisJur, 
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b 
         Inner Join CTREC c on (b.Cod_Estabe = c.Cod_Estabe) and (b.Cod_Documento = c.Cod_Documento) 
         Inner Join AGCOB a on (c.Cod_Agente = a.Codigo)
         Left Outer Join TPDOC t on (c.Tip_Documento = t.Cod_Docume)
    Where c.Cod_Agente > 0
    And IsNull(b.Vlr_Juros,0)+IsNull(b.Vlr_Acrescimos,0)+IsNull(b.Vlr_Multa,0) > 0
    And IsNull(a.Cod_CtaDevJur,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevJur,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevJur,'') <> IsNull(a.Cod_CtaDevJur,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update b
      Set b.Cod_CtaCreJur = a.Cod_CtaCreJur,
          b.Cod_LocCreJur = Case
                              When IsNull(t.Cod_LocBxaCtb,'') <> '' then t.Cod_LocBxaCtb 
                              Else a.Cod_LocCreJur 
                            End,
          b.Cod_HisJur = a.Cod_HisJur, 
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b 
         Inner Join CTREC c on (b.Cod_Estabe = c.Cod_Estabe) and (b.Cod_Documento = c.Cod_Documento) 
         Inner Join AGCOB a on (c.Cod_Agente = a.Codigo)
         Left Outer Join TPDOC t on (c.Tip_Documento = t.Cod_Docume)
    Where c.Cod_Agente > 0
    And IsNull(b.Vlr_Juros,0)+IsNull(b.Vlr_Acrescimos,0)+IsNull(b.Vlr_Multa,0) > 0
    And IsNull(a.Cod_CtaCreJur,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCreJur,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCreJur,'') <> IsNull(a.Cod_CtaCreJur,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni


    -- AG.Cobrador/descontos
    Update b
      Set b.Cod_CtaDevDsc = a.Cod_CtaDevDsc,
          b.Cod_LocDevDsc = Case
                              When IsNull(t.Cod_LocBxaCtb,'') <> '' then t.Cod_LocBxaCtb 
                              Else a.Cod_LocDevDsc 
                            End,
          b.Cod_HisDsc = a.Cod_HisDsc, 
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b 
         Inner Join CTREC c on (b.Cod_Estabe = c.Cod_Estabe) and (b.Cod_Documento = c.Cod_Documento) 
         Inner Join AGCOB a on (c.Cod_Agente = a.Codigo)
         Left Outer Join TPDOC t on (c.Tip_Documento = t.Cod_Docume)
    Where c.Cod_Agente > 0
    And IsNull(b.Vlr_Desconto,0)+IsNull(b.Vlr_Deducoes,0) > 0
    And IsNull(a.Cod_CtaDevDsc,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaDevDsc,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaDevDsc,'') <> IsNull(a.Cod_CtaDevDsc,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update b
      Set b.Cod_CtaCreDsc = a.Cod_CtaCreDsc,
          b.Cod_LocCreDsc = Case
                              When IsNull(t.Cod_LocBxaCtb,'') <> '' then t.Cod_LocBxaCtb 
                              Else a.Cod_LocCreDsc 
                            End,
          b.Cod_HisDsc = a.Cod_HisDsc, 
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b 
         Inner Join CTREC c on (b.Cod_Estabe = c.Cod_Estabe) and (b.Cod_Documento = c.Cod_Documento) 
         Inner Join AGCOB a on (c.Cod_Agente = a.Codigo)
         Left Outer Join TPDOC t on (c.Tip_Documento = t.Cod_Docume)
    Where c.Cod_Agente > 0
    And IsNull(b.Vlr_Desconto,0)+IsNull(b.Vlr_Deducoes,0) > 0
    And IsNull(a.Cod_CtaCreDsc,'') <> ''
    And (((@PFlgAtuAll = 0) and (IsNull(b.Cod_CtaCreDsc,'') = '')) or 
         ((@PFlgAtuAll = 1) and (IsNull(b.Cod_CtaCreDsc,'') <> IsNull(a.Cod_CtaCreDsc,''))))
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

-------------------------------------------------------------------------------------------------------------------------

    -- parametro/principal
    Update b
      Set b.Cod_CtaDevPri = pm.CodCtaCrePriRec,
          b.Cod_LocDevPri = pm.CodLocDevPriRec,
          b.Cod_HisPri = pm.CodHisPriRec,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b, CTREC c, PARFC pm
    Where (b.Cod_Estabe = c.Cod_Estabe) and b.Cod_Documento = c.Cod_Documento
    And IsNull(b.Vlr_Principal,0) > 0
    And IsNull(pm.CodCtaDevPriRec,'') <> ''
    And IsNull(b.Cod_CtaDevPri,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update b
      Set b.Cod_CtaCrePri = pm.CodCtaCrePriRec,
          b.Cod_LocCrePri = pm.CodLocCrePriRec,
          b.Cod_HisPri = pm.CodHisPriRec,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b, CTREC c, PARFC pm
    Where (b.Cod_Estabe = c.Cod_Estabe) and b.Cod_Documento = c.Cod_Documento
    And IsNull(b.Vlr_Principal,0) > 0
    And IsNull(pm.CodCtaCrePriRec,'') <> ''
    And IsNull(b.Cod_CtaCrePri,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni
       
    -- parametro/juros
    Update b
      Set b.Cod_CtaDevJur = pm.CodCtaDevJurRec,
          b.Cod_LocDevJur = pm.CodLocDevJurRec,
          b.Cod_HisJur = pm.CodHisJurRec,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b, CTREC c, PARFC pm
    Where (b.Cod_Estabe = c.Cod_Estabe) and b.Cod_Documento = c.Cod_Documento
    And IsNull(b.Vlr_Juros,0)+IsNull(b.Vlr_Acrescimos,0)+IsNull(b.Vlr_Multa,0) > 0
    And IsNull(pm.CodCtaDevJurRec,'') <> ''
    And IsNull(b.Cod_CtaDevJur,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update b
      Set b.Cod_CtaCreJur = pm.CodCtaCreJurRec,
          b.Cod_LocCreJur = pm.CodLocCreJurRec,
          b.Cod_HisJur = pm.CodHisJurRec,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b, CTREC c, PARFC pm
    Where (b.Cod_Estabe = c.Cod_Estabe) and b.Cod_Documento = c.Cod_Documento
    And IsNull(b.Vlr_Juros,0)+IsNull(b.Vlr_Acrescimos,0)+IsNull(b.Vlr_Multa,0) > 0
    And IsNull(pm.CodCtaCreJurRec,'') <> ''
    And IsNull(b.Cod_CtaCreJur,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

       
    -- parametro/descontos
    Update b
      Set b.Cod_CtaDevDsc = pm.CodCtaDevDscRec,
          b.Cod_LocDevDsc = pm.CodLocDevDscRec,
          b.Cod_HisDsc = pm.CodHisDscRec,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b, CTREC c, PARFC pm
    Where (b.Cod_Estabe = c.Cod_Estabe) and b.Cod_Documento = c.Cod_Documento
    And IsNull(b.Vlr_Desconto,0)+IsNull(b.Vlr_Deducoes,0) > 0
    And IsNull(pm.CodCtaDevDscRec,'') <> ''
    And IsNull(b.Cod_CtaDevDsc,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update b
      Set b.Cod_CtaCreDsc = pm.CodCtaCreDscRec,
          b.Cod_LocCreDsc = pm.CodLocCreDscRec,
          b.Cod_HisDsc = pm.CodHisDscRec,
          b.Des_CplHisPri = c.Num_Documento+c.Par_Documento
    From BXREC b, CTREC c, PARFC pm
    Where (b.Cod_Estabe = c.Cod_Estabe) and b.Cod_Documento = c.Cod_Documento
    And IsNull(b.Vlr_Desconto,0)+IsNull(b.Vlr_Deducoes,0) > 0
    And IsNull(pm.CodCtaCreDscRec,'') <> ''
    And IsNull(b.Cod_CtaCreDsc,'') = ''
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

-------------------------------------------------------------------------------------------------------------------------

   -- LANIT/Principal
    Update l
      Set l.Cod_CtaDevPri = b.Cod_CtaDevPri, 
          l.Cod_LocDevPri = b.Cod_LocDevPri,
          l.Cod_HisPri = b.Cod_HisPri, 
          l.Des_CplHisPri = b.Des_CplHisPri
    From LANIT l 
         Inner Join BXREC b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaRec = b.Cod_Documento) and (l.Cod_LanBxa = b.Cod_Lancamento))
    Where IsNull(l.Val_Princi,0) > 0
    And IsNull(b.Cod_CtaDevPri,'') <> ''
    And IsNull(l.Cod_CtaDevPri,'') <> IsNull(b.Cod_CtaDevPri,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update l
      Set l.Cod_CtaCrePri = b.Cod_CtaCrePri, 
          l.Cod_LocCrePri = b.Cod_LocCrePri,
          l.Cod_HisPri = b.Cod_HisPri, 
          l.Des_CplHisPri = b.Des_CplHisPri
    From LANIT l 
         Inner Join BXREC b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaRec = b.Cod_Documento) and (l.Cod_LanBxa = b.Cod_Lancamento))
    Where IsNull(l.Val_Princi,0) > 0
    And IsNull(b.Cod_CtaCrePri,'') <> ''
    And IsNull(l.Cod_CtaCrePri,'') <> IsNull(b.Cod_CtaCrePri,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    -- LANIT/juros
    Update l
      Set l.Cod_CtaDevJur = b.Cod_CtaDevJur, 
          l.Cod_LocDevJur = b.Cod_LocDevJur,
          l.Cod_HisJur = b.Cod_HisJur, 
          l.Des_CplHisJur = b.Des_CplHisJur
    From LANIT l 
	     Inner Join BXREC b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaRec = b.Cod_Documento) and (l.Cod_LanBxa = b.Cod_Lancamento))
    Where IsNull(l.Val_Juros,0)+IsNull(l.Val_OutAcr,0)+IsNull(l.Val_Multa,0) > 0
    And IsNull(b.Cod_CtaDevJur,'') <> ''
    And IsNull(l.Cod_CtaDevJur,'') <> IsNull(b.Cod_CtaDevJur,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update l
      Set l.Cod_CtaCreJur = b.Cod_CtaCreJur, 
          l.Cod_LocCreJur = b.Cod_LocCreJur,
          l.Cod_HisJur = b.Cod_HisJur, 
          l.Des_CplHisJur = b.Des_CplHisJur
    From LANIT l 
         Inner Join BXREC b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaRec = b.Cod_Documento) and (l.Cod_LanBxa = b.Cod_Lancamento))
    Where IsNull(l.Val_Juros,0)+IsNull(l.Val_OutAcr,0)+IsNull(l.Val_Multa,0) > 0
    And IsNull(b.Cod_CtaCreJur,'') <> ''
    And IsNull(l.Cod_CtaCreJur,'') <> IsNull(b.Cod_CtaCreJur,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    -- LANIT/Descontos
    Update l
      Set l.Cod_CtaDevDsc = b.Cod_CtaDevDsc, 
          l.Cod_LocDevDsc = b.Cod_LocDevDsc,
          l.Cod_HisDsc = b.Cod_HisDsc, 
          l.Des_CplHisDsc = b.Des_CplHisDsc
    From LANIT l 
         Inner Join BXREC b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaRec = b.Cod_Documento) and (l.Cod_LanBxa = b.Cod_Lancamento))
    Where IsNull(l.Val_Descon,0)+IsNull(l.Val_OutDed,0) > 0
    And IsNull(b.Cod_CtaDevDsc,'') <> ''
    And IsNull(l.Cod_CtaDevDsc,'') <> IsNull(b.Cod_CtaDevDsc,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

    Update l
      Set l.Cod_CtaCreDsc = b.Cod_CtaCreDsc, 
          l.Cod_LocCreDsc = b.Cod_LocCreDsc,
          l.Cod_HisDsc = b.Cod_HisDsc, 
          l.Des_CplHisDsc = b.Des_CplHisDsc
    From LANIT l 
         Inner Join BXREC b on ((b.Cod_Estabe = l.Cod_Estabe) and (l.Cod_CtaRec = b.Cod_Documento) and (l.Cod_LanBxa = b.Cod_Lancamento))
    Where IsNull(l.Val_Descon,0)+IsNull(l.Val_OutDed,0) > 0
    And IsNull(b.Cod_CtaCreDsc,'') <> ''
    And IsNull(l.Cod_CtaCreDsc,'') <> IsNull(b.Cod_CtaCreDsc,'')
    And ((@PCodEstabe = -1) or (b.Cod_Estabe = @PCodEstabe))
    And b.Dat_Registro >= @PDatIni

-----------------------------------------------------------------------------------------------------------------------------------
    -- ajusta baixas por depositos
    if Exists(Select CodCtaCreDepRec From PARFC Where IsNull(CodCtaCreDepRec,'') <> '')
      begin
        Declare @CodCtaCreDepRec varchar(15),
	            @CodLocCreDepRec varchar(03)

    	Set @CodCtaCreDepRec = (Select CodCtaCreDepRec From PARFC Where IsNull(CodCtaCreDepRec,'') <> '')
	    Set @CodLocCreDepRec = (Select CodLocCreDepRec From PARFC Where IsNull(CodCtaCreDepRec,'') <> '')

        -- ajusta conta devedora em LANCB dos depositos
	    Update la
          Set la.Cod_PlcCtbDev = cp.Cod_PlcCtb,
	          la.Cod_LocCtbDev = cp.Cod_LocCtb
            From LANCB la
                 Inner Join CTPAR cp on la.Cod_CtaPar = cp.Cod_CtaPar
          Where la.Num_Docume like 'DEPÓSIT%'
          And la.Tip_Lancam = 'RT'
          And ((IsNull(la.Cod_PlcCtbDev,'') <> IsNull(cp.Cod_PlcCtb,'')) or (IsNull(la.Cod_LocCtbDev,'') <> IsNull(cp.Cod_LocCtb,'')))
          And ((@PCodEstabe = -1) or (la.Cod_Estabe = @PCodEstabe))
          And la.Dat_Lancam >= @PDatIni

        -- ajusta conta credora em LANCB dos depositos
	    Update la
          Set la.Cod_PlcCtbCre = @CodCtaCreDepRec,
	          la.Cod_LocCtbCre = @CodLocCreDepRec
            From LANCB la
          Where la.Num_Docume like 'DEPÓSIT%'
          And la.Tip_Lancam = 'RT'
	      And ((IsNull(la.Cod_PlcCtbCre,'') = '') or (IsNull(la.Cod_LocCtbCre,'') = ''))
          And ((@PCodEstabe = -1) or (la.Cod_Estabe = @PCodEstabe))
          And la.Dat_Lancam >= @PDatIni

        -- ajusta conta credora de depósitos em LANCB
	    Update cb
          Set cb.Cod_PlcCtbCre = '',
              cb.Cod_LocCtbCre = '',
              cb.Cod_LocCtbDev = ''
          From LANCB cb
               Inner JOIN DPXLT dp on dp.Cod_Estabe = cb.Cod_Estabe and cb.Cod_Lancam = dp.Cod_Lancam
          Where IsNull(cb.Cod_PlcCtbDev,'') = ''
	      And IsNull(cb.Cod_PlcCtbCre,'') <> ''
          And ((@PCodEstabe = -1) or (cb.Cod_Estabe = @PCodEstabe))
          And cb.Dat_Lancam >= @PDatIni


    	Update cb
          Set cb.Cod_PlcCtbCre = @CodCtaCreDepRec,
              cb.Cod_LocCtbCre = Case 
		                           When @CodLocCreDepRec <> '' then @CodLocCreDepRec
				    			   Else cb.Cod_LocCtbCre
					    		end
          From LANCB cb
               Inner JOIN DPXLT dp on dp.Cod_Estabe = cb.Cod_Estabe and cb.Cod_Lancam = dp.Cod_Lancam
          Where IsNull(cb.Cod_PlcCtbDev,'') <> ''
    	  And IsNull(cb.Cod_PlcCtbCre,'') <> @CodCtaCreDepRec
          And ((@PCodEstabe = -1) or (cb.Cod_Estabe = @PCodEstabe))
          And cb.Dat_Lancam >= @PDatIni

        -- ajustas contas devedoras de deposito em BXREC
	    Update bx
          Set bx.Cod_CtaDevPri = la.Cod_PlcCtbCre,
              bx.Cod_LocDevPri = la.Cod_LocCtbCre
        From BXREC bx
             Inner Join LANCB la on bx.Cod_Estabe = la.Cod_Estabe and bx.Cod_LanDep = la.Cod_Lancam
        Where bx.Tip_Doc = 'D'
        And IsNull(la.Cod_PlcCtbCre,'') <> ''
        And IsNull(la.Cod_PlcCtbDev,'') <> ''
	    And IsNull(bx.Cod_CtaDevPri,'') <> IsNull(la.Cod_PlcCtbCre,'')
        And ((@PCodEstabe = -1) or (la.Cod_Estabe = @PCodEstabe))
        And la.Dat_Lancam >= @PDatIni

        Update bx
          Set bx.Cod_CtaDevJur = la.Cod_PlcCtbCre
        From BXREC bx
            Inner Join LANCB la on bx.Cod_Estabe = la.Cod_Estabe and bx.Cod_LanDep = la.Cod_Lancam
        Where bx.Tip_Doc = 'D'
        and IsNull(la.Cod_PlcCtbCre,'') <> ''
        and IsNull(bx.Cod_CtaDevJur,'') <> IsNull(la.Cod_PlcCtbCre,'')
        and (Isnull(bx.Vlr_Multa,0)+Isnull(bx.Vlr_Juros,0)+Isnull(bx.Vlr_Acrescimos,0)) > 0
        And ((@PCodEstabe = -1) or (la.Cod_Estabe = @PCodEstabe))
        And la.Dat_Lancam >= @PDatIni


        -- ajustas contas credoras de deposito em BXREC
        Update bx
          Set bx.Cod_CtaCreDsc = la.Cod_PlcCtbCre
        From BXREC bx
            Inner Join LANCB la on bx.Cod_Estabe = la.Cod_Estabe and bx.Cod_LanDep = la.Cod_Lancam
        Where bx.Tip_Doc = 'D'
        and IsNull(la.Cod_PlcCtbCre,'') <> ''
        and IsNull(bx.Cod_CtaCreDsc,'') <> IsNull(la.Cod_PlcCtbCre,'')
        and (IsNull(bx.Vlr_Desconto,0)+IsNull(bx.Vlr_Deducoes,0)) > 0
        And ((@PCodEstabe = -1) or (la.Cod_Estabe = @PCodEstabe))
        And la.Dat_Lancam >= @PDatIni

        Update bx
          Set bx.Cod_CtaCreDscDev = la.Cod_PlcCtbCre
        From BXREC bx
            Inner Join LANCB la on bx.Cod_Estabe = la.Cod_Estabe and bx.Cod_LanDep = la.Cod_Lancam
        Where bx.Tip_Doc = 'D'
        and IsNull(la.Cod_PlcCtbCre,'') <> ''
        and IsNull(bx.Cod_CtaCreDscDev,'') <> IsNull(la.Cod_PlcCtbCre,'')
        and IsNull(bx.Vlr_DscDev,0) > 0
        And ((@PCodEstabe = -1) or (la.Cod_Estabe = @PCodEstabe))
        And la.Dat_Lancam >= @PDatIni

      end
-----------------------------------------------------------------------------------------------------------------------------------

  end  -- if @PFlgRec = 1

-- habilita triggers
ALTER TABLE PAGBX ENABLE TRIGGER ALL
ALTER TABLE BXREC ENABLE TRIGGER ALL
ALTER TABLE LANIT ENABLE TRIGGER ALL

COMMIT TRANSACTION
GO


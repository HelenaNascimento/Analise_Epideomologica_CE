DECLARE
  @Cest int = 4,
  @CDoc int,
  @codlanc int,
  @Status varchar(1),
  @D_Lan datetime,
  @D_Reg datetime,
  @D_Cai datetime,
  @Tip_Ba varchar(1),
  @Tip_Doc varchar(1),
  @Qtd_DAtr int,
  @Vlr_Prin float,
  @Vlr_Desc float,
  @Vlr_Deduc float,
  @Vlr_Jur float,
  @Vlr_Acr float,
  @Cod_Rec int = 137494,
  @Usuar varchar(35),
  @Transac datetime,
  @Cod_CtrOr varchar(1),
  @Isn_Cta int,
  @Cod_CntC int,
  @Vlr_DscD float

Declare C_Lanc CURSOR FOR

	select max(cod_lancamento) + 1 from BXREC where Cod_Estabe = @Cest and Cod_Rec = @Cod_Rec

OPEN C_Lanc

FETCH NEXT FROM C_Lanc INTO @codlanc
WHILE @@FETCH_STATUS = 0
BEGIN
	DECLARE SELC_REC CURSOR FOR
	select 
		--TOP 1
		Cod_Estabe, 
		Cod_Documento, 
		Cod_Lancamento =  @codlanc,
		Status = 'Q',
		Dat_Lancamento = Dat_Vencimento, 
		Dat_Registro = getdate(), 
		Dat_Caixa = getdate() , 
		Tip_Baixa = 'P', 
		Tip_Doc = 'E', 
		Qtd_DiasAtraso = 0 , 
		Vlr_Principal = Vlr_Documento, 
		Vlr_Desconto = Vlr_DescConced, 
		Vlr_Deducoes = 0, 
		Vlr_Juros = 0 , 
		Vlr_Acrescimos = 0, 
		Cod_Rec = @Cod_Rec, 
		Usuario = 'SILVANIA', 
		Transacao = getdate(), 
		Cod_CtrOri = 0, 
		Isn_CtaFin, 
		Cod_CntCus,
		Vlr_DscDev = 0 
	from CTREC
	where Cod_Estabe = @Cest
	and Status <> 'Q'
	and Par_Documento = 'A'
	and Num_Documento in ('057788',
'055780',
'056008',
'057281',
'057509',
'057552',
'057776',
'057953',
'057954',
'058630',
'060569',
'058725',
'060192',
'057955',
'058950',
'059597',
'060379',
'059968',
'057272',
'057974',
'058275',
'058571',
'060662',
'058398',
'060177',
'060926',
'058404',
'058550',
'053480',
'056439',
'057846',
'059508',
'060482',
'060277',
'058475',
'059221',
'059222',
'060945',
'059212',
'059223',
'060944',
'060997',
'059823',
'058335',
'059755',
'058477',
'057808',
'060008',
'061012',
'057735',
'056521',
'058276',
'057310',
'057551',
'057801',
'058538',
'058544',
'057957',
'057968',
'058277',
'059410',
'059938',
'061090',
'062122',
'057547',
'057973',
'058429',
'058120',
'058640',
'059969',
'059695',
'059592',
'057975',
'057607',
'058295',
'059885',
'061127',
'061467',
'060666',
'062015',
'059661',
'060079',
'060809',
'055804',
'057502',
'057997',
'055666',
'057406',
'058284',
'058851',
'058905',
'059023',
'060552',
'058105',
'059758',
'059225',
'059506',
'060990',
'059591',
'059594',
'060825',
'058799',
'059596',
'057730',
'059944')

OPEN SELC_REC;
FETCH NEXT FROM SELC_REC INTO  @Cest, @CDoc, @codlanc, @Status, @D_Lan, @D_Reg, @D_Cai, @Tip_Ba, @Tip_Doc, @Qtd_DAtr, @Vlr_Prin , @Vlr_Desc, @Vlr_Deduc, @Vlr_Jur, @Vlr_Acr, @Cod_Rec , @Usuar , @Transac, @Cod_CtrOr, @Isn_Cta, @Cod_CntC ,@Vlr_DscD;
WHILE @@FETCH_STATUS = 0
BEGIN 

	insert into BXREC
		  ( Cod_Estabe, 
			Cod_Documento, 
			Cod_Lancamento, 
			Status, 
			Dat_Lancamento, 
			Dat_Registro, 
			Dat_Caixa, 
			Tip_Baixa, 
			Tip_Doc, 
			Qtd_DiasAtraso, 
			Vlr_Principal, 
			Vlr_Desconto, 
			Vlr_Deducoes, 
			Vlr_Juros, 
			Vlr_Acrescimos, 
			Cod_Rec, Usuario, 
			Transacao, 
			Cod_CtrOri, 
			Isn_CtaFin, 
			Cod_CntCus, 
			Vlr_DscDev)
	values
	(@Cest, @CDoc, @codlanc, @Status, @D_Lan, @D_Reg, @D_Cai, @Tip_Ba, @Tip_Doc, @Qtd_DAtr, @Vlr_Prin , @Vlr_Desc, @Vlr_Deduc, @Vlr_Jur, @Vlr_Acr, @Cod_Rec , @Usuar , @Transac, @Cod_CtrOr, @Isn_Cta, @Cod_CntC ,@Vlr_DscD)

FETCH NEXT FROM SELC_REC INTO @Cest, @CDoc, @codlanc, @Status, @D_Lan, @D_Reg, @D_Cai, @Tip_Ba, @Tip_Doc, @Qtd_DAtr, @Vlr_Prin , @Vlr_Desc, @Vlr_Deduc, @Vlr_Jur, @Vlr_Acr, @Cod_Rec , @Usuar , @Transac, @Cod_CtrOr, @Isn_Cta, @Cod_CntC ,@Vlr_DscD

END;
CLOSE SELC_REC
DEALLOCATE SELC_REC

FETCH NEXT FROM C_Lanc INTO @codlanc
END;

CLOSE C_Lanc
DEALLOCATE C_Lanc
DECLARE
  @Cest int = 0,
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
  @Cod_Rec int = 134815,
  @Usuar varchar(35),
  @Dt_MV smalldatetime = '20230630',
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
		Cod_Estabe, 
		Cod_Documento, 
		Cod_Lancamento =  @codlanc,
		Status = 'Q',
		Dat_Lancamento = Dat_Vencimento, 
		Dat_Registro = @Dt_MV, 
		Dat_Caixa = @Dt_MV , 
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
	where Cod_Estabe = 0 
	and Status <> 'Q'
	and Par_Documento = 'A'
	and Cod_Agente = 700
	and Num_Documento in ('1233851'
,'1234532'
,'1234736'
,'1234798'
,'1234832'
,'1234877'
,'1235112'
,'1235682'
,'1235738'
,'1235794'
,'1235822'
,'1235856'
,'1235864'
,'1236458'
,'1236762'
,'1236772'
,'1236787'
,'1237743'
,'1237745'
,'1237791'
,'1237793'
,'1237846'
,'1237847'
,'1237849'
,'1237887'
,'1238767'
,'1238837'
,'1238855'
,'1238870'
,'1238882'
,'1238914'
,'1238947'
,'1239341'
,'1239499'
,'1239500'
,'1239501'
,'1239505'
,'1239511'
,'1239512'
,'1239534'
,'1239536'
,'1239537'
,'1239546'
,'1239592'
,'1239696'
,'1240160'
,'1240174'
,'1240279'
,'1240326'
,'1240376'
,'1240378'
,'1240444'
,'1240533'
,'1241058'
,'1241378'
,'1241382'
,'1241388'
,'1241412'
,'1241701'
,'1242134'
,'1242305'
,'1242352'
,'1242354'
,'1242459'
,'1242460'
,'1242517'
,'1242775'
,'1243169'
,'1243200'
,'1243351'
,'1243484'
,'1243562'
,'1243569'
,'1243619'
,'1243667'
,'1243668'
,'1243671'
,'1243672'
,'1243689'
,'1243728'
,'1243775'
,'1243817'
,'1243849'
,'1244415'
,'1244470'
,'1244505'
,'1244529'
,'1244530'
,'1244544'
,'1244547'
,'1244590'
,'1245077'
,'1245343'
,'1245344'
,'1245356'
,'1245359'
,'1245393'
,'1245675'
,'1245788'
,'1245985'
,'1246001'
,'1246290'
,'1246304'
,'1246321'
,'1247032'
,'1247033'
,'1247161'
,'1247177'
,'1247327'
,'1248019'
,'1248025'
,'1248026'
,'1248027'
,'1248028'
,'1248042'
,'1248059'
,'1248063'
,'1248081'
,'1248094'
,'1248136'
,'1248166'
,'1248171'
,'1248175'
,'1248220'
,'1248229'
,'1248319'
,'1248320'
,'1248787'
,'1248958'
,'1248967'
,'1248973'
,'1248980'
,'1248990'
,'1249001'
,'1249044'
,'1249139'
,'1249849'
,'1249881'
,'1249923'
,'1250142'
,'1250455'
,'1250658'
,'1250707'
,'1250711'
,'1250834'
,'1251376'
,'1251383'
,'1251384'
,'1251635'
,'1252091'
,'1252123'
,'1252140'
,'1252141'
,'1252142'
,'1252282'
,'1252735'
,'1252755'
,'1252771'
,'1253476'
,'1254426'
,'1254536'
,'1255163'
,'1255458'
)



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
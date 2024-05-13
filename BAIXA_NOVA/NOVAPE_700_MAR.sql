DECLARE
  @Cest int = 0,
  @CDoc int,
  @codlanc int,
  @Status varchar(1),
  @D_Lan datetime,
  @D_Reg datetime,
  @D_Cai datetime,
  @Tip_Ba varchar(1) ,
  @Tip_Doc varchar(1) ,
  @Qtd_DAtr int ,
  @Vlr_Prin float,
  @Vlr_Desc float,
  @Vlr_Deduc float,
  @Vlr_Jur float,
  @Vlr_Acr float,
  @Cod_Rec int = ,
  @Usuar varchar(35),
  @Transac datetime,
  @Cod_CtrOr varchar(1),
  @Isn_Cta int,
  @Cod_CntC int,
  @Vlr_DscD float

Declare C_Lanc CURSOR FOR

	select max(cod_lancamento) + 1 from BXREC where Cod_Estabe = 0 and Cod_Rec = @Cod_Rec

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
		Dat_Lancamento = '20240301', 
		Dat_Registro = '20240301', 
		Dat_Caixa = '20240301' , 
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
	and Num_Documento in ('1410199'
,'1408038'
,'1411264'
,'1414487'
,'1411230'
,'1397578'
,'1415068'
,'1412741'
,'1415109'
,'1410074'
,'1408230'
,'1401545'
,'1401739'
,'1396590'
,'1400713'
,'1404298'
,'1409382'
,'1412192'
,'1414489'
,'1401562'
,'1407451'
,'1412615'
,'1401817'
,'1413671'
,'1406721'
,'1406846'
,'1407197'
,'1407985'
,'1408101'
,'1408998'
,'1409007'
,'1408991'
,'1411570'
,'1412028'
,'1414010'
,'1414037'
,'1415069'
,'1401950'
,'1409477'
,'1411274'
,'1413874'
,'1396573'
,'1405445'
,'1407365'
,'1401722'
,'1410697'
,'1400274'
,'1397819'
,'1400559'
,'1402282'
,'1402284'
,'1404429'
,'1406416'
,'1409426'
,'1412150'
,'1412154'
,'1412625'
,'1400596'
,'1404953'
,'1408509'
,'1412184'
,'1414306'
,'1418013'
,'1409445'
,'1413123'
,'1403817'
,'1408508'
,'1409468'
,'1400699'
,'1402393'
,'1405229'
,'1406447'
,'1409409'
,'1410408'
,'1409451'
,'1415773'
,'1417076'
,'1417988'
,'1401088'
,'1404935'
,'1407032'
,'1397718'
,'1398632'
,'1407196'
,'1413888'
,'1414476'
,'1412081'
,'1407967'
,'1400387'
,'1401534'
,'1401539'
,'1405085'
,'1409365'
,'1410111'
,'1415736'
,'1401549'
,'1401713'
,'1402369'
,'1403862'
,'1404129'
,'1405282'
,'1409364'
,'1410238'
,'1414418'
,'1395968'
,'1409953'
,'1409988'
,'1411550'
,'1410708'
,'1415041'
,'1398307'
,'1398667'
,'1398688'
,'1398801'
,'1400260'
,'1401177'
,'1400238'
,'1408229'
,'1409587'
,'1411995'
,'1412015'
,'1414708'
,'1399537'
,'1404239'
,'1410225'
,'1413182'
,'1412502'
,'1388899'
,'1387800'
,'1413077'
,'1415814'
,'1396310'
,'1397834'
,'1405112'
,'1408495'
,'1410108'
,'1412152'
,'1409959'
,'1409960'
,'1409973'
,'1398020'
,'1403813'
,'1407698'
,'1411066'
,'1414348'
,'1401827'
,'1403678'
,'1410805'
,'1403671'
,'1405806'
,'1410806'
,'1413898'
,'1398599'
,'1408479'
,'1419040'
,'1401375'
,'1402288'
,'1410552'
,'1405224'
,'1409349'
,'1401420'
,'1409781'
,'1403818'
,'1408681'
,'1410215'
,'1414186'
,'1402941'
,'1403792'
,'1404125'
,'1405130'
,'1405223'
,'1402940'
,'1409179'
,'1411229'
,'1412179'
,'1410037'
,'1417726'
,'1412190'
,'1410181'
,'1417053'
,'1401532'
,'1410203'
,'1404128'
,'1407343'
,'1401393'
,'1409361'
,'1409908'
,'1417152'
,'1405284'
,'1413949'
,'1414316'
,'1414317'
,'1412175'
,'1405446'
,'1410232'
,'1400688'
,'1402280'
,'1412170'
,'1400170'
,'1405218'
,'1414307'
,'1397774'
,'1401688'
,'1405179'
,'1407359'
,'1408571'
,'1410132'
,'1411271'
,'1414526'
,'1397697'
,'1402254'
,'1406304'
,'1408311'
,'1409180'
,'1410116'
,'1412181'
,'1418028'
,'1418871'
,'1414515'
,'1407388'
,'1400481'
,'1411263'
,'1397522'
,'1402286'
,'1412151'
,'1413121'
,'1412153'
,'1413391'
,'1414475'
,'1397842'
,'1412178'
,'1417989'
,'1414305'
,'1414394'
,'1408643'
,'1409177'
,'1397465'
,'1404366'
,'1410769'
,'1406613'
,'1400207'
,'1401612'
,'1410110'
,'1412191'
,'1401555'
,'1408474'
,'1403833'
,'1414647'
,'1398651'
,'1397702'
,'1400645'
,'1401556'
,'1410801'
,'1412557'
,'1414485'
,'1403809'
,'1405135'
,'1405359'
,'1403807'
,'1405283'
,'1408476'
,'1409466'
,'1411279'
,'1412521'
,'1413555'
,'1410702'
,'1417033'
,'1407838'
,'1410310'
,'1410802'
,'1400146'
,'1417190'
,'1400351')

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
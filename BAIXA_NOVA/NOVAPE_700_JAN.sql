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
		Dat_Lancamento = '20240101', 
		Dat_Registro = '20240101', 
		Dat_Caixa = '20240101' , 
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
	and Num_Documento in ('1377188' 
  ,'1369562'
  ,'01'
  ,'1368729' 
  ,'1370258' 
  ,'1374962' 
  ,'1366667'
  ,'1376037' 
  ,'1379265' 
  ,'1364485' 
  ,'1366913' 
  ,'1367259' 
  ,'1372266' 
  ,'1372837'
  ,'1373857'
  ,'1374995' 
  ,'1379017' 
  ,'1379024' 
  ,'1370467' 
  ,'1365267' 
  ,'1368025' 
  ,'1369310' 
  ,'1381410' 
  ,'1369313' 
  ,'1378270' 
  ,'1370479' 
  ,'1372087' 
  ,'1374039' 
  ,'1375848' 
  ,'1379051'
	,'1380468'	
  ,'1373344' 
  ,'1368546' 
  ,'1369681' 
  ,'1366174' 
  ,'1367106' 	
  ,'1370257'
	,'1376556'
	,'1377483'
	,'1382373'
	,'1382382'
	,'1369002'
	,'1369601'
	,'1372085'
	,'1370698'
	,'1371314'
	,'1373342'
	,'1374306'
	,'1374317'
	,'1377418'
	,'1377675'
	,'1363598'
	,'1364415'
	,'1366034'
	,'1376836'
	,'1368819'
	,'1366105'
	,'1371916'
	,'1377199'
	,'1372847'
	,'1376662'
	,'1365252'
	,'1375010'
	,'1374901'
	,'1374997'
	,'1368759'
	,'1369314'
	,'1373860'
	,'1366006'
	,'1368024'
	,'1368750'
	,'1373343'
	,'1380420'
	,'1366707'
	,'1367753'
	,'1368827'
	,'1381042'
	,'1366335'
	,'1372709'
	,'1376663'
	,'1369521'
	,'1369600'
	,'1367253'
	,'1371172'
	,'1376837'
	,'1376841'
	,'1380469'
	,'1366916'
	,'1368535'
	,'1369312'
	,'1374305'
	,'1377905'
	,'1366108'
	,'1381193'
	,'1375811'
	,'1369571'
	,'1378244'
	,'1380263'
	,'1368857'
	,'1372148'
	,'1373865'
	,'1366215'
	,'1366919'
	,'1370516'
	,'1372188'
	,'1376744'
	,'1364408'
	,'1371337'
	,'1378087'
	,'1378090'
	,'1379220'
	,'1365210'
	,'1365341'
	,'1366758'
	,'1370476'
	,'1371361'
	,'1372100'
	,'1373858'
	,'1375034'
	,'1376074'
	,'1366014'
	,'1368682'
	,'1368780'
	,'1370387'
	,'1371167'
	,'1372012'
	,'1374087'
	,'1373323'
	,'1376574'
	,'1379784'
	,'1381268'
	,'1376586'
	,'1370507'
	,'1369604'
	,'1368754'
	,'1376552'
	,'1374289'
	,'1372023'
	,'1370520'
	,'1365862'
	,'1376095'
	,'1368861'
	,'1373863'
	,'1376695'
	,'1378038'
	,'1371145'
	,'1379753'
	,'1373862'
	,'1373859'
	,'1377673'
	,'1367752'
	,'1364282'
	,'1365339'
	,'1370267'
	,'1373864'
	,'1378837'
	,'1368751'
	,'1372088'
	,'1378086'
	,'1367258'
	,'1368148'
	,'1369643'
	,'1371407'
	,'1363122'
	,'1369537'
	,'1370457'
	,'1375643'
	,'1376732'
	,'1363261'
	,'1379719'
	,'1379023'
	,'1374288'
	,'1374207'
	,'1371306')

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
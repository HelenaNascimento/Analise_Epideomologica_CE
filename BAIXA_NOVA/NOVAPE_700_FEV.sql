DECLARE
  @Cest int = 0,
  @CDoc int,
  @codlanc int,
  @Status varchar(1),
  @D_Lan datetime,
  @D_Reg datetime ,
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
		Dat_Lancamento = '20240201', 
		Dat_Registro = '20240201', 
		Dat_Caixa = '20240201' , 
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
	and Num_Documento in (,'1395155'
,'1393501'
,'1382962'
,'1391821'
,'1393354'
,'1395411'
,'1391552'
,'1395823'
,'1380835'
,'1392698'
,'1379845'
,'1383126'
,'1388152'
,'1389125'
,'1392585'
,'1393703'
,'1393224'
,'1391758'
,'1384813'
,'1389913'
,'1380661'
,'1388273'
,'1395176'
,'1392394'
,'1381677'
,'1391590'
,'1382101'
,'1386347'
,'1394285'
,'1387256'
,'1388161'
,'1389868'
,'1391752'
,'1392546'
,'1394606'
,'1384198'
,'1387200'
,'1388223'
,'1391730'
,'1393552'
,'1387025'
,'1390809'
,'1393488'
,'1384135'
,'1385339'
,'1385354'
,'1389960'
,'1393687'
,'1397720'
,'1398786'
,'1399478'
,'1395577'
,'1392844'
,'1385595'
,'1389959'
,'1392720'
,'1391427'
,'1387952'
,'1388033'
,'1389858'
,'1391609'
,'1395208'
,'1396278'
,'1385568'
,'1387186'
,'1389968'
,'1390310'
,'1388548'
,'1381696'
,'1389684'
,'1388601'
,'1391446'
,'1390796'
,'1398234'
,'1390695'
,'1380426'
,'1383161'
,'1384818'
,'1391639'
,'1394532'
,'1395831'
,'1388759'
,'1395079'
,'1387272'
,'1395858'
,'1383164'
,'1384117'
,'1379901'
,'1389337'
,'1390878'
,'1393502'
,'1389917'
,'1389181'
,'1385514'
,'1386314'
,'1395152'
,'1399353'
,'1389806'
,'1384609'
,'1384636'
,'1384779'
,'1385619'
,'1382968'
,'1388178'
,'1387355'
,'1387124'
,'1384018'
,'1367240'
,'1387180'
,'1386453'
,'1392516'
,'1393907'
,'1384213'
,'1387120'
,'1380496'
,'1381397'
,'1386329'
,'1388080'
,'1389978'
,'1385638'
,'1388029'
,'1397699'
,'1386577'
,'1385643'
,'1394707'
,'1380433'
,'1380490'
,'1388067'
,'1389152'
,'1387245'
,'1390706'
,'1394488'
,'1396254'
,'1384156'
,'1385455'
,'1384812'
,'1393711'
,'1389335'
,'1386306'
,'1398229'
,'1380432'
,'1384103'
,'1391626'
,'1391672'
,'1389657'
,'1391425'
,'1397515'
,'1390882'
,'1380492'
,'1394270'
,'1386423'
,'1390181'
,'1394146'
,'1389147'
,'1392794'
,'1380478'
,'1384092'
,'1388026'
,'1389877'
,'1384775'
,'1392535'
,'1396255'
,'1396262'
,'1397712'
,'1387123'
,'1392405')

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
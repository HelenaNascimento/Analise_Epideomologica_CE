SET NOCOUNT ON
GO

SET DATEFORMAT YMD
GO

Update PARAM
   Set FlgEmbDscDesoneItePrcUni = 0
 Where FlgEmbDscDesoneItePrcUni is null
GO

Update cb
   Set cb.Flg_AltUnvItePdv = pm.FlgAltUnvItePdv
From PDVCB cb
     Inner Join PARAM pm on cb.Cod_Estabe = pm.Cod_Estabe
Where cb.Status1 = 'P'
  And Status2 <> 'N'
  And cb.Flg_AltUnvItePdv is null
GO

Update cb
   Set cb.Flg_AltUnvItePdv = pm.FlgAltUnvItePdv
From PDVCB cb
     Inner Join PARAM pm on cb.Cod_Estabe = pm.Cod_Estabe
Where cb.Flg_AltUnvItePdv is null
  And Dat_Pedido > = (Select min(Dat_Pedido) From PDVCB Where Flg_AltUnvItePdv = 1)
GO

Update POCOM
   Set Flg_UsaTabPrcCadCli = 0
 Where Flg_UsaTabPrcCadCli is null
GO

Update TPOPE
   Set Transacao = cast('2022-02-28' as smalldatetime)
 Where Transacao is null
GO

Update TPOPE
   Set Flg_OpeVdo = 0
 Where Transacao <= cast('2022-02-28' as smalldatetime)
GO


BEGIN
Declare @CodOri char(1),
        @DesOri varchar(20),
        @OrdApr char(1)

Set @CodOri = 'G'
Set @DesOri = 'Consignação'
Set @OrdApr = 'B'
if not Exists(Select 1 From TBORP Where Cod_OriPed = @CodOri)
  Insert Into TBORP (Cod_OriPed, Des_OriPed, Per_LucMin, Cod_OrdApr, Flg_Automatico, Flg_Bloqueado)
             Values (@CodOri,    @DesOri,    0,          @OrdApr,    1,              0)
END
GO

Update PARAM
   Set FlgExbLgpdCadCli = 0
 Where FlgExbLgpdCadCli is null
GO

-- corrige lote em CNGMV
Update mv
   Set mv.Cod_Lote = it.Cod_Lote
From CNGMV mv
     inner join NFSIT it on mv.Cod_Estabe = it.Cod_Estabe and mv.Ser_Docume = it.Ser_Nota
	                    and mv.Num_Docume = it.Num_Nota and mv.Cod_Produt = it.Cod_Produto
						and mv.Qtd_Movime = it.Qtd_Produto
Where mv.Cod_Lote <> it.Cod_Lote
GO

Update rc
   Set rc.Sta_CngPed = '3'
From CNGRC rc
     inner Join NFSCB cb on rc.Cod_Estabe = cb.Cod_Estabe and rc.Num_PedVen = cb.Cod_Pedido
Where rc.Num_PedVen > 0
And rc.Sta_CngPed = '2'
GO

Update c
   Set c.Qtd_Digita = IsNull(a.QtdDig,0)
  From CNGIT c 
       Left Join (Select cb.Cod_Estabe, cb.Id_Consig, it.Cod_Produt,
                         QtdDig = sum(it.Qtd_Pedido)
                    From CNGRI it
                         Inner Join CNGRC cb on cb.Cod_Estabe = it.Cod_Estabe and cb.Id_Consig = it.Id_Consig and cb.Num_Sequen = it.Num_Sequen
                   Where ((cb.Sta_CngPed = '0') or (cb.Sta_CngPed = '1') or (cb.Sta_CngPed = '2'))
                   Group by cb.Cod_Estabe, cb.Id_Consig, it.Cod_Produt) a on c.Cod_Estabe = a.Cod_Estabe and c.Id_Consig = a.Id_Consig and c.Cod_Produt = a.Cod_Produt
Where c.Qtd_Digita <> IsNull(a.QtdDig,0)
GO

Update PARAM
   Set FlgZerAutMesSldVrbPos = 0
 Where FlgZerAutMesSldVrbPos is null
GO

Update PARAM
   Set FlgZerAutMesSldVrbNeg = 0
 Where FlgZerAutMesSldVrbNeg is null
GO

-- Corrige lançamentos em VBSLD
Update VBSLD
   Set Tip_Docume = 'E',
       Cod_Movime = 'D',
       Tip_Sai = ''
Where Tip_Docume = 'D'
And IsNull(Ser_Docume,'') = ''
GO

Update VBSLD
  Set Num_Protoc = -1,
      Tip_Sai = '',
      Tip_Docume = 'M',
      Cod_Movime = 'Z',
      Des_Movime = 'ANULA SALDO ANTERIOR'
Where Tip_Sai = 'M'
  And Tip_Docume = ''
  And ((Cod_Movime = '-') or (Cod_Movime = '+'))
  And Des_Movime like 'AJUSTE AUT%'
GO

Update VBSLD
  Set Num_Protoc = -1
Where Tip_Sai = ''
  And Tip_Docume = 'M'
  And Cod_Movime = 'Z'
  And Num_Protoc <> 999999999
GO

-- Corrige lançamentos em VBFSL
Update VBFSL
   Set Tip_Docume = 'E',
       Cod_Movime = 'D',
       Tip_Sai = ''
Where Tip_Docume = 'D'
And IsNull(Ser_Docume,'') = ''
GO

Update VBFSL
  Set Num_Protoc = -1,
      Tip_Sai = '',
      Tip_Docume = 'M',
      Cod_Movime = 'Z',
      Des_Movime = 'ANULA SALDO ANTERIOR'
Where Tip_Sai = 'M'
  And Tip_Docume = ''
  And ((Cod_Movime = '-') or (Cod_Movime = '+'))
  And Des_Movime like 'AJUSTE AUT%'
GO

Update VBFSL
  Set Num_Protoc = -1
Where Tip_Sai = ''
  And Tip_Docume = 'M'
  And Cod_Movime = 'Z'
GO

Update mv
  Set mv.Prc_unitar = it.Prc_Unitario
From CNGMV mv
     inner join NFSIT it on mv.Cod_Estabe = it.Cod_Estabe and 
                            mv.Ser_Docume = it.Ser_Nota and 
                            mv.Num_Docume = it.Num_Nota and 
                            mv.Cod_Produt = it.Cod_Produto and 
                            mv.Cod_Lote = it.Cod_Lote

Where mv.Prc_Unitar is null
  And mv.Tip_EntSai = 'S'
  And mv.Cod_Movime = 'NFR'
GO

/*
Update NFSCB
   Set Tip_frete = 'A'
Where Tip_Frete = '9'
GO
*/


Update NFECB
  Set Tip_frete = 'A'
Where Tip_Frete = '9'
GO

-- preencher com zeros à esquerda até 13 posições
Update PRODU
   Set Reg_MS = Substring('0000000000000',1,13-Len(Reg_MS))+Reg_MS
 Where IsNull(Reg_MS,'') <> ''
   And Len(IsNull(Reg_MS,'')) < 13 
   And UPPER(ISNULL(Reg_MS, '')) <> 'ISENTO' 
GO

UPDATE CLIEN 
   SET Id_Pais = 1058
 Where Id_Pais is Null 
   AND Cod_Estado <> 'EX'
GO

UPDATE FORNE 
   SET Id_Pais = 1058 
 Where Id_Pais is Null 
   AND Estado_Com <> 'EX'
GO

Update NFECB
   Set Cod_OriDesNfs = 'N'
Where IsNull(Cod_OriDesNfs,'') = ''
GO

Update cb
   Set cb.Flg_BlqTrfEstFis = cf.Flg_BlqTrfEstFis
  From NFSCB cb
       inner Join TBCFO cf on cb.Cod_Cfo1 = cf.Codigo
 Where cb.Status <> 'A'
   And Year(cb.Dat_Emissao) >= (Year(GetDate())-4)
   And cf.Flg_BlqTrfEstFis = 1
   And cb.Flg_BlqTrfEstFis is null
GO

Update cb
   Set cb.Flg_BlqTrfEstFis = cf.Flg_BlqTrfEstFis
  From NFECB cb
       inner Join TBCFO cf on cb.Cod_Cfo = cf.Codigo
 Where cb.Status <> 'A'
   And Year(cb.Dat_Emissao) >= (Year(GetDate())-4)
   And cf.Flg_BlqTrfEstFis = 1
   And cb.Flg_BlqTrfEstFis is null
GO

-- atualiza PMVDO
BEGIN TRANSACTION
Declare @CodCfoSai int,
        @DesCfoSai varchar(200),
        @CodCfoEnt int,
        @DesCfoEnt varchar(200),
        @CodCfoDev int = 0

Set @CodCfoSai = 5105
Set @DesCfoSai = 'VENDA DE PRODUÇÃO DO ESTABELECIMENTO QUE NÂO DEVA PO ELE TRANSITAR'
if not Exists(Select 1 From TBCFO Where Codigo = @CodCfoSai)
  Insert Into TBCFO (Codigo, Descricao, Tip_EntSai, Tip_NotFis, Flg_MovEst, Cod_MovEst, Flg_Blq, Flg_BlqTrfEstFis)
        Values (@CodCfoSai, @DesCfoSai, 'S',        'V',        1,          21,         0,       0)

Set @CodCfoEnt = 1102
Set @DesCfoSai = 'COMPRA PARA COMERCIALIZAÇÃO'
if not Exists(Select 1 From TBCFO Where Codigo = @CodCfoEnt)
  Insert Into TBCFO (Codigo, Descricao, Tip_EntSai, Tip_NotFis, Flg_MovEst, Cod_MovEst, Flg_Blq, Flg_BlqTrfEstFis)
        Values (@CodCfoEnt, @DesCfoEnt, 'E',        'C',        1,          11,         0,       0)

if not Exists(Select 1 From PMVDO Where Cod_CfoSai = @CodCfoSai and Cod_CfoEnt = @CodCfoEnt)
  Insert Into PMVDO (Cod_CfoSai, Cod_CfoEnt, Cod_CfoDev, Des_NatOpeSai, Des_NatOpeEnt, Flg_MovEst, Flg_AtuPrcCus, Flg_DestacIcm)
              Select @CodCfoSai, @CodCfoEnt, @CodCfoDev, ops.Descricao, ope.Descricao, 0,          0,             0
                From TBCFO ops, TBCFO ope
               Where ops.Codigo = @CodCfoSai
                 And ope.Codigo = @CodCfoEnt

Set @CodCfoEnt = 1403
Set @DesCfoSai = 'COMPRA P/ COMERCIALIZAÇÃO EM OPERAÇÃO COM MERCADORIA SUJEITA A ST'
if not Exists(Select 1 From TBCFO Where Codigo = @CodCfoEnt)
  Insert Into TBCFO (Codigo, Descricao, Tip_EntSai, Tip_NotFis, Flg_MovEst, Cod_MovEst, Flg_Blq, Flg_BlqTrfEstFis)
        Values (@CodCfoEnt, @DesCfoEnt, 'E',        'C',        1,          11,         0,       0)

if not Exists(Select 1 From PMVDO Where Cod_CfoSai = @CodCfoSai and Cod_CfoEnt = @CodCfoEnt)
  Insert Into PMVDO (Cod_CfoSai, Cod_CfoEnt, Cod_CfoDev, Des_NatOpeSai, Des_NatOpeEnt, Flg_MovEst, Flg_AtuPrcCus, Flg_DestacIcm)
              Select @CodCfoSai, @CodCfoEnt, @CodCfoDev, ops.Descricao, ope.Descricao, 0,          0,             0
                From TBCFO ops, TBCFO ope
               Where ops.Codigo = @CodCfoSai
                 And ope.Codigo = @CodCfoEnt


Set @CodCfoSai = 6105
Set @DesCfoSai = 'VENDA DE PRODUÇÃO DO ESTABELECIMENTO QUE NÂO DEVA PO ELE TRANSITAR'
if not Exists(Select 1 From TBCFO Where Codigo = @CodCfoSai)
  Insert Into TBCFO (Codigo, Descricao, Tip_EntSai, Tip_NotFis, Flg_MovEst, Cod_MovEst, Flg_Blq, Flg_BlqTrfEstFis)
        Values (@CodCfoSai, @DesCfoSai, 'S',        'V',        1,          21,         0,       0)

Set @CodCfoEnt = 2102
Set @DesCfoSai = 'COMPRA PARA COMERCIALIZAÇÃO'
if not Exists(Select 1 From TBCFO Where Codigo = @CodCfoEnt)
  Insert Into TBCFO (Codigo, Descricao, Tip_EntSai, Tip_NotFis, Flg_MovEst, Cod_MovEst, Flg_Blq, Flg_BlqTrfEstFis)
        Values (@CodCfoEnt, @DesCfoEnt, 'E',        'C',        1,          11,         0,       0)

if not Exists(Select 1 From PMVDO Where Cod_CfoSai = @CodCfoSai and Cod_CfoEnt = @CodCfoEnt)
  Insert Into PMVDO (Cod_CfoSai, Cod_CfoEnt, Cod_CfoDev, Des_NatOpeSai, Des_NatOpeEnt, Flg_MovEst, Flg_AtuPrcCus, Flg_DestacIcm)
              Select @CodCfoSai, @CodCfoEnt, @CodCfoDev, ops.Descricao, ope.Descricao, 0,          0,             0
                From TBCFO ops, TBCFO ope
               Where ops.Codigo = @CodCfoSai
                 And ope.Codigo = @CodCfoEnt

Set @CodCfoEnt = 2403
Set @DesCfoSai = 'COMPRA P/ COMERCIALIZAÇÃO EM OPERAÇÃO COM MERCADORIA SUJEITA A ST'
if not Exists(Select 1 From TBCFO Where Codigo = @CodCfoEnt)
  Insert Into TBCFO (Codigo, Descricao, Tip_EntSai, Tip_NotFis, Flg_MovEst, Cod_MovEst, Flg_Blq, Flg_BlqTrfEstFis)
        Values (@CodCfoEnt, @DesCfoEnt, 'E',        'C',        1,          11,         0,       0)

if not Exists(Select 1 From PMVDO Where Cod_CfoSai = @CodCfoSai and Cod_CfoEnt = @CodCfoEnt)
  Insert Into PMVDO (Cod_CfoSai, Cod_CfoEnt, Cod_CfoDev, Des_NatOpeSai, Des_NatOpeEnt, Flg_MovEst, Flg_AtuPrcCus, Flg_DestacIcm)
              Select @CodCfoSai, @CodCfoEnt, @CodCfoDev, ops.Descricao, ope.Descricao, 0,          0,             0
                From TBCFO ops, TBCFO ope
               Where ops.Codigo = @CodCfoSai
                 And ope.Codigo = @CodCfoEnt

COMMIT TRANSACTION
GO


-- atualiza TBMSG: IRRF
BEGIN TRANSACTION
Declare @Id_Mensag int,
        @Des_Mensag varchar(200),
		@Des_Select  varchar(2000),
		@Des_Observ varchar(800),
		@Nom_TabPri varchar(50),
        @Flg_infCpl bit, 
        @Flg_infAdProd bit, 
        @Flg_infAdFisco bit, 
        @Flg_ObsCont bit,
        @Flg_ObsFisco bit

 -- IRRF 
Set @Des_Mensag = 'Decreto nº15.258 de 14/02/2022'

Set @Flg_infCpl = 1
Set @Flg_infAdProd = 0 
Set @Flg_infAdFisco = 0 
Set @Flg_ObsCont = 0
Set @Flg_ObsFisco = 0

Set @Nom_TabPri = 'NFSCB'
Set @Des_Select = 'Select top 1 AlqIrf    = cast(IsNull(Alq_Irf,0) as money), '+
                               'VlrBasIrf = cast(sum(IsNull(Vlr_BasIrf,0)) as decimal(18,2)), '+
                               'VlrIrf    = cast(Round(sum(IsNull(Vlr_Irf,0)),2) as decimal(18,2)), '+
                               'VlrSemIrf = cast((sum(IsNull(Vlr_BasIrf,0))-Round(sum(IsNull(Vlr_Irf,0)),2)) as decimal(18,2)) '+
                    'From NFSIT '+
                   'Where Cod_Estabe = :PCodEst And Ser_Nota = :PSerNot And Num_Nota = :PNumNot '+
                     'And IsNull(Vlr_Irf,0) > 0 '+
                     'And IsNull(Alq_Irf,0) > 0 '+
                   'Group by Alq_Irf '

Set @Des_Observ = 'IRRF com base na Instrução Normativa RFB 1.234, Anexo I 11/02/2012, Alíquota de [AlqIrf]% '+
                  'Informo o valor do imposto a ser retido: Valor da Nota: R$ [VlrBasIrf] IRFF: [AlqIrf]%, '+
				  'Valor R$ [VlrSemIrf]'

if not Exists(Select 1 From TBMSG Where Des_Mensag = @Des_Mensag)
  begin
    Set @Id_Mensag = IsNull((Select max(Id_Mensag) From TBMSG),0) + 1
    Insert Into TBMSG (Id_Mensag, Des_Mensag, Des_Select, Des_Observ, Nom_TabPri, Flg_infCpl, Flg_infAdProd, Flg_infAdFisco, Flg_ObsCont)
        Values(@Id_Mensag, @Des_Mensag, @Des_Select, @Des_Observ, @Nom_TabPri, 1, 0, 0, 0)
  end

Set @Id_Mensag = (Select top 1 Id_Mensag From TBMSG Where Des_Mensag = @Des_Mensag)

Update TBMSG
   Set Des_Select = @Des_Select
     , Des_Observ = @Des_Observ
     , Nom_TabPri = @Nom_TabPri
     , Flg_infAdFisco = @Flg_infAdFisco
     , Flg_ObsCont = @Flg_ObsCont
     , Flg_ObsFisco = @Flg_ObsFisco
 Where Id_Mensag = @Id_Mensag

COMMIT TRANSACTION
GO

if Exists(Select 1
            From VBSLD
           Where Cod_Movime = 'Z'
             And Num_Protoc = -1)
  Update VBSLD
     Set Dat_Movime = Case When DAY(Dat_Movime) = 1 then Dat_Movime-1 Else Dat_Movime End, 
         Des_Movime = 'ZERA SALDO DO MÊS', 
	     Num_Protoc =  999999999
   Where Cod_Movime = 'Z'
     And Num_Protoc = -1
GO

if Exists(Select 1
            From VBFSL
           Where Cod_Movime = 'Z'
             And Num_Protoc = -1)
  Update VBFSL
     Set Dat_Movime = Case When DAY(Dat_Movime) = 1 then Dat_Movime-1 Else Dat_Movime End, 
         Des_Movime = 'ZERA SALDO DO MÊS', 
         Num_Protoc =  999999999
   Where Cod_Movime = 'Z'
     And Num_Protoc = -1
GO

Update TPOPE
  Set Flg_CalPisCof = 1
Where Cod_CfoNorInt > 0
  And ((Tip_OPe = 'V') or (Tip_Ope = 'C'))
  And IsNull(Flg_CalPisCof,0) = 0
GO

Update TPOPE
  Set Flg_CalRepIcmInt = 0
Where Flg_CalRepIcmInt is null
GO

Update PARAM
   Set FlgRegistraLogDivergencia = 0
 Where FlgRegistraLogDivergencia is null
GO

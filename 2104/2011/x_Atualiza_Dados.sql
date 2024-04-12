SET NOCOUNT ON
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

if Exists(Select 1
            From VBSLD
           Where Cod_Movime = 'Z'
             And Num_Protoc = -1)
  Update VBSLD
     Set Dat_Movime = Dat_Movime-1, 
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
     Set Dat_Movime = Dat_Movime-1, 
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

Update TRXUF
   Set Flg_BlqPdvVlrMinFre = 0
 Where Flg_BlqPdvVlrMinFre is null
GO

Insert into DPXPR(Cod_Estabe, Cod_Dep, Cod_Produt)
  Select distinct L.Cod_Estabe, L.Cod_Dep, L.Cod_Produt
    From PRLOT L
   Where not Exists(Select 1 From DPXPR dp
                      Where L.Cod_Estabe = dp.Cod_Estabe And L.Cod_Dep = dp.Cod_Dep and L.Cod_Produt = dp.Cod_Produt)
GO

-- Criar em TBGEN:  Tabela Origem de Pedidos de Venda
Declare @CodTabGen char(3) = 'ORP',
        @CodGen varchar(6),
		@DesGen varchar(30)

Set @CodGen = 'A'
Set @DesGen = 'Ativo'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'P'
Set @DesGen = 'Receptivo'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'M'
Set @DesGen = 'Móvel/Força de Vendas'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'T'
Set @DesGen = 'EDI/Eletrônico'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'W'
Set @DesGen = 'WEB/Internet'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'H'
Set @DesGen = 'Hospitalar'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'D'
Set @DesGen = 'por Desobramento'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'V'
Set @DesGen = 'por Vales'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'C'
Set @DesGen = 'por Cotação'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'G'
Set @DesGen = 'Consignação'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)

Set @CodGen = 'X'
Set @DesGen = 'Indefinido'
if not Exists(Select 1 From TBGEN Where Cod_TabGen = @CodTabGen And Cod_Gen = @CodGen)
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen)
             Values (@CodTabGen, @CodGen, @DesGen)
GO

/*
   Update PMEML.Id = Increment(1, 1..N)
*/
UPDATE PMEML
    SET Id = A_PMEML.Id
FROM PMEML AS PMEML
     INNER JOIN (
		 SELECT 
		 	 Cod_Estabe
		 	 , Usuario
		 	 , ROW_NUMBER() OVER (ORDER BY Cod_Estabe, Usuario) AS Id
		 FROM PMEML	     
	 ) AS A_PMEML 
	     ON A_PMEML.Cod_Estabe = PMEML.Cod_Estabe
		    AND A_PMEML.Usuario = PMEML.Usuario
WHERE COALESCE(PMEML.Id, 0) = 0		
GO

/*
   Redefinir senhas dos usuarios "ADMIN" e "INFARMA"
*/

UPDATE USUAR SET 
    Senha_Hash = '70b91535f16c48320b54ac8d34bb2ab8'
WHERE 1=1
      AND Nome_Login = 'ADMIN'
	  AND COALESCE(Senha_Hash, '') <> '70b91535f16c48320b54ac8d34bb2ab8'    

UPDATE USUAR SET Senha_Hash = 'a778a5d0bd628f4b2725c8b13fe47932'
WHERE 1=1
      AND Nome_Login = 'INFARMA' 
	  AND COALESCE(Senha_Hash, '') <> 'a778a5d0bd628f4b2725c8b13fe47932' 

/*
   Atualizar contas de usuários vigentes
*/  

UPDATE USUAR SET 
    Qtd_TenLogInv = 0 
WHERE 1=1

UPDATE USUAR SET 
    Qtd_TenLogInv = 5 /*Máximo*/
WHERE 1=1
      AND Nome_Login NOT IN ('ADMIN', 'INFARMA')  


/*
    Pix
*/
if not Exists(Select 1 From TBGEN Where Cod_TabGen = 'URL' and Cod_Gen = 'PIXINF')
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen, Tip_Info, Sta_Regist)
             Values ('URL', 'PIXINF', 'http://pix-api.infarmasistemas.com.br/', 'T', 0)
GO

if not Exists(Select 1 From TBGEN Where Cod_TabGen = 'URL' and Cod_Gen = 'PIXSHP')
  Insert Into TBGEN (Cod_TabGen, Cod_Gen, Des_Gen, Tip_Info, Sta_Regist)
             Values ('URL', 'PIXSHP', 'https://api-conexaoitau.shipay.com.br/', 'T', 0)
GO


/*
	Pedido Eletronico
*/
if not Exists(Select 1 From TBCLP Where IsNull(Tip_Priori,'') = '2-NORMAL')
  Update TBCLP
     Set Tip_Priori = '2-NORMAL'
   Where ((Tip_Priori = '1-BAIXA') or 
          (Tip_Priori is NULL))
GO

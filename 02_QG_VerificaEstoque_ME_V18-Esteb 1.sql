-- 05/07/2022

SET NOCOUNT ON
SET ANSI_WARNINGS OFF
GO

-- verifica qtd.fisico de PRXES com saldo em PRSLD
if not Exists (Select top 1 Cod_Produt From PRSLD)
  PRINT 'ERRO: Tabela PRSLD não foi gerada !!!!!'
GO

BEGIN 

Declare @CodEstabe int

-------------------------------------------------------
Set @CodEstabe = 1  --   -1: todos os estabelecimentos+-
-------------------------------------------------------

-- se existir somente um estabelecimento
if (@CodEstabe > -1) and not Exists(Select 1 From PRLOT Where Cod_Estabe = @CodEstabe)
  Set @CodEstabe = (Select top 1 Cod_Estabe From PRLOT)

IF OBJECT_ID('tempdb..#ESTABE') IS NOT NULL
  DROP TABLE #ESTABE

CREATE TABLE #ESTABE (Cod_Estabe int)

if @CodEstabe > -1
  Insert into #ESTABE Values (@CodEstabe)
else
  Insert into #ESTABE
    Select Cod_Estabe From ESTAB

--Select Des_RazSoc as 'Razão Social', 'Fantasia' as Des_Estabe From ESTAB Where Cod_Estabe = @CodEstabe

END
GO


Print ''
Print 'Verifica estoques negativos em PRXES:'
GO
BEGIN 

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN

  -- verifica qtd.fisico > 0 em PRXES sem registro em PRSLD
  if Exists(Select p.Cod_Produt
            From PRXES p Left Outer Join 
                 PRSLD s on p.Cod_Estabe = s.Cod_Estabe and p.Cod_Produt = s.Cod_Produt 
            Where p.Cod_Estabe = @CodEstabe 
            And p.Qtd_Fisico > 0 and s.Cod_Produt is null)
    Select p.Cod_Produt as 'Prd.c/estoque(PRXES) s/registro em PRSLD',
           p.Qtd_Fisico as 'FISICO em PRXES'
    From PRXES p Left Outer Join
         PRSLD s on p.Cod_Estabe = s.Cod_Estabe and p.Cod_Produt = s.Cod_Produt 
    Where p.Cod_Estabe = @CodEstabe 
    And p.Qtd_Fisico > 0 and s.Cod_Produt is null
    
  -- verifica estoque negativo em PRODU
  if Exists(Select Cod_Produt 
            From PRXES 
            Where Cod_Estabe = @CodEstabe 
            And ((Qtd_Dispon < 0) or (Qtd_Fisico < 0)))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem ESTOQUES NEGATIVOS EM PRXES'
      Select Cod_Estabe,
             Cod_Produt as 'Prd. c/Estoque Negativo', 
             Qtd_Fisico as 'Fisico', 
             Qtd_Reserv as 'Reservado', 
             Qtd_Avaria as 'Avariado', 
             Qtd_Solici as 'Solicitado', 
             Qtd_Dispon as 'Disponivel'
      From PRXES 
      Where Cod_Estabe = @CodEstabe 
      And ((Qtd_Dispon < 0) or (Qtd_Fisico < 0))
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Negativos em PRXES: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO


Print ''
Print 'Verifica divergências entre PRXES x PRSLD:'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN

  -- divergencias PRXES x PRSLD
  if Exists(Select p.Cod_Produt
            From PRXES p
                 Outer Apply(Select top 1 s.Qtd_SldAtu
                               From PRSLD s
                              Where s.Cod_Estabe = p.Cod_Estabe
                                And s.Cod_Produt = p.Cod_Produt
                              Order by s.Dat_Movime desc) x 
            Where p.Cod_Estabe = @CodEstabe
            and p.Qtd_Fisico <> IsNull(x.Qtd_SldAtu,0))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES: PRXES x PRSLD'
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (PRXES x PRSLD)', 
             p.Qtd_Fisico as 'Físico em PRODU', 
             IsNull(x.Qtd_SldAtu,0) as 'Físico em PRSLD'
      From PRXES p 
           Outer Apply(Select top 1 s.Qtd_SldAtu
                         From PRSLD s
                        Where s.Cod_Estabe = p.Cod_Estabe
                          And s.Cod_Produt = p.Cod_Produt
                        Order by s.Dat_Movime desc) x 
      Where p.Cod_Estabe = @CodEstabe
      and p.Qtd_Fisico <> IsNull(x.Qtd_SldAtu,0)  
    end
  else
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoque Físico: PRXES x PRSLD ok!'
      if Exists(Select top 1 1 From PRXES
                Where Cod_Estabe = @CodEstabe
                And Qtd_Dispon <> (Qtd_Fisico-Qtd_Solici-Qtd_Reserv-Qtd_Avaria))
        begin
          PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DOS ESTOQUES EM PRXES'
          Select Cod_Estabe,
		         Cod_Produt as 'Prd.c/divergência (PRXES)', 
                 Qtd_Dispon as 'Disponivel Cadastrado', 
                 (Qtd_Fisico-Qtd_Solici-Qtd_Reserv-Qtd_Avaria) as 'Disponivel Calculado'
          From PRXES 
          Where Cod_Estabe = @CodEstabe
          And Qtd_Dispon <> (Qtd_Fisico-Qtd_Solici-Qtd_Reserv-Qtd_Avaria)
        end
      else
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques em PRXES ok!'
    end

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO


-- aqui novo
Print ''
Print 'Verifica divergências de estoques físicos: PRXES x PRLOT+PRLTL :'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN
  if Exists(Select p.Cod_Produt
              From PRXES p 
            Where p.Cod_Estabe = @CodEstabe 
              And ((p.Qtd_Fisico <> dbo.FN_EstoqueFisicoPrdLote(p.Cod_Estabe, p.Cod_Produt, 'TODOS'))  or 
                    (p.Qtd_Dispon >  dbo.FN_EstoqueDisponivelPrdLote(p.Cod_Estabe, p.Cod_Produt, 'TODOS'))))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES FÍSICOS: PRXES x PRLOT+PRLTL'
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (PRXES x PRLOT+PRLTL)', p.Qtd_Fisico as 'Físico em PRXES', 
             dbo.FN_EstoqueFisicoPrdLote(p.Cod_Estabe, p.Cod_Produt, 'TODOS') as 'Físico em PRLOT+PRLTL',
             'Diferença (PRXES-LOTES)' = p.Qtd_Fisico - dbo.FN_EstoqueFisicoPrdLote(p.Cod_Estabe, p.Cod_Produt, 'TODOS')
        From PRXES p 
       Where p.Cod_Estabe = @CodEstabe 
         And ((p.Qtd_Fisico <> dbo.FN_EstoqueFisicoPrdLote(p.Cod_Estabe, p.Cod_Produt, 'TODOS'))  or 
              (p.Qtd_Dispon >  dbo.FN_EstoqueDisponivelPrdLote(p.Cod_Estabe, p.Cod_Produt, 'TODOS')))
       Order by p.Cod_Produt
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoque Físico: PRXES x PRLOT+PRLTL ok!'
  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO














Print ''
Print 'Verifica divergências em estoques reservados:'
GO

BEGIN
Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN

  -- verifica estoque reservado
  if Exists(Select p.Cod_Produt, p.Qtd_Reserv, r.QtdResTot 
            From PRXES p Left Outer Join
                 (Select Cod_Produto, QtdResTot=sum(IsNull(Qtd_Movimento,0)+IsNull(Qtd_ResDep,0))
                  From RESER
                  Where Cod_Estabe = @CodEstabe
                  Group by Cod_Produto) r on (p.Cod_Produt = r.Cod_Produto)
            Where p.Cod_Estabe = @CodEstabe 
            And p.Qtd_Reserv <> IsNull(r.QtdResTot,0))
    begin          
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES: PRXES x RESER'
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (PRXES x RESER)', 
             p.Qtd_Reserv  as 'Reservado em PRXES', 
             r.QtdResTot as 'Reservado em RESER'
      From PRXES p Left Outer Join
           (Select Cod_Produto, QtdResTot=sum(IsNull(Qtd_Movimento,0)+IsNull(Qtd_ResDep,0))
            From RESER
            Where Cod_Estabe = @CodEstabe
            Group by Cod_Produto) r on (p.Cod_Produt = r.Cod_Produto)
      Where p.Cod_Estabe = @CodEstabe 
      And p.Qtd_Reserv <> IsNull(r.QtdResTot,0)
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Reservados em PRXES: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO


Print ''
Print 'Verifica divergências em estoques avariados:'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN

  -- verifica estoque avariado
  if Exists(Select p.Cod_Produt, p.Qtd_Avaria, a.QtdAvaTot 
            From PRXES p Left Outer Join
                 (Select Cod_Produto, QtdAvaTot=sum(IsNull(Qtd_Movimento,0)+IsNull(Qtd_AvaDep,0))
                  From AVARI
                  Where Cod_Estabe = @CodEstabe
                  Group by Cod_Produto) a on (p.Cod_Produt = a.Cod_Produto)
            Where p.Cod_Estabe = @CodEstabe 
            And p.Qtd_Avaria <> IsNull(a.QtdAvaTot,0))
    begin          
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES: PRXES x AVARI'
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (PRXES x AVARI)', 
             p.Qtd_Avaria  as 'Avariado em PRXES', 
             a.QtdAvaTot as 'Avariado em AVARI'
      From PRXES p Left Outer Join
           (Select Cod_Produto, QtdAvaTot=sum(IsNull(Qtd_Movimento,0)+IsNull(Qtd_AvaDep,0))
            From AVARI
            Where Cod_Estabe = @CodEstabe
            Group by Cod_Produto) a on (p.Cod_Produt = a.Cod_Produto)
      Where p.Cod_Estabe = @CodEstabe 
      And p.Qtd_Avaria <> IsNull(a.QtdAvaTot,0)
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Avariados em PRXES: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO

Print ''
Print 'Verifica divergências em estoques em quarentena:'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN
  -- verifica estoque avariado
  if Exists(Select p.Cod_Produt, p.Qtd_Avaria, QtdQuarenTot=dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'X') 
            From PRXES p
           Where p.Cod_Estabe = @CodEstabe 
              And p.Qtd_Quaren <> dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'X'))
    begin          
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES EM QUARENTENA '
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (Quarentena)', 
             p.Qtd_Quaren  as 'Quarentena em PRXES', 
             dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'X') as 'Quarentena em PRLTL'
      From PRXES p
     Where p.Cod_Estabe = @CodEstabe 
       And p.Qtd_Quaren <> dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'X')
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Quarentena em PRXES: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO

Print ''
Print 'Verifica divergências em estoques em Cross Docking:'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN
  -- verifica estoque Cross Docking
  if Exists(Select p.Cod_Produt, p.Qtd_Avaria, QtdCrossDock=dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'C') 
            From PRXES p
           Where p.Cod_Estabe = @CodEstabe 
              And p.Qtd_CrossDock <> dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'C'))
    begin          
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES EM CROSS DOCKING '
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (Cross Docking)', 
             p.Qtd_CrossDock  as 'Cross Docking em PRXES', 
             dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'C') as 'Cross Docking em PRLTL'
      From PRXES p
     Where p.Cod_Estabe = @CodEstabe 
       And p.Qtd_CrossDock <> dbo.FN_QtdSldPrdLotDepFec(p.Cod_Estabe, p.Cod_Produt, '', 'C')
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Cross Docking em PRXES: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO


Print ''
Print 'Verifica divergências em estoques solicitados em  PRXES x (PDVIT+ORCIT):'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN

  -- verifica estoque solicitado em PRXES
  if Exists(Select p.Cod_Produt,
                   IsNull(p.Qtd_Solici,0), IsNull(x.Qtd_Solicitado,0)+IsNull(y.Qtd_Solicitado,0)
            From PRXES p LEFT OUTER JOIN 
                 (Select it.Cod_Produto,
                         sum(IsNull(it.Qtd_Pedido,0)+IsNull(it.Qtd_Bonificacao,0)) as Qtd_Solicitado
                  From PDVCB cb Inner Join
                       PDVIT it on (cb.Cod_Estabe = it.Cod_Estabe And cb.Numero = it.Cod_Pedido)
                  Where cb.Cod_Estabe = @CodEstabe      
                  and cb.Tip_Pedido = 'P'
                  and cb.Status1 = 'P'
                  and cb.Status2 <> 'N'
                  and cb.Status2 <> 'D'
                  and IsNull(cb.Bloqueio,'') <> 'SR'
                  and (IsNull(it.Qtd_Pedido,0)+IsNull(it.Qtd_Bonificacao,0)) > 0
                  Group by it.Cod_Produto) x ON (p.Cod_Produt = x.Cod_Produto) LEFT OUTER JOIN 
                 (Select it.Cod_Produt,
                         sum(IsNull(it.Qtd_Produt,0)) as Qtd_Solicitado
                  From ORCCB cb Inner Join
                       ORCIT it on (cb.Cod_Estabe = it.Cod_Estabe And cb.Cod_Orcame = it.Cod_Orcame)
                  Where cb.Cod_Estabe = @CodEstabe      
                  And cb.Sta_Movime = 'F'
                  and IsNull(cb.Num_Docume,0) = 0
                  and it.Tip_EntSai = 'S'
                  And IsNull(cb.Cod_OriVen,'') <> 'DMD'
                  and (IsNull(it.Qtd_Produt,0)) > 0
                  Group by it.Cod_Produt) y ON (p.Cod_Produt = y.Cod_Produt)
            Where p.Cod_Estabe = @CodEstabe      
            And IsNull(p.Qtd_Solici,0) <> IsNull(x.Qtd_Solicitado,0)+IsNull(y.Qtd_Solicitado,0))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES: PRXES x (PDVIT+ORCIT)'
      
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (PRXES x (PDVIT+ORCIT))', 
             p.Qtd_Solici as 'Solicitado em PRXES', 
             IsNull(x.Qtd_Solicitado,0) as 'Solicitado em PDVIT', 
             IsNull(y.Qtd_Solicitado,0) as 'Solicitado em ORCIT'
            From PRXES p LEFT OUTER JOIN
                 (Select it.Cod_Produto,
                         sum(IsNull(it.Qtd_Pedido,0)+IsNull(it.Qtd_Bonificacao,0)) as Qtd_Solicitado
                  From PDVCB cb Inner Join
                       PDVIT it on (cb.Cod_Estabe = it.Cod_Estabe and cb.Numero = it.Cod_Pedido)
                  Where cb.Cod_Estabe = @CodEstabe      
                  and cb.Tip_Pedido = 'P'
                  and cb.Status1 = 'P'
                  and cb.Status2 <> 'N'
                  and cb.Status2 <> 'D'
                  and IsNull(cb.Bloqueio,'') <> 'SR'
                  and (IsNull(it.Qtd_Pedido,0)+IsNull(it.Qtd_Bonificacao,0)) > 0
                  Group by it.Cod_Produto) x ON (p.Cod_Produt = x.Cod_Produto) LEFT OUTER JOIN
                 (Select it.Cod_Produt,
                         sum(IsNull(it.Qtd_Produt,0)) as Qtd_Solicitado
                  From ORCCB cb Inner Join
                       ORCIT it on (cb.Cod_Estabe = it.Cod_Estabe And cb.Cod_Orcame = it.Cod_Orcame)
                  Where cb.Cod_Estabe = @CodEstabe      
                  And cb.Sta_Movime = 'F'
                  and IsNull(cb.Num_Docume,0) = 0
                  and it.Tip_EntSai = 'S'
                  And IsNull(cb.Cod_OriVen,'') <> 'DMD'
                  and (IsNull(it.Qtd_Produt,0)) > 0
                  Group by it.Cod_Produt) y ON (p.Cod_Produt = y.Cod_Produt)
            Where p.Cod_Estabe = @CodEstabe      
            And IsNull(p.Qtd_Solici,0) <> IsNull(x.Qtd_Solicitado,0)+IsNull(y.Qtd_Solicitado,0)
      Order by p.Cod_Produt
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Solicitados em PRXES: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO


Print ''
Print 'Verifica divergências em estoques solicitados em PRLOT x (PDV+AVARI+RESER+ORC):'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN
  
  -- verifica lotes solicitados em PRLOT
  if Exists(Select lt.Cod_Produt, lt.Cod_Lote, lt.Qtd_Solicitado, 
                   (IsNull(a.Qtd_Sol,0)+IsNull(b.Qtd_Sol,0)+IsNull(c.Qtd_Sol,0)+IsNull(d.Qtd_Sol,0)+IsNull(e.Qtd_Sol,0)) as Qtd_SolTot
            From (Select Cod_Estabe, Cod_Produt, Cod_Lote, Qtd_Solicitado=SUM(IsNull(Qtd_Solicitado,0))
                  From PRLOT
                  Where Cod_Estabe = @CodEstabe
                  Group by Cod_Estabe, Cod_Produt, Cod_Lote) lt Left Outer Join
  
                 (Select it.Cod_Produto, it.Cod_Lote,
                         sum(Isnull(it.Qtd_Pra,0))+sum(Isnull(it.Qtd_Dep,0)) as Qtd_Sol
                  From PDVCB cb Inner Join 
                       PDVIT it on (cb.Cod_Estabe = it.Cod_Estabe And cb.Numero = it.Cod_Pedido)
                  Where cb.Cod_Estabe = @CodEstabe
                  and cb.Tip_Pedido = 'P'
                  and IsNull(cb.Flg_WMS,0) = 0
                  and cb.Status1 = 'P'
                  and cb.Status2 <> 'N'
                  and cb.Status2 <> 'D'
                  and IsNull(cb.Bloqueio,'') <> 'SR'
                  and (IsNull(it.Qtd_Pedido,0)+IsNull(it.Qtd_Bonificacao,0)) > 0
                  Group by it.Cod_Produto, it.Cod_Lote) a ON (lt.Cod_Produt = a.Cod_Produto and lt.Cod_Lote = a.Cod_Lote) Left Outer Join
                  
                 (Select it.Cod_Produt, it.Cod_Lote,
                         sum(Isnull(it.Qtd_LotePra,0))+sum(Isnull(it.Qtd_LoteDep,0)) as Qtd_Sol
                  From PDVCB cb Inner Join
                       PDVLT it on (cb.Cod_Estabe = it.Cod_Estabe And cb.Numero = it.Cod_Pedido)
                  Where cb.Cod_Estabe = @CodEstabe
                  and cb.Tip_Pedido = 'P'
                  and IsNull(cb.Flg_WMS,0) = 1
                  and cb.Status1 = 'P'
                  and cb.Status2 <> 'N'
                  and cb.Status2 <> 'D'
                  and IsNull(cb.Bloqueio,'') <> 'SR'
                  and IsNull(it.Qtd_Lote,0) > 0
                  and it.Qtd_CxaFec = 0
                  Group by it.Cod_Produt, it.Cod_Lote) b ON (lt.Cod_Produt = b.Cod_Produt and lt.Cod_Lote = b.Cod_Lote) Left Outer Join
                  
                 (Select Cod_Produto, Cod_Lote,                     
                         sum(Isnull(Qtd_Movimento,0))+sum(Isnull(Qtd_AvaDep,0)) as Qtd_Sol
                  From AVARI
                  Where Cod_Estabe = @CodEstabe
                  Group by Cod_Produto, Cod_Lote) c ON (lt.Cod_Produt = c.Cod_Produto and lt.Cod_Lote = c.Cod_Lote) Left Outer Join
  
                 (Select Cod_Produto, Cod_Lote,                     
                         sum(Isnull(Qtd_Movimento,0))+sum(Isnull(Qtd_ResDep,0)) as Qtd_Sol
                  From RESER
                  Where Cod_Estabe = @CodEstabe
                  Group by Cod_Produto, Cod_Lote) d ON (lt.Cod_Produt = d.Cod_Produto and lt.Cod_Lote = d.Cod_Lote) Left Outer Join
  
                 (Select it.Cod_Produt, it.Cod_Lote,
                         sum(Isnull(it.Qtd_Produt,0)) as Qtd_Sol
                  From ORCCB cb Inner Join
                       ORCIT it on (cb.Cod_Estabe = it.Cod_Estabe and cb.Cod_Orcame = it.Cod_Orcame)
                  Where cb.Cod_Estabe = @CodEstabe
                  And cb.Sta_Movime = 'F'
                  and IsNull(cb.Num_Docume,0) = 0
                  and it.Tip_EntSai = 'S'
                  And IsNull(cb.Cod_OriVen,'') <> 'DMD'
                  and (IsNull(it.Qtd_Produt,0)) > 0
                  Group by it.Cod_Produt, it.Cod_Lote) e ON (lt.Cod_Produt = e.Cod_Produt and lt.Cod_Lote = e.Cod_Lote) 
                  
            Where lt.Cod_Estabe = @CodEstabe      
            And lt.Qtd_Solicitado <> (IsNull(a.Qtd_Sol,0)+IsNull(b.Qtd_Sol,0)+IsNull(c.Qtd_Sol,0)+IsNull(d.Qtd_Sol,0)+IsNull(e.Qtd_Sol,0)))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE QUANTIDADES SOLICITADAS: PRLOT x (PDV+AVARI+RESER+ORC)'
      Select lt.Cod_Estabe,
	         lt.Cod_Produt as 'Produto', 
             lt.Cod_Lote as 'Lote', 
             lt.Qtd_Solicitado as 'Qtd.Solicitada em PRLOT', 
             (IsNull(a.Qtd_Sol,0)+IsNull(b.Qtd_Sol,0)+IsNull(c.Qtd_Sol,0)+IsNull(d.Qtd_Sol,0)+IsNull(e.Qtd_Sol,0)) as 'Qtd.Solicida em (PDV+RESER+AVARI+ORC)'
      From (Select Cod_Estabe, Cod_Produt, Cod_Lote, Qtd_Solicitado=SUM(IsNull(Qtd_Solicitado,0))
            From PRLOT
            Where Cod_Estabe = @CodEstabe
            Group by Cod_Estabe, Cod_Produt, Cod_Lote) lt Left Outer Join
  
           (Select it.Cod_Produto, it.Cod_Lote,
                   sum(Isnull(it.Qtd_Pra,0))+sum(Isnull(it.Qtd_Dep,0)) as Qtd_Sol
            From PDVCB cb Inner Join 
                 PDVIT it on (cb.Cod_Estabe = it.Cod_Estabe and cb.Numero = it.Cod_Pedido)
            Where cb.Cod_Estabe = @CodEstabe
            and cb.Tip_Pedido = 'P'
            and IsNull(cb.Flg_WMS,0) = 0
            and cb.Status1 = 'P'
            and cb.Status2 <> 'N'
            and cb.Status2 <> 'D'
            and IsNull(cb.Bloqueio,'') <> 'SR'
            and (IsNull(it.Qtd_Pedido,0)+IsNull(it.Qtd_Bonificacao,0)) > 0
            Group by it.Cod_Produto, it.Cod_Lote) a ON (lt.Cod_Produt = a.Cod_Produto and lt.Cod_Lote = a.Cod_Lote) Left Outer Join
      
           (Select it.Cod_Produt, it.Cod_Lote,
                   sum(Isnull(it.Qtd_LotePra,0))+sum(Isnull(it.Qtd_LoteDep,0)) as Qtd_Sol
            From PDVCB cb Inner Join
                 PDVLT it on (cb.Cod_Estabe = it.Cod_Estabe and cb.Numero = it.Cod_Pedido)
            Where cb.Cod_Estabe = @CodEstabe
            and cb.Tip_Pedido = 'P'
            and IsNull(cb.Flg_WMS,0) = 1
            and cb.Status1 = 'P'
            and cb.Status2 <> 'N'
            and cb.Status2 <> 'D'
            and IsNull(cb.Bloqueio,'') <> 'SR'
            and IsNull(it.Qtd_Lote,0) > 0
            and it.Qtd_CxaFec = 0
            Group by it.Cod_Produt, it.Cod_Lote) b ON (lt.Cod_Produt = b.Cod_Produt and lt.Cod_Lote = b.Cod_Lote) Left Outer Join
            
           (Select Cod_Produto, Cod_Lote,                     
                   sum(Isnull(Qtd_Movimento,0))+sum(Isnull(Qtd_AvaDep,0)) as Qtd_Sol
            From AVARI
            Where Cod_Estabe = @CodEstabe
            Group by Cod_Produto, Cod_Lote) c ON (lt.Cod_Produt = c.Cod_Produto and lt.Cod_Lote = c.Cod_Lote) Left Outer Join
       
           (Select Cod_Produto, Cod_Lote,                     
                   sum(Isnull(Qtd_Movimento,0))+sum(Isnull(Qtd_ResDep,0)) as Qtd_Sol
            From RESER
            Where Cod_Estabe = @CodEstabe
            Group by Cod_Produto, Cod_Lote) d ON (lt.Cod_Produt = d.Cod_Produto and lt.Cod_Lote = d.Cod_Lote) Left Outer Join
                       
           (Select it.Cod_Produt, it.Cod_Lote,
                   sum(Isnull(it.Qtd_Produt,0)) as Qtd_Sol
            From ORCCB cb Inner Join
                 ORCIT it on (cb.Cod_Estabe = it.Cod_Estabe and cb.Cod_Orcame = it.Cod_Orcame)
            Where cb.Cod_Estabe = @CodEstabe
            And cb.Sta_Movime = 'F'
            and IsNull(cb.Num_Docume,0) = 0
            and it.Tip_EntSai = 'S'
            And IsNull(cb.Cod_OriVen,'') <> 'DMD'
            and (IsNull(it.Qtd_Produt,0)) > 0
            Group by it.Cod_Produt, it.Cod_Lote) e ON (lt.Cod_Produt = e.Cod_Produt and lt.Cod_Lote = e.Cod_Lote) 
                       
      Where lt.Cod_Estabe = @CodEstabe      
      And lt.Qtd_Solicitado <> (IsNull(a.Qtd_Sol,0)+IsNull(b.Qtd_Sol,0)+IsNull(c.Qtd_Sol,0)+IsNull(d.Qtd_Sol,0)+IsNull(e.Qtd_Sol,0))
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Solicitado em PRLOT x (PDV+AVARI+RESER+ORC): ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO


Print ''
Print 'Verifica divergências de quantidades solicitadas: PRLTL x PDVLT :'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN
  
  -- verifica lotes solicitados em PRLTL
  if Exists(Select lt.Cod_Produt, lt.Cod_Lote, sum(lt.Qtd_Solicitado), x.Qtd_SolTotPdv
            From PRLTL lt 
            Inner Join (Select it.Cod_Produt, it.Cod_Lote,
                              sum(Isnull(it.Qtd_LotePra,0))+sum(Isnull(it.Qtd_LoteDep,0)) as Qtd_SolTotPdv
                       From PDVCB cb, PDVLT it
                       Where cb.Cod_Estabe = it.Cod_Estabe 
                       and cb.Numero = it.Cod_Pedido
                       and cb.Cod_Estabe = @CodEstabe
                       and cb.Tip_Pedido = 'P'
                       and IsNull(cb.Flg_WMS,0) = 1
                       and cb.Status1 = 'P'
                       and cb.Status2 <> 'N'
                       and cb.Status2 <> 'D'
                       and IsNull(cb.Bloqueio,'') <> 'SR'
                       and IsNull(it.Qtd_Lote,0) > 0
                       and it.Qtd_CxaFec > 0
                       Group by it.Cod_Produt, it.Cod_Lote) x on (lt.Cod_Produt = x.Cod_Produt and lt.Cod_Lote = x.Cod_lote)
            Where lt.Cod_Estabe = @CodEstabe
            Group by lt.Cod_Produt, lt.Cod_Lote, x.Qtd_SolTotPdv
            Having sum(lt.Qtd_Solicitado) <> IsNull(x.Qtd_SolTotPdv,0))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE QUANTIDADES SOLICITADAS: PRLTL x PDVLT'

      Select lt.Cod_Estabe,
	         lt.Cod_Produt as 'Produto', lt.Cod_Lote as 'Lote', sum(lt.Qtd_Solicitado) as 'Qtd.Solicitada em PRLTL', 
             IsNull(x.Qtd_SolTotPdv,0) as 'Qtd.Solicida em PDVCB'
      From PRLTL lt 
      Inner Join (Select it.Cod_Produt, it.Cod_Lote,
                         sum(Isnull(it.Qtd_LotePra,0))+sum(Isnull(it.Qtd_LoteDep,0)) as Qtd_SolTotPdv
                 From PDVCB cb, PDVLT it
                 Where cb.Cod_Estabe = it.Cod_Estabe 
                 and cb.Numero = it.Cod_Pedido
                 and cb.Cod_Estabe = @CodEstabe
                 and cb.Tip_Pedido = 'P'
                 and IsNull(cb.Flg_WMS,0) = 1
                 and cb.Status1 = 'P'
                 and cb.Status2 <> 'N'
                 and cb.Status2 <> 'D'
                 and IsNull(cb.Bloqueio,'') <> 'SR'
                 and IsNull(it.Qtd_Lote,0) > 0
                 and it.Qtd_CxaFec > 0
                 Group by it.Cod_Produt, it.Cod_Lote) x on (lt.Cod_Produt = x.Cod_Produt and lt.Cod_Lote = x.Cod_lote)
      Where lt.Cod_Estabe = @CodEstabe           
      Group by lt.Cod_Estabe, lt.Cod_Produt, lt.Cod_Lote, x.Qtd_SolTotPdv
      Having sum(lt.Qtd_Solicitado) <> IsNull(x.Qtd_SolTotPdv,0)
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoques Solicitado em PRLTL x PDVLT: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO



Print ''
Print 'Verifica slados negativos em PRLOT:'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN

  -- verifica saldo negativo em PRLOT
  if Exists(Select Cod_Produt From PRLOT Where Cod_Estabe = @CodEstabe And ((Qtd_SldPra < 0) or (Qtd_SldDep < 0)))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem SALDOS NEGATIVOS EM PRLOT'
      Select Cod_Estabe,
	         Cod_Produt as 'Prd. c/Saldo Negativo (PRLOT)', 
             Cod_Lote,
             Qtd_SldPra as 'Saldo 1', 
             Qtd_SldDep as 'Saldo 2'
      From PRLOT
      Where Cod_Estabe = @CodEstabe 
      And ((Qtd_SLdPra < 0) or (Qtd_SldDep < 0))
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Saldos Negativos em PRLOT: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO


Print ''
Print 'Verifica saldos negativos em PRLTL:'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN
  
  -- verifica saldo negativo em PRLTL
  if Exists(Select Cod_Produt From PRLTL Where Cod_Estabe = @CodEstabe And ((Qtd_SldPra < 0) or (Qtd_SldDep < 0)))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem SALDOS NEGATIVOS EM PRLTL'
      Select Cod_Estabe,
	         Cod_Produt as 'Prd. c/Saldo Negativo (PRLTL)', 
             Cod_Lote,
             Qtd_SldPra as 'Saldo 1', 
             Qtd_SldDep as 'Saldo 2'
      From PRLTL
      Where Cod_Estabe = @CodEstabe 
      And ((Qtd_SLdPra < 0) or (Qtd_SldDep < 0))
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Saldos Negativos em PRLTL: ok'

  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO

Print ''
Print 'Verifica divergências de estoques: PRXES x PRLOT+PRLTL :'
GO
BEGIN

Declare @CodEstabe int,
        @CodDep int

Set @CodDep = IsNull((Select top 1 Cod_Dep From TBDEP Order by Cod_Dep),0)

DECLARE Estab_Cr CURSOR Local Fast_Forward For 
  Select Cod_Estabe
  From #ESTABE
  Order by Cod_Estabe
OPEN Estab_Cr
FETCH NEXT FROM Estab_Cr INTO @CodEstabe
WHILE @@FETCH_STATUS = 0
BEGIN

  -- verifica qtd.disponivel de PRXES com saldo em PRLOT
  if Exists(Select p.Cod_Produt 
              From PRXES p Left Outer Join 
                   PRLOT l on p.Cod_Estabe = l.Cod_Estabe and p.Cod_Produt = l.Cod_Produt Left Outer Join 
                   PRLTL d on p.Cod_Estabe = d.Cod_Estabe and p.Cod_Produt = d.Cod_Produt
            Where p.Cod_Estabe = @CodEstabe
            and p.Qtd_Dispon > 0
            And l.Cod_Produt is Null And d.Cod_Produt is Null)
    Select p.Cod_Produt as 'Prd.s/Lotes disponíveis', p.Qtd_Dispon as 'Disponível em PRXES'
      From PRXES p Left Outer Join 
           PRLOT l on p.Cod_Estabe = l.Cod_Estabe and p.Cod_Produt = l.Cod_Produt Left Outer Join 
           PRLTL d on p.Cod_Estabe = d.Cod_Estabe and p.Cod_Produt = d.Cod_Produt
      Where p.Cod_Estabe = @CodEstabe
      and p.Qtd_Dispon > 0
      And l.Cod_Produt is Null And d.Cod_Produt is Null

/*-- aqui remover    
  if Exists(Select p.Cod_Produt
              From PRXES p 
                   Left Outer Join
                   (Select Cod_Produt, sum(Qtd_Saldo) as QtdSldFra, sum(Qtd_Solicitado) as QtdSolFra
                      From PRLOT 
                      Where Cod_Estabe = @CodEstabe
                      and ((Qtd_Saldo > 0) or (Qtd_Solicitado > 0))
                      Group by Cod_Produt) l on (p.Cod_Produt = l.Cod_Produt)
                   Left Outer Join
                   (Select Cod_Produt, sum(Qtd_Saldo) as QtdSldDep, sum(Qtd_Solicitado) as QtdSolDep
                      From PRLTL
                      Where Cod_Estabe = @CodEstabe
                      and ((Qtd_Saldo > 0) or (Qtd_Solicitado > 0))
                      Group by Cod_Produt) d on (p.Cod_Produt = d.Cod_Produt)     
            Where p.Cod_Estabe = @CodEstabe 
            and ( (p.Qtd_Dispon+(p.Qtd_Solici+p.Qtd_Reserv+p.Qtd_Avaria) - (IsNull(QtdSolFra,0)+IsNull(QtdSolDep,0)) <> IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0)) or
                  (p.Qtd_Dispon <> IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0)) ))
    begin
      PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': ===> Existem DIVERGENCIAS DE ESTOQUES: PRXES x PRLOT+PRLTL'
      Select p.Cod_Estabe,
	         p.Cod_Produt as 'Prd.c/divergência (PRXES x PRLOT+PRLTL)', p.Qtd_Dispon as 'Disponível em PRXES', 
             IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0) as 'Disponível em PRLOT+PRLTL',
             (p.Qtd_Solici+p.Qtd_Reserv+p.Qtd_Avaria-IsNull(QtdSolFra,0)-IsNull(QtdSolDep,0)) as 'Lotes nao solicitados (PRXES X PRLTL+PRLOT)',
             Diferença = Case
			               When (p.Qtd_Dispon <> IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0)) then p.Qtd_Dispon - (IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0))
						   Else              
			                 IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0)-(p.Qtd_Dispon+p.Qtd_Solici+p.Qtd_Reserv+p.Qtd_Avaria-IsNull(QtdSolFra,0)-IsNull(QtdSolDep,0)) 
                           End
        From PRXES p 
             Left Outer Join
            (Select Cod_Produt, sum(Qtd_Saldo) as QtdSldFra, sum(Qtd_Solicitado) as QtdSolFra
               From PRLOT 
               Where Cod_Estabe = @CodEstabe 
               and ((Qtd_Saldo > 0) or (Qtd_Solicitado > 0))
               Group by Cod_Produt) l on (p.Cod_Produt = l.Cod_Produt)
             Left Outer Join
            (Select Cod_Produt, sum(Qtd_Saldo) as QtdSldDep, sum(Qtd_Solicitado) as QtdSolDep
               From PRLTL
               Where Cod_Estabe = @CodEstabe 
               and ((Qtd_Saldo > 0) or (Qtd_Solicitado > 0))
               Group by Cod_Produt) d on (p.Cod_Produt = d.Cod_Produt)
        Where p.Cod_Estabe = @CodEstabe 
--		and p.Qtd_Dispon+(p.Qtd_Solici+p.Qtd_Reserv+p.Qtd_Avaria-IsNull(QtdSolFra,0)-IsNull(QtdSolDep,0)) <> IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0)
        and ( (p.Qtd_Dispon+(p.Qtd_Solici+p.Qtd_Reserv+p.Qtd_Avaria) - (IsNull(QtdSolFra,0)+IsNull(QtdSolDep,0)) <> IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0)) or
		      (p.Qtd_Dispon <> IsNull(l.QtdSldFra,0)+IsNull(d.QtdSldDep,0)) )
        Order by p.Cod_Produt
    end
  else
    PRINT '  Estabelecimento '+cast(@CodEstabe as varchar)+': Estoque Disponível: PRXES x PRLOT+PRLTL ok!'
*/
  FETCH NEXT FROM Estab_Cr INTO @CodEstabe
END
CLOSE Estab_Cr
DEALLOCATE Estab_Cr
END
GO




IF OBJECT_ID('tempdb..#ESTABE') IS NOT NULL
  DROP TABLE #ESTABE
GO

PRINT ''
PRINT 'FIM DE VERIFICACAO ...'
SET ANSI_WARNINGS ON
GO





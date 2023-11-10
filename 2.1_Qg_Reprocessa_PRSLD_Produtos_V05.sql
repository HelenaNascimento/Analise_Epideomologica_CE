BEGIN TRANSACTION

SET NOCOUNT ON

--***************************************************************
--***  ROTINA PARA REFAZER TODOS OS SALDOS EM PRSLD           ***
--***  DE UM ESTABELECIMENTO A PARTIR DE UMA DATA             ***
--***************************************************************

Set DateFormat ymd

Declare @DatIni smalldatetime,
        @CodEstabe int,
        @DesRazSoc varchar(40)

--====================================================================================
-- INFORME ESTABELECIMENTO e DATA INCIAL DO PROCESSAMENTO:
-- registros da PRSLD a partir da data (INCLUSIVE) serão eliminadoes e reprocessados
--====================================================================================
Set @DatIni = cast('2019-01-01 00:00:00'as smalldatetime)   
--====================================================================================

--====================================================================================
-- INFORME ESTABELECIMENTO --
--====================================================================================
Set @CodEstabe = 1
--====================================================================================

--====================================================================================
-- SELECIONA PRODUTOS --
--====================================================================================
  Update PRXES
     Set Qtd_Fisico = -987654321
   Where Cod_Estabe = @CodEstabe
     And Cod_Produt in (2219)	  
						---- INFORME AQUI OS PRODUTOS PARA REPROCESSAR 
--====================================================================================


Declare @TipArq char(1),
        @TipNot char(1),
        @FlgMovEst bit,
        @CodCfo int,
        @NumSeqBal int,
        @CodOriMov varchar(2),
        @StaNot char(1),
        @CodPrd int,
        @CodLot varchar(12),
        @CodMov int,
        @DatMov smalldatetime,
        @DatTra smalldatetime,
        @EntSai char(1),
        @QtdPed int,
        @QtdBon int,
        @QtdMovPra int,
        @QtdMovDep int,
        @FlgEmpPra bit,
        @FlgEmpDep bit,
        @QtdSldAntPra int,
        @QtdSldAntDep int,
		@QtdMovAux int,
        @QtdEst int,
        @QtdAcr int,
        @SerDoc varchar(3),
        @NumDoc int,
        @DesHisAce varchar(40),
        @DesUsuAce varchar(15),
        @QtdReg int


Select @DesRazSoc = Des_RazSoc From ESTAB Where Cod_Estabe = @CodEstabe
Print 'Estabelecimento: '+cast(@CodEstabe as varchar)+' - '+@DesRazSoc
Print 'Reprocessamento a partir de : '+CONVERT(VARCHAR(10), @DatIni, 103)
Print ''


-- ajusta acertos com cod. de movimentacao invalido
Update ACERT 
   Set Cod_TipMov = 12
 Where Cod_TipMov not in (Select Codigo From TPMVE)
   And Cod_Estabe = @CodEstabe
   And Dat_Movimento >= @DatIni

-- normaliza quantidades em NFSIT
Update i
   Set i.Qtd_Pra = i.Qtd_Produto+i.Qtd_Bonificacao-i.Qtd_Dep
  From NFSCB c 
       INNER JOIN NFSIT i ON ((c.Cod_Estabe = i.Cod_Estabe) and (c.Ser_Nota = i.Ser_Nota) and (c.Num_Nota = i.Num_Nota))
  Where c.Cod_Estabe = @CodEstabe 
    And c.Dat_Emissao >= @DatIni
    And c.Num_Nota > 0
    And c.Cod_Cfo1 > 0
    And ((c.Status = 'F') or (c.Status = 'C'))
	And (i.Qtd_Produto+i.Qtd_Bonificacao) <> (i.Qtd_Pra+i.Qtd_Dep)

Update i
   Set i.Qtd_Dep = (i.Qtd_Produto+i.Qtd_Bonificacao)-(i.Qtd_Pra+i.Qtd_Dep)
  From NFSCB c 
       INNER JOIN NFSIT i ON ((c.Cod_Estabe = i.Cod_Estabe) and (c.Ser_Nota = i.Ser_Nota) and (c.Num_Nota = i.Num_Nota))
  Where c.Cod_Estabe = @CodEstabe
    And c.Dat_Emissao >= @DatIni
    And c.Num_Nota > 0
    And c.Cod_Cfo1 = 0
    And ((c.Status = 'F') or (c.Status = 'C'))
	And (i.Qtd_Produto+i.Qtd_Bonificacao) <> (i.Qtd_Pra+i.Qtd_Dep)


if (Exists(Select p.Cod_Produt 
             From PRXES p, PRSLD s
            Where p.Cod_Estabe = s.Cod_Estabe
              and p.Cod_Produt = s.Cod_Produt
              and s.Dat_Movime = (Select max(x.Dat_Movime) 
                                    From PRSLD x 
                                   Where p.Cod_Estabe = x.Cod_Estabe
                                     And p.Cod_Produt = x.Cod_Produt)
              and p.Cod_Estabe = @CodEstabe
              and p.Qtd_Fisico <> s.Qtd_SldAtu)) or

    (Exists(Select Cod_Produt From PRXES
	         Where Cod_Estabe = @CodEstabe
               And Qtd_Fisico <> 0 
               And Cod_produt not in (Select distinct Cod_Produt From PRSLD Where Cod_Estabe = @CodEstabe)))
  begin
    -- parametros utilizados no acerto de estoque
    Set @DesHisAce = '** QG AJUSTE ESTOQUE NEGATIVO **'
    Set @DesUsuAce = 'SISTEMA'

    Set @QtdReg = 0
    DECLARE Table_Cr CURSOR LOCAL FAST_FORWARD FOR 

    Select p.Cod_Produt
      From PRXES p, PRSLD s
     Where p.Cod_Estabe = s.Cod_Estabe
       and p.Cod_Produt = s.Cod_Produt
       and s.Dat_Movime = (Select max(x.Dat_Movime) 
                             From PRSLD x 
		 					Where p.Cod_Estabe = x.Cod_Estabe
                              And p.Cod_Produt = x.Cod_Produt)
       and p.Cod_Estabe = @CodEstabe
       and p.Qtd_Fisico <> s.Qtd_SldAtu
    Union
    Select Cod_Produt 
      From PRXES
     Where Cod_Estabe = @CodEstabe
	   And Qtd_Fisico <> 0 
       And Cod_Produt not in (Select distinct Cod_Produt From PRSLD Where Cod_Estabe = @CodEstabe)
     Order by Cod_Produt

    OPEN Table_Cr
    FETCH NEXT FROM Table_Cr INTO @CodPrd
    WHILE @@FETCH_STATUS = 0
    BEGIN
      Set @QtdReg = @QtdReg + 1
      PRINT '('+cast(@QtdReg as varchar)+ ') ====> Reprocessando Produto: '+cast(@CodPrd as varchar)

      -- deletar todas os registros em PRSLD a partir da data
      Delete From PRSLD 
       Where Cod_Estabe = @CodEstabe 
         And Cod_Produt = @CodPrd   
         And Dat_Movime >= @DatIni

      -- elimina acertos realizados por este qg anteriormente
      Delete From ACERT
	   Where Cod_Estabe = @CodEstabe
         And Historico = @DesHisAce
         And Usuario = @DesUsuAce
         And Dat_Movimento >= @DatIni


      DECLARE Movime_Cr CURSOR LOCAL FAST_FORWARD FOR

      Select 'A' as Tip_Arq, 'O' as Tip_Not, 0 as Cod_Cfo, 1 as Flg_MovEst, 'F' as Status, Cod_TipMov, Dat_Movimento as Dat_Arquivo, Transacao as Dat_Trans, 
             Cod_Lote, 0, 0, Isnull(Qtd_Acerto,0), Isnull(Qtd_AceDep,0),
             'ACE', Numero, 0, IsNull(Cod_OriMov,'')
        From ACERT
       Where Cod_Estabe = @CodEstabe 
         And Cod_Produto = @CodPrd
         And Numero > 0
         And Dat_Movimento >= @DatIni
      UNION
      Select 'E' as Tip_Arq, c.Tip_Nf, c.Cod_Cfo, c.Flg_MovEst, c.Status, 0, c.Dat_Movimento as Dat_Arquivo, i.Transacao as Dat_Trans, 
             i.Cod_Lote, sum(IsNull(i.Qtd_Pedido,0)), sum(IsNull(i.Qtd_Bonificacao,0)), 0, 0,
             'PRT', i.Protocolo, IsNull(c.Num_SeqBal,0), 'NF'
        From NFECB c 
		     INNER JOIN NFEIT i ON (c.Cod_Estabe = i.Cod_Estabe and c.Protocolo = i.Protocolo)
       Where c.Cod_Estabe = @CodEstabe 
         And i.Cod_Produto = @CodPrd
         And c.Protocolo > 0
         And c.Status = 'F'
         And c.Dat_Movimento >= @DatIni
       Group by i.Cod_Lote, c.Tip_Nf, c.Cod_Cfo, c.Flg_MovEst, c.Status, c.Dat_Movimento, i.Transacao, i.Protocolo, c.Num_SeqBal
      UNION
      Select 'S' as Tip_Arq, c.Tip_Saida, c.Cod_Cfo1, c.Flg_MovEst, c.Status, 0, c.Dat_Emissao as Dat_Arquivo, i.Transacao as Dat_Trans, 
             i.Cod_Lote, sum(IsNull(i.Qtd_Produto,0)), sum(IsNull(i.Qtd_Bonificacao,0)), sum(IsNull(i.Qtd_Pra,0)), sum(IsNull(i.Qtd_Dep,0)),
             i.Ser_Nota, i.Num_Nota, IsNull(c.Num_SeqBal,0), 'NF'
        From NFSCB c 
		     INNER JOIN NFSIT i ON ((c.Cod_Estabe = i.Cod_Estabe) and (c.Ser_Nota = i.Ser_Nota) and (c.Num_Nota = i.Num_Nota))
        Where c.Cod_Estabe = @CodEstabe 
		And i.Cod_Produto = @CodPrd
        And c.Num_Nota > 0
        And ((c.Status = 'F') or (c.Status = 'C'))
        And c.Dat_Emissao >= @DatIni
		Group by i.Cod_Lote, c.Tip_Saida, c.Cod_Cfo1, c.Flg_MovEst, c.Status, c.Dat_Emissao, i.Transacao, i.Ser_Nota, i.Num_Nota, c.Num_SeqBal

      Order by Dat_Arquivo, Dat_Trans, Tip_Arq

      OPEN Movime_Cr
      FETCH NEXT FROM Movime_Cr INTO @TipArq, @TipNot, @CodCfo, @FlgMovEst, @StaNot, @CodMov, @DatMov, @DatTra, 
                                     @CodLot, @QtdPed, @QtdBon, @QtdMovPra, @QtdMovDep, @SerDoc, @NumDoc, @NumSeqBal, @CodOriMov
      WHILE @@FETCH_STATUS = 0
      BEGIN
        if @TipArq = 'A'
          begin
            -- configura flag de emprestimo de estoque
            -- arranjo para poder extornar em PRSLD o estoque correto qdo do cancelamento de uma NF
            Set @FlgEmpPra = 0
            Set @FlgEmpDep = 0
            if @QtdMovPra < 0
              begin
                Set @QtdMovPra = @QtdMovPra * (-1)
                Set @FlgEmpPra = 1
               end
            if @QtdMovDep < 0
              begin
                Set @QtdMovDep = @QtdMovDep * (-1)
                Set @FlgEmpDep = 1
              end

            -- verifica se é E / S
            Select @EntSai = Tip_Movimento From TPMVE
             Where Codigo = @CodMov

            if @EntSai = 'S'
              begin
                Set @QtdMovPra = @QtdMovPra * (-1)
                Set @QtdMovDep = @QtdMovDep * (-1)
              end

            -- acertos
            EXEC PR_AtualizaSaldoPRSLD @CodOriMov, @CodEstabe, @CodPrd, @DatMov, @EntSai, @TipNot, @QtdMovPra, @FlgEmpPra, @QtdMovDep, @FlgEmpDep
            if (@@error <> 0)
              begin
                Rollback Transaction
                Return
              end
          end

        else
        if @TipArq = 'E'
          begin
            if @FlgMovEst = 1
              begin
                Set @QtdMovPra = 0
                Set @QtdMovDep = 0
                if @CodCfo > 0
                  Set @QtdMovPra = @QtdPed + @QtdBon
                else
                  Set @QtdMovDep = @QtdPed + @QtdBon

                -- nf entrada 
                Set @EntSai = 'E'
                Exec PR_AtualizaSaldoPRSLD 'NF', @CodEstabe, @CodPrd, @DatMov, @EntSai, @TipNot, @QtdMovPra, 0, @QtdMovDep, 0
                if @@error <> 0
                  begin
                    Rollback Transaction
                    Return
                  end
              end
            else
              begin
                -- transferencia de saldo de estoque em PRSLD
                if (@CodCfo > 0) and (@StaNot = 'F')
                  begin
--------------- transfere estoque oficial
                    -- pega o ultimo movimento do produto
                    Set @QtdSldAntPra = 0
                    Set @QtdSldAntDep = 0
                
                    Select top 1 @QtdSldAntPra = Qtd_SldPra, 
                                 @QtdSldAntDep = Qtd_SldDep
                      From PRSLD 
                    Where Cod_Estabe = @CodEstabe
                    and Cod_Produt = @CodPrd 
                    and Dat_Movime <= @DatMov
                    Order By Dat_Movime desc
                
                    -- ver se inclui uma nova linha em PRSLD
                    if not Exists(Select Cod_Produt from PRSLD Where Cod_Estabe = @CodEstabe and Cod_Produt = @CodPrd and Dat_Movime = @DatMov)
                      Insert into PRSLD (Cod_Estabe,Cod_Produt,Dat_Movime,Qtd_SldPra,Qtd_SldDep) 
                                 Values (@CodEstabe,@CodPrd,@DatMov,Isnull(@QtdSldAntPra,0),Isnull(@QtdSldAntDep,0)) 
                
                    -- remaneja saldos
                    Set @QtdMovAux = @QtdPed + @QtdBon
                    if @StaNot = 'F'
                      begin
                        -- poe em PRA
                        Update PRSLD 
                          Set  Qtd_SldPra = Qtd_SldPra + @QtdMovAux
                        Where Cod_Estabe = @CodEstabe
                        and Cod_Produt = @CodPrd
                        and Dat_Movime >= @DatMov
 
                        -- tira de DEP
                        Update PRSLD 
                          Set  Qtd_SldDep = Qtd_SldDep - @QtdMovAux
                        Where Cod_Estabe = @CodEstabe 
                        and Cod_Produt = @CodPrd
                        and Dat_Movime >= @DatMov
                      end
                
                    if @StaNot = 'C'
                      begin
                        -- poe em  DEP
                        Update PRSLD 
                          Set  Qtd_SldDep = Qtd_SldDep + @QtdMovAux
                        Where Cod_Estabe = @CodEstabe
                        and Cod_Produt = @CodPrd
                        and Dat_Movime >= @DatMov

                        -- tira de PRA
                        Update PRSLD 
                          Set  Qtd_SldPra = Qtd_SldPra - @QtdMovAux
                        Where Cod_Estabe = @CodEstabe
                        and Cod_Produt = @CodPrd
                        and Dat_Movime >= @DatMov
                      end
---------------------------------------------
                  end
              end

          end

        else
        if @TipArq = 'S'
          begin
            if @FlgMovEst = 1
              begin
                -- normaliza campos
                if (@QtdMovPra+@QtdMovDep) <> (@QtdPed+@QtdBon)
                  begin
                    Set @QtdMovPra = @QtdPed + @QtdBon - @QtdMovDep
                  end

                -- configura flag de emprestimo de estoque
                Set @FlgEmpPra = 0
                Set @FlgEmpDep = 0
                if (@CodCfo > 0) and (@QtdMovDep > 0)
                  Set @FlgEmpDep = 1 
                if (@CodCfo = 0) and (@QtdMovPra > 0)
                  Set @FlgEmpPra = 1 

                -- atualiza saldo estoque
                Set @QtdMovPra = @QtdMovPra * (-1)       
                Set @QtdMovDep = @QtdMovDep * (-1)       

				-- nf saída
                Set @EntSai = 'S'
                Exec PR_AtualizaSaldoPRSLD 'NF', @CodEstabe, @CodPrd, @DatMov, @EntSai, @TipNot, @QtdMovPra, @FlgEmpPra, @QtdMovDep, @FlgEmpDep
                if @@error <> 0
                  begin
                    Rollback Transaction
                    Return
                  end
              end
            else
              begin
                -- transferencia de saldo de estoque em PRSLD
                if (@CodCfo > 0) and (@StaNot = 'F') and
                   (CharIndex(';'+cast(@CodCfo as varchar)+';',';5114;6114;5922;6922;') = 0  ) 
                  begin
---------------- transfere estoque oficial --------
                    -- pega o ultimo movimento do produto
                    Set @QtdSldAntPra = 0
                    Set @QtdSldAntDep = 0
                    Select top 1 @QtdSldAntPra = Qtd_SldPra, 
                                 @QtdSldAntDep = Qtd_SldDep
                      From PRSLD 
                     Where Cod_Estabe = @CodEstabe
                       and Cod_Produt = @CodPrd 
                       and Dat_Movime <= @DatMov
                     Order By Dat_Movime desc
                   
                    -- ver se inclui uma nova linha em PRSLD
                    if not Exists(Select Cod_Produt from PRSLD Where Cod_Estabe = @CodEstabe and Cod_Produt = @CodPrd and Dat_Movime = @DatMov)
                      Insert into PRSLD (Cod_Estabe, Cod_Produt, Dat_Movime, Qtd_SldPra, Qtd_SldDep) 
                                 Values (@CodEstabe, @CodPrd, @DatMov, Isnull(@QtdSldAntPra,0), Isnull(@QtdSldAntDep,0)) 
                   
                    -- remaneja saldos
                    Set @QtdMovAux = @QtdPed + @QtdBon
                    if @StaNot = 'F'
                      begin
                        -- tira de PRA
                        Update PRSLD 
                           Set Qtd_SldPra = Qtd_SldPra - @QtdMovAux
                         Where Cod_Estabe = @CodEstabe
                           and Cod_Produt = @CodPrd
                           and Dat_Movime >= @DatMov

                        -- repoe em DEP
                        Update PRSLD 
                           Set Qtd_SldDep = Qtd_SldDep + @QtdMovAux
                         Where Cod_Estabe = @CodEstabe
                           and Cod_Produt = @CodPrd
                           and Dat_Movime >= @DatMov
                      end
                   
                    if @StaNot = 'C'
                      begin
                        -- tira de DEP
                        Update PRSLD 
                           Set Qtd_SldDep = Qtd_SldDep - @QtdMovAux
                         Where Cod_Estabe = @CodEstabe
                           and Cod_Produt = @CodPrd
                           and Dat_Movime >= @DatMov

                        -- repoe em PRA
                        Update PRSLD 
                           Set Qtd_SldPra = Qtd_SldPra + @QtdMovAux
                         Where Cod_Estabe = @CodEstabe
                           and Cod_Produt = @CodPrd
                           and Dat_Movime >= @DatMov
                     end
----------------
                  end
              end          

          end

        FETCH NEXT FROM Movime_Cr INTO @TipArq, @TipNot, @CodCfo, @FlgMovEst, @StaNot, @CodMov, @DatMov, @DatTra, 
                                       @CodLot, @QtdPed, @QtdBon, @QtdMovPra, @QtdMovDep, @SerDoc, @NumDoc, @NumSeqBal, @CodOriMov
      END

      CLOSE Movime_Cr
      DEALLOCATE Movime_Cr


      -- atualiza saldo fisico em PRODU
      Update p
         Set p.Qtd_Fisico = IsNull(s.Qtd_SldAtu,0)
        From PRXES p, PRSLD s 
       Where p.Cod_Estabe = @CodEstabe
		 And p.Cod_Estabe = s.Cod_Estabe
		 And p.Cod_Produt = s.Cod_Produt
         And s.Dat_Movime = (Select max(x.Dat_Movime) 
                              From PRSLD x 
                             Where p.Cod_Estabe = x.Cod_Estabe 
							   And p.Cod_Produt = x.Cod_Produt)
        and p.Cod_Produt = @CodPrd
      if @@rowcount = 0
        Update PRXES
           Set Qtd_Fisico = 0
         Where Cod_Estabe = @CodEstabe
		   And Cod_Produt = @CodPrd

      FETCH NEXT FROM Table_Cr INTO @CodPrd
    END

    CLOSE Table_Cr
    DEALLOCATE Table_Cr

  end
 
-- verifica estoque fisico negativo
if Exists(Select Cod_Produt From PRXES Where Cod_Estabe = @CodEstabe And Qtd_Fisico < 0)
  begin
    -- fazer ajuste no 1o. registro da PRSLD com saldo negativo
    DECLARE Table_Cr CURSOR LOCAL FAST_FORWARD FOR 
    SELECT Cod_Produt, Qtd_Fisico
      FROM PRXES
     WHERE Cod_Estabe = @CodEstabe
	   AND Qtd_Fisico < 0
     ORDER by Cod_Produt

    OPEN Table_Cr

    FETCH NEXT FROM Table_Cr INTO @CodPrd, @QtdEst

    WHILE @@FETCH_STATUS = 0
    BEGIN
      PRINT '==> Processando ' + Cast(@CodPrd as Varchar)

      -- inclui acerto na 1a ocorrencia de saldo negativo em PRSLD
      Set @QtdAcr = Abs(@QtdEst)
      Select top 1 @DatMov = Dat_Movime
        From PRSLD
       Where Cod_Estabe = @CodEstabe
         And Cod_Produt = @CodPrd
         And Qtd_SldAtu < 0
       order by Dat_Movime
      if @DatMov is not null
        begin
          Print @DatMov

          EXEC PR_ProxNumero 'Acerto', @NumDoc output
          if @@error <> 0
            begin
              Rollback Transaction
              Return
            end
          Insert into ACERT (Cod_Estabe, Numero, Dat_Movimento, Cod_Produto, Cod_Lote, Qtd_Acerto, Qtd_AceDep, Cod_TipMov, Historico, Usuario, Transacao)
                    Values  (@CodEstabe, @NumDoc, @DatMov, @CodPrd, '*', 0, @QtdAcr, 12, @DesHisAce, @DesUsuAce, @DatMov)
        end

      FETCH NEXT FROM Table_Cr INTO @CodPrd, @QtdEst
    END

    CLOSE Table_Cr
    DEALLOCATE Table_Cr

  end

-- zera qtd.fisica em PRODU de itens sem registro na PRSLD
Update PRXES
   Set Qtd_Fisico = 0
 Where Cod_Estabe = @CodEstabe
   And Cod_Produt not in (Select distinct Cod_Produt From PRSLD Where Cod_Estabe = @CodEstabe)
   And Qtd_Fisico <> 0

-- normaliza PRODU
Update PRXES
   Set Qtd_Dispon = Qtd_Fisico-Qtd_Solici-Qtd_Reserv-Qtd_Avaria
 Where Cod_Estabe = @CodEstabe
   And Qtd_Dispon <> (Qtd_Fisico-Qtd_Solici-Qtd_Reserv-Qtd_Avaria)


PRINT ''
PRINT 'FIM DE PROCESSAMENTO'

COMMIT TRANSACTION
GO



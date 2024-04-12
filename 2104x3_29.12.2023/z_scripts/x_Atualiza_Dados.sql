SET NOCOUNT ON
GO

-- verifica e atualiza SEQUENCE "DMDseq_Pedido_Venda'
IF EXISTS (SELECT compatibility_level FROM sys.databases 
            WHERE name = 'master' AND compatibility_level >= 110)
  if Exists(Select 1 From sys.sequences 
             Where name = 'DMDseq_Pedido_Venda')
  begin
    if IsNull((Select current_value 
                 From sys.sequences 
                Where name = 'DMDSeq_Pedido_Venda'),0) <> IsNull((Select max(Numero) From PDVCB),0)+1
    begin
      Declare @SqlCmd nvarchar(max)
      Select @SqlCmd = N'ALTER SEQUENCE DMDseq_Pedido_Venda ' + '
                         RESTART WITH ' + Cast((IsNull((Select max(Numero) From PDVCB),0)+1) as nvarchar(max)); 
      Execute sp_executesql @SqlCmd=@SqlCmd;
    end
  end
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

Update RGTRI
   Set Flg_DscSbtOrgPub = 0
Where Flg_DscSbtOrgPub is null
GO

Update PARAM 
   Set FlgAtuPolComMkp = 0
 Where FlgAtuPolComMkp IS NULL
 GO


/*   
  Versao TRUNK
  Abril de 2023
  Tabela: ROTIN
  Descricao: Mantendo o acesso da PEXRO dos perfis que tem o acesso do ISN_ROTINA = 373 para ISN_ROTINA = 782
*/         

if (not exists(Select 1 From PEXRO Where Isn_Rotina = 782)) and 
   (exists(Select 1 From ROTIN Where Isn_Rotina = 782)) 
  Insert into PEXRO (Cod_Perfil, Isn_Rotina, Flg_Ativa)
              select Cod_Perfil, 782, Flg_Ativa from PEXRO where Isn_Rotina = 373 --Consultar Rentabilidade Itens do Pedido
GO

-- atualiza novos campos em PVMCB
BEGIN
Declare @IdPVMCB int = 0
Set @IdPVMCB = 0
while Exists(Select 1 
              From PVMCB
             Where Sta_Pedido = 'A'
               And Id_PVMCB > @IdPVMCB)
begin
  Select top 1 @IdPVMCB = Id_PVMCB
    From PVMCB
   Where Sta_Pedido = 'A'
     And Id_PVMCB > @IdPVMCB
  Order by Id_PVMCB
  Exec PR_AtualizaPVMIT @IdPVMCB, 0
  Exec PR_AtualizaPVMCB @IdPVMCB
end
END
GO

-- atualiza novo campo em PDVLT
Update lt
   Set lt.Qtd_Lote = lt.Qtd_Lote+0
  From PDVLT lt
       inner join PDVCB cb on lt.Cod_Estabe = cb.Cod_Estabe and lt.Cod_Pedido = cb.Numero
 Where cb.Tip_Pedido = 'P'
   And cb.Status1 = 'P'
   And cb.Status2 <> 'N'
   And cb.Status2 <> 'D'
GO

-- reprocessa tributos PDVCB
BEGIN TRANSACTION
 Declare @NumPdv int = 0,
         @CodEstabe int
While Exists(Select 1
               From PDVCB cb
                    inner join RGTRI rg on cb.Cod_RegTri = rg.Cod_RegTri
              Where cb.Numero > @NumPDv 
                And cb.Tip_Pedido = 'P'
                And cb.Status1 = 'P'
                And cb.Status2 <> 'N'
                And cb.Status2 <> 'D'
                And rg.Flg_DscSbtOrgPub = 1
                And Exists(Select 1 From PDVLT lt Where lt.Cod_Estabe = cb.Cod_Estabe And lt.Cod_Pedido = cb.Numero and lt.Vlr_SbtRetAnt > 0))
begin
  Select top 1 
         @Codestabe = cb.Cod_Estabe, 
         @NumPdv = cb.Numero
    From PDVCB cb
         inner join RGTRI rg on cb.Cod_RegTri = rg.Cod_RegTri
   Where cb.Numero > @NumPDv 
     And cb.Tip_Pedido = 'P'
     And cb.Status1 = 'P'
     And cb.Status2 <> 'N'
     And cb.Status2 <> 'D'
     And rg.Flg_DscSbtOrgPub = 1
     And Exists(Select 1 From PDVLT lt Where lt.Cod_Estabe = cb.Cod_Estabe And lt.Cod_Pedido = cb.Numero and lt.Vlr_SbtRetAnt > 0)
   Order by Numero

  Exec dbo.PR_CalcTributPDV @CodEstabe, @NumPdv
end
COMMIT TRANSACTION
GO


-- atualiza novo campo: Grupo Acripel
Update pm
   Set pm.FlgQtdPenPedCmpNeg = 1
  From PMEST pm
       inner join ESTAB es on pm.Cod_Estabe = es.Cod_Estabe
 Where pm.FlgQtdPenPedCmpNeg is null
   And Charindex(Substring(es.Num_Cnpj,1,8),';24455677;02000831;02242077;')>0
GO
Update pm
   Set pm.FlgQtdPenPedCmpNeg = 0
  From PMEST pm
 Where pm.FlgQtdPenPedCmpNeg is null
GO

-- Atualizar qtd_pendente se necessário conforme novo parametro PMEST.FlgQtdPenPedCmpNeg
Update it
   Set it.Qtd_Fatura = it.Qtd_Fatura + 0
  From PDCIT it
       inner join PDCCB cb on it.Cod_Estabe = cb.Cod_Estabe and it.Numero = cb.Numero
	   inner join PMEST pm on it.Cod_Estabe = pm.Cod_Estabe
 Where cb.Status <> 'F'
   And it.Qtd_Pendente <> Case 
                            When IsNull(pm.FlgQtdPenPedCmpNeg,0) = 1   -- permite negativar pendência
                              then it.Qtd_Pedido + it.Qtd_Bonificacao - it.Qtd_Fatura - it.Qtd_Falta                                         
	                        When (it.Qtd_Pedido + it.Qtd_Bonificacao) > (it.Qtd_Fatura + it.Qtd_Falta) 
                              then it.Qtd_Pedido + it.Qtd_Bonificacao - it.Qtd_Fatura - it.Qtd_Falta                                         
                            Else 0
                          End
GO

-- atualiza trânsito e pulmão em PRXES
BEGIN
  Declare @CodEstabe int = 1

  Set @CodEstabe = -1
  while Exists(Select 1 
                From ESTAB
               Where Cod_Estabe > @CodEstabe)
  begin

    Select top 1 @CodEstabe = Cod_Estabe
      From ESTAB
     Where Cod_Estabe > @CodEstabe
     Order by Cod_Estabe

    Exec dbo.PR_ReprocessaTransitoProduto @CodEstabe, 0

    Exec dbo.PR_ReprocessaEstoquePulmao @CodEstabe, 0  -- considera bonificação, descpnsidera devolução

  end
END
GO

Update it
   Set it.Flg_Proces = 1
  From ABAIT it
       inner join ABACB cb on it.Cod_Estabe = cb.Cod_Estabe and it.Cod_Abaste = cb.Cod_Abaste
 Where cb.Cod_Sta = 'F'
   And IsNull(it.Flg_Proces,0) = 0
GO


If Not Exists (Select 1 From FINAL Where Cod_Finali = 'CDI')
begin
  INSERT INTO FINAL (Cod_Finali, Des_Finali, Tip_Finali, Flg_Automa, Cod_Estabe) values ('CDI', 'CARTEIRA DIGITAL', 'CD', 1, 0)
end
GO
USE PROD_2023_AZAPFY
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[TR_TB_RETORNO_Integra_AZAPFY]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
 drop trigger [dbo].[TR_TB_RETORNO_Integra_AZAPFY]
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE TRIGGER [dbo].[TR_TB_RETORNO_Integra_AZAPFY] ON [dbo].[tb_retorno] WITH ENCRYPTION 
FOR INSERT,UPDATE
NOT FOR REPLICATION
AS

-- comando p/ corrigir erro delphi/ado: record was changed by another user
SET NOCOUNT ON 

if Update(DATA_RECEBIMENTO) or Update(OCORRENCIA)
  begin

	DECLARE @Cnpj varchar(15),
	        @Serie varchar(3),
	        @CodEstabe int,
	        @Numero int,
			@DataRecebimento datetime

	Select @Cnpj    = x.REMETENTE_CNPJ,
	       @Numero  = x.NUMERO_NOTA,
		   @Serie   = Convert(varchar(3),x.SERIE_NOTA),
		   @DataRecebimento = I.DATA_RECEBIMENTO
	From Inserted I 
	     Inner Join tb_envio x On x.ID_NOTA = i.ID_NOTA

	if @@ROWCOUNT > 0
      begin		
	    Select @CodEstabe = Cod_Estabe From PROD_2023.dbo.ESTAB es Where es.Num_Cnpj = @Cnpj
		Update PROD_2023.dbo.NFSCB 
		  Set Flg_RetEnt = Cast(1 as bit),
			  Dat_RetEnt = @DataRecebimento
		Where Cod_Estabe = @CodEstabe
		  And Ser_Nota   = @Serie 
		  And Num_Nota   = @Numero
          And (Flg_RetEnt = 0 Or Dat_RetEnt is Null)
		if @@ROWCOUNT > 0
			Insert into PROD_2023_AZAPFY.dbo.tb_logint(tx_registro)
			Values('Baixa efetuada: Est['+Convert(varchar(9),@CodEstabe)+'] '+
				   'NFe['+Convert(varchar(9),@Serie)+' / '+Convert(varchar(9),@Numero)+']')
		else
			Insert into PROD_2023_AZAPFY.dbo.tb_logint(tx_registro)
			Values('Baixa n�o efetuada,nota n�o localizada Est['+Convert(varchar(9),@CodEstabe)+'] '+
				   'NFe['+Convert(varchar(9),@Serie)+' / '+Convert(varchar(9),@Numero)+']')
      end
	else
      Insert into PROD_2023_AZAPFY.dbo.tb_logint(tx_registro)
      Values('Documento recebido sem evento de envio: Est['+Convert(varchar(9),@CodEstabe)+'] '+
        	   'NFe['+Convert(varchar(9),@Serie)+' / '+Convert(varchar(9),@Numero)+']')
	  	
  end

GO

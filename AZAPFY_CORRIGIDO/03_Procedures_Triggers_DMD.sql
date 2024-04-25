Use PROD_2023
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[TR_RMNCB_Integra_AZAPFY]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[TR_RMNCB_Integra_AZAPFY]
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE TRIGGER [dbo].[TR_RMNCB_Integra_AZAPFY] ON [dbo].[RMNCB] WITH ENCRYPTION 
FOR INSERT,UPDATE
NOT FOR REPLICATION
AS

-- comando p/ corrigir erro delphi/ado: record was changed by another user
SET NOCOUNT ON 

if Update(Status)
  begin

	DECLARE @CodEstabe int,
	        @Numero int,	
		    @Status varchar(1)

	Select @CodEstabe = x.Cod_Estabe,
	       @Numero  = x.Numero,
		   @Status  = x.Status
	From Inserted I 
	     Inner Join RMNCB x On x.Cod_Estabe = I.Cod_Estabe and x.Numero = I.Numero  

	if @Status = 'F' and Exists(Select 1 From FS_PARAM Where Cod_Estabe = @CodEstabe And Cod_Param = 'AZAPFY_STATUS' And Val_Param = 'ATIVO')
      begin		
		Insert Into DMD_AZAPFY.dbo.tb_envio(
					 CHAVE_NOTA,
					 NUMERO_NOTA,
					 SERIE_NOTA,
					 DATA_NOTA,
					 VALOR_NOTA,
					 REMETENTE_NOME,
					 REMETENTE_CNPJ,
					 DESTINATARIO_NOME,
					 DESTINATARIO_CNPJ,
					 DESTINATARIO_ENDERECO,
					 ID_ROMANEIO,
					 ROMANEIO,
					 DATA_ROMANEIO,
					 MOTORISTA_CPF,
					 MOTORISTA_NOME,
					 PARCEIRO_NOME,
					 PARCEIRO_CNPJ,
					 UNIDADE,
					 VOLUMES,
					 DESTINATARIO_LOGRADOURO,
					 DESTINATARIO_NUMERO,
					 DESTINATARIO_BAIRRO,
					 DESTINATARIO_CEP,
					 DESTINATARIO_CIDADE,
					 DESTINATARIO_CODIGO_CIDADE,
					 DESTINATARIO_UF,
					 DATA_REGISTRO,
                     MOTORISTA_PLACA,
                     STATUS_NF,
                     TRANSACAO,
                     TIP_ROMANEIO,
                     BD_ROMANEIO)
					Select nf.Chv_Acesso as CHAVE_NOTA ,
					nf.Num_Nota as NUMERO_NOTA,
					nf.Ser_Nota as SERIE_NOTA,
					nf.Dat_Emissao as DATA_NOTA,
					nf.Vlr_TotalNota as VALOR_NOTA,
					es.Des_RazSoc as  REMETENTE_NOME,
					es.Num_Cnpj as REMETENTE_CNPJ,
					cl.Razao_Social as DESTINATARIO_NOME,
					cl.Cgc_Cpf as DESTINATARIO_CNPJ,
					cl.Endereco+' '+cl.Numero+' '+cl.Complemento as DESTINATARIO_ENDERECO,
					Num_Romaneio as ID_ROMANEIO,
					Convert(varchar(10),rb.Num_Coleta) as ROMANEIO,
					IsNull(rb.Data_Coleta,GetDate()) as DATA_ROMANEIO,
					tr.Cgc_Cpf as MOTORISTA_CPF,
                    tr.Razao_Social as MOTORISTA_NOME,
                    '' as PARCEIRO_NOME,
                    '' as PARCEIRO_CNPJ,
					'CXA' as UNIDADE,
					nf.Qtd_Volumes as VOLUMES, 
                    cl.Endereco as DESTINATARIO_LOGRADOURO,
					cl.Numero as DESTINATARIO_NUMERO, 
                    br.Descricao as DESTINATARIO_BAIRRO,
					cl.Cep as DESTINATARIO_CEP, 
                    ci.Descricao as DESTINATARIO_CIDADE,
					ci.Cod_CidIbge as DESTINATARIO_CODIGO_CIDADE, 
                    cl.Cod_Estado as DESTINATARIO_UF,
					getdate() as DATA_REGISTRO ,
                    IsNULL('PLACA:'+tr.UF_PlaVei + tr.Cod_PlaVei, 'Não Cadastrado') as MOTORISTA_PLACA,
                    NF.[Status] as STATUS_NF,
                    nf.Transacao as TRANSACAO,
                    TIP_ROMANEIO = case 
                        when rb.Flg_EntPropria = 0 then 'Despachado'
                        when rb.Flg_EntPropria = 1 then 'EntrPropria'
                        end,
                    'DMD' as BD_ROMANEIO
					From NFSCB nf
					Inner Join ESTAB es On es.Cod_Estabe = nf.Cod_Estabe
					Inner Join CLIEN cl On cl.Codigo = nf.Cod_Cliente
					Inner Join RMNIT ri On ri.Cod_Estabe = nf.Cod_Estabe And ri.Ser_Nota = nf.Ser_Nota And ri.Num_Nota = nf.Num_Nota
					Inner Join RMNCB rb On rb.Cod_Estabe = ri.Cod_Estabe And rb.Numero = ri.Num_Romaneio
					Left Outer Join TRANS tr On tr.Codigo = nf.Cod_Transportadora
					Left Outer Join BAIRR br On br.Cod_Estado = cl.Cod_Estado and br.Cod_Cidade = cl.Cod_Cidade and br.Codigo = cl.Cod_Bairro
                    Left Outer Join CIDAD ci on ci.Cod_Estado = cl.Cod_Estado and ci.Codigo = cl.Cod_Cidade
					Where rb.Cod_Estabe = @CodEstabe
					And rb.Numero = @Numero
					And nf.Chv_Acesso is not null
      end

	  	
  end

GO
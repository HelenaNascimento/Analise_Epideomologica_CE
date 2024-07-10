use BD_DW
GO


DECLARE @BD_DW int = (SELECT COUNT(NUMERO) FROM BD_DW.dbo.R_PDVCB), 
		@BD_REMOTE int = (SELECT COUNT(NUMERO) FROM [RemoteServerName].[DMD].[dbo].[PDVCB]),
		@BD_REMOTE_1 int = (SELECT COUNT(NUMERO) FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = 1) 

--(SELECT COUNT(NUMERO) FROM PROD_2023.dbo.PDVCB)
--(SELECT TOP 1 1 FROM [RemoteServerName].[DMD].[dbo].[PDVCB])

BEGIN TRY
	BEGIN TRANSACTION;
	BEGIN
		INSERT INTO BD_DW.dbo.R_PDVCB ([Numero], [Cod_PedCli] ,[Tip_Pedido],[Cod_Cliente],[Cod_RegTri],[Cod_Vendedor],[Id_PolCom],[Cod_Prz],[Cod_Rota],[Status1],[Status2],[Dat_Pedido],[Hor_ConferIni],[Hor_ConferFim],[Hor_DbqFin],
							[Hor_DbqLic],[Hor_DbqRnt],[Hor_Entrada],[Hor_Saida],[Hor_Liberacao],[Hor_Fatura],[Hor_Fechamento],[Hor_Cancel],[Hor_ImpEtq],[Hor_Prenota],[Cod_FunSeparador],[Cod_FunEmbalador],[Cod_FunConferidor],
							[Cod_Digitador],[Usuario],[Nom_UsuCancel],[Nom_UsuDesbloq],[Nom_UsuDesbloqLic],[Nom_UsuDesbloqRnt],[Observacao],[Obs_Padrao],[Obs_NotFis],[Obs_IntFecPdv],[Obs_IntCtaRec],[C_VlrPedido])
		SELECT [Numero], [Cod_PedCli] ,[Tip_Pedido],[Cod_Cliente],[Cod_RegTri],[Cod_Vendedor],[Id_PolCom],[Cod_Prz],[Cod_Rota],[Status1],[Status2],[Dat_Pedido],[Hor_ConferIni],[Hor_ConferFim],[Hor_DbqFin],
				[Hor_DbqLic],[Hor_DbqRnt],[Hor_Entrada],[Hor_Saida],[Hor_Liberacao],[Hor_Fatura],[Hor_Fechamento],[Hor_Cancel],[Hor_ImpEtq],[Hor_Prenota],[Cod_FunSeparador],[Cod_FunEmbalador],[Cod_FunConferidor],
				[Cod_Digitador],[Usuario],[Nom_UsuCancel],[Nom_UsuDesbloq],[Nom_UsuDesbloqLic],[Nom_UsuDesbloqRnt],[Observacao],[Obs_Padrao],[Obs_NotFis],[Obs_IntFecPdv],[Obs_IntCtaRec],[C_VlrPedido]
		FROM [RemoteServerName].[DMD].[dbo].[PDVCB]
		WHERE Cod_Estabe = 1
			AND [Numero] NOT IN (SELECT [Numero] FROM BD_DW.dbo.R_PDVCB)
	END

	BEGIN 
		UPDATE R_CB 
		SET  
		 R_CB.[Cod_RegTri] = CB.[Cod_RegTri] 
		,R_CB.[Cod_Vendedor] = CB.[Cod_Vendedor] 
		,R_CB.[Id_PolCom] = CB.[Id_PolCom] 
		,R_CB.[Cod_Prz] = CB.[Cod_Prz] 
		,R_CB.[Cod_Rota]  = CB.[Cod_Rota]  
		,R_CB.[Status1]  = CB.[Status1]  
		,R_CB.[Status2] = CB.[Status2] 
		,R_CB.[Dat_Pedido] = CB.[Dat_Pedido] 
		,R_CB.[Hor_ConferIni]  = CB.[Hor_ConferIni]  
		,R_CB.[Hor_ConferFim]  = CB.[Hor_ConferFim]  
		,R_CB.[Hor_DbqFin]  = CB.[Hor_DbqFin]  
		,R_CB.[Hor_DbqLic]  = CB.[Hor_DbqLic]  
		,R_CB.[Hor_DbqRnt]  = CB.[Hor_DbqRnt]  
		,R_CB.[Hor_Entrada] = CB.[Hor_Entrada] 
		,R_CB.[Hor_Saida] = CB.[Hor_Saida] 
		,R_CB.[Hor_Liberacao] = CB.[Hor_Liberacao] 
		,R_CB.[Hor_Fatura] = CB.[Hor_Fatura] 
		,R_CB.[Hor_Fechamento] = CB.[Hor_Fechamento] 
		,R_CB.[Hor_Cancel] = CB.[Hor_Cancel] 
		,R_CB.[Hor_ImpEtq] = CB.[Hor_ImpEtq] 
		,R_CB.[Hor_Prenota] = CB.[Hor_Prenota] 
		,R_CB.[Cod_FunSeparador] = CB.[Cod_FunSeparador] 
		,R_CB.[Cod_FunEmbalador] = CB.[Cod_FunEmbalador] 
		,R_CB.[Cod_FunConferidor] = CB.[Cod_FunConferidor] 
		,R_CB.[Cod_Digitador] = CB.[Cod_Digitador] 
		,R_CB.[Usuario] = CB.[Usuario] 
		,R_CB.[Nom_UsuCancel] = CB.[Nom_UsuCancel] 
		,R_CB.[Nom_UsuDesbloq] = CB.[Nom_UsuDesbloq] 
		,R_CB.[Nom_UsuDesbloqLic] = CB.[Nom_UsuDesbloqLic] 
		,R_CB.[Nom_UsuDesbloqRnt] = CB.[Nom_UsuDesbloqRnt] 
		,R_CB.[Observacao] = CB.[Observacao] 
		,R_CB.[Obs_Padrao]  = CB.[Obs_Padrao]  
		,R_CB.[Obs_NotFis] = CB.[Obs_NotFis] 
		,R_CB.[Obs_IntFecPdv]  = CB.[Obs_IntFecPdv]  
		,R_CB.[Obs_IntCtaRec] = CB.[Obs_IntCtaRec] 
		,R_CB.[C_VlrPedido]  = CB.[C_VlrPedido]  
		FROM BD_DW.dbo.R_PDVCB R_CB
			INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
					AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
					AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
				
		WHERE 
			CB.Cod_Estabe = 1
		AND ( R_CB.[Cod_RegTri] <> CB.[Cod_RegTri] 
		OR R_CB.[Cod_Vendedor] <> CB.[Cod_Vendedor] 
		OR R_CB.[Id_PolCom] <> CB.[Id_PolCom] 
		OR R_CB.[Cod_Prz] <> CB.[Cod_Prz] 
		OR R_CB.[Cod_Rota] <> CB.[Cod_Rota]  
		OR R_CB.[Status1]  <> CB.[Status1]  
		OR R_CB.[Status2] <> CB.[Status2] 
		OR R_CB.[Dat_Pedido] <> CB.[Dat_Pedido] 
		OR R_CB.[Hor_ConferIni]  <> CB.[Hor_ConferIni]  
		OR R_CB.[Hor_ConferFim]  <> CB.[Hor_ConferFim]  
		OR R_CB.[Hor_DbqFin]  <> CB.[Hor_DbqFin]  
		OR R_CB.[Hor_DbqLic]  <> CB.[Hor_DbqLic]  
		OR R_CB.[Hor_DbqRnt]  <> CB.[Hor_DbqRnt]  
		OR R_CB.[Hor_Entrada] <> CB.[Hor_Entrada] 
		OR R_CB.[Hor_Saida] <> CB.[Hor_Saida] 
		OR R_CB.[Hor_Liberacao] <> CB.[Hor_Liberacao] 
		OR R_CB.[Hor_Fatura] <> CB.[Hor_Fatura] 
		OR R_CB.[Hor_Fechamento] <> CB.[Hor_Fechamento] 
		OR R_CB.[Hor_Cancel] <> CB.[Hor_Cancel] 
		OR R_CB.[Hor_ImpEtq] <> CB.[Hor_ImpEtq] 
		OR R_CB.[Hor_Prenota] <> CB.[Hor_Prenota] 
		OR R_CB.[Cod_FunSeparador] <> CB.[Cod_FunSeparador] 
		OR R_CB.[Cod_FunEmbalador] <> CB.[Cod_FunEmbalador] 
		OR R_CB.[Cod_FunConferidor] <> CB.[Cod_FunConferidor] 
		OR R_CB.[Cod_Digitador] <> CB.[Cod_Digitador] 
		OR R_CB.[Usuario] <> CB.[Usuario] 
		OR R_CB.[Nom_UsuCancel] <> CB.[Nom_UsuCancel] 
		OR R_CB.[Nom_UsuDesbloq] <> CB.[Nom_UsuDesbloq] 
		OR R_CB.[Nom_UsuDesbloqLic] <> CB.[Nom_UsuDesbloqLic] 
		OR R_CB.[Nom_UsuDesbloqRnt] <> CB.[Nom_UsuDesbloqRnt]
		OR R_CB.[C_VlrPedido]  <> CB.[C_VlrPedido])
	END
	COMMIT TRANSACTION;
END TRY
BEGIN CATCH

    ROLLBACK TRANSACTION;

    DECLARE @ErrorMessage NVARCHAR(4000);
    DECLARE @ErrorSeverity INT;
    DECLARE @ErrorState INT;

    SELECT 
        @ErrorMessage = ERROR_MESSAGE(),
        @ErrorSeverity = ERROR_SEVERITY(),
        @ErrorState = ERROR_STATE();

    RAISERROR (@ErrorMessage, @ErrorSeverity, @ErrorState);
END CATCH;
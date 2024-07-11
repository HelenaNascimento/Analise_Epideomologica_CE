use BD_DW
GO
BEGIN TRY
	BEGIN TRANSACTION;
		IF (SELECT MAX(dat_pedido) FROM [RemoteServerName].[DMD].[dbo].[PDVCB]) > (SELECT MAX(dat_pedido) FROM BD_DW.dbo.R_PDVCB)
			BEGIN

					INSERT INTO BD_DW.dbo.R_PDVCB ([Numero], [Cod_PedCli] ,[Tip_Pedido],[Cod_Cliente],[Cod_RegTri],[Cod_Vendedor],[Id_PolCom],[Cod_Prz],[Cod_Rota],[Status1],[Status2],[Dat_Pedido],[Hor_ConferIni],[Hor_ConferFim],[Hor_DbqFin],
								[Hor_DbqLic],[Hor_DbqRnt],[Hor_Entrada],[Hor_Saida],[Hor_Liberacao],[Hor_Fatura],[Hor_Fechamento],[Hor_Cancel],[Hor_ImpEtq],[Hor_Prenota],[Cod_FunSeparador],[Cod_FunEmbalador],[Cod_FunConferidor],
								[Cod_Digitador],[Usuario],[Nom_UsuCancel],[Nom_UsuDesbloq],[Nom_UsuDesbloqLic],[Nom_UsuDesbloqRnt],[Observacao],[Obs_Padrao],[Obs_NotFis],[Obs_IntFecPdv],[Obs_IntCtaRec],[C_VlrPedido])
					SELECT [Numero], [Cod_PedCli] ,[Tip_Pedido],[Cod_Cliente],[Cod_RegTri],[Cod_Vendedor],[Id_PolCom],[Cod_Prz],[Cod_Rota],[Status1],[Status2],[Dat_Pedido],[Hor_ConferIni],[Hor_ConferFim],[Hor_DbqFin],
					[Hor_DbqLic],[Hor_DbqRnt],[Hor_Entrada],[Hor_Saida],[Hor_Liberacao],[Hor_Fatura],[Hor_Fechamento],[Hor_Cancel],[Hor_ImpEtq],[Hor_Prenota],[Cod_FunSeparador],[Cod_FunEmbalador],[Cod_FunConferidor],
					[Cod_Digitador],[Usuario],[Nom_UsuCancel],[Nom_UsuDesbloq],[Nom_UsuDesbloqLic],[Nom_UsuDesbloqRnt],[Observacao],[Obs_Padrao],[Obs_NotFis],[Obs_IntFecPdv],[Obs_IntCtaRec],[C_VlrPedido]
					FROM [RemoteServerName].[DMD].[dbo].[PDVCB]
					WHERE Cod_Estabe = 1
					AND dat_pedido > (SELECT MAX(dat_pedido) FROM BD_DW.dbo.R_PDVCB)
					AND [Numero] NOT IN (SELECT [Numero] FROM BD_DW.dbo.R_PDVCB)
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
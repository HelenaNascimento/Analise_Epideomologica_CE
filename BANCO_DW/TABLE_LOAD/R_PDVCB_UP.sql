DECLARE 
	@ESTAB INT = 1, 
	@Num_Ped INT, 
	@DT_PED_INI smalldatetime = getdate() - 10,
	@DT_PED_FIM smalldatetime = getdate() - 2

DECLARE CURSOR_PEDIDO CURSOR FOR
	
	SELECT 
		R_CB.Numero
	FROM BD_DW.dbo.R_PDVCB R_CB
			INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
				AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
				AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
	WHERE cb.Cod_Estabe = @ESTAB
		AND cb.dat_pedido >= @DT_PED_INI
		AND cb.dat_pedido <= @DT_PED_FIM


OPEN CURSOR_PEDIDO

FETCH NEXT FROM CURSOR_PEDIDO INTO @Num_Ped
WHILE @@FETCH_STATUS = 0
BEGIN
	IF (SELECT Cod_RegTri FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Cod_RegTri FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.[Cod_RegTri] = CB.[Cod_RegTri]	
			
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Cod_Vendedor FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Cod_Vendedor FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Cod_Vendedor = CB.Cod_Vendedor	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Id_PolCom FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Id_PolCom FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Id_PolCom = CB.Id_PolCom	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Cod_Prz FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Cod_Prz FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Cod_Prz = CB.Cod_Prz	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Cod_Rota FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Cod_Rota FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Cod_Rota = CB.Cod_Rota	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Status1 FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Status1 FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Status1 = CB.Status1	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Status2 FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Status2 FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Status2 = CB.Status2	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_ConferIni FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_ConferIni FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_ConferIni = CB.Hor_ConferIni	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_ConferFim FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_ConferFim FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_ConferFim = CB.Hor_ConferFim	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END
	IF (SELECT Hor_DbqFin FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_DbqFin FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_DbqFin = CB.Hor_DbqFin	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_DbqLic FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_DbqLic FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_DbqLic = CB.Hor_DbqLic	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_DbqRnt FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_DbqRnt FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_DbqRnt = CB.Hor_DbqRnt	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_Entrada FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_Entrada FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_Entrada = CB.Hor_Entrada	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_Saida FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_Saida FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_Saida = CB.Hor_Saida	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_Liberacao FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_Liberacao FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_Liberacao = CB.Hor_Liberacao	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_Fatura FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_Fatura FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_Fatura = CB.Hor_Fatura	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_Fechamento FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_Fechamento FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_Fechamento = CB.Hor_Fechamento	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_Cancel FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_Cancel FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_Cancel = CB.Hor_Cancel	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_ImpEtq FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_ImpEtq FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_ImpEtq = CB.Hor_ImpEtq	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Hor_Prenota FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Hor_Prenota FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Hor_Prenota = CB.Hor_Prenota	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Nom_UsuCancel FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Nom_UsuCancel FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Nom_UsuCancel = CB.Nom_UsuCancel	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Nom_UsuDesbloq FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Nom_UsuDesbloq FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Nom_UsuDesbloq = CB.Nom_UsuDesbloq	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Nom_UsuDesbloqLic FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Nom_UsuDesbloqLic FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Nom_UsuDesbloqLic = CB.Nom_UsuDesbloqLic	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT Nom_UsuDesbloqRnt FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT Nom_UsuDesbloqRnt FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.Nom_UsuDesbloqRnt = CB.Nom_UsuDesbloqRnt	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END

	IF (SELECT C_VlrPedido FROM BD_DW.dbo.R_PDVCB WHERE Numero = @Num_Ped) <> (SELECT C_VlrPedido FROM [RemoteServerName].[DMD].[dbo].[PDVCB] WHERE Cod_Estabe = @ESTAB and Numero = @Num_Ped)
		BEGIN
			UPDATE R_CB
				R_CB.C_VlrPedido = CB.C_VlrPedido	
			FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
			WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
		END


	BEGIN
		UPDATE R_CB
			 R_CB.[Observacao] = CB.[Observacao] 
			,R_CB.[Obs_Padrao]  = CB.[Obs_Padrao]  
			,R_CB.[Obs_NotFis] = CB.[Obs_NotFis] 
			,R_CB.[Obs_IntFecPdv]  = CB.[Obs_IntFecPdv]  
			,R_CB.[Obs_IntCtaRec] = CB.[Obs_IntCtaRec] 	
		FROM BD_DW.dbo.R_PDVCB R_CB
				INNER JOIN [RemoteServerName].[DMD].[dbo].[PDVCB] CB ON  R_CB.[Numero] = CB.[Numero]  
						AND R_CB.[Cod_PedCli] = CB.[Cod_PedCli] 
						AND R_CB.[Cod_Cliente]  = CB.[Cod_Cliente] 
		WHERE 
				CB.Cod_Estabe = @ESTAB
			AND	CB.Numero = @Num_Ped
	END

FETCH NEXT FROM CURSOR_PEDIDO INTO @Num_Ped
END

CLOSE CURSOR_PEDIDO
DEALLOCATE CURSOR_PEDIDO
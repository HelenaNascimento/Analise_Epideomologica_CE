use BD_DW
GO


DECLARE @BD_DW int = (SELECT COUNT(NUMERO) FROM BD_DW.dbo.R_PDVIT), 
		@BD_REMOTE int = (SELECT COUNT(NUMERO) FROM [RemoteServerName].[DMD].[dbo].[PDVIT]),
		@BD_REMOTE_1 int = (SELECT COUNT(NUMERO) FROM [RemoteServerName].[DMD].[dbo].[PDVIT] WHERE Cod_Estabe = 1) 

--(SELECT COUNT(NUMERO) FROM PROD_2023.dbo.PDVCB)
--(SELECT TOP 1 1 FROM [RemoteServerName].[DMD].[dbo].[PDVCB])

BEGIN TRY
	BEGIN TRANSACTION;
	IF @BD_DW < @BD_REMOTE
	BEGIN
        INSERT INTO BD_DW.dbo.R_PDVIT ([Cod_Pedido],[Cod_Produto],[Cod_Lote],[Cod_Fabricante],[Loc_Fisica],[Qtd_Solicitado],[Qtd_Acerto],[Qtd_Pedido],[Qtd_Bonificacao],[Qtd_Pendente],[Flg_PrcUniDsc],[Per_Desconto],[Prc_Unitario],[Tip_Aux]
                            ,[Ctrl_Tributacao],[Cod_ClaTri],[Alq_Icms],[Vlr_BasIcmsNor],[Vlr_IcmsNor],[Vlr_RepIcms],[Vlr_BasRepIcms],[Vlr_SubsTrib],[Vlr_PrdSubTri],[Vlr_BasSubsTrib],[Vlr_IcmsTri],[Vlr_BasIcmsTri],[Vlr_Isento]
                            ,[Vlr_DscTri],[Vlr_Bruto],[Cod_Promocao],[Desconto],[Flg_BlqInfPar],[Qtd_PrzMax],[Per_Descon],[Per_DscVis],[C_VlrDesconto],[C_PrcUni],[C_PerDscIte],[C_PrcTotal],[C_VlrLiquido],[Prc_Tabela],[Des_Und]
                            ,[Qtd_AprPdv],[Des_UndVen],[Prc_TabBru],[Vlr_Outros],[Flg_Entreg],[Qtd_ImpFat],[Prc_UniImpFat],[Fat_CnvImpFat],[Qtd_Pra],[Qtd_Dep],[Prc_MaxCon],[Prc_Fabric],[Num_SeqDig],[Nom_UsuDesbloq],[Des_UnvImpFat]
                            ,[Des_AprImpFat],[Flg_VctLic],[Per_Rnt],[Prc_BasRnt],[Per_Comissao],[Per_ComTlmkt],[Per_ComTra],[Vlr_Comissao],[Vlr_ComTlmkt],[Val_ComTra],[Cod_ModBasCalIcm],[Cod_ModBasCalIcmSbt],[Per_RedBasCalIcm]
                            ,[Per_RedBasCalIcmSbt],[Alq_IcmSbt],[Vlr_BasTri],[Alq_AgrSbt],[Id_PolCom],[Flg_Verba],[Prc_PolCom],[Per_DscPolCom],[Per_DscVisPolCom],[Vlr_Verba],[Qtd_PrzPolCom],[Des_MsgDesbloq],[Prc_UniLiqPer]
                            ,[Prc_LiqUltEnt],[Cod_MtvRej],[Des_MtvRej],[Per_ComGer],[Vlr_ComGer],[Per_ComSup],[Vlr_ComSup],[Per_ComGerOpe],[Vlr_ComGerOpe],[Per_ComSupOpe],[Vlr_ComSupOpe],[Per_VrbPar],[Vlr_VrbPar],[Per_VrbBon]
                            ,[Vlr_VrbBon],[Prc_UniPde],[Per_DscPde],[Prc_RefFpb],[Cod_CSOSN],[Flg_PolPrm],[Cod_PrjPde],[Num_SeqPdc],[Vlr_BasSbtRes],[Vlr_SbtRes],[Num_IteCot],[Vlr_Bonificacao],[Vlr_DscBonRat],[Prc_CusMinComMarkup]
                            ,[Vlr_BasVerba],[Vlr_VrbVdr],[Per_MarkupCusCom],[Tip_Sai],[CodAnt],[NovoCodigo],[Cod_Estabe],[Per_AcrAlqIntFcp],[Vlr_IcmFcpDes],[Vlr_Despes],[Vlr_BasDespes],[Per_PrvParIcm],[Vlr_IcmParDes],[Vlr_IcmParRem]
                            ,[Alq_IntIcmDes],[Cod_KitPrm],[CST_Pis],[Vlr_BasPis],[Alq_Pis],[Vlr_Pis],[CST_Cofins],[Vlr_BasCofins],[Alq_Cofins],[Vlr_Cofins],[Per_MarBru],[Vlr_VrbPdv],[Vlr_BasCalSubTriEntMed],[Per_RntBru],[Tip_BasRnt]
                            ,[Flg_UsaDebPresum],[Alq_IcmPresum],[Vlr_DscBonDup],[Vlr_BasDscTri],[Per_DscTri],[Alq_IcmDif],[Vlr_IcmsDif],[Qtd_PesVolImpFat],[Des_UnvPesVolImpFat],[Prc_UniPesVolImpFat],[Vlr_IcmsDeson],[Cod_MtvIcmsDeson]
                            ,[Vlr_DscCalSuframa],[Flg_IncFis],[Alq_FcpIcm],[Vlr_FcpIcm],[Alq_FcpSbt],[Vlr_FcpSbt],[Alq_FcpSbtRet],[Vlr_FcpSbtRet],[Per_IcmDiferi],[Vlr_IcmDiferi],[Nom_UsuCor],[Dat_UsuCor],[Cod_MtvCor],[Des_MtvCor]
                            ,[Vlr_VrbOpe],[Vlr_VrbSup],[Per_VrbVdr],[Per_VrbOpe],[Per_VrbSup],[Alq_EfeDas],[Vlr_DscRat],[Prc_PonFin],[Vlr_DspExt],[Vlr_BasIpi],[Vlr_Ipi],[Alq_Ipi],[CST_Ipi],[Cod_EnqIpi],[Cod_OriMer],[Prc_UniSemAcrIcm]
                            ,[Flg_PrcUniAcrIcm],[Prc_UniComAcrIcm],[Per_RepIcms],[Vlr_BasIrf],[Alq_Irf],[Vlr_Irf],[Qtd_GanCot],[Per_LinCot],[Vlr_SbtRetAnt],[Vlr_BasCsl],[Alq_Csl],[Vlr_Csl],[Tip_PolCom])
        SELECT 
                [Cod_Pedido],[Cod_Produto],[Cod_Lote],[Cod_Fabricante],[Loc_Fisica],[Qtd_Solicitado],[Qtd_Acerto],[Qtd_Pedido],[Qtd_Bonificacao],[Qtd_Pendente],[Flg_PrcUniDsc],[Per_Desconto],[Prc_Unitario],[Tip_Aux]
                ,[Ctrl_Tributacao],[Cod_ClaTri],[Alq_Icms],[Vlr_BasIcmsNor],[Vlr_IcmsNor],[Vlr_RepIcms],[Vlr_BasRepIcms],[Vlr_SubsTrib],[Vlr_PrdSubTri],[Vlr_BasSubsTrib],[Vlr_IcmsTri],[Vlr_BasIcmsTri],[Vlr_Isento]
                ,[Vlr_DscTri],[Vlr_Bruto],[Cod_Promocao],[Desconto],[Flg_BlqInfPar],[Qtd_PrzMax],[Per_Descon],[Per_DscVis],[C_VlrDesconto],[C_PrcUni],[C_PerDscIte],[C_PrcTotal],[C_VlrLiquido],[Prc_Tabela],[Des_Und]
                ,[Qtd_AprPdv],[Des_UndVen],[Prc_TabBru],[Vlr_Outros],[Flg_Entreg],[Qtd_ImpFat],[Prc_UniImpFat],[Fat_CnvImpFat],[Qtd_Pra],[Qtd_Dep],[Prc_MaxCon],[Prc_Fabric],[Num_SeqDig],[Nom_UsuDesbloq],[Des_UnvImpFat]
                ,[Des_AprImpFat],[Flg_VctLic],[Per_Rnt],[Prc_BasRnt],[Per_Comissao],[Per_ComTlmkt],[Per_ComTra],[Vlr_Comissao],[Vlr_ComTlmkt],[Val_ComTra],[Cod_ModBasCalIcm],[Cod_ModBasCalIcmSbt],[Per_RedBasCalIcm]
                ,[Per_RedBasCalIcmSbt],[Alq_IcmSbt],[Vlr_BasTri],[Alq_AgrSbt],[Id_PolCom],[Flg_Verba],[Prc_PolCom],[Per_DscPolCom],[Per_DscVisPolCom],[Vlr_Verba],[Qtd_PrzPolCom],[Des_MsgDesbloq],[Prc_UniLiqPer]
                ,[Prc_LiqUltEnt],[Cod_MtvRej],[Des_MtvRej],[Per_ComGer],[Vlr_ComGer],[Per_ComSup],[Vlr_ComSup],[Per_ComGerOpe],[Vlr_ComGerOpe],[Per_ComSupOpe],[Vlr_ComSupOpe],[Per_VrbPar],[Vlr_VrbPar],[Per_VrbBon]
                ,[Vlr_VrbBon],[Prc_UniPde],[Per_DscPde],[Prc_RefFpb],[Cod_CSOSN],[Flg_PolPrm],[Cod_PrjPde],[Num_SeqPdc],[Vlr_BasSbtRes],[Vlr_SbtRes],[Num_IteCot],[Vlr_Bonificacao],[Vlr_DscBonRat],[Prc_CusMinComMarkup]
                ,[Vlr_BasVerba],[Vlr_VrbVdr],[Per_MarkupCusCom],[Tip_Sai],[CodAnt],[NovoCodigo],[Cod_Estabe],[Per_AcrAlqIntFcp],[Vlr_IcmFcpDes],[Vlr_Despes],[Vlr_BasDespes],[Per_PrvParIcm],[Vlr_IcmParDes],[Vlr_IcmParRem]
                ,[Alq_IntIcmDes],[Cod_KitPrm],[CST_Pis],[Vlr_BasPis],[Alq_Pis],[Vlr_Pis],[CST_Cofins],[Vlr_BasCofins],[Alq_Cofins],[Vlr_Cofins],[Per_MarBru],[Vlr_VrbPdv],[Vlr_BasCalSubTriEntMed],[Per_RntBru],[Tip_BasRnt]
                ,[Flg_UsaDebPresum],[Alq_IcmPresum],[Vlr_DscBonDup],[Vlr_BasDscTri],[Per_DscTri],[Alq_IcmDif],[Vlr_IcmsDif],[Qtd_PesVolImpFat],[Des_UnvPesVolImpFat],[Prc_UniPesVolImpFat],[Vlr_IcmsDeson],[Cod_MtvIcmsDeson]
                ,[Vlr_DscCalSuframa],[Flg_IncFis],[Alq_FcpIcm],[Vlr_FcpIcm],[Alq_FcpSbt],[Vlr_FcpSbt],[Alq_FcpSbtRet],[Vlr_FcpSbtRet],[Per_IcmDiferi],[Vlr_IcmDiferi],[Nom_UsuCor],[Dat_UsuCor],[Cod_MtvCor],[Des_MtvCor]
                ,[Vlr_VrbOpe],[Vlr_VrbSup],[Per_VrbVdr],[Per_VrbOpe],[Per_VrbSup],[Alq_EfeDas],[Vlr_DscRat],[Prc_PonFin],[Vlr_DspExt],[Vlr_BasIpi],[Vlr_Ipi],[Alq_Ipi],[CST_Ipi],[Cod_EnqIpi],[Cod_OriMer],[Prc_UniSemAcrIcm]
                ,[Flg_PrcUniAcrIcm],[Prc_UniComAcrIcm],[Per_RepIcms],[Vlr_BasIrf],[Alq_Irf],[Vlr_Irf],[Qtd_GanCot],[Per_LinCot],[Vlr_SbtRetAnt],[Vlr_BasCsl],[Alq_Csl],[Vlr_Csl],[Tip_PolCom]
        FROM [RemoteServerName].[DMD].[dbo].[PDVCB]
        WHERE 
            COD_ESTABE = 1
        AND exists (SELECT Cod_Pedido FROM R_PDVCB)
    END

    IF @BD_DW = @BD_REMOTE_1
	BEGIN 
		UPDATE R_CB 
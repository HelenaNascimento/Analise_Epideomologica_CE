DECLARE 
    @C_ClaTr VARCHAR(6),
    @CodRegTr INT = 89,  -- variável para filtro fixo
    @C_RegTr INT         -- variável para iterar os valores do cursor

DECLARE CLASS CURSOR FOR
    SELECT Cod_ClaTri, Cod_RegTri  
    FROM RTXCT
    WHERE COD_REGTRI = @CodRegTr;

OPEN CLASS

FETCH NEXT FROM CLASS INTO @C_ClaTr, @C_RegTr

WHILE @@FETCH_STATUS = 0
BEGIN
    BEGIN TRAN

    INSERT INTO RTXCT (
        Cod_RegTri, Cod_ClaTri, Alq_IcmIntReg, Alq_IcmExtReg, 
        Per_RedBasCalIntReg, Per_RedBasCalExtReg, Alq_AgrDebEntIcm, 
        Alq_IcmIntLoc, Alq_IcmExtLoc, Per_RedBasCalIntLoc, Per_RedBasCalExtLoc,
        Ctrl_SujSubTri, Ctrl_BasCalDebEnt, Alq_AgrDebEnt, Per_RedBasCalDebEnt,
        Ctrl_BasCalDebSai, Alq_AgrDebSai, Per_RedBasCalDebSai, Per_DscTri,
        Cod_ImpNotSai, C_PerRepIcmReg, C_PerRepIcmLoc, Msg_Nfs, Tip_PrcBasDebSbt,
        Ctrl_BasCalCreEnt, Ctrl_BasCalCreSai, Tip_PrcBasSbtEnt, Cod_ImpNotEnt,
        Tip_PrcBasCreSbtEnt, Tip_PrcBasCreSbtSai, Tip_AgrDebSbt, Tip_AgrSbtEnt,
        Per_LimBasCalCreSbtEnt, Cod_ModBasCalIcmEnt, Cod_ModBasCalIcmSai,
        Cod_ModBasCalIcmSbtEnt, Cod_ModBasCalIcmSbtSai, Flg_ImpSbtFntEnt,
        Ctr_TriSai, Tip_PrcBasSbtRecEnt, Ctrl_BasCalDebSbtRecEnt, Tip_AgrDebSbtRecEnt,
        Alq_AgrDebSbtRecEnt, Per_RedBasCalDebSbtRecEnt, Alq_DebSbtRecEnt,
        Tip_PrcBasSbtRecSai, Ctrl_BasCalDebSbtRecSai, Tip_AgrDebSbtRecSai,
        Alq_AgrDebSbtRecSai, Per_RedBasCalDebSbtRecSai, Alq_DebSbtRecSai,
        Tip_CreSbtEnt, Tip_CreSbtSai, Alq_DebSbtEnt, Alq_DebSbtSai,
        Per_LimMinIcmSbtSaiRedDeb, Per_LimMinIcmSbtEnt, Cod_CSOSNEnt, Cod_CSOSNSai,
        Per_RedBasCalDebEntOpc, Per_AcrAlqIntFcpReg, Per_PrvParIcm, Per_ResExtSai,
        Per_DspCusEnt, Tip_PrcLimMinIcmSbtEnt, Tip_PrcLimMinBasCalIcmSbtEnt,
        Per_LimMinBasCalIcmSbtEnt, Tip_PrcLimMinBasCalIcmSbtSai,
        Alq_AgrLimMinBasCalIcmSbtSai, Alq_IcmPresumReg, Alq_IcmPresumLoc,
        Obs_FiscalSai, Alq_IcmDifExtLoc, Alq_IcmDifExtReg, Per_ResExtEnt,
        Per_AcrAlqExtFcpReg, Per_AcrAlqIntFcpLoc, Per_AcrAlqExtFcpLoc,
        Per_IcmDiferiExtReg, Per_IcmDiferiExtLoc, Cod_MtvDesIcmLoc,
        Cod_MtvDesIcmReg, Flg_DscDesIcmLoc, Flg_DscDesIcmReg, Alq_IcmIntPadReg,
        Alq_IcmExtPadReg, Alq_IcmExtPadLoc, Alq_IcmIntPadLoc, Per_IcmDesoneExtReg,
        Per_IcmDesoneExtLoc, Tip_PrcLimMinIcmSbtSai, Per_LimMinIcmSbtSai,
        Cod_ClaTriExt, Id_MsgDesIcmLoc, Id_MsgDesIcmReg, Alq_IrfIntLoc,
        Alq_CslIntLoc, Flg_RetFonDedTit, Tip_BasCalDifalReg, Cod_CalDifalReg
    )
    SELECT 
        99, Cod_ClaTri, Alq_IcmIntReg, Alq_IcmExtReg, Per_RedBasCalIntReg,
        Per_RedBasCalExtReg, Alq_AgrDebEntIcm, Alq_IcmIntLoc, Alq_IcmExtLoc,
        Per_RedBasCalIntLoc, Per_RedBasCalExtLoc, Ctrl_SujSubTri, Ctrl_BasCalDebEnt,
        Alq_AgrDebEnt, Per_RedBasCalDebEnt, Ctrl_BasCalDebSai, Alq_AgrDebSai,
        Per_RedBasCalDebSai, Per_DscTri, Cod_ImpNotSai, C_PerRepIcmReg,
        C_PerRepIcmLoc, Msg_Nfs, Tip_PrcBasDebSbt, Ctrl_BasCalCreEnt,
        Ctrl_BasCalCreSai, Tip_PrcBasSbtEnt, Cod_ImpNotEnt, Tip_PrcBasCreSbtEnt,
        Tip_PrcBasCreSbtSai, Tip_AgrDebSbt, Tip_AgrSbtEnt, Per_LimBasCalCreSbtEnt,
        Cod_ModBasCalIcmEnt, Cod_ModBasCalIcmSai, Cod_ModBasCalIcmSbtEnt,
        Cod_ModBasCalIcmSbtSai, Flg_ImpSbtFntEnt, Ctr_TriSai, Tip_PrcBasSbtRecEnt,
        Ctrl_BasCalDebSbtRecEnt, Tip_AgrDebSbtRecEnt, Alq_AgrDebSbtRecEnt,
        Per_RedBasCalDebSbtRecEnt, Alq_DebSbtRecEnt, Tip_PrcBasSbtRecSai,
        Ctrl_BasCalDebSbtRecSai, Tip_AgrDebSbtRecSai, Alq_AgrDebSbtRecSai,
        Per_RedBasCalDebSbtRecSai, Alq_DebSbtRecSai, Tip_CreSbtEnt, Tip_CreSbtSai,
        Alq_DebSbtEnt, Alq_DebSbtSai, Per_LimMinIcmSbtSaiRedDeb, Per_LimMinIcmSbtEnt,
        Cod_CSOSNEnt, Cod_CSOSNSai, Per_RedBasCalDebEntOpc, Per_AcrAlqIntFcpReg,
        Per_PrvParIcm, Per_ResExtSai, Per_DspCusEnt, Tip_PrcLimMinIcmSbtEnt,
        Tip_PrcLimMinBasCalIcmSbtEnt, Per_LimMinBasCalIcmSbtEnt,
        Tip_PrcLimMinBasCalIcmSbtSai, Alq_AgrLimMinBasCalIcmSbtSai,
        Alq_IcmPresumReg, Alq_IcmPresumLoc, Obs_FiscalSai, Alq_IcmDifExtLoc,
        Alq_IcmDifExtReg, Per_ResExtEnt, Per_AcrAlqExtFcpReg, Per_AcrAlqIntFcpLoc,
        Per_AcrAlqExtFcpLoc, Per_IcmDiferiExtReg, Per_IcmDiferiExtLoc,
        Cod_MtvDesIcmLoc, Cod_MtvDesIcmReg, Flg_DscDesIcmLoc, Flg_DscDesIcmReg,
        Alq_IcmIntPadReg, Alq_IcmExtPadReg, Alq_IcmExtPadLoc, Alq_IcmIntPadLoc,
        Per_IcmDesoneExtReg, Per_IcmDesoneExtLoc, Tip_PrcLimMinIcmSbtSai,
        Per_LimMinIcmSbtSai, Cod_ClaTriExt, Id_MsgDesIcmLoc, Id_MsgDesIcmReg,
        Alq_IrfIntLoc, Alq_CslIntLoc, Flg_RetFonDedTit, Tip_BasCalDifalReg,
        Cod_CalDifalReg
    FROM RTXCT
    WHERE cod_RegTri = @C_RegTr AND Cod_ClaTri = @C_ClaTr;

    COMMIT TRAN  -- Confirma a transação.

    FETCH NEXT FROM CLASS INTO @C_ClaTr, @C_RegTr
END

CLOSE CLASS
DEALLOCATE CLASS

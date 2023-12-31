select COUNT(cod_regtri) from RTXCT
where cod_regtri in (65) 

select COUNT(cod_regtri) from RTXCT
where cod_regtri in (89) 


select * from Rgtri
where cod_regtri in (65, 89) 

select * from CLTRI
where Cod_ClaTri in (select  Cod_ClaTri from RTXCT
where cod_regtri = 60)

select count(cod_regtri)
    from CLIEN CL
        INNER JOIN ENXES ES on CL.codigo = Es.cod_client
where es.cod_estabe = 1
            and cod_regtri = 65



commit

begin tran 
update ES
set es.cod_regtri = 89
    from CLIEN CL
        INNER JOIN ENXES ES on CL.codigo = Es.cod_client
where es.cod_estabe = 1
            and cod_regtri = 65

select  * from RTXCT
where cod_regtri in (88) 

select * from Rgtri
where cod_regtri in (60, 88) 

--60 -> 88
--65 -> 89

select * from CLTRI
where Cod_ClaTri in (select  Cod_ClaTri from RTXCT
where cod_regtri = 60)


select * from RTXCT
where Cod_RegTri in (60, 88)
and Cod_ClaTri = 'ALC'




select top 100 cl.codigo, cl.razao_social, es.cod_regtri
    from CLIEN CL
        INNER JOIN ENXES ES on CL.codigo = Es.cod_client
where es.cod_estabe = 1
            and cod_regtri = 60
/*
commit

begin tran 
update ES
set es.cod_regtri = 88
    from CLIEN CL
        INNER JOIN ENXES ES on CL.codigo = Es.cod_client
where es.cod_estabe = 1
            and cod_regtri = 60
            */

/*
COMMIT

begin TRAN
UPDATE RTXCT
set Alq_DebSbtSai = 9.3 
where Cod_RegTri = 88 and Alq_DebSbtSai = 7.8 */


select count(cod_regtri)
    from CLIEN CL
        INNER JOIN ENXES ES on CL.codigo = Es.cod_client
where es.cod_estabe = 1
            and cod_regtri = 60

            /*
COMMIT

begin TRAN
UPDATE RTXCT
set Alq_icmIntReg = 20, alq_icmExtLoc = 20
where Cod_RegTri = 88 and Cod_ClaTri <> 'ALC' */


COMMIT

ROLLBACK

begin tran
INSERT INTO RTXCT (Cod_RegTri, Cod_ClaTri, Alq_IcmIntReg, Alq_IcmExtReg, Per_RedBasCalIntReg, Per_RedBasCalExtReg, Alq_AgrDebEntIcm, Alq_IcmIntLoc, Alq_IcmExtLoc, Per_RedBasCalIntLoc	,Per_RedBasCalExtLoc	,Ctrl_SujSubTri	,Ctrl_BasCalDebEnt	,Alq_AgrDebEnt	,Per_RedBasCalDebEnt	,Ctrl_BasCalDebSai	,Alq_AgrDebSai	,Per_RedBasCalDebSai	,Per_DscTri	,Cod_ImpNotSai	,C_PerRepIcmReg	,C_PerRepIcmLoc	,Msg_Nfs	,Tip_PrcBasDebSbt	,Ctrl_BasCalCreEnt	,Ctrl_BasCalCreSai	,Tip_PrcBasSbtEnt	,Cod_ImpNotEnt	,Tip_PrcBasCreSbtEnt	,Tip_PrcBasCreSbtSai	,Tip_AgrDebSbt	,Tip_AgrSbtEnt	,Per_LimBasCalCreSbtEnt	,Cod_ModBasCalIcmEnt	,Cod_ModBasCalIcmSai	,Cod_ModBasCalIcmSbtEnt	,Cod_ModBasCalIcmSbtSai	,Flg_ImpSbtFntEnt	,Ctr_TriSai	,Tip_PrcBasSbtRecEnt	,Ctrl_BasCalDebSbtRecEnt	,Tip_AgrDebSbtRecEnt	,Alq_AgrDebSbtRecEnt	,Per_RedBasCalDebSbtRecEnt	,Alq_DebSbtRecEnt	,Tip_PrcBasSbtRecSai	,Ctrl_BasCalDebSbtRecSai	,Tip_AgrDebSbtRecSai	,Alq_AgrDebSbtRecSai	,Per_RedBasCalDebSbtRecSai	,Alq_DebSbtRecSai	,Tip_CreSbtEnt	,Tip_CreSbtSai	,Alq_DebSbtEnt	,Alq_DebSbtSai	,Per_LimMinIcmSbtSaiRedDeb	,Per_LimMinIcmSbtEnt	,Cod_CSOSNEnt	,Cod_CSOSNSai	,Per_RedBasCalDebEntOpc	,Per_AcrAlqIntFcpReg	,Per_PrvParIcm	,Per_ResExtSai	,Per_DspCusEnt	,Tip_PrcLimMinIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtEnt	,Per_LimMinBasCalIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtSai	,Alq_AgrLimMinBasCalIcmSbtSai	,Alq_IcmPresumReg	,Alq_IcmPresumLoc	,Obs_FiscalSai	,Alq_IcmDifExtLoc	,Alq_IcmDifExtReg	,Per_ResExtEnt	,Per_AcrAlqExtFcpReg	,Per_AcrAlqIntFcpLoc	,Per_AcrAlqExtFcpLoc	,Per_IcmDiferiExtReg	,Per_IcmDiferiExtLoc	,Cod_MtvDesIcmLoc	,Cod_MtvDesIcmReg	,Flg_DscDesIcmLoc	,Flg_DscDesIcmReg	,Alq_IcmIntPadReg	,Alq_IcmExtPadReg	,Alq_IcmExtPadLoc	,Alq_IcmIntPadLoc	,Per_IcmDesoneExtReg	,Per_IcmDesoneExtLoc	,Tip_PrcLimMinIcmSbtSai	,Per_LimMinIcmSbtSai	,Cod_ClaTriExt	,Id_MsgDesIcmLoc	,Id_MsgDesIcmReg	,Alq_IrfIntLoc)
select 88, Cod_ClaTri, Alq_IcmIntReg, Alq_IcmExtReg, Per_RedBasCalIntReg, Per_RedBasCalExtReg, Alq_AgrDebEntIcm, Alq_IcmIntLoc, Alq_IcmExtLoc, Per_RedBasCalIntLoc	,Per_RedBasCalExtLoc	,Ctrl_SujSubTri	,Ctrl_BasCalDebEnt	,Alq_AgrDebEnt	,Per_RedBasCalDebEnt	,Ctrl_BasCalDebSai	,Alq_AgrDebSai	,Per_RedBasCalDebSai	,Per_DscTri	,Cod_ImpNotSai	,C_PerRepIcmReg	,C_PerRepIcmLoc	,Msg_Nfs	,Tip_PrcBasDebSbt	,Ctrl_BasCalCreEnt	,Ctrl_BasCalCreSai	,Tip_PrcBasSbtEnt	,Cod_ImpNotEnt	,Tip_PrcBasCreSbtEnt	,Tip_PrcBasCreSbtSai	,Tip_AgrDebSbt	,Tip_AgrSbtEnt	,Per_LimBasCalCreSbtEnt	,Cod_ModBasCalIcmEnt	,Cod_ModBasCalIcmSai	,Cod_ModBasCalIcmSbtEnt	,Cod_ModBasCalIcmSbtSai	,Flg_ImpSbtFntEnt	,Ctr_TriSai	,Tip_PrcBasSbtRecEnt	,Ctrl_BasCalDebSbtRecEnt	,Tip_AgrDebSbtRecEnt	,Alq_AgrDebSbtRecEnt	,Per_RedBasCalDebSbtRecEnt	,Alq_DebSbtRecEnt	,Tip_PrcBasSbtRecSai	,Ctrl_BasCalDebSbtRecSai	,Tip_AgrDebSbtRecSai	,Alq_AgrDebSbtRecSai	,Per_RedBasCalDebSbtRecSai	,Alq_DebSbtRecSai	,Tip_CreSbtEnt	,Tip_CreSbtSai	,Alq_DebSbtEnt	,Alq_DebSbtSai	,Per_LimMinIcmSbtSaiRedDeb	,Per_LimMinIcmSbtEnt	,Cod_CSOSNEnt	,Cod_CSOSNSai	,Per_RedBasCalDebEntOpc	,Per_AcrAlqIntFcpReg	,Per_PrvParIcm	,Per_ResExtSai	,Per_DspCusEnt	,Tip_PrcLimMinIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtEnt	,Per_LimMinBasCalIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtSai	,Alq_AgrLimMinBasCalIcmSbtSai	,Alq_IcmPresumReg	,Alq_IcmPresumLoc	,Obs_FiscalSai	,Alq_IcmDifExtLoc	,Alq_IcmDifExtReg	,Per_ResExtEnt	,Per_AcrAlqExtFcpReg	,Per_AcrAlqIntFcpLoc	,Per_AcrAlqExtFcpLoc	,Per_IcmDiferiExtReg	,Per_IcmDiferiExtLoc	,Cod_MtvDesIcmLoc	,Cod_MtvDesIcmReg	,Flg_DscDesIcmLoc	,Flg_DscDesIcmReg	,Alq_IcmIntPadReg	,Alq_IcmExtPadReg	,Alq_IcmExtPadLoc	,Alq_IcmIntPadLoc	,Per_IcmDesoneExtReg	,Per_IcmDesoneExtLoc	,Tip_PrcLimMinIcmSbtSai	,Per_LimMinIcmSbtSai	,Cod_ClaTriExt	,Id_MsgDesIcmLoc	,Id_MsgDesIcmReg	,Alq_IrfIntLoc
from RTXCT
where cod_regtri = 60 and cod_clatri = 'T1'


COMMIT

ROLLBACK

begin tran
INSERT INTO RTXCT (Cod_RegTri, Cod_ClaTri, Alq_IcmIntReg, Alq_IcmExtReg, Per_RedBasCalIntReg, Per_RedBasCalExtReg, Alq_AgrDebEntIcm, Alq_IcmIntLoc, Alq_IcmExtLoc, Per_RedBasCalIntLoc	,Per_RedBasCalExtLoc	,Ctrl_SujSubTri	,Ctrl_BasCalDebEnt	,Alq_AgrDebEnt	,Per_RedBasCalDebEnt	,Ctrl_BasCalDebSai	,Alq_AgrDebSai	,Per_RedBasCalDebSai	,Per_DscTri	,Cod_ImpNotSai	,C_PerRepIcmReg	,C_PerRepIcmLoc	,Msg_Nfs	,Tip_PrcBasDebSbt	,Ctrl_BasCalCreEnt	,Ctrl_BasCalCreSai	,Tip_PrcBasSbtEnt	,Cod_ImpNotEnt	,Tip_PrcBasCreSbtEnt	,Tip_PrcBasCreSbtSai	,Tip_AgrDebSbt	,Tip_AgrSbtEnt	,Per_LimBasCalCreSbtEnt	,Cod_ModBasCalIcmEnt	,Cod_ModBasCalIcmSai	,Cod_ModBasCalIcmSbtEnt	,Cod_ModBasCalIcmSbtSai	,Flg_ImpSbtFntEnt	,Ctr_TriSai	,Tip_PrcBasSbtRecEnt	,Ctrl_BasCalDebSbtRecEnt	,Tip_AgrDebSbtRecEnt	,Alq_AgrDebSbtRecEnt	,Per_RedBasCalDebSbtRecEnt	,Alq_DebSbtRecEnt	,Tip_PrcBasSbtRecSai	,Ctrl_BasCalDebSbtRecSai	,Tip_AgrDebSbtRecSai	,Alq_AgrDebSbtRecSai	,Per_RedBasCalDebSbtRecSai	,Alq_DebSbtRecSai	,Tip_CreSbtEnt	,Tip_CreSbtSai	,Alq_DebSbtEnt	,Alq_DebSbtSai	,Per_LimMinIcmSbtSaiRedDeb	,Per_LimMinIcmSbtEnt	,Cod_CSOSNEnt	,Cod_CSOSNSai	,Per_RedBasCalDebEntOpc	,Per_AcrAlqIntFcpReg	,Per_PrvParIcm	,Per_ResExtSai	,Per_DspCusEnt	,Tip_PrcLimMinIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtEnt	,Per_LimMinBasCalIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtSai	,Alq_AgrLimMinBasCalIcmSbtSai	,Alq_IcmPresumReg	,Alq_IcmPresumLoc	,Obs_FiscalSai	,Alq_IcmDifExtLoc	,Alq_IcmDifExtReg	,Per_ResExtEnt	,Per_AcrAlqExtFcpReg	,Per_AcrAlqIntFcpLoc	,Per_AcrAlqExtFcpLoc	,Per_IcmDiferiExtReg	,Per_IcmDiferiExtLoc	,Cod_MtvDesIcmLoc	,Cod_MtvDesIcmReg	,Flg_DscDesIcmLoc	,Flg_DscDesIcmReg	,Alq_IcmIntPadReg	,Alq_IcmExtPadReg	,Alq_IcmExtPadLoc	,Alq_IcmIntPadLoc	,Per_IcmDesoneExtReg	,Per_IcmDesoneExtLoc	,Tip_PrcLimMinIcmSbtSai	,Per_LimMinIcmSbtSai	,Cod_ClaTriExt	,Id_MsgDesIcmLoc	,Id_MsgDesIcmReg	,Alq_IrfIntLoc)
select 89, Cod_ClaTri, Alq_IcmIntReg, Alq_IcmExtReg, Per_RedBasCalIntReg, Per_RedBasCalExtReg, Alq_AgrDebEntIcm, Alq_IcmIntLoc, Alq_IcmExtLoc, Per_RedBasCalIntLoc	,Per_RedBasCalExtLoc	,Ctrl_SujSubTri	,Ctrl_BasCalDebEnt	,Alq_AgrDebEnt	,Per_RedBasCalDebEnt	,Ctrl_BasCalDebSai	,Alq_AgrDebSai	,Per_RedBasCalDebSai	,Per_DscTri	,Cod_ImpNotSai	,C_PerRepIcmReg	,C_PerRepIcmLoc	,Msg_Nfs	,Tip_PrcBasDebSbt	,Ctrl_BasCalCreEnt	,Ctrl_BasCalCreSai	,Tip_PrcBasSbtEnt	,Cod_ImpNotEnt	,Tip_PrcBasCreSbtEnt	,Tip_PrcBasCreSbtSai	,Tip_AgrDebSbt	,Tip_AgrSbtEnt	,Per_LimBasCalCreSbtEnt	,Cod_ModBasCalIcmEnt	,Cod_ModBasCalIcmSai	,Cod_ModBasCalIcmSbtEnt	,Cod_ModBasCalIcmSbtSai	,Flg_ImpSbtFntEnt	,Ctr_TriSai	,Tip_PrcBasSbtRecEnt	,Ctrl_BasCalDebSbtRecEnt	,Tip_AgrDebSbtRecEnt	,Alq_AgrDebSbtRecEnt	,Per_RedBasCalDebSbtRecEnt	,Alq_DebSbtRecEnt	,Tip_PrcBasSbtRecSai	,Ctrl_BasCalDebSbtRecSai	,Tip_AgrDebSbtRecSai	,Alq_AgrDebSbtRecSai	,Per_RedBasCalDebSbtRecSai	,Alq_DebSbtRecSai	,Tip_CreSbtEnt	,Tip_CreSbtSai	,Alq_DebSbtEnt	,Alq_DebSbtSai	,Per_LimMinIcmSbtSaiRedDeb	,Per_LimMinIcmSbtEnt	,Cod_CSOSNEnt	,Cod_CSOSNSai	,Per_RedBasCalDebEntOpc	,Per_AcrAlqIntFcpReg	,Per_PrvParIcm	,Per_ResExtSai	,Per_DspCusEnt	,Tip_PrcLimMinIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtEnt	,Per_LimMinBasCalIcmSbtEnt	,Tip_PrcLimMinBasCalIcmSbtSai	,Alq_AgrLimMinBasCalIcmSbtSai	,Alq_IcmPresumReg	,Alq_IcmPresumLoc	,Obs_FiscalSai	,Alq_IcmDifExtLoc	,Alq_IcmDifExtReg	,Per_ResExtEnt	,Per_AcrAlqExtFcpReg	,Per_AcrAlqIntFcpLoc	,Per_AcrAlqExtFcpLoc	,Per_IcmDiferiExtReg	,Per_IcmDiferiExtLoc	,Cod_MtvDesIcmLoc	,Cod_MtvDesIcmReg	,Flg_DscDesIcmLoc	,Flg_DscDesIcmReg	,Alq_IcmIntPadReg	,Alq_IcmExtPadReg	,Alq_IcmExtPadLoc	,Alq_IcmIntPadLoc	,Per_IcmDesoneExtReg	,Per_IcmDesoneExtLoc	,Tip_PrcLimMinIcmSbtSai	,Per_LimMinIcmSbtSai	,Cod_ClaTriExt	,Id_MsgDesIcmLoc	,Id_MsgDesIcmReg	,Alq_IrfIntLoc
from RTXCT
where cod_regtri = 65 and cod_clatri = 'T'

--60 -> 88
--65 -> 89
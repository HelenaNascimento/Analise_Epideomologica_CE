 Select (  it.Vlr_TotItem - IsNull(it.Vlr_DescRateado,0) - IsNull(it.Vlr_DscTri,0) + IsNull(it.Vlr_DespRateada,0)
                            + IsNull(it.Vlr_SubsTrib,0) + IsNull(it.Vlr_SbtRes,0)+ IsNull(it.Vlr_DifTri,0) + IsNull(it.Vlr_Ipi,0)
                            + IsNull(it.Vlr_Suframa,0) + IsNull(it.Vlr_Frete,0) + IsNull(it.Vlr_Seguro,0) + IsNull(it.Vlr_DspExt,0)

                            + IsNull(it.Vlr_DspCusEnt,0) + IsNull(it.Vlr_AntRec,0) 
                            - (Case 
                                 When IsNull(it.Flg_UsaCrePresum,0) = 0 then IsNull(it.Vlr_IcmsTrib,0) 
                                 Else IsNull(it.Vlr_BasIcmsTrib,0)*IsNull(it.Alq_IcmPresum,0)/100 
                               End)    
                            - IsNull(it.Vlr_Pis,0) - IsNull(it.Vlr_Cofins,0) - IsNull(it.Vlr_DscExtNotFis,0)
                            + IsNull(it.Vlr_DspExtNotFis,0) + IsNull(it.Vlr_FreCte,0)
								 
                            + IsNull(ev.Vlr_DspCusEnt,0) + IsNull(ev.Vlr_AntRec,0) 
                            - (Case 
                                 When IsNull(ev.Flg_UsaCrePresum,0) = 0 then IsNull(ev.Vlr_IcmsTrib,0) 
                                 Else IsNull(ev.Vlr_BasIcmsTrib,0)*IsNull(ev.Alq_IcmPresum,0)/100 
                               End)    
                            - IsNull(ev.Vlr_Pis,0) - IsNull(ev.Vlr_Cofins,0) - IsNull(ev.Vlr_DscExtNotFis,0)
                            + IsNull(ev.Vlr_DspExtNotFis,0) + IsNull(ev.Vlr_FreCte,0) )

                         / (it.Qtd_Pedido+IsNull(it.Qtd_Bonificacao,0) )
                      From NFEIT it
                           left Join NFEIT ev on it.Cod_Estabe  = ev.Cod_Estabe and ev.Protocolo = 0
                                             and it.Cod_Produto = ev.Cod_Produto and it.Cod_Lote = ev.Cod_Lote
                                             and it.Qtd_Pedido  = ev.Qtd_Pedido 
                     Where it.Cod_Estabe = 1 --Estabelecimento
                       and it.Protocolo = 111591 --Protocolo
                       and it.Cod_Produto = 43 -- Cod Produto
                       and it.Cod_Lote = 850548 -- Cod Lote
                       and it.Num_SeqIte = 8 --Seq do Produto na Nota
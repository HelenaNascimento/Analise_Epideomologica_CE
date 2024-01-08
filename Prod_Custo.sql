SELECT 
    p.Codigo, 
    p.Descricao as Des_Produt, 
    p.Unidade_Venda, 
    p.Tip_LisPis,       
    fb.Fantasia as Des_Fabric, 
    pr.Cod_ClaTri, 
    p.Ctrl_Preco, 
    p.Cod_Ean, 
    p.Qtd_Embalagem,        
    pr.Prc_CusLiqEnt, 
    pr.Prc_CusMed, 
    pr.Prc_Venda, 
    pr.Prc_MaxCon, 
    pr.Prc_UltEnt, 
    pr.Per_DscAut,   
    pr.Qtd_Dispon,        
    pr.Qtd_Reserv, 
    pr.Qtd_Avaria, 
    pr.Qtd_Transi, 
    p.Qtd_FraVen, 
    p.Qtd_Embalagem 
From PRXES pr, PRODU p, FABRI fb 
Where pr.Cod_Estabe = 1 
And pr.Cod_Produt = p.Codigo 
And p.Cod_Fabricante = fb.Codigo 
AND p.Flag_ImprClassif1 <> 'N' 
AND ((p.Dat_Cadastro <= '20240108 00:00:00') OR (p.Dat_Cadastro IS NULL) OR (p.Dat_Cadastro = '')) 
AND ((pr.Dat_PrcAtual <= '20240108 00:00:00') OR (Pr.Dat_PrcAtual IS NULL) OR (Pr.Dat_PrcAtual = '')) 
AND pr.Qtd_Reserv > 0 

ORDER BY Des_Produt 
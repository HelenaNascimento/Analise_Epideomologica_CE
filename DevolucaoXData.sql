SELECT 
    FORMAT (Dat_Entrada, 'd') as Dat_Entrada,
    COUNT (Protocolo),
    SUM (Vlr_Nota)
    FROM NFECB
WHERE Cod_Estabe = 1
    AND [Status] = 'F'
    AND Tip_NF = 'D'
group by Dat_Entrada
order by 1
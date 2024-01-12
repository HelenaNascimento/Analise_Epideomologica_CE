/*use PROD_DW
GO

CREATE VIEW VWBI_CLIXNOV AS 


SELECT 
D_Cadastro = case
                    when CL.Data_Cadastro like '%/01/%'  THEN 'JAN'
                    when CL.Data_Cadastro like '%/02/%'  THEN 'FEV'
                    when CL.Data_Cadastro like '%/03/%'  THEN 'MAR'
                    when CL.Data_Cadastro like '%/04/%'  THEN 'ABR'
                    when CL.Data_Cadastro like '%/05/%'  THEN 'MAI'
                    when CL.Data_Cadastro like '%/06/%'  THEN 'JUN'
                    when CL.Data_Cadastro like '%/07/%'  THEN 'JUL'
                    when CL.Data_Cadastro like '%/08/%'  THEN 'AGO'
                    when CL.Data_Cadastro like '%/09/%'  THEN 'SET'
                    when CL.Data_Cadastro like '%/10/%'  THEN 'OUT'
                    when CL.Data_Cadastro like '%/11/%'  THEN 'NOV'
                    when CL.Data_Cadastro like '%/12/%'  THEN 'DEZ'
                end 
FROM CLIEN CL
    INNER JOIN ENXES EN ON CL.CODIGO = EN.COD_CLIENT 
WHERE
    EN.COD_ESTABE = 1 and
    CL.Data_Cadastro >= dateadd(month, -6, getdate())
*/

SELECT
count(Codigo)
FROM CLIEN CL
        INNER JOIN ENXES EN ON CL.Codigo = EN.Cod_Client
WHERE
CL.Data_UltimaFatura <= dateadd(month, -6, getdate())
and CL.Data_UltimaFatura >= dateadd(month, -20, getdate())
and Val_AlvFun < GETDATE();


 select 
        distinct
        Cod_Cliente,
        Razao_Social,
        Fantasia,
        Pessoa,
        Cgc_Cpf,
        Des_Cidade,
        Val_LicSau,
        Mes = 
            case
                when Val_LicSau like '%/01/%' then 'JAN'
                when Val_LicSau like '%/02/%' then 'FEV'
                when Val_LicSau like '%/03/%' then 'MAR'
                when Val_LicSau like '%/04/%' then 'ABR'
                when Val_LicSau like '%/05/%' then 'MAI'
                when Val_LicSau like '%/06/%' then 'JUN'
                when Val_LicSau like '%/07/%' then 'JUL'
                when Val_LicSau like '%/08/%' then 'AGO'
                when Val_LicSau like '%/09/%' then 'SET'
                when Val_LicSau like '%/10/%' then 'OUT'
                when Val_LicSau like '%/11/%' then 'NOV'
                when Val_LicSau like '%/12/%' then 'DEZ'
            end

    from V_CLIEN 
    where Val_LicSau like ('%/2024')
    order by 7;


 select 
       Cod_Cliente = count(Cod_Cliente),
        Mes = 
            case
                when month(Val_LicSau) = '01' then 'JAN'
                when month(Val_LicSau) = '02'then 'FEV'
                when month(Val_LicSau) = '03' then 'MAR'
                when month(Val_LicSau) = '04' then 'ABR'
                when month(Val_LicSau) = '05' then 'MAI'
                when month(Val_LicSau) = '06' then 'JUN'
                when month(Val_LicSau) = '07' then 'JUL'
                when month(Val_LicSau) = '08' then 'AGO'
                when month(Val_LicSau) = '09' then 'SET'
                when month(Val_LicSau) = '10' then 'OUT'
                when month(Val_LicSau) = '11' then 'NOV'
                when month(Val_LicSau) = '12' then 'DEZ'
            end

    from V_CLIEN 
    where year(Val_LicSau) = '2024'
    group by 
     Val_LicSau;

select top 1 Val_LicSau from V_CLIEN 
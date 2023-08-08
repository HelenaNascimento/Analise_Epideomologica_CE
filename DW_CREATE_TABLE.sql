USE TESTE_DW

CREATE TABLE CLIENTE (
    IdCli INT PRIMARY KEY,
    DesCli VARCHAR(200),
    Dat_Cadastro DATE,
    Telefone CHAR (20),
    UF VARCHAR(2),
    Cidade VARCHAR(100),
    Bairro VARCHAR(100),
    Pessoa VARCHAR(1),
    TipCons VARCHAR(100),
    LicSaude VARCHAR (50),
    ValLicSaude DATE,
    Anvisa VARCHAR (100),
    ValAnvisa DATE,
    CRF_CRM VARCHAR (100),
    ValCRF DATE,
    AlvFunc VARCHAR (100),
    ValAlv DATE,
    Limite_Credito NUMERIC(9),
    Dat_UltComp DATE,
    Dat_LimCreAtu DATE,
    Vlr_LimCreAnt DECIMAL (10,4),
    Total_Debito DECIMAL(10,4),
    Suframa VARCHAR (9),
    Cod_RamoAtividade VARCHAR (100)
    
)
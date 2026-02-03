/*
Tabelas

NFSCB
NFECB
NFSIT
TBGEN
CTREC

FS_EMAIL,
FS_MDXML
FS_NFXML


--IP VPN INFARMA: 192.168.30.215
*/

--alterar informação sobre erro
Update FS_NFXML
set
Flg_Processado=1,Flg_Impresso=0,Flg_Cancelado=0, Des_Cstat=100, Des_Xmotivo='Autorizado o uso da NF-e'
where Num_Nota=13624

--Abrir Nota
update NFECB set Status='A'
where Protocolo=41931

--Verificar Informações sobre vendedor
select Dat_Pedido, Cod_Cliente,  * from PDVCB
where Cod_Vendedor=147 and  Dat_Pedido BETWEEN '2019-07-01' and '2019-07-31' and Cod_Cliente=6
 

--Resetar senha de Usuário 
update usuar set senha1=null

--Atualizar Data de Verificação
UPDATE PARAM
SET ULTDATACESSO = GETDATE()+10,
FLGMANUTENCAO = 0

--Consultar Romaneio
select Xml_Mdfe, * from FS_MDXML 

--Alterar chave de acesso em Nota de Entrada
update NFECB
SET Chv_Acesso='26190963400543000388550010004861831902260593'
WHERE Numero=486183

--Calculo de produtos com valor de agregação onde Cod_ModBasCalIcmSbt =! Vlr_BasSbtRes
SELECT Cod_ModBasCalIcmSbt,Vlr_BasSbtRes, Vlr_BasSubsTrib, Vlr_TotItem, Ctrl_Tributacao, Cod_Cfo, * FROM NFSIT 
where num_nota IN (119870,119872,119873,119874,119883,119884,119885,119886,119888,119889,119890,119893,119894,119895,119896,119897,119898,119899,119900,119901)

--
UPDATE NFSIT
	SET Cod_ModBasCalIcmSbt = 3
	where num_nota=27031 and Cod_Estabe=2
	
	

select Protocolo, Chv_Acesso, Vlr_Despesas, Vlr_Mercadoria, Vlr_Desconto, Vlr_Nota, Vlr_Ipi from dbo.NFECB
WHERE Vlr_Ipi > 0  and Vlr_Despesas > 0


---------------------------------------------------------------------
SELECT Chv_Nfe, SUBSTRING(Xml_Nfe, 116,44) FROM FS_NFXML
	WHERE Num_Nota IN (
	
	)

Caso encontrem notas que a chave de acesso seja diferente da chave gravado no XML segue o UPDATE que atualiza os campos atrás do XML.

UPDATE FS_NFXML
SET Chv_Nfe = SUBSTRING(Xml_Nfe, 116,44)
WHERE Num_Nota= 

UPDATE FS_NFXML
	SET FSL.Chv_Nfe = SUBSTRING(FSL.Xml_Nfe, 116,44),
		NFB.Chv_Acesso = FSL.Chv_Nfe
	FROM FS_NFXML AS FSL
	INNER JOIN NFSCB AS NFB
	ON FSL.Num_Nota = NFB.Num_Nota
WHERE FSL.Num_Nota=

--Cancelamento de notas
update FS_NFXML 
set Des_Xmotivo=' Cancelamento de NF-e homologado', Des_Cstat=101
where Num_Nota=25492

update NFSIT 
set Cod_Cfo=''
where Num_Nota=25492


update NFSCB 
set Cod_Cfo1=''
WHERE Num_Nota=25492 and Cod_Estabe=3


update pdvcb 
set
Status1='P', Status2='F', Bloqueio='SR'
where numero=25492

----------------------------------


select vlr_partit,* from CTREC where Num_Documento='495997'

select vlr_partit,* from CTREC where  Num_Documento='495997' and Par_Documento='A'

update CTREC
set vlr_documento=568.1350,
	vlr_partit=568.1350,
	vlr_saldo =568.1350
where  Num_Documento='495997' and Par_Documento='B'


update NFSCB
set
Cod_Agente=341
where Num_Nota=495997

select Flg_Autoriz,* from FS_EMAIL
where Flg_Enviado='0'
and Des_EmailTo <> ''


UPDATE NFSCB
	SET Status = 'F',
		Tip_Saida = 'V',
		Tip_OutSai = ''
		--fin = finalidade
	WHERE Num_Nota = 185978

COMMIT


Rejeição: Grupos Transportador, Veiculo Transporte e Reboque não devem ser informados

update na NFSCB 
COD_PLAVei e UF_PLAVEI = ''


Rejeição: 

rejeição: Informado indevidamente o grupo de ICMS da UF de Destino

observar se o cliente é isento e o tipo de consumo > alterar para consulmidor final 

select Cod_Cfo, * from  NFSIT
WHERE num_nota=36601


select Des_Cstat, Des_Xmotivo,  * from  FS_NFXML
WHERE num_nota=36601

begin tran
update FS_NFXML
SET Des_Cstat=101
where num_nota=373

commit


Select status, Cod_Cfo1, Ret_CStat, Ret_XMotivo, * from NFSCB
WHERE num_nota=36601


begin tran
update FS_NFXML
SET Des_XMotivo ='Cancelamento pelo Emitente'
WHERE num_nota=36601

COMMIT



SELECT Chv_Nfe, SUBSTRING(Xml_Nfe, 116,44) FROM FS_NFXML
	WHERE Num_Nota 
	
	select nfsit.Cod_Produto, nfsit.Alq_Icms,produ.Ctrl_Origem from NFSIT nfsit inner join PRODU produ on
(nfsit.Cod_Produto=produ.Codigo) and nfsit.num_nota=2434

update nfscb
set
Status='F', Tip_Saida='V'
where num_nota=2434

--ABCFARMA(MANTER PREÇO DE PRODUTO)
update ALTPR
set
Prc_Venda=prxes.Prc_Venda
from PRXES prxes inner join ALTPR altpr on (prxes.Cod_Produt=altpr.Cod_Produto)
-------------------------

--Atualizar Base de Teste 

Update USUAR
Set senha1 = ''

update TBGEN
set des_gen = 'http://HOMATC10:8089'
where Cod_Gen = 'Report'


------------------------

SELECT
    T.name AS Tabela,
    C.name AS Coluna
FROM
    sys.sysobjects    AS T (NOLOCK)
INNER JOIN sys.all_columns AS C (NOLOCK) ON T.id = C.object_id AND T.XTYPE = 'U'
WHERE
    C.NAME LIKE '%ultdatfecdia%'
ORDER BY
    T.name ASC
	
------------------------
--exclui linhas duplicadas

DELETE T
FROM
(
SELECT *
, DupRank = ROW_NUMBER() OVER (
              PARTITION BY codigo -- campo
              ORDER BY (SELECT NULL)
            )
FROM AJICM --tabela
) AS T
WHERE DupRank > 1


------------------------

select
fs.Dat_Evento, 
fs.Num_Nota, 
fs.Chv_Nfe, 
Chv_XML =  SUBSTRING(Xml_Nfe, 116,44), 
Chv_Nota = SUBSTRING(Xml_Nfe, 144,6),
nf.chv_acesso,
XMLNF = SUBSTRING(Arquivo, 182,44)
FROM FS_NFXML fs 
	inner join NFSCB nf on fs.Cod_Estabe = nf.Cod_Estabe and fs.Ser_Nota = nf.Ser_Nota and fs.Num_Nota = nf.Num_Nota 
WHERE fs.Cod_Estabe = 0 
		and SUBSTRING(Xml_Nfe, 116,44)  <> Chv_Nfe 
		and Flg_ExpArm = 0  
		and Dat_Evento = '20211227'


BEGIN TRAN
UPDATE NFSCB
SET Chv_Acesso = chv_Nfe
		FROM FS_NFXML FS
		 WHERE FS.Cod_Estabe = 0 
				AND FS.Ser_Nota = '1' 
				AND FS.Num_Nota = NFSCB.Num_Nota 
				AND FS.Chv_Nfe <> Chv_Acesso
				AND Dat_Evento = '20211227'


select distinct aud.Chave1, aud.*
	from AUDIT aud 
			inner join (select Chave1, Cod_Ope, Cod_Audit from AUDIT) aud1 on aud.Chave1 = aud1.Chave1 
			inner join CTREC ct on aud.Chave1 = ct.Cod_Documento
			left join BXREC bx on aud.Chave1 = bx.Cod_Documento and aud.Chave2 = bx.Cod_Lancamento
WHERE 
	aud.Nom_Tabela = 'BXREC' 
	and aud.Cod_Ope  <> aud1.Cod_Ope
	and ct.Status = 'A'
	
order by aud.Chave1 , aud.Transacao

UPDATE TBLPG
SET Des_DirPed = 'C:\Infarma2\teste\GESTAO40\GESTAO40\IN', Des_DirFal = 'C:\Infarma2\teste\GESTAO40\GESTAO40\OUT',
	Des_DirDev = 'C:\Infarma2\teste\GESTAO40\GESTAO40\OUT', Des_DirNot = 'C:\Infarma2\teste\GESTAO40\GESTAO40\OUT',
	Des_DirCan = 'C:\Infarma2\teste\GESTAO40\GESTAO40\OUT', Des_DirCad = 'C:\Infarma2\teste\GESTAO40\GESTAO40\OUT'
where Cod_Estabe = 15 
		and Cod_Layout = 20 
		and Cod_Grupo = 1 


select * from TBPDE
where Isn_Arquiv = 1149971

/*
--CodOpe
I = Inclusão 
E = Exclusão
A = Alteração


silvania@infarma.com.br


B%1[BX$$
*/


select 
	Cod_Estabe, 
	FlgAtuTabPrc, 
	FlgAtuPrcCusCadPro, 
	FlgAtuTabPrcEnt, 
	FlgBlqAltTabPrcCot, 
	FlgBlqAltTabPrcPdv, 
	FlgBlqCriPrcVen, 
	FlgBlqFecNfeDivPrc,
FlgCotPrcPrd from PARAM 




Infarma@060115.

SELECT * FROM TBGEN
WHERE Cod_Gen = 'REPORT'

Select distinct des_versao, Des_ScriptExtra from PARAM


update PARAM
set FlgAtuTabPrc = 0 

UPDATE TBGEN
set Des_Gen = 'http://HOMATC10:8085'
where Cod_Gen = 'report'

UPDATE USUAR
SET Senha1 = ''

UPDATE FS_PARAM
SET Val_Param = 2 -- 2 para Homologaçõa e 1 para Produção 
where Cod_Param = 'NFE_CFG_AMB'

UPDATE PARAM
SET 
	ULTDATACESSO = GETDATE()+10, 
	FLGMANUTENCAO = 0, 
	FlgAtuTabPrc = 0 


sqlcmd -S HOMATC10\SQL2019 -U sa -P inf@2016




--PDV
--ELGIN

update ESTAB
set Des_AssDigSat = 'CODIGO DE VINCULACAO AC DO MFE-CFE', 
	Num_Cnpj = '14200166000166', 
	Num_Inscri = '1234567890'
where Cod_Estabe = 0

INSERT INTO MODEX VALUES (HASHBYTES('MD5','37459962000113'),HASHBYTES('MD5','WMS'));


Cadastro de lojas / estabelecimento
CNPJ: 14200166000166 - cnpj deve ser esse
Inscrição 1234567890 - inscrição, por validar a inscrição pode ser que tenha que inserir pelo banco
Chave Vinc=CODIGO DE VINCULACAO AC DO MFE-CFE (campo Des_AssDigSat cadastro de lojas)


INSERT INTO MODEX VALUES (HASHBYTES('MD5','14200166000166'),HASHBYTES('MD5','ECF'));


--TESTE INTEGRADOR
CNPJ ESTAB: 13931743000127
IE: 06.582652-3


vmdpdv.ini (pdv 1.0) ou alt+F3 (pdv 2.0)
sh=10615281000140
producao=1


/*
--Conexão NeoSul - Servidor de Testes

Usuário: infarma2
Senha: Neosul@2021


IP=SRV03
DB=DMD_TESTE
Senha:t$gmminf

N'DMDApp', N'DMD20051643'



*/

Versão: 2011 |Acesso:HOMATC10\SQL2019  ou 192.168.30.215  sa - inf@2016 |Base: DMD_REPF
Versão: 2011 |Acesso: 192.168.16.249\sql2022 sa - Infarma@060115. |Base: Acripel |Estabelecimento: 1

Teste Encontrou Falhas ( DATA )
Versão: |Acesso: |Base:
Descrição:


Testes Pausados ( DATA )
Motivo:

Teste Realizado Com Sucesso (Data)
Versão: |Acesso: |Base:
Funcionalidade:

Cenário:
Dado
E
Quando:
Então:

Teste Realizado com Sucesso em base de produção (Data)
Versão:
Descrição:

Teste Realizado Com Observações (Data)
Base:
Versão:
Descrição:


Tarefa Retornada (Data)
Versão: Acesso:
Descrição:
Base:


Versão:  Base:  Acesso:

silvania.nascimento
1234


inf@2016
@Inf#2016.
inf@2016

@Inf#2015.


silvania.helena@infarma.com.br



Obs.: Hamachi: HOMATC10.1-  inf@2016  Sevidor: HOMATC10 - inf@2016 - Base: PLENA




StartControl - Jarel
jarel.ferreira@infarma.com.br
251289Jj

Versão:
Descrição:

--CNPJ INFARMA: 10224587000176
/*
Conexão VPN NEOSUL

Usuário: infarma2
Senha: Neosul@2021


IP=SRV03
DB=DMD_TESTE
Senha:t$gmminf
*/


INFARMA – SupInf@23.
ADMIN - 
v20.11@IN
Senha1.
500071

admin

!@#$5678Aa




\\192.168.16.10\Volume_1\Setores\Desenvolvimento\Versoes_InfarmaGerador\2011



10224587000176Senha1.
Teste@2


--24455677000182


CECILIA - !@#$5678Aa - NEOSUL

32309


gijode2287@wifame.com


Email: silvania.helena@infarma.com.br
senha: @Inf#2016.
Nome do servidor SMTP smtp-mail.outlook.com
Porta SMTP 587
Método de criptografia SMTP STARTTLS


select top 10
	pes.Cod_Produt, 
	max(Qtd_Dispon) as Disponivel, 
	format(Prc_Venda, 'c') as PRC_VENDA 
from PRXES pes
	inner join PCXPR ppr on pes.Cod_Produt = ppr.Cod_Produt
where Cod_Estabe = 0 and Qtd_Dispon >0 and Flg_BlqVen = 0 and ppr.Id_PolCom = 2
group by pes.Cod_Produt, Prc_Venda
order by Prc_Venda desc




SELECT
    T.name AS Tabela,
    C.name AS Coluna
FROM
    sys.sysobjects    AS T (NOLOCK)
INNER JOIN sys.all_columns AS C (NOLOCK) ON T.id = C.object_id AND T.XTYPE = 'U'
WHERE
    C.NAME LIKE '%ARMAZ%'
ORDER BY
    T.name ASC
	
exec PR_RetornaItensNF 1, 423741,'1', 'S'


BEGIN TRAN
INSERT INTO PEXRO (Cod_Perfil, Isn_Rotina, Flg_Ativa)
SELECT Cod_Perfil = 38, ISN_ROTINA, FLG_ATIVA FROM PEXRO
WHERE Cod_Perfil = 8

COMMIT


//ERRO PROCESSA / DIVISÃO POR ZERO / SPED

SELECT 
	distinct
	Cod_Produto
FROM NFECB CB
	join NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO 
WHERE CB.COD_ESTABE = 4
AND FATOR_FAT = ''
AND Dat_Entrada >='20260101'
AND Dat_Entrada <='20260131'

-- 6927


commit

BEGIN TRAN
update IT
SET FATOR_FAT = 1
FROM NFECB CB
	join NFEIT IT ON CB.COD_ESTABE = IT.COD_ESTABE AND CB.PROTOCOLO = IT.PROTOCOLO 
WHERE CB.COD_ESTABE = 4
AND Cod_Produto = 12512
AND FATOR_FAT = 0
AND Dat_Entrada >='20260101'
AND Dat_Entrada <='20260131'
EPG/*
Ponto
Copyright © 2007  FORTES Informática

Script de criação da base de dados INTERBASE

Importante:
-------------------------------------------------------------------------------------------
Utilizando o runscript para criar o banco, facilita a identificação de erros no script

 - A Pasta padrão do Banco de Dados é "C:\Ponto".

*/
.title 'Criação da Base de Dados'

.set ScriptVersion=252
.set VersionTable='INFO'
.set VersionField='VersaoBD'
.set StatusField='StatusBD'

.dbprop   DriverName='INTRBASE'
.dbparams 'SERVER NAME=C:\Ponto\Ponto.gdb'
.dbparams 'USER NAME=SYSDBA'
.dbparams 'PASSWORD=masterkey'
.dbparams 'LANGDRIVER=BLLT1PT0'

.connect

.begin

create domain boolean as int check(value is null or value in (0, 1));
.go

/*INFO*/
create table INFO (VERSAOBD  INTEGER,
                   STATUSBD  VARCHAR(10),
                   SISTEMA   VARCHAR(20),
                   VERSAOBDBETA INTEGER default 0 not null,
                   ATUALIZANDO boolean default 0 not null,
                   FORTES boolean default 0 not null,
                   CONVERTEPONTO3 boolean default 1);
.go

/* Usuários */
create table USU (Codigo       varchar(20) not null, /* Ex.: RAFAELA */
                  Senha        int not null,
                  UltimoAcesso timestamp,
                  Bloqueado    boolean default 0 not null,
                  constraint pk_USU primary key (Codigo));
.go

create table MUSU (Id integer not null,
                   USU_Codigo varchar(20) not null,
                   constraint PK_MUSU primary key (Id, USU_Codigo));
.go

/*empresas*/
create table EMP(Codigo         varchar(4)  not null,
                 Nome           varchar(15) not null,
                 RazaoSocial    varchar(60),
                 CNPJBase       varchar(8),
                 LogoRelat      blob SUB_TYPE 0,
                 USU_CODIGO     varchar(20),
                 CONVERTETIPOHE boolean default 1,
                 CPF            VARCHAR(11),
                 CEI            VARCHAR(12),
                 DTENCERRAMENTO TimeStamp,
                 Ultima_Atualizacao_AC Timestamp,
                 Falta_Ajustar_No_AC boolean default 0 not null,
                 ADERIU_ESOCIAL boolean default 0 not null,
                 DATA_ADESAO_ESOCIAL Timestamp,
                 DATA_ADESAO_ESOCIAL_F2 Timestamp,
                 TP_AMB_ESOCIAL integer,
                 STATUSENVIOAPP	INTEGER DEFAULT 0 NOT NULL,
                 NMFANTASIA varchar(40),
		 CNPJLICENCIADO varchar(14),
                 constraint pk_EMP      primary key (Codigo),
                 constraint uk_EMP_Nome unique (Nome),
                 constraint FK_EMP_USU foreign key (USU_CODIGO) references USU (CODIGO));
.go

create table CFE (EMP_Codigo varchar(4)   not null,
                  Codigo     varchar(50)  not null,
                  Valor      varchar(255) not null,
                  constraint pk_CFE     primary key (EMP_Codigo,Codigo),
                  constraint fk_CFE_EMP foreign key (EMP_Codigo) references EMP(Codigo));
.go

create table CFU (USU_CODIGO  VARCHAR(20) not null,
                  CODIGO      VARCHAR(50) not null,
                  VALOR       VARCHAR(255) not null,
                  constraint PK_CFU primary key (USU_CODIGO, CODIGO),
                  constraint FK_CFU_USU foreign key (USU_CODIGO) references USU (CODIGO));
.go

/*Tabela generica de lote*/
create table LOTE (
  EMP_CODIGO    varchar(4) not null,
  ID            Integer not null,
  DTHORAGERACAO timestamp not null,
  DTINICIAL     timestamp not null,
  DTFINAL       timestamp not null,
  OBS           varchar(255) not null,
  DESTINOLOTE   Integer not null,
  constraint PK_LOTE primary key (EMP_CODIGO, ID),
  constraint FK_LOTE_EMP foreign key (EMP_CODIGO) references EMP(CODIGO)
);
.go

create table RBA (EMP_Codigo varchar(4)  not null,
                  Codigo     varchar(3)  not null,
                  Nome       varchar(60) not null,
                  constraint pk_RBA primary key(EMP_Codigo, Codigo),
                  constraint uk_RBA_Nome unique(EMP_Codigo, Nome),
                  constraint fk_RBA_EMP foreign key (EMP_Codigo) references EMP(Codigo));
.go

create table DRB (EMP_Codigo varchar(4) not null,
                  RBA_codigo varchar(3) not null,
                  Ordem integer not null,
                  constraint PK_DRB primary key (EMP_Codigo, RBA_Codigo, Ordem),
                  constraint fk_DRB_EMP foreign key (EMP_codigo) references EMP (codigo),
                  constraint fk_DRB_RBA foreign key (EMP_codigo, RBA_codigo) references RBA (Emp_Codigo,codigo) on delete cascade);
.go

create table TRB (EMP_Codigo varchar(4) not null,
                  DRB_RBA_codigo varchar(3) not null,
                  DRB_Ordem integer not null,
                  restinicial timestamp not null,
                  restfinal timestamp not null,
                  turno integer not     null,
                  constraint PK_TRB primary key (EMP_Codigo, DRB_RBA_Codigo, DRB_Ordem, restinicial),
                  constraint fk_TRB_DRB foreign key (EMP_Codigo, DRB_RBA_Codigo, DRB_Ordem) references DRB (EMP_Codigo, RBA_Codigo, Ordem) on delete cascade);
.go

/* Restrições de Batidas da Henry */
create table rbh
(
emp_codigo varchar(4) not null,
codigo varchar(6) not null,
nome varchar(60) not null,
constraint PK_RBH primary key (emp_codigo, codigo),
constraint FK_RBH_EMP foreign key (EMP_CODIGO) references emp (CODIGO)
);
.go

/* Restrições de Batidas da Henry */
create table prh
(
emp_codigo varchar(4) not null,
id integer not null,
hora timestamp not null,
tolerancia integer not null,
domingo integer,
segunda integer,
terca integer,
quarta integer,
quinta integer,
sexta integer,
sabado integer,
constraint PK_PRH primary key (emp_codigo, id),
constraint FK_PRH_EMP foreign key (EMP_CODIGO) references emp (CODIGO)
);
.go

/* Restrições de Batidas da Henry */
create table RPH
(
emp_codigo varchar(4) not null,
RBH_Codigo varchar(6) not null,
PRH_Id integer not null,
constraint PK_RPH primary key (emp_codigo, RBH_Codigo, PRH_Id),
constraint FK_RPH_RBH foreign key (EMP_CODIGO, RBH_CODIGO) references rbh (EMP_CODIGO, CODIGO),
constraint FK_RPH_PRH foreign key (EMP_CODIGO, PRH_ID) references prh (EMP_CODIGO, ID)
);
.go

/*empregados*/
create table EPG (EMP_Codigo               varchar(4) not null,
              	  Codigo                   varchar(6)     not null,
                  Nome                     varchar(70),
                  DtAdmissao               timestamp,
                  DtRescisao               timestamp,
				  DtTransferencia          timestamp,
                  CTPSNumero               varchar(11), /* Número da carteira de trabalho */
                  CTPSSerie                varchar(6), /* Série da carteira de trabalho */
                  USU_Codigo               varchar(20), /* Usuário associado ao empregado */
                  NUM_Chapa                varchar(10),
                  RBH_Codigo               varchar(6),
                  DtBancoDeHoras           timestamp,
                  origem                   integer,
                  SENSIBILIDADE            integer,
                  FOTO                     BLOB SUB_TYPE 0 SEGMENT SIZE 80,
                  DIAPREFFOLGA             integer,
                  PIS                      varchar(11),
                  DtNascimento             timestamp,
                  IdentidadeNumero         varchar(15),
                  IdentidadeOrgaoExpedidor varchar(20),
                  Matricula                varchar(20),
                  DDD                      varchar(4),
                  Fone                     varchar(9),
                  Celular                  varchar(9),
                  CPF                      Varchar(11),
                  Enviado_Relogio          Integer,
                  eMail                    varchar(60),
                  STATUSENVIOAPP           INTEGER DEFAULT 0 NOT NULL,
                  TimestampApp             varchar(15),
                  BATERPONTOAPP            boolean default 0 not null,
				  SOLICITAABONOAPP         boolean default 0 not null,
				  EXIGECERCAAPP            integer default 2 not null constraint CC_EPG_EXIGECERCAAPP check(EXIGECERCAAPP in (0, 1, 2)),
				  STATUSENVIOEMPREGADOAPP  integer default 0 not null,
				  STATUSSOLICITAABONOAPP   integer default 0 not null,
				  STATUSEXIGECERCAAPP      integer default 0 not null constraint CC_EPG_STATUSEXIGECERCAAPP check(STATUSEXIGECERCAAPP in (0, 1)),
				  NUM_CARTAO_CATRACA       varChar(14),  /* Catraca fortes */
                  constraint pk_EPG  primary key (EMP_Codigo,Codigo),
                  constraint fk_EPG_EMP foreign key (EMP_Codigo) references EMP(Codigo),
                  constraint fk_EPG_RBH foreign key (EMP_Codigo, RBH_Codigo) references RBH(EMP_Codigo,Codigo),
                  constraint fk_EPG_USU foreign key (USU_Codigo) references USU(Codigo));
				 
.go

create table mEPG (Id          integer    not null,
                   EMP_Codigo  varchar(4) not null,
                   EPG_Codigo  varchar(6) not null,
                   constraint pk_MEPG primary key (Id,EMP_Codigo,EPG_Codigo));
.go

create table CLASSE(
  SIGLA      varchar(30) not null,
  DESCRICAO  varchar(100) not null,
  ORGAO 	 integer,
  constraint pk_classe primary key (SIGLA));
.go

/*cargos*/
create table CAR (EMP_Codigo varchar(4)  not null,
                  Codigo     varchar(3)  not null,
                  Nome       varchar(100) default '' not null,  /* Ex.: Analista de Sistemas */
                  SIGLA_CLASSE varchar(30),
                  constraint pk_CAR primary key (EMP_Codigo,Codigo),
                  constraint fk_CAR_EMP foreign key (EMP_Codigo) references EMP(Codigo),
                  constraint fk_CAR_SIGLA foreign key (SIGLA_CLASSE) references CLASSE(SIGLA));
.go

create table mCAR (Id          integer    not null,
                   EMP_Codigo  varchar(4) not null,
                   CAR_Codigo  varchar(3) not null,
                   constraint pk_MCAR primary key (Id,EMP_Codigo,CAR_Codigo));
.go

CREATE TABLE MHOR (
    ID          INTEGER NOT NULL,
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    HOR_CODIGO  VARCHAR(6) NOT NULL,
    CONSTRAINT PK_MHOR PRIMARY KEY (ID, EMP_CODIGO, HOR_CODIGO));
.go


/*Lotações*/
create table LOT (EMP_Codigo     varchar(4) not null,
                  Codigo         varchar(10) not null,
                  Nome           varchar(60) not null, /* Ex.: Setor Pessoal */
                  LOT_Codigo_Mae varchar(10),          /* Lotação de nível hierárquico imediatamente superior */
                  INTEGRANTES_SESMT BLOB SUB_TYPE 1 segment size 80,
                  INTEGRANTES_PLANTAO BLOB SUB_TYPE 1 segment size 80,
                  constraint pk_LOT     primary key (EMP_Codigo,Codigo),
                  constraint fk_LOT_EMP foreign key (EMP_Codigo) references EMP(Codigo));
.go

create table mLOT (Id          integer    not null,
                   EMP_Codigo  varchar(4) not null,
                   LOT_Codigo  varchar(10) not null,
                   constraint pk_MLOT primary key (Id,EMP_Codigo,LOT_Codigo));
.go

/* Unidade de Federação */
create table UFD (Sigla  varchar(2)  not null,
                  Nome   varchar(20) not null,
                  Codigo varchar(2)  not null,
                  constraint pk_UFD        primary key (Sigla),
                  constraint uk_UFD_Nome   unique (Nome),
                  constraint uk_UFD_Codigo unique (Codigo));
.go

/* Município */
create table MUN (UFD_Sigla  varchar(2)  not null,
                  Codigo     varchar(5)  not null,
                  Nome       varchar(30) not null,
                  constraint pk_MUN       primary key (UFD_Sigla,Codigo),
                  constraint uk_MUN_UF_Nm unique (UFD_Sigla,Nome),
                  constraint fk_MUN_UFD   foreign key (UFD_Sigla) references UFD(Sigla));
.go

/*Cadastro de Obra / Tomadores */
CREATE TABLE TOM (
    EMP_CODIGO      VARCHAR(4) NOT NULL,
    CODIGO          VARCHAR(6) NOT NULL,
    NOME            VARCHAR(40) NOT NULL,
    TIPOTOMADOR     INT NOT NULL,
    TIPOINSCRICAO   INT NOT NULL,
    INSCRICAO       VARCHAR(14),
    ENDLOGRADOURO   VARCHAR(40),
    ENDNUMERO       VARCHAR(5),
    ENDCOMPLEMENTO  VARCHAR(20),
    BAIRRO          VARCHAR(20),
    CEP             VARCHAR(8),
    MUN_UFD_SIGLA   VARCHAR(2),
    MUN_CODIGO      VARCHAR(5),
    DDD             VARCHAR(4),
    FONE            VARCHAR(8),
    EMAIL           VARCHAR(40),
    ORIGEM          INT,
    constraint pk_TOM primary key(EMP_Codigo, Codigo),
    constraint fk_TOM_EMP foreign key (EMP_CODIGO) references EMP(CODIGO),
    constraint fk_TOM_MUN foreign key (MUN_UFD_SIGLA, MUN_CODIGO) references MUN(UFD_SIGLA, CODIGO)
);
.go

/* Tabela de Estabelecimentos */
CREATE TABLE EST (
    EMP_CODIGO                  VARCHAR(4) NOT NULL,
    CODIGO                      VARCHAR(4) NOT NULL,
    NOME                        VARCHAR(30) NOT NULL,
    SEQCNPJ                     VARCHAR(4),
    ENDLOGRADOURO               VARCHAR(40),
    ENDNUMERO                   VARCHAR(10),
    ENDCOMPLEMENTO              VARCHAR(20),
    BAIRRO                      VARCHAR(20),
    CEP                         VARCHAR(8),
    MATRIZ                      INTEGER default 0 NOT NULL,
    mun_ufd_sigla               varchar(2),
    mun_codigo                  varchar(5),
	HABILITACERCA               boolean default 0 not null,
	LATITUDE                    varchar(25),
	LONGITUDE                   varchar(25),
	RAIO                        INTEGER default 100 NOT NULL,
    constraint PK_EST primary key (EMP_CODIGO, CODIGO),
    constraint UK_EST_NOME unique (EMP_CODIGO, NOME),
    constraint CC_EST_MATRIZ check(Matriz in (0,1)),
    constraint FK_EST_EMP foreign key (EMP_CODIGO) REFERENCES EMP (CODIGO),
    constraint FK_EST_MUN foreign key (MUN_UFD_SIGLA, MUN_CODIGO) REFERENCES MUN (UFD_SIGLA, CODIGO)
);
.go

create table mEST (Id          integer    not null,
                   EMP_Codigo  varchar(4) not null,
                   EST_Codigo  varchar(4) not null,
                   constraint pk_MEST primary key (Id,EMP_Codigo,EST_Codigo));
.go

create table mTOM (Id         integer not null,
                  EMP_Codigo  varchar(4) not null,
                  TOM_Codigo  varchar(6) not null,
                  constraint pk_MTOM primary key (Id, EMP_Codigo, TOM_Codigo));
.go

/*horários*/
create table HOR (EMP_Codigo varchar(4)  not null,
                  Codigo     varchar(6)  not null,
                  Nome       varchar(60) not null,
                  Escala     integer not null,
                  TIPOHORARIO  VARCHAR(1) DEFAULT 'X' NOT NULL,
                  TipoBtpLim varchar(1) default 'P' not null,
                  Sigla varchar(6),
                  SempreLancarPreAssinalada Boolean default 0 not null,
                  LimiteNoDia Boolean default 0 not null,
                  DSRDiaUtil Boolean,
                  StatusPessoal integer,
                  TIPOHORARIOESOCIAL integer not null,
                  Intervalo_Minimo Timestamp default '1900-1-1 00:00:00' not null,
                  OBS blob sub_type 0,
                  DESATIVADO boolean default 0 not null,
                  PreAssinaladaDiaFeriado Boolean default 0 not null,
                  constraint pk_HOR primary key(EMP_Codigo, Codigo),
                  constraint uk_HOR_Nome unique(EMP_Codigo, Nome),
                  constraint fk_HOR_EMP foreign key (EMP_Codigo) references EMP(Codigo),
                  CONSTRAINT CC_HOR_TIPOHORARIO CHECK(TIPOHORARIO in ('X','F')));
.go

create table XXX_HOG (EMP_Codigo varchar(4)  not null,
                  Codigo     varchar(3)  not null,
                  Nome       varchar(60) not null,
                  Escala     integer not null,
                  constraint pk_XXX_HOG primary key(EMP_Codigo, Codigo),
                  constraint uk_XXX_HOG_Nome unique(EMP_Codigo, Nome),
                  CONSTRAINT FK_XXX_HOG_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP(CODIGO) on delete CASCADE);
.go

/* Dia de Horario */
create table DHO (EMP_Codigo varchar(4) not null,
                  HOR_codigo varchar(6) not null,
                  Ordem integer not null,
                  BTPLIMITE timestamp,
                  CH VARCHAR(4),
                  VT BOOLEAN,
                  VR BOOLEAN,
                  Dia_Util BOOLEAN,
                  PreAssinaladaDiaFolga Boolean default 0 not null,
                  constraint PK_DHO primary key (EMP_Codigo, HOR_Codigo, Ordem),
                  constraint fk_DHO_EMP foreign key (EMP_codigo) references EMP (codigo),
                  constraint fk_DHO_HOR foreign key (EMP_codigo, HOR_codigo) references HOR (Emp_Codigo,codigo) on delete cascade);
.go

create table XXX_DHG (EMP_Codigo varchar(4) not null,
                  HOG_codigo varchar(3) not null,
                  Ordem integer not null,
                  BTPLIMITE timestamp,
                  constraint PK_XXX_DHG primary key (EMP_Codigo, HOG_Codigo, Ordem),
                  CONSTRAINT FK_XXX_DHG_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP(CODIGO) on delete CASCADE);
.go

/* Turno de Dia de Horario */
create table TDH (EMP_Codigo      varchar(4) not null,
                  DHO_HOR_codigo  varchar(6) not null,
                  DHO_Ordem       integer not null,
                  btpinicial      timestamp not null,
                  btpfinal        timestamp not null,
                  intbtpinicial   integer  default 0 not null,
                  intbtpfinal     integer default 0 not null,
                  turno           integer not null,
                  TOLENTRADA      timestamp,
                  TOLSAIDA        timestamp,
                  FLEXIVELBTPINI  BOOLEAN DEFAULT 0 NOT NULL,
                  FLEXIVELBTPFIM  BOOLEAN DEFAULT 0 NOT NULL,
                  CURSOAPRENDIZBTPINI  BOOLEAN DEFAULT 0 NOT NULL,
                  CURSOAPRENDIZBTPFIM  BOOLEAN DEFAULT 0 NOT NULL,
                  constraint PK_TDH primary key (EMP_Codigo, DHO_HOR_Codigo, DHO_Ordem, btpinicial),
                  constraint fk_TDH_DHO foreign key (EMP_Codigo, DHO_HOR_Codigo, DHO_Ordem) references DHO (EMP_Codigo, HOR_Codigo, Ordem) on delete cascade);
.go

create table XXX_TDG (EMP_Codigo varchar(4) not null,
                  DHG_HOG_codigo varchar(3) not null,
                  DHG_Ordem integer not null,
                  btpinicial timestamp not null,
                  btpfinal timestamp not null,
                  turno integer not null,
                  TOLENTRADA timestamp,
                  TOLSAIDA timestamp,
                  constraint PK_XXX_TDG primary key (EMP_Codigo, DHG_HOG_Codigo, DHG_Ordem, btpinicial),
                  CONSTRAINT FK_XXX_TDG_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP(CODIGO) on delete CASCADE);
.go

/* Feriados Fixos */
create table FFX (
  ID   INT NOT NULL,
  MES  INT NOT NULL,
  DIA  INT NOT NULL,
  NOME VARCHAR(30),
  Nivel         varchar(1) default '1' not null, /* (Federal,1/Estadual,2/Municipal,3) */
  UFD_Sigla     varchar(2),           /* Caso seja Estadual, UF  onde vinga o feriado */
  MUN_Codigo    varchar(5),            /* Caso seja Municipal, Municipio onde vinga o feriado */
  ORIGEM INTEGER default 0,
  DATAVIGOR TIMESTAMP,
  constraint pk_FFX primary key(ID),
  constraint fk_FFX_UFD foreign key (UFD_Sigla) references UFD(Sigla),
  constraint fk_FFX_MUN foreign key (UFD_Sigla, MUN_Codigo) references MUN(UFD_Sigla, Codigo),
  constraint uk_FFX_Nome unique(Nome),
  constraint CC_FFX_NIVEL check(Nivel in ('1','2','3'))
);
.go

create table Evento_FFX (
  FFX_ID   INT NOT NULL,
  EMP_CODIGO Varchar(4) not null,
  EVE_FERIADO Varchar(3),
  constraint pk_Evento_FFX primary key(FFX_ID, EMP_Codigo),
  constraint fk_Evento_FFX_EMP foreign key (EMP_CODIGO) references EMP(CODIGO) on delete CASCADE,
  constraint fk_Evento_FFX_FFX foreign key (FFX_ID) references FFX(ID) on delete cascade
);
.go

/* Feriados Móveis */
create table FMV (
  ID            int         not null,
  Data          timestamp   not null,
  Nome          varchar(30) not null,
  Nivel         varchar(1) default '1' not null, /* (Federal,1/Estadual,2/Municipal,3) */
  UFD_Sigla     varchar(2),
  MUN_Codigo    varchar(5),
  ORIGEM INTEGER default 0,
  constraint pk_FMV primary key(ID),
  constraint fk_FMV_UFD foreign key (UFD_Sigla) references UFD(Sigla),
  constraint fk_FMV_MUN foreign key (UFD_Sigla, MUN_Codigo) references MUN(UFD_Sigla, Codigo),
  constraint CC_FMV_NIVEL check(Nivel in ('1','2','3'))
);
.go

create table Evento_FMV (
  FMV_ID   INT NOT NULL,
  EMP_CODIGO Varchar(4) not null,
  EVE_FERIADO Varchar(3),
  constraint pk_Evento_FMV primary key(FMV_ID, EMP_Codigo),
  constraint fk_Evento_FMV_EMP foreign key (EMP_CODIGO) references EMP(CODIGO) on delete CASCADE,
  constraint fk_Evento_FMV_FMV foreign key (FMV_ID) references FMV(ID) on delete cascade
);
.go

/* Feriados Fixos por Empresa */
create table FFE (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    MESDIA VARCHAR(4) NOT NULL,
    NOME  VARCHAR(30),
    ORIGEM INTEGER default 0,
    DATAVIGOR TIMESTAMP,
    EVE_FERIADO Varchar(3),
    constraint PK_FFE primary key (EMP_CODIGO, MESDIA),
    constraint FK_FFE_EMP foreign key (EMP_CODIGO) references EMP (CODIGO)
);
.go

/* Feriados Móveis por Empresa */
create table FME (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    DATA  timestamp NOT NULL,
    NOME  VARCHAR(30),
    ORIGEM INTEGER default 0,
    DtFeriadoRelacionado timestamp,
    EVE_FERIADO Varchar(3),
    constraint PK_FME primary key (EMP_CODIGO, DATA),
    constraint FK_FME_EMP foreign key (EMP_CODIGO) references EMP (CODIGO)
);
.go

create table FME_DtRelacionada (
    Seq INTEGER not null,
    EMP_CODIGO VARCHAR(4) NOT NULL,
    DATA  timestamp NOT NULL,
    DtFeriadoRelacionado timestamp NOT NULL,
    UFD_Sigla     varchar(2),
    MUN_Codigo    varchar(5),
    constraint PK_FME_DtRelacionada primary key (EMP_CODIGO, DATA, DtFeriadoRelacionado, Seq),
    constraint UK_FME_DtRelacionada_Emp_Data unique (EMP_CODIGO, DATA, DtFeriadoRelacionado, UFD_Sigla, MUN_Codigo),
    constraint FK_FME_DtRelacionada_UFD foreign key (UFD_Sigla) references UFD(Sigla),
    constraint FK_FME_DtRelacionada_MUN foreign key (UFD_Sigla, MUN_Codigo) references MUN(UFD_Sigla, Codigo),
    constraint FK_FME_DtRelacionada_EMP foreign key (EMP_CODIGO) references EMP (CODIGO) on delete cascade
);
.go

/* Tabela Perfil de Eventos */
create table PEV(
  EMP_CODIGO           VARCHAR(4) NOT NULL,
  CODIGO               VARCHAR(3) NOT NULL,
  DESCRICAO            VARCHAR(60),
  EXPATRASOSAIDAANTEC  INTEGER,
  EVE_ATRASOSAIDAANTEC VARCHAR(3),
  EXPFALTA             INTEGER,
  FALTAINTEGRAL        INTEGER,
  FALTASOMAATRASO      INTEGER,
  EVE_FALTA            VARCHAR(3),
  EXPFERIADOTRABALHADO INTEGER,
  FERIADOCOMOHORAEXTRA INTEGER,
  EVE_FERIADO          VARCHAR(3),
  EXPSUSPENSAO         INTEGER,
  SUSPENSAOSOMAATRASO  INTEGER,
  EVE_SUSPENSAO        VARCHAR(3),
  EXPADICIONALNOTURNO  INTEGER,
  EVE_ADICIONALNOTURNO VARCHAR(3),
  EXPHORAEXTRA         INTEGER,
  EXPHORATRABALHADA    INTEGER,
  EVE_HORATRABALHADA   VARCHAR(3),
  FeriadosEspecificos  boolean,
  HR_INICIAL_ADICIONALNOTURNO timestamp,
  HR_FINAL_ADICIONALNOTURNO timestamp,
  OCR_FALTA            VARCHAR(3),
  EXP_FALTA_EVE        INTEGER,
  EXP_FALTA_OCR        INTEGER,
  EXP_GORJETAS         INTEGER,
  EVE_GORJETAS         VARCHAR(3),
  EXP_DSR_DESCONTO     INTEGER,
  EVE_DSR_DESCONTO     VARCHAR(3),
  DESC_ATRASO_DSR_DESCONTO integer default 0 not null,
  EXP_DSR_PROVENTO     INTEGER,
  EVE_DSR_PROVENTO     VARCHAR(3),
  EVE_HORASPREVISTAS   VARCHAR(3),
  EVE_ATRASOSA_ABONADA VARCHAR(3),
  EVE_FALTAS_ABONADAS_HS VARCHAR(3),
  EVE_FALTAS_ABONADAS_DIAS VARCHAR(3),
  EVE_AFASTAMENTOS_DIAS VARCHAR(3),
  EVE_AFASTAMENTOS_HS VARCHAR(3),
  EVE_FALTA_HS VARCHAR(3),
  DESC_FERIADO_DSR_DESCONTO integer default 0 not null,
  EXP_SALDO_BH INTEGER default 0 not null,
  EVE_SALDO_BH_CREDITO VARCHAR(3),
  EVE_SALDO_BH_DEBITO VARCHAR(3),
  DESC_FALTA_FRAC_DSR_DESCONTO integer default 0 not null,
  EXP_DESC_FALTAS_HS Integer default 0 not null,
  EXP_DESC_FALTAS_HS_Integral Integer default 0 not null,
  EVE_DESC_FALTAS_HS VARCHAR(3),
  EVE_HORASOBRIGATORIA VARCHAR(3),
  OCR_SUSPENSAO        VARCHAR(3),
  EXP_SUSPENSAO_EVE    INTEGER default 1,
  EXP_SUSPENSAO_OCR    INTEGER default 0,
  EXP_FOLGA_TRABALHADA_DIARIA INTEGER default 0 not null,
  EVE_FOLGA_TRABALHADA_DIARIA VARCHAR(3),
  Separar_Horas_Feriado Boolean default 0 not null,
  EVE_SALDO_BH_ADICIONADO VARCHAR(3),
  EVE_SALDO_BH_COMPENSADO VARCHAR(3),
  ENVIA_REF_OCR_SUSPENSAO Boolean default 0 not null,
  EXP_SALDO_BH_ESOCIAL INTEGER default 0 not null,
  EVE_SALDO_BH_INICIAL_CREDITO VARCHAR(3),
  EVE_SALDO_BH_INICIAL_DEBITO VARCHAR(3),
  AGRUPA_HE_PERC_E_EVE Boolean default 0 not null,
  Usar_Turno_Estendido Boolean default 0 not null,
  DESC_PREV_FERIADO boolean default 0 not null,
  RCO_CODIGO VARCHAR(4),
  EXPFERIADOTRABALHADODOCADASTRO INTEGER,
  constraint PK_PEV primary key(EMP_CODIGO,CODIGO),
  constraint FK_PEV_EMP foreign key (EMP_CODIGO) references EMP (CODIGO)
);
.go

CREATE TABLE HEA(
  EMP_CODIGO    VARCHAR(4) NOT NULL,
  CODIGO        VARCHAR(2) NOT NULL,
  DESCRICAO     VARCHAR(30),
  PERCENTUAL    FLOAT NOT NULL,
  EVE_HORAEXTRA VARCHAR(3),
  CONSTRAINT PK_HEA PRIMARY KEY(EMP_CODIGO, CODIGO),
  CONSTRAINT FK_HEA_EMP FOREIGN KEY(EMP_CODIGO) REFERENCES EMP(CODIGO)
);
.go

CREATE TABLE FRC ( 
  EMP_CODIGO          VARCHAR(4) NOT NULL,
  CODIGO              VARCHAR(3) NOT NULL,
  DESCRICAO           VARCHAR(100) NOT NULL,
  HEA_CODIGO          VARCHAR(2),
  DESTINO             INTEGER NOT NULL,
  EVE_CONVDEVEDOR     VARCHAR(3),
  EVE_CONVCREDOR      VARCHAR(3),
  LIMITEHORASCREDITO  TIMESTAMP,
  LIMITEHORASDEBITO   TIMESTAMP,
  EXISTEEXCECOES      BOOLEAN DEFAULT 0 NOT NULL,
  DESCSUSFOLHA        BOOLEAN DEFAULT 0 NOT NULL,
  USATOLERANCIADIARIA BOOLEAN DEFAULT 0 NOT NULL,
  TOLERANCIADIARIA    TIMESTAMP,
  AplicaPercentualNoDia BOOLEAN DEFAULT 0 NOT NULL,
  UsaToleranciaCadastroHorarios boolean default 0 not null,
  EVE_InItinere       Varchar(3),
  EXP_InItinere       Boolean default 0 not null,
  Usar_Tolerancia_Apos_Flex Boolean default 0 not null,
  Origem_Evento_Hora_Extra Integer default 0 not null,
  Limite_Hora_Extra TimeStamp,
  Origem_Evento_Atraso_SA Integer default 0 not null,
  Limite_Atraso_SA TimeStamp,
  BH_ENVIA_FALTA_FOLHA BOOLEAN,
  BH_ENVIA_FALTA_FOLHA_FRAC boolean,
  EVE_Interjornada    Varchar(3),
  EXP_Interjornada    Boolean default 0 not null,
  EVE_SobreAviso      VarChar(3),
  EXP_SobreAviso      Boolean default 0 not null,
  LancaBH_SobreAviso  Boolean default 0 not null,
  Tipo_Tolerancia_Apos_Flex Integer default -1 not null constraint CC_FRC_Tipo_Tol_Apos_Flex  check(Tipo_Tolerancia_Apos_Flex  in (-1, 0, 1)),
  EXP_Tempo_Espera    boolean default 0 not null,
  EVE_Tempo_Espera    varChar(3),
  Lancar_Tempo_Espera_BH boolean default 0 not null,
  Percentual_Tempo_Espera double precision default 0 not null,
  DESCONTAR_TEMPO_ESPERA_DA_HE boolean default 0 not null,
  Usar_Flexivel_Com_Exc_Faixa Boolean default 0 not null,
  Acumular_HE_Entre_EFX Boolean default 0 not null,
  EXPFERIADOHEDOCADASTRO Boolean default 0 not null,
  CONSTRAINT PK_FRC PRIMARY KEY (EMP_CODIGO, CODIGO),
  CONSTRAINT FK_FRC_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO),
  CONSTRAINT FK_FRC_HEA FOREIGN KEY (EMP_CODIGO, HEA_CODIGO) REFERENCES HEA (EMP_CODIGO, CODIGO)
);
.go

CREATE TABLE EFX (
	EMP_CODIGO   VARCHAR(4) NOT NULL,
	FRC_CODIGO   VARCHAR(3) NOT NULL,
	HORA_INICIAL TIMESTAMP NOT NULL,
	HORA_FINAL   TIMESTAMP NOT NULL,
	HEA_CODIGO   VARCHAR(2),
	DESTINO      INTEGER NOT NULL,
  // HASH_ID = FRC_CODIGO + HORA_INICIAL + HORA_FINAL (para ser usada como foreign da EQT)
  HASH_ID      VARCHAR(11) NOT NULL,
	CONSTRAINT PK_EFX PRIMARY KEY (EMP_CODIGO, FRC_CODIGO, HORA_INICIAL, HORA_FINAL),
	CONSTRAINT FK_EFX_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO),
	CONSTRAINT FK_EFX_FRC FOREIGN KEY (EMP_CODIGO, FRC_CODIGO) REFERENCES FRC (EMP_CODIGO, CODIGO),
	CONSTRAINT FK_EFX_HEA FOREIGN KEY (EMP_CODIGO, HEA_CODIGO) REFERENCES HEA (EMP_CODIGO, CODIGO),
  CONSTRAINT UK_EFX_HASH_ID UNIQUE (EMP_CODIGO, HASH_ID)
);
.go

CREATE TABLE EQT (
	EMP_CODIGO         VARCHAR(4) NOT NULL,
	FRC_CODIGO         VARCHAR(3) NOT NULL,
	QUANTIDADE_INICIAL TIMESTAMP NOT NULL,
	QUANTIDADE_FINAL   TIMESTAMP NOT NULL,
	HEA_CODIGO         VARCHAR(2),
	DESTINO            INTEGER NOT NULL,
  EFX_HASH_ID_PK     VARCHAR(11) DEFAULT '0' NOT NULL,
  EFX_HASH_ID_FK     VARCHAR(11),
	CONSTRAINT PK_EQT PRIMARY KEY (EMP_CODIGO, FRC_CODIGO, QUANTIDADE_INICIAL, QUANTIDADE_FINAL, EFX_HASH_ID_PK),
	CONSTRAINT FK_EQT_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO),
	CONSTRAINT FK_EQT_FRC FOREIGN KEY (EMP_CODIGO, FRC_CODIGO) REFERENCES FRC (EMP_CODIGO, CODIGO),
	CONSTRAINT FK_EQT_HEA FOREIGN KEY (EMP_CODIGO, HEA_CODIGO) REFERENCES HEA (EMP_CODIGO, CODIGO),
	CONSTRAINT FK_EQT_EFX FOREIGN KEY (EMP_CODIGO, EFX_HASH_ID_FK) REFERENCES EFX (EMP_CODIGO, HASH_ID) on delete cascade,
  CONSTRAINT UK_EQT UNIQUE (EMP_CODIGO, FRC_CODIGO, QUANTIDADE_INICIAL, QUANTIDADE_FINAL, EFX_HASH_ID_FK)
);
.go

CREATE TABLE HEP(
  EMP_CODIGO VARCHAR(4) NOT NULL,
  PEV_CODIGO VARCHAR(3) NOT NULL,
  ID INTEGER NOT NULL,
  // Estes campos depois devem ser excluídos
  DESCRICAO VARCHAR(10),
  EVE_HORAEXTRA VARCHAR(3),
  //----------------------------------------//
  SEGUNDA INTEGER default 0 not null,
  TERCA INTEGER default 0 not null,
  QUARTA INTEGER default 0 not null,
  QUINTA INTEGER default 0 not null,
  SEXTA INTEGER default 0 not null,
  SABADO INTEGER default 0 not null,
  DOMINGO INTEGER default 0 not null,
  FERIADO INTEGER default 0 not null,
  FOLGA INTEGER default 0 not null,
  FRC_CODIGO VARCHAR(3),
  CONSTRAINT PK_HEP PRIMARY KEY(EMP_CODIGO,PEV_CODIGO,ID),
  CONSTRAINT FK_HEP_PEV FOREIGN KEY(EMP_CODIGO, PEV_CODIGO) REFERENCES PEV(EMP_CODIGO, CODIGO),
  CONSTRAINT FK_HEP_FRC FOREIGN KEY (EMP_CODIGO, FRC_CODIGO) REFERENCES FRC (EMP_CODIGO, CODIGO)
);
.go

Create Table VBH(Emp_Codigo Varchar(4) not null,
                 Pev_Codigo Varchar(3) not null,
                 QtdPeriodo Integer not null,
                 TipoPeriodo Integer not null,
                 DtApartir timestamp not null,
                 DtVencimento timestamp,
                 constraint pk_VBH  primary key (EMP_Codigo, Pev_Codigo, DtApartir),
                 constraint fk_VBH_Pev foreign key (EMP_Codigo, Pev_Codigo) references PEV(EMP_Codigo, Codigo) )
.go

create table CBH (Emp_Codigo Varchar(4) not null,
                  Dias Integer not null,
                  Origem Integer default 1 not null,                  
                  constraint pk_CBH primary key(EMP_Codigo, Dias, Origem),
                  constraint fk_CBH_EMP foreign key (EMP_Codigo) references EMP(Codigo))
.go

create table FFP (
  EMP_Codigo varchar(4) not null,
  PEV_Codigo varchar(3) not null,
  FFX_ID     int not null,
  constraint pk_FFP primary key(EMP_Codigo, PEV_Codigo, FFX_ID),
  constraint fk_FFP_PEV foreign key (EMP_Codigo, PEV_Codigo) references PEV(EMP_Codigo, Codigo),
  constraint fk_FFP_FFX foreign key (FFX_ID) references FFX (ID)
);
.go

create table FMP (
  EMP_Codigo varchar(4) not null,
  PEV_Codigo varchar(3) not null,
  FMV_ID     int        not null,
  constraint pk_FMP primary key(EMP_Codigo, PEV_Codigo, FMV_ID),
  constraint fk_FMP_PEV foreign key (EMP_Codigo, PEV_Codigo) references PEV(EMP_Codigo, Codigo),
  constraint fk_FMP_FMV foreign key (FMV_ID) references FMV(ID)
);
.go

/* Feriados Fixos Especificos Desta Empresa */
CREATE TABLE FEF (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    PEV_CODIGO VARCHAR(3) NOT NULL,
    MESDIA VARCHAR(4) NOT NULL,
    CONSTRAINT PK_FEF PRIMARY KEY (EMP_CODIGO, PEV_CODIGO, MESDIA),
    CONSTRAINT FK_FEF_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO),
    CONSTRAINT FK_FEF_PEV FOREIGN KEY (EMP_CODIGO, PEV_CODIGO) REFERENCES PEV (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_FEF_FFE FOREIGN KEY (EMP_CODIGO, MESDIA) REFERENCES FFE (EMP_CODIGO, MESDIA)
);
.go

/* Feriados Móveis Especificos Desta Empresa */
CREATE TABLE FEM (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    PEV_CODIGO VARCHAR(3) NOT NULL,
    DATA TIMESTAMP NOT NULL,
    CONSTRAINT PK_FEM PRIMARY KEY (EMP_CODIGO, PEV_CODIGO, DATA),
    CONSTRAINT FK_FEM_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO),
    CONSTRAINT FK_FEM_PEV FOREIGN KEY (EMP_CODIGO, PEV_CODIGO) REFERENCES PEV (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_FEM_FME FOREIGN KEY (EMP_CODIGO, DATA) REFERENCES FME (EMP_CODIGO, DATA)
);
.go

/* Movimento de Situação em Lote */
CREATE TABLE MSL (
    EMP_CODIGO  varchar(4) not null,
    ID          integer not null,
    DATAHORA    timestamp not null,
    OBS         varchar(60),
    constraint PK_MSL primary key (EMP_CODIGO, ID),
    constraint FK_MSL_EMP foreign key(EMP_CODIGO) references EMP (CODIGO)
);
.go

/*situação do empregado*/
create table SEP (EMP_Codigo            varchar(4)       not null,
                  EPG_Codigo            varchar(6)       not null,
                  Data                  timestamp        not null,
                  CAR_codigo	          varchar(3)       not null,
                  HOR_Codigo            varchar(6),
                  LOT_Codigo            varchar(10)      not null,
                  EST_Codigo            varchar(4)       not null,
                  BatePonto             integer default 1 not null,
                  DHO_Ordem             integer default 0,
                  PEV_CODIGO            VARCHAR(3),
                  MSL_ID                integer,
                  DescontaVT            boolean default 0 not null,
                  Perc_Desc_VT          double precision default 0 not null,
                  Recebe_Gorjeta        boolean default 0 not null,
                  TOM_CODIGO            varchar(6),
                  In_Itinere            boolean default 0 not null,
                  TempoInItinereEntrada TimeStamp,
                  TempoInItinereSaida   TimeStamp,
                  Origem                Integer,
                  Reg_Classe_Car        Varchar(20),
				  tpRegJor              integer,
                  INTIMPBAT             boolean default 0 not null,
				  ENTINTIMPBAT          TimeStamp,
				  SAIINTIMPBAT          TimeStamp,
                  constraint pk_SEP primary key (EMP_Codigo,EPG_Codigo,Data),
                  constraint fk_SEP_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
                  constraint fk_SEP_CAR foreign key (EMP_Codigo,CAR_Codigo) references CAR(EMP_Codigo,Codigo),
                  constraint fk_SEP_EST foreign key (EMP_Codigo,EST_Codigo) references EST(EMP_Codigo,Codigo),
                  CONSTRAINT FK_SEP_LOT FOREIGN KEY (EMP_CODIGO,LOT_CODIGO) REFERENCES LOT (EMP_CODIGO, CODIGO) ON UPDATE CASCADE,
                  constraint fk_SEP_HOR foreign key (EMP_Codigo,HOR_Codigo) references HOR(EMP_Codigo,Codigo),
                  constraint fk_SEP_PEV foreign key (EMP_Codigo,PEV_CODIGO) references PEV(EMP_codigo,Codigo),
                  constraint FK_SEP_MSL foreign key (EMP_CODIGO, MSL_ID) references MSL (EMP_CODIGO, ID),
                  constraint FK_SEP_TOM foreign key (EMP_CODIGO, TOM_CODIGO) references TOM (EMP_Codigo, Codigo)
);
.go


/* Tabela de Fabricantes de Relogio */
create table fab(
  Codigo varchar(3) not null,
  Nome varchar(30),
  constraint PK_FAB primary key (Codigo));
.go

create table mdl(
  FAB_Codigo varchar(3) not null,
  Codigo varchar(3) not null,
  Nome varchar(30),
  constraint PK_MDL primary key (Fab_Codigo, Codigo),
  constraint FK_MDL_FAB foreign key (Fab_Codigo) references FAB(codigo));
.go

/* tabela de multiselect de relógios*/
create table MREL(
  id integer not null,
  REL_CODIGO varchar(3) not null,
  constraint PK_MREL primary key (id, rel_codigo));
.go

/* Tabela de Relogios */
CREATE TABLE REL (
    CODIGO          VARCHAR(3) NOT NULL,
    NOME            VARCHAR(30) NOT NULL,
    CONEXAO         INTEGER NOT NULL,
    IP              VARCHAR(15),
    PORTA           INTEGER,
    COM             INTEGER,
    MAQUINA         INTEGER,
    VELOCIDADE      INTEGER,
    AUTOMATICO      INTEGER,
    FAB_CODIGO      VARCHAR(3) not null,
    MDL_FAB_CODIGO  VARCHAR(3) not null,
    PADRAOAFD       INTEGER,
    ULTIMONSR       VARCHAR(9), /* Campo usado para armazenar a última batida coletada no relógio TRIX.*/
    TEMPOESPERA     INTEGER,    /* Campo usado para armazenar a última batida coletada no relógio TRIX.*/
    NrSerie         VARCHAR(17),
    Tipo_Agendamento        integer default 0 not null,
    Conf_Freq_Iniciar       timestamp,
    Conf_Freq_Finalizar     timestamp,
    Conf_Freq_Intervalo     timestamp,
    Conf_Freq_Ultima_Coleta timestamp,
    EMP_CODIGO          VARCHAR(4),
    CPF_RESPONSAVEL     Varchar(11),
    NOME_RESPONSAVEL    varchar(60),
    CHAVECOMUNICACAO    blob sub_type text,
    CHAVE_RSA           blob sub_type text,
    EXPOENTE_RSA        VARCHAR(256),
    LOGIN_USUARIO       VARCHAR(30),
    LOGIN_SENHA         blob sub_type text,
    DDNS                varchar(256),
    Tipo_Codigo_Envio   integer default 1 not null,
    SENHA_RELOGIO       varchar(256),
    DESATIVADO          integer default 0 not null,
    TODAS_EMP           Integer default 0 not null,	
    CONSTRAINT PK_REL PRIMARY KEY (CODIGO),
    CONSTRAINT UK_REL_NOME UNIQUE (NOME),
    CONSTRAINT FK_REL_FAB FOREIGN KEY (FAB_CODIGO) REFERENCES FAB (CODIGO),
    CONSTRAINT FK_REL_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP(CODIGO),
    CONSTRAINT FK_REL_MDL_FAB FOREIGN KEY (FAB_CODIGO, MDL_FAB_CODIGO) REFERENCES MDL (FAB_CODIGO, CODIGO)
);
.go

/* Agendamentos de Horários de Coletas dos Relógios */
create table ARE (
    REL_Codigo     varchar(3),
    Horario        timestamp,
    Ultima_Coleta  timestamp,
    constraint PK_ARE primary key (REL_Codigo, Horario),
    constraint FK_ARE_REL foreign key (REL_Codigo) references REL (Codigo)
);
.go

/* Controle de Importação */
create table IMP (
  EMP_Codigo        varchar(4) not null,
  ID                integer not null,
  Origem            varchar(62),
  Log               BLOB SUB_TYPE 1 SEGMENT SIZE 80,
  InicioImportacao  timestamp,
  FimImportacao     timestamp,
  Usu_Codigo        varchar(20),
  ULTIMONSR         varchar(9),/* Campo usado para armazenar a última batida da tabela REL antes da coleta no relógio TRIX.*/
  REL_CODIGO        VARCHAR(3),/* Campo usado para armazenar o Código do relógio da importação.*/
  TIPO_BATIDA       integer default 1 not null constraint CC_IMP_TIPO_BATIDA check(TIPO_BATIDA is null or TIPO_BATIDA in (1, 2, 3)),
  constraint pk_IMP primary key(EMP_Codigo, Id),
  constraint fk_IMP_EMP foreign key (EMP_Codigo) references EMP (Codigo),
  constraint fk_IMP_REL foreign key (REL_Codigo) references REL (Codigo),/* Campo usado para armazenar o Código do relógio da importação.*/
  constraint FK_USU_IMP foreign key (USU_CODIGO) references USU (CODIGO));
.go

CREATE DESCENDING INDEX IDX_INICIOIMPORTACAO ON IMP (INICIOIMPORTACAO);
.go

/* Controle de Importação do Gerenciador do Serviço */
create table IMP_GER (
  ID                integer not null,
  Origem            varchar(62),
  InicioImportacao  timestamp,
  FimImportacao     timestamp,
  USU_Codigo        varchar(20),
  UltimoNSR         varchar(9),
  REL_Codigo        VARCHAR(3),
  Tipo_Batida       integer default 1 not null constraint CC_IMP_GER_TIPO_BATIDA check(Tipo_Batida is null or Tipo_Batida in (1, 2)),
  TimestampApp      varchar(15),
  constraint PK_IMP_GER primary key(ID),
  constraint FK_IMP_GER_REL foreign key (REL_Codigo) references REL (Codigo),
  constraint FK_USU_IMP_GER foreign key (USU_CODIGO) references USU (CODIGO));
.go

/* Log de Importação de Batidas do Serviço */
create table LOG_IMP_BTP (
  ID          integer not null,
  Operacao    varchar(60),
  Status      varchar(10),
  DataHora    timestamp,
  Mensagem    varchar(255),
  Detalhe     varchar(255),
  IMP_GER_ID  integer,
  REL_Codigo  varchar(3),
  ORIGEM      integer,  
  EMP_CODIGO  varchar(4),
  constraint PK_LOG_IMP_BTP primary key(ID),
  constraint FK_LOG_IMP_BTP_EMP foreign key (EMP_CODIGO) references EMP(CODIGO) on delete cascade);
.go

/* Autoincremento da LOG_IMP_BTP */
create Sequence GEN_LOG_IMP_BTP_ID;
.go
alter Sequence GEN_LOG_IMP_BTP_ID Restart with 0;
.go
create trigger LOG_IMP_BTP_BI FOR LOG_IMP_BTP
active before insert position 0
as
begin
  if (NEW.ID is null) then
    NEW.ID = GEN_ID(GEN_LOG_IMP_BTP_ID,1);
end;
.go

/* Controle de envio de Restrições*/
create table XXX_ERR (
  EMP_Codigo        varchar(4) not null,
  ID                integer not null,
  Origem            varchar(62),
  Log               BLOB SUB_TYPE 1 SEGMENT SIZE 100,
  HoraEnvio         timestamp,
  Usu_Codigo        varchar(20),
  constraint pk_XXX_ERR primary key(EMP_Codigo, Id),
  CONSTRAINT FK_XXX_ERR_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP(CODIGO) on delete CASCADE);
.go

/* Tabela de Batidas em Lote */
create table BTL (
  Id_Ope        integer not null,
  DtHoraGeracao timestamp not null,
  DtInicial     timestamp not null,
  DtFinal       timestamp not null,
  Tipo          varchar(1),
  EMP_Codigo    varchar(4) not null,
  constraint pk_BTL primary key (Id_Ope, EMP_Codigo),
  constraint fk_BTL_EMP foreign key (EMP_Codigo) references EMP(Codigo) on delete Cascade
);
.go

CREATE TABLE MID (
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    CODIGO      VARCHAR(3) NOT NULL,
    NOME        VARCHAR(100) NOT NULL,
    NATUREZA    INTEGER default 0 NOT NULL,
    constraint UK_MID_NOME unique (EMP_CODIGO, NOME),
    constraint PK_MID primary key (EMP_CODIGO, CODIGO),
    constraint FK_MID_EMP foreign key (EMP_CODIGO) references EMP(CODIGO)
);
.go

/*batidas de ponto*/
create table BTP (
  EMP_Codigo     varchar(4) not null,
  EPG_Codigo     varchar(6) not null,
  DataHora       timestamp  not null,
  Justificativa  varchar(255),
  ID_OPE         integer,
  IMP_ID         integer,
  IMP_GER_ID     integer,
  SENTIDO        integer not null,
  BatidaTerminal boolean,
  Status         integer default 1 not null constraint CC_BTP_STATUS check(Status is null or Status in (1, 2, 3)),
  NumeroFabricacaoREP varchar(17),
  MID_Codigo     varchar(3),
  TURNO          integer default 0 not null constraint CC_BTP_TURNO check(TURNO is null or TURNO in (0, 1, 2, 3, 4)),
  Data_Referencia_Alterada timestamp,
  Origem_Referencia_Alterada Integer default 0 not null constraint CC_BTP_Origem_Ref_Alt check(Origem_Referencia_Alterada in(-1, 0, 1)),
  USU_CODIGO     varchar(20),
  ORIGEM         integer, 
  STATUSENVIOAPP integer default 0 not null,
  // -90.00000000000000000000 a 90.00000000000000000000
  Latitude       varchar(24),
  // -180.00000000000000000000 a 180.00000000000000000000
  Longitude      varchar(25),
  Endereco       varchar(255),
  CAMINHOFOTO    varchar(255),
  CAMINHOFOTOLOCAL varchar(255),
  FORADACERCA  boolean default 0 not null,  
  constraint pk_BTP primary key (EMP_Codigo,EPG_Codigo,DataHora),
  constraint fk_BTP_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo,Codigo),
  constraint fk_BTP_IMP foreign key (EMP_Codigo, IMP_Id) references IMP(EMP_Codigo, Id),
  constraint fk_BTP_BTL foreign key (Id_Ope, EMP_Codigo) references BTL(Id_Ope, EMP_Codigo),
  constraint fk_BTP_MID foreign key (EMP_Codigo, MID_Codigo) references MID (EMP_Codigo, Codigo),
  constraint FK_BTP_IMP_GER foreign key (IMP_GER_ID) references IMP_GER (ID)
);
.go

/*batidas de ponto Gerencial*/
create table BTG (
  EMP_Codigo     varchar(4) not null,
  EPG_Codigo     varchar(6) not null,
  DataHora       timestamp  not null,
  Lote Integer,
  Status         integer default 1 not null constraint CC_BTG_STATUS check(Status is null or Status in (1, 2, 3)),
  ORIGEM integer,
  constraint pk_BTG primary key (EMP_Codigo,EPG_Codigo,DataHora),
  constraint fk_BTG_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo,Codigo),
  constraint fk_BTG_Lote Foreign Key (EMP_Codigo, Lote) references LOTE (Emp_Codigo, Id)
);
.go

create table XXX_BTP (
  EMP_Codigo     varchar(4) not null,
  EPG_Codigo     varchar(6) not null,
  DataHora       timestamp  not null,
  Justificativa  varchar(255),
  ID_OPE         integer,
  IMP_ID         integer,
  SENTIDO        integer not null,
  BatidaTerminal boolean,
  Status         integer default 1 not null constraint CC_XXX_BTP_STATUS check(Status is null or Status in (1, 2, 3)),
  NumeroFabricacaoREP varchar(17),
  MID_Codigo     varchar(3),
  constraint pk_XXX_BTP primary key (EMP_Codigo,EPG_Codigo,DataHora),
  CONSTRAINT FK_XXX_BTP_EPG FOREIGN KEY (EMP_CODIGO,EPG_CODIGO) REFERENCES EPG(EMP_CODIGO,CODIGO) on delete CASCADE);
.go

/*batidas de ponto geradas*/
create table XXX_BTG (
  EMP_Codigo    varchar(4) not null,
  EPG_Codigo    varchar(6) not null,
  DataHora      timestamp  not null,
  DiaRef        timestamp  not null,
  SENTIDO       integer not null,
  BTPCORRESPONDENTE timestamp,
  TURNO         integer,
  constraint pk_XXX_BTG primary key (EMP_Codigo,EPG_Codigo,DataHora),
  CONSTRAINT FK_XXX_BTG_EPG FOREIGN KEY (EMP_CODIGO,EPG_CODIGO) REFERENCES EPG(EMP_CODIGO,CODIGO) on delete CASCADE);
.go

create table KBTL_ID (ID_OPE integer not null,
                      constraint pk_KBTL_Codigo primary key (ID_OPE));
.go

/* Código Internacional de Doenças */
create table CID (
    Codigo    varchar(4)  not null,
    Opc       varchar(1),
    Nome      varchar(50) not null,
    RestrSexo integer,
    Versao    integer,
    constraint PK_CID primary key (Codigo)
)
.go

/* Motivos de Atraso e Falta */
CREATE TABLE MAT (
    CODIGO         VARCHAR(10) NOT NULL,
    NOME           VARCHAR(60),
    MAT_CODIGO_MAE VARCHAR(10),
    CID_CODIGO     VARCHAR(4),
    MAT_TIPO       INTEGER DEFAULT 0 NOT NULL,
    constraint PK_MAT primary key (codigo),
    constraint FK_MAT_CID foreign key (CID_CODIGO) references CID(CODIGO)
);
.go

create table JFL(
  EMP_CODIGO    varchar(4) not null,
  LOTE          integer not null,
  DTHORAGERACAO timestamp not null,
  DTINICIAL     timestamp not null,
  DTFINAL       timestamp not null,
  OBS           varchar(255) not null,
  constraint pk_JFL primary key (EMP_CODIGO, LOTE),
  constraint fk_JFL_EMP foreign key (EMP_CODIGO) references EMP(CODIGO)
);
.go

create table JAL(
  EMP_CODIGO        varchar(4) not null,
  LOTE              integer not null,
  DTHORAGERACAO     timestamp not null,
  DTHORAINICIAL     timestamp not null,
  DTHORAFINAL       timestamp not null,
  OBS               varchar(255) not null,
  constraint pk_JAL primary key (EMP_CODIGO, LOTE),
  constraint fk_JAL_EMP foreign key (EMP_CODIGO) references EMP(CODIGO)
);
.go

create table JHL(
  EMP_Codigo    varchar(4) not null,
  Lote          integer not null,
  DtHoraGeracao timestamp not null,
  DtInicial     timestamp not null,
  DtFinal       timestamp not null,
  Obs           varchar(255) not null,
  constraint PK_JHL primary key (EMP_Codigo, Lote),
  constraint FK_JHL_EMP foreign key (EMP_Codigo) references EMP(Codigo)
);
.go

/* Caixa de Mensagem */
create table CXM (
    SEQ         integer not null,
    USU_CODIGO  varchar(20) not null,
    REMETENTE   varchar(10),
    DATAHORA    timestamp,
    MENSAGEM    blob sub_type 1 segment size 80,
    LIDA        BOOLEAN,
    constraint PK_CXM primary key (SEQ),
    constraint FK_CXM_USU foreign key (USU_CODIGO) references USU(CODIGO) ON DELETE CASCADE
);
.go

create table KCXM_SEQ (
    SEQ  integer not null,
    constraint PK_KCXM_SEQ primary key (SEQ)
);
.go

/* Conselho Regional de Medicina */
CREATE TABLE CRM (
    CODIGO VARCHAR(6) NOT NULL,
    CRM_NOME_MEDICO VARCHAR(70) default '' NOT NULL,
    UFD_SIGLA VARCHAR(2),
    SIGLA_CLASSE VARCHAR(30),
    REG_CODIGO VARCHAR(20),
    ORIGEM INTEGER,
    CONSTRAINT PK_CRM PRIMARY KEY (CODIGO),
    constraint FK_CRM_CLASSE FOREIGN KEY (SIGLA_CLASSE) REFERENCES CLASSE(SIGLA),
    constraint UK_CRM_UFD_CLA_REGCOD unique (UFD_SIGLA, SIGLA_CLASSE, REG_CODIGO)
);
.go

/*Cria backup da tabela CRM - Ana Paula - Foi criada aqui para o compara banco rodar - apagar depois da atualização da versão que altera a PK_CRM - 23/11*/
CREATE TABLE CRM_BACKUP (
    UFD_SIGLA VARCHAR(2) NOT NULL,
    SIGLA_CLASSE VARCHAR(30) NOT NULL,    
    CRM_CODIGO VARCHAR(20) NOT NULL,
    CRM_NOME_MEDICO VARCHAR(70) default '' NOT NULL,
    ORIGEM INTEGER
);
.go

create table MCRM (
	Id         integer not null,
	CRM_Codigo VARCHAR(6) not null,
	constraint PK_MCRM primary key (Id, CRM_Codigo)
);
.go


/*lançamento de justificativa de falta*/
create table LJF (EMP_Codigo varchar(4) not null,
                  EPG_Codigo varchar(6) not null,
                  Data       timestamp  not null,
                  MAT_Codigo varchar(10) not null,
                  Abono      boolean,
                  Obs        varchar(255),
                  Lote       integer,
                  CRM_Codigo VARCHAR(6),
                  CID_Codigo varchar(4),
                  DiaJustificado boolean,
                  AbonaOsTurnos boolean,
                  Turnos integer default 0 not null,
                  Seq Integer default 1 not null,
                  AbonaFrac timestamp,
                  STATUSENVIOAPP INTEGER DEFAULT 0 NOT NULL,
                  enviarfaltafolha boolean Default 0 not null,
                  constraint pk_LJF  primary key (EMP_Codigo,EPG_Codigo,Data,Seq),
                  constraint fk_LJF_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
                  constraint fk_LJF_MAT foreign key (MAT_Codigo) references MAT (Codigo),
                  constraint FK_LJF_JFL foreign key(EMP_CODIGO, LOTE) references JFL(EMP_CODIGO, LOTE),
                  constraint fk_LJF_CID foreign key (CID_Codigo) references CID(Codigo),
                  constraint FK_LJF_CRM foreign key(CRM_Codigo) references CRM(Codigo));
.go

/*lançamento de justificativa de Atraso ou Saida Antecipada*/
create table LJA (EMP_Codigo       varchar(4) not null,
                  EPG_Codigo       varchar(6) not null,
                  Datahora         timestamp  not null,
                  MAT_Codigo       varchar(10) not null,
                  Abono            boolean,
                  Obs              varchar(255),
                  CRM_Codigo       VARCHAR(6),
                  Lote             integer,
                  AbonoDia         boolean,
                  JustificativaDia boolean,
                  LancaDebitoLBH Boolean,
                  AbonaFrac        timestamp,
                  CID_Codigo       varchar(4),
                  PrimeiraBtFlex timestamp,
				  MaioresAtrasos boolean default 0 not null,
                  constraint pk_LJA  primary key (EMP_Codigo,EPG_Codigo,DataHora),
                  constraint fk_LJA_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
                  constraint fk_LJA_MAT foreign key (MAT_Codigo) references MAT (Codigo),
                  constraint FK_LJA_CRM foreign key(CRM_Codigo) references CRM(Codigo),
                  constraint fk_LJA_CID foreign key (CID_Codigo) references CID(Codigo),
                  constraint FK_LJA_JAL foreign key(EMP_CODIGO, LOTE) references JAL(EMP_CODIGO, LOTE));
.go

/*compensação de falta*/
create table CPF (EMP_Codigo varchar(4) not null,
                  EPG_Codigo varchar(6) not null,
                  Seq        integer    not null,
                  Justificativa varchar(255),
                  Lote Integer,
                  DataHora   timestamp, /* data/hora do lançamento */
                  LimiteNoDia boolean default 0 not null,
                  constraint pk_CPF  primary key (EMP_Codigo,EPG_Codigo,Seq),
                  constraint fk_CPF_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
                  constraint fk_CPF_Lote Foreign Key (EMP_Codigo, Lote) references LOTE (Emp_Codigo, Id));
.go

/*origem  da compensação de falta*/
create table OCP ( EMP_Codigo varchar(4) not null,
                   EPG_Codigo varchar(6) not null,
                   CPF_Seq    integer    not null,
                   Data       timestamp  not null,
                   constraint pk_OCP  primary key (EMP_Codigo,EPG_Codigo,CPF_Seq,Data),
                   constraint fk_OCP_CPF foreign key (EMP_Codigo,EPG_Codigo,CPF_Seq) references CPF(EMP_Codigo,EPG_Codigo,Seq));
.go

/*turno da origem da compensação de falta*/
create table TOC ( EMP_Codigo     varchar(4) not null,
                   OCP_EPG_Codigo varchar(6) not null,
                   OCP_CPF_Seq    integer    not null,
                   OCP_Data       timestamp  not null,
                   HoraIni        timestamp not null,
                   HoraFim        timestamp,
                   Turno          integer not null,
                   constraint pk_TOC  primary key (EMP_Codigo, OCP_EPG_Codigo, OCP_CPF_Seq, OCP_Data, HoraIni),
                   constraint fk_TOC_OCP foreign key (EMP_Codigo, OCP_EPG_Codigo, OCP_CPF_Seq, OCP_Data) references OCP (EMP_Codigo,EPG_Codigo,CPF_Seq,Data));
.go

/*destino  da compensação de falta*/
create table DCP ( EMP_Codigo varchar(4) not null,
                   EPG_Codigo varchar(6) not null,
                   CPF_Seq    integer    not null,
                   Data       timestamp  not null,
                   BTPLIMITE timestamp,
                   constraint pk_DCP  primary key (EMP_Codigo,EPG_Codigo,CPF_Seq,Data),
                   constraint fk_DCP_CPF foreign key (EMP_Codigo,EPG_Codigo,CPF_Seq) references CPF(EMP_Codigo,EPG_Codigo,Seq));
.go

/*turno do destino da compensação de falta*/
create table TDC ( EMP_Codigo     varchar(4) not null,
                   DCP_EPG_Codigo varchar(6) not null,
                   DCP_CPF_Seq    integer    not null,
                   DCP_Data       timestamp  not null,
                   HoraIni        timestamp not null,
                   HoraFim        timestamp,
                   Turno          integer not null,
                   TOLENTRADA timestamp,
                   TOLSAIDA timestamp,
                   FLEXIVELBTPINI  INTEGER DEFAULT 0 NOT NULL,
                   FLEXIVELBTPFIM  INTEGER DEFAULT 0 NOT NULL,
                   constraint pk_TDC  primary key (EMP_Codigo, DCP_EPG_Codigo, DCP_CPF_Seq, DCP_Data, HoraIni),
                   constraint fk_TDC_DCP foreign key (EMP_Codigo, DCP_EPG_Codigo, DCP_CPF_Seq, DCP_Data) references DCP (EMP_Codigo,EPG_Codigo,CPF_Seq,Data));
.go

/* cadastro de refeição */
create table RFC
(
  CODIGO varchar(3) not null,
  NOME varchar(60) not null,
  constraint PK_RFC primary key (CODIGO),
  constraint UK_RFC_NOME unique (NOME)
);
.go

/* cadastro de ocorrencias*/
create table OCR (EMP_Codigo   varchar(4)  not null,
                  Codigo       varchar(3)  not null,
                  Descricao    varchar(40) not null,
                  constraint pk_OCR primary key (EMP_Codigo,Codigo),
                  constraint fk_OCR_EMP foreign key (EMP_Codigo) references EMP(Codigo))
.go

create table MOCR (
    id integer not null,
    OCR_CODIGO varchar(3) not null,
    constraint PK_MOCR primary key (id, OCR_CODIGO))
.go

create table OCE (EMP_Codigo   varchar(4)  not null,
                  EPG_Codigo   varchar(6)  not null,
                  OCR_Codigo   varchar(3)  not null,
                  Data         timestamp   not null,
                  Obs          blob sub_type text,
                  IPS_ID       INTEGER,
                  LOTE         INTEGER,
                  constraint pk_OCE primary key (EMP_Codigo,EPG_Codigo,OCR_Codigo,Data),
                  constraint fk_OCE_EMP foreign key (EMP_Codigo) references EMP(Codigo),
                  constraint fk_OCE_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
                  constraint fk_OCE_OCR foreign key (EMP_Codigo,OCR_Codigo) references OCR(EMP_Codigo,Codigo),
                  constraint fk_OCE_LOTE foreign key (EMP_Codigo,LOTE) references LOTE(EMP_Codigo,ID))
.go

create table MRFC
(
    ID integer not null,
    RFC_CODIGO varchar(3) not null,
    constraint PK_MRFC primary key (ID, RFC_CODIGO)
)
.go

/* cadastro de refeitorios */
create table RFT
(
  CODIGO varchar(3) not null,
  NOME varchar(60) not null,
  constraint PK_RFT primary key (CODIGO),
  constraint UK_RFT_NOME unique (NOME)
);
.go

create table MRFT
(
    ID integer not null,
    RFT_CODIGO varchar(3) not null,
    constraint PK_MRFT primary key (ID, RFT_CODIGO)
)
.go

/* Cadastro de Refeições do Refeitório */
create table RRF
(
  RFT_CODIGO varchar(3) not null,
  RFC_CODIGO varchar(3) not null,
  BTPINICIAL timestamp not null,
  BTPFINAL timestamp not null,
  EPG_CUSTO DOUBLE PRECISION not null,
  EMP_CUSTO DOUBLE PRECISION not null,
  constraint PK_RRF primary key (RFT_CODIGO, RFC_CODIGO),
  constraint FK_RRF_RFT foreign key (RFT_CODIGO) references RFT (CODIGO) on delete cascade,
  constraint FK_RRF_RFC foreign key (RFC_CODIGO) references RFC (CODIGO) on delete cascade
);
.go

/* Cadastro de Refeições Realizadas pelos Empregados */
create table RRE
(
  EMP_CODIGO  VARCHAR(4) NOT NULL,
  EPG_CODIGO  VARCHAR(6) NOT NULL,
  DATA        timestamp not null,
  HORA        timestamp not null,
  RFT_CODIGO  VARCHAR(3) NOT NULL,
  RFC_CODIGO VARCHAR(3) not null,
  ORIGEM INTEGER not null,
  IMP_ID  INTEGER,
  constraint PK_RRE primary key (EMP_CODIGO, EPG_CODIGO, DATA, HORA),
  constraint FK_RRE_EPG foreign key (EMP_CODIGO, EPG_CODIGO) references EPG (EMP_CODIGO, CODIGO),
  constraint FK_RRE_RFT foreign key (RFT_CODIGO) references RFT (CODIGO),
  constraint FK_RRE_RFC foreign key (RFC_CODIGO) references RFC (CODIGO),
  CONSTRAINT FK_RRE_IMP FOREIGN KEY (EMP_CODIGO, IMP_ID) REFERENCES IMP (EMP_CODIGO, ID)
);
.go

create table RER(
  RFT_CODIGO varchar(3) not null,
  REL_CODIGO varchar(3) not null,
  constraint PK_RER primary key (RFT_CODIGO, REL_CODIGO),
  constraint FK_RER_RFT foreign key (RFT_CODIGO, REL_CODIGO) references RER (RFT_CODIGO, REL_CODIGO)
);
.go


/*complementar destino  da compensação de falta*/

/* Configuração Geral */
create table CFG (CODIGO  VARCHAR(50) NOT NULL,
                  VALOR   VARCHAR(255) NOT NULL,
                  constraint PK_CFG primary key (Codigo));
.go


/* Perfil */
create table PER(
    Codigo varchar(2) not null,
    Nome   varchar(20) not null,
    Padrao integer default 0 not null constraint CC_PER_PADRAO check(Padrao in (0,1)),
    constraint UK_PER_SIS_NM unique (NOME),
    constraint PK_PER primary key (CODIGO));
.go


/* Módulo de Acesso*/
create table MOD (
    Codigo      varchar(20) not null,
    Nome        varchar(60) not null,
    Operacao    integer default 0 not null,
    constraint CC_MOD_Operacao check(Operacao in (0,1)),
    constraint PK_MOD primary key (Codigo));
.go

/* Módulos do Perfil */
CREATE TABLE MPF (
    PER_CODIGO  VARCHAR(2) NOT NULL,
    MOD_CODIGO  VARCHAR(20) NOT NULL,
    CONSTRAINT PK_MPF PRIMARY KEY (PER_CODIGO, MOD_CODIGO),
    CONSTRAINT FK_MPF_MOD FOREIGN KEY (MOD_CODIGO) REFERENCES MOD (CODIGO),
    CONSTRAINT FK_MPF_PER FOREIGN KEY (PER_CODIGO) REFERENCES PER (CODIGO));
.go

/* Perfis dos Usuários nas Empresas */
create table PUE (USU_Codigo varchar(20) not null,
                  EMP_Codigo varchar(4) not null,
                  PER_Codigo varchar(2) not null,
                  constraint pk_PUE primary key (USU_Codigo, EMP_Codigo),
                  constraint fk_PUE_USU foreign key (USU_Codigo) references USU(Codigo) on delete cascade,
                  constraint fk_PUE_EMP foreign key (EMP_Codigo) references EMP(Codigo) on delete cascade,
                  constraint fk_PUE_PER foreign key (PER_Codigo) references PER(Codigo) on delete cascade);
.go


/* Auditoria */
CREATE TABLE AUD (
    DATAHORA           timestamp NOT NULL,
    EMP_CODIGO         VARCHAR(4),
    USU_CODIGO         VARCHAR(20) NOT NULL,
    MAQUINA            VARCHAR(15),
    MODULO             VARCHAR(10) NOT NULL,
    OPERACAO           VARCHAR(1),
    DADOS              BLOB SUB_TYPE 1 SEGMENT SIZE 80);
.go


/* Motivos de Afastamento */
create table MAF (
  Codigo     varchar(3) not null,
  Descricao  varchar(60),
  GeraFalta  boolean,
  GeraAbsent boolean default 1 not null,
  AC_Codigo  varchar(2),
  CID_Codigo varchar(4),
  Emergencial_Eletivo integer default 0 not null,
  TipoAcident Integer default 0 not null,
  GeraAtraso boolean default 0 not null,
  constraint pk_MAF primary key (Codigo),
  constraint fk_MAF_CID foreign key (CID_Codigo) references CID(Codigo)
);
.go


/* Afastamentos em lote*/
create table AFAL(
  EMP_CODIGO    varchar(4) not null,
  LOTE          integer not null,
  DTHORAGERACAO timestamp not null,
  DTINICIAL     timestamp not null,
  DTFINAL       timestamp not null,
  OBS           varchar(255) not null,
  constraint pk_AFAL primary key (LOTE, EMP_CODIGO),
  constraint fk_AFAL_EMP foreign key (EMP_CODIGO) references EMP(CODIGO)
);
.go

/* Afastamentos*/
create table AFA (
  EMP_Codigo varchar(4) not null,
  EPG_Codigo varchar(6) not null,
  DtInicial timestamp not null,
  DtFinal timestamp,
  MAF_Codigo varchar(3),
  OBS varchar(255),
  LOTE integer,
  AFA_Inicial_Pai timestamp,
  DtProcessamentoVT timestamp,
  GerarHEDiasTrabalhados boolean,
  NaoGerarHEPrimeiroDia boolean,
  GeraFalta  boolean default 0 not null,
  GeraAtraso boolean default 0 not null,
  DataEnvioApp timestamp,
  constraint pk_AFA primary key (EMP_Codigo, EPG_Codigo, DtInicial),
  constraint fk_AFA_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo, Codigo),
  constraint fk_AFA_MAF foreign key (MAF_Codigo) references MAF(Codigo)
);
.go

/* Afastamentos excluídos e estão no app*/
create table AFA_EXC (
  EMP_Codigo varchar(4) not null,
  EPG_Codigo varchar(6) not null,
  DtInicial timestamp not null,
  DtFinal timestamp,
  MAF_Codigo varchar(3),
  DataEnvioApp timestamp,
  constraint fk_AFA_EXC_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo, Codigo),
  constraint fk_AFA_EXC_MAF foreign key (MAF_Codigo) references MAF(Codigo)
);
.go

create trigger T_AFA_BeforeDelete for AFA active before delete as
  declare variable valor_data timestamp;
begin
  if ((OLD.DataEnvioApp is not null) and ((select CFE.Valor from CFE where CFE.EMP_Codigo = OLD.EMP_Codigo and CFE.Codigo = 'APPCOLABORADOR') = 1)) then
  begin
    if (not exists (select 1 from AFA_EXC where EMP_Codigo = OLD.EMP_Codigo and EPG_Codigo = OLD.EPG_Codigo and DtInicial = OLD.DtInicial)) then
    begin
      valor_data = OLD.DtInicial;
      while (valor_data <= OLD.DataEnvioApp) do
      begin
        insert into AFA_EXC (EMP_Codigo, EPG_Codigo, DtInicial, DtFinal, MAF_Codigo, DataEnvioApp)
                     values (OLD.EMP_Codigo, OLD.EPG_Codigo, OLD.DtInicial, OLD.DtFinal, OLD.MAF_Codigo, :valor_data);
        valor_data = valor_data + 1;
       end
    end
  end
end;
.go

create trigger T_AFA_BeforeUpdate for AFA active before update as
  declare variable valor_data timestamp;
begin
  if ((OLD.DataEnvioApp is not null) and (OLD.DataEnvioApp > NEW.DtFinal) and
      ((select CFE.Valor from CFE where CFE.EMP_Codigo = OLD.EMP_Codigo and CFE.Codigo = 'APPCOLABORADOR') = 1)) then
  begin
    valor_data = NEW.DtFinal + 1;
    while (valor_data <= OLD.DataEnvioApp) do
    begin
      insert into AFA_EXC (EMP_Codigo, EPG_Codigo, DtInicial, DtFinal, MAF_Codigo, DataEnvioApp)
                   values (OLD.EMP_Codigo, OLD.EPG_Codigo, OLD.DtInicial, OLD.DtFinal, OLD.MAF_Codigo, :valor_data);
      valor_data = valor_data + 1;
     end
  end
end
.go

/* Atestado Médico */
create table ATM (
  EMP_CODIGO varchar(4) not null,
  EPG_CODIGO varchar(6) not null,
  DTINICIAL timestamp not null,
  SEQUENCIAL integer not null,
  QTDDIASAFASTAMENTO integer,
  CRM_CODIGO  VARCHAR(6),
  CID_CODIGO varchar(4),
  CONSTRAINT PK_ATM PRIMARY KEY (EMP_CODIGO, EPG_CODIGO, DTINICIAL, SEQUENCIAL),
  CONSTRAINT FK_ATM_AFA FOREIGN KEY (EMP_CODIGO, EPG_CODIGO, DTINICIAL) REFERENCES AFA(EMP_CODIGO, EPG_CODIGO, DTINICIAL) on delete cascade,
  CONSTRAINT FK_ATM_CRM FOREIGN KEY (CRM_CODIGO) REFERENCES CRM (CODIGO)
);
.go

create table kIMP_Id (
  EMP_Codigo varchar(4) not null,
  ID         int not null,
  constraint pk_KIMP_ID primary key (EMP_Codigo, ID));
.go

create table kERR_Id (
  EMP_Codigo varchar(4) not null,
  ID         int not null,
  constraint pk_KERR_ID primary key (EMP_Codigo, ID));
.go

create table kCAR_Codigo (
  Codigo varchar(3) not null,
  constraint pk_KCAR_Codigo primary key (Codigo));
.go

create table kHOR_Codigo (
  EMP_Codigo varchar(4) not null,
  Codigo         int not null,
  constraint pk_KHOR_Codigo primary key (EMP_Codigo, Codigo));
.go

/*Escalas*/

create table ESCL (
  EMP_CODIGO    varchar(4) not null,
  LOTE          integer not null,
  DTHORAGERACAO timestamp not null,
  DTINICIAL     timestamp not null,
  DTFINAL       timestamp not null,
  OBS           varchar(255) not null,
  constraint PK_ESCL primary key (LOTE, EMP_CODIGO),
  constraint FK_ESCL_EMP foreign key (EMP_CODIGO) references EMP(CODIGO)
);
.go

CREATE TABLE MESC (
    ID          INTEGER NOT NULL,
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    ESC_CODIGO  INTEGER NOT NULL,
    CONSTRAINT PK_MESC PRIMARY KEY (ID, EMP_CODIGO, ESC_CODIGO)
);
.go

CREATE TABLE ESC (
    EMP_CODIGO   VARCHAR(4) NOT NULL,
    CODIGO       INTEGER NOT NULL,
    OBS          VARCHAR(60),
    DATAINICIAL  DATE NOT NULL,
    DATAFINAL    DATE NOT NULL,
    EST_CODIGO   VARCHAR(4),
    LOT_CODIGO   VARCHAR(10),
    HOR_CODIGO   VARCHAR(6),
    CAR_CODIGO   VARCHAR(3),
    EPG_CODIGO   VARCHAR(6),
    LOTE integer,
    USU_CODIGO_DONO VARCHAR(20),
    BLOQUEADA boolean default 0 not null,
    constraint pk_ESC  PRIMARY KEY (EMP_Codigo, Codigo),
    CONSTRAINT FK_ESC_CAR FOREIGN KEY (EMP_CODIGO, CAR_CODIGO) REFERENCES CAR (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_ESC_EST FOREIGN KEY (EMP_CODIGO, EST_CODIGO) REFERENCES EST (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_ESC_HOR FOREIGN KEY (EMP_CODIGO, HOR_CODIGO) REFERENCES HOR (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_ESC_LOT FOREIGN KEY (EMP_CODIGO, LOT_CODIGO) REFERENCES LOT (EMP_CODIGO, CODIGO) ON UPDATE CASCADE,
    CONSTRAINT FK_ESC_ESCL FOREIGN KEY (LOTE, EMP_CODIGO) REFERENCES ESCL (LOTE, EMP_CODIGO),
    CONSTRAINT FK_ESC_USU FOREIGN KEY (USU_CODIGO_DONO) REFERENCES USU (CODIGO));
.go

/*Empregados da Escala*/
CREATE TABLE EES (
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    ESC_CODIGO  INTEGER NOT NULL,
    EPG_CODIGO  VARCHAR(6) NOT NULL,
    constraint pk_EES  primary key (EMP_Codigo, ESC_Codigo, EPG_CODIGO),
    constraint fk_EES_ESC foreign key (EMP_Codigo, ESC_Codigo) references ESC(EMP_Codigo, Codigo),
    constraint fk_EES_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo, Codigo));
.go

/*Dias dos Empregados da Escala*/
CREATE TABLE DES (
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    ESC_CODIGO  INTEGER NOT NULL,
    EPG_CODIGO  VARCHAR(6) NOT NULL,
    DATA        DATE NOT NULL,
    BTPLIMITE   DATE NOT NULL,
    VT BOOLEAN,
    VT_SOBREAVISO boolean default 0 not null,
    VR BOOLEAN,
    Dia_Util BOOLEAN,
    TIPOHORARIO  VARCHAR(1) DEFAULT 'X' NOT NULL CONSTRAINT CC_DES_TIPOHORARIO CHECK(TIPOHORARIO in ('X','F')),
    DESC_PREV_FERIADO boolean default 0 not null,
    LimiteNoDia BOOLEAN DEFAULT 0 NOT NULL,
    Intervalo_Minimo Timestamp default '1900-1-1 00:00:00' not null,
    Lancar_No_BH boolean default 0 not null,
    CONSTRAINT PK_DES PRIMARY KEY (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO, DATA),
    constraint UK_DES unique (EMP_CODIGO,EPG_CODIGO,DATA),
    CONSTRAINT FK_DES_EES FOREIGN KEY (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO) REFERENCES EES (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO));
.go

/*Turnos dos Dias dos Empregados da Escala*/
CREATE TABLE TDE (
    EMP_CODIGO     VARCHAR(4) NOT NULL,
    ESC_CODIGO     INTEGER NOT NULL,
    EPG_CODIGO     VARCHAR(6) NOT NULL,
    BTPINICIAL     DATE NOT NULL,
    BTPFINAL       DATE NOT NULL,
    INTBTPINICIAL  integer default 0 NOT NULL,
    INTBTPFINAL    integer default 0 NOT NULL,
    TURNO          INTEGER NOT NULL,
    TOLENTRADA     DATE,
    TOLSAIDA       DATE,
    DATA           DATE NOT NULL,
    FLEXIVELBTPINI BOOLEAN DEFAULT 0 NOT NULL,
    FLEXIVELBTPFIM BOOLEAN DEFAULT 0 NOT NULL,
    SOBREAVISO Integer default 0 not null,
    SOBREAVISOANTESDOPREVISTO Integer default 0 not null,
    CursoAprendizBtpIni BOOLEAN DEFAULT 0 NOT NULL,
    CursoAprendizBtpFim BOOLEAN DEFAULT 0 NOT NULL,
    CONSTRAINT PK_TDE PRIMARY KEY (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO, DATA, BTPINICIAL),
    CONSTRAINT FK_TDE_DES FOREIGN KEY (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO, DATA) REFERENCES DES (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO, DATA));
.go


/* Tabela de Leitores Biometricos USB */
Create table LBU (Codigo     VARCHAR(3) NOT NULL,
                  Nome       VARCHAR(30),
                  NumSerie   VARCHAR(255),
                  CHKPOINT   timestamp,
                  Constraint pk_LBU PRIMARY KEY (CODIGO));
.go

/* tabela de multiselect de Leitores Biometricos USB*/
Create table MLBU (Id integer not null,
                   LBU_Codigo varchar(3) not null,
                   constraint pk_MLBU primary key (Id, LBU_Codigo));
.go

/* Associação Empregados - Leitores Biométricos*/
create table ELB(EMP_Codigo varchar(4) not null,
                 EPG_Codigo varchar(6) not null,
                 LBU_Codigo varchar(3) not null,
                 Lote integer,
                 Data  timestamp,
                 Constraint PK_ELB primary key(EMP_Codigo, EPG_Codigo, LBU_Codigo),
                 Constraint FK_ELB_EMP foreign key (EMP_Codigo) references EMP (Codigo),
                 Constraint FK_ELB_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
                 Constraint FK_ELB_LBU foreign key (LBU_Codigo) references LBU(Codigo));
.go


/* Tabela de Leiautes */
create table LEI (
  Codigo          varchar(3) not null,
  Descricao       varchar(50),
  MatriculaPosIni int,
  MatriculaTam    int,
  DiaPosIni       int,
  DiaTam          int,
  MesPosIni       int,
  MesTam          int,
  AnoPosIni       int,
  AnoTam          int,
  HoraPosIni      int,
  HoraTam         int,
  MinutoPosIni    int,
  MinutoTam       int,
  Exemplo         blob sub_type text,
  constraint pk_LEI primary key (Codigo)
);
.go

CREATE TABLE TPF (
  EMP_CODIGO VARCHAR(4) NOT NULL,
  SIGLA VARCHAR(2) NOT NULL,
  DESCRICAO VARCHAR(60) NOT NULL,
  DEDUZBANCOHORAS INT,
  DNR Boolean default 0 not null,
  Deduz_BH_Padrao_HE boolean default 0 not null,
  CONSTRAINT PK_TPF PRIMARY KEY (EMP_CODIGO, SIGLA),
  CONSTRAINT FK_TPF_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO)
);
.go

create table ESFL (
  EMP_CODIGO    varchar(4) not null,
  LOTE          integer not null,
  DTHORAGERACAO timestamp not null,
  DTINICIAL     timestamp not null,
  DTFINAL       timestamp not null,
  OBS           varchar(255) not null,
  constraint PK_ESFL primary key (LOTE, EMP_CODIGO),
  constraint FK_ESFL_EMP foreign key (EMP_CODIGO) references EMP(CODIGO)
);
.go

 /*escalas de folga*/
CREATE TABLE ESF (
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    CODIGO      VARCHAR(6) NOT NULL,
    OBS         VARCHAR(255),
    EST_Codigo  Varchar(4),
    LOT_Codigo  Varchar(10),
    CAR_Codigo  Varchar(3),
    HOR_Codigo  Varchar(6),
    EPG_Codigo  Varchar(6),
    DATAINICIAL TIMESTAMP NOT NULL,
    DATAFINAL   TIMESTAMP NOT NULL,
    LOTE Integer,
    USU_CODIGO_DONO VARCHAR(20),
    BLOQUEADA boolean default 0 not null,
    CONSTRAINT PK_ESF PRIMARY KEY (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_ESF_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO),
    constraint Fk_ESF_CAR foreign key (EMP_Codigo,CAR_Codigo) references CAR(EMP_Codigo,Codigo) on delete Cascade,
    constraint Fk_ESF_EST foreign key (EMP_Codigo,EST_Codigo) references EST(EMP_Codigo,Codigo) on delete Cascade,
    CONSTRAINT FK_ESF_LOT FOREIGN KEY (EMP_CODIGO,LOT_CODIGO) REFERENCES LOT (EMP_CODIGO, CODIGO) ON UPDATE CASCADE,
    constraint Fk_ESF_HOR foreign key (EMP_Codigo,HOR_Codigo) references HOR(EMP_Codigo,Codigo) on delete Cascade,
    constraint FK_ESF_USU foreign key (USU_CODIGO_DONO) references USU(Codigo),
    constraint FK_ESF_ESFL foreign key (LOTE, EMP_CODIGO) REFERENCES ESFL (LOTE, EMP_CODIGO) on delete Cascade);
.go

/*empregados da escala de folga*/
CREATE TABLE EEF (
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    ESF_CODIGO  VARCHAR(6) NOT NULL,
    EPG_CODIGO  VARCHAR(6) NOT NULL,
    CONSTRAINT PK_EEF PRIMARY KEY (EMP_CODIGO, ESF_CODIGO, EPG_CODIGO),
    CONSTRAINT FK_EEF_ESF FOREIGN KEY (EMP_CODIGO, ESF_CODIGO) REFERENCES ESF (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_EEF_EPG FOREIGN KEY (EMP_CODIGO, EPG_CODIGO) REFERENCES EPG (EMP_CODIGO, CODIGO)
);
.go

/*dias dos empregados da escala de folga*/
CREATE TABLE DEF (
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    ESF_CODIGO  VARCHAR(6) NOT NULL,
    EPG_CODIGO  VARCHAR(6) NOT NULL,
    DATA        timestamp NOT NULL,
    TPF_SIGLA   VARCHAR(2),
    TRABALHODIAFOLGA BOOLEAN,
    CONSTRAINT PK_DEF PRIMARY KEY (EMP_CODIGO, ESF_CODIGO, EPG_CODIGO, DATA),
    CONSTRAINT FK_DEF_EEF FOREIGN KEY (EMP_CODIGO, ESF_CODIGO, EPG_CODIGO) REFERENCES EEF (EMP_CODIGO, ESF_CODIGO, EPG_CODIGO),
    CONSTRAINT FK_DEF_ESF FOREIGN KEY (EMP_CODIGO, ESF_CODIGO) REFERENCES ESF (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_DEF_TPF FOREIGN KEY (EMP_CODIGO, TPF_SIGLA) REFERENCES TPF (EMP_CODIGO, SIGLA),
    CONSTRAINT UK_DEF_EPG_DATA UNIQUE (EMP_CODIGO, DATA, EPG_CODIGO)
);
.go

/* Lançamentos do Banco de Horas */
create table LBH (
  EMP_CODIGO  VARCHAR(4) not null,
  EPG_CODIGO  VARCHAR(6) not null,
  DATA        timestamp not null,
  SEQ         INT not null,
  ORIGEM      INT, /* 0: manual; 1: automático - turno; 2: automático - conversão */
  MINUTOS     INT,
  NATUREZA    INT constraint cc_LBH_Natureza check(Natureza in (1, -1)), /* 1: Crédito; -1: Débito */
  DESCRICAO   VARCHAR(60),
  CREDITODESPREZADO INT default 0 not null,
  DEBITODESPREZADO INT default 0 not null,
  ID_CONV     INT,
  ENVIARFOLHA INT default 0 not null,
  PERCENTUAL  FLOAT default 0 not null,
  ACRESCIMO   INT default 0 not null,
  TURNO       INT,
  EVE_CONVCREDOR varchar(3),
  EVE_CONVDEVEDOR varchar(3),
  EVE_EXPORTACAO varchar(3),
  EFX_HASH_ID VARCHAR(11),
  DATA_EXPORTACAO timestamp,
  ID_CONV_PAI int,
  Saldo Int,
  StatusConversao Int,
  constraint PK_LBH primary key (EMP_CODIGO, EPG_CODIGO, DATA, SEQ),
  constraint UK_LBH unique (EMP_CODIGO, EPG_CODIGO, DATA, ORIGEM, DESCRICAO, ID_CONV, PERCENTUAL, EFX_HASH_ID),
  constraint FK_LBH_EPG foreign key (EMP_CODIGO, EPG_CODIGO) references EPG(EMP_CODIGO, CODIGO) on delete cascade,
	constraint FK_LBH_EFX foreign key (EMP_CODIGO, EFX_HASH_ID) references EFX (EMP_CODIGO, HASH_ID) on delete cascade
);
.go

CREATE TABLE FBH(
    EMP_CODIGO VARCHAR(4) NOT NULL,
    EPG_CODIGO VARCHAR(6) NOT NULL,
    DATA TIMESTAMP NOT NULL,
    TURNO INT Default -1 not null,
    DESTINO INT,
    CONSTRAINT PK_FBH PRIMARY KEY (EMP_CODIGO,EPG_CODIGO,DATA,TURNO));
.go

CREATE trigger t_mat_beforeinsert for mat
active before insert position 0
as
  declare variable wMAT_Codigo_Mae varchar(10);
begin
  select max(Codigo) from MAT where new.Codigo like Codigo||'%' into wMAT_Codigo_Mae;
  if (wMAT_Codigo_Mae is not null) then
    new.MAT_Codigo_Mae=wMAT_Codigo_Mae;
end
.go

create table MMAT
(
    ID          INTEGER NOT NULL,
    MAT_CODIGO  VARCHAR(10) NOT NULL,
    constraint PK_MMAT primary key (id,mat_codigo)
);
.go

create table ERE(
  EMP_Codigo varchar(4) not null,
  EPG_Codigo varchar(6) not null,
  REL_Codigo varchar(3) not null,
  Lote integer,
  Data  timestamp,
  VerificarDigital boolean,
  InformarMatricula boolean,
  Constraint PK_ERE primary key(EMP_Codigo, EPG_Codigo,REL_Codigo),
  Constraint FK_EMP foreign key (EMP_Codigo) references EMP (Codigo),
  Constraint FK_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
  Constraint FK_REL foreign key (REL_Codigo) references REL(Codigo));
.go

create table UPV (
  USU_CODIGO varchar(20) not null,
  EMP_CODIGO varchar(4) not null,
  VISIBILIDADE_TOTAL boolean default 0 not null,
  constraint PK_UPV primary key (USU_CODIGO, EMP_CODIGO),
  constraint FK_UPV_USU foreign key (USU_CODIGO) references USU (CODIGO),
  constraint FK_UPV_EMP foreign key (EMP_CODIGO) references EMP (CODIGO) on delete cascade
);
.go

create table UPG
(
  USU_CODIGO varchar(20) not null,
  EMP_CODIGO varchar(4) not null,
  EPG_CODIGO varchar(6) not null,
  STATUSENVIOAPP INTEGER DEFAULT 0 NOT NULL,
  constraint PK_UPG primary key (USU_CODIGO, EMP_CODIGO, EPG_CODIGO),
  constraint FK_UPG_EPG foreign key (EMP_CODIGO, EPG_CODIGO) references EPG (EMP_CODIGO, CODIGO) on delete cascade,
  constraint FK_UPG_UPV foreign key (USU_CODIGO, EMP_CODIGO) references UPV (USU_CODIGO, EMP_CODIGO) on delete cascade);
.go

create table BTA
(
EMP_CODIGO varchar(4) not null,
EPG_CODIGO varchar(6) not null,
BTP_PREVISTA TimeStamp not null,
BTP_REALIZADA TimeStamp not null,
DESCONSIDERAR integer default 0 not null,
STATUSENVIOAPP integer default 0 not null,
constraint PK_BTA primary key (EMP_CODIGO, EPG_CODIGO, BTP_PREVISTA),
constraint FK_BTA_EPG foreign key (EMP_CODIGO, EPG_CODIGO) references EPG (EMP_CODIGO, CODIGO));
.go

create table XXX_BTA
(
EMP_CODIGO varchar(4) not null,
EPG_CODIGO varchar(6) not null,
BTP_PREVISTA timestamp not null,
BTP_REALIZADA timestamp not null,
DESCONSIDERAR integer default 0 not null,
constraint PK_XXX_BTA primary key (EMP_CODIGO, EPG_CODIGO, BTP_PREVISTA),
CONSTRAINT FK_XXX_BTA_EPG FOREIGN KEY (EMP_CODIGO,EPG_CODIGO) REFERENCES EPG(EMP_CODIGO,CODIGO) on delete CASCADE);
.go

create table ded
(
    Codigo integer not null,
    Nome varchar(40),
    constraint PK_DED primary key (codigo)
)
.go

create table dep
(
    EMP_Codigo varchar(4) not null,
    EPG_Codigo varchar(6) not null,
    DED_Codigo integer not null,
    Digital Blob Sub_Type 0 Segment size 80 not null,
    constraint PK_DEP primary key (EMP_Codigo, EPG_Codigo, DED_Codigo),
    constraint FK_DEP_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG (emp_codigo, codigo),
    constraint FK_DEP_DED foreign key (DED_Codigo) references DED (Codigo)
)
.go

create table MESF
(
    ID          INTEGER NOT NULL,
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    ESF_CODIGO  VARCHAR(6) NOT NULL,
    CONSTRAINT PK_MESF PRIMARY KEY (ID, EMP_CODIGO, ESF_CODIGO)
)
.go

create table LEG (
  EMP_Codigo     varchar(4) not null,
  CODIGO         varchar(2) not null,
  BTPINICIAL     timestamp not null,
  BTPFINAL       timestamp not null,
  constraint pk_LEG primary key (EMP_Codigo, BTPINICIAL, BTPFINAL),
  constraint FK_LEG_EMP foreign key(EMP_CODIGO) references EMP (CODIGO)
);
.go

create table XXX_HEA(EMP_CODIGO varchar(4) not null,
                     EPG_CODIGO varchar(6) not null,
                     DATA_REF timestamp,
                     QUANTIDADE float,
                     EVE_CODIGO varchar(3) not null,
                     BTP_INICIAL timestamp not null,
                     BTP_FINAL timestamp   not null,
                     JUSTIFICATIVA varchar(255),
                     constraint PK_XXX_HEA primary key (EMP_CODIGO,EPG_CODIGO, BTP_INICIAL, BTP_FINAL),
                     CONSTRAINT FK_XXX_HEA_EPG FOREIGN KEY (EMP_CODIGO,EPG_CODIGO) REFERENCES EPG(EMP_CODIGO,CODIGO) on delete CASCADE);
.go

create table MMAF(
  ID integer not null,
  MAF_CODIGO varchar(3) not null,
  constraint PK_MMAF primary key (ID, MAF_CODIGO)
);
.go

create table MCID(
  ID integer not null,
  CID_CODIGO varchar(4) not null,
  constraint PK_MCID primary key (ID, CID_CODIGO)
);
.go

create table VRF (
    EMP_Codigo varchar(4) not null,
    Tipo       varchar(2) not null,
    Descricao  varchar(25),
    Categoria  varchar(2) constraint CC_VRF_CATEGORIA check(Categoria is null or Categoria in ('VA','VR')),
    constraint pk_VRF primary key (EMP_Codigo,Tipo),
    constraint fk_VRF_EMP foreign key (EMP_Codigo) references EMP(Codigo))
.go

create table MVRF(
  ID integer not null,
  EMP_CODIGO varchar(4) not null,
  VRF_TIPO   varchar(2) not null,
  constraint PK_MVRF primary key (ID, EMP_CODIGO, VRF_TIPO)
)
.go

create table VVR (
    EMP_Codigo varchar(4)       not null,
    VRF_Tipo   varchar(2)       not null,
    Data       timestamp        not null,
    Valor      double precision not null,
    constraint pk_VVR primary key (Emp_Codigo,VRF_Tipo,Data),
    constraint fk_VVR_VRF foreign key (EMP_Codigo,VRF_Tipo) references VRF(EMP_Codigo,Tipo))
.go

create table VFE (
    EMP_Codigo      varchar(4) not null,
    SEP_EPG_Codigo  varchar(6) not null,
    SEP_Data        timestamp  not null,
    VRF_Tipo        varchar(2) not null,
    constraint pk_VFE primary key (EMP_Codigo,SEP_EPG_Codigo,SEP_Data,VRF_Tipo),
    constraint fk_VFE_SEP foreign key (EMP_Codigo,SEP_EPG_Codigo,SEP_Data) references SEP(EMP_Codigo,EPG_Codigo, Data),
    constraint fk_VFE_VRF foreign key (EMP_CODIGO,VRF_Tipo) references VRF(EMP_CODIGO,Tipo))
.go

CREATE TABLE VTR (
    TIPO        varchar(2) not null,
    DESCRICAO   varchar(25),
    EMP_CODIGO  varchar(4) not null,
    CATEGORIA   varchar(1),
    CONSTRAINT PK_VTR PRIMARY KEY (EMP_CODIGO, TIPO),
    CONSTRAINT FK_VTR_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO)
)
.go

CREATE TABLE VVT (
    VTR_TIPO    VARCHAR(2) NOT NULL,
    EMP_CODIGO  VARCHAR(4) NOT NULL,
    DATA        timestamp NOT NULL,
    VALOR       DOUBLE PRECISION NOT NULL,
    CONSTRAINT PK_VVT PRIMARY KEY (EMP_CODIGO, VTR_TIPO, DATA),
    CONSTRAINT FK_VVT_VTR FOREIGN KEY (EMP_CODIGO, VTR_TIPO) REFERENCES VTR (EMP_CODIGO, TIPO)
)
.go

create table VSE (
    EMP_Codigo      varchar(4) not null,
    SEP_EPG_Codigo  varchar(6) not null,
    SEP_Data        timestamp  not null,
    VTR_Tipo        varchar(2) not null,  /* Tipo de Vale-Transporte */
    Quantidade      integer    not null,  /* Quantidade de Vales-Transporte daquele Tipo */
    Especie         varchar(1) default 'I' not null,
    constraint pk_VSE primary key (EMP_Codigo,SEP_EPG_Codigo,SEP_Data,VTR_Tipo),
    constraint fk_VSE_SEP foreign key (EMP_Codigo,SEP_EPG_Codigo,SEP_Data)  references SEP(EMP_Codigo,EPG_Codigo, Data),
    constraint fk_VSE_VTR foreign key (EMP_CODIGO,VTR_Tipo) references VTR(EMP_CODIGO,Tipo))
.go

create table PVT (
    EMP_Codigo    varchar(4)  not null,
    DtReferencia  timestamp   not null,
    DtInicial     timestamp   not null,
    DtFinal       timestamp   not null,
    constraint PK_PVT     primary key (EMP_Codigo, DtReferencia),
    constraint FK_PVT_EMP foreign key (EMP_Codigo) references EMP(Codigo))
.go

create table PVE (
    EMP_Codigo              varchar(4)  not null,
    PVT_DtReferencia        timestamp   not null,
    EPG_Codigo              varchar(6)  not null,
    VTR_Tipo                varchar(2)  not null,
    QtdDias                 integer     Default 0 not null,
    QtdDiasDireito         integer     Default 0 not null,
    QtdVales                integer     Default 0 not null,
    QtdValesDireito        integer     Default 0 not null,
    Especie                 varchar(1)  not null,
    QtdDescFaltas           integer Default 0 not null,
    QtdDescAfasta           integer Default 0 not null,
    QtdProvAfasta           integer Default 0 not null,
    QtdDescSuspensao        integer Default 0 not null,
    QtdFolgasTrabalhada     integer Default 0 not null,
    QtdValesDescFaltas      integer Default 0 not null,
    QtdValesDescAfasta      integer Default 0 not null,
    QtdValesProvAfasta      integer Default 0 not null,
    QtdValesDescSuspensao   integer Default 0 not null,
    QtdValesFolgaTrabalhada integer Default 0 not null,
    QtdValesOutrosDesconto  integer Default 0 not null,
    QtdValesOutrosAcrescimo integer Default 0 not null,
    QtdValesSobreAvisoTrabalhado integer Default 0 not null,
    QtdDiasSobreAvisoTrabalhado integer Default 0 not null,
    constraint PK_PVE     primary key (EMP_Codigo, PVT_DtReferencia, EPG_Codigo, VTR_Tipo),
    constraint FK_PVE_EPG foreign key (EMP_Codigo, EPG_Codigo)        references EPG(EMP_Codigo, Codigo),
    constraint FK_PVE_PVT foreign key (EMP_Codigo, PVT_DtReferencia)  references PVT(EMP_Codigo, DtReferencia),
    constraint FK_PVE_VTR foreign key (EMP_Codigo, VTR_Tipo)          references VTR(EMP_Codigo, Tipo))
.go

create table AVT (
    EMP_Codigo        varchar(4)  not null,
    PVT_DtReferencia  timestamp   not null,
    EPG_Codigo varchar(6) not null,
    DtInicial  timestamp   not null,
    DtUltimoProcessamento timestamp not null,
    constraint PK_AVT     primary key (EMP_Codigo, PVT_DtReferencia, EPG_Codigo, DtInicial),
    constraint FK_AVT_EMP foreign key (EMP_Codigo) references EMP(Codigo))
.go

CREATE TABLE CHO (
    CH             VARCHAR(4) NOT NULL,
    CONCATHORARIO  VARCHAR(255),
    CONSTRAINT PK_CHO PRIMARY KEY (CH)
)
.go

create table EFE (
   EMP_CODIGO varchar(4) not null,
   ESC_CODIGO integer not null,
   EPG_CODIGO_FOLGUISTA varchar(6) not null,
   EPG_CODIGO_RENDIDO varchar(6) not null,
   DIASRENDIDOS varchar(150),
   constraint PK_EFE primary key (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO_FOLGUISTA, EPG_CODIGO_RENDIDO),
   constraint FK_EFE_FOLGUISTA foreign key (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO_FOLGUISTA) references EES (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO),
   constraint FK_EFE_RENDIDO foreign key (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO_RENDIDO) references EES (EMP_CODIGO, ESC_CODIGO, EPG_CODIGO)
)
.go

/* Agendamentos para sincronização */
create table AGE (
  EMP_Codigo  varchar(4) not null,
  Tipo        integer default 1 not null,
  Hora        timestamp not null,
  Ultima_Coleta timestamp,
  Hora_Final  timestamp,
  Status      integer default 1 not null,
  Encerrar_Periodo  boolean default 0 not null,
  Processado_Ate timestamp,
  constraint PK_AGE primary key (EMP_Codigo, Tipo, Hora),
  constraint FK_AGE_EMP foreign key (EMP_Codigo) references EMP (Codigo)
);
.go

create generator g_ID_SEM;
.go

/* Semáforos */
create table SEM (
  EMP_Codigo varchar(4) not null,
  ID integer not null,
  Maquina varchar(15) not null,
  HoraInicio timestamp not null,
  Operacao varchar(255) not null,
  constraint PK_SEM primary key (EMP_Codigo, ID),
  constraint FK_SEM_EMP foreign key (EMP_Codigo) references EMP (Codigo)
);
.go

/* Sincronização - Empregados excluídos do AC Pessoal */
create table SIN_EPG_EXC (
  EMP_Codigo varchar(4) not null,
  EPG_Codigo varchar(6) not null,
  Num_Chapa varchar(10),
  EPG_Nome varchar(70),
  constraint PK_SIN_EPG_EXC primary key (EMP_Codigo, EPG_Codigo),
  constraint FK_SIN_EPG_EXC_EMP foreign key (EMP_Codigo) references EMP (Codigo)
);
.go

/* Sincronização - Lotações com máscara incorreta */
create table SIN_LOT_ALT (
  EMP_Codigo varchar(4) not null,
  LOT_Codigo varchar(10) not null,
  NEW_Codigo varchar(20),
  LOT_Nome varchar(60),
  LOT_Codigo_Mae varchar(10),
  constraint PK_SIN_LOT_ALT primary key (EMP_Codigo, LOT_Codigo),
  constraint FK_SIN_LOT_ALT_EMP foreign key (EMP_Codigo) references EMP (Codigo)
);
.go

/* Sincronização - Empregados não inclusos */
create table SIN_EPG_NAO_INC (
  EMP_Codigo varchar(4) not null,
  EPG_Codigo varchar(6) not null,
  EPG_Nome varchar(70) default '' not null,
  Num_Chapa varchar(10),
  constraint PK_SIN_EPG_NAO_INC primary key (EMP_Codigo, EPG_Codigo),
  constraint FK_SIN_EPG_NAO_INC_EMP foreign key (EMP_Codigo) references EMP (Codigo)
);
.go
          
/* Sincronização - Feriados com origens alteradas */
create table SIN_FER_ALT_ORI (
  EMP_Codigo varchar(4) not null,
  ID integer not null,
  FER_Nome varchar(30) not null,
  FER_Tipo varchar(30) not null,
  constraint PK_SIN_FER_ALT_ORI primary key (EMP_Codigo, ID),
  constraint FK_SIN_FER_ALT_ORI_EMP foreign key (EMP_Codigo) references EMP (Codigo)
);
.go

create table XXX_LJA (EMP_Codigo varchar(4) not null,
                      EPG_Codigo varchar(6) not null,
                      Datahora   timestamp  not null,
                      MAT_Codigo varchar(10) not null,
                      Abono      boolean,
                      Obs        varchar(255),
                      constraint pk_XXX_LJA  primary key (EMP_Codigo,EPG_Codigo,DataHora),
                      CONSTRAINT FK_XXX_LJA_EPG FOREIGN KEY (EMP_CODIGO,EPG_CODIGO) REFERENCES EPG(EMP_CODIGO,CODIGO) on delete CASCADE);
.go

create table XXX_LBH (
  EMP_CODIGO  VARCHAR(4) not null,
  EPG_CODIGO  VARCHAR(6) not null,
  DATA        timestamp not null,
  SEQ         INT not null,
  ORIGEM      INT, /* 0: manual; 1: automático - turno; 2: automático - conversão */
  MINUTOS     INT,
  NATUREZA    INT constraint cc_XXX_LBH_Natureza check(Natureza in (1, -1)), /* 1: Crédito; -1: Débito */
  DESCRICAO   VARCHAR(60),
  CREDITODESPREZADO INT default 0 not null,
  DEBITODESPREZADO INT default 0 not null,
  ID_CONV     INT,
  ENVIARFOLHA INT default 0 not null,
  constraint PK_XXX_LBH primary key (EMP_CODIGO, EPG_CODIGO, DATA, SEQ),
  constraint UK_XXX_LBH unique (EMP_CODIGO,EPG_CODIGO,DATA,ORIGEM,DESCRICAO, ID_CONV),
  CONSTRAINT FK_XXX_LBH_EPG FOREIGN KEY (EMP_CODIGO,EPG_CODIGO) REFERENCES EPG(EMP_CODIGO,CODIGO) on delete CASCADE);
.go

create table GVR (
  EMP_CODIGO varchar(4) not null,
  CODIGO integer not null,
  DESCRICAO varchar(200) not null,
  DATAGERACAO timestamp not null,
  DTREFERENCIA timestamp not null,
  DTINICIAL timestamp not null,
  DTFINAL timestamp not null,
  GERARMINIMO integer default 0,
  MINIMO integer,
  DESCONTARFALTA integer default 0,
  DTINICIAL_DESCFAL timestamp,
  DTFINAL_DESCFAL timestamp,
  DESCONTARAFAEMERGENCIAL integer default 0 not null,
  DESCONTARAFAELETIVO integer default 0 not null,
  GERARCURSOAPRENDIZ integer default 0 not null,
  COMENTARIO VARCHAR(200),
  constraint PK_GVR primary key (EMP_CODIGO, CODIGO),
  constraint FK_GVR_EMP foreign key (EMP_CODIGO) references EMP (CODIGO));
.go

create table EGV(
  EMP_CODIGO varchar(4) not null,
  GVR_CODIGO integer not null,
  EPG_CODIGO varchar(6) not null,
  VRF_TIPO varchar(2) not null,
  QTD_VRF integer not null,
  QTD_VRF_CALCULADO integer not null,  
  MEM_CALCULO BLOB SUB_TYPE 1 SEGMENT SIZE 80,
  constraint PK_EGV primary key (EMP_CODIGO, GVR_CODIGO, EPG_CODIGO, VRF_TIPO),
  constraint FK_EGV_GVR foreign key (EMP_CODIGO, GVR_CODIGO) references GVR (EMP_CODIGO, CODIGO),
  constraint FK_EGV_EPG foreign key (EMP_CODIGO, EPG_CODIGO) references EPG (EMP_CODIGO, CODIGO),
  constraint FK_EGV_VRF foreign key (EMP_CODIGO, VRF_TIPO) references VRF (EMP_CODIGO, TIPO));
.go

CREATE TABLE ABO (
    EMP_CODIGO        VARCHAR(4) NOT NULL,
    EPG_CODIGO        VARCHAR(6) NOT NULL,
    DATAHORAINICIAL   TIMESTAMP NOT NULL,
    DATAHORAFINAL     TIMESTAMP,
    EVENTO            INTEGER,
    STATUS            INTEGER,
    MAT_CODIGO        VARCHAR(10),
    MAF_CODIGO        VARCHAR(3),
    CRM_CODIGO        VARCHAR(6),
    CID_CODIGO        VARCHAR(4),
    TURNOS            INTEGER Default 0 not null,
    OBSERVACAO        VARCHAR(255),
    ABONODIA          BOOLEAN,
    SEQFALTA          INTEGER,
    EXIBEDETALHE      INTEGER Default 0 not null,
    ATRASODIARIO      INTEGER,    
    LANCADEBITOLBH    INTEGER,
    ABONAFRAC         TIMESTAMP,
	ORIGEM            INTEGER Default 0 not null,
	APROVACAOENVIADAAPP   INTEGER Default 0 not null,
	SOLICITACAOENVIADAAPP INTEGER Default 0 not null,
	ANEXOABONO        varchar(50),
	LOCALANEXOABONO   varchar(200),
	MOTIVO_INDEFERIMENTO VARCHAR(255),
    CONSTRAINT PK_ABO PRIMARY KEY (EMP_CODIGO, EPG_CODIGO, DATAHORAINICIAL, TURNOS),
    CONSTRAINT FK_ABO_CID FOREIGN KEY (CID_CODIGO) REFERENCES CID (CODIGO),
    CONSTRAINT FK_ABO_CRM FOREIGN KEY (CRM_Codigo) REFERENCES CRM (Codigo),
    CONSTRAINT FK_ABO_EPG FOREIGN KEY (EMP_CODIGO, EPG_CODIGO) REFERENCES EPG (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_ABO_MAF FOREIGN KEY (MAF_CODIGO) REFERENCES MAF (CODIGO),
    CONSTRAINT FK_ABO_MAT FOREIGN KEY (MAT_CODIGO) REFERENCES MAT (CODIGO));
.go

CREATE TABLE ABO_EXC (
    EMP_CODIGO        VARCHAR(4) NOT NULL,
    EPG_CODIGO        VARCHAR(6) NOT NULL,
    DATAHORAINICIAL   TIMESTAMP NOT NULL,
    EVENTO            INTEGER,
    MAT_CODIGO        VARCHAR(10),
    TURNOS            INTEGER Default 0 not null,
    ABONODIA          BOOLEAN,
    CONSTRAINT PK_ABO_EXC PRIMARY KEY (EMP_CODIGO, EPG_CODIGO, DATAHORAINICIAL, TURNOS),
    CONSTRAINT FK_ABO_EXC_EPG FOREIGN KEY (EMP_CODIGO, EPG_CODIGO) REFERENCES EPG (EMP_CODIGO, CODIGO),
    CONSTRAINT FK_ABO_EXC_MAT FOREIGN KEY (MAT_CODIGO) REFERENCES MAT (CODIGO));
.go

create trigger T_ABO_BeforeDelete for ABO active before delete as
begin
  /*
  Esta trigger insere registros na tabela ABO_EXC, utilizada na integracao com o Fortes Colabore, para garantir que a solicitação de abono seja excluida da nuvem no caso de
  falha no consumo da URL momento da exclusao
  */
  if ((select CFE.VALOR from CFE where CFE.EMP_CODIGO = OLD.EMP_CODIGO and CFE.CODIGO = 'APPCOLABORADOR' )=1) then
  begin
    if (not exists (SELECT 1 FROM ABO_EXC WHERE EMP_CODIGO = OLD.EMP_CODIGO AND EPG_CODIGO = OLD.EPG_CODIGO AND DATAHORAINICIAL = OLD.DATAHORAINICIAL AND TURNOS = OLD.TURNOS)) then
      INSERT INTO ABO_EXC SELECT EMP_CODIGO, EPG_CODIGO, DATAHORAINICIAL, EVENTO, MAT_CODIGO, TURNOS,  ABONODIA FROM ABO WHERE SOLICITACAOENVIADAAPP =1 AND EMP_CODIGO = OLD.EMP_CODIGO AND EPG_CODIGO = OLD.EPG_CODIGO 
	  AND DATAHORAINICIAL = OLD.DATAHORAINICIAL AND TURNOS = OLD.TURNOS;
  end				  
end;
.go

insert into ded values (1,'Mínimo Esquerdo')
.go
insert into ded values (2,'Anelar Esquerdo')
.go
insert into ded values (3,'Médio Esquerdo')
.go
insert into ded values (4,'Indicador Esquerdo')
.go
insert into ded values (5,'Polegar Esquerdo')
.go
insert into ded values (6,'Polegar Direito')
.go
insert into ded values (7,'Indicador Direito')
.go
insert into ded values (8,'Médio Direito')
.go
insert into ded values (9,'Anelar Direito')
.go
insert into ded values (10,'Mínimo Direito')
.go

create generator g_ID;
.go

/* Tabela de Modelos de Relógio */
insert into fab (codigo, nome) values ('001','ControlID');
.go
insert into fab (codigo, nome) values ('002','Henry');
.go
insert into fab (codigo, nome) values ('003','Top Data');
.go
insert into fab (codigo, nome) values ('004','FingerSec');
.go
insert into fab (codigo, nome) values ('005','Biometrix');
.go
insert into fab (codigo, nome) values ('006','Trix');
.go
insert into fab (codigo, nome) values ('007','Dimep');
.go
insert into fab (codigo, nome) values ('008','Henry Prisma');
.go
insert into fab (codigo, nome) values ('009', 'Gertec');
.go
insert into fab (codigo, nome) values ('010', 'ID Data');
.go
insert into fab (codigo, nome) values ('011', 'ZPM');
.go
insert into fab (codigo, nome) values ('013','ControlID Port. 1510');
.go
insert into fab (codigo, nome) values ('012', 'KeyPass');
.go
insert into fab (codigo, nome) values ('014','Proveu');
.go
insert into mdl (fab_codigo, codigo, nome) values ('001','001','ID 200');
.go
insert into mdl (fab_codigo, codigo, nome) values ('001','002','ID 618');
.go
insert into mdl (fab_codigo, codigo, nome) values ('001','003','ID 628');
.go
insert into mdl (fab_codigo, codigo, nome) values ('001','004','ID 6000');
.go
insert into mdl (fab_codigo, codigo, nome) values ('001','005','ID 8000');
.go
insert into mdl (fab_codigo, codigo, nome) values ('002','001','CARD I');
.go
insert into mdl (fab_codigo, codigo, nome) values ('002','002','CARD II');
.go
insert into mdl (fab_codigo, codigo, nome) values ('002','003','CARD III');
.go
insert into mdl (fab_codigo, codigo, nome) values ('002','004','CARD IV');
.go
insert into mdl (fab_codigo, codigo, nome) values ('002','005','CARD V');
.go
insert into mdl (fab_codigo, codigo, nome) values ('002','006','Bio Card');
.go
insert into mdl (fab_codigo, codigo, nome) values ('002','007','Super Fácil');
.go
insert into mdl(fab_codigo, codigo, nome) values('002','008','ORION 6');
.go
insert into mdl (fab_codigo, codigo, nome) values ('003','008','EasyInner');
.go
insert into mdl (fab_codigo,codigo,nome) values ('003','009','T 1000');
.go
insert into mdl(fab_codigo,codigo,nome) values ('004','010','FS 9400 IP');
.go
insert into mdl (fab_codigo,codigo,nome) values ('005','001','400');
.go
insert into mdl (fab_codigo,codigo,nome) values ('007','001','PrintPoint II');
.go
insert into mdl (fab_codigo,codigo,nome) values ('007','002','MiniPrint');
.go
insert into mdl (fab_codigo,codigo,nome) values ('003','010','Inner Rep');
.go
insert into mdl (fab_codigo, codigo, nome) values ('008','001','Super Fácil');
.go
insert into mdl (fab_codigo, codigo, nome) values ('008','002','Prisma E');
.go
insert into mdl (fab_codigo, codigo, nome) values ('008','003','Prisma Hexa6');
.go
insert into mdl (fab_codigo, codigo, nome) values ('008','004','Prisma ADVANCED');
.go
insert into mdl (fab_codigo, codigo, nome) values ('009', '001', 'REP - BPD');
.go
insert into mdl (fab_codigo, codigo, nome) values ('009', '002', 'REP - G3');
.go
insert into mdl (fab_codigo, codigo, nome) values ('010', '001', 'ID REP');
.go
insert into mdl (fab_codigo, codigo, nome) values ('011', '001', 'R300');
.go
insert into mdl (fab_codigo, codigo, nome) values ('011', '002', 'R130 Lite')
.go
insert into mdl (fab_codigo, codigo, nome) values ('011', '003', 'ECO 500 BPR')
.go
insert into mdl (fab_codigo, codigo, nome) values ('012','001','KP1510');
.go
insert into mdl (fab_codigo, codigo, nome) values ('013','001','IDX');
.go
insert into mdl (fab_codigo, codigo, nome) values ('013','002','IDClass');
.maygo
insert into mdl (fab_codigo,codigo,nome) values ('003','011','Inner Rep Plus');
.maygo
insert into mdl (fab_codigo,codigo,nome) values ('007','003','PrintPoint III');
.maygo
insert into mdl (fab_codigo,codigo,nome) values ('003','012','Inner Rep Bioprox 2i');
.maygo
insert into MDL (FAB_Codigo, Codigo, Nome) values ('003', '013', 'Inner Rep Plus LFD');
.maygo
insert into mdl (fab_codigo,codigo,nome) values ('014','001','Kurumim REP 3 MAX BR PX');
.go
insert into mdl (fab_codigo,codigo,nome) values ('007','004', 'Smart B');
.go

insert into MOD values ('CD', 'Cadastros', 0);
.go
insert into MOD values ('CD.REL', 'Relógios', 0);
.go
insert into MOD values ('CD.REL.E', 'Excluir', 1);
.go
insert into MOD values ('CD.REL.I', 'Incluir', 1);
.go
insert into MOD values ('CD.REL.A', 'Editar', 1);
.go
insert into MOD values ('CD.REL.J', 'Ajustar', 1);
.go
insert into MOD values ('CD.REL.G','Gerenciador de Agendamentos',1);
.go
insert into MOD values ('CD.LBU', 'Leitores Biométricos USB', 0);
.go
insert into MOD values ('CD.LBU.I', 'Incluir', 1);
.go
insert into MOD values ('CD.LBU.A', 'Editar', 1);
.go
insert into MOD values ('CD.LBU.E', 'Excluir', 1);
.go
insert into MOD values ('CD.EPG', 'Empregados', 0);
.go
insert into MOD values ('CD.EPG.CAD', 'Cadastro', 0);
.go
insert into MOD values ('CD.EPG.CAD.E', 'Excluir', 1);
.go
insert into MOD values ('CD.EPG.CAD.I', 'Incluir', 1);
.go
insert into MOD values ('CD.EPG.CAD.A', 'Editar', 1);
.go
insert into MOD values ('CD.EPG.CAD.L', 'Listar', 1);
.go
insert into MOD values ('CD.EPG.CAD.ERE','Assoc. com Relógios',1);
.go
insert into MOD values ('CD.EPG.CAD.ELB', 'Assoc. com Leitor Biométrico', 1);
.go
insert into MOD values ('CD.EPG.CAD.APP', 'Integração com o Fortes Colabore', 1);
.go
insert into MOD values ('CD.HOR', 'Horários', 0);
.go
insert into MOD values ('CD.HOR.E', 'Excluir', 1);
.go
insert into MOD values ('CD.HOR.I', 'Incluir', 1);
.go
insert into MOD values ('CD.HOR.A', 'Editar', 1);
.go
insert into MOD values ('CD.HOR.C', 'Copiar', 1);
.go
insert into MOD values ('CD.TOM', 'Obra/Tomador', 0);
.go
insert into MOD values ('CD.TOM.E', 'Excluir', 1);
.go
insert into MOD values ('CD.TOM.I', 'Incluir', 1);
.go
insert into MOD values ('CD.TOM.A', 'Editar', 1);
.go
insert into MOD values ('CD.LEG', 'Legendas', 0);
.go
insert into MOD values ('CD.LEG.E', 'Excluir', 1);
.go
insert into MOD values ('CD.LEG.I', 'Incluir', 1);
.go
insert into MOD values ('CD.LEG.A', 'Editar', 1);
.go
insert into MOD values ('CD.LOT', 'Lotações', 0);
.go
insert into MOD values ('CD.LOT.E', 'Excluir', 1);
.go
insert into MOD values ('CD.LOT.I', 'Incluir', 1);
.go
insert into MOD values ('CD.LOT.A', 'Editar', 1);
.go
insert into MOD values ('CD.LOT.J', 'Ajustar', 1);
.go
insert into MOD values ('CD.VRF', 'Vales-Refeição', 0);
.go
insert into MOD values ('CD.VRF.E', 'Excluir', 1);
.go
insert into MOD values ('CD.VRF.I', 'Incluir', 1);
.go
insert into MOD values ('CD.VRF.A', 'Editar', 1);
.go
insert into MOD values ('CD.VTR', 'Vales-Transporte', 0);
.go
insert into MOD values ('CD.VTR.E', 'Excluir', 1);
.go
insert into MOD values ('CD.VTR.I', 'Incluir', 1);
.go
insert into MOD values ('CD.VTR.A', 'Editar', 1);
.go
insert into MOD values ('CD.CAR', 'Cargos', 0);
.go
insert into MOD values ('CD.CAR.E', 'Excluir', 1);
.go
insert into MOD values ('CD.CAR.I', 'Incluir', 1);
.go
insert into MOD values ('CD.CAR.A', 'Editar', 1);
.go
insert into MOD values ('CD.MAF', 'Motivos de Afastamento', 0);
.go
insert into MOD values ('CD.MAF.E', 'Excluir', 1);
.go
insert into MOD values ('CD.MAF.I', 'Incluir', 1);
.go
insert into MOD values ('CD.MAF.A', 'Editar', 1);
.go
insert into MOD values ('CD.FER', 'Feriados', 0);
.go
insert into MOD values ('CD.FER.GER', 'Gerais', 0);
.go
insert into MOD values ('CD.FER.GER.FFX', 'Fixos', 0);
.go
insert into MOD values ('CD.FER.GER.FFX.E', 'Excluir', 1);
.go
insert into MOD values ('CD.FER.GER.FFX.I', 'Incluir', 1);
.go
insert into MOD values ('CD.FER.GER.FFX.A', 'Editar', 1);
.go
insert into MOD values ('CD.FER.GER.FMV', 'Móveis', 0);
.go
insert into MOD values ('CD.FER.GER.FMV.E', 'Excluir', 1);
.go
insert into MOD values ('CD.FER.GER.FMV.I', 'Incluir', 1);
.go
insert into MOD values ('CD.FER.GER.FMV.A', 'Editar', 1);
.go
insert into MOD values ('CD.FER.EMP', 'Desta Empresa', 0);
.go
insert into MOD values ('CD.FER.EMP.FFE', 'Fixos', 0);
.go
insert into MOD values ('CD.FER.EMP.FFE.E', 'Excluir', 1);
.go
insert into MOD values ('CD.FER.EMP.FFE.I', 'Incluir', 1);
.go
insert into MOD values ('CD.FER.EMP.FFE.A', 'Editar', 1);
.go
insert into MOD values ('CD.FER.EMP.FME', 'Móveis', 0);
.go
insert into MOD values ('CD.FER.EMP.FME.E', 'Excluir', 1);
.go
insert into MOD values ('CD.FER.EMP.FME.I', 'Incluir', 1);
.go
insert into MOD values ('CD.FER.EMP.FME.A', 'Editar', 1);
.go
insert into MOD values ('CD.EMP', 'Empresas', 0);
.go
insert into MOD values ('CD.EMP.E', 'Excluir', 1);
.go
insert into MOD values ('CD.EMP.I', 'Incluir', 1);
.go
insert into MOD values ('CD.EMP.A', 'Editar', 1);
.go
insert into MOD values ('CD.USU', 'Usuários', 0);
.go
insert into MOD values ('CD.USU.PER', 'Perfis', 0);
.go
insert into MOD values ('CD.USU.PER.E', 'Excluir', 1);
.go
insert into MOD values ('CD.USU.PER.I', 'Incluir', 1);
.go
insert into MOD values ('CD.USU.PER.A', 'Editar', 1);
.go
insert into MOD values ('CD.USU.CAD', 'Cadastro', 0);
.go
insert into MOD values ('CD.USU.CAD.E', 'Excluir', 1);
.go
insert into MOD values ('CD.USU.CAD.I', 'Incluir', 1);
.go
insert into MOD values ('CD.USU.CAD.A', 'Editar', 1);
.go
insert into MOD values ('CD.USU.CAD.NBH', 'Notificação Vencimento Banco de Horas', 1);
.go
insert into MOD values ('CD.USU.AUE', 'Associação com Empregado', 0);
.go
insert into MOD values ('CD.USU.UVP', 'Visibilidade de Empregados', 0);
.go
insert into MOD values ('CD.USU.UVP.E', 'Excluir', 1);
.go
insert into MOD values ('CD.USU.UVP.I', 'Incluir', 1);
.go
insert into MOD values ('CD.USU.UVP.A', 'Editar', 1);
.go
insert into MOD values ('CD.LEI', 'Leiautes', 0);
.go
insert into MOD values ('CD.LEI.LEI', 'Leiautes de Importação de Batidas', 0);
.go
insert into MOD values ('CD.LEI.LEI.I', 'Incluir', 1);
.go
insert into MOD values ('CD.LEI.LEI.A', 'Editar', 1);
.go
insert into MOD values ('CD.LEI.LEI.E', 'Excluir', 1);
.go
insert into MOD values ('CD.LEI.LER', 'Leiautes de Exportação para o Relógio', 0);
.go
insert into MOD values ('CD.LEI.LER.I', 'Incluir', 1);
.go
insert into MOD values ('CD.LEI.LER.A', 'Editar', 1);
.go
insert into MOD values ('CD.LEI.LER.E', 'Excluir', 1);
.go
insert into MOD values ('CD.LEI.LEIEVT', 'Leiautes de Exportação de Eventos', 0);
.go
insert into MOD values ('CD.LEI.LEIEVT.A', 'Editar', 1);
.go
insert into MOD values ('CD.LEI.LEIEVT.E', 'Excluir', 1);
.go
insert into MOD values ('CD.LEI.LEIEVT.I', 'Incluir', 1);
.go
insert into MOD values ('CD.USU.CAD.S', 'Alterar Senha', 1);
.go
insert into MOD values ('CD.MUN', 'Municípios', 0);
.go
insert into MOD values ('CD.MUN.E', 'Excluir', 1);
.go
insert into MOD values ('CD.MUN.I', 'Incluir', 1);
.go
insert into MOD values ('CD.MUN.A', 'Editar', 1);
.go
insert into MOD values ('CD.UFD', 'Unidades Federativas', 0);
.go
insert into MOD values ('CD.UFD.E', 'Excluir', 1);
.go
insert into MOD values ('CD.UFD.I', 'Incluir', 1);
.go
insert into MOD values ('CD.UFD.A', 'Editar', 1);
.go
insert into MOD values ('CD.MAT','Motivos de Atraso/SA e Falta',0);
.go
insert into MOD values ('CD.MAT.A','Editar',1);
.go
insert into MOD values ('CD.MAT.E','Excluir',1);
.go
insert into MOD values ('CD.MAT.I','Incluir',1);
.go
insert into MOD values ('CD.CRM','Profissional de Saúde',0);
.go
insert into MOD values ('CD.CRM.A','Editar',1);
.go
insert into MOD values ('CD.CRM.E','Excluir',1);
.go
insert into MOD values ('CD.CRM.I','Incluir',1);
.go
insert into MOD values ('CD.CEM', 'Classe dos Empregados', 0);
.go
insert into MOD values ('CD.CEM.A','Editar',1);
.go
insert into MOD values ('CD.CEM.E','Excluir',1);
.go
insert into MOD values ('CD.CEM.I','Incluir',1);
.go
insert into MOD values ('CD.OCR', 'Ocorrências', 0);
.go
insert into MOD values ('CD.OCR.A', 'Editar', 1);
.go
insert into MOD values ('CD.OCR.E', 'Excluir', 1);
.go
insert into MOD values ('CD.OCR.I', 'Incluir', 1);
.go
insert into MOD values ('CD.HEA', 'Percentuais de Horas-Extras ou Banco de Horas', 0);
.go
insert into MOD values ('CD.HEA.A', 'Editar', 1);
.go
insert into MOD values ('CD.HEA.E', 'Excluir', 1);
.go
insert into MOD values ('CD.HEA.I', 'Incluir', 1);
.go
insert into MOD values ('CD.TPF', 'Tipos de Folgas', 0);
.go
insert into MOD values ('CD.TPF.I', 'Incluir', 1);
.go
insert into MOD values ('CD.TPF.A', 'Editar', 1);
.go
insert into MOD values ('CD.TPF.E', 'Excluir', 1);
.go
insert into MOD values ('CD.FRC', 'Regras de Horas-Extras e Banco de Horas', 0);
.go
insert into MOD values ('CD.FRC.I', 'Incluir', 1);
.go
insert into MOD values ('CD.FRC.A', 'Editar', 1);
.go
insert into MOD values ('CD.FRC.E', 'Excluir', 1);
.go
insert into mod values ('CD.REL.N','Último NSR',1);
.go
insert into MOD values ('CD.RCO', 'Regras de Conversões de Banco de Horas', 0)
.go
insert into MOD values ('CD.RCO.I', 'Incluir', 1);
.go
insert into MOD values ('CD.RCO.A', 'Editar', 1);
.go
insert into MOD values ('CD.RCO.E', 'Excluir', 1);
.go
insert into MOD values ('CD.EPG.RBH.S','Assistente',1);
.go
insert into mod values ('CD.RFC','Refeições',0);
.go
insert into mod values ('CD.RFC.I','Incluir',1);
.go
insert into mod values ('CD.RFC.E','Excluir',1);
.go
insert into mod values ('CD.RFC.A','Editar',1);
.go
insert into mod values ('CD.RFT','Refeitórios',0);
.go
insert into mod values ('CD.RFT.I','Incluir',1);
.go
insert into mod values ('CD.RFT.E','Excluir',1);
.go
insert into mod values ('CD.RFT.A','Editar',1);
.go
insert into MOD values ('MV', 'Movimentos', 0);
.go
insert into MOD values ('MV.EXT', 'Exportar', 0);
.go
insert into MOD values ('MV.EXT.EXP', 'Fortes Pessoal', 0);
.go
insert into MOD values ('MV.EXT.EER', 'Empregados para Relógio', 0);
.go
INSERT INTO MOD VALUES ('MV.EXT.EAE', 'Atestados para o Fortes RH', 0);
.go
insert into MOD values ('MV.EXT.FAA','Faltas e Atrasos abonadas em horas (CSV)', 0);
.go
insert into MOD values ('MV.EXT.EECFT', 'Empregados para catraca Fortes', 0);
.go
insert into MOD values ('MV.CBO', 'Consulta de Batidas', 0);
.go
insert into MOD values ('MV.JUS', 'Justificativas', 0)
.go
insert into MOD values ('MV.JUS.ASA', 'Atrasos e Saídas Antecipadas', 0);
.go
insert into MOD values ('MV.JUS.ASA.I', 'Incluir', 1);
.go
insert into MOD values ('MV.JUS.ASA.A', 'Editar', 1);
.go
insert into MOD values ('MV.JUS.ASA.E', 'Excluir', 1);
.go
insert into MOD values ('MV.JUS.ASA.G', 'Gerar Lote', 1);
.go
insert into MOD values ('MV.JUS.ASA.X', 'Excluir Lote', 1);
.go
insert into MOD values ('MV.JUS.LJF', 'Faltas', 0);
.go
insert into MOD values ('MV.JUS.LJF.E', 'Excluir', 1);
.go
insert into MOD values ('MV.JUS.LJF.I', 'Incluir', 1);
.go
insert into MOD values ('MV.JUS.LJF.A', 'Editar', 1);
.go
insert into MOD values ('MV.JUS.LJF.G', 'Gerar Lote', 1);
.go
insert into MOD values ('MV.JUS.LJF.X', 'Excluir Lote', 1);
.go
insert into MOD values ('MV.JUS.LJH', 'Horas-Extras', 0);
.go
insert into MOD values ('MV.JUS.LJH.A', 'Editar', 1);
.go
insert into MOD values ('MV.JUS.LJH.E', 'Excluir', 1);
.go
insert into MOD values ('MV.JUS.LJH.I', 'Incluir', 1);
.go
insert into MOD values ('MV.JUS.LJH.G', 'Gerar Lote', 1);
.go
insert into MOD values ('MV.JUS.LJH.X', 'Excluir Lote', 1);
.go
insert into MOD values ('MV.CPF', 'Compensações', 0);
.go
insert into MOD values ('MV.CPF.E', 'Excluir', 1);
.go
insert into MOD values ('MV.CPF.I', 'Incluir', 1);
.go
insert into MOD values ('MV.CPF.A', 'Editar', 1);
.go
insert into MOD values ('MV.CPF.G', 'Gerar Lote', 1);
.go
insert into MOD values ('MV.CPF.X', 'Excluir Lote', 1);
.go
insert into MOD values ('MV.AFA', 'Afastamentos', 0);
.go
insert into MOD values ('MV.AFA.E', 'Excluir', 1);
.go
insert into MOD values ('MV.AFA.I', 'Incluir', 1);
.go
insert into MOD values ('MV.AFA.A', 'Editar', 1);
.go
insert into MOD values ('MV.AFA.D', 'Definir', 1);
.go
insert into MOD values ('MV.AFA.G', 'Gerar Lote', 1);
.go
insert into MOD values ('MV.AFA.X', 'Excluir Lote', 1);
.go
insert into MOD values ('MV.IMP', 'Importar', 0);
.go
insert into MOD values ('MV.IMP.IBR', 'Batidas do Relógio', 0);
.go
insert into MOD values ('MV.IMP.IBP', 'Batidas do Relógio padrão AFD', 0);
.go
insert into MOD values ('MV.LOGIMP', 'Log de Importações', 0);
.go
insert into MOD values ('MV.LOGIMP.BTP', 'Batidas', 0);
.go
insert into MOD values ('MV.LOGIMP.ERR', 'Erros', 0);
.go
insert into MOD values ('MV.APPCOLAB', 'Fortes Colabore', 0);
.go
insert into MOD values ('MV.APPCOLAB.SIN', 'Sincronizar', 0);
.go
insert into MOD values ('MV.APPCOLAB.MSG', 'Enviar Mensagens', 0);
.go
insert into MOD values ('MV.SIT', 'Situações', 0)
.go
insert into MOD values ('MV.SIT.HEM', 'Empresas', 0)
.go
insert into MOD values ('MV.SIT.HEM.I', 'Incluir', 1);
.go
insert into MOD values ('MV.SIT.HEM.A', 'Editar', 1);
.go
insert into MOD values ('MV.SIT.HEM.E', 'Excluir', 1);
.go
insert into MOD values ('MV.SIT.SEP', 'Empregados', 0);
.go
insert into MOD values ('MV.SIT.SEP.E', 'Excluir', 1);
.go
insert into MOD values ('MV.SIT.SEP.I', 'Incluir', 1);
.go
insert into MOD values ('MV.SIT.SEP.A', 'Editar', 1);
.go
insert into MOD values ('MV.SIT.SEP.G', 'Gerar Lote', 1);
.go
insert into MOD values ('MV.SIT.SEP.X', 'Excluir Lote', 1);
.go
insert into MOD values ('MV.PEV', 'Perfil de Eventos', 0);
.go
insert into MOD values ('MV.PEV.E', 'Excluir', 1);
.go
insert into MOD values ('MV.PEV.I', 'Incluir', 1);
.go
insert into MOD values ('MV.PEV.A', 'Editar', 1);
.go
insert into MOD values ('MV.PEV.C', 'Clonar', 1);
.go
insert into MOD values ('MV.BPA', 'Batidas de Ponto Pré-Assinaladas', 0);
.go
insert into MOD values ('MV.BPA.D', 'Desconsiderar', 1);
.go
insert into MOD values ('MV.BPA.R', 'Reconsiderar', 1);
.go
insert into MOD values ('MV.ESL', 'Escalas', 0);
.go
insert into MOD values ('MV.ESL.ESC', 'Trabalho', 0);
.go
insert into MOD values ('MV.ESL.ESC.I', 'Incluir', 1);
.go
insert into MOD values ('MV.ESL.ESC.A', 'Editar', 1);
.go
insert into MOD values ('MV.ESL.ESC.E', 'Excluir', 1);
.go
insert into MOD values ('MV.ESL.ESC.B', 'Bloquear Qualquer Uma', 1);
.go
insert into MOD values ('MV.ESL.ESC.BD', 'Bloquear Somente as Minhas', 1);
.go
insert into MOD values ('MV.ESL.ESC.D', 'Desbloquear Qualquer Uma', 1);
.go
insert into MOD values ('MV.ESL.ESC.AS', 'Assistente', 1);
.go
insert into MOD values ('MV.ESL.ESC.EL', 'Excluir Escala em Lote', 1);
.go
insert into MOD values ('MV.ESL.ESF', 'Folga', 0);
.go
insert into MOD values ('MV.ESL.ESF.I', 'Incluir', 1);
.go
insert into MOD values ('MV.ESL.ESF.A', 'Editar', 1);
.go
insert into MOD values ('MV.ESL.ESF.E', 'Excluir', 1);
.go
insert into MOD values ('MV.ESL.ESF.P', 'Importar', 1);
.go
insert into MOD values ('MV.ESL.ESF.B', 'Bloquear Qualquer Uma', 1);
.go
insert into MOD values ('MV.ESL.ESF.BD', 'Bloquear Somente as Minhas', 1);
.go
insert into MOD values ('MV.ESL.ESF.D', 'Desbloquear Qualquer Uma', 1);
.go
insert into MOD values ('MV.ESL.ESF.AS', 'Assistente', 1);
.go
insert into MOD values ('MV.ESL.ESF.EL', 'Excluir Escala em Lote', 1);
.go
insert into MOD values ('MV.ESL.IES', 'Importar Escalas', 0);
.go
insert into MOD values ('MV.BTP', 'Bater Ponto', 0);
.go
insert into MOD values ('MV.BTP.BTP', 'Bater Ponto', 1);
.go
insert into MOD values ('MV.BTP.REG', 'Registrar Batida', 1);
.go
insert into MOD values ('MV.BTG', 'Gerenciamento Batidas Pendentes', 0);
.go
insert into MOD values ('MV.BTG.EF','Efetivar', 1);
.go
insert into MOD values ('MV.BTG.RE','Reverter', 1);
.go
insert into MOD values ('MV.BTG.GL', 'Geração Bater Ponto Pendentes em Lote', 0);
.go
insert into MOD values ('MV.BTG.EL', 'Exclusão Bater Ponto Pendentes em Lote', 0);
.go
insert into MOD Values ('MV.APRABO', 'Aprovação de Abono',0);
.go
insert into MOD values ('MV.ENP', 'Encerramento Período', 0);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP','Tratamento de Batidas (Inclusões e Desconsiderações)',0);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP.A','Editar',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP.D','Desconsiderar',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP.R','Reconsiderar',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP.E','Excluir',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP.G','Gerar Lote',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP.I','Incluir',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('MV.LBP.X','Excluir Lote',1);
.go
insert into MOD values ('MV.GBH', 'Gerenciamento de Banco de Horas', 0);
.go
insert into MOD values ('MV.GBH.LBH', 'Lançamento do Banco de Horas', 0);
.go
insert into MOD values ('MV.GBH.LBH.E', 'Excluir', 1);
.go
insert into MOD values ('MV.GBH.LBH.I', 'Incluir', 1);
.go
insert into MOD values ('MV.GBH.LBH.A', 'Editar', 1);
.go
insert into MOD values ('MV.GBH.CSB', 'Conversão de Saldo do Banco de Horas', 0);
.go
insert into MOD values ('MV.GBH.CSB.CNV', 'Converter', 0);
.go
insert into MOD values ('MV.GBH.CSB.DES', 'Desfazer', 0);
.go
insert into MOD values ('MV.EPG', 'Empregados', 0);
.go
insert into MOD values ('MV.EPG.OCE', 'Ocorrências', 0);
.go
insert into MOD values ('MV.EPG.OCE.E', 'Excluir', 1);
.go
insert into MOD values ('MV.EPG.OCE.A', 'Editar', 1);
.go
insert into MOD values ('MV.EPG.OCE.I', 'Incluir', 1);
.go
insert into MOD values ('MV.EPG.OCE.G', 'Gerar Lote', 1);
.go
insert into MOD values ('MV.EPG.OCE.X', 'Excluir Lote', 1);
.go
insert into MOD values('MV.EPG.ABO' ,'Solicitação de Abono', 0)
.go
insert into MOD values ('MV.EPG.ABO.I', 'Incluir', 1)
.go
insert into MOD values ('MV.EPG.ABO.A', 'Editar', 1)
.go
insert into MOD values ('MV.EPG.ABO.E', 'Excluir', 1)
.go
insert into MOD values ('MV.EPG.ABO.SAFA', 'Afastamento', 1)
.go
insert into MOD values ('MV.EPG.ABO.SATS', 'Atraso/SA', 1)
.go
insert into MOD values ('MV.EPG.ABO.SFAL', 'Falta', 1)
.go
insert into MOD values('MV.EPG.TEP' ,'Tempo de Espera', 0)
.go
insert into MOD values ('MV.EPG.TEP.I', 'Incluir', 1)
.go
insert into MOD values ('MV.EPG.TEP.A', 'Editar', 1)
.go
insert into MOD values ('MV.EPG.TEP.E', 'Excluir', 1)
.go
insert into MOD values ('MV.RRE', 'Refeições dos Empregados', 0)
.go
insert into MOD values ('MV.RRE.E', 'Excluir', 1);
.go
insert into MOD values ('MV.RRE.I', 'Incluir', 1);
.go
insert into MOD values ('MV.RRE.A', 'Editar', 1);
.go
insert into MOD values ('MV.GAQ', 'Geração de Arquivos', 0);
.go
insert into MOD values ('MV.GAQ.AFDT', 'Arquivo Fonte de Dados Tratado - AFDT', 0);
.go
insert into MOD values ('MV.GAQ.ACJEF', 'Arquivo de Controle de Jornada - ACJEF', 0);
.go
insert into MOD values ('MV.RVR', 'Remessa de Vale-Refeição', 0);
.go
insert into MOD values ('MV.RVR.E', 'Excluir', 1);
.go
insert into MOD values ('MV.RVR.X', 'Exportar', 1);
.go
insert into MOD values ('MV.RVR.G', 'Gerar', 1);
.go
insert into MOD values ('MV.RVR.A', 'Editar', 1);
.go
insert into MOD values ('MV.AVTE', 'Gerar Arquivo de Vale-Transporte Eletrônico', 0);
.go
insert into MOD values ('MV.VTE', 'Vale-Transporte Eletrônico', 0);
.go
insert into MOD values ('RL', 'Relatórios', 0);
.go
insert into MOD values ('RL.E', 'Enviar', 1);
.go
insert into MOD values ('RL.I', 'Imprimir', 1);
.go
insert into MOD values ('RL.S', 'Salvar', 1);
.go
insert into MOD values ('RL.V', 'Visualizar', 1);
.go
insert into MOD values ('RL.ADH', 'Adicionais Noturnos', 0);
.go
insert into MOD values ('RL.HEX', 'Horas-Extras', 0);
.go
insert into MOD values ('RL.HIN','In-Intinere',0);
.go
insert into MOD values ('RL.SBA','Sobre Aviso',0);
.go
insert into MOD values ('RL.TEP','Tempo de Espera',0);
.go
insert into MOD values ('RL.FAL', 'Faltas e DSR', 0);
.go
insert into MOD values ('RL.FAL.BI', 'Gerar Dados Fortes BI', 0);
.go
insert into MOD values ('RL.ASS', 'Assiduidade', 0);
.go
insert into MOD values ('RL.PON', 'Ponto', 0);
.go
insert into MOD values ('RL.PON.PTO', 'Ponto', 0);
.go
insert into MOD values ('RL.PON.ESP', 'Espelho de Ponto', 0);
.go
insert into MOD values ('RL.PON.EPP', 'Espelho de Ponto - Portaria MTE nº 1.510', 0);
.go
insert into MOD values ('RL.PON.PCD', 'Ponto para Controle Diário', 0);
.go
insert into MOD values ('RL.ATR', 'Atrasos', 0);
.go
insert into MOD values ('RL.ATR.ATS', 'Atrasos e Saidas Antecipadas', 0);
.go
insert into MOD values ('RL.ATR.ATF', 'Atrasos/Saídas Antecipadas e Faltas Fracionadas', 0);
.go
insert into MOD values ('RL.QDR', 'Quebra de Regras', 0);
.go
insert into MOD values ('RL.AII', 'Análise de Intervalos e Interjornadas', 0);
.go
insert into MOD values ('RL.FTR', 'Feriados Trabalhados', 0);
.go
insert into MOD values ('RL.ACH', 'Acordo de Compensação de Horas', 0);
.go
insert into MOD values ('RL.ESL', 'Escalas', 0);
.go
insert into MOD values ('RL.ESL.ESC', 'Trabalho', 0);
.go
insert into MOD values ('RL.ESL.ESF', 'Folga', 0);
.go
insert into MOD values ('RL.EFS', 'Escalas de Fins de Semana', 0);
.go
insert into MOD values ('RL.CHT', 'Comparativo de Horas Trabalhadas', 0);
.go
insert into MOD values ('RL.SUS', 'Suspensões', 0);
.go
insert into MOD values ('RL.CPF', 'Compensações', 0);
.go
insert into MOD values ('RL.CRA', 'Crachás', 0);
.go
insert into MOD values ('RL.PRE', 'Previsão de Horários',0);
.go
insert into MOD values ('RL.QUA', 'Quadro de Horários',0);
.go
insert into MOD values ('RL.EHO', 'Empregados e Horários',0);
.go
insert into MOD values ('RL.DEP', 'Digitais dos Empregados',0);
.go
insert into MOD values ('RL.BHO', 'Banco de Horas', 0);
.go
insert into MOD values ('RL.BHO.EBH', 'Extrato do Banco de Horas', 0);
.go
insert into MOD values ('RL.BHO.RBH', 'Resumo do Banco de Horas', 0);
.go
Insert Into MOD Values ('RL.BHO.SBH', 'Saldo Banco de Horas em Período',0);
.go
Insert Into MOD Values ('RL.BHO.SDBH', 'Saldos Diários do Banco de Horas',0);
.go
insert into MOD values ('RL.BHO.CSB', 'Saldos Convertidos', 0);
.go
insert Into MOD Values ('RL.SER', 'Empregados Pendentes de Reprocessamento',0);
.go
insert into MOD values ('RL.MAF', 'Afastamentos', 0);
.go
insert into MOD values ('RL.OCO', 'Ocorrências', 0);
.go
insert into MOD values ('RL.OCO.OCE', 'Ocorrência', 0);
.go
Insert Into MOD Values ('RL.OCO.OAP', 'Apuração do  Ponto',0);
.go
insert into MOD values ('RL.HOR', 'Horários', 0);
.go
insert into MOD values ('RL.RFT', 'Refeitórios', 0);
.go
insert into MOD values ('RL.AAB', 'Análise de Absenteísmo', 0);
.go
insert into MOD values ('RL.VAL', 'Vales', 0);
.go
insert into MOD values ('RL.VAL.VTR', 'Transporte', 0);
.go
insert into MOD values ('RL.VAL.VTR.PED', 'Pedido', 0);
.go
insert into MOD values ('RL.VAL.VTR.REC', 'Recibo de Entrega', 0);
.go
insert into MOD values ('RL.VAL.VTR.TER', 'Termo de Compromisso', 0);
.go
insert into MOD values ('RL.VAL.VRF', 'Refeição', 0);
.go
insert into MOD values ('RL.VAL.VRF.REC', 'Recibo de Entrega', 0);
.go
insert into MOD values ('RL.PTH', 'Previsão de Trabalho por Horários', 0);
.go
insert into MOD values ('RL.FSE', 'Ficha de Serviço Externo', 0);
.go
insert into MOD values ('RL.CFR', 'Controle de Frequência', 0);
.go
insert into MOD values ('RL.ATE', 'Atestados', 0);
.go
insert into MOD values ('RL.DSR', 'DSR', 0);
.go
insert into MOD values ('RL.BTG','Relatório de Batidas Pendentes',0);
.go
insert into MOD values ('UT', 'Utilitários', 0);
.go
insert into MOD values ('UT.ASE', 'Alterar Senha', 0);
.go
insert into MOD values ('UT.OUT', 'Outra Empresa', 0);
.go
insert into MOD values ('UT.ELO', 'Efetuar logoff', 0);
.go
insert into MOD values ('UT.CFG', 'Configuração', 0);
.go
insert into MOD values ('UT.AUD', 'Auditoria', 0);
.go
insert into MOD values ('UT.EMP', 'Empresa', 0);
.go
insert into MOD values ('UT.EMP.COP', 'Cópia', 0);
.go
insert into MOD values ('UT.EMP.RES', 'Restauração', 0);
.go
insert into MOD values ('UT.CGE', 'Cópia de Segurança Geral', 0);
.go
insert into MOD values ('UT.EMP.SIN', 'Sincronizar Cadastros', 0);
.go
insert into MOD values ('UT.SAP', 'Sincronizar com o Fortes Pessoal', 0);
.go
insert into MOD Values ('UT.ESO', 'eSocial', 0);
.go
insert into MOD values ('UT.ESO.SHP', 'Sincronizar Jornadas de Trabalho com o Fortes Pessoal', 0);
.go
insert into MOD Values ('UT.ESO.MEP', 'Mapear Empregados com o Fortes Pessoal', 0);
.go
insert into MOD values ('UT.IHX', 'Exportar para IHX Access Pro', 0);
.go
insert into MOD values ('UT.DRE', 'Diretório de Recursos', 0);
.go
insert into MOD values ('UT.CIE', 'Corrigir Inconsistências de Escalas', 0);
.go
insert into MOD values ('UT.ACE', 'Alterar Código do Empregado', 0);
.go
insert into MOD values ('UT.ACT', 'Alterar Código Obra/Tomador', 0);
.go
insert into MOD values('UT.ASB', 'Alterar Sensibilidade Biométrica', 0);
.go
insert into MOD values ('UT.ATS', 'Atualizar o Sistema', 0);
.go
insert into MOD values ('UT.EFO', 'Enviar arquivo para Fortes...', 0);
.go
insert into MOD values ('UT.ICD', 'Importar CID...', 0);
.go
insert into MOD values ('UT.CEP', 'Baixar CEP', 0);
.go
insert into MOD values ('UT.PBH', 'Processamento do Banco de Horas', 0);
.go
insert into MOD values ('UT.IBH', 'Invalidação do Banco de Horas', 0);
.go
insert into MOD values ('UT.LTP', 'Limpeza de Tabelas Temporárias', 0);
.go
insert into MOD values ('UT.UTLTRANS', 'Transferência de Empregados entre Empresas', 0);
.go
insert into MOD values ('UT.GPC', 'Gerar Pacote para o Coletor', 0);
.go
insert into MOD values ('CL', 'Coletor', 0);
.go
insert into MOD values ('CL.BDS', 'Banco de Dados', 0);
.go
insert into MOD values ('CL.CON', 'Configurações do Coletor', 0);
.go
insert into MOD (codigo, nome, operacao) values ('UT.CXM', 'Caixa de Mensagens', 0);
.go
insert into MOD (codigo, nome, operacao) values ('UT.EMS', 'Enviar Mensagem', 0);
.go
insert into MOD (codigo, nome, operacao) values ('UT.VVS', 'Verificar Versão do SGBD', 0);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('CD.MID','Motivos de Inclusão e Desconsideração de Batidas',0);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('CD.MID.A','Editar',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('CD.MID.E','Excluir',1);
.go
insert into MOD(CODIGO,NOME,OPERACAO) values ('CD.MID.I','Incluir',1);
.go
insert into MOD values ('AG', 'Agendador', 0);
.go
INSERT INTO MOD(CODIGO,NOME,OPERACAO) VALUES ('DSB','Dashboard',0);
.go
insert into PER(CODIGO,NOME) values ('01','Admin');
.go
insert into MPF(PER_Codigo, MOD_Codigo) select '01', Codigo from MOD;
.go
insert into PER(CODIGO,NOME) values ('99','Colaborador');
.go
insert into MPF (PER_CODIGO, MOD_CODIGO) values ('99', 'RL');
.go
insert into MPF (PER_CODIGO, MOD_CODIGO) values  ('99', 'RL.BHO.EBH');
.go
insert into MPF (PER_CODIGO, MOD_CODIGO) values ('99', 'RL.PON.ESP');
.go
insert into MPF (PER_CODIGO, MOD_CODIGO) values  ('99', 'RL.BHO.RBH');
.go
insert into MPF (PER_CODIGO, MOD_CODIGO) values ('99', 'MV');
.go
insert into MPF (PER_CODIGO, MOD_CODIGO) values  ('99', 'MV.BTP');
.go
insert into MPF(PER_CODIGO,MOD_CODIGO) values ('99', 'UT');
.go
insert into MPF(PER_CODIGO,MOD_CODIGO) values ('99', 'UT.ASE');
.go
insert into FFX(ID,MES,DIA,NIVEL,NOME,DATAVIGOR) values (1,1,1,1,'Confraternização Universal','2008-01-01 00:00:00');
.go
insert into FFX(ID,MES,DIA,NIVEL,NOME,DATAVIGOR) values (2,9,7,1,'Independência do Brasil','2008-01-01 00:00:00');
.go
insert into FFX(ID,MES,DIA,NIVEL,NOME,DATAVIGOR) values (3,10,12,1,'Nossa Senhora Aparecida','2008-01-01 00:00:00');
.go
insert into FFX(ID,MES,DIA,NIVEL,NOME,DATAVIGOR) values (4,11,2,1,'Finados','2008-01-01 00:00:00');
.go
insert into FFX(ID,MES,DIA,NIVEL,NOME,DATAVIGOR) values (5,11,15,1,'Proclamação da República','2008-01-01 00:00:00');
.go
insert into FFX(ID,MES,DIA,NIVEL,NOME,DATAVIGOR) values (6,12,25,1,'Natal','2008-01-01 00:00:00');
.go
insert into FMV(ID,DATA,NOME,NIVEL,UFD_SIGLA,MUN_CODIGO) values (1,'2008-04-12 00:00:00','Páscoa','1',null,null);
.go
insert into MAF(codigo, descricao, gerafalta, geraatraso) values ('002', 'Suspenso por Má Conduta', 1, 1);
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('AC','Acre','02');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('AL','Alagoas','27');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('AM','Amazonas','13');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('AP','Amapá','16');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('BA','Bahia','29');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('CE','Ceará','23');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('DF','Distrito Federal','53');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('ES','Espírito Santo','32');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('GO','Goiás','52');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('MA','Maranhão','21');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('MG','Minas Gerais','31');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('MS','Mato Grosso do Sul','50');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('MT','Mato Grosso','51');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('PA','Pará','15');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('PB','Paraíba','25');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('PE','Pernambuco','26');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('PI','Piauí','22');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('PR','Paraná','41');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('RJ','Rio de Janeiro','33');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('RN','Rio Grande do Norte','24');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('RO','Rondônia','11');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('RR','Roraima','14');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('RS','Rio Grande do Sul','43');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('SC','Santa Catarina','42');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('SE','Sergipe','28');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('SP','São Paulo','35');
.go
insert into UFD (SIGLA,NOME,CODIGO) values ('TO','Tocantins','17');
.go
insert into CFG (codigo, valor) values ('JALOGOU', 'N');
.go

/* Tabela de Justificativas de Horas-extras */
create table LJH (
    EMP_Codigo    varchar(4)  not null,
    EPG_Codigo    varchar(6)  not null,
    DATA          timestamp   not null,
    MAT_CODIGO    varchar(10) not null,
    OBS           varchar(60),
    Lote          integer,
    constraint PK_LJH     primary key (EMP_Codigo,EPG_Codigo,DATA),
    constraint FK_LJH_EPG foreign key (EMP_Codigo,EPG_Codigo) references EPG(EMP_Codigo,Codigo),
	  constraint FK_LJH_MAT foreign key (MAT_Codigo) references MAT (Codigo),
    constraint FK_LJH_JHL foreign key (EMP_Codigo, Lote) references JHL(EMP_Codigo, Lote))
.go

/* Tabela de cadastro de leiautes de exportação */
create table LER (
    Codigo       varchar(4)  not null,
    Descricao    varchar(60) not null,
    Delimitador  varchar(1),
	Tipo integer default 1 not null,
    constraint PK_LER primary key (Codigo, Tipo)
)
.go

/* Tabela de campos dos leiautes d exportação */
create table CLE (
    Codigo          integer      not null,
    Ordem           integer      not null,
    LER_Codigo      varchar(4)   not null,
    InfoCampo       varchar(30),
    InfoFixa        varchar(20),
    Tamanho         integer,
    FormatoData     varchar(10),
    CompletarCom    varchar(1),
	LER_Tipo        integer default 1 not null,
	Tipo integer default 2 not null,
    constraint PK_CLE     primary key (Codigo, LER_Codigo, LER_Tipo, Tipo),
    constraint FK_CLE_LER foreign key (LER_Codigo, LER_Tipo) references LER(Codigo, Tipo)
)
.go

create table MAP (
    emp_codigo varchar(4) not null,
    pev_codigo varchar(3) not null,
    maf_codigo varchar(3) not null,
    exporta integer default 0 not null,
    excecao_vr integer default 0 not null,
    qtd_dias_vr integer default 0 not null,
    todos_dias_vr integer default 0 not null,
    constraint PK_MAP primary key (EMP_CODIGO, PEV_CODIGO, MAF_CODIGO),
    constraint FK_PEV_MAP foreign key (EMP_CODIGO, PEV_CODIGO) references PEV (EMP_CODIGO, CODIGO),
    constraint FK_MAF_MAP foreign key (MAF_CODIGO) references MAF (CODIGO)
)
.go

/* ECO */
/* Pesquisa enviada pela Fortes */
CREATE TABLE ECO (
    ECOXML      BLOB SUB_TYPE 1 SEGMENT SIZE 4096,
    ATUALIZADO  TIMESTAMP
)
.go

/* Usuários que já responderam a pesquisa */
CREATE TABLE EUP (
    USU_CODIGO  VARCHAR(20) NOT NULL,
    QTS_ID      INTEGER NOT NULL,
    constraint pk_EUP     primary key (USU_CODIGO, QTS_ID),
    constraint fk_EUP_USU foreign key (USU_CODIGO) references USU (CODIGO)
)
.go

/* Configurações de conexão com IHX*/
CREATE TABLE CIC (
    EMP_CODIGO         VARCHAR(4) NOT NULL,
    IP_SERVIDOR_BD     VARCHAR(15) NOT NULL,
    PORTA_SERVIDOR_BD  INTEGER NOT NULL,
    NOME_BD            VARCHAR(60) NOT NULL,
    USUARIO_BD         VARCHAR(60) NOT NULL,
    SENHA_BD           VARCHAR(255),
    EMPRESA_IHX        INTEGER,
    HORARIO_IHX        INTEGER,
    CONSTRAINT PK_CIC PRIMARY KEY (EMP_CODIGO),
    CONSTRAINT FK_CIC_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO)
)
.go

/* Tabela de Convênios - Vales-Refeição */
create table CVN(
  Codigo varchar(3) not null,
  Nome varchar(30),
  constraint PK_CVN primary key (Codigo));
.go

/* Tabela de Dados Business Inteligence */
create table DBI(
  EMP_Codigo varchar(4)   not null,
  EPG_Codigo varchar(6)   not null,
  EPG_Nome   varchar(70)  default '' not null,
  Data       timestamp    not null,
  Qtd        float        not null,
  Turnos     varchar(8)   not null,
  Abonadas   varchar(8)   not null,
  Observacao varchar(60)  ,
  Origem     varchar(3)   not null);
.go

create table KAFAL_LOTE (
  EMP_CODIGO varchar(4) not null,
  LOTE       int not null,
  constraint PK_KAFAL_LOTE primary key (EMP_CODIGO, LOTE)
);
.go

create table KJFL_LOTE (
  EMP_CODIGO varchar(4) not null,
  LOTE       int not null,
  constraint PK_KJFL_LOTE primary key (EMP_CODIGO, LOTE)
);
.go

create table KJAL_LOTE (
  EMP_CODIGO varchar(4) not null,
  LOTE       int not null,
  constraint PK_KJAL_LOTE primary key (EMP_CODIGO, LOTE)
);
.go

create table KJHL_LOTE (
  EMP_CODIGO varchar(4) not null,
  LOTE       int not null,
  constraint PK_KJHL_LOTE primary key (EMP_CODIGO, LOTE)
);
.go

/* histórico separação conversão de saldos */
create table HISTCONVSALDOBH (
  ID          integer     not null,   
  EMP_Codigo  varchar(4)  not null,
	DATA        timestamp   not null,
	MARCADO     integer     not null,
	USU_Codigo  varchar(20) not null,
  constraint pk_HISTCONVSALDOBH  primary key (ID),
  constraint fk_HISTCONVSALDOBH_EMP foreign key (EMP_Codigo) references EMP(Codigo));
.go

----------------------------------------------
/* 17090000273 - Trazer eSocial para Master */
----------------------------------------------

.message 'Situações de Empregados a Serem Reprocessadas'
create table SER (EMP_Codigo varchar(4) not null,
                  EPG_Codigo varchar(6) not null,
                  SEP_Data timestamp not null,
                  Motivo integer not null,
                  Status integer not null,
                  Valor_Antigo varchar(255) not null,
                  Valor_Novo varchar(255) not null,
                  Data_Hora_Inclusao timestamp not null,
                  Data_Hora_Alteracao timestamp not null,
                  USU_Codigo_Inclusao varchar(20) not null,
                  USU_Codigo_Alteracao varchar(20) not null,
                  constraint PK_SER primary key (EMP_Codigo, EPG_Codigo, SEP_Data),
                  constraint FK_SER_EMP foreign key (EMP_Codigo) references EMP(Codigo) on delete cascade);
                  //constraint FK_SER_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo, Codigo),
                  //constraint FK_SER_SEP foreign key (EMP_Codigo, EPG_Codigo, SEP_Data) references SEP(EMP_Codigo, EPG_Codigo, Data));
.go

.message 'Tabela de Mapeamento de horários entre Ponto e AC Pessoal'
CREATE TABLE MHR(
  EMP_PONTO VARCHAR(4) NOT NULL,
  CODIGOPONTO VARCHAR(6) NOT NULL,
  EMP_PESSOAL VARCHAR(4) NOT NULL,
  CODIGOPESSOAL VARCHAR(6) NOT NULL,
  NOME VARCHAR(60),
  ESCALA INTEGER,
  MAPEADO INTEGER,
  NOVOCODIGOPONTO VARCHAR(6),
  ACAO INTEGER, /* Mapear = 1, Exportar para o AC = 2, Importar do AC = 3 */
  constraint pk_MHR primary key(EMP_PONTO, CODIGOPONTO, EMP_PESSOAL, CODIGOPESSOAL)
)
.go

.message 'Horários do Fortes Pessoal'
create table AC_HOR (EMP_Codigo varchar(4)  not null,
                  Codigo     varchar(6)  not null,
                  Nome       varchar(60) not null,
                  Horista    integer default 0 not null,
                  TipoEscala integer default 0,
                  TrabalhaEmFeriado integer default 0,
                  DiasCiclo  integer ,
                  OBS        blob sub_type 0,
                  constraint pk_AC_HOR primary key(EMP_Codigo, Codigo)
)
.go


.message 'Jornadas do Horário do Fortes Pessoal'
create table AC_JOR (EMP_Codigo varchar(4) not null,
                  ID integer not null,
                  CODIGO varchar(6)  not null,
                  NOME varchar(100) not null,
                  constraint pk_AC_JOR primary key(EMP_Codigo, ID)
)
.go

.message 'Tabela de dias do horário do Fortes Pessoal'
create table AC_DHOR(EMP_Codigo varchar(4) not null,
                  HOR_CODIGO VARCHAR(6) NOT NULL,
                  OrdemDia 	integer not null,
                  JOR_ID     integer,
                  VR         integer default 0 not null,
                  VT         integer default 0 not null,
                  DiaUtil    integer default 0 not null,
                  constraint pk_AC_DHOR primary key(EMP_Codigo, HOR_CODIGO, OrdemDia),
                  constraint fk_AC_DHOR_AC_JOR foreign key (EMP_Codigo, JOR_ID) references AC_JOR (EMP_Codigo, ID),
                  constraint fk_AC_DHOR_AC_HOR foreign key (EMP_Codigo, HOR_CODIGO) references AC_HOR (EMP_Codigo, CODIGO)
)
.go

.message 'Histórico de Horário do eSocial do Fortes Pessoal'
create table AC_SJOR(EMP_Codigo varchar(4) not null,
                  ID integer not null,
                  JOR_ID integer not null,
                  DATA timestamp not null,
                  DATAFINAL timestamp,
                  HORAENTRADA varchar(4) not null,
                  HORASAIDA varchar(4) not null,
                  FLEXIVEL varchar(1),
                  DATAANT timestamp,
                  DATAFINALANT timestamp,
                  Acao integer default 1 not null,
                  Status integer default 1 not null,
                  constraint pk_AC_SJOR primary key(EMP_Codigo, ID),
                  constraint fk_AC_SJOR_AC_JOR foreign key (EMP_Codigo, JOR_ID) references AC_JOR (EMP_Codigo, ID)
)
.go

.message 'Intervalos de Horário do eSocial do Fortes Pessoal'
create table AC_SJOR_INT(EMP_Codigo varchar(4) not null,
                      ID integer not null,
                      SJOR_ID integer not null,
                      ORDEM integer not null,
                      TIPOINTERVALO integer not null,
                      HORAENTRADA varchar(4) not null,
                      HORASAIDA varchar(4) not null,
                      constraint pk_AC_SJOR_INT primary key (EMP_Codigo, ID),
                      constraint fk_AC_SJOR_INT_AC_SJOR foreign key (EMP_codigo, SJOR_ID) references AC_SJOR(Emp_Codigo, ID)
)
.go
----------------------------------------------

create table TEP (
    EMP_Codigo  varchar(4) not null,
    EPG_Codigo  varchar(6) not null,
    Data        timeStamp not null,
    Sequencial  integer	not null,
    HoraInicial timeStamp,
		HoraFinal   timeStamp,
    Quantidade  integer,
		TotalHoras  integer,
    Observacao  varchar(255),
    constraint PK_TEP  primary key (EMP_Codigo, EPG_Codigo, Data, Sequencial),
    constraint FK_TEP_EMP foreign key (EMP_Codigo) references EMP(Codigo),
    constraint FK_TEP_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo, Codigo) on delete cascade)
.go

CREATE TABLE SBH (
    EMP_CODIGO         VARCHAR(4) NOT NULL,
    EPG_CODIGO         VARCHAR(6) NOT NULL,
    LOTE_ID            INTEGER NOT NULL,
    DATA               TIMESTAMP NOT NULL,
    EVE_CODIGO         VARCHAR(3) NOT NULL,
    MINUTOS_CREDITO    INTEGER,
    MINUTOS_DEBITO     INTEGER,
    MINUTOS_PREVISTO   INTEGER,
    MINUTOS_REALIZADO  INTEGER,
    CONSTRAINT PK_SBH PRIMARY KEY (EMP_CODIGO, EPG_CODIGO, LOTE_ID, DATA),
    CONSTRAINT FK_SBH_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP(CODIGO) ON DELETE CASCADE,
    CONSTRAINT FK_SBH_LOTE FOREIGN KEY (EMP_CODIGO, LOTE_ID) REFERENCES LOTE(EMP_CODIGO, ID) ON DELETE CASCADE);
.go

/*batidas de ponto excluídas*/
create table BTP_EXC (
  EMP_Codigo     varchar(4) not null,
  EPG_Codigo     varchar(6) not null,
  DataHora       timestamp  not null,
  constraint pk_BTP_EXC primary key (EMP_Codigo,EPG_Codigo,DataHora),
  constraint fk_BTP_EXC_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo,Codigo)
);
.go

create trigger T_BTP_BeforeDelete for BTP active before delete as
begin
  /*
  Esta trigger insere registros na tabela BTP_EXC, utilizada na integracao com o Fortes Colabore, para garantir que a batida seja excluida da nuvem no caso de
  falha no consumo da URL momento da exclusao e\ou desconsideracao de batidas
  */
  if ((select CFE.VALOR from CFE where CFE.EMP_CODIGO = OLD.EMP_CODIGO and CFE.CODIGO = 'APPCOLABORADOR' )=1) then
  begin
    if (not exists (SELECT 1 FROM BTP_EXC WHERE EMP_CODIGO = OLD.EMP_CODIGO AND EPG_CODIGO = OLD.EPG_CODIGO AND DATAHORA = OLD.DATAHORA)) then
      INSERT INTO BTP_EXC SELECT EMP_CODIGO, EPG_CODIGO, DATAHORA FROM BTP WHERE STATUSENVIOAPP = 1 AND EMP_CODIGO = OLD.EMP_CODIGO AND EPG_CODIGO = OLD.EPG_CODIGO AND DATAHORA = OLD.DATAHORA;
  end
end;
.go

CREATE TABLE RESUMO_EPG_BH (
  EMP_CODIGO  VARCHAR(4) NOT NULL,
  EPG_CODIGO  VARCHAR(6) NOT NULL,
  DATA        TIMESTAMP NOT NULL,
  SALDO_BH    INT NOT NULL,
  CONSTRAINT PK_RESUMO_EPG_BH PRIMARY KEY (EMP_CODIGO, EPG_CODIGO),
  CONSTRAINT FK_RESUMO_EPG_BH_EPG FOREIGN KEY (EMP_CODIGO, EPG_CODIGO) REFERENCES EPG(EMP_CODIGO, CODIGO) ON DELETE CASCADE
);
.go

CREATE TABLE RESUMO_EPG_HE (
  EMP_CODIGO  VARCHAR(4) NOT NULL,
  EPG_CODIGO  VARCHAR(6) NOT NULL,
  DATA        TIMESTAMP NOT NULL,
  TOTAL_HE    INT NOT NULL,
  CONSTRAINT PK_RESUMO_EPG_HE PRIMARY KEY (EMP_CODIGO, EPG_CODIGO),
  CONSTRAINT FK_RESUMO_EPG_HE_EPG FOREIGN KEY (EMP_CODIGO, EPG_CODIGO) REFERENCES EPG(EMP_CODIGO, CODIGO) ON DELETE CASCADE
);
.go

CREATE TABLE COLABORE_COGNITO (
  USUARIO VARCHAR(20),
  SENHA   BLOB sub_type text,
  TOKEN   BLOB sub_type text
);
.go

CREATE TABLE DSB_BH (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    USU_CODIGO VARCHAR(20) NOT NULL,
    ANOMES VARCHAR(10) NOT NULL,
    SALDODEVEDOR INTEGER,
    QTDEPGDEVEDOR INTEGER,
    SALDOCREDOR INTEGER,
    QTDEPGCREDOR INTEGER,
    DATA_INI timestamp NOT NULL,
    DATA_FIN timestamp NOT NULL,
    HORA_PROCESSAMENTO timestamp NOT NULL,
	
	constraint PK_DSB_BH primary key (EMP_CODIGO,USU_CODIGO, ANOMES),
    constraint FK_DSB_BH_EMP foreign key (EMP_CODIGO) REFERENCES emp (CODIGO),
    constraint FK_DSB_BH_USU foreign key (USU_CODIGO) REFERENCES usu (CODIGO)
	);
.go

CREATE TABLE DSB_HE (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    USU_CODIGO VARCHAR(20) NOT NULL,
    ANOMES VARCHAR(10) NOT NULL,
    PERCENTUAL FLOAT NOT NULL,
    QTDE INTEGER,
    DATA_INI timestamp NOT NULL,
    DATA_FIN timestamp NOT NULL,
    HORA_PROCESSAMENTO timestamp NOT NULL,
	
	constraint PK_DSB_HE primary key (EMP_CODIGO,USU_CODIGO, ANOMES, PERCENTUAL),
    constraint FK_DSB_HE_EMP foreign key (EMP_CODIGO) REFERENCES emp (CODIGO),
    constraint FK_DSB_HE_USU foreign key (USU_CODIGO) REFERENCES usu (CODIGO)
	);
.go

CREATE TABLE DSB_FTA (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    USU_CODIGO VARCHAR(20) NOT NULL,
	ANOMES VARCHAR(10) NOT NULL,
    QTD_FALTAS INTEGER,
    DATA_INI timestamp NOT NULL,
    DATA_FIN timestamp NOT NULL,
    HORA_PROCESSAMENTO timestamp NOT NULL,
	
	constraint PK_DSB_FTA primary key (EMP_CODIGO,USU_CODIGO, ANOMES),
    constraint FK_DSB_FTA_EMP foreign key (EMP_CODIGO) REFERENCES emp (CODIGO),
    constraint FK_DSB_FTA_USU foreign key (USU_CODIGO) REFERENCES usu (CODIGO)
	);
.go

CREATE TABLE DSB_ABS_AN (
    EMP_CODIGO VARCHAR(4) NOT NULL,
    USU_CODIGO VARCHAR(20) NOT NULL,
	ANOMES VARCHAR(10) NOT NULL,
	HORASFALTABH FLOAT,
	QTDEFALTASANALITICO FLOAT,
	QTDEAFASTANALITICO FLOAT,
	PERCENTUALQDEANALITICO FLOAT,
	QTDEFALTASHRSANALITICO FLOAT,
	QTDEAFASTHRSANALITICO FLOAT,
	QTDEATRASOANALITICO FLOAT,
	PERCENTUALHRSANALITICO FLOAT,
	QTDETOTAL FLOAT,
	QTDETOTALHORAS FLOAT,
	TIPODADOS INTEGER,
	DATA_INI timestamp NOT NULL,
	DATA_FIN timestamp NOT NULL,
	HORA_PROCESSAMENTO timestamp NOT NULL,
	
	constraint PK_DSB_ABS_AN primary key (EMP_CODIGO,USU_CODIGO, ANOMES),
    constraint FK_DSB_ABS_AN_EMP foreign key (EMP_CODIGO) REFERENCES emp (CODIGO),
    constraint FK_DSB_ABS_AN_USU foreign key (USU_CODIGO) REFERENCES usu (CODIGO)
    );
.go

CREATE TABLE DSB_ADN (
	EMP_CODIGO VARCHAR(4) NOT NULL,
	USU_CODIGO VARCHAR(20) NOT NULL,
	ANOMES VARCHAR(10) NOT NULL,
	HORASTRABALHADAS INTEGER,
	HORASADDNOTURNO INTEGER,
	PERCENTUAL VARCHAR(20),
	DATA_INI timestamp NOT NULL,
	DATA_FIN timestamp NOT NULL,
	HORA_PROCESSAMENTO timestamp NOT NULL,
	
	constraint PK_DSB_ADN primary key (EMP_CODIGO,USU_CODIGO, ANOMES),
    constraint FK_DSB_ADN_EMP foreign key (EMP_CODIGO) REFERENCES emp (CODIGO),
    constraint FK_DSB_ADN_USU foreign key (USU_CODIGO) REFERENCES usu (CODIGO)
);
.go

CREATE TABLE DSB_MAF (
	EMP_CODIGO VARCHAR(4) NOT NULL,
	USU_CODIGO VARCHAR(20) NOT NULL,
	MAF_CODIGO VARCHAR(10) NOT NULL,
	DESC_MAF   VARCHAR (100),
	QTDE INTEGER,
	DATA_INI timestamp NOT NULL,
	DATA_FIN timestamp NOT NULL,
	HORA_PROCESSAMENTO timestamp NOT NULL,
	
	constraint PK_DSB_MAF primary key (EMP_CODIGO,USU_CODIGO, MAF_CODIGO),
    constraint FK_DSB_MAF_EMP foreign key (EMP_CODIGO) REFERENCES emp (CODIGO),
    constraint FK_DSB_MAF_USU foreign key (USU_CODIGO) REFERENCES usu (CODIGO)
);
.go
CREATE TABLE DSB_INF (
	DSB_CODIGO VARCHAR(50) NOT NULL,
	EMP_CODIGO VARCHAR(4) NOT NULL,
	USU_CODIGO VARCHAR(20) NOT NULL,
	PERIODO VARCHAR(40) NOT NULL,
	EST VARCHAR(50) NOT NULL,
	LOT VARCHAR(50) NOT NULL,
	EXP_FOLHA VARCHAR(40),
	ABONA VARCHAR(40),
	CONSIDERA_ATRASO_SA VARCHAR(4),
	ATRASO_SA VARCHAR(40),
	CONSIDERA_FALTAS VARCHAR(4),
	FALTAS VARCHAR(40),
	AFASTAMENTO VARCHAR(40),
	TIPO_DADOS_ABSENTEISMO VARCHAR(50),
	
	constraint PK_DSB_INF primary key (DSB_CODIGO, EMP_CODIGO,USU_CODIGO),
	constraint FK_DSB_INF_EMP FOREIGN KEY (EMP_CODIGO) REFERENCES EMP (CODIGO),
	constraint FK_DSB_INF_USU FOREIGN KEY (USU_CODIGO) REFERENCES USU (CODIGO)
);
.go 

create table KMSGCOLABORE_ID (
  EMP_Codigo varchar(4) not null,
  ID         int not null,
  constraint pk_KMSGCOLABORE_ID primary key (EMP_Codigo, ID));
.go

create table COLABORE_MSG (
  EMP_Codigo varchar(4) not null,
  ID         varchar(6) not null,
  DataEnvio  timestamp,
  Titulo     varchar(255),
  Corpo      BLOB sub_type text,
  Retorno    varchar(255),  
  constraint PK_COLABORE_MSG primary key (EMP_Codigo, ID),
  constraint FK_COLABORE_MSG_EMP foreign key (EMP_Codigo) references EMP(Codigo));
.go

create table COLAB_DET_MSG (
  EMP_Codigo varchar(4) not null,
  ID         varchar(6) not null,
  EPG_CODIGO varchar(6) not null,  
  constraint PK_COLAB_DET_MSG primary key (EMP_Codigo, ID, EPG_CODIGO),
  constraint FK_COLAB_DET_MSG_COLABORE_MSG foreign key (EMP_Codigo, ID) references COLABORE_MSG(EMP_Codigo, ID));
.go

create table HEM (
  EMP_Codigo VARCHAR(4) not null,
  Data timestamp not null,
  AN_Converter boolean default 0 not null,
  AN_Considerar_Intervalos boolean default 0 not null,
  AN_Nunca_Estender boolean default 0 not null,
  AN_Estender integer default 0 not null,
  ATSA_Considerar_Intervalo boolean default 0 not null,
  constraint PK_HEM primary key (EMP_Codigo, Data),
  constraint FK_HEM_EMP foreign key (EMP_Codigo) references EMP(Codigo) on delete cascade
);
.go

create table UPG_EXC
(
  USU_CODIGO varchar(20) not null,
  EMP_CODIGO varchar(4) not null,
  EPG_CODIGO varchar(6) not null,
  constraint PK_UPG_EXC primary key (USU_CODIGO, EMP_CODIGO, EPG_CODIGO));
.go


create trigger T_UPG_BeforeDelete for UPG active before delete as
begin
  /*
  Esta trigger insere registros na tabela UPG_EXC, utilizada na integracao com o Fortes Colabore, para garantir que o colaborador excluido da visisbilidade seja excluido da nuvem
  */
  if ((select CFE.VALOR from CFE where CFE.EMP_CODIGO = OLD.EMP_CODIGO and CFE.CODIGO = 'APPCOLABORADOR' )=1) then
  begin
    if (not exists (SELECT 1 FROM UPG_EXC WHERE USU_CODIGO = OLD.USU_CODIGO AND EMP_CODIGO = OLD.EMP_CODIGO AND EPG_CODIGO = OLD.EPG_CODIGO)) then
      INSERT INTO UPG_EXC SELECT USU_CODIGO, EMP_CODIGO, EPG_CODIGO FROM UPG WHERE STATUSENVIOAPP = 1 AND USU_CODIGO = OLD.USU_CODIGO AND EMP_CODIGO = OLD.EMP_CODIGO AND EPG_CODIGO = OLD.EPG_CODIGO;
  end
end;
.go

create table RCO (
	EMP_CODIGO  VARCHAR(4) not null,
	CODIGO VARCHAR(4) not null,
	NOME VARCHAR(50) not null,
	constraint PK_RCO primary key (EMP_CODIGO, CODIGO),
	constraint FK_RCO_EMP foreign key (EMP_CODIGO) REFERENCES EMP (CODIGO)
);
.go

create table QRC (
	EMP_CODIGO VARCHAR(4) not null,
	RCO_CODIGO VARCHAR(4) not null,
	QUANTIDADE_INICIAL INTEGER not null,
	QUANTIDADE_FINAL   INTEGER not null,
	HEA_CODIGO VARCHAR(2) not null,
	constraint PK_QRC primary key (EMP_CODIGO, RCO_CODIGO, QUANTIDADE_INICIAL, QUANTIDADE_FINAL),
	constraint FK_QRC_EMP foreign key (EMP_CODIGO) REFERENCES EMP (CODIGO),
	constraint FK_QRC_RCO foreign key (EMP_CODIGO, RCO_CODIGO) REFERENCES RCO (EMP_CODIGO, CODIGO),
	constraint FK_QRC_HEA foreign key (EMP_CODIGO, HEA_CODIGO) REFERENCES HEA (EMP_CODIGO, CODIGO)
);
.go

CREATE TABLE LOG_AGENDADOR (
	ID         INT NOT NULL,
	EMP_CODIGO VARCHAR(4) NOT NULL,
	TIPO       INT NOT NULL,
	DATAHORA   TIMESTAMP,
	DESCRICAO  VARCHAR(255),
	DETALHE    BLOB SUB_TYPE 1,
	EXIBIR     INT DEFAULT 0 NOT NULL,
	constraint PK_LOG_AGENDADOR primary key (ID),
	constraint FK_LOG_AGENDADOR_EMP foreign key (EMP_CODIGO) REFERENCES EMP (CODIGO));
.go

/* Autoincremento da LOG_AGENDADOR */
create Sequence GEN_LOG_AGENDADOR_ID;
.go

alter Sequence GEN_LOG_AGENDADOR_ID Restart with 0;
.go

create trigger LOG_AGENDADOR_BI FOR LOG_AGENDADOR
active before insert position 0
as
begin
  if (NEW.ID is null) then
    NEW.ID = GEN_ID(GEN_LOG_AGENDADOR_ID,1);
end;
.go

insert into USU (CODIGO, SENHA, BLOQUEADO) values ('FORTES', '159753', 1);
.go

insert into USU (CODIGO, SENHA, BLOQUEADO) values ('SOS', '159753', 1);
.go

/* Justificativas de faltas excluídas */
create table LJF_EXC (
  EMP_Codigo  varchar(4) not null,
  EPG_Codigo  varchar(6) not null,
  Data        timestamp not null,
  Seq         integer not null,
  constraint PK_LJF_EXC primary key (EMP_Codigo, EPG_Codigo, Data, Seq),
  constraint FK_LJF_EXC_EPG foreign key (EMP_Codigo, EPG_Codigo) references EPG(EMP_Codigo, Codigo)
);
.go

create trigger T_LJF_BeforeDelete for LJF active before delete as
begin
  /*
  Esta trigger insere registros na tabela LJF_EXC, utilizada na integracao com o Fortes Colabore, para garantir que a LJF seja excluida da nuvem
  */
  if ((OLD.StatusEnvioApp = 1) and ((select CFE.Valor from CFE where CFE.EMP_Codigo = OLD.EMP_Codigo and CFE.Codigo = 'APPCOLABORADOR') = 1)) then
  begin
    if (not exists (select 1 from LJF_EXC where EMP_Codigo = OLD.EMP_Codigo and EPG_Codigo = OLD.EPG_Codigo and Data = OLD.Data and Seq = OLD.Seq)) then
      insert into LJF_EXC (EMP_Codigo, EPG_Codigo, Data, Seq) values (OLD.EMP_Codigo, OLD.EPG_Codigo, OLD.Data, OLD.Seq);
  end
end;
.go

create trigger T_LJF_BeforeUpdate for LJF active before update as
begin
  /*
  Esta trigger insere registros na tabela LJF_EXC, utilizada na integracao com o Fortes Colabore, para garantir que a LJF seja excluida da nuvem
  */
  if ((OLD.StatusEnvioApp = 1) and ((select CFE.Valor from CFE where CFE.EMP_Codigo = OLD.EMP_Codigo and CFE.Codigo = 'APPCOLABORADOR') = 1)) then
  begin
    if (not exists (select 1 from LJF_EXC where EMP_Codigo = OLD.EMP_Codigo and EPG_Codigo = OLD.EPG_Codigo and Data = OLD.Data and Seq = OLD.Seq)) then
      insert into LJF_EXC (EMP_Codigo, EPG_Codigo, Data, Seq) values (OLD.EMP_Codigo, OLD.EPG_Codigo, OLD.Data, OLD.Seq);
    NEW.StatusEnvioApp = 0;
  end
end;
.go

CREATE TABLE EPG_ENDER (
    CODIGO                      VARCHAR(4) NOT NULL,
    EPG_CODIGO                  VARCHAR(6) NOT NULL,
    EMP_CODIGO                  VARCHAR(4) NOT NULL,
    DESCRICAO                   VARCHAR(30) NOT NULL,
    ENDLOGRADOURO               VARCHAR(40) NOT NULL,
    ENDNUMERO                   VARCHAR(10) NOT NULL,
    BAIRRO                      VARCHAR(20) NOT NULL,
    CEP                         VARCHAR(8) NOT NULL,
    mun_ufd_sigla               varchar(2) NOT NULL,
    mun_codigo                  varchar(5) NOT NULL,
	LATITUDE                    varchar(25) NOT NULL,
	LONGITUDE                   varchar(25) NOT NULL,
	RAIO                        INTEGER default 100 NOT NULL,
	STATUSENVIOAPP              INTEGER DEFAULT 0 NOT NULL,
    constraint PK_EPG_ENDER primary key (CODIGO, EMP_CODIGO, EPG_CODIGO),
    constraint UK_EPG_ENDER_DESC unique (DESCRICAO, EMP_CODIGO, EPG_CODIGO),
	constraint FK_EPG_ENDER_EPG foreign key (EMP_CODIGO,EPG_CODIGO) REFERENCES EPG (EMP_CODIGO,CODIGO),
    constraint FK_EPG_ENDER_EMP foreign key (EMP_CODIGO) REFERENCES EMP (CODIGO),
    constraint FK_EPG_ENDER_MUN foreign key (MUN_UFD_SIGLA, MUN_CODIGO) REFERENCES MUN (UFD_SIGLA, CODIGO)
);
.go

create table EPG_GESTOR_EXC
(
  EMP_CODIGO varchar(4) not null,
  EPG_CODIGO varchar(6) not null,
  USU_CODIGO varchar(20) not null,
  CPF        Varchar(11) not null,
  constraint PK_EPG_GESTOR_EXC primary key (EMP_CODIGO, EPG_CODIGO, USU_CODIGO));
.go


/* Esta linha sempre deverá ser a última para ser de fácil visualização */
insert into INFO values (252, 'OK', 'PONTO', 0, 0, 0, 1);
.go

.end

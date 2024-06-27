-- Adicionando um novo servidor vinculado
EXEC sp_addlinkedserver 
   @server = 'RemoteServerName',  -- Nome do servidor vinculado
   @srvproduct = '',             -- Pode ser deixado vazio para SQL Server
   @provider = 'SQLNCLI',        -- Provedor OLE DB para SQL Server
   @datasrc = '192.168.100.6,1433', -- Endereço IP público do servidor remoto e a porta
   @catalog = 'DMD';  -- Nome do banco de dados remoto

-- Adicionando informações de segurança
EXEC sp_addlinkedsrvlogin 
   @rmtsrvname = 'RemoteServerName', 
   @useself = 'False', 
   @locallogin = NULL, 
   @rmtuser = 'sainfarma',  -- Usuário do servidor remoto
   @rmtpassword = 'SAInfarma2022@'; -- Senha do servidor remoto


EXEC sp_testlinkedserver @servername = 'RemoteServerName';


SELECT * 
FROM [RemoteServerName].[RemoteDatabaseName].[Schema].[TableName]


INSERT INTO LocalTable (Column1, Column2)
SELECT Column1, Column2 
FROM [RemoteServerName].[RemoteDatabaseName].[dbo].[RemoteTable]


SELECT local.Column1, remote.Column2
FROM LocalTable local
JOIN [RemoteServerName].[RemoteDatabaseName].[dbo].[RemoteTable] remote
ON local.ID = remote.ID

USE [DMD];
GO

/* =========================================
   1) CONFIGURAÇÕES
   ========================================= */
DECLARE @Login SYSNAME = 'katigua';
DECLARE @Senha NVARCHAR(128) = 'katigua20264#';
DECLARE @User  SYSNAME = 'katigua';

/* =========================================
   2) CRIAR LOGIN NO SERVIDOR
   ========================================= */
IF NOT EXISTS (
    SELECT 1
    FROM sys.server_principals
    WHERE name = @Login
)
BEGIN
    DECLARE @SqlLogin NVARCHAR(MAX);
    SET @SqlLogin = '
        CREATE LOGIN [' + @Login + ']
        WITH PASSWORD = ''' + @Senha + ''',
             CHECK_POLICY = ON,
             CHECK_EXPIRATION = ON;
    ';
    EXEC(@SqlLogin);
END
GO

/* =========================================
   3) CRIAR USUÁRIO NO BANCO
   ========================================= */
IF NOT EXISTS (
    SELECT 1
    FROM sys.database_principals
    WHERE name = 'katigua'
)
BEGIN
    CREATE USER [katigua] FOR LOGIN [katigua];
END
GO

/* =========================================
   4) REMOVER ACESSOS AMPLOS, SE EXISTIREM
   ========================================= */
BEGIN TRY
    ALTER ROLE db_owner DROP MEMBER [katigua];
END TRY
BEGIN CATCH
END CATCH;
GO

BEGIN TRY
    ALTER ROLE db_datareader DROP MEMBER [katigua];
END TRY
BEGIN CATCH
END CATCH;
GO

BEGIN TRY
    ALTER ROLE db_datawriter DROP MEMBER [katigua];
END TRY
BEGIN CATCH
END CATCH;
GO

/* =========================================
   5) LIBERAR APENAS AS VIEWS NECESSÁRIAS
   ========================================= */
GRANT SELECT ON OBJECT::dbo.VW_KATIGUA_ESTOQUE TO [katigua];
GRANT SELECT ON OBJECT::dbo.VW_KATIGUA_VENDAS TO [katigua];
GRANT SELECT ON OBJECT::dbo.VW_KATIGUA_METAS TO [katigua];
GRANT SELECT ON OBJECT::dbo.VW_KATIGUA_VENDEDORES TO [katigua];
GO

/* =========================================
   6) OPCIONAL: PERMITIR VER DEFINIÇÃO
   evita alguns erros/chatices no SSMS
   ========================================= 
GRANT VIEW DEFINITION ON OBJECT::dbo.VW_MAXNUTRI_NOTAS TO [UsuarioTerceiro];
GRANT VIEW DEFINITION ON OBJECT::dbo.VW_MAXNUTRI_PRODUTOS TO [UsuarioTerceiro];
GRANT VIEW DEFINITION ON OBJECT::dbo.VW_MAXNUTRI_CLIENTES TO [UsuarioTerceiro];
GO
*/
/* =========================================
   7) TESTE
   ========================================= */
EXECUTE AS USER = 'katigua';
SELECT TOP 1 * FROM dbo.VW_katigua_VENDEDORES;
REVERT;
GO
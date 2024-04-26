---------------------------------------------------------------
-- Definicoes para funcao
---------------------------------------------------------------
--Alphabetic only:
--SELECT dbo.fn_StripCharacters('a1!s2@d3#f4$', '^a-z')

--Numeric only:
--SELECT dbo.fn_StripCharacters('a1!s2@d3#f4$', '^0-9')

--Alphanumeric only:
--SELECT dbo.fn_StripCharacters('a1!s2@d3#f4$', '^a-z0-9')

--Non-alphanumeric:
--SELECT dbo.fn_StripCharacters('a1!s2@d3#f4$', 'a-z0-9')
---------------------------------------------------------------

IF EXISTS (SELECT *
           FROM   sys.objects
           WHERE  object_id = OBJECT_ID(N'[dbo].[FN_StripCharacters]')
           AND type IN ( N'FN', N'IF', N'TF', N'FS', N'FT' ))
  DROP FUNCTION [dbo].[FN_StripCharacters]

GO 

CREATE FUNCTION [dbo].[FN_StripCharacters]
(
    @String NVARCHAR(MAX), 
    @MatchExpression VARCHAR(255)
)
RETURNS NVARCHAR(MAX)
AS
BEGIN
    SET @MatchExpression =  '%['+@MatchExpression+']%'

    WHILE PatIndex(@MatchExpression, @String) > 0
        SET @String = Stuff(@String, PatIndex(@MatchExpression, @String), 1, '')

    RETURN @String

END
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
SET ANSI_WARNINGS OFF
GO
SET NOCOUNT ON 
GO

Select Cod_EAN
    From  PREAN
    Where Cod_EAN is not null
	And (dbo.fn_StripCharacters(Cod_EAN, '^a-z')<>''
	Or dbo.fn_StripCharacters(Cod_EAN, 'a-z0-9')<>'') 

Select Cod_EAN
    From  PRODU
    Where Cod_EAN is not null
	And (dbo.fn_StripCharacters(Cod_EAN, '^a-z')<>''
	Or dbo.fn_StripCharacters(Cod_EAN, 'a-z0-9')<>'') 

BEGIN TRANSACTION
if Exists(Select Top 1 1
          From  PREAN
          Where Cod_EAN is not null
		  And (dbo.fn_StripCharacters(Cod_EAN, '^a-z')<>''
		  Or dbo.fn_StripCharacters(Cod_EAN, 'a-z0-9')<>'') )
	Update PREAN set Cod_EAN = dbo.fn_StripCharacters(Cod_EAN, '^0-9')
    Where Cod_EAN is not null
	And (dbo.fn_StripCharacters(Cod_EAN, '^a-z')<>''
	Or dbo.fn_StripCharacters(Cod_EAN, 'a-z0-9')<>'') 

if Exists(Select Top 1 1
          From  PRODU
          Where Cod_EAN is not null
		  And (dbo.fn_StripCharacters(Cod_EAN, '^a-z')<>''
		  Or dbo.fn_StripCharacters(Cod_EAN, 'a-z0-9')<>'') )
	Update PRODU set Cod_EAN = dbo.fn_StripCharacters(Cod_EAN, '^0-9')
    Where Cod_EAN is not null
	And (dbo.fn_StripCharacters(Cod_EAN, '^a-z')<>''
	Or dbo.fn_StripCharacters(Cod_EAN, 'a-z0-9')<>'') 
COMMIT
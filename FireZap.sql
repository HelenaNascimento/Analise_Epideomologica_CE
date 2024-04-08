DECLARE @Fl_Ole_Automation_Ativado BIT = (SELECT (CASE WHEN CAST([value] AS VARCHAR(MAX)) = '1' THEN 1 ELSE 0 END) FROM sys.configurations WHERE [name] = 'Ole Automation Procedures')
IF (@Fl_Ole_Automation_Ativado = 0)
BEGIN
    EXECUTE sp_configure 'show advanced options', 1;
    RECONFIGURE WITH OVERRIDE;
    EXEC sp_configure 'Ole Automation Procedures', 1;
    RECONFIGURE WITH OVERRIDE;
END
 
DECLARE
    @obj INT,
    @Url VARCHAR(255) = 'https://api.firezap.pro/api/messages/send',
    @Autorizacao VARCHAR(100)= 'Bearer LhaNJ7yv3qcqjyTJnCvS3cqQXO6sxa',
    @resposta VARCHAR(8000),
    @Body NVARCHAR(MAX),
    @Destinatario VARCHAR(30) = '5581564526',
	@ResponseText AS VARCHAR(8000),
	@StatusCode INT
 
--SET @Payload = '{"number": "' + @Destinatario + '", "body": "TESTE do ZAP do SQL", "closeTicket": "false", "sendSignature": "true"}'
 
SET @Body = '{
  "number": "558581564526",
  "body": "TESTE do ZAP do SQL",
  "closeTicket": "false",
  "sendSignature": "true"
}';
 
EXEC sys.sp_OACreate 'MSXML2.ServerXMLHTTP', @obj OUT
EXEC sys.sp_OAMethod @obj, 'open', NULL, 'POST', @Url, 'FALSE'
EXEC sys.sp_OAMethod @obj, 'setRequestHeader', NULL, 'Content-Type', 'application/json'
EXEC sys.sp_OAMethod @obj, 'setRequestHeader', NULL, 'Authorization', 'Bearer 1mzT3j7KIvaahFhgbCSpwWfSVsGO84'
 
EXEC sys.sp_OAMethod @obj, 'send', NULL, @Body
 
-- Obtém o status da resposta
EXEC sp_OAGetProperty @obj, 'status', @StatusCode OUT;
-- Obtém a resposta
EXEC sp_OAGetProperty @obj, 'responseText', @ResponseText OUT;
 
-- Limpa o objeto OLE
EXEC sp_OADestroy @obj;
 
-- Exibe o status e a resposta (opcional)
SELECT @StatusCode AS StatusCode, @ResponseText AS ResponseText;
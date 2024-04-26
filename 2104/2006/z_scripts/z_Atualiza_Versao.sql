Update PARAM
   Set Des_Versao = '20.06'
 Where Des_Versao <> '20.06'

Update PARAM
   Set Des_VersaoMin = '20.06'
 Where (Des_VersaoMin is null) or (Des_VersaoMin <> '20.06')
  
GO

Update PARAM
   Set Des_Versao = '20.11'
 Where Des_Versao <> '20.11'

Update PARAM
   Set Des_VersaoMin = '20.11'
 Where IsNull(Des_VersaoMin,'') <> '20.11'
  
GO

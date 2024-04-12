Update PARAM
   Set Des_Versao = '21.04'
 Where Des_Versao <> '21.04'

Update PARAM
   Set Des_VersaoMin = '21.04'
 Where (Des_VersaoMin is null) or (Des_VersaoMin <> '21.04')
  
GO

Declare @PCodEstabe int  =  0,     -- Codigo do Estabelecimento
        @PCodFornec int  =  1819,    -- Codigo do Fornecedor
        @PNewRegTri int  =  17,    -- Nova Regiãotributaria para calculo de tributos
        @PDatRef datetime

Exec QG_TributosRetidosFornecedor @PCodEstabe, @PCodFornec, @PNewRegTri, 0

GO



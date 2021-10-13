object DMDContabilizacion: TDMDContabilizacion
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 510
  Width = 638
  object QFacturas: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 80
    Top = 192
  end
  object QNumFacturas: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 80
    Top = 136
  end
  object QSalidas: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 188
    Top = 208
  end
  object QGastos: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 188
    Top = 336
  end
  object Database: TDatabase
    AliasName = 'comercializacion'
    DatabaseName = 'BDContabilizarFacturas'
    LoginPrompt = False
    SessionName = 'Default'
    Left = 40
    Top = 32
  end
  object QImporteFacturas: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 80
    Top = 248
  end
  object QDescuentoCliente: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 188
    Top = 272
  end
  object QAux: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 184
    Top = 32
  end
  object QTotalesSalidas: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 260
    Top = 208
  end
  object qryProductoSal: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 348
    Top = 208
  end
end

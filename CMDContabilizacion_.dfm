object DMDContabilizacion: TDMDContabilizacion
  OldCreateOrder = False
  OnDestroy = DataModuleDestroy
  Left = 343
  Top = 235
  Height = 508
  Width = 734
  object QDocs: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 136
    Top = 24
  end
  object QNumDocs: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 64
    Top = 24
  end
  object QTercero: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 64
    Top = 80
  end
  object QCambio: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 64
    Top = 136
  end
  object QSalidas: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 64
    Top = 192
  end
  object QSeccionesFactura: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 152
    Top = 192
  end
  object QGastos: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 240
    Top = 192
  end
  object mtContabilizadas: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 440
    Top = 24
  end
  object mtNoContabilizadas: TkbmMemTable
    DesignActivation = True
    AttachedAutoRefresh = True
    AttachMaxCount = 1
    FieldDefs = <>
    IndexDefs = <>
    SortOptions = []
    AllDataOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveBlobs, mtfSaveFiltered, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail, mtfSaveDeltas]
    CommaTextOptions = [mtfSaveData]
    CSVQuote = '"'
    CSVFieldDelimiter = ','
    CSVRecordDelimiter = ','
    CSVTrueString = 'True'
    CSVFalseString = 'False'
    PersistentSaveOptions = [mtfSaveData, mtfSaveNonVisible, mtfSaveIgnoreRange, mtfSaveIgnoreMasterDetail]
    PersistentBackup = False
    ProgressFlags = [mtpcLoad, mtpcSave, mtpcCopy]
    FilterOptions = []
    Version = '2.53g'
    LanguageID = 0
    SortID = 0
    SubLanguageID = 1
    LocaleID = 1024
    Left = 440
    Top = 80
  end
  object QMarcarFactura: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 64
    Top = 256
  end
  object QDirectorioConta: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 408
    Top = 144
  end
  object QFormaPago: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 152
    Top = 256
  end
  object QFacturaAsociada: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 232
    Top = 256
  end
  object facturas_conta_t: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 64
    Top = 320
  end
  object facturas_conta_d: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 168
    Top = 320
  end
  object facturas_conta_e: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 360
    Top = 320
  end
  object facturas_conta_a: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 264
    Top = 320
  end
  object aux_facturas_conta_t1: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 96
    Top = 392
  end
  object DSAUX_facturas_conta: TDataSource
    DataSet = aux_facturas_conta_t1
    Left = 184
    Top = 416
  end
  object aux_facturas_conta_d: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    DataSource = DSAUX_facturas_conta
    Left = 272
    Top = 392
  end
  object aux_facturas_conta_a: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    DataSource = DSAUX_facturas_conta_d
    Left = 480
    Top = 440
  end
  object aux_facturas_conta_e: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    DataSource = DSAUX_facturas_conta
    Left = 272
    Top = 456
  end
  object aux_facturas_conta_t2: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 96
    Top = 456
  end
  object DSAUX_facturas_conta_d: TDataSource
    DataSet = aux_facturas_conta_d
    Left = 384
    Top = 416
  end
  object QLineasAbono: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 344
    Top = 224
  end
  object QEmpresaProcedencia: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 424
    Top = 224
  end
  object QSeccionContable: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 512
    Top = 224
  end
  object QDirSum: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 136
    Top = 80
  end
  object QTipoIva: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 192
    Top = 80
  end
  object Database: TDatabase
    DatabaseName = 'BDContabilizarFacturas'
    DriverName = 'INFORMIX'
    LoginPrompt = False
    Params.Strings = (
      'SERVER NAME=server1_tcp'
      'DATABASE NAME=comer'
      'USER NAME=info'
      'OPEN MODE=READ/WRITE'
      'SCHEMA CACHE SIZE=8'
      'LANGDRIVER=DB850ES0'
      'SQLQRYMODE=SERVER'
      'SQLPASSTHRU MODE=SHARED AUTOCOMMIT'
      'LOCK MODE=5'
      'DATE MODE=1'
      'DATE SEPARATOR=/'
      'SCHEMA CACHE TIME=-1'
      'MAX ROWS=-1'
      'BATCH COUNT=200'
      'ENABLE SCHEMA CACHE=FALSE'
      'SCHEMA CACHE DIR='
      'ENABLE BCD=FALSE'
      'LIST SYNONYMS=NONE'
      'DBNLS='
      'COLLCHAR='
      'BLOBS TO CACHE=64'
      'BLOB SIZE=32'
      'PASSWORD=unix1q2w')
    SessionName = 'Default'
    Left = 288
    Top = 16
  end
  object QEmailAdmi: TQuery
    DatabaseName = 'BDContabilizarFacturas'
    Left = 488
    Top = 144
  end
end

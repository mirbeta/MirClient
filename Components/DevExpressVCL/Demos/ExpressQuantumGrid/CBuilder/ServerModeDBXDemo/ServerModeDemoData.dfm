object ServerModeDemoDataDM: TServerModeDemoDataDM
  OldCreateOrder = True
  Height = 261
  Width = 271
  object SQLConnection: TSQLConnection
    Left = 32
    Top = 16
  end
  object SQLQuery: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SQLConnection
    Left = 128
    Top = 16
  end
  object ServerModeDataSource: TdxServerModeDBXDataSource
    Options.SchemaName = 'dbo'
    SQLAdapterClassName = 'TdxServerModeMSSQLAdapter'
    OnFatalError = ServerModeDataSourceFatalError
    OnInconsistentCache = ServerModeDataSourceInconsistentCache
    Connection = SQLConnection
    Left = 80
    Top = 80
  end
end

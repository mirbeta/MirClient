object ServerModeDemoDataDM: TServerModeDemoDataDM
  OldCreateOrder = False
  Height = 261
  Width = 200
  object ADOConnection: TADOConnection
    Left = 32
    Top = 16
  end
  object ADOQuery: TADOQuery
    Connection = ADOConnection
    Parameters = <>
    Left = 128
    Top = 16
  end
  object ServerModeDataSource: TdxServerModeADODataSource
    SQLAdapterClassName = 'TdxServerModeMSSQLAdapter'
    OnFatalError = ServerModeDataSourceFatalError
    OnInconsistentCache = ServerModeDataSourceInconsistentCache
    Connection = ADOConnection
    Left = 80
    Top = 80
  end
end

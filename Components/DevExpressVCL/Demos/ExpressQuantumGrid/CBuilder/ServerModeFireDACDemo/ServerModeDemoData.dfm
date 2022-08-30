object ServerModeDemoDataDM: TServerModeDemoDataDM
  OldCreateOrder = True
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 261
  Width = 271
  object ServerModeDataSource: TdxServerModeFireDACDataSource
    SQLAdapterClassName = 'TdxServerModeMSSQLAdapter'
    OnFatalError = ServerModeDataSourceFatalError
    OnInconsistentCache = ServerModeDataSourceInconsistentCache
    Left = 112
    Top = 104
  end
end

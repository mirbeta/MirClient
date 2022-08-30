object ServerModeDemoDataDM: TServerModeDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 267
  Width = 256
  object ServerModeQueryDataSource: TdxServerModeFireDACQueryDataSource
    KeyFieldNames = 'OID'
    SQLAdapterClassName = 'TdxServerModeMSSQLAdapter'
    OnFatalError = ServerModeQueryDataSourceFatalError
    OnInconsistentCache = ServerModeQueryDataSourceInconsistentCache
    SQL.Strings = (
      'SELECT'
      '"ServerModeGridOrdersDemo"."OID"'
      ',"CustomerID"'
      ',"OrderDate"'
      ',"Trademark"'
      ',"Model"'
      ',"HP"'
      ',"Cyl"'
      ',"TransmissSpeedCount"'
      ',"TransmissAutomatic"'
      ',"Category"'
      ',"Price"'
      ''
      ',"FirstName"'
      ',"LastName"'
      ',"Company"'
      ',"Prefix"'
      ',"Title"'
      ',"Address"'
      ',"City"'
      ',"State"'
      ',"ZipCode"'
      ',"Source"'
      ',"Customer"'
      ',"HomePhone"'
      ',"FaxPhone"'
      ',"Description"'
      ',"Email"'
      'FROM'
      '"ServerModeGridOrdersDemo"'
      
        'left join "ServerModeGridCustomersDemo" on("ServerModeGridOrders' +
        'Demo"."CustomerID" = "ServerModeGridCustomersDemo"."OID")')
    Left = 112
    Top = 112
  end
end

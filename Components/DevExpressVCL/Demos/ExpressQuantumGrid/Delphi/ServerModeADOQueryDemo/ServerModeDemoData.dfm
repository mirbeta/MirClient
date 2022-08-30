object ServerModeDemoDataDM: TServerModeDemoDataDM
  OldCreateOrder = False
  Height = 267
  Width = 227
  object ADOConnection: TADOConnection
    LoginPrompt = False
    Left = 32
    Top = 16
  end
  object ADOQuery: TADOQuery
    Connection = ADOConnection
    Parameters = <>
    Left = 128
    Top = 16
  end
  object ServerModeQueryDataSource: TdxServerModeADOQueryDataSource
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
    Connection = ADOConnection
    Left = 96
    Top = 88
  end
end

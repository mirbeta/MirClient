object dmDemo: TdmDemo
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 2
  Top = 114
  Height = 257
  Width = 272
  object dsOrders: TDataSource
    DataSet = mdLayoutControl
    Left = 120
    Top = 64
  end
  object mdLayoutControl: TdxMemData
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F26000000080000000B000D00507572636861736544
      61746500080000000B000C004F72646572735F54696D65000700000001000C00
      5061796D656E7454797065000800000006000E005061796D656E74416D6F756E
      740004000000030009005175616E74697479001900000001000A004669727374
      4E616D650019000000010009004C6173744E616D65003200000001000800436F
      6D70616E79000F00000001000700507265666978000F00000001000600546974
      6C65003200000001000800416464726573730014000000010005004369747900
      02000000010006005374617465000A000000010008005A6970436F6465000A00
      000001000700536F75726365000100000001000900437573746F6D6572000F00
      000001000A00486F6D6550686F6E65000F0000000100090046617850686F6E65
      00320000000100070053706F757365001900000001000B004F63637570617469
      6F6E00FF00000001000600456D61696C003200000001000A0054726164656D61
      726B0032000000010006004D6F64656C00020000000200030048500008000000
      060006004C6974657200020000000200040043796C0002000000020014005472
      616E736D6973735370656564436F756E740003000000010013005472616E736D
      6973734175746F6D617469630002000000020009004D50475F43697479000200
      000002000C004D50475F4869676877617900070000000100090043617465676F
      727900000000000E001100436172735F4465736372697074696F6E0032000000
      01000A0048797065726C696E6B00000000000D00080050696374757265000800
      0000060006005072696365000400000003000D00437573746F6D6572735F4944
      000400000003000A004F72646572735F4944000400000003000800436172735F
      494400}
    SortOptions = []
    Left = 128
    Top = 64
    object mdLayoutControlPurchaseDate: TDateTimeField
      FieldName = 'PurchaseDate'
    end
    object mdLayoutControlOrders_Time: TDateTimeField
      DisplayLabel = 'Time'
      FieldName = 'Orders_Time'
    end
    object mdLayoutControlPaymentType: TStringField
      FieldName = 'PaymentType'
      Size = 7
    end
    object mdLayoutControlPaymentAmount: TFloatField
      FieldName = 'PaymentAmount'
    end
    object mdLayoutControlQuantity: TIntegerField
      FieldName = 'Quantity'
    end
    object mdLayoutControlFirstName: TStringField
      FieldName = 'FirstName'
      Size = 25
    end
    object mdLayoutControlLastName: TStringField
      FieldName = 'LastName'
      Size = 25
    end
    object mdLayoutControlCompany: TStringField
      FieldName = 'Company'
      Size = 50
    end
    object mdLayoutControlPrefix: TStringField
      FieldName = 'Prefix'
      Size = 15
    end
    object mdLayoutControlTitle: TStringField
      FieldName = 'Title'
      Size = 15
    end
    object mdLayoutControlAddress: TStringField
      FieldName = 'Address'
      Size = 50
    end
    object mdLayoutControlCity: TStringField
      FieldName = 'City'
    end
    object mdLayoutControlState: TStringField
      FieldName = 'State'
      Size = 2
    end
    object mdLayoutControlZipCode: TStringField
      FieldName = 'ZipCode'
      Size = 10
    end
    object mdLayoutControlSource: TStringField
      FieldName = 'Source'
      Size = 10
    end
    object mdLayoutControlCustomer: TStringField
      FieldName = 'Customer'
      Size = 1
    end
    object mdLayoutControlHomePhone: TStringField
      FieldName = 'HomePhone'
      Size = 15
    end
    object mdLayoutControlFaxPhone: TStringField
      FieldName = 'FaxPhone'
      Size = 15
    end
    object mdLayoutControlSpouse: TStringField
      FieldName = 'Spouse'
      Size = 50
    end
    object mdLayoutControlOccupation: TStringField
      FieldName = 'Occupation'
      Size = 25
    end
    object mdLayoutControlEmail: TStringField
      FieldName = 'Email'
      Size = 255
    end
    object mdLayoutControlTrademark: TStringField
      FieldName = 'Trademark'
      Size = 50
    end
    object mdLayoutControlModel: TStringField
      FieldName = 'Model'
      Size = 50
    end
    object mdLayoutControlHP: TSmallintField
      FieldName = 'HP'
    end
    object mdLayoutControlLiter: TFloatField
      FieldName = 'Liter'
    end
    object mdLayoutControlCyl: TSmallintField
      FieldName = 'Cyl'
    end
    object mdLayoutControlTransmissSpeedCount: TSmallintField
      FieldName = 'TransmissSpeedCount'
    end
    object mdLayoutControlTransmissAutomatic: TStringField
      FieldName = 'TransmissAutomatic'
      Size = 3
    end
    object mdLayoutControlMPG_City: TSmallintField
      FieldName = 'MPG_City'
    end
    object mdLayoutControlMPG_Highway: TSmallintField
      FieldName = 'MPG_Highway'
    end
    object mdLayoutControlCategory: TStringField
      FieldName = 'Category'
      Size = 7
    end
    object mdLayoutControlCars_Description: TMemoField
      FieldName = 'Cars_Description'
      BlobType = ftMemo
      Size = 10
    end
    object mdLayoutControlHyperlink: TStringField
      FieldName = 'Hyperlink'
      Size = 50
    end
    object mdLayoutControlPicture: TBlobField
      FieldName = 'Picture'
      Size = 10
    end
    object mdLayoutControlPrice: TFloatField
      FieldName = 'Price'
    end
    object mdLayoutControlCustomers_ID: TIntegerField
      FieldName = 'Customers_ID'
    end
    object mdLayoutControlOrders_ID: TIntegerField
      FieldName = 'Orders_ID'
    end
    object mdLayoutControlCars_ID: TIntegerField
      FieldName = 'Cars_ID'
    end
  end
end

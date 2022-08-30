object RowsMultiEditorsDemoDataDM: TRowsMultiEditorsDemoDataDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 644
  Top = 321
  Height = 221
  Width = 286
  object dsOrders: TDataSource
    DataSet = mdOrders
    Left = 120
    Top = 64
  end
  object StyleRepository: TcxStyleRepository
    Left = 120
    Top = 8
    PixelsPerInch = 96
    object Sunny: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clNavy
    end
    object Dark: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14920832
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object Golden: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object Summer: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16379615
      TextColor = clBlack
    end
    object Autumn: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15252642
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = 11032875
    end
    object Bright: TcxStyle
      AssignedValues = [svColor]
      Color = 16749885
    end
    object Cold: TcxStyle
      AssignedValues = [svColor]
      Color = 14872561
    end
    object Spring: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15519398
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = 12742441
    end
    object Light: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object Winter: TcxStyle
      AssignedValues = [svColor, svFont]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
    end
    object Depth: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object UserStyleSheet: TcxVerticalGridStyleSheet
      Styles.Background = Autumn
      Styles.Content = Summer
      Styles.Inactive = Bright
      Styles.Selection = Dark
      Styles.Category = Dark
      Styles.Header = Spring
      Styles.IncSearch = Golden
      BuiltIn = True
    end
  end
  object mdOrders: TdxMemData
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
    SortedField = 'Orders_ID'
    Left = 52
    Top = 64
    object mdOrdersPurchaseDate: TDateTimeField
      FieldName = 'PurchaseDate'
    end
    object mdOrdersOrders_Time: TDateTimeField
      DisplayLabel = 'Time'
      FieldName = 'Orders_Time'
    end
    object mdOrdersPaymentType: TStringField
      FieldName = 'PaymentType'
      Size = 7
    end
    object mdOrdersPaymentAmount: TFloatField
      FieldName = 'PaymentAmount'
    end
    object mdOrdersQuantity: TIntegerField
      FieldName = 'Quantity'
    end
    object mdOrdersFirstName: TStringField
      FieldName = 'FirstName'
      Size = 25
    end
    object mdOrdersLastName: TStringField
      FieldName = 'LastName'
      Size = 25
    end
    object mdOrdersCompany: TStringField
      FieldName = 'Company'
      Size = 50
    end
    object mdOrdersPrefix: TStringField
      FieldName = 'Prefix'
      Size = 15
    end
    object mdOrdersTitle: TStringField
      FieldName = 'Title'
      Size = 15
    end
    object mdOrdersAddress: TStringField
      FieldName = 'Address'
      Size = 50
    end
    object mdOrdersCity: TStringField
      FieldName = 'City'
    end
    object mdOrdersState: TStringField
      FieldName = 'State'
      Size = 2
    end
    object mdOrdersZipCode: TStringField
      FieldName = 'ZipCode'
      Size = 10
    end
    object mdOrdersSource: TStringField
      FieldName = 'Source'
      Size = 10
    end
    object mdOrdersCustomer: TStringField
      FieldName = 'Customer'
      Size = 1
    end
    object mdOrdersHomePhone: TStringField
      FieldName = 'HomePhone'
      Size = 15
    end
    object mdOrdersFaxPhone: TStringField
      FieldName = 'FaxPhone'
      Size = 15
    end
    object mdOrdersSpouse: TStringField
      FieldName = 'Spouse'
      Size = 50
    end
    object mdOrdersOccupation: TStringField
      FieldName = 'Occupation'
      Size = 25
    end
    object mdOrdersEmail: TStringField
      FieldName = 'Email'
      Size = 255
    end
    object mdOrdersTrademark: TStringField
      FieldName = 'Trademark'
      Size = 50
    end
    object mdOrdersModel: TStringField
      FieldName = 'Model'
      Size = 50
    end
    object mdOrdersHP: TSmallintField
      FieldName = 'HP'
    end
    object mdOrdersLiter: TFloatField
      FieldName = 'Liter'
    end
    object mdOrdersCyl: TSmallintField
      FieldName = 'Cyl'
    end
    object mdOrdersTransmissSpeedCount: TSmallintField
      FieldName = 'TransmissSpeedCount'
    end
    object mdOrdersTransmissAutomatic: TStringField
      FieldName = 'TransmissAutomatic'
      Size = 3
    end
    object mdOrdersMPG_City: TSmallintField
      FieldName = 'MPG_City'
    end
    object mdOrdersMPG_Highway: TSmallintField
      FieldName = 'MPG_Highway'
    end
    object mdOrdersCategory: TStringField
      FieldName = 'Category'
      Size = 7
    end
    object mdOrdersCars_Description: TMemoField
      FieldName = 'Cars_Description'
      BlobType = ftMemo
      Size = 10
    end
    object mdOrdersHyperlink: TStringField
      FieldName = 'Hyperlink'
      Size = 50
    end
    object mdOrdersPicture: TBlobField
      FieldName = 'Picture'
      Size = 10
    end
    object mdOrdersPrice: TFloatField
      FieldName = 'Price'
    end
    object mdOrdersCustomers_ID: TIntegerField
      FieldName = 'Customers_ID'
    end
    object mdOrdersOrders_ID: TIntegerField
      FieldName = 'Orders_ID'
    end
    object mdOrdersCars_ID: TIntegerField
      FieldName = 'Cars_ID'
    end
  end
end

object StylesSimpleDemoDataDM: TStylesSimpleDemoDataDM
  OldCreateOrder = False
  OnCreate = StylesSimpleDemoDataDMCreate
  Left = 733
  Top = 408
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
      5665728FC2F5285C8FFE3F28000000080000000B000D00507572636861736544
      617465000700000001000C005061796D656E7454797065000800000006000E00
      5061796D656E74416D6F756E740004000000030009005175616E746974790019
      00000001000A0046697273744E616D650019000000010009004C6173744E616D
      65003200000001000800436F6D70616E79000F00000001000700507265666978
      000F000000010006005469746C65003200000001000800416464726573730014
      00000001000500436974790002000000010006005374617465000A0000000100
      08005A6970436F6465000A00000001000700536F757263650001000000010009
      00437573746F6D6572000F00000001000A00486F6D6550686F6E65000F000000
      0100090046617850686F6E6500320000000100070053706F7573650019000000
      01000B004F636375706174696F6E00FF00000001000600456D61696C00320000
      0001000A0054726164656D61726B0032000000010006004D6F64656C00020000
      000200030048500008000000060006004C697465720002000000020004004379
      6C0002000000020014005472616E736D6973735370656564436F756E74000300
      0000010013005472616E736D6973734175746F6D617469630002000000020009
      004D50475F43697479000200000002000C004D50475F48696768776179000700
      00000100090043617465676F727900000000000E001100436172735F44657363
      72697074696F6E003200000001000A0048797065726C696E6B00000000000D00
      0800506963747572650008000000060006005072696365000400000003000D00
      437573746F6D6572735F4944000400000003000B00437573746F6D6572494400
      0400000003000A004F72646572735F4944000400000003000A0050726F647563
      744944000400000003000800436172735F494400080000000B000C004F726465
      72735F54696D6500}
    SortOptions = []
    SortedField = 'Orders_ID'
    Left = 52
    Top = 68
    object mdOrdersPurchaseDate: TDateTimeField
      FieldName = 'PurchaseDate'
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
    end
    object mdOrdersHyperlink: TStringField
      FieldName = 'Hyperlink'
      Size = 50
    end
    object mdOrdersPicture: TBlobField
      FieldName = 'Picture'
    end
    object mdOrdersPrice: TFloatField
      FieldName = 'Price'
    end
    object mdOrdersCustomers_ID: TIntegerField
      FieldName = 'Customers_ID'
    end
    object mdOrdersCustomerID: TIntegerField
      FieldName = 'CustomerID'
    end
    object mdOrdersOrders_ID: TIntegerField
      FieldName = 'Orders_ID'
    end
    object mdOrdersProductID: TIntegerField
      FieldName = 'ProductID'
    end
    object mdOrdersCars_ID: TIntegerField
      FieldName = 'Cars_ID'
    end
    object mdOrdersOrders_Time: TDateTimeField
      FieldName = 'Orders_Time'
    end
  end
end

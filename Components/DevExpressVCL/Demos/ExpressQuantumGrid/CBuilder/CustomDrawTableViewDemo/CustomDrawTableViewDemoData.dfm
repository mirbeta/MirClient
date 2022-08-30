object CustomDrawTableViewDemoMainDM: TCustomDrawTableViewDemoMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 334
  Width = 491
  object StyleRepository: TcxStyleRepository
    Left = 120
    Top = 8
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle5: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clBlack
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor]
      Color = 14872561
    end
    object cxStyle8: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle9: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle10: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle11: TcxStyle
      AssignedValues = [svColor]
      Color = 8453888
    end
    object cxStyle12: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle13: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16777088
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlue
    end
    object cxStyle14: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 12937777
      TextColor = clWhite
    end
    object GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      Caption = 'DevExpress'
      Styles.Background = cxStyle1
      Styles.Content = cxStyle2
      Styles.ContentEven = cxStyle3
      Styles.ContentOdd = cxStyle4
      Styles.FilterBox = cxStyle5
      Styles.Inactive = cxStyle10
      Styles.IncSearch = cxStyle11
      Styles.Selection = cxStyle14
      Styles.Footer = cxStyle6
      Styles.Group = cxStyle7
      Styles.GroupByBox = cxStyle8
      Styles.Header = cxStyle9
      Styles.Indicator = cxStyle12
      Styles.Preview = cxStyle13
      BuiltIn = True
    end
  end
  object dsCustomers: TDataSource
    DataSet = cdsCustomers
    Left = 40
    Top = 112
  end
  object dsOrders: TDataSource
    DataSet = cdsOrders
    Left = 40
    Top = 168
  end
  object cdsCustomers: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 120
    Top = 112
    object cdsCustomersID: TAutoIncField
      FieldName = 'ID'
      ProviderFlags = [pfInUpdate, pfInWhere, pfInKey]
      ReadOnly = True
    end
    object cdsCustomersFirstName: TStringField
      FieldName = 'FirstName'
      Size = 25
    end
    object cdsCustomersLastName: TStringField
      FieldName = 'LastName'
      Size = 25
    end
    object cdsCustomersCompany: TStringField
      FieldName = 'Company'
      Size = 50
    end
    object cdsCustomersPrefix: TStringField
      FieldName = 'Prefix'
      Size = 15
    end
    object cdsCustomersTitle: TStringField
      FieldName = 'Title'
      Size = 15
    end
    object cdsCustomersAddress: TStringField
      FieldName = 'Address'
      Size = 50
    end
    object cdsCustomersCity: TStringField
      FieldName = 'City'
    end
    object cdsCustomersState: TStringField
      FieldName = 'State'
      Size = 2
    end
    object cdsCustomersZipCode: TStringField
      FieldName = 'ZipCode'
      Size = 10
    end
    object cdsCustomersSource: TStringField
      FieldName = 'Source'
      Size = 10
    end
    object cdsCustomersCustomer: TStringField
      FieldName = 'Customer'
      Size = 1
    end
    object cdsCustomersHomePhone: TStringField
      FieldName = 'HomePhone'
      Size = 15
    end
    object cdsCustomersFaxPhone: TStringField
      FieldName = 'FaxPhone'
      Size = 15
    end
    object cdsCustomersSpouse: TStringField
      FieldName = 'Spouse'
      Size = 50
    end
    object cdsCustomersOccupation: TStringField
      FieldName = 'Occupation'
      Size = 25
    end
    object cdsCustomersDescription: TMemoField
      FieldName = 'Description'
      BlobType = ftMemo
      Size = 10
    end
    object cdsCustomersEmail: TStringField
      FieldName = 'Email'
      Size = 255
    end
  end
  object cdsOrders: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 120
    Top = 168
    object cdsOrdersID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsOrdersCustomerID: TIntegerField
      FieldName = 'CustomerID'
    end
    object cdsOrdersProductID: TIntegerField
      FieldName = 'ProductID'
    end
    object cdsOrdersPurchaseDate: TDateTimeField
      FieldName = 'PurchaseDate'
    end
    object cdsOrdersTime: TDateTimeField
      FieldName = 'Time'
    end
    object cdsOrdersPaymentType: TStringField
      FieldName = 'PaymentType'
      Size = 7
    end
    object cdsOrdersDescription: TMemoField
      FieldName = 'Description'
      BlobType = ftMemo
      Size = 10
    end
    object cdsOrdersQuantity: TIntegerField
      FieldName = 'Quantity'
    end
    object cdsOrdersPaymentAmount: TCurrencyField
      FieldName = 'PaymentAmount'
    end
  end
end

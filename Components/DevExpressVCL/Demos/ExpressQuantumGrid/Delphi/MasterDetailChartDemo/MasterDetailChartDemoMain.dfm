inherited frmMain: TfrmMain
  Left = 449
  Top = 142
  Caption = 'ExpressQuantumGrid Master-Detail Chart Demo'
  ClientHeight = 646
  ClientWidth = 887
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 887
    Height = 32
    Caption = 
      '  This demo shows how the ChartView can be used as a detail view' +
      ' in a master-detail grid. Click '#39'About this demo'#39' for more infor' +
      'mation.'
  end
  inherited sbMain: TStatusBar
    Top = 627
    Width = 887
  end
  object grMain: TcxGrid [2]
    Left = 0
    Top = 32
    Width = 887
    Height = 595
    Align = alClient
    TabOrder = 0
    object tvCustomers: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = dsCustomers
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      object tvCustomersID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Width = 34
      end
      object tvCustomersFirstName: TcxGridDBColumn
        Caption = 'First Name'
        DataBinding.FieldName = 'FirstName'
        Width = 77
      end
      object tvCustomersLastName: TcxGridDBColumn
        Caption = 'Last Name'
        DataBinding.FieldName = 'LastName'
        Width = 79
      end
      object tvCustomersCompany: TcxGridDBColumn
        DataBinding.FieldName = 'Company'
        Width = 135
      end
      object tvCustomersAddress: TcxGridDBColumn
        DataBinding.FieldName = 'Address'
        Width = 133
      end
      object tvCustomersCity: TcxGridDBColumn
        DataBinding.FieldName = 'City'
        Width = 77
      end
      object tvCustomersState: TcxGridDBColumn
        DataBinding.FieldName = 'State'
        Width = 57
      end
      object tvCustomersZipCode: TcxGridDBColumn
        Caption = 'Zip Code'
        DataBinding.FieldName = 'ZipCode'
      end
      object tvCustomersEmail: TcxGridDBColumn
        DataBinding.FieldName = 'Email'
        Width = 181
      end
    end
    object chvOrders: TcxGridDBChartView
      Categories.DataBinding.FieldName = 'ID'
      Categories.OnGetValueDisplayText = chvOrdersCategoriesGetValueDisplayText
      DataController.DataSource = dsOrders
      DataController.DetailKeyFieldNames = 'CustomerID'
      DataController.MasterKeyFieldNames = 'ID'
      DiagramColumn.Active = True
      DiagramColumn.AxisCategory.GridLines = False
      DiagramColumn.AxisCategory.TickMarkKind = tmkNone
      Legend.Position = cppNone
      OptionsView.CategoriesPerPage = 10
      OnGetValueHint = chvOrdersGetValueHint
      object chvOrdersPaymentAmountSeries: TcxGridDBChartSeries
        DataBinding.FieldName = 'PaymentAmount'
        DisplayText = 'Payment Amount'
        SortOrder = soDescending
        ValueCaptionFormat = '$,0'
      end
      object chvOrdersProductIDSeries: TcxGridDBChartSeries
        DataBinding.FieldName = 'ProductID'
        Visible = False
      end
      object chvOrdersQuantitySeries: TcxGridDBChartSeries
        DataBinding.FieldName = 'Quantity'
        Visible = False
      end
    end
    object chvProducts: TcxGridDBChartView
      Categories.DataBinding.FieldName = 'Name'
      DataController.DataSource = dsProducts
      DataController.DetailKeyFieldNames = 'CustomerID'
      DataController.MasterKeyFieldNames = 'ID'
      DiagramColumn.Active = True
      DiagramColumn.AxisValue.Title.Text = 'Copies'
      DiagramColumn.Values.VaryColorsByCategory = True
      Legend.Position = cppNone
      object chvProductsCopiesSeries: TcxGridDBChartSeries
        DataBinding.FieldName = 'SUM OF Quantity'
        DisplayText = 'Quantity'
      end
    end
    object grMainLevel1: TcxGridLevel
      GridView = tvCustomers
      MaxDetailHeight = 250
      Options.DetailTabsPosition = dtpTop
      object grMainLevel2: TcxGridLevel
        Caption = 'Orders'
        GridView = chvOrders
      end
      object grMainLevel3: TcxGridLevel
        Caption = 'Products'
        GridView = chvProducts
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object dsOrders: TDataSource
    DataSet = cdsOrders
    Left = 360
    Top = 156
  end
  object dsCustomers: TDataSource
    DataSet = cdsCustomers
    Left = 360
    Top = 116
  end
  object dsProducts: TDataSource
    DataSet = cdsProductSumOfQuantity
    Left = 360
    Top = 192
  end
  object cdsCustomers: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\Customers.xml'
    Params = <>
    Left = 400
    Top = 112
    object cdsCustomersID: TAutoIncField
      FieldName = 'ID'
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
    FileName = '..\..\Data\Orders.xml'
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly, faUnNamed]
        DataType = ftAutoInc
      end
      item
        Name = 'CustomerID'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'ProductID'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'PurchaseDate'
        Attributes = [faUnNamed]
        DataType = ftDateTime
      end
      item
        Name = 'Time'
        Attributes = [faUnNamed]
        DataType = ftDateTime
      end
      item
        Name = 'PaymentType'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 7
      end
      item
        Name = 'Description'
        Attributes = [faUnNamed]
        DataType = ftMemo
        Size = 10
      end
      item
        Name = 'Quantity'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'PaymentAmount'
        Attributes = [faUnNamed]
        DataType = ftCurrency
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end
      item
        Name = 'cdsOrdersIndex3'
      end>
    IndexFieldNames = 'CustomerID'
    Params = <>
    StoreDefs = True
    Left = 400
    Top = 192
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
  object cdsProductSumOfQuantity: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\ProductSumOfQuantity.xml'
    FieldDefs = <
      item
        Name = 'CustomerID'
        Attributes = [faUnNamed]
        DataType = ftInteger
      end
      item
        Name = 'Name'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 100
      end
      item
        Name = 'SUM OF Quantity'
        Attributes = [faUnNamed]
        DataType = ftFloat
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
      end>
    Params = <>
    StoreDefs = True
    Left = 400
    Top = 232
    object cdsProductSumOfQuantityCustomerID: TIntegerField
      FieldName = 'CustomerID'
    end
    object cdsProductSumOfQuantityName: TStringField
      FieldName = 'Name'
      Size = 100
    end
    object cdsProductSumOfQuantitySUMOFQuantity: TFloatField
      FieldName = 'SUM OF Quantity'
    end
  end
  object cdsProducts: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\PRODUCTS.xml'
    FieldDefs = <
      item
        Name = 'ID'
        Attributes = [faReadonly, faUnNamed]
        DataType = ftAutoInc
      end
      item
        Name = 'Name'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 100
      end
      item
        Name = 'Description'
        Attributes = [faUnNamed]
        DataType = ftMemo
        Size = 10
      end
      item
        Name = 'Platform'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 20
      end
      item
        Name = 'Logo'
        Attributes = [faUnNamed]
        DataType = ftParadoxOle
        Size = 10
      end
      item
        Name = 'Link'
        Attributes = [faUnNamed]
        DataType = ftMemo
        Size = 10
      end>
    IndexDefs = <>
    IndexFieldNames = 'ID'
    Params = <>
    StoreDefs = True
    Left = 400
    Top = 152
    object cdsProductsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsProductsName: TStringField
      FieldName = 'Name'
      Size = 100
    end
    object cdsProductsDescription: TMemoField
      FieldName = 'Description'
      BlobType = ftMemo
      Size = 10
    end
    object cdsProductsPlatform: TStringField
      FieldName = 'Platform'
    end
    object cdsProductsLogo: TBlobField
      FieldName = 'Logo'
      BlobType = ftParadoxOle
      Size = 10
    end
    object cdsProductsLink: TMemoField
      FieldName = 'Link'
      BlobType = ftMemo
      Size = 10
    end
  end
end

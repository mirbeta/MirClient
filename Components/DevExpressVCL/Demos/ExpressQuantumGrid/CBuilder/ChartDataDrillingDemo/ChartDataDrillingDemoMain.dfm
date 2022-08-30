inherited frmMain: TfrmMain
  Left = 300
  Top = 120
  Caption = 'ExpressQuantumGrid ChartDataDrilling Demo'
  ClientHeight = 566
  ClientWidth = 842
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 842
    Caption = 
      '  This demo shows the chart'#39's data drilling capabilities. Click ' +
      #39'About this demo'#39' for more information.'
  end
  object Grid: TcxGrid
    Left = 0
    Top = 16
    Width = 842
    Height = 550
    Align = alClient
    TabOrder = 0
    LevelTabs.Style = 9
    RootLevelOptions.DetailTabsPosition = dtpTop
    OnActiveTabChanged = GridActiveTabChanged
    object ChartView: TcxGridDBChartView
      Categories.DataBinding.FieldName = 'ID'
      Categories.DisplayText = 'Order'
      DataController.DataSource = dsOrders
      DiagramColumn.Active = True
      DiagramPie.Legend.Position = cppRight
      Legend.Position = cppNone
      OptionsView.CategoriesPerPage = 10
      ToolBox.CustomizeButton = True
      ToolBox.DataLevelsInfoVisible = dlivAlways
      ToolBox.DiagramSelector = True
      object ChartViewDataGroupPurchaseDate: TcxGridDBChartDataGroup
        DataBinding.FieldName = 'PurchaseDate'
        DisplayText = 'Purchase Date'
        SortOrder = soNone
        Visible = False
      end
      object ChartViewDataGroupProduct: TcxGridDBChartDataGroup
        DataBinding.FieldName = 'Product'
        DisplayText = 'Product'
      end
      object ChartViewDataGroupCustomer: TcxGridDBChartDataGroup
        DataBinding.FieldName = 'Customer'
        DisplayText = 'Customer'
      end
      object ChartViewDataGroupPaymentType: TcxGridDBChartDataGroup
        DataBinding.FieldName = 'PaymentType'
        DisplayText = 'Payment Type'
      end
      object ChartViewSeriesPaymentAmount: TcxGridDBChartSeries
        DataBinding.FieldName = 'PaymentAmount'
        DisplayText = 'Payment Amount'
        SortOrder = soDescending
      end
      object ChartViewSeriesQuantity: TcxGridDBChartSeries
        DataBinding.FieldName = 'Quantity'
        DisplayText = 'Quantity'
        Visible = False
      end
      object ChartViewSeriesCount: TcxGridDBChartSeries
        DisplayText = 'Order Count'
        Visible = False
        GroupSummaryKind = skCount
      end
    end
    object TableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.First.Visible = True
      Navigator.Buttons.PriorPage.Visible = True
      Navigator.Buttons.Prior.Visible = True
      Navigator.Buttons.Next.Visible = True
      Navigator.Buttons.NextPage.Visible = True
      Navigator.Buttons.Last.Visible = True
      Navigator.Buttons.Insert.Visible = True
      Navigator.Buttons.Delete.Visible = True
      Navigator.Buttons.Edit.Visible = True
      Navigator.Buttons.Post.Visible = True
      Navigator.Buttons.Cancel.Visible = True
      Navigator.Buttons.Refresh.Visible = True
      Navigator.Buttons.SaveBookmark.Visible = True
      Navigator.Buttons.GotoBookmark.Visible = True
      Navigator.Buttons.Filter.Visible = True
      DataController.DataSource = dsOrders
      DataController.Summary.DefaultGroupSummaryItems = <
        item
          Format = '$0,'
          Kind = skSum
          Column = TableViewPaymentAmount
        end
        item
          Format = '0'
          Kind = skSum
          Column = TableViewQuantity
        end
        item
          Format = '0'
          Kind = skCount
          Column = TableViewOrderCount
        end>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnSorting = False
      OptionsSelection.MultiSelect = True
      OptionsView.GroupSummaryLayout = gslAlignWithColumns
      Styles.OnGetGroupStyle = TableViewStylesGetGroupStyle
      object TableViewProduct: TcxGridDBColumn
        DataBinding.FieldName = 'Product'
        Width = 131
      end
      object TableViewCustomer: TcxGridDBColumn
        DataBinding.FieldName = 'Customer'
        Width = 131
      end
      object TableViewPaymentType: TcxGridDBColumn
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PaymentType'
        Width = 91
      end
      object TableViewPurchaseDate: TcxGridDBColumn
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PurchaseDate'
      end
      object TableViewQuantity: TcxGridDBColumn
        DataBinding.FieldName = 'Quantity'
        Width = 76
      end
      object TableViewPaymentAmount: TcxGridDBColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PaymentAmount'
        Width = 107
      end
      object TableViewOrderCount: TcxGridDBColumn
        Caption = 'Order Count'
        GroupSummaryAlignment = taRightJustify
        Width = 83
      end
    end
    object GridLevelChart: TcxGridLevel
      Caption = '  Chart  '
      GridView = ChartView
    end
    object GridLevelTable: TcxGridLevel
      Caption = '  Table  '
      GridView = TableView
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    object styleActiveGroup: TcxStyle [14]
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object dsOrders: TDataSource
    DataSet = tblOrders
    Left = 232
    Top = 156
  end
  object tblOrders: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 260
    Top = 156
    object tblOrdersID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object tblOrdersCustomerID: TIntegerField
      FieldName = 'CustomerID'
    end
    object tblOrdersProductID: TIntegerField
      FieldName = 'ProductID'
    end
    object tblOrdersPurchaseDate: TDateTimeField
      FieldName = 'PurchaseDate'
    end
    object tblOrdersTime: TDateTimeField
      FieldName = 'Time'
    end
    object tblOrdersPaymentType: TStringField
      FieldName = 'PaymentType'
      Size = 7
    end
    object tblOrdersPaymentAmount: TCurrencyField
      FieldName = 'PaymentAmount'
    end
    object tblOrdersDescription: TMemoField
      FieldName = 'Description'
      BlobType = ftMemo
      Size = 10
    end
    object tblOrdersQuantity: TIntegerField
      FieldName = 'Quantity'
    end
    object tblOrdersCustomer: TStringField
      FieldKind = fkLookup
      FieldName = 'Customer'
      LookupDataSet = tblCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Company'
      KeyFields = 'CustomerID'
      Lookup = True
    end
    object tblOrdersProduct: TStringField
      FieldKind = fkLookup
      FieldName = 'Product'
      LookupDataSet = tblProducts
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'ProductID'
      Size = 100
      Lookup = True
    end
  end
  object tblCustomers: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'ID'
    Params = <>
    Left = 260
    Top = 184
  end
  object tblProducts: TClientDataSet
    Aggregates = <>
    IndexFieldNames = 'ID'
    Params = <>
    Left = 260
    Top = 212
  end
end

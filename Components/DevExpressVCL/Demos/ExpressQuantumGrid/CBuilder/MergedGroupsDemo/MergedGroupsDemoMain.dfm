inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Merged Groups Demo'
  ClientHeight = 551
  ClientWidth = 985
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 985
    Height = 32
    Caption = 
      'This demo shows how to group records in merged column grouping m' +
      'ode, allowing for a more compact record list. Press and hold the' +
      ' Ctrl key to use this grouping mode when dropping column headers' +
      ' within the Group By box.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 985
  end
  object Grid: TcxGrid [2]
    Left = 0
    Top = 16
    Width = 985
    Height = 516
    Align = alClient
    TabOrder = 1
    object TableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      Navigator.Buttons.First.Visible = True
      Navigator.Buttons.PriorPage.Visible = True
      Navigator.Buttons.Prior.Visible = True
      Navigator.Buttons.Next.Visible = True
      Navigator.Buttons.NextPage.Visible = True
      Navigator.Buttons.Last.Visible = True
      Navigator.Buttons.Insert.Visible = True
      Navigator.Buttons.Append.Visible = False
      Navigator.Buttons.Delete.Visible = True
      Navigator.Buttons.Edit.Visible = True
      Navigator.Buttons.Post.Visible = True
      Navigator.Buttons.Cancel.Visible = True
      Navigator.Buttons.Refresh.Visible = True
      Navigator.Buttons.SaveBookmark.Visible = True
      Navigator.Buttons.GotoBookmark.Visible = True
      Navigator.Buttons.Filter.Visible = True
      FindPanel.DisplayMode = fpdmManual
      DataController.DataSource = dsOrders
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      DataController.OnGroupingChanged = TableViewDataControllerGroupingChanged
      OptionsBehavior.ColumnMergedGrouping = True
      OptionsBehavior.FixedGroups = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object TableViewCompanyName: TcxGridDBColumn
        DataBinding.FieldName = 'Company Name'
        Visible = False
        GroupIndex = 1
        Width = 175
      end
      object TableViewPurchaseDate: TcxGridDBColumn
        Tag = 5
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PurchaseDate'
        Width = 99
      end
      object TableViewModel: TcxGridDBColumn
        Tag = -1
        DataBinding.FieldName = 'Model'
        Width = 247
      end
      object TableViewUnitPrice: TcxGridDBColumn
        Tag = 3
        DataBinding.FieldName = 'Unit Price'
        Width = 91
      end
      object TableViewQuantity: TcxGridDBColumn
        Tag = 2
        DataBinding.FieldName = 'Quantity'
        Width = 55
      end
      object TableViewPaymentAmount: TcxGridDBColumn
        Tag = 4
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PaymentAmount'
        Width = 102
      end
      object TableViewPaymentType: TcxGridDBColumn
        Tag = 1
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PaymentType'
        Visible = False
        GroupIndex = 0
        Width = 83
      end
      object TableViewTrademark: TcxGridDBColumn
        Caption = 'Trademark'
        DataBinding.FieldName = 'ProductID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'Trademark'
          end>
        Properties.ListSource = dmGridCars.dsModels
        Visible = False
        GroupIndex = 2
        IsChildInMergedGroup = True
      end
    end
    object GridLevel1: TcxGridLevel
      GridView = TableView
    end
  end
  inherited mmMain: TMainMenu
    Left = 180
    Top = 248
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 112
    Top = 248
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 16
    Top = 248
  end
  object erMain: TcxEditRepository
    Left = 188
    Top = 340
    object erMainFlag: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
  end
  object dsOrders: TDataSource
    DataSet = cdsOrders
    Left = 24
    Top = 304
  end
  object dsCustomers: TDataSource
    DataSet = cdsCustomers
    Left = 24
    Top = 361
  end
  object cdsOrders: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\Orders.xml'
    Params = <>
    OnCalcFields = cdsOrdersCalcFields
    Left = 104
    Top = 305
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
    object cdsOrdersPaymentType: TStringField
      FieldName = 'PaymentType'
      Size = 7
    end
    object cdsOrdersQuantity: TIntegerField
      FieldName = 'Quantity'
    end
    object cdsOrdersUnitPrice: TCurrencyField
      FieldKind = fkLookup
      FieldName = 'Unit Price'
      LookupDataSet = dmGridCars.mdModels
      LookupKeyFields = 'ID'
      LookupResultField = 'Price'
      KeyFields = 'ProductID'
      ReadOnly = True
      Lookup = True
    end
    object cdsOrdersCompanyName: TStringField
      FieldKind = fkLookup
      FieldName = 'Company Name'
      LookupDataSet = cdsCustomers
      LookupKeyFields = 'ID'
      LookupResultField = 'Company'
      KeyFields = 'CustomerID'
      ReadOnly = True
      Size = 255
      Lookup = True
    end
    object cdsOrdersPaymentAmount: TCurrencyField
      FieldKind = fkCalculated
      FieldName = 'PaymentAmount'
      ReadOnly = True
      Calculated = True
    end
    object cdsOrdersCarName: TStringField
      FieldKind = fkLookup
      FieldName = 'Model'
      LookupDataSet = dmGridCars.mdModels
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'ProductID'
      Lookup = True
    end
  end
  object cdsCustomers: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\Customers.xml'
    Params = <>
    Left = 104
    Top = 361
    object cdsCustomersID: TIntegerField
      FieldName = 'ID'
    end
    object cdsCustomersCompany: TStringField
      FieldName = 'Company'
      Size = 50
    end
  end
end

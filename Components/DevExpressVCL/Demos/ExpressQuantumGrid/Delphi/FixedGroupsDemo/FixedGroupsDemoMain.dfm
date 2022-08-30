inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Fixed Groups Demo'
  ClientHeight = 551
  ClientWidth = 763
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 763
    Height = 32
    Caption = 
      'This demo shows how to anchor group rows to the top of the grid ' +
      'View so that scrolling through their content doesn'#39't take them o' +
      'ut of view.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 763
  end
  object gbOptions: TcxGroupBox [2]
    Left = 0
    Top = 16
    Align = alTop
    TabOrder = 1
    Height = 41
    Width = 763
    object tsFixedGroups: TdxToggleSwitch
      Left = 8
      Top = 12
      Action = acFixedGroups
      Caption = 'Fixed Groups:'
      Checked = True
      TabOrder = 0
      Transparent = True
    end
  end
  object Grid: TcxGrid [3]
    Left = 0
    Top = 57
    Width = 763
    Height = 475
    Align = alClient
    TabOrder = 2
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
      OptionsBehavior.FixedGroups = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object TableViewCompanyName: TcxGridDBColumn
        DataBinding.FieldName = 'Company Name'
        Visible = False
        GroupIndex = 1
        Width = 175
      end
      object TableViewCarName: TcxGridDBColumn
        DataBinding.FieldName = 'Car Name'
        Width = 247
      end
      object TableViewQuantity: TcxGridDBColumn
        DataBinding.FieldName = 'Quantity'
        Width = 55
      end
      object TableViewUnitPrice: TcxGridDBColumn
        DataBinding.FieldName = 'Unit Price'
        Width = 91
      end
      object TableViewPaymentAmount: TcxGridDBColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PaymentAmount'
        Width = 102
      end
      object TableViewPaymentType: TcxGridDBColumn
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PaymentType'
        Visible = False
        GroupIndex = 0
        Width = 83
      end
      object TableViewPurchaseDate: TcxGridDBColumn
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PurchaseDate'
        Width = 99
      end
    end
    object GridLevel1: TcxGridLevel
      GridView = TableView
    end
  end
  inherited mmMain: TMainMenu
    Left = 220
    Top = 192
    object miFixedGroupsOptions: TMenuItem [1]
      Caption = 'Options'
      object miFixedGroups: TMenuItem
        Action = acFixedGroups
        AutoCheck = True
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 128
    Top = 192
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 24
    Top = 192
  end
  object erMain: TcxEditRepository
    Left = 380
    Top = 196
    object erMainFlag: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
  end
  object alAction: TActionList
    Left = 304
    Top = 192
    object acFixedGroups: TAction
      AutoCheck = True
      Caption = 'Fixed Groups'
      Checked = True
      OnExecute = acFixedGroupsExecute
    end
  end
  object dsOrders: TDataSource
    DataSet = cdsOrders
    Left = 24
    Top = 304
  end
  object cdsOrders: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\Orders.xml'
    Params = <>
    OnCalcFields = cdsOrdersCalcFields
    Left = 176
    Top = 304
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
      FieldName = 'Car Name'
      LookupDataSet = dmGridCars.mdModels
      LookupKeyFields = 'ID'
      LookupResultField = 'Name'
      KeyFields = 'ProductID'
      Lookup = True
    end
  end
  object dsCustomers: TDataSource
    DataSet = cdsCustomers
    Left = 24
    Top = 360
  end
  object cdsCustomers: TClientDataSet
    Aggregates = <>
    FileName = '..\..\Data\Customers.xml'
    Params = <>
    Left = 176
    Top = 360
    object cdsCustomersID: TIntegerField
      FieldName = 'ID'
    end
    object cdsCustomersCompany: TStringField
      FieldName = 'Company'
      Size = 50
    end
  end
end

inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Fixed Data Rows Demo'
  ClientHeight = 551
  ClientWidth = 793
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 793
    Height = 64
    Caption = 
      'This demo shows how to anchor specific data rows to either the t' +
      'op or bottom of the grid so that these rows are always displayed' +
      ' when scrolling through the grid. You can click on the pin for e' +
      'ach row to fix it, or select the corresponding option in the row' +
      ' context menu to fix/unfix the row, as well as try different pin' +
      ' visibility options and select pin click actions.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 793
  end
  object cxGroupBox1: TcxGroupBox [2]
    Left = 0
    Top = 64
    Align = alTop
    TabOrder = 1
    Height = 81
    Width = 793
    object gbPinClickAction: TcxGroupBox
      Left = 236
      Top = 11
      Caption = 'Pin Click Action'
      TabOrder = 1
      Height = 62
      Width = 227
      object rbShowPopup: TcxRadioButton
        Left = 10
        Top = 17
        Width = 83
        Height = 17
        Caption = 'Show Popup'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = FixationCapabilityChanged
        GroupIndex = 2
        Transparent = True
      end
      object rbNone: TcxRadioButton
        Left = 10
        Top = 35
        Width = 70
        Height = 17
        Caption = 'None'
        TabOrder = 1
        OnClick = FixationCapabilityChanged
        GroupIndex = 2
        Transparent = True
      end
      object rbFixRowToBottom: TcxRadioButton
        Left = 102
        Top = 35
        Width = 116
        Height = 17
        Caption = 'Fix Row to Bottom'
        TabOrder = 2
        OnClick = FixationCapabilityChanged
        GroupIndex = 2
        Transparent = True
      end
      object rbFixRowToTop: TcxRadioButton
        Left = 102
        Top = 17
        Width = 95
        Height = 17
        Caption = 'Fix Row to Top'
        TabOrder = 3
        OnClick = FixationCapabilityChanged
        GroupIndex = 2
        Transparent = True
      end
    end
    object gbPinVisibility: TcxGroupBox
      Left = 9
      Top = 11
      Caption = 'Pin Visibility'
      TabOrder = 0
      Height = 62
      Width = 221
      object rbPinVisibilityNever: TcxRadioButton
        Left = 10
        Top = 17
        Width = 79
        Height = 17
        Caption = 'Never'
        TabOrder = 0
        OnClick = PinVisibilityChanged
        GroupIndex = 1
        Transparent = True
      end
      object rbPinVisibilityAlways: TcxRadioButton
        Left = 10
        Top = 35
        Width = 79
        Height = 17
        Caption = 'Always'
        TabOrder = 1
        OnClick = PinVisibilityChanged
        GroupIndex = 1
        Transparent = True
      end
      object rbPinVisibilityRowHover: TcxRadioButton
        Left = 77
        Top = 35
        Width = 135
        Height = 17
        Caption = 'When Hover over Row'
        Checked = True
        TabOrder = 2
        TabStop = True
        OnClick = PinVisibilityChanged
        GroupIndex = 1
        Transparent = True
      end
      object rbPinVisibilityHover: TcxRadioButton
        Left = 77
        Top = 17
        Width = 71
        Height = 17
        Caption = 'On Hover'
        TabOrder = 3
        OnClick = PinVisibilityChanged
        GroupIndex = 1
        Transparent = True
      end
    end
    object tbSeparatorWidth: TcxTrackBar
      Left = 545
      Top = 26
      Position = 6
      Properties.Min = 2
      Properties.OnChange = tbSeparatorWidthPropertiesChange
      TabOrder = 2
      Transparent = True
      Height = 41
      Width = 196
    end
    object lbSeparatorWidth: TcxLabel
      Left = 468
      Top = 36
      Caption = 'Separator Width'
      Transparent = True
    end
  end
  object Grid: TcxGrid [3]
    Left = 0
    Top = 145
    Width = 793
    Height = 387
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
      DataController.DataSource = dsOrders
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <
        item
          Format = '0'
          Kind = skSum
          Column = TableViewQuantity
        end
        item
          Kind = skSum
          Column = TableViewPaymentAmount
        end
        item
          Kind = skCount
          Column = TableViewPurchaseDate
        end>
      DataController.Summary.SummaryGroups = <>
      FixedDataRows.PinVisibility = rpvRowHotTrack
      OptionsCustomize.DataRowFixing = True
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object TableViewCompanyName: TcxGridDBColumn
        DataBinding.FieldName = 'Company Name'
        Width = 152
      end
      object TableViewCarName: TcxGridDBColumn
        DataBinding.FieldName = 'Car Name'
        Width = 210
      end
      object TableViewQuantity: TcxGridDBColumn
        DataBinding.FieldName = 'Quantity'
        Width = 55
      end
      object TableViewUnitPrice: TcxGridDBColumn
        DataBinding.FieldName = 'Unit Price'
        Options.Editing = False
        Width = 91
      end
      object TableViewPaymentAmount: TcxGridDBColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PaymentAmount'
        Options.Editing = False
        Width = 102
      end
      object TableViewPaymentType: TcxGridDBColumn
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PaymentType'
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
    PixelsPerInch = 96
    object erMainFlag: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
  end
  object alAction: TActionList
    Left = 304
    Top = 192
    object actClearFindOnClose: TAction
      AutoCheck = True
      Caption = 'Clear Find Filter Text On Close'
      Checked = True
    end
    object actShowClearButton: TAction
      AutoCheck = True
      Caption = 'Show Clear Button'
      Checked = True
    end
    object actShowCloseButton: TAction
      AutoCheck = True
      Caption = 'Show Close Button'
      Checked = True
    end
    object actShowFindButton: TAction
      AutoCheck = True
      Caption = 'Show Find Button'
      Checked = True
    end
    object actHighlightSearchResults: TAction
      AutoCheck = True
      Caption = 'Highlight Search Results'
      Checked = True
    end
    object actUseDelayedSearch: TAction
      AutoCheck = True
      Caption = 'Use Delayed Search'
      Checked = True
    end
    object actUseExtendedSyntax: TAction
      AutoCheck = True
      Caption = 'Use Extended Syntax'
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

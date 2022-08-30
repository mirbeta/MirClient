inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Find Panel Demo'
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
      '  This demo illustrates the search capabilities provided by the ' +
      'built-in Find Panel. To execute a search, simply enter text with' +
      'in the Find Panel'#39's box and the grid will display those records ' +
      'that have matching values.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 763
  end
  object cxGroupBox1: TcxGroupBox [2]
    Left = 0
    Top = 32
    Align = alTop
    TabOrder = 1
    Height = 113
    Width = 763
    object cbClearFindOnClose: TcxCheckBox
      Left = 467
      Top = 10
      Action = actClearFindOnClose
      TabOrder = 0
      Transparent = True
    end
    object cbShowClearButton: TcxCheckBox
      Left = 641
      Top = 58
      Action = actShowClearButton
      TabOrder = 1
      Transparent = True
    end
    object cbShowCloseButton: TcxCheckBox
      Left = 641
      Top = 10
      Action = actShowCloseButton
      TabOrder = 2
      Transparent = True
    end
    object cbShowFindButton: TcxCheckBox
      Left = 641
      Top = 34
      Action = actShowFindButton
      TabOrder = 3
      Transparent = True
    end
    object cbHighlightSearchResults: TcxCheckBox
      Left = 467
      Top = 34
      Action = actHighlightSearchResults
      TabOrder = 4
      Transparent = True
    end
    object seFindDelay: TcxSpinEdit
      Left = 120
      Top = 58
      Properties.MaxValue = 5000.000000000000000000
      Properties.MinValue = 100.000000000000000000
      Properties.OnChange = seFindDelayPropertiesChange
      TabOrder = 5
      Value = 1000
      Width = 334
    end
    object lbSearchDelay: TcxLabel
      Left = 12
      Top = 60
      Caption = 'Search Delay, ms:'
      Transparent = True
    end
    object icbFindFilterColumns: TcxImageComboBox
      Left = 118
      Top = 82
      Properties.Items = <>
      Properties.OnChange = icbFindFilterColumnsPropertiesChange
      TabOrder = 7
      Width = 336
    end
    object lbSearchableColumns: TcxLabel
      Left = 12
      Top = 84
      Caption = 'Searchable Columns:'
      Transparent = True
    end
    object cbeFindPanelPosition: TcxComboBox
      Left = 120
      Top = 34
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Top'
        'Bottom')
      Properties.OnChange = cbFindPanelPositionPropertiesChange
      TabOrder = 9
      Text = 'Top'
      Width = 334
    end
    object lbFindPanelPosition: TcxLabel
      Left = 12
      Top = 36
      Caption = 'Find Panel Position:'
      Transparent = True
    end
    object cbeDisplayMode: TcxComboBox
      Left = 120
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Never'
        'Manual (Focus the grid and press Ctrl+F)'
        'Always')
      Properties.OnChange = cbDisplayModePropertiesChange
      TabOrder = 11
      Text = 'Manual (Focus the grid and press Ctrl+F)'
      Width = 334
    end
    object lbDisplayMode: TcxLabel
      Left = 12
      Top = 12
      Caption = 'Display Mode:'
      Transparent = True
    end
    object cbUseDelayedSearch: TcxCheckBox
      Left = 467
      Top = 58
      Action = actUseDelayedSearch
      TabOrder = 13
      Transparent = True
    end
    object cbUseExtendedSyntax: TcxCheckBox
      Left = 467
      Top = 82
      Action = actUseExtendedSyntax
      TabOrder = 14
      Transparent = True
    end
  end
  object Grid: TcxGrid [3]
    Left = 0
    Top = 145
    Width = 763
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
      FindPanel.DisplayMode = fpdmManual
      DataController.DataSource = dsOrders
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      object TableViewCompanyName: TcxGridDBColumn
        DataBinding.FieldName = 'Company Name'
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
    object miFindPanelOptions: TMenuItem [1]
      Caption = 'Find Panel Options'
      object ClearFindOnClose1: TMenuItem
        Action = actClearFindOnClose
        AutoCheck = True
      end
      object HighlightFindResult1: TMenuItem
        Action = actHighlightSearchResults
        AutoCheck = True
      end
      object miVisibleButtons: TMenuItem
        Caption = 'Button Visibility'
        object ShowClearButton2: TMenuItem
          Action = actShowClearButton
          AutoCheck = True
        end
        object ShowCloseButton2: TMenuItem
          Action = actShowCloseButton
          AutoCheck = True
        end
        object ShowFindButton2: TMenuItem
          Action = actShowFindButton
          AutoCheck = True
        end
      end
      object UseDelayedFind1: TMenuItem
        Action = actUseDelayedSearch
        AutoCheck = True
      end
      object UseExtendedSyntax1: TMenuItem
        Action = actUseExtendedSyntax
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
    object actClearFindOnClose: TAction
      AutoCheck = True
      Caption = 'Clear Find Filter Text On Close'
      Checked = True
      OnExecute = actClearFindOnCloseChange
    end
    object actShowClearButton: TAction
      AutoCheck = True
      Caption = 'Show Clear Button'
      Checked = True
      OnExecute = actShowClearButtonChange
    end
    object actShowCloseButton: TAction
      AutoCheck = True
      Caption = 'Show Close Button'
      Checked = True
      OnExecute = actShowCloseButtonChange
    end
    object actShowFindButton: TAction
      AutoCheck = True
      Caption = 'Show Find Button'
      Checked = True
      OnExecute = actShowFindButtonEChange
    end
    object actHighlightSearchResults: TAction
      AutoCheck = True
      Caption = 'Highlight Search Results'
      Checked = True
      OnExecute = actHighlightFindResultChange
    end
    object actUseDelayedSearch: TAction
      AutoCheck = True
      Caption = 'Use Delayed Search'
      Checked = True
      OnExecute = actUseDelayedSearchExecute
    end
    object actUseExtendedSyntax: TAction
      AutoCheck = True
      Caption = 'Use Extended Syntax'
      OnExecute = actUseExtendedSyntaxExecute
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

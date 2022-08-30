inherited frmMain: TfrmMain
  Left = 401
  Top = 185
  Caption = 'ExpressQuantumGrid Web-Style Row Selection Demo'
  ClientHeight = 551
  ClientWidth = 793
  Constraints.MinHeight = 600
  Constraints.MinWidth = 770
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 793
    Height = 48
    Caption = 
      'This demo shows an alternative approach to row selection within ' +
      'the grid. Check boxes, and the selected state synchronization be' +
      'tween group rows and their child rows, allow selecting rows with' +
      ' fewer mouse clicks. Try different selection and check box optio' +
      'ns while selecting rows using mouse clicks and Shift/Ctrl key co' +
      'mbinations.'
  end
  inherited sbMain: TStatusBar
    Top = 532
    Width = 793
  end
  object cxGroupBox1: TcxGroupBox [2]
    Left = 0
    Top = 48
    Align = alTop
    TabOrder = 1
    Height = 102
    Width = 793
    object gbCheckBoxPosition: TcxGroupBox
      Left = 130
      Top = 11
      Caption = 'Check Box Position'
      TabOrder = 0
      Height = 81
      Width = 122
      object rbFirstColumn: TcxRadioButton
        Left = 5
        Top = 19
        Width = 113
        Height = 17
        Caption = 'First Column'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = CheckBoxPositionChanged
        GroupIndex = 2
        Transparent = True
      end
      object rbIndicator: TcxRadioButton
        Left = 5
        Top = 37
        Width = 70
        Height = 17
        Caption = 'Indicator'
        TabOrder = 1
        OnClick = CheckBoxPositionChanged
        GroupIndex = 2
        Transparent = True
      end
    end
    object cbShowCheckBoxesDynamically: TcxCheckBox
      Left = 254
      Top = 50
      Caption = 'Show Check Boxes Dynamically'
      Properties.OnEditValueChanged = cbShowCheckBoxesDynamicallyChanged
      TabOrder = 1
      Transparent = True
      Width = 173
    end
    object cbClearSelectionOnClickOutsideSelection: TcxCheckBox
      Left = 254
      Top = 32
      Caption = 'Clear Selection With a Click Outside'
      Properties.OnEditValueChanged = cbClearSelectionWithAClickOutsideChanged
      TabOrder = 2
      Transparent = True
      Width = 193
    end
    object cxGroupBox2: TcxGroupBox
      Left = 9
      Top = 11
      Caption = 'Check Box Visibility'
      TabOrder = 3
      Height = 81
      Width = 115
      object cbDataRowCheckBoxVisible: TcxCheckBox
        Left = 4
        Top = 15
        Caption = 'Data Rows'
        Properties.OnEditValueChanged = cbDataRowCheckBoxVisibilityChanged
        State = cbsChecked
        TabOrder = 0
        Transparent = True
        Width = 76
      end
      object cbGroupRowCheckBoxVisible: TcxCheckBox
        Left = 4
        Top = 33
        Caption = 'Group Rows'
        Properties.OnEditValueChanged = cbGroupRowCheckBoxVisibilityChanged
        State = cbsChecked
        TabOrder = 1
        Transparent = True
        Width = 82
      end
      object cbColumnHeaderCheckBoxSelectorVisible: TcxCheckBox
        Left = 4
        Top = 51
        Caption = 'Column Header'
        Properties.OnEditValueChanged = cbColumnHeaderCheckBoxVisibilityChanged
        State = cbsChecked
        TabOrder = 2
        Transparent = True
        Width = 97
      end
    end
    object cbPersistentSelection: TcxCheckBox
      Left = 254
      Top = 14
      Caption = 'Persistent Selection'
      Properties.OnEditValueChanged = cbPersistentSelectionPropertiesEditValueChanged
      State = cbsChecked
      TabOrder = 4
      Transparent = True
      Width = 118
    end
  end
  object Grid: TcxGrid [3]
    Left = 0
    Top = 150
    Width = 793
    Height = 382
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
      DataController.Options = [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoMultiSelectionSyncGroupWithChildren]
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
      DataController.Summary.Options = [soSelectedRecords]
      OptionsSelection.MultiSelect = True
      OptionsSelection.CheckBoxVisibility = [cbvDataRow, cbvGroupRow, cbvColumnHeader]
      OptionsSelection.MultiSelectMode = msmPersistent
      OptionsView.ColumnAutoWidth = True
      OptionsView.Footer = True
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

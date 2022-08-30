inherited FilterByCodeDemoMainForm: TFilterByCodeDemoMainForm
  Left = 115
  Top = 197
  Caption = 'ExpressQuantumGrid FilterByCode Demo'
  ClientHeight = 487
  ClientWidth = 835
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 835
    Height = 32
    Caption = 
      'This demo shows grid filtering. Try to change the combo box belo' +
      'w and column headers dropdown. Click '#39'About this demo'#39' for more ' +
      'information.'
  end
  inherited sbMain: TStatusBar
    Top = 468
    Width = 835
  end
  object cxGrid: TcxGrid [2]
    Left = 0
    Top = 73
    Width = 835
    Height = 395
    Align = alClient
    TabOrder = 1
    object tvCustomers: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = FilterByCodeDemoMainDM.dsCustomers
      DataController.Filter.OnChanged = tvCustomersDataControllerFilterChanged
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.GroupByBox = False
      Styles.StyleSheet = GridTableViewStyleSheetDevExpress
      object tvCustomersFIRSTNAME: TcxGridDBColumn
        Caption = 'First Name'
        DataBinding.FieldName = 'FIRSTNAME'
        Width = 76
      end
      object tvCustomersLASTNAME: TcxGridDBColumn
        Caption = 'Last Name'
        DataBinding.FieldName = 'LASTNAME'
        Width = 86
      end
      object tvCustomersCOMPANYNAME: TcxGridDBColumn
        Caption = 'Company'
        DataBinding.FieldName = 'COMPANYNAME'
        Width = 146
      end
      object tvCustomersPAYMENTTYPE: TcxGridDBColumn
        Caption = 'Payment Type'
        DataBinding.FieldName = 'PAYMENTTYPE'
        RepositoryItem = FilterByCodeDemoMainDM.edrepDXPaymentTypeImageCombo
        Width = 119
      end
      object tvCustomersPRODUCTID: TcxGridDBColumn
        Caption = 'Product'
        DataBinding.FieldName = 'PRODUCTID'
        RepositoryItem = FilterByCodeDemoMainDM.edrepDXLookupProducts
        OnGetFilterValues = tvCustomersPRODUCTIDGetFilterValues
        OnUserFiltering = tvCustomersPRODUCTIDUserFiltering
        Width = 120
      end
      object tvCustomersCUSTOMER: TcxGridDBColumn
        Caption = 'Customer'
        DataBinding.FieldName = 'CUSTOMER'
        Width = 65
      end
      object tvCustomersPURCHASEDATE: TcxGridDBColumn
        Caption = 'Purchase Date'
        DataBinding.FieldName = 'PURCHASEDATE'
        PropertiesClassName = 'TcxDateEditProperties'
        Width = 88
      end
      object tvCustomersPAYMENTAMOUNT: TcxGridDBColumn
        Caption = 'Payment Amount'
        DataBinding.FieldName = 'PAYMENTAMOUNT'
        Width = 75
      end
      object tvCustomersCOPIES: TcxGridDBColumn
        Caption = 'Copies'
        DataBinding.FieldName = 'COPIES'
        PropertiesClassName = 'TcxSpinEditProperties'
        Properties.MaxValue = 100.000000000000000000
        Width = 66
      end
    end
    object lvCustomers: TcxGridLevel
      GridView = tvCustomers
    end
  end
  object Panel1: TPanel [3]
    Left = 0
    Top = 32
    Width = 835
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Color = 4707838
    TabOrder = 2
    object Label1: TcxLabel
      Left = 8
      Top = 14
      Caption = 'Current Active Filter'
      Transparent = True
    end
    object cbFilters: TcxComboBox
      Left = 112
      Top = 10
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsEditFixedList
      Properties.OnChange = cbFiltersChange
      TabOrder = 0
      Width = 385
    end
  end
  inherited mmMain: TMainMenu
    Left = 640
    Top = 40
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miFilterBoxPosition: TMenuItem
        Caption = 'FilterBox &position'
        object miFilterBoxPosTop: TMenuItem
          Caption = 'Top'
          Hint = 'The FilterBox is moved to the top of the GridView'
          RadioItem = True
          OnClick = miFilterBoxPosClick
        end
        object miFilterBoxPosBottom: TMenuItem
          Tag = 1
          Caption = 'Bottom'
          Checked = True
          Hint = 'The FilterBox is moved to the bottom of the GridView'
          RadioItem = True
          OnClick = miFilterBoxPosClick
        end
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
end

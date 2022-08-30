inherited frmCompactLayout: TfrmCompactLayout
  Left = 32
  Top = 83
  Caption = 'PivotGrid - Compact Layout Demo'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Caption = 
      'This demo demonstrates a new View option - Compact Layout. In th' +
      'is View, all row fields in the Row Area are stacked in one colum' +
      'n.'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 8
    Top = 32
    Width = 799
    Height = 512
    Customization.AvailableFieldsSorted = True
    Customization.FormStyle = cfsAdvanced
    Align = alClient
    DataSource = dmOrders.dsOrders
    GroupHeaderImages = dmOrders.PaymentTypeImages
    Groups = <>
    OptionsView.ColumnFields = False
    OptionsView.DataFields = False
    OptionsView.FilterFields = False
    OptionsView.FilterSeparator = False
    OptionsView.RowFields = False
    OptionsView.RowTotalsLocation = rtlTree
    OptionsView.TotalsForSingleValues = True
    TabOrder = 0
    OnCustomization = DBPivotGridCustomization
    object pgfPurchaseDate: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Purchase Date'
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateMonth
      Visible = True
    end
    object pgfPaymentType: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Type'
      DataBinding.FieldName = 'PaymentType'
      Visible = True
      OnGetGroupImageIndex = pgfPaymentTypeGetGroupImageIndex
    end
    object pgfQuantity: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      DataBinding.FieldName = 'Quantity'
      DisplayFormat = '0'
      Visible = True
      Width = 49
    end
    object pgfCarName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 2
      DataBinding.FieldName = 'Car Name'
      Visible = True
      Width = 77
    end
    object pgfUnitPrice: TcxDBPivotGridField
      AreaIndex = 0
      DataBinding.FieldName = 'Unit Price'
      Visible = True
    end
    object pgfCompanyName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      DataBinding.FieldName = 'Company Name'
      Visible = True
      Width = 131
    end
    object pgfPaymentAmount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Payment Amount'
      DataBinding.FieldName = 'PaymentAmount'
      Visible = True
      Width = 90
    end
  end
  object cxSplitter1: TcxSplitter [2]
    Left = 0
    Top = 32
    Width = 8
    Height = 512
    HotZoneClassName = 'TcxSimpleStyle'
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      inherited miElementsVisibility: TMenuItem
        inherited miShowColumnFields: TMenuItem
          Caption = 'Show Column Fields'
        end
        inherited miShowDataFields: TMenuItem
          Caption = 'Show Data Fields'
        end
        inherited miShowFilterFields: TMenuItem
          Caption = 'Show Filter Fields'
        end
        inherited miShowFilterSeparator: TMenuItem
          Caption = 'Show Filter Separator'
        end
        inherited miShowRowFields: TMenuItem
          Caption = 'Show Row Fields'
        end
      end
      object miCustomization: TMenuItem
        AutoCheck = True
        Caption = 'Customization'
        ShortCut = 113
        OnClick = miCustomizationClick
      end
    end
  end
end

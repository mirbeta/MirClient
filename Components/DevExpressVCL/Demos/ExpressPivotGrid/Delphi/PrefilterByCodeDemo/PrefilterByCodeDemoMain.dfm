inherited fmPrefilterByCode: TfmPrefilterByCode
  Left = 94
  Top = 135
  Caption = 'PivotGrid - PrefilterByCode'
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Height = 16
    Caption = 
      'This demo shows pivot grid prefiltering. Select prefilter criter' +
      'ia in the combo box below, to apply it to the pivot grid.'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 0
    Top = 80
    Width = 807
    Height = 471
    Align = alClient
    DataSource = dmOrders.dsOrders
    GroupHeaderImages = dmOrders.PaymentTypeImages
    Groups = <>
    TabOrder = 0
    OnFilterChanged = DBPivotGridFilterChanged
    object pgfPurchaseDate: TcxDBPivotGridField
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Purchase Date'
      DataBinding.FieldName = 'PurchaseDate'
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
      AreaIndex = 1
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
  object Panel1: TPanel [2]
    Left = 0
    Top = 16
    Width = 807
    Height = 64
    Align = alTop
    BevelOuter = bvNone
    Color = 4707838
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 108
      Height = 13
      Caption = 'Current Active Prefilter:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object cbFilters: TComboBox
      Left = 128
      Top = 21
      Width = 497
      Height = 19
      Style = csOwnerDrawFixed
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
      OnChange = cbFiltersChange
    end
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
      object PrefilterPosition1: TMenuItem
        Caption = 'Prefilter Position'
        object op1: TMenuItem
          AutoCheck = True
          Caption = 'Top'
          RadioItem = True
          OnClick = Bottom1Click
        end
        object Bottom1: TMenuItem
          Tag = 1
          AutoCheck = True
          Caption = 'Bottom'
          Checked = True
          RadioItem = True
          OnClick = Bottom1Click
        end
      end
    end
  end
end

inherited frmSortBySummary: TfrmSortBySummary
  Left = 253
  Top = 157
  Caption = 'PivotGrid - Sort By Summary'
  ClientWidth = 793
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Width = 793
    Height = 16
    Caption = 
      'This demo allows you to sort values of one field by the summarie' +
      's of another field.'
  end
  object DBPivotGrid: TcxDBPivotGrid [1]
    Left = 0
    Top = 93
    Width = 793
    Height = 451
    Align = alClient
    DataSource = dmOrders.dsOrders
    Groups = <>
    Styles.OnGetColumnHeaderStyle = GetGroupHeaderStyle
    Styles.OnGetRowHeaderStyle = GetGroupHeaderStyle
    TabOrder = 0
    OnLayoutChanged = DBPivotGridLayoutChanged
    object pgfPurchaseQuarter: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 1
      IsCaptionAssigned = True
      Caption = 'Purchase Quarter'
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateQuarter
      Visible = True
      UniqueName = 'Purchase Quarter'
    end
    object pgfPaymentType: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Type'
      DataBinding.FieldName = 'PaymentType'
      Visible = True
      UniqueName = 'Payment Type'
    end
    object pgfQuantity: TcxDBPivotGridField
      Area = faData
      AreaIndex = 1
      DataBinding.FieldName = 'Quantity'
      Visible = True
      UniqueName = 'Quantity'
    end
    object pgfCarName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 1
      DataBinding.FieldName = 'Car Name'
      SummaryType = stCount
      SortOrder = soDescending
      Visible = True
      Width = 111
      UniqueName = 'Car Name'
    end
    object pgfUnitPrice: TcxDBPivotGridField
      AreaIndex = 0
      DataBinding.FieldName = 'Unit Price'
      Visible = True
      UniqueName = 'Unit Price'
    end
    object pgfCompanyName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      DataBinding.FieldName = 'Company Name'
      SortBySummaryInfo.Field = pgfPaymentAmount
      SortOrder = soDescending
      TopValueCount = 3
      TopValueShowOthers = True
      Visible = True
      Width = 142
      UniqueName = 'Company Name'
    end
    object pgfPaymentAmount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Amount'
      DataBinding.FieldName = 'PaymentAmount'
      Visible = True
      UniqueName = 'Payment Amount'
    end
  end
  object pnSettings: TPanel [2]
    Left = 0
    Top = 16
    Width = 793
    Height = 77
    Align = alTop
    TabOrder = 1
    object lbSortThe: TLabel
      Left = 16
      Top = 15
      Width = 37
      Height = 13
      Caption = 'Sort the'
      FocusControl = cbxSortField
      Transparent = True
    end
    object lbSortBy: TLabel
      Left = 198
      Top = 15
      Width = 33
      Height = 13
      Caption = 'field by'
      FocusControl = cbxSortByField
      Transparent = True
    end
    object lbShowTop: TLabel
      Left = 16
      Top = 50
      Width = 45
      Height = 13
      Caption = 'Show top'
      FocusControl = speTopCount
      Transparent = True
    end
    object lbValues: TLabel
      Left = 136
      Top = 50
      Width = 31
      Height = 13
      Caption = 'values'
      Transparent = True
    end
    object bvSplitter: TBevel
      Left = 16
      Top = 39
      Width = 369
      Height = 9
      Shape = bsTopLine
    end
    object cbxSortField: TcxComboBox
      Left = 62
      Top = 11
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Car Name'
        'Company Name'
        'Payment Type'
        'Purchase Quarter')
      Properties.OnChange = SortFieldChanged
      TabOrder = 0
      Text = 'Company Name'
      Width = 127
    end
    object cbxSortByField: TcxComboBox
      Left = 241
      Top = 11
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'None'
        'Payment Amount'
        'Quantity')
      Properties.OnChange = SortByChanged
      TabOrder = 1
      Text = 'Payment Amount'
      Width = 136
    end
    object speTopCount: TcxSpinEdit
      Left = 72
      Top = 47
      Properties.AssignedValues.MinValue = True
      Properties.OnChange = speTopCountPropertiesChange
      TabOrder = 2
      Width = 57
    end
    object cbxTopValuesShowOthers: TcxCheckBox
      Left = 238
      Top = 46
      Caption = 'TopValuesShowOthers'
      Properties.OnChange = cbxTopValuesShowOthersPropertiesChange
      TabOrder = 3
      Transparent = True
      Width = 145
    end
    object rgSortOrder: TcxRadioGroup
      Left = 400
      Top = 6
      Caption = 'Sort By Summary Default Order'
      Properties.Columns = 3
      Properties.Items = <
        item
          Caption = 'Default'
          Value = 0
        end
        item
          Caption = 'Ascending'
          Value = 1
        end
        item
          Caption = 'Descending'
          Value = 2
        end>
      ItemIndex = 0
      Style.Edges = [bLeft, bTop, bRight, bBottom]
      TabOrder = 4
      OnClick = rgSortOrderClick
      Height = 60
      Width = 377
    end
  end
end

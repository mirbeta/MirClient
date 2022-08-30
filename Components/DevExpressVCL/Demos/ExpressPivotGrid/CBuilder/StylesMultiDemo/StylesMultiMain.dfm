inherited frmStylesMulti: TfrmStylesMulti
  Left = 253
  Top = 140
  Caption = 'PivotGrid - StylesMulti Demo'
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescrip: TLabel
    Height = 16
    Caption = 
      'This demo allows you to apply styles to the PivotGrid that are s' +
      'tored in the style repository.'
  end
  object Splitter: TSplitter [1]
    Left = 177
    Top = 16
    Width = 2
    Height = 527
    MinSize = 4
  end
  object DBPivotGrid: TcxDBPivotGrid [2]
    Left = 179
    Top = 16
    Width = 628
    Height = 527
    Align = alClient
    DataSource = dmOrders.dsOrders
    Groups = <>
    OptionsPrefilter.Visible = pfvAlways
    OptionsSelection.MultiSelect = True
    TabOrder = 0
    TabStop = True
    object PivotGridPurchaseDate: TcxDBPivotGridField
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Purchase Quarter'
      DataBinding.FieldName = 'PurchaseDate'
      GroupInterval = giDateQuarter
      Visible = True
    end
    object PivotGridPaymentType: TcxDBPivotGridField
      Area = faColumn
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Type'
      DataBinding.FieldName = 'PaymentType'
      Visible = True
    end
    object PivotGridQuantity: TcxDBPivotGridField
      Area = faData
      AreaIndex = 1
      DataBinding.FieldName = 'Quantity'
      Visible = True
    end
    object PivotGridCarName: TcxDBPivotGridField
      AreaIndex = 2
      DataBinding.FieldName = 'Car Name'
      Visible = True
      Width = 156
    end
    object PivotGridUnitPrice: TcxDBPivotGridField
      AreaIndex = 1
      DataBinding.FieldName = 'Unit Price'
      Visible = True
    end
    object PivotGridCompanyName: TcxDBPivotGridField
      Area = faRow
      AreaIndex = 0
      DataBinding.FieldName = 'Company Name'
      Visible = True
      Width = 142
    end
    object PivotGridPaymentAmount: TcxDBPivotGridField
      Area = faData
      AreaIndex = 0
      IsCaptionAssigned = True
      Caption = 'Payment Amount'
      DataBinding.FieldName = 'PaymentAmount'
      Visible = True
      Width = 101
    end
  end
  object pnlLeft: TPanel [3]
    Left = 0
    Top = 16
    Width = 177
    Height = 527
    Align = alLeft
    Anchors = [akLeft]
    BevelInner = bvLowered
    BevelOuter = bvNone
    Color = 15451300
    TabOrder = 1
    object gbUserDefined: TcxGroupBox
      Left = 1
      Top = 370
      TabStop = True
      Align = alBottom
      Caption = 'User Defined Style Sheets'
      Enabled = False
      TabOrder = 3
      Height = 156
      Width = 175
      object cbUserStyleSheets: TComboBox
        Left = 10
        Top = 31
        Width = 154
        Height = 19
        Style = csOwnerDrawFixed
        Anchors = [akLeft, akTop, akRight]
        Color = 16247513
        Enabled = False
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbUserStyleSheetsChange
      end
      object btnLoad: TcxButton
        Left = 11
        Top = 89
        Width = 153
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = '&LoadFromFile...'
        Enabled = False
        TabOrder = 2
        OnClick = btnLoadClick
      end
      object btnSave: TcxButton
        Left = 11
        Top = 116
        Width = 153
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = '&SaveToFile...'
        Enabled = False
        TabOrder = 3
        OnClick = btnSaveClick
      end
      object btnEdit: TcxButton
        Left = 11
        Top = 62
        Width = 153
        Height = 25
        Anchors = [akLeft, akTop, akRight]
        Caption = '&Edit Style Sheet'
        Enabled = False
        TabOrder = 1
        OnClick = btnEditClick
      end
    end
    object RadioGroup: TcxRadioGroup
      Left = 1
      Top = 36
      Align = alTop
      Caption = 'Use styles'
      ItemIndex = 1
      Properties.Items = <
        item
          Caption = 'None'
        end
        item
          Caption = 'Predefined'
        end
        item
          Caption = 'User defined'
        end>
      TabOrder = 1
      OnClick = RadioGroupClick
      Height = 88
      Width = 175
    end
    object gbPredefined: TcxGroupBox
      Left = 1
      Top = 124
      TabStop = True
      Align = alClient
      Caption = 'Predefined Style Sheets'
      TabOrder = 2
      Height = 246
      Width = 175
      object lbPredefinedStyleSheets: TcxListBox
        Left = 2
        Top = 16
        Width = 171
        Height = 228
        Align = alClient
        ItemHeight = 13
        Style.Color = 16247513
        TabOrder = 0
        OnClick = lbPredefinedStyleSheetsClick
      end
    end
    object pnlCurrentStyleSheet: TPanel
      Left = 1
      Top = 1
      Width = 175
      Height = 35
      Align = alTop
      BevelOuter = bvLowered
      Color = 15451300
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
  end
  object srPredefined: TcxStyleRepository
    Left = 696
    Top = 80
    object ClassicBackground: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object ClassicContent: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object ClassicHeader: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object ClassicRowHeader: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object ClassicInactive: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object ClassicSelection: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 12937777
      TextColor = clWhite
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 8894686
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial Narrow'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clNavy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Narrow'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15136253
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 7346457
      Font.Height = -12
      Font.Name = 'Arial Narrow'
      Font.Style = []
      TextColor = 7346457
    end
    object cxStyle10: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clNavy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial Narrow'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle13: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 9157775
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle14: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4615972
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle17: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle21: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 5736750
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle24: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13749760
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle25: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 166
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle28: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle32: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 191
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle35: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle36: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 85
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle39: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle43: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 85
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle46: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 10862530
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle47: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6392205
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle50: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle54: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6392205
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle57: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12307669
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle58: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle61: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle65: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle68: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 11055248
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle69: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 7897176
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle72: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle76: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 7897176
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle79: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14264494
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Garamond'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle80: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 11619930
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle83: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Garamond'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle87: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 11619930
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle90: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 11458790
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle91: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4630214
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle94: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle98: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4630214
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle101: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12107912
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle102: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6053956
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle105: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14213320
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle109: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6053956
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle112: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 9476264
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle113: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4210816
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = 13160664
    end
    object cxStyle116: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13160664
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle120: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4210816
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = 13160664
    end
    object cxStyle123: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 10343916
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle124: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4944971
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle127: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle131: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4944971
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle134: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 11639171
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle135: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 8217935
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle138: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle142: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 8217935
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle145: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle146: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clMaroon
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Times New Roman'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle149: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Times New Roman'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle153: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clMaroon
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -15
      Font.Name = 'Times New Roman'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle156: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13158655
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle157: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 5855675
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle160: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15461375
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle164: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 5855675
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle167: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13158655
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle168: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 5855675
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle171: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15461375
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle175: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 5855675
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle178: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13154717
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle179: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 9928789
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle182: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle186: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 9928789
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle189: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 11126946
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle190: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6592345
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle193: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle197: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6592345
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle200: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle201: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4194432
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle204: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle208: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 4194432
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle211: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle212: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle215: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle219: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle222: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 10542814
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle223: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clOlive
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle226: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle230: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clOlive
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle233: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle234: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clNavy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle237: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle241: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clNavy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle244: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle245: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clNavy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle248: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle252: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clNavy
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle255: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13160660
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle256: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6956042
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle259: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle263: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6956042
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle266: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13160660
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle267: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6956042
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle270: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle274: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 6956042
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle277: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle278: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 206
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle281: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clYellow
    end
    object cxStyle285: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 206
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle286: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle288: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle289: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 206
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle292: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clYellow
    end
    object cxStyle296: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 206
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle297: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle299: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clYellow
    end
    object cxStyle300: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle303: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle307: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle308: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle310: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clYellow
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clYellow
    end
    object cxStyle311: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle314: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle318: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clTeal
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle319: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle321: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle322: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clPurple
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle325: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle329: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clPurple
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle330: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle332: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle333: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clPurple
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle336: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle340: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clPurple
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle341: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle343: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle344: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle347: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle351: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle354: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle355: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle358: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle362: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object PivotGridStyleSheetDevExpress: TcxPivotGridStyleSheet
      Caption = 'DevExpress'
      Styles.Background = ClassicContent
      Styles.ColumnHeader = ClassicHeader
      Styles.Content = ClassicContent
      Styles.FieldHeader = ClassicHeader
      Styles.HeaderBackground = ClassicBackground
      Styles.Inactive = ClassicInactive
      Styles.Prefilter = ClassicBackground
      Styles.RowHeader = ClassicHeader
      Styles.Selected = ClassicSelection
      BuiltIn = True
    end
    object PivotGridStyleSheetUserFormat1: TcxPivotGridStyleSheet
      Caption = 'UserFormat1'
      Styles.Background = cxStyle6
      Styles.ColumnHeader = cxStyle2
      Styles.Content = cxStyle6
      Styles.FieldHeader = cxStyle2
      Styles.Inactive = cxStyle10
      Styles.RowHeader = cxStyle2
      Styles.Selected = cxStyle3
      BuiltIn = True
    end
    object PivotGridStyleSheetUserFormat2: TcxPivotGridStyleSheet
      Caption = 'UserFormat2'
      Styles.Background = cxStyle17
      Styles.ColumnHeader = cxStyle13
      Styles.Content = cxStyle17
      Styles.FieldHeader = cxStyle13
      Styles.Inactive = cxStyle21
      Styles.RowHeader = cxStyle13
      Styles.Selected = cxStyle14
      BuiltIn = True
    end
    object PivotGridStyleSheetUserFormat3: TcxPivotGridStyleSheet
      Caption = 'UserFormat3'
      Styles.Background = cxStyle28
      Styles.ColumnHeader = cxStyle24
      Styles.Content = cxStyle28
      Styles.FieldHeader = cxStyle24
      Styles.Inactive = cxStyle32
      Styles.RowHeader = cxStyle24
      Styles.Selected = cxStyle25
      BuiltIn = True
    end
    object PivotGridStyleSheetUserFormat4: TcxPivotGridStyleSheet
      Caption = 'UserFormat4'
      Styles.Background = cxStyle39
      Styles.ColumnHeader = cxStyle35
      Styles.Content = cxStyle39
      Styles.FieldHeader = cxStyle35
      Styles.Inactive = cxStyle43
      Styles.RowHeader = cxStyle35
      Styles.Selected = cxStyle36
      BuiltIn = True
    end
    object PivotGridStyleSheetBrick: TcxPivotGridStyleSheet
      Caption = 'Brick'
      Styles.Background = cxStyle50
      Styles.ColumnHeader = cxStyle46
      Styles.Content = cxStyle50
      Styles.FieldHeader = cxStyle46
      Styles.Inactive = cxStyle54
      Styles.RowHeader = cxStyle46
      Styles.Selected = cxStyle47
      BuiltIn = True
    end
    object PivotGridStyleSheetDesert: TcxPivotGridStyleSheet
      Caption = 'Desert'
      Styles.Background = cxStyle61
      Styles.ColumnHeader = cxStyle57
      Styles.Content = cxStyle61
      Styles.FieldHeader = cxStyle57
      Styles.Inactive = cxStyle65
      Styles.RowHeader = cxStyle57
      Styles.Selected = cxStyle58
      BuiltIn = True
    end
    object PivotGridStyleSheetEggplant: TcxPivotGridStyleSheet
      Caption = 'Eggplant'
      Styles.Background = cxStyle72
      Styles.ColumnHeader = cxStyle68
      Styles.Content = cxStyle72
      Styles.HeaderBackground = cxStyle68
      Styles.Inactive = cxStyle76
      Styles.Prefilter = cxStyle68
      Styles.RowHeader = cxStyle68
      Styles.Selected = cxStyle69
      BuiltIn = True
    end
    object PivotGridStyleSheetLilac: TcxPivotGridStyleSheet
      Caption = 'Lilac'
      Styles.Background = cxStyle83
      Styles.ColumnHeader = cxStyle79
      Styles.Content = cxStyle83
      Styles.FieldHeader = cxStyle79
      Styles.Inactive = cxStyle87
      Styles.RowHeader = cxStyle79
      Styles.Selected = cxStyle80
      BuiltIn = True
    end
    object PivotGridStyleSheetMaple: TcxPivotGridStyleSheet
      Caption = 'Maple'
      Styles.Background = cxStyle94
      Styles.ColumnHeader = cxStyle90
      Styles.Content = cxStyle94
      Styles.FieldHeader = cxStyle90
      Styles.Inactive = cxStyle98
      Styles.RowHeader = cxStyle90
      Styles.Selected = cxStyle91
      BuiltIn = True
    end
    object PivotGridStyleSheetMarinehighcolor: TcxPivotGridStyleSheet
      Caption = 'Marine (high color)'
      Styles.Background = cxStyle105
      Styles.ColumnHeader = cxStyle101
      Styles.Content = cxStyle105
      Styles.FieldHeader = cxStyle101
      Styles.Inactive = cxStyle109
      Styles.RowHeader = cxStyle101
      Styles.Selected = cxStyle102
      BuiltIn = True
    end
    object PivotGridStyleSheetPlumhighcolor: TcxPivotGridStyleSheet
      Caption = 'Plum (high color)'
      Styles.Background = cxStyle116
      Styles.ColumnHeader = cxStyle112
      Styles.Content = cxStyle116
      Styles.FieldHeader = cxStyle112
      Styles.Inactive = cxStyle120
      Styles.RowHeader = cxStyle112
      Styles.Selected = cxStyle113
      BuiltIn = True
    end
    object PivotGridStyleSheetPumpkinlarge: TcxPivotGridStyleSheet
      Caption = 'Pumpkin (large)'
      Styles.Background = cxStyle127
      Styles.ColumnHeader = cxStyle123
      Styles.Content = cxStyle127
      Styles.FieldHeader = cxStyle123
      Styles.Inactive = cxStyle131
      Styles.RowHeader = cxStyle123
      Styles.Selected = cxStyle124
      BuiltIn = True
    end
    object PivotGridStyleSheetRainyDay: TcxPivotGridStyleSheet
      Caption = 'Rainy Day'
      Styles.Background = cxStyle138
      Styles.ColumnHeader = cxStyle134
      Styles.Content = cxStyle138
      Styles.FieldHeader = cxStyle134
      Styles.Inactive = cxStyle142
      Styles.RowHeader = cxStyle134
      Styles.Selected = cxStyle135
      BuiltIn = True
    end
    object PivotGridStyleSheetRedWhiteandBlueVGA: TcxPivotGridStyleSheet
      Caption = 'Red, White, and Blue (VGA)'
      Styles.Background = cxStyle149
      Styles.ColumnHeader = cxStyle145
      Styles.Content = cxStyle149
      Styles.FieldHeader = cxStyle145
      Styles.Inactive = cxStyle153
      Styles.RowHeader = cxStyle145
      Styles.Selected = cxStyle146
      BuiltIn = True
    end
    object PivotGridStyleSheetRose: TcxPivotGridStyleSheet
      Caption = 'Rose'
      Styles.Background = cxStyle160
      Styles.ColumnHeader = cxStyle156
      Styles.Content = cxStyle160
      Styles.FieldHeader = cxStyle156
      Styles.Inactive = cxStyle164
      Styles.RowHeader = cxStyle156
      Styles.Selected = cxStyle157
      BuiltIn = True
    end
    object PivotGridStyleSheetRoselarge: TcxPivotGridStyleSheet
      Caption = 'Rose (large)'
      Styles.Background = cxStyle171
      Styles.ColumnHeader = cxStyle167
      Styles.Content = cxStyle171
      Styles.FieldHeader = cxStyle167
      Styles.Inactive = cxStyle175
      Styles.RowHeader = cxStyle167
      Styles.Selected = cxStyle168
      BuiltIn = True
    end
    object PivotGridStyleSheetSlate: TcxPivotGridStyleSheet
      Caption = 'Slate'
      Styles.Background = cxStyle182
      Styles.ColumnHeader = cxStyle178
      Styles.Content = cxStyle182
      Styles.FieldHeader = cxStyle178
      Styles.Inactive = cxStyle186
      Styles.RowHeader = cxStyle178
      Styles.Selected = cxStyle179
      BuiltIn = True
    end
    object PivotGridStyleSheetSpruce: TcxPivotGridStyleSheet
      Caption = 'Spruce'
      Styles.Background = cxStyle193
      Styles.ColumnHeader = cxStyle189
      Styles.Content = cxStyle193
      Styles.FieldHeader = cxStyle189
      Styles.Inactive = cxStyle197
      Styles.RowHeader = cxStyle189
      Styles.Selected = cxStyle190
      BuiltIn = True
    end
    object PivotGridStyleSheetStormVGA: TcxPivotGridStyleSheet
      Caption = 'Storm (VGA)'
      Styles.Background = cxStyle204
      Styles.ColumnHeader = cxStyle200
      Styles.Content = cxStyle204
      Styles.FieldHeader = cxStyle200
      Styles.Inactive = cxStyle208
      Styles.RowHeader = cxStyle200
      Styles.Selected = cxStyle201
      BuiltIn = True
    end
    object PivotGridStyleSheetTealVGA: TcxPivotGridStyleSheet
      Caption = 'Teal (VGA)'
      Styles.Background = cxStyle215
      Styles.ColumnHeader = cxStyle211
      Styles.Content = cxStyle215
      Styles.FieldHeader = cxStyle211
      Styles.Inactive = cxStyle219
      Styles.RowHeader = cxStyle211
      Styles.Selected = cxStyle212
      BuiltIn = True
    end
    object PivotGridStyleSheetWheat: TcxPivotGridStyleSheet
      Caption = 'Wheat'
      Styles.Background = cxStyle226
      Styles.ColumnHeader = cxStyle222
      Styles.Content = cxStyle226
      Styles.FieldHeader = cxStyle222
      Styles.Inactive = cxStyle230
      Styles.RowHeader = cxStyle222
      Styles.Selected = cxStyle223
      BuiltIn = True
    end
    object PivotGridStyleSheetWindowsClassic: TcxPivotGridStyleSheet
      Caption = 'Windows Classic'
      Styles.Background = cxStyle237
      Styles.ColumnHeader = cxStyle233
      Styles.Content = cxStyle237
      Styles.FieldHeader = cxStyle233
      Styles.Inactive = cxStyle241
      Styles.RowHeader = cxStyle233
      Styles.Selected = cxStyle234
      BuiltIn = True
    end
    object PivotGridStyleSheetWindowsClassiclarge: TcxPivotGridStyleSheet
      Caption = 'Windows Classic (large)'
      Styles.Background = cxStyle248
      Styles.ColumnHeader = cxStyle244
      Styles.Content = cxStyle248
      Styles.FieldHeader = cxStyle244
      Styles.Inactive = cxStyle252
      Styles.RowHeader = cxStyle244
      Styles.Selected = cxStyle245
      BuiltIn = True
    end
    object PivotGridStyleSheetWindowsStandard: TcxPivotGridStyleSheet
      Caption = 'Windows Standard'
      Styles.Background = cxStyle259
      Styles.ColumnHeader = cxStyle255
      Styles.Content = cxStyle259
      Styles.FieldHeader = cxStyle255
      Styles.Inactive = cxStyle263
      Styles.RowHeader = cxStyle255
      Styles.Selected = cxStyle256
      BuiltIn = True
    end
    object PivotGridStyleSheetWindowsStandardlarge: TcxPivotGridStyleSheet
      Caption = 'Windows Standard (large)'
      Styles.Background = cxStyle270
      Styles.ColumnHeader = cxStyle266
      Styles.Content = cxStyle270
      Styles.FieldHeader = cxStyle266
      Styles.Inactive = cxStyle274
      Styles.RowHeader = cxStyle266
      Styles.Selected = cxStyle267
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrast1: TcxPivotGridStyleSheet
      Caption = 'High Contrast #1'
      Styles.Background = cxStyle281
      Styles.ColumnHeader = cxStyle277
      Styles.Content = cxStyle281
      Styles.FieldHeader = cxStyle286
      Styles.Inactive = cxStyle285
      Styles.RowHeader = cxStyle277
      Styles.Selected = cxStyle278
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrast1large: TcxPivotGridStyleSheet
      Caption = 'High Contrast #1 (large)'
      Styles.Background = cxStyle292
      Styles.ColumnHeader = cxStyle288
      Styles.Content = cxStyle292
      Styles.FieldHeader = cxStyle297
      Styles.Inactive = cxStyle296
      Styles.RowHeader = cxStyle288
      Styles.Selected = cxStyle289
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrast2: TcxPivotGridStyleSheet
      Caption = 'High Contrast #2'
      Styles.Background = cxStyle303
      Styles.ColumnHeader = cxStyle299
      Styles.Content = cxStyle303
      Styles.FieldHeader = cxStyle308
      Styles.Inactive = cxStyle307
      Styles.RowHeader = cxStyle299
      Styles.Selected = cxStyle300
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrast2large: TcxPivotGridStyleSheet
      Caption = 'High Contrast #2 (large)'
      Styles.Background = cxStyle314
      Styles.ColumnHeader = cxStyle310
      Styles.Content = cxStyle314
      Styles.FieldHeader = cxStyle319
      Styles.Inactive = cxStyle318
      Styles.RowHeader = cxStyle310
      Styles.Selected = cxStyle311
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrastBlack: TcxPivotGridStyleSheet
      Caption = 'High Contrast Black'
      Styles.Background = cxStyle325
      Styles.ColumnHeader = cxStyle321
      Styles.Content = cxStyle325
      Styles.FieldHeader = cxStyle330
      Styles.Inactive = cxStyle329
      Styles.RowHeader = cxStyle321
      Styles.Selected = cxStyle322
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrastBlacklarge: TcxPivotGridStyleSheet
      Caption = 'High Contrast Black (large)'
      Styles.Background = cxStyle336
      Styles.ColumnHeader = cxStyle332
      Styles.Content = cxStyle336
      Styles.FieldHeader = cxStyle341
      Styles.Inactive = cxStyle340
      Styles.RowHeader = cxStyle332
      Styles.Selected = cxStyle333
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrastWhite: TcxPivotGridStyleSheet
      Caption = 'High Contrast White'
      Styles.Background = cxStyle347
      Styles.ColumnHeader = cxStyle343
      Styles.Content = cxStyle347
      Styles.FieldHeader = cxStyle343
      Styles.Inactive = cxStyle351
      Styles.RowHeader = cxStyle343
      Styles.Selected = cxStyle344
      BuiltIn = True
    end
    object PivotGridStyleSheetHighContrastWhitelarge: TcxPivotGridStyleSheet
      Caption = 'High Contrast White (large)'
      Styles.Background = cxStyle358
      Styles.ColumnHeader = cxStyle354
      Styles.Content = cxStyle358
      Styles.FieldHeader = cxStyle354
      Styles.Inactive = cxStyle362
      Styles.RowHeader = cxStyle354
      Styles.Selected = cxStyle355
      BuiltIn = True
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofExtensionDifferent, ofEnableSizing]
    Left = 552
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.ini'
    Filter = '*.ini|*.ini'
    Left = 584
  end
  object srUserDefined: TcxStyleRepository
    Left = 696
    Top = 120
    object cxStyle378: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle379: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16247513
      TextColor = clBlack
    end
    object cxStyle380: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16247513
      TextColor = clBlack
    end
    object cxStyle381: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16247513
      TextColor = clBlack
    end
    object cxStyle382: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clBlack
    end
    object cxStyle383: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle384: TcxStyle
      AssignedValues = [svColor]
      Color = 14872561
    end
    object cxStyle385: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle386: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle387: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle388: TcxStyle
      AssignedValues = [svColor]
      Color = 8453888
    end
    object cxStyle389: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle390: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16777088
      TextColor = clBlue
    end
    object cxStyle391: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 167498850
      TextColor = clWhite
    end
    object cxStyle392: TcxStyle
      AssignedValues = [svColor]
      Color = clGray
    end
    object cxStyle393: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = clWhite
      TextColor = clNavy
    end
    object cxStyle394: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16711808
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle395: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16751515
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle396: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
    object cxStyle397: TcxStyle
      AssignedValues = [svColor, svFont]
      Color = 14079702
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Arial'
      Font.Style = [fsBold, fsItalic]
    end
    object cxStyle398: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 15982987
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsItalic]
      TextColor = clBlue
    end
    object cxStyle399: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clMaroon
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle400: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = clRed
      TextColor = clWhite
    end
    object cxStyle401: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16711808
      TextColor = clWhite
    end
    object cxStyle402: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16711808
      TextColor = clYellow
    end
    object cxStyle403: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16751515
      TextColor = clRed
    end
    object cxStyle404: TcxStyle
      AssignedValues = [svColor]
      Color = 13290932
    end
    object cxStyle405: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = clBlack
      TextColor = clWhite
    end
    object ClassicPrefilter: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object ClassicFieldHeader: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object ClassicHeaderBackground: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle1: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16711808
      TextColor = clWhite
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 166
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -12
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxPivotGridStyleSheet1: TcxPivotGridStyleSheet
      Caption = 'Classic'
      Styles.Background = ClassicBackground
      Styles.ColumnHeader = ClassicHeader
      Styles.Content = ClassicContent
      Styles.FieldHeader = ClassicFieldHeader
      Styles.HeaderBackground = ClassicHeaderBackground
      Styles.Inactive = ClassicInactive
      Styles.Prefilter = ClassicPrefilter
      Styles.RowHeader = ClassicRowHeader
      Styles.Total = cxStyle10
      Styles.Selected = ClassicSelection
      BuiltIn = True
    end
    object cxPivotGridStyleSheet2: TcxPivotGridStyleSheet
      Caption = 'Alternative'
      Styles.Background = cxStyle392
      Styles.ColumnHeader = cxStyle1
      Styles.Content = cxStyle390
      Styles.FieldHeader = cxStyle400
      Styles.HeaderBackground = cxStyle25
      Styles.Inactive = cxStyle405
      Styles.Prefilter = cxStyle4
      Styles.RowHeader = cxStyle401
      Styles.Total = cxStyle299
      Styles.Selected = cxStyle385
      BuiltIn = True
    end
  end
end

object FmeEdits: TFmeEdits
  Left = 0
  Top = 0
  Width = 695
  Height = 359
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  TabStop = True
  object GrpEdits: TRzGroupBox
    Left = 8
    Top = 264
    Width = 397
    Height = 77
    Caption = 'TRzEdit and TRzMaskEdit'
    ParentColor = True
    TabOrder = 3
    object RzLabel5: TRzLabel
      Left = 8
      Top = 24
      Width = 57
      Height = 13
      AutoSize = False
      Caption = 'TRzEdit'
      BevelWidth = 0
    end
    object RzLabel6: TRzLabel
      Left = 8
      Top = 52
      Width = 85
      Height = 13
      AutoSize = False
      Caption = 'TRzMaskEdit'
      BevelWidth = 0
    end
    object Label1: TLabel
      Left = 204
      Top = 52
      Width = 159
      Height = 13
      Caption = 'EditMask = '#39'#,###.##;1;0'#39
    end
    object RzEdit1: TRzEdit
      Left = 100
      Top = 20
      Width = 289
      Height = 21
      Text = 'TRzEdit supports right-justified text'
      Alignment = taRightJustify
      FrameController = RzFrameController1
      TabOrder = 0
    end
    object RzMaskEdit1: TRzMaskEdit
      Left = 100
      Top = 48
      Width = 81
      Height = 21
      Alignment = taRightJustify
      EditMask = '#,###.##;1;0'
      FrameController = RzFrameController1
      MaxLength = 8
      TabOrder = 1
      Text = '1,234.56'
    end
  end
  object GrpSpinEdits: TRzGroupBox
    Left = 8
    Top = 108
    Width = 161
    Height = 137
    Caption = 'TRzSpinEdit'
    ParentColor = True
    TabOrder = 1
    object RzLabel1: TRzLabel
      Left = 8
      Top = 112
      Width = 73
      Height = 13
      Caption = 'Button Width'
      BevelWidth = 0
    end
    object spnPreview: TRzSpinEdit
      Left = 8
      Top = 20
      Width = 145
      Height = 21
      Decimals = 2
      Increment = 0.010000000000000000
      Max = 1000.000000000000000000
      PageSize = 1.000000000000000000
      FrameController = RzFrameController1
      TabOrder = 0
    end
    object chkSpnFlat: TRzCheckBox
      Left = 8
      Top = 48
      Width = 86
      Height = 15
      Caption = 'Flat Buttons'
      Checked = True
      HighlightColor = 7879740
      HotTrack = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkSpnFlatClick
    end
    object chkHorzBtns: TRzCheckBox
      Left = 8
      Top = 68
      Width = 123
      Height = 15
      Caption = 'Horizontal Buttons'
      HighlightColor = 7879740
      HotTrack = True
      State = cbUnchecked
      TabOrder = 2
      OnClick = chkHorzBtnsClick
    end
    object chkDirection: TRzCheckBox
      Left = 8
      Top = 88
      Width = 139
      Height = 15
      Caption = 'Direction (Left-Right)'
      HighlightColor = 7879740
      HotTrack = True
      State = cbUnchecked
      TabOrder = 3
      OnClick = chkDirectionClick
    end
    object SpnButtonWidth: TRzSpinner
      Left = 88
      Top = 110
      Width = 65
      Max = 30
      Min = 10
      Value = 17
      OnChange = SpnButtonWidthChange
      ParentColor = False
      TabOrder = 4
    end
  end
  object GrpButtonEdits: TRzGroupBox
    Left = 180
    Top = 108
    Width = 225
    Height = 137
    Caption = 'TRzButtonEdit'
    ParentColor = True
    TabOrder = 2
    object RzLabel2: TRzLabel
      Left = 8
      Top = 72
      Width = 86
      Height = 13
      Caption = 'Primary Button'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      BevelWidth = 0
      LightTextStyle = True
    end
    object RzLabel3: TRzLabel
      Left = 124
      Top = 72
      Width = 95
      Height = 13
      Caption = 'Alternate Button'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      BevelWidth = 0
      LightTextStyle = True
    end
    object bedPreview: TRzButtonEdit
      Left = 8
      Top = 20
      Width = 209
      Height = 21
      FrameController = RzFrameController1
      TabOrder = 0
      AltBtnWidth = 15
      ButtonWidth = 15
    end
    object chkBtnFlat: TRzCheckBox
      Left = 8
      Top = 48
      Width = 86
      Height = 15
      Caption = 'Flat Buttons'
      Checked = True
      HighlightColor = 7879740
      HotTrack = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkBtnFlatClick
    end
    object chkButtonVisible: TRzCheckBox
      Left = 8
      Top = 88
      Width = 56
      Height = 15
      Caption = 'Visible'
      Checked = True
      HighlightColor = 7879740
      HotTrack = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkButtonVisibleClick
    end
    object chkAltBtnVisible: TRzCheckBox
      Left = 124
      Top = 88
      Width = 56
      Height = 15
      Caption = 'Visible'
      HighlightColor = 7879740
      HotTrack = True
      State = cbUnchecked
      TabOrder = 3
      OnClick = chkAltBtnVisibleClick
    end
    object CbxButtonKind: TRzComboBox
      Left = 8
      Top = 108
      Width = 97
      Height = 21
      Style = csDropDownList
      Ctl3D = False
      FrameController = RzFrameController1
      ParentCtl3D = False
      TabOrder = 4
      Text = 'bkLookup'
      OnChange = CbxButtonKindChange
      Items.Strings = (
        'bkLookup'
        'bkDropDown'
        'bkCalendar '
        'bkAccept'
        'bkReject'
        'bkFolder'
        'bkFind'
        'bkSearch')
      ItemIndex = 0
    end
    object CbxAltBtnKind: TRzComboBox
      Left = 124
      Top = 108
      Width = 97
      Height = 21
      Style = csDropDownList
      Ctl3D = False
      FrameController = RzFrameController1
      ParentCtl3D = False
      TabOrder = 5
      Text = 'bkLookup'
      OnChange = CbxAltBtnKindChange
      Items.Strings = (
        'bkLookup'
        'bkDropDown'
        'bkCalendar '
        'bkAccept'
        'bkReject'
        'bkFolder'
        'bkFind'
        'bkSearch')
      ItemIndex = 0
    end
  end
  object GrpDateTimeColor: TRzGroupBox
    Left = 8
    Top = 44
    Width = 397
    Height = 53
    Caption = 'TRzDateTimeEdit'
    ParentColor = True
    TabOrder = 0
    object RzLabel4: TRzLabel
      Left = 12
      Top = 24
      Width = 27
      Height = 13
      Caption = 'Date'
    end
    object RzLabel7: TRzLabel
      Left = 212
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Time'
    end
    object RzDateTimeEdit1: TRzDateTimeEdit
      Left = 52
      Top = 20
      Width = 133
      Height = 21
      EditType = etDate
      PopupHeight = 0
      PopupWidth = 0
      FrameController = RzFrameController1
      TabOrder = 0
    end
    object RzDateTimeEdit2: TRzDateTimeEdit
      Left = 252
      Top = 20
      Width = 133
      Height = 21
      ShowHowToUseHint = False
      EditType = etTime
      PopupHeight = 0
      PopupWidth = 0
      FrameController = RzFrameController1
      TabOrder = 1
    end
  end
  object pnlHeader: TRzPanel
    Left = 0
    Top = 0
    Width = 695
    Height = 35
    Align = alTop
    Alignment = taLeftJustify
    BorderOuter = fsFlat
    BorderSides = [sdBottom]
    Caption = 'Edit Controls'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9856100
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    GradientColorStop = 11855600
    TextMargin = 4
    ParentFont = False
    TabOrder = 4
    VisualStyle = vsGradient
    WordWrap = False
  end
  object RzGroupBox1: TRzGroupBox
    Left = 420
    Top = 44
    Width = 261
    Height = 149
    Caption = 'TRzColorEdit'
    ParentColor = True
    TabOrder = 5
    object RzColorEdit1: TRzColorEdit
      Left = 12
      Top = 20
      Width = 89
      Height = 21
      PopupHeight = 0
      PopupWidth = 0
      ShowDefaultColor = True
      ShowSystemColors = True
      FrameController = RzFrameController1
      TabOrder = 0
    end
    object chkNoColorArea: TRzCheckBox
      Left = 120
      Top = 68
      Width = 100
      Height = 15
      Caption = 'No Color Area'
      HighlightColor = 7879740
      HotTrack = True
      State = cbUnchecked
      TabOrder = 1
      OnClick = chkNoColorAreaClick
    end
    object chkDefaultColor: TRzCheckBox
      Left = 120
      Top = 20
      Width = 95
      Height = 15
      Caption = 'Default Color'
      Checked = True
      HighlightColor = 7879740
      HotTrack = True
      State = cbChecked
      TabOrder = 2
      OnClick = chkDefaultColorClick
    end
    object chkCustomColorArea: TRzCheckBox
      Left = 120
      Top = 92
      Width = 129
      Height = 15
      Caption = 'Custom Color Area'
      HighlightColor = 7879740
      HotTrack = True
      State = cbUnchecked
      TabOrder = 3
      OnClick = chkCustomColorAreaClick
    end
    object chkSystemColors: TRzCheckBox
      Left = 120
      Top = 44
      Width = 103
      Height = 15
      Caption = 'System Colors'
      Checked = True
      HighlightColor = 7879740
      HotTrack = True
      State = cbChecked
      TabOrder = 4
      OnClick = chkSystemColorsClick
    end
    object chkColorHints: TRzCheckBox
      Left = 120
      Top = 116
      Width = 82
      Height = 15
      Caption = 'Color Hints'
      Checked = True
      HighlightColor = 7879740
      HotTrack = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkColorHintsClick
    end
  end
  object RzGroupBox2: TRzGroupBox
    Left = 420
    Top = 208
    Width = 261
    Height = 133
    Caption = 'TRzNumericEdit'
    ParentColor = True
    TabOrder = 6
    object RzLabel8: TRzLabel
      Left = 12
      Top = 48
      Width = 82
      Height = 13
      Caption = 'DisplayFormat'
    end
    object RzNumericEdit1: TRzNumericEdit
      Left = 12
      Top = 20
      Width = 105
      Height = 21
      TabOrder = 0
      IntegersOnly = False
      DisplayFormat = '0.00;(0.00)'
    end
    object chkCalculatorVisible: TRzCheckBox
      Left = 132
      Top = 20
      Width = 118
      Height = 15
      Caption = 'Calculator Visible'
      State = cbUnchecked
      TabOrder = 1
      OnClick = chkCalculatorVisibleClick
    end
    object optDisplayFormat3: TRzRadioButton
      Left = 24
      Top = 108
      Width = 56
      Height = 15
      Caption = ',0;(,0)'
      TabOrder = 5
      OnClick = DisplayFormatClick
    end
    object optDisplayFormat1: TRzRadioButton
      Left = 24
      Top = 68
      Width = 84
      Height = 15
      Caption = '0.00;(0.00)'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = DisplayFormatClick
    end
    object optDisplayFormat2: TRzRadioButton
      Left = 24
      Top = 88
      Width = 86
      Height = 15
      Caption = '#.######'
      TabOrder = 4
      OnClick = DisplayFormatClick
    end
    object chkIntegersOnly: TRzCheckBox
      Left = 132
      Top = 48
      Width = 97
      Height = 15
      Caption = 'Integers Only'
      State = cbUnchecked
      TabOrder = 2
      OnClick = chkIntegersOnlyClick
    end
  end
  object RzFrameController1: TRzFrameController
    FrameVisible = True
    Left = 4
    Top = 256
  end
end

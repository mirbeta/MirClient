object frmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog: TfrmSpreadSheetConditionalFormattingDataBarRuleStyleEditDialog
  Left = 0
  Top = 0
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  ClientHeight = 568
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 486
    Height = 568
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object ccbPositiveBarColor: TcxColorComboBox
      Tag = 1
      Left = 91
      Top = 139
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Width = 304
    end
    object cbbFillMode: TcxComboBox
      Left = 91
      Top = 28
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 0
      Width = 304
    end
    object cbbBarDirection: TcxComboBox
      Left = 91
      Top = 82
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 2
      Width = 304
    end
    object ccbPositiveBarBorderColor: TcxColorComboBox
      Tag = 1
      Left = 91
      Top = 164
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 304
    end
    object ccbNegativeBarColor: TcxColorComboBox
      Tag = 1
      Left = 127
      Top = 219
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Width = 268
    end
    object rbNegativeBarColor: TcxRadioButton
      Left = 22
      Top = 219
      Width = 99
      Height = 19
      Caption = 'C&olor:'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 5
      OnClick = rbNegativeBarColorClick
      GroupIndex = 1
      ParentBackground = False
      Transparent = True
    end
    object rbNegativeBarColorAuto: TcxRadioButton
      Left = 22
      Top = 244
      Width = 373
      Height = 19
      Caption = '&Apply same fill color as positive bar'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 7
      OnClick = rbNegativeBarColorClick
      GroupIndex = 1
      ParentBackground = False
      Transparent = True
    end
    object rbNegativeBarBorderColor: TcxRadioButton
      Left = 22
      Top = 281
      Width = 99
      Height = 19
      Caption = 'Bo&rder color:'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 8
      OnClick = rbNegativeBarBorderColorClick
      GroupIndex = 2
      ParentBackground = False
      Transparent = True
    end
    object ccbNegativeBarBorderColor: TcxColorComboBox
      Tag = 1
      Left = 127
      Top = 281
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Width = 268
    end
    object rbNegativeBarBorderColorAuto: TcxRadioButton
      Left = 22
      Top = 306
      Width = 373
      Height = 19
      Caption = 'A&pply same border color as positive bar'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 10
      OnClick = rbNegativeBarBorderColorClick
      GroupIndex = 2
      ParentBackground = False
      Transparent = True
    end
    object btnOk: TcxButton
      Left = 231
      Top = 473
      Width = 85
      Height = 25
      Caption = 'btnOk'
      Default = True
      ModalResult = 1
      TabOrder = 15
    end
    object btnCancel: TcxButton
      Left = 322
      Top = 473
      Width = 85
      Height = 25
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 16
    end
    object rbAxisAuto: TcxRadioButton
      Left = 22
      Top = 361
      Width = 373
      Height = 19
      Caption = 
        'A&utomatic (display at variable position based on negative value' +
        's)'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 11
      OnClick = rbAxisAutoClick
      GroupIndex = 3
      ParentBackground = False
      Transparent = True
    end
    object rbAxisMidpoint: TcxRadioButton
      Left = 22
      Top = 386
      Width = 373
      Height = 19
      Caption = 'Cell &midpoint'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 12
      OnClick = rbAxisAutoClick
      GroupIndex = 3
      ParentBackground = False
      Transparent = True
    end
    object rbAxisNone: TcxRadioButton
      Left = 22
      Top = 411
      Width = 373
      Height = 19
      Caption = 'Non&e (show negative value bars in same direction as positive)'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 13
      OnClick = rbAxisAutoClick
      GroupIndex = 3
      ParentBackground = False
      Transparent = True
    end
    object ccbAxisColor: TcxColorComboBox
      Tag = 1
      Left = 91
      Top = 436
      Properties.AllowSelectColor = True
      Properties.ColorDialogShowFull = True
      Properties.ColorDialogType = cxcdtAdvanced
      Properties.CustomColors = <>
      Properties.NamingConvention = cxncHTML4
      Properties.PrepareList = cxplHTML4
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Width = 192
    end
    object cbbBorderStyle: TcxComboBox
      Left = 91
      Top = 55
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbbPositiveBarBorderStylePropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 304
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object lcgPositiveBar: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Positive Bar Appearance'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object lciPositiveBarColor: TdxLayoutItem
      Parent = lcgPositiveBar
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = '&Color:'
      Control = ccbPositiveBarColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 304
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgCommon: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Common'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lciFillMode: TdxLayoutItem
      Parent = lcgCommon
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = '&Fill mode:'
      Control = cbbFillMode
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 304
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciBarDirection: TdxLayoutItem
      Parent = lcgCommon
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Bar &direction:'
      Control = cbbBarDirection
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 304
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciPositiveBarBorderColor: TdxLayoutItem
      Parent = lcgPositiveBar
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Border co&lor:'
      Control = ccbPositiveBarBorderColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 304
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgNegativeBar: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Negative Bar Appearance'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = ccbNegativeBarColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 268
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = rbNegativeBarColor
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 99
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcgNegativeBar
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainItem8: TdxLayoutItem
      Parent = lcgNegativeBar
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = rbNegativeBarColorAuto
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 373
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcgNegativeBar
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object lcMainItem9: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbNegativeBarBorderColor
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 99
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem10: TdxLayoutItem
      Parent = lcMainGroup2
      Control = ccbNegativeBarBorderColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 268
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcgNegativeBar
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object lcMainItem11: TdxLayoutItem
      Parent = lcgNegativeBar
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbNegativeBarBorderColorAuto
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 373
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcMainGroup4: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object lcMainItem16: TdxLayoutItem
      Parent = lcMainGroup4
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem17: TdxLayoutItem
      Parent = lcMainGroup4
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgAxis: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Axis Settings'
      ButtonOptions.Buttons = <>
      Index = 3
    end
    object lcMainItem13: TdxLayoutItem
      Parent = lcgAxis
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxRadioButton6'
      CaptionOptions.Visible = False
      Control = rbAxisAuto
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 373
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem12: TdxLayoutItem
      Parent = lcgAxis
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cxRadioButton5'
      CaptionOptions.Visible = False
      Control = rbAxisMidpoint
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 373
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem14: TdxLayoutItem
      Parent = lcgAxis
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbAxisNone
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 373
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciAxisColor: TdxLayoutItem
      Parent = lcgAxis
      AlignHorz = ahLeft
      CaptionOptions.Text = 'A&xis color:'
      Control = ccbAxisColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciPositiveBarBorderStyle: TdxLayoutItem
      Parent = lcgCommon
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = '&Border style:'
      Control = cbbBorderStyle
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 304
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 544
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

object dxSpreadSheetContainerCustomizationDialogForm: TdxSpreadSheetContainerCustomizationDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Customize Object'
  ClientHeight = 500
  ClientWidth = 440
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 440
    Height = 500
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnOK: TcxButton
      Left = 264
      Top = 465
      Width = 80
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 54
    end
    object btnCancel: TcxButton
      Left = 350
      Top = 465
      Width = 80
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 55
    end
    object rbNoFill: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 527
      Height = 17
      Caption = '&No fill'
      TabOrder = 20
      Visible = False
      OnClick = rbTextureFillClick
      Transparent = True
    end
    object rbSolidFill: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 527
      Height = 17
      Caption = '&Solid fill'
      TabOrder = 21
      Visible = False
      OnClick = rbTextureFillClick
      Transparent = True
    end
    object rbGradientFill: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 527
      Height = 17
      Caption = '&Gradient fill'
      TabOrder = 22
      Visible = False
      OnClick = rbTextureFillClick
      Transparent = True
    end
    object rbTextureFill: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 527
      Height = 17
      Caption = '&Texture fill'
      TabOrder = 23
      Visible = False
      OnClick = rbTextureFillClick
      Transparent = True
    end
    object btnSolidFillColor: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = '&Color'
      OptionsImage.Spacing = 8
      TabOrder = 24
      Visible = False
      OnClick = btnColorClick
    end
    object imTextureFill: TcxImage
      Left = 10000
      Top = 10000
      Properties.PopupMenuLayout.MenuItems = []
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 29
      Visible = False
      Height = 200
      Width = 446
    end
    object btnTextureFillSave: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Action = acTextureFillSave
      TabOrder = 31
      Visible = False
    end
    object btnTextureFillLoad: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Action = acTextureFillLoad
      TabOrder = 30
      Visible = False
    end
    object ccbGradientFillDirection: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.TransparentBorder = False
      TabOrder = 25
      Visible = False
      Width = 192
    end
    object btnGradientFillColor: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = '&Color'
      Enabled = False
      OptionsImage.Spacing = 8
      TabOrder = 26
      Visible = False
      OnClick = btnGradientFillColorClick
    end
    object btnGradientFillAddStop: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = '&Add'
      OptionsImage.Layout = blGlyphTop
      TabOrder = 27
      Visible = False
      OnClick = btnGradientFillAddStopClick
    end
    object btnGradientFillRemoveStop: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = 'Remo&ve'
      Enabled = False
      OptionsImage.Layout = blGlyphTop
      TabOrder = 28
      Visible = False
      OnClick = btnGradientFillRemoveStopClick
    end
    object rbNoLine: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 527
      Height = 17
      Caption = '&No line'
      TabOrder = 32
      Visible = False
      OnClick = rbGradientLineClick
      GroupIndex = 1
      Transparent = True
    end
    object rbSolidLine: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 527
      Height = 17
      Caption = '&Solid line'
      TabOrder = 33
      Visible = False
      OnClick = rbGradientLineClick
      GroupIndex = 1
      Transparent = True
    end
    object rbGradientLine: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 527
      Height = 17
      Caption = '&Gradient line'
      TabOrder = 34
      Visible = False
      OnClick = rbGradientLineClick
      GroupIndex = 1
      Transparent = True
    end
    object btnSolidLineColor: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = '&Color'
      OptionsImage.Spacing = 8
      TabOrder = 37
      Visible = False
      OnClick = btnColorClick
    end
    object ccbLineStyle: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 35
      Visible = False
      Width = 140
    end
    object ceLineWidth: TcxSpinEdit
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.ValueType = vtFloat
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 36
      Visible = False
      Width = 140
    end
    object btnGradientLineColor: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = '&Color'
      Enabled = False
      OptionsImage.Spacing = 8
      TabOrder = 39
      Visible = False
      OnClick = btnGradientLineColorClick
    end
    object btnGradientLineAddStop: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = '&Add'
      OptionsImage.Spacing = 8
      TabOrder = 40
      Visible = False
      OnClick = btnGradientLineAddStopClick
    end
    object btnGradientLineRemoveStop: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 26
      Caption = 'Remo&ve'
      Enabled = False
      OptionsImage.Spacing = 8
      TabOrder = 41
      Visible = False
      OnClick = btnGradientLineRemoveStopClick
    end
    object ccbGradientLineDirection: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 38
      Visible = False
      Width = 140
    end
    object lbSizeAndRotate: TcxLabel
      Left = 21
      Top = 44
      AutoSize = False
      Caption = 'Size and rotate'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 398
      AnchorY = 53
    end
    object seHeight: TcxSpinEdit
      Left = 87
      Top = 68
      Properties.AssignedValues.MinValue = True
      Properties.ImmediatePost = True
      Properties.OnChange = seHeightPropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 75
    end
    object seRotation: TcxSpinEdit
      Left = 87
      Top = 95
      Properties.DisplayFormat = '0.##### '#176
      Properties.ImmediatePost = True
      Properties.ValueType = vtFloat
      Style.HotTrack = False
      TabOrder = 2
      Width = 75
    end
    object seWidth: TcxSpinEdit
      Left = 211
      Top = 68
      Properties.AssignedValues.MinValue = True
      Properties.ImmediatePost = True
      Properties.OnChange = seWidthPropertiesChange
      Style.HotTrack = False
      TabOrder = 3
      Width = 75
    end
    object lbPosition: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Positioning'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 527
      AnchorY = 10009
    end
    object rbTwoCells: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 511
      Height = 17
      Caption = 'Move and &size with cells'
      TabOrder = 17
      Visible = False
      GroupIndex = 2
      Transparent = True
    end
    object rbOneCell: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 511
      Height = 17
      Caption = '&Move but don'#39't size with cells'
      TabOrder = 18
      Visible = False
      GroupIndex = 2
      Transparent = True
    end
    object rbAbsolute: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 511
      Height = 17
      Caption = '&Don'#39't move or size with cells'
      TabOrder = 19
      Visible = False
      GroupIndex = 2
      Transparent = True
    end
    object lbScale: TcxLabel
      Left = 21
      Top = 122
      AutoSize = False
      Caption = 'Scale'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 398
      AnchorY = 131
    end
    object seScaleHeight: TcxSpinEdit
      Left = 87
      Top = 146
      Properties.AssignedValues.MinValue = True
      Properties.DisplayFormat = '0 %'
      Properties.ImmediatePost = True
      Properties.OnChange = seScaleHeightPropertiesChange
      Style.HotTrack = False
      TabOrder = 5
      Width = 75
    end
    object seScaleWidth: TcxSpinEdit
      Left = 211
      Top = 146
      Properties.AssignedValues.MinValue = True
      Properties.DisplayFormat = '0 %'
      Properties.ImmediatePost = True
      Properties.OnChange = seScaleWidthPropertiesChange
      Style.HotTrack = False
      TabOrder = 6
      Width = 75
    end
    object cbLockAspectRatio: TcxCheckBox
      Left = 37
      Top = 173
      Caption = 'Lock &aspect ratio'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = cbLockAspectRatioClick
    end
    object cbRelativeToPictureSize: TcxCheckBox
      Left = 37
      Top = 196
      Caption = '&Relative to original picture size'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      OnClick = cbRelativeToPictureSizeClick
    end
    object lbOriginalSize: TcxLabel
      Left = 21
      Top = 297
      AutoSize = False
      Caption = 'Original size'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 398
      AnchorY = 306
    end
    object lbCrop: TcxLabel
      Left = 21
      Top = 219
      AutoSize = False
      Caption = 'Crop from'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 398
      AnchorY = 228
    end
    object seCropLeft: TcxSpinEdit
      Left = 87
      Top = 243
      Properties.ImmediatePost = True
      Properties.OnChange = seCropHorzPropertiesChange
      Style.HotTrack = False
      TabOrder = 10
      Width = 75
    end
    object seCropRight: TcxSpinEdit
      Left = 87
      Top = 270
      Properties.ImmediatePost = True
      Properties.OnChange = seCropHorzPropertiesChange
      Style.HotTrack = False
      TabOrder = 11
      Width = 75
    end
    object seCropTop: TcxSpinEdit
      Left = 211
      Top = 243
      Properties.ImmediatePost = True
      Properties.OnChange = seCropVertPropertiesChange
      Style.HotTrack = False
      TabOrder = 12
      Width = 75
    end
    object seCropBottom: TcxSpinEdit
      Left = 211
      Top = 270
      Properties.ImmediatePost = True
      Properties.OnChange = seCropVertPropertiesChange
      Style.HotTrack = False
      TabOrder = 13
      Width = 75
    end
    object btnReset: TcxButton
      Left = 37
      Top = 341
      Width = 75
      Height = 25
      Caption = 'Re&set'
      TabOrder = 15
      OnClick = btnResetClick
    end
    object lbTextAlignment: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Alignment'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 438
      AnchorY = 10009
    end
    object cbTextBoxHorzAlign: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 45
      Visible = False
      Width = 85
    end
    object cbTextBoxVertAlign: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 46
      Visible = False
      Width = 85
    end
    object lbTextPadding: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Padding'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 438
      AnchorY = 10009
    end
    object seTextPaddingLeft: TcxSpinEdit
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.ImmediatePost = True
      Style.HotTrack = False
      TabOrder = 48
      Visible = False
      Width = 85
    end
    object seTextPaddingRight: TcxSpinEdit
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.ImmediatePost = True
      Style.HotTrack = False
      TabOrder = 49
      Visible = False
      Width = 85
    end
    object seTextPaddingTop: TcxSpinEdit
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.ImmediatePost = True
      Style.HotTrack = False
      TabOrder = 50
      Visible = False
      Width = 85
    end
    object seTextPaddingBottom: TcxSpinEdit
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.ImmediatePost = True
      Style.HotTrack = False
      TabOrder = 51
      Visible = False
      Width = 85
    end
    object cbTextBoxAutoSize: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbTextBoxAutoSize'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 52
      Transparent = True
      Visible = False
    end
    object cbTextBoxWordWrap: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbTextBoxWordWrap'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 53
      Transparent = True
      Visible = False
    end
    object meText: TcxMemo
      Left = 10000
      Top = 10000
      Lines.Strings = (
        'meText')
      Properties.ScrollBars = ssVertical
      Style.HotTrack = False
      TabOrder = 42
      Visible = False
      Height = 393
      Width = 438
    end
    object btnTextFont: TcxButton
      Left = 10000
      Top = 10000
      Width = 80
      Height = 25
      Caption = 'btnFont'
      TabOrder = 43
      Visible = False
      OnClick = btnTextFontClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avParentManaged
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgTabs: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object lcMainGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lcgFill: TdxLayoutGroup
      Parent = lcgTabs
      CaptionOptions.Text = 'Fill'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 2
    end
    object lcgLine: TdxLayoutGroup
      Parent = lcgTabs
      CaptionOptions.Text = 'Line'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 3
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcgFill
      CaptionOptions.Visible = False
      Control = rbNoFill
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 527
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcgFill
      CaptionOptions.Visible = False
      Control = rbSolidFill
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 385
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcgFill
      CaptionOptions.Visible = False
      Control = rbGradientFill
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 385
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcgFill
      CaptionOptions.Visible = False
      Control = rbTextureFill
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 385
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcgSolidFill: TdxLayoutItem
      Parent = lcMainGroup3
      AlignHorz = ahLeft
      Control = btnSolidFillColor
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcgFill
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 5
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcgTextureFill
      AlignHorz = ahClient
      AlignVert = avClient
      Control = imTextureFill
      ControlOptions.OriginalHeight = 200
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem8: TdxLayoutItem
      Parent = lcMainGroup5
      CaptionOptions.Visible = False
      Control = btnTextureFillSave
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgTextureFill: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup3
      AlignVert = avClient
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lcMainGroup5: TdxLayoutAutoCreatedGroup
      Parent = lcgTextureFill
      Index = 1
      AutoCreated = True
    end
    object lcMainItem10: TdxLayoutItem
      Parent = lcMainGroup5
      CaptionOptions.Visible = False
      Control = btnTextureFillLoad
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgGradientFill: TdxLayoutGroup
      Parent = lcMainGroup3
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      ShowBorder = False
      Index = 1
    end
    object lciGradientFillDirection: TdxLayoutItem
      Parent = lcgGradientFill
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Direction:'
      CaptionOptions.Layout = clTop
      Control = ccbGradientFillDirection
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 192
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciGradientFillStops: TdxLayoutItem
      Parent = lcgGradientFill
      CaptionOptions.Text = '&Stops:'
      CaptionOptions.Layout = clTop
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup6: TdxLayoutAutoCreatedGroup
      Parent = lcgGradientFill
      AlignHorz = ahClient
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lcMainItem13: TdxLayoutItem
      Parent = lcMainGroup6
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnGradientFillColor
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object lcMainItem12: TdxLayoutItem
      Parent = lcMainGroup7
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      AllowRemove = False
      Control = btnGradientFillAddStop
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem14: TdxLayoutItem
      Parent = lcMainGroup7
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnGradientFillRemoveStop
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 1
    end
    object lcMainGroup7: TdxLayoutGroup
      Parent = lcMainGroup6
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcgFill
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 4
    end
    object lciPenNoLine: TdxLayoutItem
      Parent = lciLinePenStyle
      CaptionOptions.Visible = False
      Control = rbNoLine
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 368
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciPenSolidLine: TdxLayoutItem
      Parent = lciLinePenStyle
      CaptionOptions.Visible = False
      Control = rbSolidLine
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 368
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciPenGradientLine: TdxLayoutItem
      Parent = lciLinePenStyle
      CaptionOptions.Visible = False
      Control = rbGradientLine
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 368
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lciLinePenStyle
      CaptionOptions.Text = 'Separator'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 3
    end
    object lcgSolidLine: TdxLayoutItem
      Parent = lcgLine
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnSolidLineColor
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciLineStyle: TdxLayoutItem
      Parent = lcgLine
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Style:'
      CaptionOptions.Layout = clTop
      Control = ccbLineStyle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciLineWidth: TdxLayoutItem
      Parent = lcgLine
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Width:'
      CaptionOptions.Layout = clTop
      Control = ceLineWidth
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcgGradientLine: TdxLayoutGroup
      Parent = lcgLine
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      ShowBorder = False
      Index = 4
    end
    object lciGradientLineStops: TdxLayoutItem
      Parent = lcgGradientLine
      CaptionOptions.Text = '&Stops:'
      CaptionOptions.Layout = clTop
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup4: TdxLayoutAutoCreatedGroup
      Parent = lcgGradientLine
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lcMainItem16: TdxLayoutItem
      Parent = lcMainGroup4
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnGradientLineColor
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object lcMainGroup8: TdxLayoutGroup
      Parent = lcMainGroup4
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcMainItem18: TdxLayoutItem
      Parent = lcMainGroup8
      CaptionOptions.Visible = False
      Control = btnGradientLineAddStop
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem19: TdxLayoutItem
      Parent = lcMainGroup8
      CaptionOptions.Visible = False
      Control = btnGradientLineRemoveStop
      ControlOptions.OriginalHeight = 26
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 1
    end
    object lciGradientLineDirection: TdxLayoutItem
      Parent = lcgGradientLine
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Direction:'
      CaptionOptions.Layout = clTop
      Control = ccbGradientLineDirection
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgSize: TdxLayoutGroup
      Parent = lcgTabs
      CaptionOptions.Text = 'Size'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object lcMainItem23: TdxLayoutItem
      Parent = lcgSize
      CaptionOptions.Text = 'lbSizeAndRotate'
      CaptionOptions.Visible = False
      Control = lbSizeAndRotate
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 360
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup11: TdxLayoutGroup
      Parent = lcgSize
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcMainGroup10: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup11
      AlignHorz = ahLeft
      Index = 0
      AutoCreated = True
    end
    object lciHeight: TdxLayoutItem
      Parent = lcMainGroup10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'H&eight:'
      Control = seHeight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRotation: TdxLayoutItem
      Parent = lcMainGroup10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Ro&tation:'
      Control = seRotation
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciWidth: TdxLayoutItem
      Parent = lcMainGroup11
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Wi&dth'
      Control = seWidth
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgProperties: TdxLayoutGroup
      Parent = lcgTabs
      CaptionOptions.Text = 'Properties'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object lcMainItem17: TdxLayoutItem
      Parent = lcgProperties
      CaptionOptions.Text = 'lbPosition'
      CaptionOptions.Visible = False
      Control = lbPosition
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 356
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup9: TdxLayoutGroup
      Parent = lcgProperties
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainItem20: TdxLayoutItem
      Parent = lcMainGroup9
      CaptionOptions.Text = 'rbTwoCells'
      CaptionOptions.Visible = False
      Control = rbTwoCells
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem21: TdxLayoutItem
      Parent = lcMainGroup9
      CaptionOptions.Text = 'rbOneCell'
      CaptionOptions.Visible = False
      Control = rbOneCell
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 356
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem22: TdxLayoutItem
      Parent = lcMainGroup9
      CaptionOptions.Text = 'rbAbsolute'
      CaptionOptions.Visible = False
      Control = rbAbsolute
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 356
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem24: TdxLayoutItem
      Parent = lcgSize
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbScale
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 360
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainGroup13: TdxLayoutGroup
      Parent = lcgSize
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 3
    end
    object lcMainGroup12: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup13
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object lciScaleHeight: TdxLayoutItem
      Parent = lcMainGroup12
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Height:'
      Control = seScaleHeight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciScaleWidth: TdxLayoutItem
      Parent = lcMainGroup12
      CaptionOptions.Text = '&Width:'
      Control = seScaleWidth
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem27: TdxLayoutItem
      Parent = lcMainGroup13
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbLockAspectRatio
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciRelativeToPictureSize: TdxLayoutItem
      Parent = lcMainGroup13
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbRelativeToPictureSize
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciOriginalSize: TdxLayoutItem
      Parent = lcgSize
      CaptionOptions.Visible = False
      Control = lbOriginalSize
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 360
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lciCrop: TdxLayoutItem
      Parent = lcgSize
      CaptionOptions.Text = 'lbCrop'
      CaptionOptions.Visible = False
      Control = lbCrop
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 360
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcgCrop: TdxLayoutGroup
      Parent = lcgSize
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 5
    end
    object lcMainGroup14: TdxLayoutAutoCreatedGroup
      Parent = lcgCrop
      AlignHorz = ahLeft
      Index = 0
      AutoCreated = True
    end
    object lciCropLeft: TdxLayoutItem
      Parent = lcMainGroup14
      AlignHorz = ahLeft
      CaptionOptions.Text = '&Left:'
      Control = seCropLeft
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCropRight: TdxLayoutItem
      Parent = lcMainGroup14
      CaptionOptions.Text = 'Ri&ght:'
      Control = seCropRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup15: TdxLayoutAutoCreatedGroup
      Parent = lcgCrop
      Index = 1
      AutoCreated = True
    end
    object lciCropTop: TdxLayoutItem
      Parent = lcMainGroup15
      CaptionOptions.Text = 'To&p:'
      Control = seCropTop
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciCropBottom: TdxLayoutItem
      Parent = lcMainGroup15
      CaptionOptions.Text = 'Botto&m:'
      Control = seCropBottom
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgOriginalSize: TdxLayoutGroup
      Parent = lcgSize
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 7
    end
    object lclOriginalSize: TdxLayoutLabeledItem
      Parent = lcgOriginalSize
      CaptionOptions.Text = 'Height: / Width'
      Index = 0
    end
    object lcMainItem25: TdxLayoutItem
      Parent = lcgOriginalSize
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnReset
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcgTextBox: TdxLayoutGroup
      Parent = lcgTabs
      CaptionOptions.Text = 'Text Box'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 5
    end
    object lcMainItem26: TdxLayoutItem
      Parent = lcgTextBox
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = lbTextAlignment
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 358
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciTextBoxHorzAlign: TdxLayoutItem
      Parent = lcMainGroup19
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'lciTextBoxHorzAlign'
      Control = cbTextBoxHorzAlign
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciTextBoxVertAlign: TdxLayoutItem
      Parent = lcMainGroup19
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'lciTextBoxVertAlign'
      Control = cbTextBoxVertAlign
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup19: TdxLayoutGroup
      Parent = lcgTextBox
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lcMainItem31: TdxLayoutItem
      Parent = lcgTextBox
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = lbTextPadding
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 358
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainGroup18: TdxLayoutGroup
      Parent = lcgTextBox
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 16
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object lcMainGroup16: TdxLayoutGroup
      Parent = lcMainGroup18
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object lciTextPaddingLeft: TdxLayoutItem
      Parent = lcMainGroup16
      CaptionOptions.Text = '&Left:'
      Control = seTextPaddingLeft
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciTextPaddingRight: TdxLayoutItem
      Parent = lcMainGroup16
      CaptionOptions.Text = 'Ri&ght:'
      Control = seTextPaddingRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup17: TdxLayoutGroup
      Parent = lcMainGroup18
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lciTextPaddingTop: TdxLayoutItem
      Parent = lcMainGroup17
      CaptionOptions.Text = 'To&p:'
      Control = seTextPaddingTop
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciTextPaddingBottom: TdxLayoutItem
      Parent = lcMainGroup17
      CaptionOptions.Text = 'Botto&m:'
      Control = seTextPaddingBottom
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem28: TdxLayoutItem
      Parent = lcgTextBox
      AlignVert = avTop
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbTextBoxAutoSize
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lciTextBoxWordWrap: TdxLayoutItem
      Parent = lcgTextBox
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = cbTextBoxWordWrap
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lcgText: TdxLayoutGroup
      Parent = lcgTabs
      CaptionOptions.Text = 'Text'
      ButtonOptions.Buttons = <>
      Index = 4
    end
    object lcMainItem33: TdxLayoutItem
      Parent = lcgText
      AlignHorz = ahClient
      AlignVert = avClient
      Control = meText
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem34: TdxLayoutItem
      Parent = lcgText
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnTextFont
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 80
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainSeparatorItem3: TdxLayoutSeparatorItem
      Parent = lcgTextBox
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
    object lciLinePenStyle: TdxLayoutGroup
      Parent = lcgLine
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 376
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ColorDialog: TdxColorDialog
    Options.ColorPicker.DefaultVisible = True
    Left = 272
  end
  object TextureOpenDialog: TOpenDialog
    Left = 336
  end
  object TextureSaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 336
    Top = 32
  end
  object alActions: TActionList
    Left = 304
    object acTextureFillLoad: TAction
      Caption = '&Load'
      OnExecute = acTextureFillLoadExecute
    end
    object acTextureFillSave: TAction
      Caption = '&Save'
      OnExecute = acTextureFillSaveExecute
      OnUpdate = acTextureFillSaveUpdate
    end
  end
  object fdTextBoxFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Left = 304
    Top = 32
  end
end

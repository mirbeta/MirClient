object dxSpreadSheetPageSetupDialogForm: TdxSpreadSheetPageSetupDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'dxSpreadSheetPageSetupDialogForm'
  ClientHeight = 482
  ClientWidth = 384
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmAuto
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 384
    Height = 482
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnCancel: TcxButton
      Left = 299
      Top = 447
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 44
      OnClick = btnCancelClick
    end
    object btnOK: TcxButton
      Left = 218
      Top = 447
      Width = 75
      Height = 25
      Caption = 'btnOK'
      Default = True
      TabOrder = 43
      OnClick = btnOKClick
    end
    object tePrintArea: TcxButtonEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ertiArea
      Properties.Buttons = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 29
      Text = 'tePrintArea'
      Visible = False
      OnEnter = tePrintAreaEnter
      OnExit = tePrintAreaExit
      Width = 282
    end
    object lbPrint: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'lbPrint'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 16
      Width = 342
    end
    object cbPrintGridLines: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbPrintGridLines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 34
      Transparent = True
      Visible = False
    end
    object cbPrintHeaders: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbGridLines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 37
      Transparent = True
      Visible = False
    end
    object cbPrintCommentsMode: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 38
      Visible = False
      Width = 140
    end
    object cbPrintCellErrorsMode: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 39
      Visible = False
      Width = 140
    end
    object cbBlackAndWhite: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbGridLines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 35
      Transparent = True
      Visible = False
    end
    object cbDraftQuality: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbGridLines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 36
      Transparent = True
      Visible = False
    end
    object lbPrintTitles: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'lbPrintTitles'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 16
      Width = 342
    end
    object teRowsToRepeat: TcxButtonEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ertiArea
      Properties.Buttons = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 31
      Text = 'teRowsToRepeat'
      Visible = False
      OnEnter = tePrintAreaEnter
      OnExit = tePrintAreaExit
      Width = 317
    end
    object teColumnsToRepeat: TcxButtonEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ertiArea
      Properties.Buttons = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 32
      Text = 'teColumnsToRepeat'
      Visible = False
      OnEnter = tePrintAreaEnter
      OnExit = tePrintAreaExit
      Width = 317
    end
    object lbPageOrder: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'lbPageOrder'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 16
      Width = 342
    end
    object rbtnDownThenOver: TcxRadioButton
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 252
      Height = 17
      Caption = '&Down, then over'
      Checked = True
      Color = clBtnFace
      ParentColor = False
      TabOrder = 41
      TabStop = True
      Visible = False
      OnClick = rbtnOverThenDownClick
      GroupIndex = 1
      ParentBackground = False
      Transparent = True
    end
    object rbtnOverThenDown: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 252
      Height = 17
      Caption = 'O&ver, then down'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 42
      Visible = False
      OnClick = rbtnOverThenDownClick
      GroupIndex = 1
      ParentBackground = False
      Transparent = True
    end
    object pbxPageOrder: TPaintBox
      Left = 10000
      Top = 10000
      Width = 74
      Height = 49
      Color = clBtnFace
      ParentColor = False
      Visible = False
      OnPaint = pbxPageOrderPaint
    end
    object lbOrientation: TcxLabel
      Left = 21
      Top = 70
      AutoSize = False
      Caption = 'lbOrientation'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 16
      Width = 342
    end
    object pbxPageOrientation: TPaintBox
      Left = 31
      Top = 96
      Width = 32
      Height = 32
      Color = clBtnFace
      ParentColor = False
      OnPaint = pbxPageOrientationPaint
    end
    object rbPageOrientationPortrait: TcxRadioButton
      Tag = 1
      Left = 73
      Top = 92
      Width = 290
      Height = 17
      Caption = 'Portrait'
      Checked = True
      Color = clBtnFace
      ParentColor = False
      TabOrder = 4
      TabStop = True
      OnClick = rbPageOrientationPortraitClick
      GroupIndex = 2
      ParentBackground = False
      Transparent = True
    end
    object rbPageOrientationLandscape: TcxRadioButton
      Left = 73
      Top = 115
      Width = 290
      Height = 17
      Caption = 'Landscape'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 5
      OnClick = rbPageOrientationPortraitClick
      GroupIndex = 2
      ParentBackground = False
      Transparent = True
    end
    object meFirstPageNumber: TcxMaskEdit
      Left = 108
      Top = 253
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\d*'
      Style.HotTrack = False
      TabOrder = 13
      Width = 45
    end
    object seMarginLeft: TcxSpinEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ersiMargins
      Style.HotTrack = False
      TabOrder = 14
      Visible = False
      Width = 75
    end
    object seMarginTop: TcxSpinEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ersiMargins
      Style.HotTrack = False
      TabOrder = 15
      Visible = False
      Width = 75
    end
    object seMarginRight: TcxSpinEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ersiMargins
      Style.HotTrack = False
      TabOrder = 20
      Visible = False
      Width = 75
    end
    object seMarginHeader: TcxSpinEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ersiMargins
      Style.HotTrack = False
      TabOrder = 19
      Visible = False
      Width = 75
    end
    object seMarginBottom: TcxSpinEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ersiMargins
      Style.HotTrack = False
      TabOrder = 17
      Visible = False
      Width = 75
    end
    object seMarginFooter: TcxSpinEdit
      Left = 10000
      Top = 10000
      RepositoryItem = ersiMargins
      Style.HotTrack = False
      TabOrder = 18
      Visible = False
      Width = 75
    end
    object gbPagePreviewHolder: TcxGroupBox
      Left = 10000
      Top = 10000
      PanelStyle.Active = True
      ParentBackground = False
      ParentColor = False
      Style.Color = clBtnFace
      Style.TransparentBorder = False
      TabOrder = 16
      Visible = False
      Height = 160
      Width = 160
    end
    object lbCenterOnPage: TcxLabel
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'lbCenterOnPage'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 16
      Width = 342
    end
    object cbCenterHorizontally: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbCenterHorizontally'
      Properties.OnChange = PageSettingsChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
    end
    object cbCenterVertically: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbCenterVertically'
      Properties.OnChange = PageSettingsChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
    end
    object cbPaperSize: TcxComboBox
      Left = 108
      Top = 226
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = PageSettingsChanged
      Style.HotTrack = False
      TabOrder = 12
      Width = 255
    end
    object lbScaling: TcxLabel
      Left = 21
      Top = 138
      AutoSize = False
      Caption = 'lbScaling'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 16
      Width = 342
    end
    object seAdjustTo: TcxSpinEdit
      Left = 127
      Top = 160
      Properties.Increment = 5.000000000000000000
      Properties.OnChange = seAdjustToPropertiesChange
      Style.HotTrack = False
      TabOrder = 8
      Value = 10
      Width = 50
    end
    object sePagesWide: TcxSpinEdit
      Left = 127
      Top = 187
      Properties.OnChange = sePagesWidePropertiesChange
      Style.HotTrack = False
      TabOrder = 10
      Width = 50
    end
    object sePagesTall: TcxSpinEdit
      Left = 252
      Top = 187
      AutoSize = False
      Style.HotTrack = False
      TabOrder = 11
      Height = 21
      Width = 50
    end
    object rbAdjustTo: TcxRadioButton
      Left = 31
      Top = 162
      Width = 90
      Height = 17
      Caption = 'rbAdjustTo'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 7
      OnClick = rbAdjustToClick
      ParentBackground = False
      Transparent = True
    end
    object rbFitTo: TcxRadioButton
      Left = 31
      Top = 189
      Width = 90
      Height = 17
      Caption = 'rbFitTo'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 9
      OnClick = rbFitToClick
      ParentBackground = False
      Transparent = True
    end
    object cbHeaderTemplates: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbHeaderTemplatesPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Visible = False
      Width = 342
    end
    object cbFooterTemplates: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbFooterTemplatesPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 26
      Visible = False
      Width = 342
    end
    object btnCustomHeaderFooter: TcxButton
      Left = 10000
      Top = 10000
      Width = 160
      Height = 25
      Caption = 'btnCustomHeaderFooter'
      TabOrder = 25
      Visible = False
      OnClick = btnCustomHeaderFooterClick
    end
    object cbAlignWithPageMargins: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cbAlignWithPageMargins'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 28
      Transparent = True
      Visible = False
    end
    object cbScaleWithDocument: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'cxCheckBox1'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Transparent = True
      Visible = False
    end
    object pbHeaderPreview: TPaintBox
      Left = 10000
      Top = 10000
      Width = 342
      Height = 50
      Color = clBtnFace
      ParentColor = False
      Visible = False
      OnPaint = pbHeaderPreviewPaint
    end
    object pbFooterPreview: TPaintBox
      Left = 10000
      Top = 10000
      Width = 342
      Height = 50
      Color = clBtnFace
      ParentColor = False
      Visible = False
      OnPaint = pbFooterPreviewPaint
    end
    object btnPrint: TcxButton
      Left = 165
      Top = 405
      Width = 96
      Height = 25
      Caption = 'btnPrint'
      TabOrder = 1
      OnClick = btnPrintClick
    end
    object btnPrintPreview: TcxButton
      Left = 267
      Top = 405
      Width = 96
      Height = 25
      Caption = 'btnPrintPreview'
      TabOrder = 2
      OnClick = btnPrintPreviewClick
    end
    object beAreaSelector: TcxButtonEdit
      Left = 10
      Top = 10
      Properties.Buttons = <
        item
          Glyph.SourceDPI = 96
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000004649444154785ECDD3310A00200C43510F93FB5F310A8EC58690A10A
            5FB7072DB84846DDEB3C6E15304E0750F53DA073018E0078CC0E0934088C110A
            826407307610FC85A01CD878555AB18AC525EA0000000049454E44AE426082}
          Kind = bkGlyph
        end>
      Properties.ReadOnly = True
      Properties.OnButtonClick = beAreaSelectorPropertiesButtonClick
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 364
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lgButtons
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = lgButtons
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgTabs: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      OnTabChanged = lgTabsTabChanged
      Index = 1
    end
    object lgButtons: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object lgSheet: TdxLayoutGroup
      Parent = lgTabs
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 7
      Index = 3
    end
    object liPrintArea: TdxLayoutItem
      Parent = lgSheet
      CaptionOptions.Text = 'tePrintArea'
      Control = tePrintArea
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lgSheet
      CaptionOptions.Visible = False
      Control = lbPrint
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 46
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Offsets.Left = 10
      Control = cbPrintGridLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Offsets.Left = 10
      Control = cbPrintHeaders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liPrintCommentsMode: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahRight
      Control = cbPrintCommentsMode
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lgSheet
      LayoutDirection = ldHorizontal
      Index = 5
    end
    object liPrintCellErrorsMode: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahRight
      Control = cbPrintCellErrorsMode
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 140
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      Index = 1
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      Index = 0
    end
    object liBlackAndWhite: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Offsets.Left = 10
      Control = cbBlackAndWhite
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Padding.Left = 10
      Padding.AssignedValues = [lpavLeft]
      Control = cbDraftQuality
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = lgSheet
      CaptionOptions.Visible = False
      Control = lbPrintTitles
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liRowsToRepeat: TdxLayoutItem
      Parent = lgSheet
      CaptionOptions.Text = '1'
      Offsets.Left = 14
      Control = teRowsToRepeat
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liColumnsToRepeat: TdxLayoutItem
      Parent = lgSheet
      CaptionOptions.Text = '1'
      Offsets.Left = 14
      Control = teColumnsToRepeat
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = lgSheet
      CaptionOptions.Visible = False
      Control = lbPageOrder
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = rbtnDownThenOver
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = rbtnOverThenDown
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      AlignVert = avTop
      Offsets.Left = 10
      Control = pbxPageOrder
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 49
      ControlOptions.OriginalWidth = 74
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = lgSheet
      AlignHorz = ahClient
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 7
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      AlignVert = avCenter
      Index = 1
    end
    object lgPage: TdxLayoutGroup
      Parent = lgTabs
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = lgPage
      CaptionOptions.Visible = False
      Control = lbOrientation
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      Padding.Bottom = 4
      Padding.Left = 10
      Padding.Right = 4
      Padding.Top = 4
      Padding.AssignedValues = [lpavBottom, lpavLeft, lpavRight, lpavTop]
      Control = pbxPageOrientation
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 32
      ControlOptions.OriginalWidth = 32
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      CaptionOptions.Visible = False
      Control = rbPageOrientationPortrait
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = rbPageOrientationLandscape
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = lgPage
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      Index = 1
    end
    object liFirstPageNumber: TdxLayoutItem
      Parent = lgPage
      AlignHorz = ahLeft
      CaptionOptions.Text = 'FirstPageNumber'
      Control = meFirstPageNumber
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 45
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object lgMargins: TdxLayoutGroup
      Parent = lgTabs
      CaptionOptions.Text = 'New Group'
      SizeOptions.Height = 360
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 1
    end
    object liMarginLeft: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'liMarginLeft'
      CaptionOptions.Layout = clTop
      SizeOptions.Width = 75
      Control = seMarginLeft
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liMarginTop: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahCenter
      CaptionOptions.Text = 'MarginTop'
      CaptionOptions.Layout = clTop
      SizeOptions.Width = 75
      Control = seMarginTop
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liMarginRight: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignVert = avCenter
      CaptionOptions.Text = 'MarginRight'
      CaptionOptions.Layout = clTop
      SizeOptions.Width = 75
      Control = seMarginRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liMarginHeader: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Text = 'MarginHeader'
      CaptionOptions.Layout = clTop
      SizeOptions.Width = 75
      Control = seMarginHeader
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liMarginBottom: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahCenter
      CaptionOptions.Text = 'liMarginBottom'
      CaptionOptions.Layout = clTop
      SizeOptions.Width = 75
      Control = seMarginBottom
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liMarginFooter: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignVert = avBottom
      CaptionOptions.Text = 'liMarginFooter'
      CaptionOptions.Layout = clTop
      SizeOptions.Width = 75
      Control = seMarginFooter
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignVert = avTop
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = gbPagePreviewHolder
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 160
      ControlOptions.OriginalWidth = 160
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = lgMargins
      AlignHorz = ahCenter
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup9
      Index = 1
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup9
      Index = 2
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = lgMargins
      CaptionOptions.Visible = False
      Control = lbCenterOnPage
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = lgMargins
      CaptionOptions.Visible = False
      Offsets.Left = 10
      Control = cbCenterHorizontally
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 93
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = lgMargins
      CaptionOptions.Visible = False
      Offsets.Left = 10
      Control = cbCenterVertically
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 93
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liPaperSize: TdxLayoutItem
      Parent = lgPage
      AlignHorz = ahClient
      CaptionOptions.Text = 'liPaperSize'
      Control = cbPaperSize
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = lgPage
      CaptionOptions.Visible = False
      Control = lbScaling
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lgPage
      CaptionOptions.Text = 'Separator'
      Index = 6
    end
    object liAdjustTo: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      CaptionOptions.Text = 'liAdjustTo'
      CaptionOptions.Layout = clRight
      Control = seAdjustTo
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 50
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liPagesWide: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      CaptionOptions.Text = 'sePagesWide'
      CaptionOptions.Layout = clRight
      Control = sePagesWide
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 50
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liPagesTall: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      AlignVert = avClient
      CaptionOptions.Text = 'sePagesTall'
      CaptionOptions.Layout = clRight
      Control = sePagesTall
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 50
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Offsets.Left = 10
      Control = rbAdjustTo
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Offsets.Left = 10
      Control = rbFitTo
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = lgPage
      LayoutDirection = ldHorizontal
      Index = 4
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = lgPage
      LayoutDirection = ldHorizontal
      Index = 5
    end
    object lgHeaderFooter: TdxLayoutGroup
      Parent = lgTabs
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 2
    end
    object liHeader: TdxLayoutItem
      Parent = lgHeaderFooter
      CaptionOptions.Text = 'Caption:'
      CaptionOptions.Layout = clTop
      Control = cbHeaderTemplates
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liFooter: TdxLayoutItem
      Parent = lgHeaderFooter
      CaptionOptions.Text = 'Caption:'
      CaptionOptions.Layout = clTop
      Control = cbFooterTemplates
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = lgHeaderFooter
      AlignHorz = ahCenter
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = btnCustomHeaderFooter
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 160
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = lgHeaderFooter
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = cbAlignWithPageMargins
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 84
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = lgHeaderFooter
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = cbScaleWithDocument
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 84
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = lgHeaderFooter
      Control = pbHeaderPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 50
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = lgHeaderFooter
      CaptionOptions.Layout = clTop
      Control = pbFooterPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 50
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = lgPrintButtons
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnPrint
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = lgPrintButtons
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = btnPrintPreview
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lgPrintButtons: TdxLayoutGroup
      Parent = lgPage
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object liAreaSelector: TdxLayoutItem
      Parent = lcMainGroup_Root
      Visible = False
      Control = beAreaSelector
      ControlOptions.OriginalHeight = 20
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object LayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 440
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object EditRepository: TcxEditRepository
    Left = 304
    Top = 8
    PixelsPerInch = 96
    object ertiArea: TcxEditRepositoryButtonItem
      Properties.Buttons = <
        item
          Glyph.SourceDPI = 96
          Glyph.Data = {
            89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
            610000004C49444154785EBD92610A001014831D66F7BFE24352B2A219567BF2
            E7DB5BA488B0DC463954EF01A3F8EE03AE57E0C48504002543A84000181B7800
            541F559852D1C19F5E81F4F02339F60119D3D864CBC6C560CA0000000049454E
            44AE426082}
          Kind = bkGlyph
        end>
      Properties.ValidationErrorIconAlignment = taRightJustify
      Properties.ValidationOptions = [evoRaiseException, evoShowErrorIcon]
      Properties.OnButtonClick = ertiAreaPropertiesButtonClick
      Properties.OnValidate = ertiAreaPropertiesValidate
    end
    object ersiMargins: TcxEditRepositorySpinItem
      Properties.AssignedValues.MinValue = True
      Properties.EditFormat = '0.00'
      Properties.ValueType = vtFloat
      Properties.OnChange = PageSettingsChanged
    end
  end
  object ilPrintOrders: TcxImageList
    SourceDPI = 96
    BkColor = clWhite
    Height = 49
    Width = 74
    FormatVersion = 1
    DesignInfo = 524562
    ImageInfo = <
      item
        Image.Data = {
          DE380000424DDE3800000000000036000000280000004A000000310000000100
          200000000000A838000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000040000000C0000001000000011000000110000
          0012000000120000001300000013000000140000001400000015000000150000
          0016000000160000001600000017000000170000001800000018000000190000
          0019000000190000001A0000001A0000001A0000001500000006000000000000
          00040000000C0000001000000011000000110000001200000012000000130000
          0013000000140000001400000015000000150000001600000016000000160000
          00170000001700000018000000180000001900000019000000190000001A0000
          001A0000001A0000001500000006000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000BC89164FFC99164FFC891
          63FFC89162FFC79062FFC69061FFC68F61FFC68F61FFC68E60FFC68D5FFFC58D
          5FFFC58D5FFFC58D5EFFC48C5EFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFF0000
          0014000000000000000BC89164FFC99164FFC89163FFC89162FFC79062FFC690
          61FFC68F61FFC68F61FFC68E60FFC68D5FFFC58D5FFFC58D5FFFC58D5EFFC48C
          5EFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFF0000001400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000EC891
          64FFFFFFFFFFF7EEE5FFF7EEE5FFF6EDE4FFF6EDE4FFF6EDE3FFF6ECE3FFF6EC
          E2FFF6ECE1FFF5EBE1FFF6ECE1FFF5ECE0FFF5EBE0FFF6EBDFFFF5EBDEFFF6EA
          DEFFF5EADEFFF5E9DEFFF5E9DDFFF5E9DDFFF5E9DDFFF4E9DDFFF5E9DDFFF5E8
          DCFFC48C5DFF0000001A000000000000000EC89164FFFFFFFFFFF7EEE5FFF7EE
          E5FFF6EDE4FFF6EDE4FFF6EDE3FFF6ECE3FFF6ECE2FFF6ECE1FFF5EBE1FFF6EC
          E1FFF5ECE0FFF5EBE0FFF6EBDFFFF5EBDEFFF6EADEFFF5EADEFFF5E9DEFFF5E9
          DDFFF5E9DDFFF5E9DDFFF4E9DDFFF5E9DDFFF5E8DCFFC48C5DFF0000001A0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000DC89164FFFFFFFFFFFDFAF4FFFDF9F4FFFDF8F4FFFCF8F3FFFCF8
          F3FFFCF8F2FFFCF7F2FFFCF7F1FFFCF7F1FFFCF7F0FFFBF6F0FFFCF6EFFFFBF6
          EFFFFBF6EEFFFBF6EEFFFBF5EDFFFBF5EDFFFBF5ECFFFBF5ECFFFBF5ECFFFAF3
          EBFFFBF4EBFFF4E9DDFFC48C5DFF00000019000000000000000DC89164FFFFFF
          FFFFFDFAF4FFFDF9F4FFFDF8F4FFFCF8F3FFFCF8F3FFFCF8F2FFFCF7F2FFFCF7
          F1FFFCF7F1FFFCF7F0FFFBF6F0FFFCF6EFFFFBF6EFFFFBF6EEFFFBF6EEFFFBF5
          EDFFFBF5EDFFFBF5ECFFFBF5ECFFFBF5ECFFFAF3EBFFFBF4EBFFF4E9DDFFC48C
          5DFF000000190000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000CC89164FFFFFFFFFFFDF9F5FFFDF9F4FFFCF9
          F4FFFCF9F3FFFDF8F3FFFDF8F2FFFCF8F2FFFCF8F2FFFCF7F1FFFCF7F1FFFCF7
          F0FFFCF7F0FFFCF6F0FFFCF6EFFFFCF5EFFFFBF5EEFFFBF6EEFFFBF5EDFFFBF4
          EDFFFBF4EDFFFAF4ECFFFBF4EBFFF5E8DDFFC48C5DFF00000019000000000000
          000CC89164FFFFFFFFFFFDF9F5FFFDF9F4FFFCF9F4FFFCF9F3FFFDF8F3FFFDF8
          F2FFFCF8F2FFFCF8F2FFFCF7F1FFFCF7F1FFFCF7F0FFFCF7F0FFFCF6F0FFFCF6
          EFFFFCF5EFFFFBF5EEFFFBF6EEFFFBF5EDFFFBF4EDFFFBF4EDFFFAF4ECFFFBF4
          EBFFF5E8DDFFC48C5DFF00000019000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000CC89164FFFFFFFFFFFDFA
          F5FFFCF9F5FFFDF9F5FFFDF9F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8
          F2FFFCF8F1FFFCF7F0FFFCF7F0FFFCF6F0FFFCF6EFFFFBF6EEFFFBF5EFFFFBF6
          EEFFFBF5EEFFFBF5EDFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000
          0018000000000000000CC89164FFFFFFFFFFFDFAF5FFFCF9F5FFFDF9F5FFFDF9
          F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F2FFFCF8F1FFFCF7F0FFFCF7
          F0FFFCF6F0FFFCF6EFFFFBF6EEFFFBF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5
          EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000001800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000BC891
          64FFFFFFFFFFFDFAF5FFFDFAF6FFFDF9F5FFFDF9F4FFFDF9F4FFFDF8F3FFFCF8
          F3FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F1FFFCF8F1FFFCF6F0FFFCF7EFFFFCF7
          EFFFFBF6EFFFFBF5EFFFFBF5EEFFFBF5EEFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9
          DDFFC48C5DFF00000017000000000000000BC89164FFFFFFFFFFFDFAF5FFFDFA
          F6FFFDF9F5FFFDF9F4FFFDF9F4FFFDF8F3FFFCF8F3FFF9F5F0FFF1ECE7FFF0EC
          E6FFF9F4EEFFFCF8F1FFFCF6F0FFFCF7EFFFFCF7EFFFFBF6EFFFFBF5EFFFFBF5
          EEFFFBF5EEFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF000000170000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000AC89164FFFFFFFFFFFDFAF6FFFDFAF6FFFDF9F5FFFDF9F5FFFDF9
          F5FFFDF9F4FFFCF8F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F2FFFCF7F1FFFCF7
          F0FFFCF7F0FFFBF7F0FFFCF6EFFFFBF6EFFFFCF5EFFFFBF6EEFFFBF5EEFFFBF5
          EDFFFBF5ECFFF5EADDFFC48C5DFF00000017000000000000000AC89164FFFFFF
          FFFFFDFAF6FFFDFAF6FFFDF9F5FFFDF9F5FFFDF9F5FFFDF9F4FFF9F5F1FFE6E2
          DDFF458C42FF458C42FFE5E1DCFFF9F4EEFFFCF7F0FFFCF7F0FFFBF7F0FFFCF6
          EFFFFBF6EFFFFCF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5ECFFF5EADDFFC48C
          5DFF000000170000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000009C89164FFFFFFFFFFFDFBF7FFFDFBF7FFFDFA
          F6FFFDFAF6FFFDF9F5FFFDF9F4FFFCF9F4FFFDF9F4FFFDF8F3FFFCF8F3FFFAF5
          EFFFF0ECE6FFECE8E2FFECE7E2FFEDE9E3FFF3EFE9FFF9F4EDFFFBF6EFFFFBF5
          EFFFFCF6EEFFFBF5EEFFFCF5EDFFF5EADEFFC48C5DFF00000016000000000000
          0009C89164FFFFFFFFFFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDF9F5FFFAF6
          F1FFE6E3DFFF438B41FF278D30FF14841DFF428A3FFFE5E1DCFFF9F4EEFFFCF7
          F1FFFBF7F0FFFBF7F0FFFBF6EFFFFBF6EFFFFBF5EFFFFCF6EEFFFBF5EEFFFCF5
          EDFFF5EADEFFC48C5DFF00000016000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000009C89164FFFFFFFFFFFDFB
          F7FFFDFBF6FFFDFAF6FFFCFAF6FFFDFAF6FFFCF9F5FFFCFAF4FFFCF9F4FFFDF8
          F4FFFDF8F4FFF1EDE7FF007B00FF007B00FF007A00FF007A00FF7BAE75FFF0EB
          E5FFF9F4EDFFFCF6EFFFFBF6EEFFFBF5EEFFFBF5EDFFF5EADEFFC48C5DFF0000
          00150000000000000009C89164FFFFFFFFFFFDFBF7FFFDFBF6FFFDFAF6FFFCFA
          F6FFFAF7F3FFE7E4E0FF438D41FF288E30FF29983BFF29983BFF14841DFF428A
          3FFFE5E1DCFFF9F5EEFFFCF7F1FFFCF7F0FFFCF7F0FFFBF6EFFFFCF6EFFFFBF6
          EEFFFBF5EEFFFBF5EDFFF5EADEFFC48C5DFF0000001500000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000008C891
          64FFFFFFFFFFFDFBF8FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF6FFFCFAF6FFFCF9
          F5FFFDF9F4FFFCF9F4FFFCF8F4FFEEEAE5FF007C00FF80CD95FF38B057FF31A8
          4DFF07810BFF7BAC75FFEFEAE5FFF9F5EDFFFCF6EFFFFCF6EEFFFBF6EEFFF5EA
          DEFFC48C5DFF000000150000000000000008C89164FFFFFFFFFFFDFBF8FFFDFB
          F7FFFDFAF6FFFAF7F3FFE8E5E2FF448E42FF288F30FF29993DFF29983CFF2998
          3BFF28983BFF14841DFF428A3FFFE5E1DBFFF9F4EEFFFCF7F0FFFCF6F1FFFBF6
          F0FFFBF7EFFFFCF6EFFFFCF6EEFFFBF6EEFFF5EADEFFC48C5DFF000000150000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000007C89164FFFFFFFFFFFEFCF8FFFDFBF8FFFDFBF7FFFDFBF7FFFDFA
          F6FFFDFAF6FFFDFAF5FFFDFAF5FFFDF9F4FFFCF9F4FFEEECE6FF007D00FF81CE
          96FF39B159FF38B057FF40AE59FF07810BFF7BAD75FFF0EBE5FFFAF4EDFFFCF6
          EFFFFCF6EFFFF5EBDFFFC48C5DFF000000140000000000000007C89164FFFFFF
          FFFFFEFCF8FFFDFBF8FFFAF8F4FFE9E7E3FF458E43FF3C9943FF77BE84FF77BE
          84FF76BE83FF29983CFF28983BFF76BD81FF3C9641FF428B40FFE5E1DCFFF9F4
          EFFFFCF7F0FFFCF7F0FFFCF6F0FFFCF6EFFFFCF6EFFFFCF6EFFFF5EBDFFFC48C
          5DFF000000140000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000006C89164FFFFFFFFFFFEFBF8FFFDFBF8FFFEFB
          F8FFFDFBF7FFFDFBF7FFFDFBF6FFFDFAF6FFFDF9F6FFFDFAF5FFFDF9F5FFEFEC
          E7FF007C00FF82CE97FF3AB25BFF2BA445FF7DCB91FF40AE59FF07810BFF7BAD
          75FFF0ECE5FFFAF4EDFFFCF6EFFFF5EBDFFFC48C5DFF00000013000000000000
          0006C89164FFFFFFFFFFFEFBF8FFFDFBF8FFF6F3F0FF4B964AFF007300FF0073
          00FF007200FF007100FF77BE83FF29993CFF007100FF007000FF007000FF006F
          00FF4A9146FFF3EFE8FFFCF7F1FFFCF7F0FFFCF7F0FFFCF7F0FFFCF6EFFFFCF6
          EFFFF5EBDFFFC48C5DFF00000013000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000006C89164FFFFFFFFFFFDFC
          F9FFFDFBF8FFFEFCF8FFFDFBF8FFFDFBF7FFFDFAF7FFFDFAF7FFFDFAF6FFFDFA
          F5FFFDFAF5FFF0EDE9FF007D00FF82CF98FF3AB35CFF037F04FF62BA72FF7DCB
          92FF40AF59FF07810BFF7BAD75FFF0ECE5FFFAF4EDFFF6EBE0FFC48C5DFF0000
          00120000000000000006C89164FFFFFFFFFFFDFCF9FFFDFBF8FFFBF9F5FFF1F0
          EDFFEAE8E4FFE8E5E3FFD8D5D3FF007200FF77BF84FF2A9A3EFF007000FFC6CA
          C0FFE4E2DCFFE8E4DFFFEFEBE6FFF9F5EFFFFDF8F2FFFCF7F1FFFCF7F1FFFCF7
          F1FFFCF7F0FFFCF6EFFFF6EBE0FFC48C5DFF0000001200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000005C891
          64FFFFFFFFFFFEFCF9FFFDFBF8FFFDFBF9FFFDFBF8FFFDFBF7FFFDFBF7FFFEFB
          F7FFFDFAF6FFFDFAF6FFFDFAF5FFF1EEE9FF007D00FF82D098FF3BB45DFF007C
          00FF0C820DFF62BA72FF7DCB92FF40AF59FF07810BFF7BAE75FFF1ECE5FFF3E9
          DFFFC48C5DFF000000120000000000000005C89164FFFFFFFFFFFEFCF9FFFDFB
          F8FFFDFBF9FFFDFBF8FFFDFBF7FFFDFBF7FFE9E6E3FF007200FF77C084FF2B9B
          3EFF007100FFD7D9CFFFFBF7F3FFFCF9F4FFFDF9F3FFFCF8F3FFFCF8F2FFFCF7
          F2FFFCF8F2FFFCF7F1FFFCF7F0FFFCF7F0FFF5EBE1FFC48C5DFF000000120000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000005C89164FFFFFFFFFFFDFCF9FFFDFCF9FFFDFCF9FFFDFBF8FFFDFB
          F8FFFDFBF8FFFEFBF7FFFDFAF7FFFDFAF6FFFDFAF6FFF1EEEAFF007E00FF82D1
          99FF3CB65EFF007D00FF91B68BFF0D820EFF62BA72FF7DCB92FF40AF59FF0781
          0BFF7BAE75FFEBE0D7FFC38B5CFF000000110000000000000005C89164FFFFFF
          FFFFFDFCF9FFFDFCF9FFFDFCF9FFFDFBF8FFFDFBF8FFFDFBF8FFEAE7E3FF0072
          00FF77C085FF2B9B40FF007100FFD7D9D1FFFBF8F2FFFCF9F4FFFDF9F4FFFDF8
          F3FFFDF8F3FFFCF7F2FFFCF7F2FFFCF7F2FFFCF7F1FFFCF7F1FFF6EBE1FFC48C
          5DFF000000110000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000004C89164FFFFFFFFFFFEFCF9FFFEFCF9FFFEFC
          F9FFFDFCF8FFFDFCF8FFFEFBF8FFFEFBF7FFFDFBF8FFFDFBF6FFFDFAF7FFF2EF
          EBFF007F00FF84D29AFF3CB660FF007E00FFD9DED2FFA3C39DFF0D830EFF62BA
          72FF7DCB92FF40AF59FF07810BFF78A86FFFBB8659FF00000012000000000000
          0004C89164FFFFFFFFFFFEFCF9FFFEFCF9FFFEFCF9FFFDFCF8FFFDFCF8FFFEFB
          F8FFEAE7E3FF007300FF78C086FF2B9D41FF007200FFD8DBD0FFFBF7F3FFFDF9
          F5FFFCF9F4FFFCF9F4FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F2FFFCF7F1FFFBF7
          F1FFF6ECE2FFC48C5DFF00000010000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003C99265FFFFFFFFFFFDFD
          FAFFFEFCF9FFFDFCF9FFFEFCF9FFFEFBF8FFFEFBF8FFFDFBF8FFFDFBF8FFFEFB
          F7FFFDFBF7FFF2EFEBFF007F00FF84D29BFF3EB862FF007E00FFDFE5D8FFF5F2
          EDFFA4C49FFF0D830EFF62BA72FF7DCB92FF40AF59FF07810BFF61792EFF0000
          001A0000000200000003C99265FFFFFFFFFFFDFDFAFFFEFCF9FFFDFCF9FFFEFC
          F9FFFEFBF8FFFEFBF8FFE9E8E5FF007300FF79C187FF2C9D42FF007200FFD8DB
          D2FFFBF8F5FFFCF9F6FFFDF9F5FFFCF9F4FFFCF9F4FFFCF9F4FFFCF9F3FFFCF8
          F2FFFCF8F2FFFCF7F2FFF5ECE2FFC58C5EFF0000000F00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000003C993
          65FFFFFFFFFFFEFCFBFFFEFDFAFFFEFCFAFFFEFCFAFFFEFCF9FFFEFCF9FFFDFC
          F9FFFEFBF9FFFDFBF8FFFEFBF8FFF3F1EDFF008000FF84D39CFF3EB962FF007E
          00FFE1E6DAFFFBF8F3FFF6F2EDFFA4C49EFF0D830EFF62BA72FF7DCB92FF40AF
          59FF07810BFF003500890000000B00000005C99365FFFFFFFFFFFEFCFBFFFEFD
          FAFFFEFCFAFFFEFCFAFFFEFCF9FFFEFCF9FFEAE9E6FF007400FF79C287FF2C9E
          42FF007300FFD8DDD3FFFCF9F5FFFDFAF6FFFDFAF5FFFDFAF5FFFCF9F4FFFDF9
          F4FFFCF8F3FFFDF9F3FFFCF8F3FFFCF8F2FFF6EDE2FFC58C5EFF0000000E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000003C99365FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5FF008000FF85D4
          9DFF3FBA64FF007E00FFE4EBE4FFFEFEFEFFFEFEFEFFF8F8F8FFA6C9A6FF0D83
          0EFF62BA73FF7DCB92FF40AF59FF07800BFF003500820000000EC89264FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECECECFF0074
          00FF79C288FF2D9F44FF007300FFDAE1DAFFFEFEFEFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC58D
          5FFF0000000E0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000002CA9465FFC99366FFC99265FFC99265FFC891
          64FFC89164FFC89163FFC79062FFC79062FFC78F61FFC78F61FFC68F60FFBF89
          5CFF008000FF86D49EFF3FBB65FF007F00FFB08655FFC48E5FFFC58E5FFFC48E
          5FFFC08A5CFF80803EFF0C800AFF62BA73FF7DCB92FF40AF59FF07810BFF0035
          0083C18D61FFC89265FFC99265FFC99265FFC89164FFC89164FFC89163FFC790
          62FFB9865BFF007400FF79C389FF2DA045FF007300FFA97F51FFC48D5EFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFFC58E5FFFC58E5FFF0000000A000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002000000030000
          0003000000030000000300000003000000040000000400000004000000050000
          0005000000050000000F008000FF86D59EFF40BC66FF007F00FF000700220000
          000900000009000000090000000B0000001000230060067C07F862BA73FF7DCB
          92FF40AF59FF07810BFF003500830000000E0000000500000003000000030000
          0003000000040000000400000016007500FF7AC489FF2FA146FF007400FF0007
          0029000000080000000700000008000000080000000800000009000000090000
          000A0000000A0000000B0000000B000000090000000300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000009008100FF86D59FFF40BD68FF0080
          00FF0007001A0000000100000000000000000000000000000001000000070023
          0059067C07F862BA72FF7DCB92FF40AF59FF07810BFF003500820000000B0000
          00020000000000000000000000000000000000000012007500FF7AC48AFF2FA1
          47FF007400FF0007002400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000040000000C00000010000000110000001100000012000000120000
          001300000013000000140000001400000015000000150000001E008100FF86D6
          A0FF42BE6AFF008000FF0008002F000000190000001900000019000000190000
          001A0000001B0000001F00230066067B07F862BA72FF7DCC92FF40AF59FF0780
          0BFF0034008A0000001C00000014000000120000001300000013000000240076
          00FF7BC58BFF30A349FF007500FF000700370000001700000017000000170000
          0018000000180000001900000019000000190000001A0000001A0000001A0000
          0015000000060000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000BC89164FFC99164FFC89163FFC89162FFC790
          62FFC69061FFC68F61FFC68F61FFC68E60FFC68D5FFFC58D5FFFC58D5FFFBF89
          5BFF008100FF86D7A0FF43BF6AFF008100FFB18654FFC38C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC38C5DFFBF885BFF00230065067B07F862BA
          72FF7DCC92FF40B059FF07810BFF637C31FFBF8A5EFFC58F60FFC68F61FFC68F
          61FFB9855AFF007600FF7BC58CFF30A449FF007500FFA97F51FFC38B5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFF00000014000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000EC89164FFFFFFFFFFF7EE
          E5FFF7EEE5FFF6EDE4FFF6EDE4FFF6EDE3FFF6ECE3FFF6ECE2FFF6ECE1FFF5EB
          E1FFF6ECE1FFEEE5D9FF008200FF87D7A2FF43C06BFF008100FFDDDAC8FFF4E8
          DDFFF5E9DDFFF5E9DDFFF5E9DDFFF4E9DDFFF5E9DDFFF5E8DCFFC38C5DFF0000
          001F00230059067C07F863BA73FF7DCB92FF40AF59FF07800BFF79AA70FFECE3
          DAFFF4EBE2FFF6ECE3FFE6DDD4FF007700FF7CC68DFF31A54BFF007500FFD4D2
          C2FFF5EADEFFF5EBDEFFF6EADEFFF5EADEFFF5E9DEFFF5E9DDFFF5E9DDFFF5E9
          DDFFF4E9DDFFF5E9DDFFF5E8DCFFC48C5DFF0000001A00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000DC891
          64FFFFFFFFFFFDFAF4FFFDF9F4FFFDF8F4FFFCF8F3FFFCF8F3FFFCF8F2FFFCF7
          F2FFFCF7F1FFFCF7F1FFFCF7F0FFF4EFE9FF008300FF87D8A2FF44C16DFF0081
          00FFE3E6D7FFFAF4ECFFFBF5ECFFFBF5ECFFFBF5ECFFFAF3EBFFFBF4EBFFF4E9
          DDFFC48C5DFF0000001A00000006002300600C800AFF62BA73FF7DCC92FF40AF
          59FF07810BFF7DB078FFF1EDE9FFFAF6F0FFECE8E3FF007700FF7DC78DFF32A6
          4CFF007600FFDADDCFFFFAF5EEFFFBF6EEFFFBF6EEFFFBF5EDFFFBF5EDFFFBF5
          ECFFFBF5ECFFFBF5ECFFFAF3EBFFFBF4EBFFF4E9DDFFC48C5DFF000000190000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000CC89164FFFFFFFFFFFDF9F5FFFDF9F4FFFCF9F4FFFCF9F3FFFDF8
          F3FFFDF8F2FFFCF8F2FFFCF8F2FFFCF7F1FFFCF7F1FFF5F0EAFF008200FF88D8
          A2FF44C26DFF008200FFE4E6D8FFFAF5EDFFFBF5EDFFFBF4EDFFFBF4EDFFFAF4
          ECFFFBF4EBFFF5E8DDFFC48C5DFF000000190000000100000012838241FF0D83
          0EFF62BA72FF7DCB92FF40AF59FF07810BFF7DAF78FFF2EEE8FFECE8E2FF0078
          00FF7DC88EFF32A74EFF007600FFDBDED1FFFBF5EFFFFCF6EFFFFCF5EFFFFBF5
          EEFFFBF6EEFFFBF5EDFFFBF4EDFFFBF4EDFFFAF4ECFFFBF4EBFFF5E8DDFFC48C
          5DFF000000190000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000CC89164FFFFFFFFFFFDFAF5FFFCF9F5FFFDF9
          F5FFFDF9F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F2FFFCF8F1FFF6F1
          EAFF008300FF88D9A3FF44C36FFF008200FFE4E6D9FFFAF5EDFFFBF5EEFFFBF5
          EDFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF00000018000000000000
          000DC38D62FFA7CAA7FF0D820EFF62BA73FF7DCB92FF40AF59FF07800BFF7DB0
          78FFE3E0DBFF007800FF7DC990FF34A94FFF007600FFDBDED1FFFBF5EFFFFCF6
          EFFFFBF6EEFFFBF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5EDFFFBF5ECFFFBF4
          ECFFF5E9DDFFC48C5DFF00000018000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000BC89164FFFFFFFFFFFDFA
          F5FFFDFAF6FFFDF9F5FFFDF9F4FFFDF9F4FFFDF8F3FFFCF8F3FFFCF8F3FFFDF8
          F2FFFCF8F2FFF6F1EBFF008300FF89D9A4FF45C370FF008300FFE4E8D9FFFAF4
          EEFFFBF5EEFFFBF5EEFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000
          0017000000000000000BC79164FFF9F9F9FFA6C7A1FF0D830EFF62BA73FF7DCB
          92FF40B059FF07810BFF76A972FF007800FF7EC990FF34AA50FF007700FFDCDF
          D2FFFBF5EFFFFCF7EFFFFCF7EFFFFBF6EFFFFBF5EFFFFBF5EEFFFBF5EEFFFBF5
          EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000001700000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000AC891
          64FFFFFFFFFFFDFAF6FFFDFAF6FFFDF9F5FFFDF9F5FFFDF9F5FFFDF9F4FFFCF8
          F4FFFCF8F3FFFCF8F3FFFCF8F3FFF6F2EDFF008300FF89DAA4FF45C571FF0082
          00FFE6E8D9FFFAF5EFFFFCF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5ECFFF5EA
          DDFFC48C5DFF00000017000000000000000AC89164FFFEFEFEFFF7F4F0FFA6C7
          A1FF0D830EFF63BA73FF7DCB92FF40AF59FF07800BFF007A00FF75C689FF35AB
          52FF007800FFDCDFD3FFFBF6EFFFFCF7F0FFFBF7F0FFFCF6EFFFFBF6EFFFFCF5
          EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5ECFFF5EADDFFC48C5DFF000000170000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000009C89164FFFFFFFFFFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDF9
          F5FFFDF9F4FFFCF9F4FFFDF9F4FFFDF8F3FFFCF8F3FFF8F3EDFF008400FF89DB
          A5FF46C571FF008300FFE5E9DBFFFBF6EFFFFBF6EFFFFBF5EFFFFCF6EEFFFBF5
          EEFFFCF5EDFFF5EADEFFC48C5DFF000000160000000000000009C89164FFFFFF
          FFFFFCFAF6FFF7F5F1FFA6C7A1FF0D830EFF62BA73FF7DCB92FF40AF5AFF0780
          0BFF3FB15BFF36AC53FF007800FFDDE0D4FFFBF6F0FFFCF7F1FFFBF7F0FFFBF7
          F0FFFBF6EFFFFBF6EFFFFBF5EFFFFCF6EEFFFBF5EEFFFCF5EDFFF5EADEFFC48C
          5DFF000000160000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000009C89164FFFFFFFFFFFDFBF7FFFDFBF6FFFDFA
          F6FFFCFAF6FFFDFAF6FFFCF9F5FFFCFAF4FFFCF9F4FFFDF8F4FFFDF8F4FFF7F3
          EDFF008300FF89DBA6FF47C573FF008300FFE7EADBFFFCF7F0FFFBF6EFFFFCF6
          EFFFFBF6EEFFFBF5EEFFFBF5EDFFF5EADEFFC48C5DFF00000015000000000000
          0009C89164FFFFFFFFFFFDFBF7FFFCFAF5FFF7F4F0FFA6C7A1FF0D820EFF62BA
          73FF7DCB92FF40B059FF37AE56FF36AD54FF007800FFDDE0D5FFFBF6F1FFFCF8
          F1FFFCF7F1FFFCF7F0FFFCF7F0FFFBF6EFFFFCF6EFFFFBF6EEFFFBF5EEFFFBF5
          EDFFF5EADEFFC48C5DFF00000015000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000008C89164FFFFFFFFFFFDFB
          F8FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF6FFFCFAF6FFFCF9F5FFFDF9F4FFFCF9
          F4FFFCF8F4FFF7F3EEFF008400FF89DBA6FF47C673FF008300FFE7EADBFFFCF6
          F1FFFBF6F0FFFBF7EFFFFCF6EFFFFCF6EEFFFBF6EEFFF5EADEFFC48C5DFF0000
          00150000000000000008C89164FFFFFFFFFFFDFBF8FFFDFBF7FFFCF9F5FFF7F4
          F0FFA6C7A2FF0D820EFF63BA73FF7DCC92FF49B666FF37AF56FF007900FFDEE2
          D5FFFBF7F1FFFCF7F1FFFCF7F1FFFCF7F0FFFCF6F1FFFBF6F0FFFBF7EFFFFCF6
          EFFFFCF6EEFFFBF6EEFFF5EADEFFC48C5DFF0000001500000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000007C891
          64FFFFFFFFFFFEFCF8FFFDFBF8FFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFA
          F5FFFDFAF5FFFDF9F4FFFCF9F4FFF8F5EFFF008400FF8ADBA6FF8ADBA6FF0084
          00FFE8EBDEFFFCF7F0FFFCF7F0FFFCF6F0FFFCF6EFFFFCF6EFFFFCF6EFFFF5EB
          DFFFC48C5DFF000000140000000000000007C89164FFFFFFFFFFFEFCF8FFFDFB
          F8FFFDFBF7FFFCFAF6FFF7F4F0FFA6C7A2FF0D820EFF62BA73FF80CD95FF80CD
          94FF108413FFDFE1D7FFFCF7F2FFFCF8F3FFFCF7F2FFFCF7F2FFFCF7F0FFFCF7
          F0FFFCF6F0FFFCF6EFFFFCF6EFFFFCF6EFFFF5EBDFFFC48C5DFF000000140000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000006C89164FFFFFFFFFFFEFBF8FFFDFBF8FFFEFBF8FFFDFBF7FFFDFB
          F7FFFDFBF6FFFDFAF6FFFDF9F6FFFDFAF5FFFDF9F5FFF9F6F1FF008400FF0084
          00FF008400FF008400FFE8ECDFFFFCF7F1FFFCF7F0FFFCF7F0FFFCF7F0FFFCF6
          EFFFFCF6EFFFF5EBDFFFC48C5DFF000000130000000000000006C89164FFFFFF
          FFFFFEFBF8FFFDFBF8FFFEFBF8FFFDFBF7FFFCFAF6FFF7F5F0FFA7C8A2FF007B
          00FF007B00FF007B00FF007A00FFE2E6DBFFFCF9F4FFFDF8F3FFFCF8F2FFFCF8
          F1FFFCF7F1FFFCF7F0FFFCF7F0FFFCF7F0FFFCF6EFFFFCF6EFFFF5EBDFFFC48C
          5DFF000000130000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000006C89164FFFFFFFFFFFDFCF9FFFDFBF8FFFEFC
          F8FFFDFBF8FFFDFBF7FFFDFAF7FFFDFAF7FFFDFAF6FFFDFAF5FFFDFAF5FFFCF8
          F4FFF9F6F1FFF8F5EFFFF8F4EFFFF8F4EFFFFBF7F1FFFDF8F2FFFCF7F1FFFCF7
          F1FFFCF7F1FFFCF7F0FFFCF6EFFFF6EBE0FFC48C5DFF00000012000000000000
          0006C89164FFFFFFFFFFFDFCF9FFFDFBF8FFFEFCF8FFFDFBF8FFFDFBF7FFFCF9
          F6FFF9F6F3FFF4F1EDFFF1EEEAFFF1EEE9FFF3EFECFFF9F6F1FFFCF9F3FFFDF9
          F3FFFCF8F3FFFCF8F2FFFDF8F2FFFCF7F1FFFCF7F1FFFCF7F1FFFCF7F0FFFCF6
          EFFFF6EBE0FFC48C5DFF00000012000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000005C89164FFFFFFFFFFFEFC
          F9FFFDFBF8FFFDFBF9FFFDFBF8FFFDFBF7FFFDFBF7FFFEFBF7FFFDFAF6FFFDFA
          F6FFFDFAF5FFFDFAF5FFFDF9F5FFFDF9F5FFFCF9F4FFFDF9F3FFFCF8F3FFFCF8
          F2FFFCF7F2FFFCF8F2FFFCF7F1FFFCF7F0FFFCF7F0FFF5EBE1FFC48C5DFF0000
          00120000000000000005C89164FFFFFFFFFFFEFCF9FFFDFBF8FFFDFBF9FFFDFB
          F8FFFDFBF7FFFDFBF7FFFEFBF7FFFDFAF6FFFDFAF6FFFDFAF5FFFDFAF5FFFDF9
          F5FFFDF9F5FFFCF9F4FFFDF9F3FFFCF8F3FFFCF8F2FFFCF7F2FFFCF8F2FFFCF7
          F1FFFCF7F0FFFCF7F0FFF5EBE1FFC48C5DFF0000001200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000005C891
          64FFFFFFFFFFFDFCF9FFFDFCF9FFFDFCF9FFFDFBF8FFFDFBF8FFFDFBF8FFFEFB
          F7FFFDFAF7FFFDFAF6FFFDFAF6FFFDFAF5FFFDF9F6FFFDFAF4FFFCF9F4FFFDF9
          F4FFFDF8F3FFFDF8F3FFFCF7F2FFFCF7F2FFFCF7F2FFFCF7F1FFFCF7F1FFF6EB
          E1FFC48C5DFF000000110000000000000005C89164FFFFFFFFFFFDFCF9FFFDFC
          F9FFFDFCF9FFFDFBF8FFFDFBF8FFFDFBF8FFFEFBF7FFFDFAF7FFFDFAF6FFFDFA
          F6FFFDFAF5FFFDF9F6FFFDFAF4FFFCF9F4FFFDF9F4FFFDF8F3FFFDF8F3FFFCF7
          F2FFFCF7F2FFFCF7F2FFFCF7F1FFFCF7F1FFF6EBE1FFC48C5DFF000000110000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000004C89164FFFFFFFFFFFEFCF9FFFEFCF9FFFEFCF9FFFDFCF8FFFDFC
          F8FFFEFBF8FFFEFBF7FFFDFBF8FFFDFBF6FFFDFAF7FFFDFAF6FFFDFAF5FFFDF9
          F5FFFDF9F5FFFCF9F4FFFCF9F4FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F2FFFCF7
          F1FFFBF7F1FFF6ECE2FFC48C5DFF000000100000000000000004C89164FFFFFF
          FFFFFEFCF9FFFEFCF9FFFEFCF9FFFDFCF8FFFDFCF8FFFEFBF8FFFEFBF7FFFDFB
          F8FFFDFBF6FFFDFAF7FFFDFAF6FFFDFAF5FFFDF9F5FFFDF9F5FFFCF9F4FFFCF9
          F4FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F2FFFCF7F1FFFBF7F1FFF6ECE2FFC48C
          5DFF000000100000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000003C99265FFFFFFFFFFFDFDFAFFFEFCF9FFFDFC
          F9FFFEFCF9FFFEFBF8FFFEFBF8FFFDFBF8FFFDFBF8FFFEFBF7FFFDFBF7FFFDFA
          F6FFFDFAF6FFFDFAF6FFFCF9F6FFFDF9F5FFFCF9F4FFFCF9F4FFFCF9F4FFFCF9
          F3FFFCF8F2FFFCF8F2FFFCF7F2FFF5ECE2FFC58C5EFF0000000F000000000000
          0003C99265FFFFFFFFFFFDFDFAFFFEFCF9FFFDFCF9FFFEFCF9FFFEFBF8FFFEFB
          F8FFFDFBF8FFFDFBF8FFFEFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF6FFFCF9
          F6FFFDF9F5FFFCF9F4FFFCF9F4FFFCF9F4FFFCF9F3FFFCF8F2FFFCF8F2FFFCF7
          F2FFF5ECE2FFC58C5EFF0000000F000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003C99365FFFFFFFFFFFEFC
          FBFFFEFDFAFFFEFCFAFFFEFCFAFFFEFCF9FFFEFCF9FFFDFCF9FFFEFBF9FFFDFB
          F8FFFEFBF8FFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF5FFFDFAF5FFFCF9
          F4FFFDF9F4FFFCF8F3FFFDF9F3FFFCF8F3FFFCF8F2FFF6EDE2FFC58C5EFF0000
          000E0000000000000003C99365FFFFFFFFFFFEFCFBFFFEFDFAFFFEFCFAFFFEFC
          FAFFFEFCF9FFFEFCF9FFFDFCF9FFFEFBF9FFFDFBF8FFFEFBF8FFFDFBF7FFFDFB
          F7FFFDFAF6FFFDFAF6FFFDFAF5FFFDFAF5FFFCF9F4FFFDF9F4FFFCF8F3FFFDF9
          F3FFFCF8F3FFFCF8F2FFF6EDE2FFC58C5EFF0000000E00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000003C993
          65FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFC58D5FFF0000000E0000000000000003C99365FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC58D5FFF0000000E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000002CA9465FFC99366FFC99265FFC99265FFC89164FFC89164FFC891
          63FFC79062FFC79062FFC78F61FFC78F61FFC68F60FFC68E60FFC68D5FFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFF0000000A0000000000000002CA9465FFC993
          66FFC99265FFC99265FFC89164FFC89164FFC89163FFC79062FFC79062FFC78F
          61FFC78F61FFC68F60FFC68E60FFC68D5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFF0000000A0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000300000003000000030000
          0003000000030000000400000004000000040000000500000005000000050000
          0006000000060000000700000007000000080000000800000008000000090000
          00090000000A0000000A0000000B0000000B0000000900000003000000000000
          0001000000020000000300000003000000030000000300000003000000040000
          0004000000040000000500000005000000050000000600000006000000070000
          000700000008000000080000000800000009000000090000000A0000000A0000
          000B0000000B0000000900000003000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          DE380000424DDE3800000000000036000000280000004A000000310000000100
          200000000000A838000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000040000000C0000001000000011000000110000
          0012000000120000001300000013000000140000001400000015000000150000
          0016000000160000001600000017000000170000001800000018000000190000
          0019000000190000001A0000001A0000001A0000001500000006000000000000
          00040000000C0000001000000011000000110000001200000012000000130000
          0013000000140000001400000015000000150000001600000016000000160000
          00170000001700000018000000180000001900000019000000190000001A0000
          001A0000001A0000001500000006000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000BC89164FFC99164FFC891
          63FFC89162FFC79062FFC69061FFC68F61FFC68F61FFC68E60FFC68D5FFFC58D
          5FFFC58D5FFFC58D5EFFC48C5EFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFF0000
          0014000000000000000BC89164FFC99164FFC89163FFC89162FFC79062FFC690
          61FFC68F61FFC68F61FFC68E60FFC68D5FFFC58D5FFFC58D5FFFC58D5EFFC48C
          5EFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFF0000001400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000EC891
          64FFFFFFFFFFF7EEE5FFF7EEE5FFF6EDE4FFF6EDE4FFF6EDE3FFF6ECE3FFF6EC
          E2FFF6ECE1FFF5EBE1FFF6ECE1FFF5ECE0FFF5EBE0FFF6EBDFFFF5EBDEFFF6EA
          DEFFF5EADEFFF5E9DEFFF5E9DDFFF5E9DDFFF5E9DDFFF4E9DDFFF5E9DDFFF5E8
          DCFFC48C5DFF0000001A000000000000000EC89164FFFFFFFFFFF7EEE5FFF7EE
          E5FFF6EDE4FFF6EDE4FFF6EDE3FFF6ECE3FFF6ECE2FFF6ECE1FFF5EBE1FFF6EC
          E1FFF5ECE0FFF5EBE0FFF6EBDFFFF5EBDEFFF6EADEFFF5EADEFFF5E9DEFFF5E9
          DDFFF5E9DDFFF5E9DDFFF4E9DDFFF5E9DDFFF5E8DCFFC48C5DFF0000001A0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000DC89164FFFFFFFFFFFDFAF4FFFDF9F4FFFDF8F4FFFCF8F3FFFCF8
          F3FFFCF8F2FFFCF7F2FFFCF7F1FFFCF7F1FFFCF7F0FFFBF6F0FFFCF6EFFFFBF6
          EFFFFBF6EEFFFBF6EEFFFBF5EDFFFBF5EDFFFBF5ECFFFBF5ECFFFBF5ECFFFAF3
          EBFFFBF4EBFFF4E9DDFFC48C5DFF00000019000000000000000DC89164FFFFFF
          FFFFFDFAF4FFFDF9F4FFFDF8F4FFFCF8F3FFFCF8F3FFF9F5EFFFF3EEE9FFF9F4
          EEFFFCF7F1FFFCF7F0FFFBF6F0FFFCF6EFFFFBF6EFFFFBF6EEFFFBF6EEFFFBF5
          EDFFFBF5EDFFFBF5ECFFFBF5ECFFFBF5ECFFFAF3EBFFFBF4EBFFF4E9DDFFC48C
          5DFF000000190000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000CC89164FFFFFFFFFFFDF9F5FFFDF9F4FFFCF9
          F4FFFCF9F3FFFDF8F3FFFDF8F2FFFCF8F2FFFCF8F2FFFCF7F1FFFCF7F1FFFCF7
          F0FFFCF7F0FFFCF6F0FFFCF6EFFFFCF5EFFFFBF5EEFFFBF6EEFFFBF5EDFFFBF4
          EDFFFBF4EDFFFAF4ECFFFBF4EBFFF5E8DDFFC48C5DFF00000019000000000000
          000CC89164FFFFFFFFFFFDF9F5FFFDF9F4FFFCF9F4FFFCF9F3FFFDF8F3FFF0EB
          E5FF4A9146FFE5E2DCFFF9F4EEFFFCF7F1FFFCF7F0FFFCF7F0FFFCF6F0FFFCF6
          EFFFFCF5EFFFFBF5EEFFFBF6EEFFFBF5EDFFFBF4EDFFFBF4EDFFFAF4ECFFFBF4
          EBFFF5E8DDFFC48C5DFF00000019000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000CC89164FFFFFFFFFFFDFA
          F5FFFCF9F5FFFDF9F5FFFDF9F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8
          F2FFFCF8F1FFFCF7F0FFFCF7F0FFFCF6F0FFFCF6EFFFFBF6EEFFFBF5EFFFFBF6
          EEFFFBF5EEFFFBF5EDFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000
          0018000000000000000CC89164FFFFFFFFFFFDFAF5FFFCF9F5FFFDF9F5FFFDF9
          F4FFFCF8F3FFE7E3DFFF006F00FF428B40FFE5E1DCFFF9F5EEFFFCF7F0FFFCF7
          F0FFFCF6F0FFFCF6EFFFFBF6EEFFFBF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5
          EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000001800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000BC891
          64FFFFFFFFFFFDFAF5FFFDFAF6FFFDF9F5FFFDF9F4FFFDF9F4FFFDF8F3FFFCF8
          F3FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F1FFFCF8F1FFFCF6F0FFFBF6EEFFFBF6
          EEFFFAF5EEFFFAF4EEFFFAF4EDFFFAF4EDFFFAF4ECFFFAF4EBFFFAF3EBFFF4E8
          DCFFC38B5DFF00000018000000010000000CC79063FFFEFEFEFFFCF9F4FFFBF8
          F5FFFBF7F3FFFBF7F2FFFBF7F2FFE5E1DCFF007000FF3C9641FF428A3FFFE5E1
          DCFFF9F4EEFFFCF8F1FFFCF6F0FFFCF7EFFFFCF7EFFFFBF6EFFFFBF5EFFFFBF5
          EEFFFBF5EEFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF000000170000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000AC89164FFFFFFFFFFFDFAF6FFFDFAF6FFFDF9F5FFFDF9F5FFFDF9
          F5FFFDF9F4FFFCF8F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F2FFF9F4EEFFE2E4
          D8FFDEE1D4FFDDE0D4FFDDE0D2FFDCDFD2FFDCDED1FFDBDED0FFDADDCFFFDADC
          CEFFD9DCCCFFD4D1BFFFA97F50FF00070037000700240007002DAB8356FFDAE1
          DAFFD8DCD2FFD8DBD2FFD8DAD0FFD7D9D0FFD7D9CFFFC7CAC0FF007000FF76BD
          81FF14841DFF428A40FFE5E1DCFFF9F4EEFFFCF7F0FFFCF7F0FFFBF7F0FFFCF6
          EFFFFBF6EFFFFCF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5ECFFF5EADDFFC48C
          5DFF000000170000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000009C89164FFFFFFFFFFFDFBF7FFFDFBF7FFFDFA
          F6FFFDFAF6FFFDF9F5FFFDF9F4FFFCF9F4FFFDF9F4FFFDF8F3FFFCF8F3FFFDF8
          F2FFF2EEE9FF007A00FF108413FF007900FF007800FF007800FF007800FF0077
          00FF007600FF007600FF007600FF007500FF007500FF007500FF007400FF0074
          00FF007300FF007300FF007300FF007200FF007200FF007100FF007100FF0070
          00FF007100FF28983BFF28983BFF14841DFF428A3FFFE5E1DCFFF9F4EEFFFCF7
          F1FFFBF7F0FFFBF7F0FFFBF6EFFFFBF6EFFFFBF5EFFFFCF6EEFFFBF5EEFFFCF5
          EDFFF5EADEFFC48C5DFF00000016000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000009C89164FFFFFFFFFFFDFB
          F7FFFDFBF6FFFDFAF6FFFCFAF6FFFDFAF6FFFCF9F5FFFCFAF4FFFCF9F4FFFDF8
          F4FFFDF8F4FFFCF8F2FFF0ECE7FF007B00FF80CD94FF37AF56FF36AD54FF36AC
          53FF35AB52FF34AA50FF34A94FFF32A74EFF32A64CFF31A54BFF30A449FF30A3
          49FF2FA147FF2FA146FF2DA045FF2D9F44FF2C9E42FF2C9D42FF2B9D41FF2B9B
          40FF2B9B3EFF2A9A3EFF29993CFF29983CFF29983BFF29983BFF14841DFF458C
          42FFF0EBE6FFFCF8F1FFFCF7F1FFFCF7F0FFFCF7F0FFFBF6EFFFFCF6EFFFFBF6
          EEFFFBF5EEFFFBF5EDFFF5EADEFFC48C5DFF0000001500000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000008C891
          64FFFFFFFFFFFDFBF8FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF6FFFCFAF6FFFCF9
          F5FFFDF9F4FFFCF9F4FFFCF8F4FFFCF8F3FFF0EDE7FF007B00FF80CD95FF49B6
          66FF37AE56FF3FB15BFF75C689FF7EC990FF7DC990FF7DC88EFF7DC78DFF7CC6
          8DFF7BC58CFF7BC58BFF7AC48AFF7AC489FF79C389FF79C288FF79C287FF79C1
          87FF78C086FF77C085FF77C084FF77BF84FF77BE83FF76BE83FF29983CFF2998
          3BFF278D30FF458D42FFF0ECE7FFFCF7F1FFFCF7F1FFFCF7F0FFFCF6F1FFFBF6
          F0FFFBF7EFFFFCF6EFFFFCF6EEFFFBF6EEFFF5EADEFFC48C5DFF000000150000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000007C89164FFFFFFFFFFFEFCF8FFFDFBF8FFFDFBF7FFFDFBF7FFFDFA
          F6FFFDFAF6FFFDFAF5FFFDFAF5FFFDF9F4FFFCF9F4FFFCF9F3FFF4EFEAFF007B
          00FF62BA73FF7DCC92FF40B059FF07800BFF007A00FF007800FF007800FF0078
          00FF007700FF007700FF007600FF007600FF007500FF007500FF007400FF0074
          00FF007400FF007300FF007300FF007200FF007200FF007200FF007100FF77BE
          84FF29993DFF288E30FF438B40FFE7E2DDFFFAF5F0FFFCF8F3FFFCF7F2FFFCF7
          F2FFFCF7F0FFFCF7F0FFFCF6F0FFFCF6EFFFFCF6EFFFFCF6EFFFF5EBDFFFC48C
          5DFF000000140000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000006C89164FFFFFFFFFFFEFBF8FFFDFBF8FFFEFB
          F8FFFDFBF7FFFDFBF7FFFDFBF6FFFDFAF6FFFDF9F6FFFDFAF5FFFDF9F5FFFCF9
          F4FFF8F5F0FFA6C7A1FF0D820EFF63BA73FF7DCB92FF40AF5AFF07800BFF76A8
          70FFE3DFD9FFECE6E0FFECE7E0FFE5DCD1FFB78357FF00000023000000120000
          0017B9865DFFECECECFFEBE8E5FFE9E8E5FFEAE7E4FFE9E7E3FFE8E6E3FFD8D6
          D2FF007200FF77BE84FF288F30FF438C41FFE6E3DFFFF9F6F1FFFCF9F4FFFDF8
          F3FFFCF8F2FFFCF8F1FFFCF7F1FFFCF7F0FFFCF7F0FFFCF7F0FFFCF6EFFFFCF6
          EFFFF5EBDFFFC48C5DFF00000013000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000006C89164FFFFFFFFFFFDFC
          F9FFFDFBF8FFFEFCF8FFFDFBF8FFFDFBF7FFFDFAF7FFFDFAF7FFFDFAF6FFFDFA
          F5FFFDFAF5FFFDF9F5FFFBF8F3FFF6F3EDFFA6C7A0FF0D820EFF62BA73FF7DCB
          92FF40AF59FF07810BFF7DAF77FFF1EDE6FFFAF4EDFFF6EBE0FFC48C5DFF0000
          00120000000000000006C89164FFFFFFFFFFFDFCF9FFFDFBF8FFFEFCF8FFFDFB
          F8FFFDFBF7FFE8E5E3FF007300FF3C9943FF448E42FFE8E5E0FFFAF6F2FFFCF9
          F4FFFCF9F3FFFDF9F3FFFCF8F3FFFCF8F2FFFDF8F2FFFCF7F1FFFCF7F1FFFCF7
          F1FFFCF7F0FFFCF6EFFFF6EBE0FFC48C5DFF0000001200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000005C891
          64FFFFFFFFFFFEFCF9FFFDFBF8FFFDFBF9FFFDFBF8FFFDFBF7FFFDFBF7FFFEFB
          F7FFFDFAF6FFFDFAF6FFFDFAF5FFFDFAF5FFFDF9F5FFFCF8F4FFF6F3EEFFA6C7
          A0FF0D820EFF62BA73FF7DCB92FF40B059FF07800BFF7DAF77FFF1ECE6FFF3E9
          E0FFC48C5DFF000000120000000000000005C89164FFFFFFFFFFFEFCF9FFFDFB
          F8FFFDFBF9FFFDFBF8FFFDFBF7FFEAE8E4FF007300FF458E43FFE8E5E2FFFAF7
          F2FFFDFAF5FFFDF9F5FFFDF9F5FFFCF9F4FFFDF9F3FFFCF8F3FFFCF8F2FFFCF7
          F2FFFCF8F2FFFCF7F1FFFCF7F0FFFCF7F0FFF5EBE1FFC48C5DFF000000120000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000005C89164FFFFFFFFFFFDFCF9FFFDFCF9FFFDFCF9FFFDFBF8FFFDFB
          F8FFFDFBF8FFFEFBF7FFFDFAF7FFFDFAF6FFFDFAF6FFFDFAF5FFFDF9F6FFFDFA
          F4FFFBF8F3FFF7F3EEFFA6C6A0FF0D830EFF63BA73FF7DCB92FF40AF59FF0781
          0BFF7DAF77FFECE1D7FFC38B5CFF000000110000000000000005C89164FFFFFF
          FFFFFDFCF9FFFDFCF9FFFDFCF9FFFDFBF8FFFDFBF8FFF1F0EDFF4C964AFFE9E6
          E3FFFAF7F3FFFDFAF6FFFDFAF5FFFDF9F6FFFDFAF4FFFCF9F4FFFDF9F4FFFDF8
          F3FFFDF8F3FFFCF7F2FFFCF7F2FFFCF7F2FFFCF7F1FFFCF7F1FFF6EBE1FFC48C
          5DFF000000110000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000004C89164FFFFFFFFFFFEFCF9FFFEFCF9FFFEFC
          F9FFFDFCF8FFFDFCF8FFFEFBF8FFFEFBF7FFFDFBF8FFFDFBF6FFFDFAF7FFFDFA
          F6FFFDFAF5FFFDF9F5FFFDF9F5FFFBF8F3FFF6F3EEFFA6C6A0FF0D830EFF62BA
          73FF7DCB92FF40AF59FF07810BFF79A96FFFBC8659FF00000012000000000000
          0004C89164FFFFFFFFFFFEFCF9FFFEFCF9FFFEFCF9FFFDFCF8FFFDFCF8FFFBF8
          F5FFF6F3EFFFFAF8F5FFFDFBF6FFFDFAF7FFFDFAF6FFFDFAF5FFFDF9F5FFFDF9
          F5FFFCF9F4FFFCF9F4FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F2FFFCF7F1FFFBF7
          F1FFF6ECE2FFC48C5DFF00000010000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003C99265FFFFFFFFFFFDFD
          FAFFFEFCF9FFFDFCF9FFFEFCF9FFFEFBF8FFFEFBF8FFFDFBF8FFFDFBF8FFFEFB
          F7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF6FFFCF9F6FFFDF9F5FFFBF8F3FFF6F3
          EEFFA6C7A0FF0D830EFF62BA73FF7DCB92FF40AF59FF07800BFF617A2EFF0000
          00190000000200000003C99265FFFFFFFFFFFDFDFAFFFEFCF9FFFDFCF9FFFEFC
          F9FFFEFBF8FFFEFBF8FFFDFBF8FFFDFBF8FFFEFBF7FFFDFBF7FFFDFAF6FFFDFA
          F6FFFDFAF6FFFCF9F6FFFDF9F5FFFCF9F4FFFCF9F4FFFCF9F4FFFCF9F3FFFCF8
          F2FFFCF8F2FFFCF7F2FFF5ECE2FFC58C5EFF0000000F00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000003C993
          65FFFFFFFFFFFEFCFBFFFEFDFAFFFEFCFAFFFEFCFAFFFEFCF9FFFEFCF9FFFDFC
          F9FFFEFBF9FFFDFBF8FFFEFBF8FFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFA
          F5FFFDFAF5FFFBF8F3FFF7F3EEFFA5C69FFF0D820EFF62BA72FF7DCC92FF40AF
          59FF07810BFF003500880000000B00000005C99365FFFFFFFFFFFEFCFBFFFEFD
          FAFFFEFCFAFFFEFCFAFFFEFCF9FFFEFCF9FFFDFCF9FFFEFBF9FFFDFBF8FFFEFB
          F8FFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF5FFFDFAF5FFFCF9F4FFFDF9
          F4FFFCF8F3FFFDF9F3FFFCF8F3FFFCF8F2FFF6EDE2FFC58C5EFF0000000E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000003C99365FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEFFF9F9F9FFA7CAA7FF0D83
          0EFF62BA73FF7DCB92FF40B059FF07800BFF003500820000000EC89264FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC58D
          5FFF0000000E0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000002CA9465FFC99366FFC99265FFC99265FFC891
          64FFC89164FFC89163FFC79062FFC79062FFC78F61FFC78F61FFC68F60FFC68E
          60FFC68D5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC48E
          5FFFC08A5DFF81803EFF0C800AFF63BA73FF7DCC92FF40AF59FF07810BFF0035
          0083C18D61FFC89265FFC99265FFC99265FFC89164FFC89164FFC89163FFC790
          62FFC79062FFC78F61FFC78F61FFC68F60FFC68E60FFC68D5FFFC58E5FFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFFC58E5FFFC58E5FFF0000000A000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002000000030000
          0003000000030000000300000003000000040000000400000004000000050000
          0005000000050000000600000006000000070000000700000008000000080000
          000800000009000000090000000B0000001000230060067C07F862BA72FF7DCC
          92FF40AF59FF07810BFF003500830000000E0000000500000003000000030000
          0003000000040000000400000004000000050000000500000005000000060000
          0006000000070000000700000008000000080000000800000009000000090000
          000A0000000A0000000B0000000B000000090000000300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000060023
          0059067B07F862BA72FF7DCB92FF40AF59FF07810BFF003500820000000B0000
          0002000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000040000000C00000010000000110000001100000012000000120000
          0013000000130000001400000014000000150000001500000016000000160000
          0016000000170000001700000018000000180000001900000019000000190000
          001A0000001B0000001F00230066067B07F862BA72FF7DCB92FF40AF59FF0780
          0BFF0035008B0000001C00000014000000120000001300000013000000140000
          0014000000150000001500000016000000160000001600000017000000170000
          0018000000180000001900000019000000190000001A0000001A0000001A0000
          0015000000060000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000BC89164FFC99164FFC89163FFC89162FFC790
          62FFC69061FFC68F61FFC68F61FFC68E60FFC68D5FFFC58D5FFFC58D5FFFC58D
          5EFFC48C5EFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC38C5DFFBF885BFF00230065067C07F862BA
          73FF7DCB92FF40AF59FF07810BFF627C30FFBE8A5EFFC58F60FFC68F61FFC68F
          61FFC68E60FFC68D5FFFC58D5FFFC58D5FFFC58D5EFFC48C5EFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C5DFFC48C
          5DFFC48C5DFFC48C5DFF00000014000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000EC89164FFFFFFFFFFF7EE
          E5FFF7EEE5FFF6EDE4FFF6EDE4FFF6EDE3FFF6ECE3FFF6ECE2FFF6ECE1FFF5EB
          E1FFF6ECE1FFF5ECE0FFF5EBE0FFF6EBDFFFF5EBDEFFF6EADEFFF5EADEFFF5E9
          DEFFF5E9DDFFF5E9DDFFF5E9DDFFF4E9DDFFF5E9DDFFF5E8DCFFC38C5DFF0000
          001F00230059067C07F862BA73FF7DCB92FF40AF59FF07810BFF78A86FFFEBE2
          DAFFF4EBE1FFF6ECE3FFF6ECE2FFF6ECE1FFF5EBE1FFF6ECE1FFF5ECE0FFF5EB
          E0FFF6EBDFFFF5EBDEFFF6EADEFFF5EADEFFF5E9DEFFF5E9DDFFF5E9DDFFF5E9
          DDFFF4E9DDFFF5E9DDFFF5E8DCFFC48C5DFF0000001A00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000DC891
          64FFFFFFFFFFFDFAF4FFFDF9F4FFFDF8F4FFFCF8F3FFFCF8F3FFFCF8F2FFFCF7
          F2FFFCF7F1FFFCF7F1FFFCF7F0FFFBF6F0FFFCF6EFFFFBF6EFFFFBF6EEFFFBF6
          EEFFFBF5EDFFFBF5EDFFFBF5ECFFFBF5ECFFFBF5ECFFFAF3EBFFFBF4EBFFF4E9
          DDFFC48C5DFF0000001A00000007002300610C800AFF62BA73FF7DCB92FF40AF
          59FF07810BFF7BAE77FFF1EDE8FFFAF6F0FFFCF7F2FFFCF7F1FFFCF7F1FFFCF7
          F0FFFBF6F0FFFCF6EFFFFBF6EFFFFBF6EEFFFBF6EEFFFBF5EDFFFBF5EDFFFBF5
          ECFFFBF5ECFFFBF5ECFFFAF3EBFFFBF4EBFFF4E9DDFFC48C5DFF000000190000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000CC89164FFFFFFFFFFFDF9F5FFFDF9F4FFFCF9F4FFFCF9F3FFFDF8
          F3FFFDF8F2FFFCF8F2FFFCF8F2FFFCF7F1FFFCF7F1FFFCF7F0FFFCF7F0FFFCF6
          F0FFFCF6EFFFFCF5EFFFFBF5EEFFFBF6EEFFFBF5EDFFFBF4EDFFFBF4EDFFFAF4
          ECFFFBF4EBFFF5E8DDFFC48C5DFF000000190000000100000012838241FF0D83
          0EFF62BA72FF7DCB92FF40AF59FF07810BFF7CAE77FFF1EDE7FFFAF6F0FFFCF8
          F2FFFCF7F1FFFCF7F1FFFCF7F0FFFCF7F0FFFCF6F0FFFCF6EFFFFCF5EFFFFBF5
          EEFFFBF6EEFFFBF5EDFFFBF4EDFFFBF4EDFFFAF4ECFFFBF4EBFFF5E8DDFFC48C
          5DFF000000190000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000CC89164FFFFFFFFFFFDFAF5FFFCF9F5FFFDF9
          F5FFFDF9F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F2FFFCF8F1FFFCF7
          F0FFFCF7F0FFFCF6F0FFFCF6EFFFFBF6EEFFFBF5EFFFFBF6EEFFFBF5EEFFFBF5
          EDFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF00000018000000000000
          000DC38D61FFA6C9A6FF0D830EFF62BA72FF7DCB92FF40AF59FF07810BFF7BAE
          77FFF0EDE8FFFAF6F1FFFCF8F2FFFCF8F1FFFCF7F0FFFCF7F0FFFCF6F0FFFCF6
          EFFFFBF6EEFFFBF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5EDFFFBF5ECFFFBF4
          ECFFF5E9DDFFC48C5DFF00000018000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000BC89164FFFFFFFFFFFDFA
          F5FFFDFAF6FFFDF9F5FFFDF9F4FFFDF9F4FFFDF8F3FFFCF8F3FFFCF8F3FFFDF8
          F2FFFCF8F2FFFCF7F1FFFCF8F1FFFCF6F0FFFCF7EFFFFCF7EFFFFBF6EFFFFBF5
          EFFFFBF5EEFFFBF5EEFFFBF5EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000
          0017000000000000000BC79164FFF8F8F8FFA4C59FFF0D830EFF62BA72FF7DCB
          92FF40AF59FF07810BFF7BAE76FFF0ECE8FFFBF6F0FFFCF8F2FFFCF7F1FFFCF8
          F1FFFCF6F0FFFCF7EFFFFCF7EFFFFBF6EFFFFBF5EFFFFBF5EEFFFBF5EEFFFBF5
          EDFFFBF5ECFFFBF4ECFFF5E9DDFFC48C5DFF0000001700000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000AC891
          64FFFFFFFFFFFDFAF6FFFDFAF6FFFDF9F5FFFDF9F5FFFDF9F5FFFDF9F4FFFCF8
          F4FFFCF8F3FFFCF8F3FFFCF8F3FFFCF8F2FFFCF7F1FFFCF7F0FFFCF7F0FFFBF7
          F0FFFCF6EFFFFBF6EFFFFCF5EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5ECFFF5EA
          DDFFC48C5DFF00000017000000000000000AC89164FFFEFEFEFFF6F3EFFFA4C5
          A0FF0D830EFF62BA72FF7DCB92FF40AF59FF07810BFF7BAD76FFF0ECE8FFFAF6
          F1FFFCF8F2FFFCF7F1FFFCF7F0FFFCF7F0FFFBF7F0FFFCF6EFFFFBF6EFFFFCF5
          EFFFFBF6EEFFFBF5EEFFFBF5EDFFFBF5ECFFF5EADDFFC48C5DFF000000170000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000009C89164FFFFFFFFFFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDF9
          F5FFFDF9F4FFFCF9F4FFFDF9F4FFFDF8F3FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7
          F1FFFCF7F1FFFBF7F0FFFBF7F0FFFBF6EFFFFAF5EFFFFAF4EEFFFBF5EDFFFAF4
          EDFFFBF4ECFFF4E9DDFFC38C5DFF00000017000000010000000AC79164FFFEFE
          FEFFFCFAF6FFF6F4F0FFA4C49FFF0D820EFF62BA72FF7DCB92FF40AE59FF0781
          0BFF7BAD76FFF0ECE7FFFBF6F0FFFCF8F2FFFCF7F1FFFCF7F1FFFBF7F0FFFBF7
          F0FFFBF6EFFFFBF6EFFFFBF5EFFFFCF6EEFFFBF5EEFFFCF5EDFFF5EADEFFC48C
          5DFF000000160000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000009C89164FFFFFFFFFFFDFBF7FFFDFBF6FFFDFA
          F6FFFCFAF6FFFDFAF6FFFCF9F5FFFCFAF4FFFCF9F4FFFDF8F4FFFDF8F4FFFCF8
          F2FFFBF7F1FFE8EBDFFFE8EBDDFFE7EADCFFE7EADBFFE6E9DBFFE5E8D9FFE5E8
          D9FFE4E7D8FFE4E6D8FFE3E6D7FFDDDAC8FFB18654FF0008002C0007001A0007
          0023B38959FFE4EBE4FFE1E6DCFFE0E6DAFFDADFD4FF90B88DFF0C820DFF62BA
          72FF7DCB91FF40AE59FF07810BFF7CAF77FFF4F0EBFFFCF8F2FFFCF7F2FFFCF8
          F1FFFCF7F1FFFCF7F0FFFCF7F0FFFBF6EFFFFCF6EFFFFBF6EEFFFBF5EEFFFBF5
          EDFFF5EADEFFC48C5DFF00000015000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000008C89164FFFFFFFFFFFDFB
          F8FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF6FFFCFAF6FFFCF9F5FFFDF9F4FFFCF9
          F4FFFCF8F4FFFCF8F3FFF8F5EEFF008400FF008400FF008300FF008300FF0083
          00FF008200FF008300FF008200FF008200FF008100FF008100FF008100FF0080
          00FF008000FF007F00FF007F00FF007E00FF007E00FF007E00FF007E00FF007D
          00FF007C00FF037F04FF2BA445FF38B057FF31A84DFF007A00FFEEEAE5FFFCF9
          F2FFFCF8F2FFFCF7F1FFFCF7F1FFFCF7F0FFFCF6F1FFFBF6F0FFFBF7EFFFFCF6
          EFFFFCF6EEFFFBF6EEFFF5EADEFFC48C5DFF0000001500000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000007C891
          64FFFFFFFFFFFEFCF8FFFDFBF8FFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFA
          F5FFFDFAF5FFFDF9F4FFFCF9F4FFFCF9F3FFF8F3EFFF008400FF8ADBA6FF47C6
          73FF47C573FF46C571FF45C571FF45C370FF44C36FFF44C26DFF44C16DFF43C0
          6BFF43BF6AFF42BE6AFF40BD68FF40BC66FF3FBB65FF3FBA64FF3EB962FF3EB8
          62FF3CB660FF3CB65EFF3BB45DFF3AB35CFF3AB25BFF39B159FF38B057FF007A
          00FFECE9E4FFFDF8F3FFFDF8F3FFFCF8F3FFFCF7F2FFFCF7F2FFFCF7F0FFFCF7
          F0FFFCF6F0FFFCF6EFFFFCF6EFFFFCF6EFFFF5EBDFFFC48C5DFF000000140000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000006C89164FFFFFFFFFFFEFBF8FFFDFBF8FFFEFBF8FFFDFBF7FFFDFB
          F7FFFDFBF6FFFDFAF6FFFDF9F6FFFDFAF5FFFDF9F5FFFCF9F4FFF8F5F0FF0084
          00FF8ADBA6FF89DBA6FF89DBA6FF89DBA5FF89DAA4FF89D9A4FF88D9A3FF88D8
          A2FF87D8A2FF87D7A2FF86D7A0FF86D6A0FF86D59FFF86D59EFF86D49EFF85D4
          9DFF84D39CFF84D29BFF84D29AFF82D199FF82D098FF82CF98FF82CE97FF81CE
          96FF80CD95FF007B00FFECEAE5FFFCF9F4FFFCF9F4FFFDF8F3FFFCF8F2FFFCF8
          F1FFFCF7F1FFFCF7F0FFFCF7F0FFFCF7F0FFFCF6EFFFFCF6EFFFF5EBDFFFC48C
          5DFF000000130000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000006C89164FFFFFFFFFFFDFCF9FFFDFBF8FFFEFC
          F8FFFDFBF8FFFDFBF7FFFDFAF7FFFDFAF7FFFDFAF6FFFDFAF5FFFDFAF5FFFDF9
          F5FFF9F6F1FF008400FF008400FF008400FF008300FF008400FF008300FF0083
          00FF008300FF008200FF008300FF008200FF008100FF008100FF008100FF0080
          00FF008000FF008000FF008000FF007F00FF007F00FF007E00FF007D00FF007D
          00FF007C00FF007D00FF007C00FF007B00FFF1EDE9FFFCF9F4FFFCF9F3FFFDF9
          F3FFFCF8F3FFFCF8F2FFFDF8F2FFFCF7F1FFFCF7F1FFFCF7F1FFFCF7F0FFFCF6
          EFFFF6EBE0FFC48C5DFF00000012000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000005C89164FFFFFFFFFFFEFC
          F9FFFDFBF8FFFDFBF9FFFDFBF8FFFDFBF7FFFDFBF7FFFEFBF7FFFDFAF6FFFDFA
          F6FFFDFAF5FFFDFAF5FFFCF8F4FFFAF6F2FFF8F5F0FFF8F4EEFFF7F3EEFFF7F3
          EDFFF6F2EDFFF6F2ECFFF6F1EBFFF5F0EAFFF5F0E9FFEEE4DAFFBE885AFF0000
          001A000000090000000DC08C60FFF5F5F5FFF4F2EFFFF2F0EDFFF2F0EEFFF1EF
          EDFFF1EFEBFFF0EEEBFFF1EEEAFFEFECE9FFEFECE8FFF2EFEAFFFAF7F2FFFDF9
          F5FFFDF9F5FFFCF9F4FFFDF9F3FFFCF8F3FFFCF8F2FFFCF7F2FFFCF8F2FFFCF7
          F1FFFCF7F0FFFCF7F0FFF5EBE1FFC48C5DFF0000001200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000005C891
          64FFFFFFFFFFFDFCF9FFFDFCF9FFFDFCF9FFFDFBF8FFFDFBF8FFFDFBF8FFFEFB
          F7FFFDFAF7FFFDFAF6FFFDFAF6FFFDFAF5FFFDF9F6FFFDFAF4FFFCF9F4FFFDF9
          F4FFFDF8F3FFFDF8F3FFFCF7F2FFFCF7F2FFFCF7F2FFFCF7F1FFFCF7F1FFF6EB
          E1FFC48C5DFF000000110000000000000005C89164FFFFFFFFFFFDFCF9FFFDFC
          F9FFFDFCF9FFFDFBF8FFFDFBF8FFFDFBF8FFFEFBF7FFFDFAF7FFFDFAF6FFFDFA
          F6FFFDFAF5FFFDF9F6FFFDFAF4FFFCF9F4FFFDF9F4FFFDF8F3FFFDF8F3FFFCF7
          F2FFFCF7F2FFFCF7F2FFFCF7F1FFFCF7F1FFF6EBE1FFC48C5DFF000000110000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000004C89164FFFFFFFFFFFEFCF9FFFEFCF9FFFEFCF9FFFDFCF8FFFDFC
          F8FFFEFBF8FFFEFBF7FFFDFBF8FFFDFBF6FFFDFAF7FFFDFAF6FFFDFAF5FFFDF9
          F5FFFDF9F5FFFCF9F4FFFCF9F4FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F2FFFCF7
          F1FFFBF7F1FFF6ECE2FFC48C5DFF000000100000000000000004C89164FFFFFF
          FFFFFEFCF9FFFEFCF9FFFEFCF9FFFDFCF8FFFDFCF8FFFEFBF8FFFEFBF7FFFDFB
          F8FFFDFBF6FFFDFAF7FFFDFAF6FFFDFAF5FFFDF9F5FFFDF9F5FFFCF9F4FFFCF9
          F4FFFCF8F3FFFDF8F2FFFCF8F2FFFCF7F2FFFCF7F1FFFBF7F1FFF6ECE2FFC48C
          5DFF000000100000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000003C99265FFFFFFFFFFFDFDFAFFFEFCF9FFFDFC
          F9FFFEFCF9FFFEFBF8FFFEFBF8FFFDFBF8FFFDFBF8FFFEFBF7FFFDFBF7FFFDFA
          F6FFFDFAF6FFFDFAF6FFFCF9F6FFFDF9F5FFFCF9F4FFFCF9F4FFFCF9F4FFFCF9
          F3FFFCF8F2FFFCF8F2FFFCF7F2FFF5ECE2FFC58C5EFF0000000F000000000000
          0003C99265FFFFFFFFFFFDFDFAFFFEFCF9FFFDFCF9FFFEFCF9FFFEFBF8FFFEFB
          F8FFFDFBF8FFFDFBF8FFFEFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF6FFFCF9
          F6FFFDF9F5FFFCF9F4FFFCF9F4FFFCF9F4FFFCF9F3FFFCF8F2FFFCF8F2FFFCF7
          F2FFF5ECE2FFC58C5EFF0000000F000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003C99365FFFFFFFFFFFEFC
          FBFFFEFDFAFFFEFCFAFFFEFCFAFFFEFCF9FFFEFCF9FFFDFCF9FFFEFBF9FFFDFB
          F8FFFEFBF8FFFDFBF7FFFDFBF7FFFDFAF6FFFDFAF6FFFDFAF5FFFDFAF5FFFCF9
          F4FFFDF9F4FFFCF8F3FFFDF9F3FFFCF8F3FFFCF8F2FFF6EDE2FFC58C5EFF0000
          000E0000000000000003C99365FFFFFFFFFFFEFCFBFFFEFDFAFFFEFCFAFFFEFC
          FAFFFEFCF9FFFEFCF9FFFDFCF9FFFEFBF9FFFDFBF8FFFEFBF8FFFDFBF7FFFDFB
          F7FFFDFAF6FFFDFAF6FFFDFAF5FFFDFAF5FFFCF9F4FFFDF9F4FFFCF8F3FFFDF9
          F3FFFCF8F3FFFCF8F2FFF6EDE2FFC58C5EFF0000000E00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000003C993
          65FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFC58D5FFF0000000E0000000000000003C99365FFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC58D5FFF0000000E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000002CA9465FFC99366FFC99265FFC99265FFC89164FFC89164FFC891
          63FFC79062FFC79062FFC78F61FFC78F61FFC68F60FFC68E60FFC68D5FFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFF0000000A0000000000000002CA9465FFC993
          66FFC99265FFC99265FFC89164FFC89164FFC89163FFC79062FFC79062FFC78F
          61FFC78F61FFC68F60FFC68E60FFC68D5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E5FFFC58E
          5FFF0000000A0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000300000003000000030000
          0003000000030000000400000004000000040000000500000005000000050000
          0006000000060000000700000007000000080000000800000008000000090000
          00090000000A0000000A0000000B0000000B0000000900000003000000000000
          0001000000020000000300000003000000030000000300000003000000040000
          0004000000040000000500000005000000050000000600000006000000070000
          000700000008000000080000000800000009000000090000000A0000000A0000
          000B0000000B0000000900000003000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end>
  end
  object ilLarge: TcxImageList
    SourceDPI = 96
    Height = 32
    Width = 32
    FormatVersion = 1
    DesignInfo = 524532
    ImageInfo = <
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000010000000200000004000000060000000700000007000000070000
          0007000000070000000800000008000000080000000800000008000000080000
          0008000000080000000800000008000000080000000800000008000000080000
          0008000000070000000500000003000000010000000000000000000000000000
          0000000000020000000800000011000000180000001B0000001C0000001D0000
          001D0000001D0000001D0000001D0000001E0000001E0000001E0000001E0000
          001F0000001F0000001F0000001F0000001F0000002000000020000000200000
          001F0000001C0000001400000009000000020000000000000000000000000000
          000100000004000000117E5E52C1AF8271FFAE8172FFAE8171FFAE8070FFAD80
          70FFAD7F70FFAC7F6FFFAC7E6EFFAC7E6EFFAB7E6DFFAB7D6DFFAB7D6CFFAB7D
          6CFFAA7C6CFFAA7B6BFFA97C6BFFA97A6AFFA97A6AFFA97A6AFFA87969FFA879
          69FFA87969FF78564AC300000014000000050000000100000000000000000000
          00010000000600000016B08374FFFDFCFAFFFBF8F6FFFBF8F5FFFBF7F5FFFBF7
          F4FFFAF7F4FFFBF6F3FFFBF6F3FFFAF6F2FFFAF5F2FFFAF5F1FFFAF4F1FFF9F4
          F1FFF9F4F0FFF9F3EFFFF8F2EEFFF8F3EEFFF8F2EDFFF8F2ECFFF8F1ECFFF7F0
          ECFFF7F0EBFFA8796AFF0000001B000000070000000100000000000000000000
          00010000000600000018B18576FFFDFCFBFFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFF6EDE7FFF5EDE6FFF5ECE6FFF5ECE6FFF6ECE6FFF5ECE5FFF5EBE5FFF5EC
          E5FFF4EBE4FFF5EBE4FFF5EBE4FFF5EAE4FFF4EBE3FFF5EAE3FFF4EAE3FFF4EA
          E3FFF8F1ECFFA97B6AFF0000001E000000080000000100000000000000000000
          00010000000600000018B38777FFFEFCFBFFF6EEE9FFF7EFE8FFF6EFE9FFF6EE
          E8FFF6EDE8FFF6EDE7FFF6EDE7FFF5EDE7FFF6EDE7FFF5ECE6FFF5EDE6FFF5EC
          E6FFF5ECE5FFF5ECE5FFF5ECE5FFF4ECE5FFF4EBE4FFF5EBE4FFF5EAE4FFF5EA
          E3FFF9F2EDFFAA7D6CFF0000001E000000070000000100000000000000000000
          00010000000600000017B48979FFFEFCFBFFF6F0EAFFF7EFEAFFF6EFEAFFF6EF
          E9FFF7EEE8FFF7EEE8FFF6EEE8FFF6EEE7FFF6EEE7FFF6EDE7FFF5EDE6FFF6ED
          E6FFF6ECE6FFF5EDE6FFF5ECE5FFF5EBE5FFF5EBE5FFF4EBE5FFF5EBE4FFF5EB
          E4FFF8F2EDFFAC7D6DFF0000001D000000070000000100000000000000000000
          00010000000500000016B58B7CFFFEFDFCFFF8F0EBFFF7EFEAFFF7EFEAFFF7EF
          EAFFF7EFEAFFF7EFE9FFF6EEE9FFF6EEE9FFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFF5EDE7FFF6EDE7FFF6ECE6FFF5ECE6FFF5ECE5FFF5ECE5FFF5EBE5FFF5EC
          E4FFF9F3EEFFAC7F6FFF0000001C000000070000000100000000000000000000
          00010000000500000015B68C7EFFFEFDFCFFF7F1ECFFF7F1EBFFF8F0EBFFF8F0
          EBFFF7EFEAFFF6F0EAFFF6EFEAFFF6EFEAFFF7EFE9FFF6EFE8FFF6EEE8FFF6EE
          E8FFF6EEE7FFF6EDE8FFF6EDE7FFF6EDE6FFF5ECE7FFF5ECE6FFF5ECE6FFF5EC
          E6FFFAF4F0FFAD8070FF0000001B000000070000000100000000000000000000
          00000000000500000014B78E80FFFEFDFDFFF8F1EDFFF8F1ECFFF8F1ECFFF8F0
          EBFFF8F0ECFFF7F0EBFFF7F0EAFFF7F0EAFFF7EFEAFFF7EFE9FFF7EFE9FFF7EF
          E8FFF6EEE8FFF6EEE8FFF6EEE8FFF6EDE7FFF6EDE7FFF6EDE7FFF6EDE7FFF5EC
          E7FFF9F4F1FFAF8272FF0000001A000000070000000100000000000000000000
          00000000000500000013B88F82FFFEFDFDFFF8F2EEFFF8F1EDFFF8F2EDFFF8F1
          ECFFF8F1ECFFF8F1ECFFF8F1EBFFF7F0EBFFF7F0EBFFF7F0EAFFF7F0EAFFF6EF
          EAFFF7EFEAFFF6EFE9FFF6EEE9FFF6EEE8FFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFFAF5F2FFAF8373FF00000019000000060000000100000000000000000000
          00000000000400000012BB9184FFFEFEFDFFF8F3EEFFF8F2EEFFF8F2EDFFF8F2
          EDFFF8F2ECFFF7F1ECFFF8F1ECFFF8F1EBFFF8F1ECFFF7F0EBFFF8F0EBFFF8F0
          EAFFF7F0EAFFF7F0EAFFF7EFE9FFF6EFE9FFF7EFE9FFF6EEE9FFF6EEE8FFF6EE
          E8FFFAF5F3FFB18575FF00000018000000060000000100000000000000000000
          00000000000400000011BB9485FFFEFEFDFFF9F4F0FFF9F3EFFFF9F3EEFFF9F3
          EEFFFCF8F6FFFBF8F6FFFBF8F6FFFBF8F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7
          F5FFFBF7F5FFFBF7F5FFFBF7F5FFFBF7F4FFF7F0EAFFF6EFE9FFF7EEE9FFF6EE
          E8FFFAF6F3FFB28677FF00000017000000060000000100000000000000000000
          00000000000400000010BC9788FFFEFEFDFFF9F3F0FFF9F4EFFFF9F3F0FFF9F4
          EFFFC09987FFC09887FFC09887FFC09887FFC09887FFBF9787FFBF9887FFC097
          86FFBF9786FFBF9686FFBF9685FFBF9685FFF8F0EBFFF7F0EAFFF6F0EAFFF7F0
          EAFFFBF7F4FFB48979FF00000016000000060000000100000000000000000000
          0000000000040000000FBF988AFFFEFEFEFFFAF5F1FFF9F4F1FFFAF4F0FFF9F4
          EFFFF3EBE5FFF3EAE5FFF3EAE4FFF2EAE5FFF2E9E4FFF3EAE3FFF3E9E3FFF2E9
          E3FFF2E9E2FFF2E9E2FFF1E8E2FFF1E7E2FFF8F0ECFFF8F1ECFFF7F0EBFFF8F0
          EBFFFBF8F5FFB58A7AFF00000015000000050000000100000000000000000000
          0000000000040000000EC0998BFFFEFEFEFFFAF5F2FFFAF5F1FFFAF5F2FFF9F4
          F0FFC29C8AFFC29B8BFFC29B8AFFC29B8AFFC29A8AFFC29A89FFC29A89FFC19A
          89FFC19989FFC19988FFC19989FFC19988FFF8F2ECFFF8F1ECFFF7F0ECFFF8F0
          EBFFFBF8F6FFB58C7DFF00000015000000050000000100000000000000000000
          0000000000030000000EC19C8DFFFFFEFEFFFBF6F3FFFAF6F2FFFAF6F2FFFAF5
          F2FFF4ECE7FFF4EBE7FFF3EBE6FFF4EBE6FFF3EBE6FFF3EAE5FFF3EAE6FFF2EA
          E5FFF3EAE5FFF2E9E4FFF2E9E3FFF2E9E4FFF8F1EDFFF8F1EDFFF8F1EDFFF8F1
          ECFFFBF9F7FFB78E7FFF00000014000000050000000100000000000000000000
          0000000000030000000DC29D8FFFFFFEFEFFFBF7F4FFFAF6F4FFFAF6F3FFFAF6
          F2FFC59F8FFFC49F8EFFC49F8EFFC49E8EFFC49E8DFFC49E8DFFC49D8DFFC49E
          8DFFC49D8DFFC39D8CFFC39C8CFFC39C8CFFF9F3EEFFF8F2EEFFF8F2EEFFF8F1
          EDFFFCF9F7FFB88F81FF00000013000000050000000000000000000000000000
          0000000000030000000CC49F90FFFFFEFEFFFBF7F5FFFBF7F4FFFBF6F3FFFBF7
          F3FFF6EDE9FFF5EDE9FFF5ECE9FFF5ECE8FFF5ECE7FFF5ECE7FFF5ECE7FFF4EB
          E7FFF5EBE7FFF4EBE7FFF4EBE6FFF4EAE5FFF9F3EFFFF8F3EFFFF8F2EEFFF8F2
          EEFFFAF7F5FFB99283FF00000012000000040000000000000000000000000000
          0000000000030000000BC4A192FFFFFFFEFFFBF7F5FFFBF8F5FFFBF7F5FFFAF7
          F4FFC7A392FFC7A392FFC7A392FFC7A292FFC6A292FFC6A291FFC6A191FFC6A2
          91FFC6A190FFC6A190FFC5A190FFC5A090FFF9F3F0FFF9F3EFFFF9F3EFFFF7F1
          ECFFF9F4F3FFBB9284FF00000011000000040000000000000000000000000000
          0000000000020000000AC6A294FFFFFFFEFFFCF8F6FFFBF8F5FFFBF8F5FFFCF8
          F5FFFBF7F4FFFBF8F4FFFBF7F4FFFBF7F4FFFBF6F3FFFAF6F3FFFAF6F2FFFAF5
          F3FFFAF6F2FFFAF5F2FFFAF5F2FFFAF5F1FFFAF5F1FFF9F4F1FFF7F2EDFFF6EF
          EAFFF7F2EFFFBD9486FF00000010000000040000000000000000000000000000
          00000000000200000009C8A495FFFFFFFFFFFCF9F6FFFBF9F6FFFCF9F6FFFBF8
          F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFAF7F4FFFBF7F4FFFBF7F4FFFBF7
          F3FFFAF6F3FFF9F5F2FFFAF6F3FFF9F5F1FFF9F4F0FFF7F2EDFFF5EFEAFFF3EA
          E6FFF3EDEAFFBD9888FF0000000F000000040000000000000000000000000000
          00000000000200000008C8A597FFFFFFFFFFFCFAF8FFFCFAF7FFFCF9F7FFFCF9
          F6FFFBF8F7FFFBF8F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFBF7F4FFFAF7
          F5FFFBF7F4FFF9F4F1FFF7F1EDFFF6EFEBFFF4ECE6FFF1E7E3FFEFE4DFFFECE0
          DBFFECE1DDFFBF988AFF0000000E000000040000000000000000000000000000
          00000000000200000008C9A698FFFFFFFFFFFCFAF9FFFCFAF8FFFCF9F8FFFCF9
          F7FFFCF9F7FFFCF9F7FFFBF9F6FFFCF8F6FFFBF8F6FFFCF8F6FFFBF8F6FFFAF6
          F4FFFAF6F3FFF6EFEAFFEFE3DEFFE7D9D2FFE2D3CBFFE1CFC8FFDFCCC4FFDCC8
          BFFFDCC9C2FFC19A8CFF0000000D000000030000000000000000000000000000
          00000000000200000007CBA899FFFFFFFFFFFDFAF9FFFDFAF9FFFCFAF8FFFCFA
          F8FFFCFAF7FFFDFAF7FFFCF9F7FFFBF9F7FFFCF9F7FFFCF9F6FFFBF8F6FFFCF8
          F6FFF9F3F1FFF0E7E1FFB89284FFAC7F6FFFAB7E6DFFAB7D6DFFAB7C6CFFAA7C
          6CFFD1B7AFFFB78C7DFF0000000A000000030000000000000000000000000000
          00000000000100000006CBA99BFFFFFFFFFFFDFBFAFFFDFAFAFFFCFAF9FFFCFA
          F9FFFDFAF8FFFCFBF8FFFCFAF8FFFCFAF8FFFCF9F7FFFCF9F7FFFCF9F7FFFBF9
          F6FFF8F3F0FFEDE0DCFFB18676FFFFFFFFFFFFFEFEFFFFFDFCFFFEFCFAFFFCF9
          F7FFD1B7AEFF533C358600000006000000020000000000000000000000000000
          00000000000100000005CBAA9DFFFFFFFFFFFDFBFAFFFDFBFAFFFDFBF9FFFDFB
          FAFFFDFAF9FFFDFBF9FFFDFAF8FFFDFAF9FFFCFAF8FFFCFAF8FFFCFAF7FFFCF9
          F7FFF7F2EEFFECE0DBFFB68D7DFFFFFEFEFFFEFBFAFFFDF9F7FFFCF6F3FFD4BA
          B0FF553F38860000000800000003000000010000000000000000000000000000
          00000000000100000005CCAB9DFFFFFFFFFFFEFCFBFFFEFCFAFFFEFBFAFFFDFB
          FBFFFDFCFAFFFDFBFAFFFDFBF9FFFDFBFAFFFDFAF9FFFCFAF8FFFCFAF9FFFAF7
          F5FFF6F1EDFFEDE2DCFFBC9485FFFFFEFEFFFDF9F6FFFBF6F3FFD6BCB4FF5843
          3B86000000080000000300000001000000000000000000000000000000000000
          00000000000100000004CDAC9FFFFFFFFFFFFDFCFCFFFDFDFCFFFDFCFBFFFDFC
          FBFFFDFCFAFFFDFCFAFFFEFBFAFFFDFBFAFFFDFBF9FFFDFBF9FFFAF8F7FFF9F5
          F3FFF5EEECFFECE2DDFFC19C8CFFFFFEFEFFFBF6F3FFD9C1B7FF5B463F850000
          0007000000030000000100000000000000000000000000000000000000000000
          00000000000100000003CEAD9FFFFFFFFFFFFEFCFCFFFEFDFCFFFEFDFCFFFDFC
          FCFFFDFDFCFFFEFCFBFFFEFCFBFFFDFCFAFFFDFCFAFFFBF8F7FFF9F6F4FFF7F2
          EFFFF3ECE8FFEDE2DDFFC6A293FFFFFEFEFFDBC3BAFF5D494284000000060000
          0002000000010000000000000000000000000000000000000000000000000000
          00000000000100000002CEADA0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBF9F9FFF9F6F4FFF6F1
          F0FFF2ECE9FFEEE3E0FFE5D4CDFFE0CCC4FF5F4D458300000005000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          00000000000000000001998076BECEAEA0FFCEADA0FFCEAE9FFFCEADA0FFCEAD
          9FFFCDAC9FFFCEACA0FFCDAC9FFFCDAC9EFFCDAC9FFFCCAC9EFFCCAB9EFFCCAA
          9DFFCCAB9CFFCBAA9CFFCBAA9CFF614F48820000000400000002000000010000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000000000000020000
          0003000000050000000600000006000000060000000600000006000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000060000000600000007000000070000000700000007000000070000
          0007000000070000000700000007000000060000000400000002000000060000
          000C000000120000001500000015000000160000001600000016000000160000
          0017000000170000001700000017000000180000001800000018000000180000
          00190000001900000019000000190000001A0000001A0000001A0000001A0000
          001B0000001B0000001B0000001A0000001800000011000000080000000C8264
          59BFB48A7BFFB4897AFFB4887AFFB38779FFB28778FFB18677FFB18577FFB084
          75FFB08475FFB08374FFAF8273FFAF8272FFAE8171FFAE8071FFAD8070FFAD80
          70FFAD7F6FFFAC7F6EFFAC7E6EFFAC7E6DFFAB7E6DFFAB7D6DFFAA7C6CFFAA7C
          6CFFAA7C6CFFAA7C6CFFAA7B6BFFA97B6BFF79594DC10000001100000010BA91
          84FFFEFDFDFFFBF7F4FFFBF7F4FFFBF7F4FFFAF6F4FFFBF7F3FFFAF7F4FFFAF6
          F3FFFAF6F3FFFAF6F3FFFAF6F3FFFAF6F3FFFAF6F3FFFAF5F2FFFAF6F2FFFAF5
          F2FFFAF5F2FFFAF5F2FFFAF5F2FFF9F5F2FFF9F5F1FFFAF5F1FFFAF4F1FFFAF4
          F1FFFAF5F1FFF9F4F0FFF9F4F1FFFCF9F6FFAF8272FF0000001700000011BB94
          85FFFEFEFDFFF7F0EBFFF7F0EBFFF7F0EAFFF6F0EAFFF7EFEAFFF6EFEAFFF6EF
          E9FFF7EEE8FFF7EEE8FFF6EEE8FFF6EEE7FFF6EEE7FFF6EDE7FFF5EDE6FFF6ED
          E6FFF6ECE6FFF5EDE6FFF5ECE5FFF5EBE5FFF5EBE5FFF4EBE5FFF5EBE4FFF5EB
          E4FFF4EAE3FFF4EAE4FFF5EAE3FFF9F3EFFFAF8373FF0000001800000011BD96
          87FFFEFEFDFFF7F1ECFFF7F1EBFFF8F0EBFFF8F0EBFFF7EFEAFFF7EFEAFFF7EF
          EAFFF7EFEAFFF7EFE9FFF6EEE9FFF6EEE9FFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFF5EDE7FFF6EDE7FFF6ECE6FFF5ECE6FFF5ECE5FFF5ECE5FFF5EBE5FFF5EC
          E4FFF5EBE4FFF4EBE4FFF4EBE4FFFAF4F0FFB18575FF0000001800000010BE97
          89FFFEFEFEFFF8F1EDFFF8F1ECFFF7F1EBFFF7F1ECFFF7F1EBFFF8F0EBFFF8F0
          EBFFF7EFEAFFF6F0EAFFF6EFEAFFF6EFEAFFF7EFE9FFF6EFE8FFF6EEE8FFF6EE
          E8FFF6EEE7FFF6EDE8FFF6EDE7FFF6EDE6FFF5ECE7FFF5ECE6FFF5ECE6FFF5EC
          E6FFF6ECE6FFF5ECE5FFF5ECE5FFFAF5F1FFB28677FF000000170000000FC099
          8BFFFEFEFEFFF9F2EEFFF8F1EDFFF8F1EDFFF8F1EDFFF8F1ECFFF8F1ECFFF8F0
          EBFFF8F0ECFFF7F0EBFFF7F0EAFFF7F0EAFFF7EFEAFFF7EFE9FFF7EFE9FFF7EF
          E8FFF6EEE8FFF6EEE8FFF6EEE8FFF6EDE7FFF6EDE7FFF6EDE7FFF6EDE7FFF5EC
          E7FFF5ECE6FFF5EDE6FFF5ECE5FFFAF5F2FFB48979FF000000160000000EC19C
          8DFFFFFEFEFFF8F3EEFFF8F3EEFFF9F2EEFFF8F2EEFFF8F1EDFFF8F2EDFFF8F1
          ECFFF8F1ECFFF8F1ECFFF8F1EBFFF7F0EBFFF7F0EBFFF7F0EAFFF7F0EAFFF6EF
          EAFFF7EFEAFFF6EFE9FFF6EEE9FFF6EEE8FFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFF6EDE7FFF6EDE7FFF5EDE6FFFBF6F3FFB58A7AFF000000150000000DC29D
          8FFFFFFEFEFFF9F3EFFFF9F4EFFFF9F3EFFFF8F3EEFFF8F2EEFFF8F2EDFFFBF8
          F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFBF7F5FFFBF7
          F4FFFBF7F4FFFBF7F4FFFBF7F4FFFAF7F4FFFBF7F4FFF6EEE9FFF6EEE8FFF6EE
          E8FFF6EDE8FFF6EEE7FFF6EDE7FFFBF7F3FFB58C7DFF000000150000000CC49F
          91FFFFFEFEFFF9F4F0FFF9F4F0FFF9F4EFFFF9F4F0FFF9F3EFFFF9F3EEFFC39D
          8DFFC39D8CFFC29B8BFFC29B8BFFC29A8AFFC09989FFC19988FFC09887FFC097
          87FFBF9786FFBF9685FFBE9585FFBE9584FFBE9584FFF6EFE9FFF7EEE9FFF6EE
          E8FFF6EEE8FFF6EEE9FFF6EEE8FFFBF7F5FFB78E7FFF000000140000000BC5A0
          92FFFFFFFEFFF9F4F1FFFAF5F1FFFAF4F0FFF9F3F0FFF9F4EFFFF9F3F0FFF4EB
          E5FFF4EAE4FFF3EAE4FFF3EAE4FFF3E9E4FFF3E9E3FFF2E9E3FFF2E8E3FFF2E8
          E3FFF2E7E2FFF1E8E1FFF1E8E1FFF1E7E1FFF2E7E1FFF7F0EAFFF6F0EAFFF7F0
          EAFFF7EFEAFFF7EFE9FFF6EEE9FFFBF8F6FFB88F81FF000000130000000BC6A2
          93FFFFFFFFFFFAF6F2FFFAF5F1FFF9F4F1FFFAF5F1FFF9F4F1FFFAF4F0FFC6A1
          91FFC5A191FFC59F8FFFC49F8FFFC49E8EFFC39D8DFFC39C8DFFC39B8BFFC29B
          8BFFC19B8AFFC19A88FFC09888FFC09888FFBF9787FFF8F1ECFFF7F0EBFFF8F0
          EBFFF7F0EBFFF7F0EAFFF6EFEAFFF9F6F3FFB99283FF000000120000000AC7A3
          96FFFFFFFFFFFAF6F2FFFAF6F2FFFAF6F2FFFAF5F2FFFAF5F1FFFAF5F2FFF4EC
          E7FFF4ECE7FFF4ECE6FFF4ECE6FFF4EBE6FFF4EBE5FFF4EAE5FFF4EAE4FFF3E9
          E4FFF3E9E3FFF2E8E3FFF2E9E3FFF2E8E3FFF2E9E2FFF8F1ECFFF7F0ECFFF8F0
          EBFFF7F0EBFFF7F0EBFFF5EEE8FFF8F3F1FFBB9284FF0000001100000009C8A5
          97FFFFFFFFFFFAF7F3FFFAF7F3FFFBF6F3FFFBF6F3FFFAF6F2FFFAF6F2FFC8A5
          96FFC8A495FFC8A494FFC7A393FFC7A393FFC6A292FFC6A091FFC4A090FFC49F
          8FFFC39E8EFFC39D8DFFC39D8CFFC29B8CFFC19B8BFFF8F1EDFFF8F1EDFFF8F1
          ECFFF7F1ECFFF6EFE9FFF4EBE5FFF6F0EEFFBD9486FF0000001000000008C9A6
          99FFFFFFFFFFFBF7F4FFFAF7F4FFFBF6F3FFFBF7F4FFFAF6F4FFFAF6F3FFF5EE
          E9FFF5EDEAFFF5EDE9FFF4EDE8FFF4ECE8FFF5ECE7FFF4EDE7FFF4ECE7FFF4EB
          E6FFF4EBE5FFF4EAE5FFF4EAE5FFF3EAE4FFF3E9E3FFF8F2EEFFF7F1EDFFF7F0
          ECFFF5EEE9FFF4EBE5FFF1E8E2FFF3ECE9FFBD9888FF0000000F00000007CBA8
          99FFFFFFFFFFFBF8F5FFFBF8F5FFFBF7F5FFFBF7F5FFFBF7F4FFFBF6F3FFCCAA
          9AFFCBA99AFFCAA898FFCAA798FFC9A697FFC8A697FFC8A495FFC8A495FFC7A3
          94FFC6A293FFC6A192FFC5A091FFC4A08FFFC49F8EFFF5EEE9FFF4EBE7FFF2E8
          E3FFEFE5DFFFEEE2DAFFEADED7FFECE1DCFFBF988AFF0000000E00000006CBA9
          9BFFFFFFFFFFFBF8F6FFFBF9F5FFFBF8F5FFFBF7F5FFFBF8F5FFFBF7F5FFFAF7
          F4FFFAF7F4FFFAF6F3FFFAF6F3FFFBF6F2FFFAF6F2FFFAF6F2FFFAF5F2FFFAF5
          F1FFFAF5F1FFFAF4F1FFF9F4EFFFF9F3EEFFF4EBE7FFEDE0DAFFE6D5CFFFE1D0
          C8FFE0CDC5FFDEC9C1FFDBC6BDFFDCC8C1FFC19A8CFF0000000D00000006CBA9
          9DFFFFFFFFFFFCF9F7FFFBF9F7FFFCF9F6FFFCF8F6FFFBF8F5FFFBF8F5FFFCF8
          F5FFFBF7F4FFFBF8F4FFFBF7F4FFFBF7F4FFFBF6F3FFFAF6F3FFFAF6F2FFFAF5
          F3FFFAF6F2FFFAF5F2FFFAF5F2FFF8F0EDFFEFE4DEFFB89183FFAC7F6FFFAB7E
          6DFFAB7D6DFFAB7C6CFFAA7C6CFFD1B7AFFFB78C7DFF0000000A00000005CCAB
          9DFFFFFFFFFFFCF9F8FFFCF9F7FFFCF9F7FFFCF9F6FFFBF9F6FFFCF9F6FFFBF8
          F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFAF7F4FFFBF7F4FFFBF7F4FFFBF7
          F3FFFAF6F3FFFAF6F3FFFAF6F3FFF6F0EBFFEBDED8FFB18676FFFFFFFFFFFFFE
          FEFFFFFDFCFFFEFCFAFFFCF9F7FFD1B7AEFF533C35860000000600000004CDAB
          9EFFFFFFFFFFFDFAF8FFFCFAF8FFFCFAF8FFFCFAF8FFFCFAF7FFFCF9F7FFFCF9
          F6FFFBF8F7FFFBF8F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFBF7F4FFFAF7
          F5FFFBF7F4FFFAF6F4FFFAF6F3FFF5EFEBFFEBDED8FFB68D7DFFFFFEFEFFFEFB
          FAFFFDF9F7FFFCF6F3FFD4BAB0FF553F3886000000080000000300000004CDAC
          9FFFFFFFFFFFFCFBF9FFFDFAF9FFFCFAF8FFFCFAF9FFFCFAF8FFFCF9F8FFFCF9
          F7FFFCF9F7FFFCF9F7FFFBF9F6FFFCF8F6FFFBF8F6FFFCF8F6FFFBF8F6FFFBF7
          F5FFFBF7F5FFFBF7F4FFF9F5F1FFF5EEE8FFEBDFD9FFBC9485FFFFFEFEFFFDF9
          F6FFFBF6F3FFD6BCB4FF58433B8600000008000000030000000100000003CEAD
          A0FFFFFFFFFFFDFBFAFFFDFBF9FFFDFBFAFFFDFAF9FFFDFAF9FFFCFAF8FFFCFA
          F8FFFCFAF7FFFDFAF7FFFCF9F7FFFBF9F7FFFCF9F7FFFCF9F6FFFBF8F6FFFCF8
          F6FFFBF8F6FFF9F6F2FFF7F3EFFFF3ECE8FFEBE0DAFFC19C8CFFFFFEFEFFFBF6
          F3FFD9C1B7FF5B463F850000000700000003000000010000000000000002CEAD
          A0FFFFFFFFFFFDFCFAFFFDFCFAFFFDFCFAFFFDFBFAFFFDFAFAFFFCFAF9FFFCFA
          F9FFFDFAF8FFFCFBF8FFFCFAF8FFFCFAF8FFFCF9F7FFFCF9F7FFFCF9F7FFFBF9
          F6FFFAF6F4FFF8F3F0FFF6EFECFFF2E9E5FFEBE0DAFFC6A293FFFFFEFEFFDBC3
          BAFF5D494284000000060000000200000001000000000000000000000002CEAD
          A0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFBF9F9FFF9F6F4FFF6F1F0FFF2ECE8FFEEE3DFFFE5D4CDFFE0CCC4FF5F4D
          4583000000050000000200000001000000000000000000000000000000019980
          76BECEADA0FFCEAD9FFFCEADA0FFCEADA0FFCEADA0FFCEADA0FFCEAEA0FFCEAD
          A0FFCEAE9FFFCEADA0FFCEAD9FFFCDAC9FFFCEACA0FFCDAC9FFFCDAC9EFFCDAC
          9FFFCCAC9EFFCCAB9EFFCCAA9DFFCCAB9CFFCBAA9CFFCBAA9CFF614F48820000
          0004000000020000000100000000000000000000000000000000000000000000
          0001000000010000000100000001000000020000000200000002000000020000
          0002000000020000000200000003000000030000000300000003000000030000
          0003000000030000000400000004000000040000000400000004000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end>
  end
end

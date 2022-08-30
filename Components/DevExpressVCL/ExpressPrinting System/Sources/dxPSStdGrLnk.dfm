inherited dxGridReportLinkDesignWindow: TdxGridReportLinkDesignWindow
  Left = 422
  Top = 253
  Caption = 'dxGridReportLinkDesigner'
  ClientHeight = 377
  ClientWidth = 601
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 601
    Height = 377
    inherited btnApply: TcxButton
      Top = 305
      TabOrder = 40
    end
    inherited btnCancel: TcxButton
      Top = 305
      TabOrder = 39
    end
    inherited btnOK: TcxButton
      Top = 305
      TabOrder = 38
    end
    inherited btnHelp: TcxButton
      Top = 305
      TabOrder = 41
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 305
      TabOrder = 42
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 305
      TabOrder = 43
    end
    inherited btnTitleProperties: TcxButton
      Top = 305
      TabOrder = 44
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 305
      TabOrder = 45
    end
    object Image2: TcxImage [8]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 26
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object Image3: TcxImage [9]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 30
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgMiscellaneous: TcxImage [10]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 34
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgGrid: TcxImage [11]
      Left = 21
      Top = 207
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Height = 48
      Width = 48
    end
    object Image1: TcxImage [12]
      Left = 21
      Top = 68
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Height = 48
      Width = 48
    end
    object pnlPreview: TPanel [13]
      Left = 293
      Top = 29
      Width = 554
      Height = 253
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 37
    end
    object lblOnEveryPage: TcxLabel [14]
      Left = 21
      Top = 44
      AutoSize = False
      Caption = 'On Every Page'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 254
    end
    object lblShow: TcxLabel [15]
      Left = 21
      Top = 183
      AutoSize = False
      Caption = 'Show'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 254
    end
    object chbxShowBorders: TcxCheckBox [16]
      Left = 75
      Top = 68
      Caption = 'Border'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 200
    end
    object chbxFixedRowsOnEveryPage: TcxCheckBox [17]
      Left = 75
      Top = 207
      Caption = 'Fi&xed Rows'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      OnClick = chbxFixedRowsOnEveryPageClick
      Width = 200
    end
    object cbxDrawMode: TcxImageComboBox [18]
      Left = 10000
      Top = 10000
      Properties.Items = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Visible = False
      OnClick = cbxDrawModeClick
      Width = 188
    end
    object chbxTransparent: TcxCheckBox [19]
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Visible = False
      OnClick = chbxTransparentClick
      Width = 17
    end
    object stTransparent: TcxLabel [20]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' &Transparent '
      FocusControl = chbxTransparent
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stTransparentClick
      Height = 18
      Width = 231
      AnchorY = 10009
    end
    object ccbxColor: TcxColorComboBox [21]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 13
      Visible = False
      Width = 144
    end
    object ccbxEvenColor: TcxColorComboBox [22]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 14
      Visible = False
      Width = 144
    end
    object chbxFixedTransparent: TcxCheckBox [23]
      Tag = 1
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      Visible = False
      OnClick = chbxTransparentClick
      Width = 17
    end
    object stFixedTransparent: TcxLabel [24]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' Fixed T&ransparent '
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stFixedTransparentClick
      Height = 18
      Width = 231
      AnchorY = 10009
    end
    object ccbxFixedColor: TcxColorComboBox [25]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 17
      Visible = False
      Width = 144
    end
    object ccbxGridLineColor: TcxColorComboBox [26]
      Tag = 3
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 18
      Visible = False
      Width = 144
    end
    object btnFixedFont: TcxButton [27]
      Tag = 2
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Fi&xed Font ...'
      TabOrder = 19
      Visible = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontClick
    end
    object edFixedFont: TcxTextEdit [28]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Visible = False
      Width = 254
    end
    object btnFont: TcxButton [29]
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Fo&nt ...'
      TabOrder = 21
      Visible = False
      OnClick = btnFontClick
    end
    object edFont: TcxTextEdit [30]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Visible = False
      Width = 254
    end
    object btnEvenFont: TcxButton [31]
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'E&ven Font ...'
      TabOrder = 23
      Visible = False
      OnClick = btnFontClick
    end
    object edEvenFont: TcxTextEdit [32]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Visible = False
      Width = 254
    end
    object lblSelection: TcxLabel [33]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Selection'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 254
      AnchorY = 10009
    end
    object lblLookAndFeel: TcxLabel [34]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Look And Feel'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 254
      AnchorY = 10009
    end
    object lblMiscellaneous: TcxLabel [35]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 254
      AnchorY = 10009
    end
    object chbxOnlySelected: TcxCheckBox [36]
      Left = 10000
      Top = 10000
      Caption = 'Only &selected cells'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Transparent = True
      Visible = False
      OnClick = chbxOnlySelectedClick
      Width = 200
    end
    object chbxIncludeFixed: TcxCheckBox [37]
      Left = 10000
      Top = 10000
      Caption = '&Including fixed cells'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 28
      Transparent = True
      Visible = False
      OnClick = chbxIncludeFixedClick
      Width = 200
    end
    object chbxUse3DEffects: TcxCheckBox [38]
      Tag = 12
      Left = 10000
      Top = 10000
      Caption = '&Use 3D Effects'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 31
      Transparent = True
      Visible = False
      OnClick = chbxUse3DEffectsClick
      Width = 200
    end
    object chbxUseSoft3D: TcxCheckBox [39]
      Tag = 13
      Left = 10000
      Top = 10000
      Caption = 'Soft &3D'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 32
      Transparent = True
      Visible = False
      OnClick = chbxUseSoft3DClick
      Width = 200
    end
    object chbxAutoWidth: TcxCheckBox [40]
      Left = 10000
      Top = 10000
      Caption = 'AutoWidth'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 35
      Transparent = True
      Visible = False
      OnClick = chbxAutoWidthClick
      Width = 200
    end
    object chbxRowAutoHeight: TcxCheckBox [41]
      Left = 10000
      Top = 10000
      Caption = '&Row Auto Height'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 36
      Transparent = True
      Visible = False
      OnClick = chbxRowAutoHeightClick
      Width = 200
    end
    object chbxShowHorzLines: TcxCheckBox [42]
      Tag = 1
      Left = 75
      Top = 91
      Caption = 'Horizontal Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 200
    end
    object chbxShowVertLines: TcxCheckBox [43]
      Tag = 2
      Left = 75
      Top = 114
      Caption = 'Vertical Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 200
    end
    object chbxShowFixedHorzLines: TcxCheckBox [44]
      Tag = 3
      Left = 75
      Top = 137
      Caption = 'Fixed &Horizontal Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 200
    end
    object chbxShowFixedVertLines: TcxCheckBox [45]
      Tag = 4
      Left = 75
      Top = 160
      Caption = 'Fixed &Vertical Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 200
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
    end
    inherited dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Index = 1
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'lblPreview'
      CaptionOptions.Layout = clTop
      SizeOptions.Height = 250
      SizeOptions.Width = 250
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 230
      ControlOptions.OriginalWidth = 250
      Index = 1
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object tshOptions: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshOptions'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object tshColor: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshColor'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object tshFont: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFont'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 2
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshBehaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 3
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblOnEveryPage
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblShow
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      AlignHorz = ahLeft
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxFixedRowsOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object lblDrawMode: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblDrawMode'
      Control = cbxDrawMode
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = stTransparent
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = tshColor
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lblColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblColor'
      Offsets.Left = 27
      Control = ccbxColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblEvenColor: TdxLayoutItem
      Parent = tshColor
      AlignHorz = ahClient
      CaptionOptions.Text = 'lblEvenColor'
      Offsets.Left = 27
      Control = ccbxEvenColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = chbxFixedTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = stFixedTransparent
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshColor
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object lblFixedColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblFixedColor'
      Offsets.Left = 27
      Control = ccbxFixedColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = tshColor
      CaptionOptions.Text = 'Separator'
      Index = 6
    end
    object lblGridLinesColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblGridLinesColor'
      Offsets.Left = 27
      Control = ccbxGridLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFixedFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = tshFont
      Control = edFixedFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = tshFont
      Control = edFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnEvenFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = tshFont
      Control = edEvenFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSelection
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblLookAndFeel
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblMiscellaneous
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahLeft
      Control = Image2
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahLeft
      Control = Image3
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahLeft
      Control = imgMiscellaneous
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxOnlySelected
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Visible = False
      Control = chbxIncludeFixed
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxUse3DEffects
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxUseSoft3D
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object lichbxRowAutoHeight: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      CaptionOptions.Visible = False
      Control = chbxRowAutoHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      AlignVert = avClient
      Control = imgGrid
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = chbxShowHorzLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      AlignVert = avClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowVertLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 82
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowFixedHorzLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 124
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Visible = False
      Control = chbxShowFixedVertLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 4
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

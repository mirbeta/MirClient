inherited cxfmMCListBoxDesignWindow: TcxfmMCListBoxDesignWindow
  Left = 391
  Top = 313
  BorderIcons = [biSystemMenu, biMinimize, biMaximize, biHelp]
  Caption = 'cxfmMCListBoxDesignWindow'
  ClientHeight = 332
  ClientWidth = 574
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 574
    Height = 332
    inherited btnApply: TcxButton
      Top = 305
      TabOrder = 34
    end
    inherited btnCancel: TcxButton
      Top = 305
      TabOrder = 33
    end
    inherited btnOK: TcxButton
      Top = 305
      TabOrder = 32
    end
    inherited btnHelp: TcxButton
      Top = 305
      TabOrder = 35
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 305
      TabOrder = 36
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 305
      TabOrder = 37
    end
    inherited btnTitleProperties: TcxButton
      Top = 305
      TabOrder = 38
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 305
      TabOrder = 39
    end
    object imgGrid: TImage [8]
      Left = 21
      Top = 84
      Width = 48
      Height = 48
      Transparent = True
    end
    object Image1: TImage [9]
      Left = 21
      Top = 200
      Width = 48
      Height = 48
      Transparent = True
    end
    object Image2: TImage [10]
      Left = 10000
      Top = 10000
      Width = 48
      Height = 48
      Transparent = True
      Visible = False
    end
    object Image3: TImage [11]
      Left = 10000
      Top = 10000
      Width = 48
      Height = 48
      Transparent = True
      Visible = False
    end
    object Image4: TImage [12]
      Left = 10000
      Top = 10000
      Width = 48
      Height = 48
      Transparent = True
      Visible = False
    end
    object pnlPreview: TPanel [13]
      Left = 305
      Top = 45
      Width = 542
      Height = 253
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 31
    end
    object lblShow: TcxLabel [14]
      Left = 21
      Top = 60
      AutoSize = False
      Caption = 'Show'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 266
    end
    object chbxShowBorders: TcxCheckBox [15]
      Left = 75
      Top = 84
      Caption = 'Border'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      OnClick = ShowClick
      Width = 150
    end
    object chbxShowHorzLines: TcxCheckBox [16]
      Tag = 1
      Left = 75
      Top = 107
      Caption = 'Horizontal Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = ShowClick
      Width = 150
    end
    object chbxShowVertLines: TcxCheckBox [17]
      Tag = 2
      Left = 75
      Top = 130
      Caption = 'Vertical Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = ShowClick
      Width = 150
    end
    object chbxShowColumnHeaders: TcxCheckBox [18]
      Tag = 3
      Left = 75
      Top = 153
      Caption = '&Column Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = ShowClick
      Width = 150
    end
    object lblOnEveryPage: TcxLabel [19]
      Left = 21
      Top = 176
      AutoSize = False
      Caption = 'On Every Page'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 266
    end
    object chbxHeadersOnEveryPage: TcxCheckBox [20]
      Left = 75
      Top = 200
      Caption = 'Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = HeadersOnEveryPageClick
      Width = 212
    end
    object cbxDrawMode: TcxImageComboBox [21]
      Left = 10000
      Top = 10000
      Properties.Items = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Visible = False
      OnClick = DrawModeClick
      Width = 200
    end
    object chbxTransparent: TcxCheckBox [22]
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Visible = False
      OnClick = TransparentClick
      Width = 17
    end
    object stTransparent: TcxLabel [23]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' Transparent'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stTransparentClick
      Height = 18
      Width = 243
      AnchorY = 10009
    end
    object ccbxColor: TcxColorComboBox [24]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ColorChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Visible = False
      Width = 156
    end
    object ccbxEvenColor: TcxColorComboBox [25]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ColorChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Visible = False
      Width = 156
    end
    object chbxTransparentHeaders: TcxCheckBox [26]
      Tag = 1
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Visible = False
      OnClick = TransparentClick
      Width = 17
    end
    object stTransparentHeaders: TcxLabel [27]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' Transparent Headers '
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stTransparentHeadersClick
      Height = 18
      Width = 243
      AnchorY = 10009
    end
    object ccbxHeadersColor: TcxColorComboBox [28]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ColorChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Visible = False
      Width = 156
    end
    object ccbxGridLineColor: TcxColorComboBox [29]
      Tag = 3
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ColorChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Visible = False
      Width = 156
    end
    object edFixedFont: TcxTextEdit [30]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Visible = False
      Width = 283
    end
    object btnHeadersFont: TcxButton [31]
      Tag = 2
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Headers Font...'
      TabOrder = 16
      Visible = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = FontClick
    end
    object btnFont: TcxButton [32]
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Fo&nt...'
      TabOrder = 18
      Visible = False
      OnClick = FontClick
    end
    object edFont: TcxTextEdit [33]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Visible = False
      Width = 283
    end
    object btnEvenFont: TcxButton [34]
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'E&ven Font...'
      TabOrder = 20
      Visible = False
      OnClick = FontClick
    end
    object edEvenFont: TcxTextEdit [35]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 21
      Visible = False
      Width = 283
    end
    object lblMiscellaneous: TcxLabel [36]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 283
    end
    object lblSelection: TcxLabel [37]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Selection'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 283
    end
    object lblLookAndFeel: TcxLabel [38]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Look And Feel'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 283
    end
    object chbxOnlySelected: TcxCheckBox [39]
      Left = 10000
      Top = 10000
      Caption = 'Only &selected cells'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      OnClick = OnlySelectedClick
      Width = 108
    end
    object chbxIncludeFixed: TcxCheckBox [40]
      Left = 10000
      Top = 10000
      Caption = '&Including fixed cells'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
      Visible = False
      OnClick = IncludeFixedClick
      Width = 113
    end
    object chbxUse3DEffects: TcxCheckBox [41]
      Tag = 12
      Left = 10000
      Top = 10000
      Caption = '3D Effects'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 26
      Transparent = True
      Visible = False
      OnClick = Use3DEffectsClick
      Width = 70
    end
    object chbxUseSoft3D: TcxCheckBox [42]
      Tag = 13
      Left = 10000
      Top = 10000
      Caption = 'Soft &3D'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Transparent = True
      Visible = False
      OnClick = UseSoft3DClick
      Width = 56
    end
    object chbxAutoWidth: TcxCheckBox [43]
      Left = 10000
      Top = 10000
      Caption = 'AutoWidth'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 29
      Transparent = True
      Visible = False
      OnClick = chbxAutoWidthClick
      Width = 71
    end
    object chbxRowAutoHeight: TcxCheckBox [44]
      Left = 10000
      Top = 10000
      Caption = '&Row Auto Height'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 30
      Transparent = True
      Visible = False
      OnClick = RowAutoHeightClick
      Width = 101
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Preview:'
      CaptionOptions.Layout = clTop
      SizeOptions.Height = 250
      SizeOptions.Width = 250
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 250
      ControlOptions.OriginalWidth = 250
      Index = 1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tshOptions: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Options'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object tshColor: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Color'
      ButtonOptions.Buttons = <>
      ItemIndex = 7
      Index = 1
    end
    object tshFont: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Font'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 2
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Behaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 3
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tshOptions
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = lblShow
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      Control = imgGrid
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowHorzLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowVertLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowColumnHeaders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = tshOptions
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = lblOnEveryPage
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxHeadersOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignVert = avClient
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
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
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = stTransparent
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
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
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblEvenColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblEvenColor'
      Offsets.Left = 27
      Control = ccbxEvenColor
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = chbxTransparentHeaders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = stTransparentHeaders
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = tshColor
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object lblHeadersColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblHeadersColor'
      Offsets.Left = 27
      Control = ccbxHeadersColor
      ControlOptions.OriginalHeight = 19
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
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = tshFont
      Control = edFixedFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnHeadersFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahClient
      Control = edFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnEvenFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshFont
      Control = edEvenFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblMiscellaneous
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSelection
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblLookAndFeel
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      Control = Image2
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahLeft
      Control = Image3
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahLeft
      Control = Image4
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxOnlySelected
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxIncludeFixed
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxUse3DEffects
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 74
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxUseSoft3D
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxRowAutoHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

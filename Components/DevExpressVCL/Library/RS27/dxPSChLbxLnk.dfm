inherited dxChlbxReportLinkDesignWindow: TdxChlbxReportLinkDesignWindow
  Left = 335
  Top = 330
  Caption = 'dxCLbReportLinkDesigner'
  ClientHeight = 297
  ClientWidth = 529
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 529
    Height = 297
    inherited btnApply: TcxButton
      Top = 243
      TabOrder = 22
    end
    inherited btnCancel: TcxButton
      Top = 243
      TabOrder = 21
    end
    inherited btnOK: TcxButton
      Top = 243
      TabOrder = 20
    end
    inherited btnHelp: TcxButton
      Top = 243
      TabOrder = 23
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 243
      TabOrder = 24
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 243
      TabOrder = 25
    end
    inherited btnTitleProperties: TcxButton
      Top = 243
      TabOrder = 26
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 243
      TabOrder = 27
    end
    object pnlPreview: TPanel [8]
      Left = 273
      Top = 29
      Width = 574
      Height = 191
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 19
      OnResize = pnlPreviewResize
    end
    object lblShow: TcxLabel [9]
      Left = 21
      Top = 44
      AutoSize = False
      Caption = 'Show'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 19
      Width = 234
    end
    object chbxShowBorders: TcxCheckBox [10]
      Left = 75
      Top = 69
      Caption = 'Border'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 121
    end
    object chbxShowHorzLines: TcxCheckBox [11]
      Tag = 1
      Left = 75
      Top = 92
      Caption = 'Horizontal Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 121
    end
    object lblMiscellaneous: TcxLabel [12]
      Left = 21
      Top = 123
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 234
    end
    object chbxFlatCheckMarks: TcxCheckBox [13]
      Tag = 2
      Left = 75
      Top = 147
      Caption = 'Flat Check &Marks'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 180
    end
    object chbxAutoWidth: TcxCheckBox [14]
      Left = 75
      Top = 170
      Caption = 'AutoWidth'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = chbxAutoWidthClick
      Width = 180
    end
    object chbxRowAutoHeight: TcxCheckBox [15]
      Left = 75
      Top = 193
      Caption = '&Row Auto Height'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      OnClick = chbxRowAutoHeightClick
      Width = 180
    end
    object cbxDrawMode: TcxImageComboBox [16]
      Left = 10000
      Top = 10000
      Properties.Items = <>
      Style.HotTrack = False
      TabOrder = 9
      Visible = False
      OnClick = cbxDrawModeClick
      Width = 168
    end
    object chbxTransparent: TcxCheckBox [17]
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      Visible = False
      OnClick = chbxTransparentClick
      Width = 17
    end
    object stTransparent: TcxLabel [18]
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
      Width = 211
      AnchorY = 10009
    end
    object ccbxColor: TcxColorComboBox [19]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 12
      Visible = False
      Width = 124
    end
    object ccbxEvenColor: TcxColorComboBox [20]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 13
      Visible = False
      Width = 124
    end
    object ccbxGridLineColor: TcxColorComboBox [21]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 14
      Visible = False
      Width = 124
    end
    object btnFont: TcxButton [22]
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Fo&nt ...'
      TabOrder = 15
      Visible = False
      OnClick = btnFontClick
    end
    object edFont: TcxTextEdit [23]
      Left = 10000
      Top = 10000
      TabStop = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Text = 'edFont'
      Visible = False
      Width = 234
    end
    object btnEvenFont: TcxButton [24]
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'E&ven Font ...'
      TabOrder = 17
      Visible = False
      OnClick = btnFontClick
    end
    object edEvenFont: TcxTextEdit [25]
      Left = 10000
      Top = 10000
      TabStop = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Text = 'edFont'
      Visible = False
      Width = 234
    end
    object imgGrid: TcxImage [26]
      Left = 21
      Top = 69
      Properties.PopupMenuLayout.MenuItems = []
      Properties.ShowFocusRect = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Height = 48
      Width = 48
    end
    object Image1: TcxImage [27]
      Left = 21
      Top = 147
      Properties.PopupMenuLayout.MenuItems = []
      Properties.ShowFocusRect = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Height = 48
      Width = 48
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
    end
    inherited dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Index = 1
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Preview:'
      CaptionOptions.Layout = clTop
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 151
      ControlOptions.OriginalWidth = 185
      Index = 1
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object tshColor: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshColor'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 1
    end
    object tshOptions: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshOptions'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblShow
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      Control = imgGrid
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowHorzLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblMiscellaneous
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxFlatCheckMarks
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 180
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Visible = False
      Control = chbxRowAutoHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 101
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object tshFont: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'pnlFont'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object lblDrawMode: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblDrawMode'
      Control = cbxDrawMode
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      AlignVert = avCenter
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
      CaptionOptions.Text = 'lblEvenColor'
      Offsets.Left = 27
      Control = ccbxEvenColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lblGridLinesColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblGridLinesColor'
      Offsets.Left = 27
      Control = ccbxGridLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = tshColor
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = tshFont
      Control = edFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnEvenFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = tshFont
      Control = edEvenFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

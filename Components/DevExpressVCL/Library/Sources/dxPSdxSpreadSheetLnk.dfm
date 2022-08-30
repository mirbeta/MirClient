inherited fmdxSpreadSheet2DesignWindow: TfmdxSpreadSheet2DesignWindow
  Left = 402
  Top = 300
  Caption = 'SpreadSheet DesignWindow'
  ClientHeight = 400
  ClientWidth = 577
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 577
    Height = 400
    inherited btnApply: TcxButton
      Top = 332
      TabOrder = 28
    end
    inherited btnCancel: TcxButton
      Top = 332
      TabOrder = 27
    end
    inherited btnOK: TcxButton
      Top = 332
      TabOrder = 26
    end
    inherited btnHelp: TcxButton
      Top = 332
      TabOrder = 29
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 332
      TabOrder = 30
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 332
      TabOrder = 31
    end
    inherited btnTitleProperties: TcxButton
      Top = 332
      TabOrder = 32
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 332
      TabOrder = 33
    end
    object imgGrid: TcxImage [8]
      Left = 21
      Top = 69
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Height = 48
      Width = 48
    end
    object Image2: TcxImage [9]
      Left = 21
      Top = 148
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Height = 48
      Width = 48
    end
    object Image1: TcxImage [10]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object Image3: TcxImage [11]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object pnlPreview: TPanel [12]
      Left = 317
      Top = 29
      Width = 530
      Height = 280
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 25
    end
    object lblShow: TcxLabel [13]
      Left = 21
      Top = 44
      AutoSize = False
      Caption = 'Show'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 19
      Width = 278
    end
    object lblMiscellaneous: TcxLabel [14]
      Left = 21
      Top = 123
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 19
      Width = 278
    end
    object chbxShowRowAndColumnHeadings: TcxCheckBox [15]
      Left = 75
      Top = 69
      Caption = 'Row and column headings'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbxOptionsViewChanged
      Width = 224
    end
    object chbxShowGridlines: TcxCheckBox [16]
      Tag = 1
      Left = 75
      Top = 92
      Caption = 'Gridlines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = chbxOptionsViewChanged
      Width = 224
    end
    object chbxRowAutoHeight: TcxCheckBox [17]
      Left = 75
      Top = 148
      Caption = '&Row AutoHeight'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = chbxRowAutoHeightClick
      Width = 224
    end
    object chbxSuppressSourceFormats: TcxCheckBox [18]
      Tag = 2
      Left = 75
      Top = 171
      Caption = '&Suppress source formats'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = chbxOptionsViewChanged
      Width = 224
    end
    object chbxTransparent: TcxCheckBox [19]
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Visible = False
      OnClick = chbxFixedTransparentClick
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
      Width = 255
      AnchorY = 10009
    end
    object ccbxColor: TcxColorComboBox [21]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 10
      Visible = False
      Width = 168
    end
    object chbxFixedTransparent: TcxCheckBox [22]
      Tag = 1
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Visible = False
      OnClick = chbxFixedTransparentClick
      Width = 17
    end
    object stFixedTransparent: TcxLabel [23]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' Fixed Transparent '
      FocusControl = chbxFixedTransparent
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stFixedTransparentClick
      Height = 18
      Width = 255
      AnchorY = 10009
    end
    object ccbxFixedColor: TcxColorComboBox [24]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 13
      Visible = False
      Width = 168
    end
    object ccbxLineColor: TcxColorComboBox [25]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 14
      Visible = False
      Width = 168
    end
    object btnFont: TcxButton [26]
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Fo&nt...'
      TabOrder = 15
      Visible = False
      OnClick = btnFixedFontClick
    end
    object edFont: TcxTextEdit [27]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Visible = False
      Width = 278
    end
    object btnFixedFont: TcxButton [28]
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Fi&xed Font...'
      TabOrder = 17
      Visible = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFixedFontClick
    end
    object edFixedFont: TcxTextEdit [29]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Visible = False
      Width = 278
    end
    object lblSelection: TcxLabel [30]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Selection'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 278
    end
    object lblOnEveryPage: TcxLabel [31]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'On Every Page'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 278
    end
    object chbxFixedRowsOnEveryPage: TcxCheckBox [32]
      Left = 10000
      Top = 10000
      Caption = 'Fi&xed Rows On Every Page'
      Style.HotTrack = False
      TabOrder = 21
      Transparent = True
      Visible = False
      OnClick = chbxFixedRowsOnEveryPageClick
      Width = 224
    end
    object chbxOnlySelected: TcxCheckBox [33]
      Left = 10000
      Top = 10000
      Caption = 'Only &selected cells'
      Style.HotTrack = False
      TabOrder = 24
      Transparent = True
      Visible = False
      OnClick = chbxOnlySelectedClick
      Width = 224
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
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
      SizeOptions.Height = 300
      SizeOptions.Width = 300
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 168
      ControlOptions.OriginalWidth = 185
      Index = 1
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
    object dxLayoutItem2: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblMiscellaneous
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      Control = imgGrid
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      Control = Image2
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowRowAndColumnHeadings
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxShowGridlines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxRowAutoHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 102
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Visible = False
      Control = chbxSuppressSourceFormats
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 143
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = stTransparent
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      SizeOptions.Width = 300
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
      ItemIndex = 2
      Index = 0
    end
    object tshColor: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshColor'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 1
    end
    object tshFont: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFont'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshBehaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 3
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = tshColor
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lblColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblColor'
      Offsets.Left = 27
      Control = ccbxColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = chbxFixedTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
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
      Index = 2
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
      Index = 3
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = tshColor
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
    object lblGridLinesColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblGridLinesColor'
      Offsets.Left = 27
      Control = ccbxLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = tshFont
      Control = edFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFixedFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshFont
      Control = edFixedFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSelection
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblOnEveryPage
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahLeft
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahLeft
      Control = Image3
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxFixedRowsOnEveryPage
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 154
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxOnlySelected
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

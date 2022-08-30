object dxFEFDialog: TdxFEFDialog
  Left = 446
  Top = 168
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Fill Effects'
  ClientHeight = 329
  ClientWidth = 441
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 441
    Height = 329
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object pbxPreview: TPaintBox
      Left = 332
      Top = 222
      Width = 77
      Height = 74
      Color = clBtnFace
      ParentColor = False
      OnPaint = pbxPreviewPaint
    end
    object dgPattern: TDrawGrid
      Left = 10000
      Top = 10000
      Width = 280
      Height = 157
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvSpace
      BorderStyle = bsNone
      ColCount = 8
      DefaultColWidth = 34
      DefaultRowHeight = 25
      DefaultDrawing = False
      FixedCols = 0
      RowCount = 6
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
      ParentShowHint = False
      ScrollBars = ssNone
      ShowHint = True
      TabOrder = 2
      Visible = False
      OnClick = dgPatternClick
      OnDblClick = dgPatternDblClick
      OnDrawCell = dgPatternDrawCell
      OnMouseMove = dgPatternMouseMove
      ColWidths = (
        34
        34
        34
        34
        34
        34
        34
        34)
      RowHeights = (
        25
        25
        25
        25
        25
        25)
    end
    object cbxForeColor: TcxColorComboBox
      Left = 10000
      Top = 10000
      Properties.CustomColors = <>
      Properties.OnChange = cbxColorChange
      Style.HotTrack = False
      TabOrder = 3
      Visible = False
      Width = 124
    end
    object cbxBackColor: TcxColorComboBox
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Properties.CustomColors = <>
      Properties.OnChange = cbxColorChange
      Style.HotTrack = False
      TabOrder = 4
      Visible = False
      Width = 125
    end
    object btnInvert: TcxButton
      Left = 10000
      Top = 10000
      Width = 282
      Height = 23
      Caption = 'I&nvert colors'
      TabOrder = 5
      Visible = False
      OnClick = btnInvertClick
    end
    object btnPreview: TcxButton
      Left = 10000
      Top = 10000
      Width = 121
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Pre&view ...'
      TabOrder = 7
      Visible = False
      OnClick = PicturePreviewClick
    end
    object btnSelectPicture: TcxButton
      Left = 10000
      Top = 10000
      Width = 121
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Se&lect Picture ...'
      TabOrder = 8
      Visible = False
      OnClick = SelectPictureClick
    end
    object btnOK: TcxButton
      Left = 320
      Top = 10
      Width = 101
      Height = 23
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 9
    end
    object btnCancel: TcxButton
      Left = 320
      Top = 39
      Width = 101
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 10
    end
    object btnApply: TcxButton
      Left = 320
      Top = 68
      Width = 101
      Height = 23
      Caption = '&Apply'
      TabOrder = 11
      OnClick = btnApplyClick
    end
    object btnHelp: TcxButton
      Left = 320
      Top = 97
      Width = 101
      Height = 23
      Caption = '&Help'
      TabOrder = 12
    end
    object cbxPaintMode: TcxComboBox
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbxPaintModeChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Visible = False
      Width = 282
    end
    object dgTexture: TDrawGrid
      Left = 22
      Top = 45
      Width = 280
      Height = 202
      BevelInner = bvNone
      BevelKind = bkFlat
      BevelOuter = bvSpace
      BorderStyle = bsNone
      ColCount = 4
      Ctl3D = True
      DefaultColWidth = 65
      DefaultRowHeight = 65
      DefaultDrawing = False
      FixedCols = 0
      RowCount = 6
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goThumbTracking]
      ParentCtl3D = False
      ScrollBars = ssVertical
      TabOrder = 0
      OnClick = dgTextureClick
      OnDblClick = dgTextureDblClick
      OnDrawCell = dgTextureDrawCell
      OnMouseMove = dgTextureMouseMove
      ColWidths = (
        65
        65
        65
        65)
      RowHeights = (
        65
        65
        65
        65
        65
        65)
    end
    object btnOtherTexture: TcxButton
      Left = 21
      Top = 274
      Width = 121
      Height = 23
      Caption = '&Other Texture ...'
      TabOrder = 1
      OnClick = btnOtherTextureClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object lgTabbed: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      OnTabChanged = PageControl1Change
      Index = 0
    end
    object tshTexture: TdxLayoutGroup
      Parent = lgTabbed
      CaptionOptions.Text = '&Texture'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object tshPattern: TdxLayoutGroup
      Parent = lgTabbed
      CaptionOptions.Text = '&Pattern'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = tshPattern
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avClient
      Control = dgPattern
      ControlOptions.OriginalHeight = 157
      ControlOptions.OriginalWidth = 280
      Index = 0
    end
    object pnlPatternName: TdxLayoutLabeledItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = '-'
      Index = 1
    end
    object dxLayoutGroup8: TdxLayoutGroup
      Parent = tshPattern
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lblForeground: TdxLayoutItem
      Parent = dxLayoutGroup8
      CaptionOptions.Text = '&Foreground : '
      CaptionOptions.Layout = clTop
      Control = cbxForeColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 124
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblBackground: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahRight
      CaptionOptions.Text = '&Background : '
      CaptionOptions.Layout = clTop
      Control = cbxBackColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = tshPattern
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'btnInvert'
      CaptionOptions.Visible = False
      Control = btnInvert
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 280
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object tshPicture: TdxLayoutGroup
      Parent = lgTabbed
      CaptionOptions.Text = 'P&icture'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object dxLayoutGroup9: TdxLayoutGroup
      Parent = tshPicture
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignVert = avBottom
      CaptionOptions.Text = 'btnPreview'
      CaptionOptions.Visible = False
      Control = btnPreview
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnSelectPicture'
      CaptionOptions.Visible = False
      Control = btnSelectPicture
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup10: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahClient
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 101
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahClient
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblApply: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahClient
      CaptionOptions.Text = 'btnApply'
      CaptionOptions.Visible = False
      Control = btnApply
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblHelp: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahClient
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lblSample: TdxLayoutGroup
      Parent = dxLayoutGroup10
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'Sample : '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 4
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = lblSample
      AlignHorz = ahClient
      AlignVert = avClient
      Control = pbxPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 74
      ControlOptions.OriginalWidth = 77
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPaintMode: TdxLayoutItem
      Parent = tshPicture
      CaptionOptions.Text = 'Paint &mode'
      CaptionOptions.Layout = clTop
      Control = cbxPaintMode
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 161
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlPicture: TdxLayoutItem
      Parent = tshPicture
      AlignVert = avClient
      CaptionOptions.Text = '-'
      CaptionOptions.Layout = clBottom
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      Control = dgTexture
      ControlOptions.OriginalHeight = 199
      ControlOptions.OriginalWidth = 280
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOtherTexture'
      CaptionOptions.Visible = False
      Control = btnOtherTexture
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = tshTexture
      Index = 0
      AutoCreated = True
    end
    object pnlTextureName: TdxLayoutLabeledItem
      Parent = dxLayoutAutoCreatedGroup1
      CaptionOptions.Text = '-'
      Index = 1
    end
  end
  object pmPicture: TPopupMenu
    Images = ilMenu
    OnPopup = pmPicturePopup
    Left = 372
    Top = 50
    object miPreview: TMenuItem
      Caption = 'Pre&view...'
      ImageIndex = 8
      OnClick = PicturePreviewClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object miLoad: TMenuItem
      Caption = '&Load...'
      ImageIndex = 13
      ShortCut = 45
      OnClick = SelectPictureClick
    end
    object miDelete: TMenuItem
      Caption = '&Delete'
      ImageIndex = 5
      ShortCut = 46
      OnClick = miDeleteClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miCut: TMenuItem
      Caption = 'Cu&t'
      ImageIndex = 3
      ShortCut = 16472
      OnClick = miCutClick
    end
    object miCopy: TMenuItem
      Caption = '&Copy'
      ImageIndex = 2
      ShortCut = 16451
      OnClick = miCopyClick
    end
    object miPaste: TMenuItem
      Caption = '&Paste'
      ImageIndex = 4
      ShortCut = 16470
      OnClick = miPasteClick
    end
  end
  object ilMenu: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 5243256
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 632
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

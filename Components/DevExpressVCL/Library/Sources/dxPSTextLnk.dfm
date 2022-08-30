inherited dxfmTextReportLinkDesignWindow: TdxfmTextReportLinkDesignWindow
  Left = 375
  Top = 286
  Caption = 'dxfmTextReportLinkDesignWindow'
  ClientHeight = 363
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Height = 363
    Constraints.MinWidth = 560
    inherited btnApply: TcxButton
      Left = 676
      Top = 329
      TabOrder = 7
    end
    inherited btnCancel: TcxButton
      Left = 595
      Top = 329
      TabOrder = 6
    end
    inherited btnOK: TcxButton
      Left = 514
      Top = 329
      TabOrder = 5
    end
    inherited btnHelp: TcxButton
      Left = 757
      Top = 329
      TabOrder = 8
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 329
      TabOrder = 9
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 329
      TabOrder = 10
    end
    inherited btnTitleProperties: TcxButton
      Top = 329
      TabOrder = 11
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 329
      TabOrder = 12
    end
    object pnlPreview: TPanel [8]
      Left = 570
      Top = 61
      Width = 250
      Height = 250
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 4
      object pbxPreview: TPaintBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 244
        Height = 244
        Align = alClient
        OnPaint = pbxPreviewPaint
      end
    end
    object cbxAlignment: TcxComboBox [9]
      Left = 92
      Top = 60
      Properties.DropDownListStyle = lsFixedList
      Properties.OnDrawItem = cbxAlignmentPropertiesDrawItem
      Style.HotTrack = False
      TabOrder = 0
      OnClick = cbxAlignmentClick
      Width = 471
    end
    object seLineSpacing: TcxSpinEdit [10]
      Left = 92
      Top = 87
      Properties.Increment = 0.100000000000000000
      Properties.MaxValue = 10.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValueType = vtFloat
      Properties.OnChange = LineSpacingChanged
      Style.HotTrack = False
      TabOrder = 1
      Value = 1.000000000000000000
      Width = 471
    end
    object btnFont: TcxButton [11]
      Left = 21
      Top = 126
      Width = 75
      Height = 23
      Caption = 'Fo&nt ...'
      TabOrder = 2
      OnClick = btnFontClick
    end
    object edFont: TcxTextEdit [12]
      Left = 21
      Top = 155
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      TabOrder = 3
      Width = 542
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
    end
    inherited libtnApply: TdxLayoutItem
      Parent = dxLayoutGroup2
      Index = 2
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = nil
      Index = -1
      Special = True
    end
    inherited libtnCancel: TdxLayoutItem
      Parent = dxLayoutGroup2
      Index = 1
    end
    inherited libtnOk: TdxLayoutItem
      Parent = dxLayoutGroup2
      Index = 0
    end
    inherited libtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup2
      Index = 3
    end
    inherited libtnRestoreOriginal: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      Index = 4
    end
    inherited libtnRestoreDefaults: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      Index = 5
    end
    inherited libtnTitleProperties: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      Index = 6
    end
    inherited libtnFootnoteProperties: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      Index = 7
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 1
    end
    object tshOptions: TdxLayoutGroup
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Options'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object lblPreview: TdxLayoutItem
      Parent = tshOptions
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Layout = clTop
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 250
      ControlOptions.OriginalWidth = 250
      Index = 1
    end
    object lblAlignment: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'lblAlignment'
      Control = cbxAlignment
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblLineSpacing: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'lblLineSpacing'
      Control = seLineSpacing
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      AlignHorz = ahClient
      Index = 0
      AutoCreated = True
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup2
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      Control = edFont
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ilAlignments: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 4194336
    ImageInfo = <
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          424D360400000000000036000000280000001000000010000000010020000000
          000000000000C40E0000C40E0000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          424D360400000000000036000000280000001000000010000000010020000000
          000000000000C40E0000C40E0000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          424D360400000000000036000000280000001000000010000000010020000000
          000000000000C40E0000C40E0000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000}
      end
      item
        ImageClass = 'TdxSmartImage'
        Image.Data = {
          424D360400000000000036000000280000001000000010000000010020000000
          000000000000C40E0000C40E0000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000FF000000FF0000
          00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000}
      end>
  end
end

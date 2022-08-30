object dxfmAddComponent: TdxfmAddComponent
  Left = 412
  Top = 157
  BorderStyle = bsDialog
  ClientHeight = 468
  ClientWidth = 392
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 398
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000000000000000000000FFFFFFFFF0000000F
    FFFFFFFF0000000FFFFFFFFF0000000FFFFFFFFF0000000FFFFFFFFF0000000F
    FFFFFFFF0000000FFFFFFFFF0000000FFFFFFFFF0000000FFFFFF0000000000F
    FFFFF0F00000000FFFFFF000000000000000000000000000000000000000FFFF
    0000FFFF0000C0070000C0070000C0070000C0070000C0070000C0070000C007
    0000C0070000C0070000C0070000C00F0000C01F0000C03F0000FFFF0000}
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 392
    Height = 468
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object edName: TcxTextEdit
      Left = 56
      Top = 10
      Anchors = [akLeft, akTop, akRight]
      Style.HotTrack = False
      TabOrder = 0
      Width = 326
    end
    object edCaption: TcxTextEdit
      Left = 56
      Top = 37
      Anchors = [akLeft, akTop, akRight]
      Style.HotTrack = False
      TabOrder = 1
      Width = 326
    end
    object edCreator: TcxTextEdit
      Left = 56
      Top = 64
      Anchors = [akLeft, akTop, akRight]
      Style.HotTrack = False
      TabOrder = 2
      Width = 237
    end
    object btnDescription: TcxButton
      Left = 299
      Top = 64
      Width = 83
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Description...'
      TabOrder = 3
      OnClick = btnDescriptionClick
    end
    object lvItems: TcxListView
      Left = 21
      Top = 127
      Width = 350
      Height = 195
      Columns = <>
      HideSelection = False
      MultiSelect = True
      PopupMenu = pmItems
      ReadOnly = True
      RowSelect = True
      TabOrder = 4
      ViewStyle = vsReport
      OnChange = lvItemsChange
      OnColumnClick = lvItemsColumnClick
      OnCompare = lvItemsCompare
      OnDblClick = lvItemsDblClick
      OnResize = lvItemsResize
      OnSelectItem = lvItemsSelectItem
    end
    object chbxHideCustomContainers: TcxCheckBox
      Left = 21
      Top = 355
      Caption = 'Hide Custom Containers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = chbxClick
    end
    object chbxOnlyInCurrentModule: TcxCheckBox
      Left = 21
      Top = 378
      Caption = 'Only Components in the Active &Form'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = chbxClick
    end
    object chbxOnlyUnLinked: TcxCheckBox
      Left = 21
      Top = 401
      Caption = 'Only Components &without Existing ReportLinks'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      OnClick = chbxClick
    end
    object btnOK: TcxButton
      Left = 24
      Top = 435
      Width = 85
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 9
    end
    object btnCancel: TcxButton
      Left = 115
      Top = 435
      Width = 85
      Height = 23
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = 'OK'
      ModalResult = 2
      TabOrder = 10
    end
    object btnDesign: TcxButton
      Left = 206
      Top = 435
      Width = 85
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      TabOrder = 11
      OnClick = btnDesignClick
    end
    object btnHelp: TcxButton
      Left = 297
      Top = 435
      Width = 85
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      TabOrder = 12
    end
    object cbReportLinkClasses: TcxComboBox
      Left = 21
      Top = 328
      Properties.DropDownListStyle = lsFixedList
      Properties.ImmediatePost = True
      Properties.ValidationErrorIconAlignment = taRightJustify
      Properties.ValidationOptions = [evoShowErrorIcon]
      Properties.OnChange = cbReportLinkClassesPropertiesChange
      Properties.OnValidate = cbReportLinkClassesPropertiesValidate
      Style.HotTrack = False
      TabOrder = 5
      Width = 350
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = 0
    end
    object lblName: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = '&Name:'
      Control = edName
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 236
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblCaption: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = '&Caption:'
      Control = edCaption
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 236
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object pnlCreatorHost: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lblCreator: TdxLayoutItem
      Parent = pnlCreatorHost
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Creato&r:'
      Control = edCreator
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 236
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = pnlCreatorHost
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnDescription'
      CaptionOptions.Visible = False
      Control = btnDescription
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup11: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 3
    end
    object tbsItems: TdxLayoutGroup
      Parent = dxLayoutGroup11
      CaptionOptions.Text = '&Available Components'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = tbsItems
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lvItems
      ControlOptions.OriginalHeight = 201
      ControlOptions.OriginalWidth = 360
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = tbsItems
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 2
    end
    object lichbxHideCustomContainers: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'chbxHideCustomContainers'
      CaptionOptions.Visible = False
      Control = chbxHideCustomContainers
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 135
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lichbxOnlyInCurrentModule: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'chbxOnlyInCurrentModule'
      CaptionOptions.Visible = False
      Control = chbxOnlyInCurrentModule
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 195
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lichbxOnlyUnLinked: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = 'chbxOnlyUnLinked'
      CaptionOptions.Visible = False
      Control = chbxOnlyUnLinked
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 243
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnDesign: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignVert = avBottom
      CaptionOptions.Text = 'btnDesign'
      CaptionOptions.Visible = False
      Control = btnDesign
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignVert = avBottom
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liReportLinkClasses: TdxLayoutItem
      Parent = tbsItems
      Control = cbReportLinkClasses
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object pnlNoItems: TcxLabel
    Left = 21
    Top = 226
    TabStop = False
    AutoSize = False
    Caption = 'There are no Items in this View'
    ParentColor = False
    ParentFont = False
    Style.TextColor = clBtnShadow
    Properties.Alignment.Horz = taCenter
    Transparent = True
    Height = 19
    Width = 345
    AnchorX = 194
  end
  object pmItems: TPopupMenu
    Images = ilItems
    OnPopup = pmItemsPopup
    Left = 88
    Top = 136
    object miAdd: TMenuItem
      Caption = '&Add'
      Default = True
      OnClick = lvItemsDblClick
    end
    object miLine1: TMenuItem
      Caption = '-'
    end
    object miDesign: TMenuItem
      Caption = 'Add and D&esign Report...'
      ImageIndex = 0
      OnClick = btnDesignClick
    end
  end
  object ilItems: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 8912954
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000A0000001B0000001D0000
          000C000000020000000100000000000000000000000000000000000000000000
          00000000000100000005000000060000000B0A08053A624C35E2604B34E30C09
          0749000000110000000A00000009000000030000000000000000000000000000
          00010000000916120D431C1610560907053D544635BFA99A8AFF9D8D7AFF5241
          30CB0806045118130D6814100A59000000160000000300000000000000000000
          000318130E41917C63FD947F68FF7D654BFAA89887FFC2B9AFFFB6ACA0FF9D8D
          7BFF765E44FB816B51FF7D654BFD17120C5F0000000A00000001000000000000
          00031C16104798836CFFE7E4E0FFC5BDB3FFCEC6BDFFCCC3BAFFC2BAAFFFB6AC
          A0FFB6AA9FFFB6AA9FFF826B52FF1A140D6B0000000B00000001000000000000
          000409070626876E54F9D9D3CEFFD1C9C1FFCFC7BFFFD9D3CCFFE7E2DEFFE5E1
          DDFFD5CFC7FFB6AB9FFF765E44FB080604510000001000000002000000020A08
          061C615040B4BEB2A4FFD3CCC6FFD2CBC4FFB5A89AFF98846EFF99846FFFC9BE
          B3FFE6E1DDFFB6ACA1FF9E8E7DFF534331CD0806043F0000000B000000046853
          3FC5CABFB4FFE0DCD8FFD5CFC9FFCBC5BDFF917B64FF2019123F1F19123F9984
          70FFE7E3DEFFC4BAB1FFB6ACA1FF9E8E7CFF584530D60000001B00000003725C
          46D5CEC3B9FFF1EFEDFFE0DCD8FFC4BDB5FF917B64FF2019133F1F19123F9883
          6EFFD9D3CCFFCCC4BBFFC4BAB1FFAB9D8DFF624C35E20000001A000000010D0B
          081D645543B1CEC3B8FFF1EFEDFFC6BFB9FFAA9D8EFF917C65FF917C64FFB6A8
          9AFFCFC7C0FFCEC6BDFFAC9E8DFF594938C60D0A07400000000A000000000000
          00010A0806198D775EF8EAE7E4FFCFCAC5FFC6C0B9FFC4BDB6FFCBC6BEFFD2CC
          C5FFD1CAC3FFC6BEB6FF7D654BFA0907053E0000000C00000002000000000000
          00001E19123AA18D77FFEFEDEBFFEBE7E4FFF1EFEDFFE1DED9FFD5CFCAFFD3CD
          C7FFD9D4CFFFE7E4E1FF95806AFF1D1710590000000700000001000000000000
          000019150F2F9D8971FDA18D77FF8D775DF8C9BEB2FFF1EFEDFFE1DEDAFFBEB2
          A5FF876E54F998836BFF927C65FD19140E490000000500000000000000000000
          0000000000001915102F1E19123A0A0806195F5040A8C9BEB2FFCAC0B5FF6151
          40B4090706261C16104718130E41000000090000000200000000000000000000
          0000000000000000000000000000000000010D0B081D766048DD766047DE0E0B
          0826000000040000000300000003000000010000000000000000000000000000
          0000000000000000000000000000000000000000000100000003000000050000
          0002000000010000000000000000000000000000000000000000}
      end>
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 27
    Top = 136
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ilColumns: TcxImageList
    SourceDPI = 96
    Height = 8
    Width = 8
    FormatVersion = 1
    DesignInfo = 8913014
    ImageInfo = <
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36010000424D3601000000000000360000002800000008000000080000000100
          2000000000000001000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000008080
          8000808080008080800080808000808080008080800080808000000000000000
          0000808080008080800080808000808080008080800000000000000000000000
          0000000000008080800080808000808080000000000000000000000000000000
          0000000000000000000080808000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          5E000000424D5E000000000000003E0000002800000008000000080000000100
          010000000000200000000000000000000000020000000000000000000000FFFF
          FF00FF000000FF00000080000000C1000000E3000000F7000000FF000000FF00
          0000}
      end
      item
        ImageClass = 'TBitmap'
        Image.Data = {
          36010000424D3601000000000000360000002800000008000000080000000100
          2000000000000001000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000080808000000000000000000000000000000000000000
          0000000000008080800080808000808080000000000000000000000000000000
          0000808080008080800080808000808080008080800000000000000000008080
          8000808080008080800080808000808080008080800080808000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          5E000000424D5E000000000000003E0000002800000008000000080000000100
          010000000000200000000000000000000000020000000000000000000000FFFF
          FF00FF000000FF000000F7000000E3000000C100000080000000FF000000FF00
          0000}
      end>
  end
end

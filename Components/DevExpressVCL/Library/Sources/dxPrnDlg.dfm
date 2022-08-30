object dxfmPrintDialog: TdxfmPrintDialog
  Left = 281
  Top = 188
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Print'
  ClientHeight = 500
  ClientWidth = 535
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 535
    Height = 500
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object pbxCollate: TPaintBox
      Left = 332
      Top = 269
      Width = 118
      Height = 54
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      OnDblClick = pbxCollateDblClick
      OnPaint = pbxCollatePaint
    end
    object btnPreview: TcxButton
      Left = 112
      Top = 447
      Width = 96
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Print Pre&view'
      TabOrder = 22
      OnClick = btnPreviewClick
    end
    object btnPageSetup: TcxButton
      Left = 10
      Top = 447
      Width = 96
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'Page Set&up...'
      OptionsImage.Layout = blGlyphRight
      TabOrder = 21
      OnClick = btnPageSetupClick
    end
    object btnOK: TcxButton
      Left = 249
      Top = 447
      Width = 85
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 23
    end
    object btnCancel: TcxButton
      Left = 340
      Top = 447
      Width = 85
      Height = 23
      Anchors = [akLeft, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 24
    end
    object btnHelp: TcxButton
      Left = 431
      Top = 447
      Width = 85
      Height = 23
      Anchors = [akLeft, akBottom]
      Caption = '&Help'
      TabOrder = 25
      OnClick = btnHelpClick
    end
    object lbxPrintStyles: TcxListBox
      Left = 22
      Top = 359
      Width = 368
      Height = 70
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      PopupMenu = pmPrintStyles
      Style.TransparentBorder = False
      TabOrder = 18
      OnClick = lbxPrintStylesClick
      OnDblClick = PageSetup2Click
      OnDrawItem = lbxPrintStylesDrawItem
    end
    object btnDefineStyles: TcxButton
      Left = 396
      Top = 388
      Width = 96
      Height = 23
      Caption = 'Define S&tyles...'
      TabOrder = 20
      OnClick = DefineStylesClick
    end
    object btnPageSetup2: TcxButton
      Left = 396
      Top = 359
      Width = 96
      Height = 23
      Caption = 'Page Set&up...'
      Default = True
      TabOrder = 19
      OnClick = PageSetup2Click
    end
    object seCopies: TcxSpinEdit
      Left = 372
      Top = 221
      Properties.SpinButtons.Position = sbpHorzRight
      Properties.OnChange = seCopiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      OnExit = seCopiesExit
      Width = 132
    end
    object cbxNumberOfPages: TcxComboBox
      Left = 372
      Top = 196
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbxNumberOfPagesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Width = 132
    end
    object chbxCollate: TcxCheckBox
      Left = 278
      Top = 246
      Caption = 'Co&llate copies'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Transparent = True
      OnClick = chbxCollateClick
    end
    object rbtnAllPages: TcxRadioButton
      Left = 22
      Top = 196
      Width = 75
      Height = 17
      Caption = '&All'
      Checked = True
      TabOrder = 10
      TabStop = True
      OnClick = rbtnPagesClick
      Transparent = True
    end
    object rbtnCurrentPage: TcxRadioButton
      Tag = 1
      Left = 22
      Top = 219
      Width = 100
      Height = 17
      Caption = 'Curr&ent page'
      TabOrder = 11
      OnClick = rbtnPagesClick
      Transparent = True
    end
    object rbtnPageRanges: TcxRadioButton
      Tag = 2
      Left = 22
      Top = 242
      Width = 75
      Height = 17
      Caption = 'Pa&ges: '
      TabOrder = 13
      OnClick = rbtnPagesClick
      Transparent = True
    end
    object edPageRanges: TcxTextEdit
      Left = 103
      Top = 242
      Properties.OnChange = edPageRangesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      OnExit = edPageRangesExit
      OnKeyPress = edPageRangesKeyPress
      Width = 145
    end
    object rbtnSelection: TcxRadioButton
      Tag = 3
      Left = 128
      Top = 219
      Width = 75
      Height = 17
      Caption = '&Selection'
      TabOrder = 12
      OnClick = rbtnPagesClick
      Transparent = True
    end
    object cbxPrinters: TcxComboBox
      Left = 76
      Top = 28
      AutoSize = False
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 22
      Properties.OnChange = cbxPrintersChange
      Properties.OnDrawItem = cbxPrintersPropertiesDrawItem
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 23
      Width = 347
    end
    object btnPrinterProperties: TcxButton
      Left = 429
      Top = 28
      Width = 75
      Height = 23
      Caption = 'P&roperties...'
      TabOrder = 8
      OnClick = btnPrinterPropertiesClick
    end
    object btnNetwork: TcxButton
      Left = 429
      Top = 57
      Width = 75
      Height = 23
      Caption = 'Net&work...'
      TabOrder = 9
      OnClick = btnNetworkClick
    end
    object lStatus: TcxLabel
      Left = 76
      Top = 57
      Caption = 'Status'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Transparent = True
    end
    object lType: TcxLabel
      Left = 76
      Top = 77
      Caption = 'Type'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Transparent = True
    end
    object lWhere: TcxLabel
      Left = 76
      Top = 97
      Caption = 'Where'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Transparent = True
    end
    object lComment: TcxLabel
      Left = 76
      Top = 117
      Caption = 'Comment'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Transparent = True
    end
    object chbxPrintToFile: TcxCheckBox
      Left = 22
      Top = 140
      Caption = 'Print to &file'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = chbxPrintToFileClick
    end
    object cbxFileName: TcxComboBox
      Left = 135
      Top = 137
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Text = 'cbxFileName'
      OnExit = cbxFileNameExit
      Height = 23
      Width = 288
    end
    object btnBrowse: TcxButton
      Left = 429
      Top = 137
      Width = 75
      Height = 23
      Caption = '&Browse...'
      TabOrder = 7
      OnClick = btnBrowseClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object libtnPreview: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'btnPreview'
      CaptionOptions.Visible = False
      Control = btnPreview
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnPageSetup: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'btnPageSetup'
      CaptionOptions.Visible = False
      Control = btnPageSetup
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object libtnOK: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnCancel: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object gbxPrintStyles: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Print Styles'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = gbxPrintStyles
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = lbxPrintStyles
      ControlOptions.OriginalHeight = 70
      ControlOptions.OriginalWidth = 368
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object libtnDefineStyles: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btnDefineStyles'
      CaptionOptions.Visible = False
      Control = btnDefineStyles
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnPageSetup2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'btnPageSetup2'
      CaptionOptions.Visible = False
      Control = btnPageSetup2
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = gbxPrintStyles
      AlignHorz = ahLeft
      Index = 1
      AutoCreated = True
    end
    object gbxCopies: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = 'C&opies'
      SizeOptions.Width = 250
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = gbxCopies
      AlignHorz = ahCenter
      Control = pbxCollate
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 54
      ControlOptions.OriginalWidth = 118
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lblNumberOfCopies: TdxLayoutItem
      Parent = gbxCopies
      CaptionOptions.Text = 'seCopies'
      Control = seCopies
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblNumberOfPages: TdxLayoutItem
      Parent = gbxCopies
      CaptionOptions.Text = 'Number of pa&ges :'
      Control = cbxNumberOfPages
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = gbxCopies
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxCollate
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object gbxPageRange: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Text = ' Page ra&nge '
      SizeOptions.Width = 250
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 0
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lirbtnAllPages: TdxLayoutItem
      Parent = gbxPageRange
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = rbtnAllPages
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lirbtnCurrentPage: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbtnCurrentPage
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lirbtnPageRanges: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = rbtnPageRanges
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liedPageRanges: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      Control = edPageRanges
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = gbxPageRange
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lirbtnSelection: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = rbtnSelection
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = gbxPageRange
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object gbxPrinter: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = ' &Printer '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object lblName: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Name:'
      Control = cbxPrinters
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object libtnPrinterProperties: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = btnPrinterProperties
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnNetwork: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      CaptionOptions.Visible = False
      Control = btnNetwork
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = gbxPrinter
      AlignHorz = ahRight
      Index = 1
      AutoCreated = True
    end
    object lblStatus: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Text = 'Status:'
      Control = lStatus
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 31
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = gbxPrinter
      AlignHorz = ahClient
      Index = 0
      AutoCreated = True
    end
    object lblType: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Text = 'Type:'
      Control = lType
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblWhere: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Text = 'Where:'
      Control = lWhere
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 32
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lblComment: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Text = 'Comment:'
      Control = lComment
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 45
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lichbxPrintToFile: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxPrintToFile
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 45
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object licbxFileName: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      AlignVert = avClient
      Control = cbxFileName
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup9
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object libtnBrowse: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = btnBrowse
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblDescription: TdxLayoutLabeledItem
      Parent = gbxPageRange
      AlignVert = avTop
      CaptionOptions.Text = 'Enter page number and/or page ranges'
      CaptionOptions.WordWrap = True
      Index = 3
    end
    object bvlPRWarningHolder: TdxLayoutItem
      Parent = gbxPageRange
      AlignHorz = ahClient
      AlignVert = avClient
      Index = 4
    end
  end
  object pmPrintStyles: TPopupMenu
    OnPopup = pmPrintStylesPopup
    Left = 27
    Top = 357
    object miPageSetup: TMenuItem
      Caption = 'Page Set&up...'
      Default = True
      ShortCut = 16397
      OnClick = PageSetup2Click
    end
    object miLine1: TMenuItem
      Caption = '-'
    end
    object miDefineStyles: TMenuItem
      Caption = 'Define Styles...'
      OnClick = DefineStylesClick
    end
  end
  object ilPrinters: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 1900672
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000500000012000000170000001700000018000000180000
          0018000000190000001400000005000000000000000000000000000000030000
          000C0000001300000020C79060FFC68F5EFFC58D5DFFC48C5BFFC48B5AFFC38A
          5AFFC38858FFC28858FF00000026000000170000000F000000040000000B705D
          4EC49A806AFF997F69FFC89161FFFAF2EBFFFBF1E9FFFAF0E7FFFAEFE6FFF9EE
          E4FFF9EDE3FFC28958FF8A6F58FF896D56FF604E3DC80000000F000000109E84
          6EFFFFFFFFFFDED8D2FFC99362FFFBF2EBFFFBF1EAFFFAF1E9FFFAF0E8FFFAF0
          E6FFF9EEE5FFC3895AFFD1C8BFFFD1C7BEFF896D57FF00000017000000109F85
          70FFFFFFFFFFEEEBE8FF9F816BFF9D7F68FF997B66FF957861FF90735DFF8B6F
          58FF876C54FF836851FFDFD9D3FFD1C8BFFF8A6E58FF000000180000000FA186
          71FFFFFFFFFFEFECEAFFEDEAE8FFECE8E4FFEAE6E2FFE8E4E0FFE6E2DDFFE5DF
          DBFFE3DDD8FFE2DCD6FFE0DAD4FFD2C8C0FF8B7059FF000000180000000EA287
          73FFFFFFFFFFC9905BFFB66921FFB5671FFFB3641CFFB1621AFFAF5F18FFAD5D
          16FFAC5C15FFAB5A13FFBA7E49FFD3CAC1FF8D725CFF000000170000000CA489
          74FFFFFFFFFFBA6D25FFF2B17BFFF0AE7AFFEEAC77FFEDA976FFEAA674FFE9A4
          70FFE8A26FFFE6A16DFFAA5A13FFD4CAC2FF8E735DFF000000170000000BA48B
          76FFFFFFFFFFBC6F27FFC47D39FFC37A36FFBF7733FFBD732EFFB96D2AFFB669
          25FFB36622FFB2631FFFAB5B14FFD4CBC3FF90755FFF0000001600000009A68C
          77FFFFFFFFFFBD7128FFC79061FFFBF2ECFFFAF2EBFFFAF1E9FFFAF0E7FFF9EE
          E5FFF9EDE3FFC28858FFAC5D15FFFFFFFFFF917661FF00000013000000057C6A
          59C3A68C77FFB8793DFFC79161FFFBF3EDFFFBF2ECFFFAF1EBFFFAF1E9FFF9EF
          E7FFFAEFE5FFC38A59FFA8662BFF957A64FF6D5A49C90000000D000000010000
          00050000000800000011C99263FFFBF3EDFFFBF3EDFFFBF2ECFFFBF1EAFFFBF0
          E9FFFAF0E7FFC38A5BFF0000001E000000100000000C00000003000000000000
          00000000000000000008C99365FFFBF3EDFFFBF3EDFFFBF3EDFFFBF2ECFFFAF2
          EBFFFAF1E9FFC48B5CFF00000010000000000000000000000000000000000000
          00000000000000000007CA9565FFFBF3EDFFFBF3EDFFFBF3EDFFFBF3EDFFFBF2
          ECFFFBF2EBFFC48C5DFF0000000F000000000000000000000000000000000000
          00000000000000000005CB9666FFCA9466FFC99464FFC89363FFC89161FFC78F
          60FFC68F5FFFC58D5DFF0000000B000000000000000000000000000000000000
          00000000000000000001000000050000000700000008000000090000000A0000
          000B0000000C0000000A00000003000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000002A73B7FF276DB2FF2667ADFF2461
          A9FF000000000000000000000000000000000000000000000000000000007A5D
          46FF7A5D46FF7A5D46FF7A5D46FF7A5D46FF2D7CBEFFA3E7F8FF62CDEFFF2669
          AEFF7A5D46FF7A5D46FF7A5D46FF7A5D46FF7A5D46FF0000000000000000B4A0
          91FFB4A091FFB4A091FFB4A091FFB4A091FF2F84C3FFAEEDFAFFA3E6F8FF2972
          B5FFB4A091FFB4A091FFB4A091FFB4A091FFB4A091FF00000000000000000000
          0000000000000000000000000000000000003189C7FF2F86C4FF2E80C0FF2C7A
          BCFF000000000000000000000000000000000000000000000000000000000000
          0002000000080000000E0000000F0000001000000011B4A091FF7A5D46FF0000
          00130000001400000015000000150000000E0000000400000000000000000000
          0007705D4DBF9A806AFF997E68FF987D67FF967B65FF947963FF927761FF9075
          5FFF8F745DFF8D725CFF8C705AFF635040C50000000D00000000000000000000
          00099E846EFFFFFFFFFFDDD7D1FFDCD5CFFFDBD3CDFFD9D2CAFFD7D0C8FFD7CE
          C7FFD5CDC4FFD3CAC3FFD2C9C1FF8C715AFF0000001300000000000000000000
          00079F856FFFFFFFFFFFEDEAE8FFECE8E4FFEAE6E2FFE8E4E0FFE6E2DDFFE5DF
          DBFFE3DDD8FFE2DCD6FFD3CAC2FF8D725CFF0000001300000000000000000000
          0006A08772FFFFFFFFFFC68E59FFB5671FFFB3641CFFB1621AFFAF5F18FFAD5D
          16FFAC5C15FFBB7F4AFFD4CBC3FF8F745DFF0000001100000000000000000000
          0004A28873FFFFFFFFFFB96B22FFF2B17BFFEEAC77FFEDA976FFEAA674FFE8A2
          6FFFE6A16DFFAC5B14FFD5CBC4FF90755FFF0000000F00000000000000000000
          0003A38A75FFFFFFFFFFBA6D25FFC27936FFBE7631FFBB702CFFB76A27FFB467
          22FFB26421FFAD5D15FFD6CDC5FF917761FF0000000E00000000000000000000
          0002A58B76FFFFFFFFFFBC6F27FFC68F5FFFFAF2EBFFFAF1E9FFFAF0E7FFF9EE
          E5FFC2895AFFAE5F17FFFFFFFFFF937863FF0000000C00000000000000000000
          00017B6859BEA58B75FFC6874EFFC79060FFFBF2ECFFFAF1EBFFFAF1E9FFF9EF
          E7FFC38A5AFFBA783EFF967B66FF6F5B4AC40000000800000000000000000000
          0000000000010000000200000003C89162FFFBF3EDFFFBF2ECFFFBF1EAFFFBF0
          E9FFC48B5BFF0000000800000009000000060000000200000000000000000000
          0000000000000000000000000000C89362FFFBF3EDFFFBF3EDFFFBF2ECFFFAF2
          EBFFC48C5DFF0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000C99364FFC89263FFC89162FFC79061FFC58E
          5FFFC58D5DFF0000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          00080000000E0000000F00000010000000110000001200000012000000130000
          001400000015000000150000000E00000004000000000000000000000007705D
          4DBF9A806AFF997E68FF987D67FF967B65FF947963FF927761FF90755FFF8F74
          5DFF8D725CFF8C705AFF635040C50000000D0000000000000000000000099E84
          6EFFFFFFFFFFDDD7D1FFDCD5CFFFDBD3CDFFD9D2CAFFD7D0C8FFD7CEC7FFD5CD
          C4FFD3CAC3FFD2C9C1FF8C715AFF000000130000000000000000000000079F85
          6FFFFFFFFFFFEDEAE8FFECE8E4FFEAE6E2FFE8E4E0FFE6E2DDFFE5DFDBFFE3DD
          D8FFE2DCD6FFD3CAC2FF8D725CFF00000013000000000000000000000006A087
          72FFFFFFFFFFC68E59FFB5671FFFB3641CFFB1621AFFAF5F18FFAD5D16FFAC5C
          15FFBB7F4AFFD4CBC3FF8F745DFF00000011000000000000000000000004A288
          73FFFFFFFFFFB96B22FFF2B17BFFEEAC77FFEDA976FFEAA674FFE8A26FFFE6A1
          6DFFAC5B14FFD5CBC4FF90755FFF0000000F000000000000000000000003A38A
          75FFFFFFFFFFBA6D25FFC27936FFBE7631FFBB702CFFB76A27FFA15C1EFF4124
          0CFF160C03FF1E1D1CFF382E26FF00000022000000000000000000000002A58B
          76FFFFFFFFFFBC6F27FFC68F5FFFFAF2EBFFFAF1E9FFDDD4CCFF121110FF0000
          00FF000000FF000000FF000000FF000000E80000001500000000000000017B68
          59BEA58B75FFC6874EFFC79060FFFBF2ECFFFAF1EBFF524F4DFF000000FF0000
          00FF777777FF000000FF000000FF000000FF0000009C00000000000000000000
          00010000000200000003C89162FFFBF3EDFFFBF2ECFF090908FF000000FF8282
          82FFFEFEFEFF767676FF000000FF000000FF000000E700000000000000000000
          00000000000000000000C89362FFFBF3EDFFFBF3EDFF060606FF000000FFF2F2
          F2FF777777FFF2F2F2FF595959FF000000FF000000EA00000000000000000000
          00000000000000000000C99364FFC89263FFC89162FF38291BFF000000FF0000
          00FF000000FF4B4B4BFFF2F2F2FF000000FF000000A800000000000000000000
          0000000000000000000000000000000000000000000000000021000000F00000
          00FF000000FF000000FF000000FF000000EA0000001B00000000000000000000
          0000000000000000000000000000000000000000000000000000000000210000
          00B1000000ED000000EA000000AB0000001B0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000000000000A2A73B7FF276DB2FF2667ADFF2461A9FF0000
          00120000000000000000000000000000000000000000000000007A5D46FF7A5D
          46FF7A5D46FF7A5D46FF755943FF2D7CBEFFA3E7F8FF62CDEFFF2669AEFF7055
          40FF7A5D46FF7A5D46FF7A5D46FF7A5D46FF0000000000000000B4A091FFB4A0
          91FFB4A091FFB4A091FFAE9B8CFF2F84C3FFAEEDFAFFA3E6F8FF2972B5FFA694
          86FFB4A091FFB4A091FFB4A091FFB4A091FF0000000000000000000000020000
          00080000000E0000000F000000143189C7FF2F86C4FF2E80C0FF2C7ABCFF0000
          002100000015000000150000000E00000004000000000000000000000007705D
          4DBF9A806AFF997E68FF977D67FF937963FF8F7560FF8C725DFF8A705BFF8D73
          5CFF8D725CFF8C705AFF635040C50000000D0000000000000000000000099E84
          6EFFFFFFFFFFDDD7D1FFDCD5CFFFDBD3CDFFD9D2CAFFD7D0C8FFD7CEC7FFD5CD
          C4FFD3CAC3FFD2C9C1FF8C715AFF000000130000000000000000000000079F85
          6FFFFFFFFFFFEDEAE8FFECE8E4FFEAE6E2FFE8E4E0FFE6E2DDFFE5DFDBFFE3DD
          D8FFE2DCD6FFD3CAC2FF8D725CFF00000013000000000000000000000006A087
          72FFFFFFFFFFC68E59FFB5671FFFB3641CFFB1621AFFAF5F18FFAD5D16FFAC5C
          15FFBB7F4AFFD4CBC3FF8F745DFF00000011000000000000000000000004A288
          73FFFFFFFFFFB96B22FFF2B17BFFEEAC77FFEDA976FFEAA674FFCF9163FF543B
          28FF160C03FF1E1D1CFF382D25FF00000023000000000000000000000003A38A
          75FFFFFFFFFFBA6D25FFC27936FFBE7631FFBB702CFFA15E22FF0D0702FF0000
          00FF000000FF000000FF000000FF000000E8000000150000000000000002A58B
          76FFFFFFFFFFBC6F27FFC68F5FFFFAF2EBFFFAF1E9FF524F4CFF000000FF0000
          00FF777777FF000000FF000000FF000000FF0000009C00000000000000017B68
          59BEA58B75FFC6874EFFC79060FFFBF2ECFFFAF1EBFF090908FF000000FF8282
          82FFFEFEFEFF767676FF000000FF000000FF000000E700000000000000000000
          00010000000200000003C89162FFFBF3EDFFFBF2ECFF060606FF000000FFF2F2
          F2FF777777FFF2F2F2FF595959FF000000FF000000EA00000000000000000000
          00000000000000000000C89362FFFBF3EDFFFBF3EDFF474443FF000000FF0000
          00FF000000FF4B4B4BFFF2F2F2FF000000FF000000A800000000000000000000
          00000000000000000000C99364FFC89263FFC89162FFAD7D54FF0C0806FF0000
          00FF000000FF000000FF000000FF000000EA0000001B00000000000000000000
          0000000000000000000000000000000000000000000000000000000000210000
          00B1000000ED000000EA000000AB0000001B0000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000701162B6F02234CC40006
          0E460000000A0000000100000000000000000000000000000000000000100000
          00150000001600000018000000190000001A00000027033F72D54F9FD9FF1659
          97F9000812560000000B00000001000000000000000000000000C99263FFC991
          63FFC89061FFC78F5FFFC68E5FFFC68D5EFFBE8759FF32678CFF6EB2E1FF59A9
          E3FF0C427DF5000812560000000B000000010000000000000000CA9365FFFBF6
          EFFFFBF4EEFFFAF3EDFFFAF3EBFFFAF2E9FFF6EEE5FFC6CDCEFF267AB5FF80C3
          EEFF58A9E3FF0E4178F5060A1159000000180000000D00000004CB9566FFFBF6
          F1FFFBF5F0FFFBF5EEFFFBF3EDFFFAF3EBFFF9F2EAFFF4ECE5FFB6C4CAFF2D80
          B9FF7CA9C6FF75675AFF6A5543FD5B493AE8332A219F0A080632CB9568FFFCF8
          F3FFFBF6F2FFFBF5EFFFFBF4EEFFFAF4EDFFFAF3ECFFF9F1E8FFF5ECE4FFB4C2
          C8FF346486FFC0B1A2FFC2B4A3FFC0AF9DFF9B8876FF342A219ECC9769FFFCF9
          F5FFFCF7F3FFFCF7F2FFFBF6F0FFFBF5EEFFFBF4EDFFFAF3ECFFF8F1E9FFEEE5
          DEFF7C6B5AFFCABEB0FFD8CBBDFF857261FF83705EFF574738E1CD986AFFFDFA
          F6FFFCF8F3FFFBF7F1FFFBF6F0FFFBF5EFFFFAF5EDFFFAF3ECFFF9F2EAFFF2EA
          E3FF837061FFCDC2B6FF8A7868FF49423B941A15115E594839DECD996BFFFDFA
          F7FFFCF8F3FFFBF7F1FFFBF6F0FFFBF5EFFFFAF5EDFFFAF3ECFFF9F2EAFFE2D9
          D0FFAB9D92FFAC9D90FF8D7D6CFF1C17136C0000000E0D0B082CCE9A6DFFFDFB
          F8FFFCF8F3FFFAF6F2FFFAF7F1FFFBF6F0FFF9F4EEFFF9F4EDFFFAF3EBFFFAF3
          EBFFD7CEC4FFAB9E92FF7D644EFF5F4E40DE0E0C0A2B00000003CF9B6DFFFDFC
          FAFFFDFDFBFFFDFCFAFFFDFBF9FFFDFAF8FFFDFBF6FFFCF9F5FFFBF8F4FFFCF7
          F3FFFCF6F1FFF6F0E9FFC18C5DFF000000200000000300000000CF9B6EFFFEFD
          FBFFFDFDFBFFFDFCFAFFFDFBF9FFFDFAF8FFFDFBF6FFFCF9F5FFFBF8F4FFFCF7
          F3FFFCF6F1FFFAF5EEFFC89162FF000000180000000000000000CF9C6FFFFEFD
          FDFFFDFDFBFFFDFCFAFFFDFBF9FFFDFAF8FFFDFBF6FFFCF9F5FFFBF8F4FFFCF7
          F3FFFCF6F1FFFBF6EFFFC99263FF000000170000000000000000D09D70FFFEFE
          FDFFFDFDFBFFFDFBFBFFFCFCFAFFFDFBF9FFFCFBF7FFFBFAF6FFFCF9F5FFFCF8
          F4FFFBF7F2FFFBF7F1FFCA9364FF000000160000000000000000D09D70FFFEFE
          FDFFFEFEFDFFFEFDFCFFFEFDFBFFFDFDFBFFFDFCFAFFFDFBF8FFFDFAF7FFFDFA
          F5FFFCF9F4FFFCF7F3FFCA9466FF000000140000000000000000D09D70FFD09D
          70FFD09D70FFD09C6FFFD09C6FFFCF9B6EFFCE9B6EFFCE9A6DFFCD996CFFCD98
          6AFFCD9769FFCC9668FFCB9567FF0000000F0000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000003000000090000000F0000001400000015000000120000000E0000
          0006000000020000000000000000000000000000000000000000000000010000
          0008030D13310C385197125479D713608EF6136191FF0F4C74DC0A3552AE0312
          1D54000000120000000500000001000000000000000000000000000000090B35
          497F2B82AEFF3797C1FF41A9CFFF43B1D7FF3FB1D8FF32A0CAFF268CB8FF176B
          9AFF0A3756B901080C360000000B0000000200000000000000000D3C508178B4
          CFFF70CCE9FF60C9E8FF5BC8E8FF54C5E6FF4CBFE1FF43B9DDFF3AB1D9FF33AC
          D5FF238CB9FF125B89F8051A286F0000000E00000002000000001C7CA5ECE0F1
          F8FFE9F8FCFFC7EDF8FF62CBEAFFA88858FFAE7C43FF4DC1E4FF46BEE2FF3DB6
          DDFF34ADD5FF2DA5D0FF176B9AFC041724660000000A00000001197094D32B8E
          B7FF5EA8C8FFE9F8FCFFC7EDF8FFAB8A5AFFB17F46FF55C4E4FF2DA38FFF198D
          5BFF34AEBCFF35AED7FF2BA1CCFF10537FED0106092D00000003000000050208
          0B18173643675EA7C8FFE8F8FCFF69CFECFF62CBEAFF5BC8E8FF1D9562FF0FA0
          63FF2EA49BFF3FB9DFFF34ADD5FF1E7EABFF07263D8F0000000B000000000000
          0002020A0D1E2E8FB9FFB4E8F7FF6FD2EEFF69CFEDFF62CCEBFF4FBECEFF3CAF
          ADFF4ABFDDFF46BEE2FF3CB4DBFF2D9CC7FF0E486FDA00000012000000000000
          000310465C8688C2DAFF8DDDF4FF75D5EFFF6FD3EEFF69CFECFF62CCEAFF5BC8
          E8FF54C5E6FF4DC1E4FF45BBE0FF37AAD3FF125886F400000015000000000000
          00031D7CA1DBD3EBF4FF84DCF3FF7BD8F2FF75D5EFFF6FD2EEFF5588D4FF4141
          BDFF4B79CFFF54C4E6FF4CC0E3FF3EB0D6FF135E8FFB00000015000000000000
          0003228FBAF7ECF8FCFF8CE0F5FF7FDAF3FF7BD7F1FF75D5F0FF4750C2FF8382
          EFFF4244BEFF5CC6E8FF54C5E6FF3DA7CEFF105279DA00000010000000000000
          0002218BB4EBE2F4F9FFC6EFFAFF84DCF4FF80DBF3FF7BD8F1FF5E8ED8FF4446
          BEFF5484D3FF62CBEAFF59C6E6FF499BBFFF0B31488D00000009000000000000
          00011041546F7CC2DDFFF3FCFEFFE1F7FDFFAAE7F7FF8BDDF3FF7CD7F1FF8FDD
          F3FF8BDBF1FFACE4F4FF9DCADEFF1B6389DB0104061A00000002000000000000
          00000000000111485D7B46A8CDFFACD9E9FFDDF1F7FFF1FBFDFFC8E5F0FFC5E2
          EEFF89BFD7FF358DB6FF125371B702080C220000000400000000000000000000
          00000000000000000001061A212D176581AB1F86ACE52290BCFC1A7194D0196E
          91CE10445B8B030F152B00000008000000020000000000000000000000000000
          0000000000000000000000000000000000010000000300000004000000050000
          0005000000040000000200000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          000B0000001A00000021000000230000002500000027000000290000002B0000
          002C0000002E0000002F0000003100000029000000120000000300000008034D
          7D620471B8FF056DB6FF056CB4FF0469B1FF0468B0FF0566ACFF0563AAFF0562
          A8FF0461A7FF045EA4FF055CA3FF045BA2FF0231557D000000100000000F056C
          ADCE8FD0EBFF37D2F5FF01C2F0FF01BFEEFF01BAECFF01B6ECFF5DCEF1FF5DCB
          F0FF01A9E5FF01A6E3FF01A3E3FF0290D3FF04508CE00000001F0000000D046F
          AFC88AD1EDFF9BEFFEFF03D3FBFF00CFF9FF00CAF8FF003845FF1D3D46FF66D7
          F6FF00B8F0FF00B3EDFF00ACEAFF0491D2FF034E8AD90000001D00000005034F
          7C4B2791CCFDBDF4FEFF47E1FDFF00D3FBFF00CEF9FF00303BFF08313BFF20C9
          F5FF00BCF1FF00B7F0FF00B0EBFF1172B5FE022B4B6A0000000C000000010000
          000D036FAEB57BC9E8FFABF2FFFF04D6FDFF00D2FBFF02A1C5FF2AA7C6FF36D1
          F7FF00C1F4FF00BCF2FF0495D3FF03528BC90000001E00000002000000000000
          000302466D31188AC8FBBBF3FEFF5BE6FEFF01D5FCFF02809DFF2D87A0FF4BD9
          FAFF01C5F6FF00BBF0FF0E75B7FC0228434F0000000800000000000000000000
          0000000000090370AC9B67BEE4FFB2F4FFFF0DDBFEFF026277FF2A6677FF60E1
          FBFF01CAF8FF0A9AD5FF03558CB1000000160000000100000000000000000000
          000000000002024265241288C8F2B2EEFBFF68EAFFFF013C48FF1F404BFF76E7
          FDFF01C3F2FF0B77BAF501243B38000000060000000000000000000000000000
          000000000000000000070372AE875AB7E1FFBBF5FFFF02151AFF09181CFF64E5
          FDFF0D9FD7FF035C92970000000F000000010000000000000000000000000000
          0000000000000000000101293D140885C6E8A9E9F9FF1C3539FF00333BFF01C8
          F3FF0779BCEA0116222100000003000000000000000000000000000000000000
          00000000000000000000000000040374AE6A46AEDDFFBDF6FFFF25DEFEFF109E
          D7FF036194760000000900000000000000000000000000000000000000000000
          00000000000000000000000000010120300D0687C8D69CE1F5FF9CDFF3FF0780
          C0D901121B160000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000030377B051038ACEFF0389CCFF036A
          9F58000000060000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003000000080000000A0000
          0005000000010000000000000000000000000000000000000000}
      end>
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 504
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

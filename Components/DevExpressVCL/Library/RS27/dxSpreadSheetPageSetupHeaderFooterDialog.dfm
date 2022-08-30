object dxSpreadSheetPageSetupHeaderFooterDialogForm: TdxSpreadSheetPageSetupHeaderFooterDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'dxSpreadSheetPageSetupHeaderFooterDialogForm'
  ClientHeight = 399
  ClientWidth = 625
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 625
    Height = 399
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object btnOK: TcxButton
      Left = 459
      Top = 364
      Width = 75
      Height = 25
      Caption = 'btnOK'
      Default = True
      ModalResult = 1
      TabOrder = 11
    end
    object btnCancel: TcxButton
      Left = 540
      Top = 364
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 12
    end
    object meFooterRight: TcxMemo
      Left = 414
      Top = 258
      RepositoryItem = ermiSection
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Height = 89
      Width = 190
    end
    object meFooterCenter: TcxMemo
      Left = 217
      Top = 258
      RepositoryItem = ermiSection
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Height = 89
      Width = 191
    end
    object meFooterLeft: TcxMemo
      Left = 21
      Top = 258
      RepositoryItem = ermiSection
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Height = 89
      Width = 190
    end
    object meHeaderRight: TcxMemo
      Left = 414
      Top = 82
      RepositoryItem = ermiSection
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Height = 89
      Width = 190
    end
    object meHeaderCenter: TcxMemo
      Left = 217
      Top = 82
      RepositoryItem = ermiSection
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Height = 89
      Width = 191
    end
    object meHeaderLeft: TcxMemo
      Left = 21
      Top = 82
      RepositoryItem = ermiSection
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 89
      Width = 190
    end
    object btnInsertSheetName: TcxButton
      Tag = 5
      Left = 378
      Top = 193
      Width = 25
      Height = 25
      OptionsImage.ImageIndex = 4
      OptionsImage.Images = ilImages
      OptionsImage.Spacing = 0
      SpeedButtonOptions.CanBeFocused = False
      TabOrder = 7
      OnClick = btnInsertSheetNameClick
    end
    object btnInsertTime: TcxButton
      Tag = 4
      Left = 331
      Top = 193
      Width = 25
      Height = 25
      OptionsImage.ImageIndex = 3
      OptionsImage.Images = ilImages
      OptionsImage.Spacing = 0
      SpeedButtonOptions.CanBeFocused = False
      TabOrder = 6
      OnClick = btnInsertSheetNameClick
    end
    object btnInsertDate: TcxButton
      Tag = 3
      Left = 300
      Top = 193
      Width = 25
      Height = 25
      OptionsImage.ImageIndex = 2
      OptionsImage.Images = ilImages
      OptionsImage.Spacing = 0
      SpeedButtonOptions.CanBeFocused = False
      TabOrder = 5
      OnClick = btnInsertSheetNameClick
    end
    object btnInsertPageTotal: TcxButton
      Tag = 2
      Left = 253
      Top = 193
      Width = 25
      Height = 25
      OptionsImage.ImageIndex = 1
      OptionsImage.Images = ilImages
      OptionsImage.Spacing = 0
      SpeedButtonOptions.CanBeFocused = False
      TabOrder = 4
      OnClick = btnInsertSheetNameClick
    end
    object btnInsertPageNumber: TcxButton
      Tag = 1
      Left = 222
      Top = 193
      Width = 25
      Height = 25
      OptionsImage.ImageIndex = 0
      OptionsImage.Images = ilImages
      OptionsImage.Spacing = 0
      SpeedButtonOptions.CanBeFocused = False
      TabOrder = 3
      OnClick = btnInsertSheetNameClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liFooterRight: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = '1'
      CaptionOptions.Layout = clTop
      Control = meFooterRight
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liFooterCenter: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.Text = '1'
      CaptionOptions.Layout = clTop
      Control = meFooterCenter
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liFooterLeft: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = '1'
      CaptionOptions.Layout = clTop
      Control = meFooterLeft
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liHeaderRight: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.AlignHorz = taRightJustify
      CaptionOptions.Text = '1'
      CaptionOptions.Layout = clTop
      Control = meHeaderRight
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liHeaderCenter: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.Text = '1'
      CaptionOptions.Layout = clTop
      Control = meHeaderCenter
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liHeaderLeft: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = '1'
      CaptionOptions.Layout = clTop
      Control = meHeaderLeft
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnInsertSheetName
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnInsertTime
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnInsertDate
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnInsertPageTotal
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnInsertPageNumber
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
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
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lgCommonHeaderFooter: TdxLayoutGroup
      Parent = lgTabs
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lgCommonHeaderFooter
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = lgCommonHeaderFooter
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = lgCommonHeaderFooter
      AlignHorz = ahCenter
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 2
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 5
    end
    object dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem
      Parent = lgCommonHeaderFooter
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 2
    end
    object dxLayoutEmptySpaceItem4: TdxLayoutEmptySpaceItem
      Parent = lgCommonHeaderFooter
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 4
    end
    object lliDescription: TdxLayoutLabeledItem
      Parent = lgCommonHeaderFooter
      CaptionOptions.Text = 'Label'
      CaptionOptions.WordWrap = True
      Index = 0
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 24
    Top = 64
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object EditRepository: TcxEditRepository
    Left = 56
    Top = 64
    PixelsPerInch = 96
    object ermiSection: TcxEditRepositoryMemoItem
      Properties.ScrollBars = ssVertical
    end
  end
  object ilImages: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 12583096
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00030000000D000000140000001700000018000000190000001A0000001B0000
          001C0000001D0000001E0000001D000000140000000500000000000000000000
          000B946D4CC4CC9668FFCC9467FFCA9364FFC99163FFC98F61FFC78E5FFFC78C
          5EFFC68C5CFFC48A5AFFC4895AFF8C6240CC0000001300000000000000000000
          000FCF9A6DFFFFFFFFFFF5EADEFFF4E9DDFFF4E8DBFFF4E7D9FFF3E6D8FFF3E6
          D7FFF2E4D5FFF2E4D4FFF1E2D2FFC4895AFF0000001C00000000000000000000
          000FD19C70FFFFFFFFFFF9F1E7FFF8F0E6FFF8EFE5FFF8EFE4FFF8EEE3FFF7EE
          E1FFF7EDE1FFF6ECE0FFF3E4D4FFC58A5BFF0000001C00000000000000000000
          000DD19E72FFFFFFFFFFF9F2EAFFF8F1E8FFF9F1E6FFF8F0E5FFF8EFE4FFF7EE
          E3FFF7EEE2FFF7EDE1FFF2E4D5FFC68C5DFF0000001A00000000000000000000
          000CD3A175FFFFFFFFFFF5E9DEFFAF6331FFF4E7DBFFF3E7DAFFAB5E2AFFF2E5
          D7FFF8EFE3FFF7EEE3FFF3E6D7FFC78E5FFF0000001900000000000000000000
          000AD4A277FFFFFFFFFFFAF4EDFFC58E68FFDFC0A8FFF9F2E9FFC18A62FFDCBC
          A3FFF8F0E5FFF8EEE3FFF4E7D9FFC89061FF0000001700000000000000000000
          0009D6A47AFFFFFFFFFFAE622CFFAD6029FFAA5C26FFAA5924FFA85722FFA656
          1FFFA5531DFFF8F0E5FFF4E8DBFFCA9263FF0000001500000000000000000000
          0008D7A77CFFFFFFFFFFFBF6F0FFF0E2D5FFB26A36FFFAF4ECFFEEDFD0FFAE62
          2FFFF8F2E9FFF8F0E7FFF5EADDFFCB9466FF0000001400000000000000000000
          0006D8A87EFFFFFFFFFFFBF7F2FFFBF6F0FFBE8053FFEBD7C7FFFAF4EDFFBA78
          4CFFE9D4C2FFF9F1E9FFF6EBDFFFCC9669FF0000001200000000000000000000
          0005DAAB80FFFFFFFFFFFCF8F3FFB36732FFB0652FFFAF622CFFAD6029FFAA5C
          27FFA95A24FFA95721FFF6ECE1FFCE986BFF0000001000000000000000000000
          0004DBAC82FFFFFFFFFFFCF8F4FFFCF9F4FFECDBCCFFBE8156FFFBF6F0FFEBD7
          C7FFBB7A4EFFFAF4ECFFF6EEE4FFD09B6EFF0000000F00000000000000000000
          0003DCAD84FFFFFFFFFFFDF9F6FFFCF9F5FFFCF8F4FFB7713EFFF6EDE4FFFBF6
          F0FFB36A36FFFAF5EEFFF7EFE6FFD19C71FF0000000D00000000000000000000
          0002DDAF85FFFFFFFFFFFDFAF7FFFCFAF7FFFCF9F6FFFCF9F4FFFCF8F3FFFCF7
          F3FFFBF6F1FFFBF5F0FFF8F0E9FFD29E73FF0000000C00000000000000000000
          0001DEB087FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFD4A176FF0000000A00000000000000000000
          0000A58365BEDEB187FFDDAF86FFDCAE85FFDBAD83FFDBAC82FFDAAB81FFD9A9
          7FFFD8A87DFFD7A67CFFD6A57AFF9D7959C20000000600000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000030000000C00000013000000150000001600000017000000180000
          00190000001A0000001B0000001B000000120000000400000000000000000000
          00000000000A946E4EC3CE986BFFCD9769FFCB9567FFCB9366FFC99263FFC890
          62FFC88E60FFC68D5EFFC68C5CFF8D6341CA0000001200000000000000030000
          000C00000020D19C70FFFFFFFFFFF6EBDFFFF5EADEFFF5E9DDFFF5E9DBFFF5E7
          DAFFF3E7D7FFF3E6D7FFF2E4D5FFC68C5DFF00000019000000000000000A946E
          4EC3C39065FFD19E72FFFFFFFFFFF9F2EAFF29921FFFF2EEDFFF238E18FFE2E5
          CFFFF7EEE3FFF7EEE2FFF3E6D7FFC78D5FFF00000019000000000000000ED19C
          70FFF3F3F3FFD3A175FFFFFFFFFFFAF3EBFF2C9523FFD6E1C6FF25901BFFC6D9
          B6FFF8F0E4FFF8EFE3FFF3E7D9FFC88F61FF00000018000000000000000DD19E
          72FFF5F5F5FFD4A277FFFFFFFFFF33992CFF309628FF2C9424FF28921FFF2590
          1BFF228E18FFDBE3C9FFF5E7DAFFC99264FF00000016000000000000000CD3A1
          75FFF6F6F6FFD6A47AFFFFFFFFFFFBF5EEFFE6ECD9FF309727FFF2F0E4FF2891
          20FFF8F1E8FFF9F1E7FFF5EADCFFCA9465FF00000014000000000000000AD4A2
          77FFF7F7F7FFD7A77CFFFFFFFFFFFBF6F0FFFAF6EFFF34992CFFF3F1E5FF2C94
          23FFE5E9D5FFF8F2E9FFF5EADFFFCC9668FF000000130000000000000009D6A4
          7AFFF9F9F9FFD8A87EFFFFFFFFFFE2EBD9FF3A9D34FF379B30FF33982CFF3097
          28FF2B9523FF28921FFFF6EBE1FFCE986BFF000000110000000000000008D7A7
          7CFFFAFAFAFFDAAB80FFFFFFFFFFFCF8F3FFFBF7F2FFCEE2C4FF379B30FFD8E5
          CDFF2F9627FFFAF3ECFFF6EEE3FFCF9A6DFF0000000F0000000000000006D8A8
          7EFFFBFBFBFFDBAC82FFFFFFFFFFFCF8F4FFFCF9F4FFE8EFE0FF3A9E33FFF4F3
          E9FF33992CFFFBF4EEFFF7EFE5FFD19D70FF0000000E0000000000000005DAAB
          80FFFCFCFCFFDCAD84FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFD29F73FF0000000C0000000000000004DBAC
          82FFFEFEFEFFE3C0A0FFDCAE84FFDBAD83FFDBAB82FFDAAA80FFD8A97EFFD7A7
          7DFFD7A57BFFD5A478FFD5A277FF9C7756C3000000070000000000000003DCAD
          84FFFFFFFFFFFEFEFEFFFDFDFDFFFCFCFCFFFCFCFCFFFBFBFBFFFAFAFAFFFAFA
          FAFFF9F9F9FFCC9B70FF0000001300000005000000020000000000000001A382
          62BFDCAE84FFDBAD83FFDBAB82FFDAAA80FFD8A97EFFD7A77DFFD7A57BFFD5A4
          78FFD5A277FF9C7756C300000007000000000000000000000000000000000000
          0001000000020000000300000003000000040000000500000005000000060000
          0007000000070000000500000002000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000020000
          000A000000100000001100000011000000110000001200000012000000120000
          0012000000120000001300000013000000120000000C000000030000000A8159
          4CC2B47C69FFB37B69FFB37B68FFB37A68FFB37A68FFB27A68FFB37968FFB279
          68FFB27967FFB27867FFB17867FFB17866FF7F5649C30000000B0000000EB77F
          6EFFFBF7F4FFF7EEE8FFF7EDE7FFF7EDE7FFF7EDE7FFF6EDE7FFF6EDE6FFF7ED
          E5FFF6ECE5FFF6EDE5FFF6ECE6FFF6ECE5FFB47B69FF000000110000000EB984
          72FFFBF8F5FFF8EFE9FFF8EEE9FFF7EEE8FFF7EEE8FFF7EEE8FFF7EEE7FFF7EE
          E7FFF7EEE7FFF7EDE7FFF6ECE6FFF7EDE7FFB77F6EFF000000110000000EBC89
          78FFFCF9F7FFF8F1ECFF5D6CDDFF5E6BDDFFF8EFEAFFD2B2A3FFD2B1A3FFF7EF
          E9FFD0B1A1FFD0B0A1FFF7EEE8FFF8EEE9FFB98472FF000000100000000DC08E
          7DFFFCFAF8FFF9F1EEFF5E6CDDFF5E6BDDFFF9F1EBFFD3B3A4FFD2B2A4FFF8EF
          EAFFD2B2A3FFD2B2A3FFF8EFEAFFF8F1EBFFBC8977FF000000100000000CC394
          82FFFCFBF9FFFAF3EFFFFAF2EFFFF9F2EEFFF9F2EEFFF8F1EDFFF9F1EDFFF9F1
          ECFFF8F0ECFFF8F0EBFFF9F2EDFFF9F2EEFFC08E7CFF0000000F0000000BC798
          87FFFDFCFAFFFAF4F2FFD5B8ABFFD5B8AAFFF9F3F0FFD5B6A9FFD4B6A9FFF9F2
          EFFFD4B5A7FFD3B4A6FFFAF3F0FFFAF4F0FFC49381FF0000000E0000000BC99D
          8CFFFDFCFBFFFBF5F3FFD6BAACFFD6B9ACFFFBF4F1FFD6B9ABFFD5B8AAFFFAF4
          F1FFD5B6A9FFD4B5A7FFFAF5F2FFFAF6F2FFC69886FF0000000D0000000ACDA1
          90FFFEFDFCFFFCF6F5FFFCF6F4FFFBF6F4FFFBF6F3FFFBF5F3FFFBF5F2FFFAF5
          F2FFFBF4F1FFFAF4F1FFFBF6F5FFFBF7F5FFC99D8BFF0000000D00000009CFA5
          94FFFEFDFDFFFCF7F6FFFCF8F6FFFBF7F6FFFCF7F5FFFBF6F4FFFBF6F4FFFBF6
          F4FFFBF6F4FFFBF5F4FFFCF9F7FFFCF9F7FFCCA290FF0000000C000000084B53
          C3FF8D9EECFF687CE3FF6678E2FF6476E1FF6172E0FF5F70DFFF5D6CDEFF5B69
          DCFF5966DBFF5664DAFF5462D9FF616DDCFF3337AAFF0000000B000000084C55
          C4FF93A4EEFF6C80E6FF6A7EE4FF687BE4FF6678E2FF6375E1FF6172E0FF5E6F
          DEFF5C6CDDFF5A69DCFF5766DAFF6472DDFF3538ABFF0000000A000000074D56
          C6FF96A7EFFF95A6EFFF93A4EDFF90A2EDFF8F9FEDFF8B9BEBFF8898EAFF8595
          EAFF8291E7FF7F8DE7FF7D89E5FF7987E5FF3539ACFF00000009000000043A40
          93C14D55C5FF4B53C3FF4A51C1FF484FBFFF464DBEFF444BBBFF4249B9FF4046
          B7FF3E44B4FF3C41B3FF3A3EB0FF393CAEFF282B80C200000006000000010000
          0004000000060000000600000006000000070000000700000007000000070000
          0007000000070000000800000008000000070000000500000001}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000001000000050000000A0000000D0000000E0000000D0000
          000A000000050000000100000000000000000000000000000000000000000000
          0000000000030000000A0B0C3065191A67BF1F2285EE222492FF1F2085EE1719
          66BF0B0B30650000000B00000003000000010000000000000000000000000000
          000403040D241A1C67B83439A5FF515CC7FF606DD6FF6574DEFF5E6CD6FF4E5A
          C6FF3034A2FF181A65BA03030C25000000040000000100000000000000030606
          1632292B8FEA525BC1FF6574DBFF535DCBFF484FC0FF444CBEFF4750C1FF535E
          CBFF6371DAFF4851BDFF23278BEB04040E2700000003000000000101020D2629
          84D5636ECBFF6472D7FF464DBEFF999CD9FFDBDCF1FFFEFDFDFFDBDBF1FF999C
          DAFF484EC0FF626ED7FF515CC4FF1C1E69B80000000A0000000112143D694A52
          B8FF7887E0FF4A50BEFFD2D3EBFFFCFBFAFFFDFBF9FFFCFBFAFF73584EFFE2E1
          E0FFD5D5ECFF4B52C0FF6B7ADDFF3E46AFFF0F10366500000004262A79BB7681
          D5FF5964CBFFA1A2D8FFFBF8F5FFFCF8F6FFFBF8F6FFFBF8F6FF755950FFE1DF
          DDFFFBF8F6FFA2A3D9FF5761CBFF606CCEFF222574BD00000008363AA2EC92A0
          E7FF454CBCFFE1DEEAFFFAF5F2FFF9F5F2FFFAF5F2FFFAF5F2FF775C51FFE0DC
          D9FFFAF5F2FFE2DFEAFF464DBEFF7485DFFF2E3398EA000000093E45B4FFA1AF
          EEFF3F43B7FFF4EEEDFFF8F2EDFFE0DCD8FFE3E2E1FFE5E5E5FF785D53FFE0DC
          D8FFF7F2EDFFF4EEECFF4146B8FF8193E7FF363BA9FA000000093B42AAECA1AE
          EBFF454BBAFFDED8E2FF4945B2FF433CACFF3B33A5FF352B9EFF5B4770FFF8F7
          F6FFF6EFE9FFDED9E2FF464BBCFF8595E5FF343BA2EA00000008313685B9919C
          E1FF6770CCFF9492CBFFF5ECE6FFFEFDFCFFFFFFFFFFFFFFFFFFFFFFFFFFFEFD
          FCFFF4ECE5FF9593CDFF5D68CCFF7D8ADCFF2C3280BA000000061A1C44607982
          D6FFA5B3EBFF4246B7FFC5C0D7FFF5EEE7FFFBF8F5FFFEFDFCFFFBF8F6FFF5EE
          E7FFC6C0D7FF4247B7FF8E9FE7FF6B73CFFF17193F6000000003010102085A61
          AACF9FAAE8FF96A1E3FF4044B5FF8C89C6FFCDC6D7FFF0E6DFFFCDC6D7FF8C8A
          C7FF3F44B6FF8593DFFF949EE3FF464A8CB40000000600000001000000011112
          1E296F75C4E99AA5E6FFADBAEDFF6A74CCFF444AB8FF383CB0FF454AB8FF636C
          CBFFA1B0EBFF959EE3FF6269BDE90A0B141F0000000100000000000000000000
          00010A0B121A535894B18690DEFFACB7EDFFBDC9F5FFC5D2F8FFBDC8F5FFAAB6
          ECFF828BDCFF4E5491B2090A111B000000020000000000000000000000000000
          000000000001000000032527404F494D7F9A6168ABCC7780D4F96066ABCC474C
          7F9B23263F500000000400000001000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000090000
          000E0000000F0000000F0000000F0000000F0000001000000010000000100000
          0010000000100000001000000010000000100000000F0000000A815A4DC1B47D
          6BFFB37C6CFFB37C6BFFB37C6BFFB37C6AFFB37B6AFFB37B6AFFB27B69FFB37B
          69FFB37A69FFB37A69FFB27968FFB27968FFB27968FF80574BC1B57F6FFFF8F2
          EFFFF8F2EEFFF8F2EEFFF8F2EEFFCFA89CFFF7F2EEFFF8F2EEFFF7F2EEFFF8F2
          EEFFCFA89CFFF9F3EFFFF7F2EEFFF7F2EDFFF7F2EDFFB37B69FFB68272FFF9F3
          EFFFF8F0ECFFF7F0ECFFF8F0ECFFC69A8DFFF8F1ECFFF7F0ECFFF7F0EBFFF7F0
          EBFFC69A8CFFF9F1EFFFF7F0EBFFF7EFEBFFF8F2EEFFB47D6CFFB88575FFF9F3
          F1FFF8F1EEFFF8F1EEFFF8F1EDFFC79C8EFFF8F1EEFFF8F1EDFFF8F1EDFFF8F1
          EDFFC69C8EFFF9F3F0FFF8F2EFFFF9F3F0FFFAF5F1FFB57F6EFFB98879FFD1AC
          9FFFC89E90FFC89D90FFC89D8FFFC89D8FFFC89D8FFFC79D8FFFC89D8FFFC79D
          8FFFC89D8FFFC89D8EFFC79C8FFFC89D8EFFCFAA9DFFB68272FFBB8C7DFFFAF7
          F3FFF9F3F0FFF9F3F1FFFAF4F0FFC99E91FFF9F3F0FFF8F3EFFFF9F2F0FFF9F3
          F0FFC89E90FFF8F3EFFFF9F3EFFFF8F3EFFFF9F5F2FFB88676FFBC8F81FFFCF8
          F5FFFAF4F2FFFAF4F1FFFAF4F1FFC9A092FFF9F4F2FFFAF4F1FFF9F4F1FFFAF4
          F1FFC9A092FFF9F4F1FFF9F4F1FFF9F4F0FFFAF5F2FFBA8979FFBE9385FFFCF8
          F7FFFBF7F5FFFCF7F5FFFBF7F5FFCBA293FFFAF5F3FFFBF5F3FFFAF5F3FFFAF5
          F2FFCAA193FFFAF5F2FFFAF5F2FFFAF4F2FFFBF7F4FFBB8C7DFFBF9689FFD6B4
          A9FFCFA89AFFCEA99AFFCEA89AFFCCA395FFCCA395FFCCA395FFCBA395FFCBA2
          94FFCBA294FFCBA295FFCBA294FFCBA294FFD3AFA3FFBC9081FFC1998CFFFCF9
          F7FFFCF7F5FFFCF7F6FFFBF8F5FFCCA497FFFBF7F6FFFCF7F5FFFBF7F5FFFCF7
          F6FFCCA495FFFCF7F5FFFCF7F4FFFBF6F4FFFCF8F7FFBE9285FFC29B8FFFFCF9
          F8FFFCF8F6FFFDF9F7FFFCF8F7FFCDA698FFFCF8F6FFFCF8F6FFFCF8F7FFFCF8
          F6FFCDA597FFFCF8F7FFFBF8F6FFFCF8F6FFFCF9F7FFBF9688FFC39E91FFFCFA
          F9FFFCFAF9FFFCFAF8FFFCFAF9FFD5B4A6FFFCFAF9FFFDFAF9FFFDFAF9FFFCFA
          F8FFD5B3A6FFFCF9F8FFFCF9F8FFFCFAF8FFFCFAF8FFC1998BFF91776EC0C4A0
          94FFC4A094FFC49F93FFC39F93FFC49F92FFC49F92FFC39E91FFC39E91FFC39E
          91FFC39D91FFC39D90FFC29C90FFC29C90FFC29C90FF907469C0000000020000
          0003000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000004000000040000000400000003}
      end>
  end
end

inherited dxRichEditSymbolDialogForm: TdxRichEditSymbolDialogForm
  AutoSize = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Symbol'
  ClientHeight = 316
  ClientWidth = 535
  Constraints.MinHeight = 261
  Constraints.MinWidth = 527
  Font.Name = 'Arial'
  Icon.Data = {
    0000010001001010000000002000680400001600000028000000100000002000
    000001002000000000004004000000000000000000000000000000000000D3BC
    AAFFD39960FFD29253FFD09051FFCF8B4FFFCE894DFFCA854AFFCA8747FFC884
    45FFC78044FFC57F43FFC47B40FFC47A3EFFC0773DFFC17D48FFCCB2A2FFD49E
    67FFF0D8C0FFE5BB8EFFE4B98AFFE3B687FFE2B484FFE1B281FFE0B07DFFDFAE
    7AFFDFAB76FFDEAA74FFDDA871FFDCA66FFFDCA56CFFD89F65FFC07D4AFFD89A
    5FFFF6E7D5FFEAC69EFFE9C59AFFE9C297FFE9C194FFE8C091FFE7BE8EFFE6BD
    8BFFE6BB89FFE5BA86FFE6B883FFE5B781FFE3B37CFFDCA56CFFC2773DFFD99B
    60FFF7E9DAFFEAC8A1FFEAC69EFFE9C59AFFE9C398FFE9C194FFE7C091FFE8BF
    8EFFE7BD8BFFE6BB88FFE6BA86FFE6B984FFE5B781FFDDA76FFFC47A3EFFDA9F
    62FFF8ECDFFFEBC9A4FFEBC8A1FFEAC79EFFE9C59BFFF0D4B6FFF9F0E4FFF9EF
    E3FFF2DBC2FFE8C092FFF6E7D6FFF0D6B6FFE5B884FFDDA971FFC47B40FFDBA0
    64FFF9EEE2FFECCCA7FFECCAA4FFEBC8A1FFB58B60FF885120FF8C5421FFA471
    41FFEDD7C1FFA16B3AFFB68759FFEFD3B2FFE6BA86FFDEAA73FFC67F42FFDCA3
    67FFFAF0E6FFECCDABFFECCBA7FFD5B189FF855224FFDAC2AAFFEAC59AFF9965
    34FF9E6B3BFFD4B9A0FFB58353FFE7BC8BFFE6BB89FFDFAC77FFC78044FFDCA5
    6AFFFAF2E9FFEECFAEFFEDCDABFFAE855CFF92673CFFF6E6D5FFEBC69EFFCCA3
    77FF8C5521FFF0E1D1FFEECFADFFE7BE8EFFE7BD8CFFE0AE7AFFC88445FFDEA7
    6BFFFBF4ECFFEED1B1FFEDCFAEFFAE855DFF8C5E32FFF9EFE4FFEED0AFFFEAC6
    9DFF8E5A2AFFC9A887FFF4E0C9FFE8BF91FFE8BE8FFFE0B07DFFC98747FFDFA8
    6DFFFBF5EFFFEED3B5FFEED1B1FFE0C19EFF7A4A1CFFD1B393FFF9EFE4FF9D75
    4EFFA67B53FF9B6B3BFFF8EDE0FFE8C194FFE8C091FFE1B280FFCA884AFFE1AA
    6FFFFCF6F1FFEFD4B8FFEED2B5FFEED0B1FFB6906AFF7A4A1BFF794718FFA77D
    51FFE0BB93FF875221FFEAC59AFFE9C398FFE9C195FFE2B584FFCE894DFFE2AC
    73FFFCF8F2FFF0D6BBFFEFD4B7FFEED3B7FFEED0B1FFEECFAEFFECCDAAFFECCC
    A7FFEBCAA4FFEBC8A1FFEBC69EFFEAC49BFFE9C397FFE3B687FFCF8B4FFFE2AE
    74FFFCF8F4FFF1D7BEFFF0D6BBFFEFD4B8FFEED2B5FFEED1B2FFEDCFAEFFEDCD
    AAFFECCBA7FFECCAA4FFEBC8A1FFEAC69EFFEAC59BFFE4B98CFFD09051FFE3AF
    76FFFDF9F6FFF3E0CBFFF0D8BEFFEFD5BAFFEFD5B8FFEFD2B5FFEED1B1FFEDCF
    AEFFEDCDABFFECCCA8FFECCAA4FFEAC8A1FFEAC69EFFE4BB8FFFD29253FFE2B4
    7EFFF7EADBFFFDF9F6FFFDF8F4FFFCF7F3FFFCF6F0FFFBF5EFFFFBF4ECFFFAF2
    E9FFF9F0E6FFF9EEE2FFF8ECDEFFF7E9DAFFF6E7D5FFEDD0B3FFD49861FFDFCE
    BFFFE1B98BFFE3AF74FFE3AE73FFE2AD70FFDFAA6FFFDFA76EFFDEA76BFFDDA5
    6AFFDCA366FFDBA066FFDA9F62FFD99E60FFD89A5FFFD6A675FFDBC7B7FF0000
    FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
    FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF}
  PixelsPerInch = 96
  TextHeight = 14
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 535
    Height = 316
    Align = alClient
    AutoSize = False
    object cmbFont: TdxRichEditFontNameComboBox [0]
      Left = 93
      Top = 10
      Properties.FontPreview.Visible = False
      Properties.OnChange = cmbFontPropertiesChange
      Style.HotTrack = False
      TabOrder = 0
      Width = 165
    end
    object lbSymbolList: TdxSymbolListBox [1]
      Left = 10
      Top = 38
      Width = 515
      Height = 237
      FontName = 'Tahoma'
      ItemHeight = 33
    end
    object btnOk: TcxButton [2]
      Left = 369
      Top = 281
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton [3]
      Left = 450
      Top = 281
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object edtCharacterCode: TcxTextEdit [4]
      Left = 93
      Top = 281
      Style.HotTrack = False
      TabOrder = 2
      OnKeyPress = edtCharacterCodeKeyPress
      Width = 52
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      Index = -1
    end
    object lciFont: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = '&Font:'
      CaptionOptions.Width = 68
      Control = cmbFont
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = lbSymbolList
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lciCharacterCode: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = '&Character code:'
      Control = edtCharacterCode
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
end

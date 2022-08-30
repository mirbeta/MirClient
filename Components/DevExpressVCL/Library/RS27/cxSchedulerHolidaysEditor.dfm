object fmHolidaysEditor: TfmHolidaysEditor
  Left = 542
  Top = 223
  Caption = 'fmHolidaysEditor'
  ClientHeight = 420
  ClientWidth = 533
  Color = clBtnFace
  Constraints.MinHeight = 458
  Constraints.MinWidth = 542
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 533
    Height = 420
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnLocationDelete: TcxButton
      Tag = 2
      Left = 51
      Top = 56
      Width = 23
      Height = 22
      Action = actDeleteLocation
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 3
    end
    object clbLocations: TcxCheckListBox
      Left = 22
      Top = 84
      Width = 191
      Height = 106
      AllowDblClickToggle = False
      EditValueFormat = cvfIndices
      Items = <>
      PopupMenu = pmLocations
      Style.TransparentBorder = False
      TabOrder = 4
      OnClickCheck = clbLocationsClickCheck
      OnDblClick = clbLocationsDblClick
      OnKeyDown = clbLocationsKeyDown
      OnMouseDown = ListBoxMouseDown
    end
    object btnHolidaysDelete: TcxButton
      Tag = 2
      Left = 279
      Top = 56
      Width = 23
      Height = 22
      Action = actDeleteHoliday
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 6
    end
    object clbHolidays: TcxCheckListBox
      Left = 250
      Top = 84
      Width = 261
      Height = 106
      AllowDblClickToggle = False
      EditValueFormat = cvfStatesString
      Items = <>
      PopupMenu = pmHolidays
      Style.TransparentBorder = False
      TabOrder = 7
      OnClickCheck = clbHolidaysClickCheck
      OnDblClick = clbHolidaysDblClick
      OnKeyDown = clbHolidaysKeyDown
      OnMouseDown = ListBoxMouseDown
    end
    object lbxAllAddedHolidays: TcxListBox
      Left = 22
      Top = 226
      Width = 489
      Height = 143
      ItemHeight = 16
      ListStyle = lbOwnerDrawFixed
      Style.TransparentBorder = False
      TabOrder = 8
    end
    object btnOk: TcxButton
      Left = 175
      Top = 387
      Width = 112
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 9
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton
      Left = 293
      Top = 387
      Width = 112
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 10
    end
    object btnApply: TcxButton
      Left = 411
      Top = 387
      Width = 112
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Apply'
      TabOrder = 11
      OnClick = btnApplyClick
    end
    object btnImport: TcxButton
      Left = 10
      Top = 10
      Width = 23
      Height = 22
      Action = actImport
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 0
    end
    object btnLocationAdd: TcxButton
      Left = 22
      Top = 56
      Width = 23
      Height = 22
      Action = actAddLocation
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 2
    end
    object btnHolidaysAdd: TcxButton
      Left = 250
      Top = 56
      Width = 23
      Height = 22
      Action = actAddHoliday
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 5
    end
    object btnExport: TcxButton
      Left = 39
      Top = 10
      Width = 23
      Height = 22
      Action = actExport
      PaintStyle = bpsGlyph
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 1
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object gbLocations: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Locations'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnLocationDelete'
      CaptionOptions.Visible = False
      Control = btnLocationDelete
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = gbLocations
      AlignHorz = ahClient
      AlignVert = avClient
      Control = clbLocations
      ControlOptions.OriginalHeight = 101
      ControlOptions.OriginalWidth = 190
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutSplitterItem1: TdxLayoutSplitterItem
      Parent = dxLayoutGroup2
      AlignVert = avClient
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 1
    end
    object gbHolidays: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Holidays'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'btnHolidaysDelete'
      CaptionOptions.Visible = False
      Control = btnHolidaysDelete
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = gbHolidays
      AlignHorz = ahClient
      AlignVert = avClient
      Control = clbHolidays
      ControlOptions.OriginalHeight = 101
      ControlOptions.OriginalWidth = 260
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object gbAddedHolidays: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Added holidays'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = gbAddedHolidays
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbxAllAddedHolidays
      ControlOptions.OriginalHeight = 138
      ControlOptions.OriginalWidth = 480
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup7: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup7
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnApply'
      CaptionOptions.Visible = False
      Control = btnApply
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'btnImport'
      CaptionOptions.Visible = False
      Control = btnImport
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'btnLocationAdd'
      CaptionOptions.Visible = False
      Control = btnLocationAdd
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = gbLocations
      AlignHorz = ahClient
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'btnHolidaysAdd'
      CaptionOptions.Visible = False
      Control = btnHolidaysAdd
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = gbHolidays
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'btnExport'
      CaptionOptions.Visible = False
      Control = btnExport
      ControlOptions.OriginalHeight = 22
      ControlOptions.OriginalWidth = 23
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object ilActions: TcxImageList
    FormatVersion = 1
    DesignInfo = 18939992
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000B0B0B210B0B0B210B0B0B210B0B
          0B210B0B0B210000000000000000000000000000000000000000000000000000
          00000000000000000000000000000A8261FF09805FFF077E5DFF077C5BFF067A
          5AFF0B0B0B210000000000000000000000000000000000000000000000000000
          00000000000000000000000000000C8665FF3CC2AAFF3CC2AAFF3BC2AAFF077D
          5CFF0B0B0B210000000000000000000000000000000000000000000000000000
          00000000000000000000000000000F8968FF3DC5ADFF3DC5ACFF3DC5ACFF0980
          5FFF0B0B0B210000000000000000000000000000000000000000000000000000
          00000B0B0B210B0B0B210B0B0B21128D6CFF40C8B0FF40C8B0FF40C7B0FF0A82
          62FF0B0B0B210B0B0B210B0B0B210B0B0B210B0B0B2100000000000000001B9D
          7CFF1A9A79FF189776FF169573FF149270FF44CDB4FF44CCB3FF44CBB3FF0D87
          66FF0C8463FF0A8261FF097F5EFF077E5DFF0B0B0B2100000000000000001FA0
          7FFF4AD3BAFF4AD3B9FF49D2B9FF49D2B8FF49D0B8FF47D0B8FF47D0B7FF47CF
          B6FF46CFB7FF46CFB6FF46CEB5FF09805FFF0B0B0B21000000000000000020A4
          83FF4ED7BEFF4DD7BEFF4DD6BDFF4DD6BDFF4CD6BCFF4CD5BCFF4BD4BCFF4BD4
          BBFF4BD4BAFF49D3B9FF49D2B9FF0B8462FF0B0B0B21000000000000000022A6
          86FF51DCC2FF51DBC2FF51DBC1FF50DAC1FF50DAC1FF50DAC0FF4FD9C0FF4FD9
          BFFF4ED8BFFF4ED8BFFF4DD7BDFF0E8866FF0B0B0B21000000000000000023A9
          88FF23A886FF22A685FF20A383FF1EA180FF53DDC4FF53DEC4FF53DDC3FF1797
          76FF169473FF14916FFF128E6DFF108B6AFF0000000000000000000000000000
          000000000000000000000000000021A583FF57E1C7FF56E0C7FF56E0C6FF1A9A
          79FF0B0B0B210000000000000000000000000000000000000000000000000000
          000000000000000000000000000023A786FF58E3C9FF58E3C9FF58E3C8FF1C9E
          7DFF0B0B0B210000000000000000000000000000000000000000000000000000
          000000000000000000000000000024A989FF58E3C9FF58E3C9FF58E3C9FF1FA2
          81FF0B0B0B210000000000000000000000000000000000000000000000000000
          000000000000000000000000000025AB8AFF25AA89FF23A988FF23A786FF21A5
          84FF000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000A1C290141ACFF171717450000000000000000000000000000
          00000141ACFF1717174500000000000000000000000000000000000000000000
          0000000A1C290141ACFF2D60DFFF0141ACFF1717174500000000000000000141
          ACFF2D60DFFF0141ACFF17171745000000000000000000000000000000000000
          00000141ACFF2D60DFFF3666E9FF2D60DFFF0141ACFF171717450141ACFF2D60
          DFFF3666E9FF2D60DFFF0141ACFF171717450000000000000000000000000000
          0000000A1C290141ACFF2F63E1FF3869EBFF2F63E1FF0141ACFF2F63E1FF3969
          EBFF2F63E1FF0141ACFF17171745000000000000000000000000000000000000
          000000000000000A1C290141ACFF3368E3FF3C6EEEFF3C6EEEFF3C6FEEFF3367
          E3FF0141ACFF1717174500000000000000000000000000000000000000000000
          00000000000000000000000A1C290141ACFF4074F2FF4075F1FF4074F1FF0141
          ACFF171717450000000000000000000000000000000000000000000000000000
          000000000000000A1C290141ACFF3871E8FF4379F4FF4379F4FF437AF4FF3970
          E8FF0141ACFF1717174500000000000000000000000000000000000000000000
          0000000A1C290141ACFF3B73EAFF467DF6FF3B73EAFF0141ACFF3B73EAFF467D
          F6FF3B73EAFF0141ACFF17171745000000000000000000000000000000000000
          00000141ACFF3B73EAFF467DF6FF3B73EAFF0141ACFF171717450141ACFF3B73
          EAFF467DF6FF3B73EAFF0141ACFF171717450000000000000000000000000000
          0000000A1C290141ACFF3B73EAFF0141ACFF1717174500000000000000000141
          ACFF3B73EAFF0141ACFF17171745000000000000000000000000000000000000
          000000000000000A1C290141ACFF171717450000000000000000000000000000
          00000141ACFF1717174500000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000066666600616161005C5C5C0057575700515151004D4D4D004747
          4700414141003C3C3C0037373700343434002C2C2C0000000000000000000000
          0000000000006B6B6B00E3E3E300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003232320000000000000000000000
          0000000000006B6B6B00E3E3E300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003434340000000000000000000000
          00000000000070707000E3E3E300FFFFFF00FFFFFF00BF611B00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003737370000000000000000000000
          00000000000073737300E3E3E300FFFFFF00FFFFFF00C5631B00C5631B00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF003C3C3C000000000000000000E07A
          2E00DD762A00DA722600D6712600D46C2200D16B1F00CB661C00CB661C00C563
          1B00FFFFFF00FFFFFF00FFFFFF00FFFFFF003C3C3C000000000000000000E883
          3A00E37E3400E37E3400E07A2E00DD762A00DA722600D6712600D16B1F00CE69
          1F00CB661C00FFFFFF00FFFFFF00FFFFFF00414141000000000000000000EE8D
          4600EE8D4600E8833A00E8833A00E37E3400E37E3400E07A2E00DD762A00D671
          2600FFFFFF00FFFFFF00FFFFFF00FFFFFF004545450000000000000000000000
          00000000000081818100E3E3E300FFFFFF00FFFFFF00E8833A00E37E3400FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004848480000000000000000000000
          00000000000081818100E3E3E300FFFFFF00FFFFFF00EE8D4600FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004B4B4B0000000000000000000000
          00000000000085858500E3E3E300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004E4E4E0000000000000000000000
          00000000000089898900E3E3E300FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF005353530000000000000000000000
          0000000000008A8A8A00E3E3E300E3E3E300E3E3E300E3E3E300E3E3E300E3E3
          E300E3E3E300E3E3E300E3E3E300E3E3E3005757570000000000000000000000
          0000000000008D8D8D008989890085858500818181007D7D7D00777777007373
          73007070700066666600646464005E5E5E005959590000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        MaskColor = clBlack
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000C5A6
          7B00B26C3400CA8B580024242400242424001919190019191900191919000F0F
          0F000F0F0F000F0F0F00BA774300A1551A00B78E5D000000000000000000BA77
          4300EFCA9100EBC5880034343400323232002C2C2C002C2C2C0024242400E2B9
          7A00E2B97A0019191900E2B97A00DAA65600A1551A000000000000000000C17E
          4800F2CE9600EFCA910045454500414141003C3C3C003737370034343400E5BD
          7D00E4BA7C0024242400E2B97A00DAA65600A1551A000000000000000000C683
          4E00F4D19D00F2CE960059595900535353004E4E4E004848480045454500E7BD
          8300E5BD7D0037373700E2B97A00DAA65600A85E24000000000000000000CA8B
          5800F6D5A500F4D19D006B6B6B0066666600616161005C5C5C00575757005353
          53004D4D4D0048484800E5BD7D00DAA65600A85E24000000000000000000CA8B
          5800FADAAB00F6D5A500F4D19D00EFCA9100EFCA9100EBC58800EBC58800EBC5
          8800E7BD8300E7BD8300E7BD8300E1AE5F00A85E24000000000000000000D191
          5D00FBDDB100F2CE9600F3C88600F3C88600EDC07A00ECBC7200E9B86D00E6B5
          6900E6B56900E3B16400E3B16400E1AE5F00B26C34000000000000000000D99B
          6800FFE4BB00E3AF7900E3AF7900E3B27500E3B27500E4AF6E00E4AF6E00E2AD
          6800E2AD6800E2AD6800E2AD6800E3B16400B26C34000000000000000000D99B
          6800FFE4BB00E3AF7900FCFCFC00FCFCFC00FAFAFA00FAFAFA00F8F8F800F8F8
          F800F6F6F600F6F6F600E2AD6800E6B56900B26C34000000000000000000DFA4
          7100FFE6C200DFA47100FEFEFE00D6D6D600D6D6D600D6D6D600D6D6D600D2D2
          D200D2D2D200F8F8F800E0A96A00E6B56900BA7743000000000000000000DFA4
          7100FFECC900DFA47100FFFFFF00FFFFFF00FEFEFE00FEFEFE00FCFCFC00FCFC
          FC00FAFAFA00FAFAFA00E0A96A00E9B86D00BA7743000000000000000000E4A7
          7500FFEFCF00D99B6800FFFFFF00D9D9D900D9D9D900D9D9D900D9D9D900D6D6
          D600D6D6D600FCFCFC00E0A96A00ECBC7200C17E48000000000000000000E4A7
          7500FFF4D400DFAE8500FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
          FF00FFFFFF00FCFCFC00E7BD8300EFCA9100C6834E000000000000000000E2CF
          B000E4A77500E4A77500D9D9D900D9D9D900D9D9D900D9D9D900D9D9D900D9D9
          D900D9D9D900D9D9D900D1915D00CA8B5800CCB5920000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        MaskColor = clBlack
      end>
  end
  object actlCommands: TActionList
    Images = ilActions
    Left = 152
    Top = 289
    object actAddLocation: TAction
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 0
      OnExecute = actAddClick
    end
    object actDeleteLocation: TAction
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 1
      OnExecute = actDeleteClick
    end
    object actImport: TAction
      Caption = 'Import'
      Hint = 'Import'
      ImageIndex = 2
      OnExecute = actImportExportClick
    end
    object actExport: TAction
      Tag = 1
      Caption = 'Export'
      Hint = 'Export'
      ImageIndex = 3
      OnExecute = actImportExportClick
    end
    object actAddHoliday: TAction
      Tag = 1
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 0
      OnExecute = actAddClick
    end
    object actDeleteHoliday: TAction
      Tag = 1
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 1
      OnExecute = actDeleteClick
    end
  end
  object pmLocations: TPopupMenu
    Images = ilActions
    Left = 80
    Top = 120
    object miAddLocation: TMenuItem
      Action = actAddLocation
    end
    object miEditLocations: TMenuItem
      Caption = 'Edit...'
      OnClick = miEditLocationsClick
    end
    object miDeleteLocation: TMenuItem
      Action = actDeleteLocation
    end
  end
  object pmHolidays: TPopupMenu
    Images = ilActions
    Left = 344
    Top = 120
    object miAddHoliday: TMenuItem
      Action = actAddHoliday
    end
    object miEditHolidays: TMenuItem
      Caption = 'Edit...'
      OnClick = miEditHolidaysClick
    end
    object miDeleteHoliday: TMenuItem
      Action = actDeleteHoliday
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end

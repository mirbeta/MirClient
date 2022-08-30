object wndMain: TwndMain
  Left = 0
  Top = 0
  Caption = 'Wi-Fi Available Networks'
  ClientHeight = 371
  ClientWidth = 859
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    859
    371)
  PixelsPerInch = 96
  TextHeight = 13
  object lv: TListView
    Left = 10
    Top = 8
    Width = 841
    Height = 355
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = '    SSID'
        Width = 200
      end
      item
        Alignment = taRightJustify
        Caption = 'Signal'
      end
      item
        Caption = 'Authentication'
        Width = 100
      end
      item
        Caption = 'Security'
        Width = 75
      end
      item
        Caption = 'PHY Type'
        Width = 100
      end
      item
        Caption = 'BSS Type'
        Width = 80
      end
      item
        Caption = 'MAC Address'
        Width = 120
      end
      item
        Alignment = taRightJustify
        Caption = 'RSSI'
        Width = 75
      end
      item
        Alignment = taRightJustify
        Caption = 'Channel Freq'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Channel #'
      end
      item
        Alignment = taRightJustify
        Caption = 'Max Speed'
        Width = 75
      end
      item
        Caption = 'Interface'
        Width = 250
      end>
    ColumnClick = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    SmallImages = ilWIFI
    SortType = stText
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 424
    Top = 134
  end
  object ilWIFI: TImageList
    Left = 272
    Top = 200
    Bitmap = {
      494C010104000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000B8CD0E87336B4D8D20CA6CEF307A0CDF80A9CCCF5179AC9E8309DC7CF57AF
      D2A8000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FDFEFE0460AB7BF6298525FA20730BF43780
      27D883B4837CF8FBF8070000000000000000000000000000000B8CD0E87336B4
      D8D20CA6CEF307A0CDF8119ECEF596CFE2F560AB7BF6298525FA20730BF43780
      27D883B4837C0000000700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000037B9
      DDCFABFFFFFF4CFFFFFF00FFFFFF00EEFFFF00D9FFFF00B5E9FF00AEE2FF0197
      CBFE44A6CEBB0000000000000000000000000000000000000000000000000000
      0000FBFFFF0400000000000000002B9E3FFF009A00FF00A207FF009700FF0E89
      00FF2E7100FF468A3DC2F8FBF807000000000000000037B9DDCFABFFFFFF4CFF
      FFFF00FFFFFF04EEFFFF8CEAF9FF2B9E3FFF009A00FF00A207FF009700FF0E89
      00FF2E7100FF468A3DC200000007000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000025BF
      E0EBC6FFFFFF43FFFFFF00FCFFFF00E7FFFF00D3FFFF00C3F6FF00B6E9FF00AC
      DFFF088CBFF70000000000000000000000000000000000000000000000000000
      0000FDFEFF02F5FEFF0E5ABE8AFF0DA71AFF2CB53FFFFDFBFFFF6FD488FF0098
      00FF009500FF307000FF83B4837C000000000000000025BFE0EBC6FFFFFF43FF
      FFFF00FCFFFF4DEEFFFF5ABE8AFF0DA71AFF2CB53FFFFDFBFFFF6FD488FF0098
      00FF009500FF307000FF83B4837C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000022BE
      E1EDC5FFFFFF43FFFFFF00FCFFFF00E7FFFF00D3FFFF00C3F6FF00B6E9FF00AA
      DDFF098DBEF60000000000000000000000000000000000000000000000000000
      0000FDFEFF020000000036AC4CFF2EB33CFFF5ECF4FFFFF6FFFFFFFFFFFF6FD5
      88FF009A00FF128800FF368126D9000000000000000022BEE1EDC5FFFFFF43FF
      FFFF00FCFFFF89F4FFFF36AC4CFF2EB33CFFF5ECF4FFFFF6FFFFFFFFFFFF6FD5
      88FF009A00FF128800FF368126D9000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000022C1
      E3EFC5FFFFFF43FFFFFF00FCFFFF00E7FFFF00D3FFFF00C3F6FF00B6E9FF00AA
      DDFF0B8FC0F40000000000000000000000000000000000000000000000000000
      0000FBFEFF04F9FEFF0E36AE45FF5DB964FFC0D6BCFF03AD21FF9DDBA7FFFFFF
      FFFF6DD487FF009500FF1F750AF5000000000000000022C1E3EFC5FFFFFF43FF
      FFFF00FCFFFF9CF6FFFF36AE45FF5DB964FFC0D6BCFF03AD21FF9DDBA7FFFFFF
      FFFF6DD487FF009500FF1F750AF5000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000021C4
      E5F1CAFFFFFF46FFFFFF00FFFFFF00EBFFFF00D5FFFF00C4F9FF00B8ECFF00AB
      DEFF0B90C1F40000000000000000000000000000000000000000000000000000
      0000FAFFFF05F8FEFF0E41B65CFF43CD6DFF19BB42FF25C04FFF0BB22EFF9DD9
      A5FFFFFFFFFF67CF7CFF308828D7000000000000000021C4E5F1CAFFFFFF46FF
      FFFF00FFFFFF89F6FFFF41B65CFF43CD6DFF19BB42FF25C04FFF0BB22EFF9DD9
      A5FFFFFFFFFF67CF7CFF308828D7000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000017C2
      E7F345EFFFFF1ACBEFFF19B5DEFF1AB7E3FF14B3E0FF0AAADAFF019FD3FF0099
      CEFF0B93C5F40000000000000000000000000000000000000000000000000000
      000000000000F6FCFD0E60BF88FF72DD96FF35CE6AFF2EC75EFF25BE4BFF09AF
      27FF8BD094FF4EBB59FF86BC8679000000000000000017C2E7F345EFFFFF1ACB
      EFFF19B5DEFF5DCCEBFF60BF88FF72DD96FF35CE6AFF2EC75EFF25BE4BFF09AF
      27FF8BD094FF4EBB59FF86BC8679000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003DE6
      FFE3508FA8FF405968FF51D3E7FF5BECFFFF4AE7FFFF3AE6FFFF28BAE0FF176E
      87FF0C87B8F30000000000000000000000000000000000000000000000000000
      000000000000000000000000000047BE61FF71DD96FF4AD37AFF29C252FF1CB7
      3BFF0DAA22FF41A341BEF9FCF90600000000000000003DE6FFE3508FA8FF4059
      68FF51D3E7FF5DECFFFFABF1FAFF47BE61FF71DD96FF4AD37AFF29C252FF1CB7
      3BFF0DAA22FF41A341BE00000006000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      003CA9B8B9E66E6E6CFF37D8EBFF3BCAEBFF32C8EBFF1FC6EBFF84A6B2FF5B52
      4FFD49AFD7B60000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000076BE80FF4AB256FF39B149FD46B1
      4DD389C68976F9FCF9060000000000000000000000000000003CA9B8B9E66E6E
      6CFF37D8EBFF3BCAEBFF38CAECFF98E3F1FF76BE80FF4AB256FF39B149FD46B1
      4DD389C689760000000600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E6DEDEBF7F716EE80000001B000000070000000800000007B5B7B9C66F69
      66ED000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E6DEDEBF7F71
      6EE80000001B00000007000000080000000ACACCCDD7BCB9B7F7000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F3F3F3BB726C6AF500000000000000000000000000000000C1BDBBB27370
      6FEE000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F3F3F3BB726C
      6AF500000000000000000000000000000000C1BDBBB273706FEE000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F5F5F593939090FF8C8785B5000000000000000000000028969696EF9898
      98C4000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F5F5F5939390
      90FF8C8785B5000000000000000000000028969696EF989898C4000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000002BE3E3E3F68E8B8AFE797473EE7B7776E8848180FC989898FED6D6
      D656000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000002BE3E3
      E3F68E8B8AFE797473EE7B7776E8848180FC989898FED6D6D656000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000037DBDBDBB6BEBEBEECA9A9A9F7A6A6A6D0D8D8D85B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0037DBDBDBB6BEBEBEECA9A9A9F7A6A6A6D0D8D8D85B00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFF00FFE03C007
      FFFFE007F6018003FFFFE007F0018001FFFFE007F4018001FFFFE007F0018001
      FFFFE007F0018001FFFFE007F8018001FFFFE007FE018003FFFFF007FF03C007
      FFFFF3CFFFFFCF3FFFFFF3CFFFFFCF3FFFFFF1CFFFFFC73FFFFFF80FFFFFE03F
      FFFFFC1FFFFFF07FFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end

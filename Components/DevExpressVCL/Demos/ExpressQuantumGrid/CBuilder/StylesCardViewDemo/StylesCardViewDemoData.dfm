object StylesCardViewDemoMainDM: TStylesCardViewDemoMainDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 285
  Top = 161
  Height = 479
  Width = 741
  object dsPersons: TDataSource
    DataSet = cdsPersons
    Left = 40
    Top = 104
  end
  object StyleRepository: TcxStyleRepository
    Left = 216
    Top = 8
    PixelsPerInch = 96
    object stBlueDark: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object stGold: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object stBlueLight: TcxStyle
      AssignedValues = [svColor]
      Color = 16247513
    end
    object stBlueBright: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16749885
      TextColor = clWhite
    end
    object stYellowLight: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clBlack
    end
    object stGreyLight: TcxStyle
      AssignedValues = [svColor]
      Color = 14872561
    end
    object stBlueSky: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle4: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle5: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 14811135
      TextColor = clBlack
    end
    object cxStyle6: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14811135
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clNavy
    end
    object cxStyle7: TcxStyle
      AssignedValues = [svColor]
      Color = 14872561
    end
    object cxStyle8: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 4707838
      TextColor = clBlack
    end
    object cxStyle9: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle10: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle11: TcxStyle
      AssignedValues = [svColor]
      Color = 8453888
    end
    object cxStyle12: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle13: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16777088
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlue
    end
    object cxStyle14: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 12937777
      TextColor = clWhite
    end
    object cxStyle15: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle16: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 12937777
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle17: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle18: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle19: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle20: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 16247513
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle21: TcxStyle
      AssignedValues = [svColor]
      Color = 15451300
    end
    object cxStyle22: TcxStyle
      AssignedValues = [svColor]
      Color = 8453888
    end
    object cxStyle23: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 16749885
      TextColor = clWhite
    end
    object cxStyle24: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = 12937777
      TextColor = clWhite
    end
    object cxStyle25: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13154717
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle26: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle27: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13154717
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle28: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 14933198
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle29: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 9928789
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle30: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 13154717
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clBlack
    end
    object cxStyle31: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 9928789
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle32: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle33: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clYellow
    end
    object cxStyle34: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clBlack
    end
    object cxStyle35: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle36: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clGreen
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle37: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle38: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clGreen
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Microsoft Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle39: TcxStyle
      AssignedValues = [svColor]
      Color = clSilver
    end
    object cxStyle40: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
    object cxStyle41: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clGray
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle42: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 8421440
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle43: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
    object cxStyle44: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clSilver
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle45: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = 33023
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      TextColor = clWhite
    end
    object cxStyle46: TcxStyle
      AssignedValues = [svColor, svFont, svTextColor]
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      TextColor = clWhite
    end
    object cxStyle47: TcxStyle
      AssignedValues = [svColor]
      Color = clWhite
    end
    object cvssDevExpress: TcxGridCardViewStyleSheet
      Caption = 'DevExpress'
      Styles.Background = cxStyle15
      Styles.Content = cxStyle18
      Styles.ContentEven = cxStyle19
      Styles.ContentOdd = cxStyle20
      Styles.Inactive = cxStyle21
      Styles.IncSearch = cxStyle22
      Styles.Selection = cxStyle24
      Styles.CaptionRow = cxStyle16
      Styles.CardBorder = cxStyle17
      Styles.RowCaption = cxStyle23
      BuiltIn = True
    end
    object cvssSlate: TcxGridCardViewStyleSheet
      Caption = 'Slate'
      Styles.Content = cxStyle26
      Styles.ContentEven = cxStyle27
      Styles.ContentOdd = cxStyle28
      Styles.Inactive = cxStyle29
      Styles.Selection = cxStyle31
      Styles.CaptionRow = cxStyle25
      Styles.RowCaption = cxStyle30
      BuiltIn = True
    end
    object cvssHighContrast: TcxGridCardViewStyleSheet
      Caption = 'High Contrast'
      Styles.Content = cxStyle33
      Styles.ContentEven = cxStyle34
      Styles.ContentOdd = cxStyle35
      Styles.Inactive = cxStyle36
      Styles.Selection = cxStyle38
      Styles.CaptionRow = cxStyle32
      Styles.RowCaption = cxStyle37
      BuiltIn = True
    end
    object cvssUserDefined: TcxGridCardViewStyleSheet
      Caption = 'User Defined'
      Styles.Background = cxStyle39
      Styles.Content = cxStyle40
      Styles.ContentEven = cxStyle42
      Styles.ContentOdd = cxStyle43
      Styles.Inactive = cxStyle44
      Styles.Selection = cxStyle46
      Styles.CaptionRow = cxStyle41
      Styles.CardBorder = cxStyle47
      Styles.RowCaption = cxStyle45
      BuiltIn = True
    end
  end
  object dsCountries: TDataSource
    DataSet = cdsCountries
    Left = 112
    Top = 104
  end
  object EditRepository: TcxEditRepository
    Left = 216
    Top = 64
    object edrepGender: TcxEditRepositoryImageComboBoxItem
      Properties.Images = ilPics
      Properties.Items = <
        item
          Description = 'Female'
          ImageIndex = 0
          Value = False
        end
        item
          Description = 'Male'
          ImageIndex = 1
          Value = True
        end>
    end
    object edrepCountry: TcxEditRepositoryLookupComboBoxItem
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'ACRONYM'
        end
        item
          FieldName = 'NAME'
        end>
      Properties.ListFieldIndex = 1
      Properties.ListSource = dsCountries
    end
  end
  object ilPics: TImageList
    Left = 221
    Top = 120
    Bitmap = {
      494C010102000400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      000000000000000000000000000000000000000000001710FE001710FE001710
      FE001710FE00334AA800334AA800334AA800334AA8001710FE001710FE001710
      FE006C6C6D00000000000000000000000000000000005F79E7005F79E7005F79
      E7005F79E7005F79E7005F79E70082A9D40081A7D1005D77E2005E77E300576E
      CE005E77E3005F79E7005F79E7007381C0000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000262B6A001710FE001710
      FE001710FE004A8CB1005EB6E8005EB6E8005EB6E8001710FE001710FE007B7B
      7C00000000000000000000000000000000005F79E7005B73DA005B73DA005B73
      D9005971D5005D76E0005A72D70097C8FD0097C8FD006E8DDD005F79E7005C74
      DC005E78E5005F79E7006379D300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F0F0F0007B7B8B001511
      95001511CE004A8CB1005EB6E8005EB6E8005AABDA0015119500262B6A000000
      0000000000000000000000000000000000005F79E7005C75DE005970D3005F79
      E7005F79E7006883BE0085AFED0097C8FD0097C8FD0084ACE2005F79E7005B73
      D9005A71D100919BC400E6E6E600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000345A710042799A0037667E0037667E00E1E1E200000000000000
      0000000000000000000000000000000000005F79E7005F79E700576ECE005F79
      E7006A88E7008BB6E5008EBBEB0091BFF10097C8FD0097C8FD005F77BF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F0F0
      F000495255005197C10037667E002E4672005197C10037667E00152D3400467D
      83006C6C6D0089898A00A8A8A900000000008792C3005A71D1005971D5005F79
      E70080A7E6008BB6E5008BB6E5008FBCED0095C5F90097C8FD007184AE000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002F4F
      65005EB6E8005EB6E80037667E002733880037667E005EB6E8005EB6E80000FF
      FF0000FFFF0000FFFF0000FFFF00000000000000000000000000000000005F79
      E7006C8ADF0084ACD7008AB4E30096C6FA0094C4F70096C7FC0093BCE8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007B7B7C00467D83003766
      7E005EB6E8005EB6E80055A2CE0024354D004A8CB1005EB6E8005EB6E80000E0
      F00000FFFF0000FFFF0000FFFF0000000000000000000000000000000000B8BC
      CA009FA9C30089B3E1008DB8E8008AB5E300A1C5EB0090BEEF0091BFF1000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF002E88
      A3005EB6E8005EB6E8005EB6E800F0F0F00042799A005EB6E8005EB6E8005EB6
      E8002F4F650000D2E10000FFFF00C4C4C4000000000000000000000000000000
      0000000000008BB6E5008FBCED008EBBEB0089B2E00094C4F70092C0F3008AB5
      E300C5CFD9000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000FFFF0000FFFF0004B3
      C3005EB6E8005EB6E8005EB6E8005EB6E8005EB6E8005EB6E8005EB6E8005EB6
      E80024354D0004B3C30000FFFF00C4C4C4000000000000000000000000000000
      0000E9F1FA008BB6E5008EBBEB0095C5F90086AFDC0093C1F40096C7FC0091BF
      F100809EC1000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001181890000FFFF002E88
      A30037667E005EB6E8005EB6E8005AABDA005EB6E80024354D00181D23005AAB
      DA005EB6E80000FFFF0000FFFF00E1E1E2000000000000000000000000000000
      0000B6CAE1008BB6E50090BEF0009FB4CC007798BD0081818200747476006075
      8E0047515C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000006C6C6D0000F1FF00259E
      B800181D23005EB6E80055A2CE00152D34002B46570042799A005AABDA0055A2
      CE005EB6E80000FFFF0000FFFF00000000000000000000000000000000000000
      0000A1BCDC0086AEDA007494B700606367005F748C00495460003B3E430085AC
      D800353537000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000089898A0000FF
      FF0000FFFF005EB6E8005EB6E8005EB6E8005EB6E8005EB6E8005197C1005EB6
      E80055A2CE0000FFFF0000939C00000000000000000000000000000000000000
      0000000000005B6D83008BB6E5008BB6E50093C1F40097C8FD0097C8FD0085AC
      D800353537000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000F0F0F00000FF
      FF0000FFFF003C98BB005EB6E8003B738A00345A71005EB6E8005EB6E8005AAB
      DA001294A60000FFFF005A646800000000000000000000000000000000000000
      0000D9D9D9004B5563008BB6E5008BB6E50091BFF10097C8FD0097C8FD00667F
      9A00353537000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A8A8
      A90000939C0000FFFF0000FFFF002E88A3004A8CB1003A8DAD00259EB80000FF
      FF0000FFFF00D3D3D30000000000000000000000000000000000000000000000
      0000F2F2F2003A3D420086AEDA00454D58003535370035353700353537003535
      3700353537000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D3D3D30000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000E0
      F0000F707B000000000000000000000000000000000000000000000000000000
      0000000000003535370040454D00353537003535370035353700353537003535
      37004E4E50000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000009797980089898A00E1E1E200000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B3B3B4006868690068686900B3B3B400D9D9D9000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF008007800000000000800F000100000000
      801F000100000000F83F001F00000000E001001F00000000E001E01F00000000
      8001E01F000000008000F807000000008000F007000000008000F00700000000
      8001F00700000000C001F80700000000C001F00700000000E003F00700000000
      F007F80700000000FE3FFC1F0000000000000000000000000000000000000000
      000000000000}
  end
  object cdsPersons: TClientDataSet
    Aggregates = <>
    Params = <>
    OnCalcFields = cdsPersonsCalcFields
    Left = 40
    Top = 56
    object cdsPersonsID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsPersonsFIRSTNAME: TStringField
      FieldName = 'FIRSTNAME'
      Size = 50
    end
    object cdsPersonsSECONDNAME: TStringField
      FieldName = 'SECONDNAME'
      Size = 50
    end
    object cdsPersonsGENDER: TBooleanField
      FieldName = 'GENDER'
    end
    object cdsPersonsBIRTHNAME: TStringField
      FieldName = 'BIRTHNAME'
      Size = 50
    end
    object cdsPersonsDATEOFBIRTH: TDateTimeField
      FieldName = 'DATEOFBIRTH'
    end
    object cdsPersonsBIRTHCOUNTRY: TIntegerField
      FieldName = 'BIRTHCOUNTRY'
    end
    object cdsPersonsLOCATIONOFBIRTH: TStringField
      FieldName = 'LOCATIONOFBIRTH'
      Size = 50
    end
    object cdsPersonsBIOGRAPHY: TMemoField
      FieldName = 'BIOGRAPHY'
      BlobType = ftMemo
      Size = 10
    end
    object cdsPersonsNICKNAME: TStringField
      FieldName = 'NICKNAME'
      Size = 50
    end
    object cdsPersonsHOMEPAGE: TStringField
      FieldName = 'HOMEPAGE'
      Size = 100
    end
    object cdsPersonsFullName: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'FullName'
      Calculated = True
    end
  end
  object cdsCountries: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 112
    Top = 56
    object cdsCountriesID: TAutoIncField
      FieldName = 'ID'
      ReadOnly = True
    end
    object cdsCountriesNAME: TStringField
      FieldName = 'NAME'
      Size = 60
    end
    object cdsCountriesACRONYM: TStringField
      FieldName = 'ACRONYM'
      Size = 50
    end
    object cdsCountriesNATIONALFLAG: TBlobField
      FieldName = 'NATIONALFLAG'
      BlobType = ftParadoxOle
      Size = 10
    end
  end
end

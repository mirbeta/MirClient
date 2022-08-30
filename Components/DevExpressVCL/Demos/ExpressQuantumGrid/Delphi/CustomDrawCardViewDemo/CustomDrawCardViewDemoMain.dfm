inherited CustomDrawCardViewDemoMainForm: TCustomDrawCardViewDemoMainForm
  Left = 679
  Top = 140
  Caption = 'ExpressQuantumGrid CustomDrawCardView Demo'
  ClientHeight = 637
  ClientWidth = 812
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 812
    Caption = 
      'This demo shows some examples of custom draw. Click '#39'About this ' +
      'demo'#39' for more information.'
  end
  object Splitter: TSplitter [1]
    Left = 241
    Top = 16
    Width = 2
    Height = 602
  end
  object pnPersonLines: TPanel [2]
    Left = 0
    Top = 16
    Width = 241
    Height = 602
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object pnPersonLinesCaption: TPanel
      Left = 0
      Top = 0
      Width = 241
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Occupation'
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object cxgPersonLine: TcxGrid
      Left = 0
      Top = 33
      Width = 241
      Height = 569
      Align = alClient
      TabOrder = 1
      object tvPersonLine: TcxGridDBTableView
        DataController.DataSource = FilmsDemoDM.dsPersonLines
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsData.Deleting = False
        OptionsData.Editing = False
        OptionsData.Inserting = False
        OptionsSelection.InvertSelect = False
        OptionsView.ColumnAutoWidth = True
        OptionsView.GroupByBox = False
        Styles.StyleSheet = GridTableViewStyleSheetDevExpress
        object tvPersonLineNAME: TcxGridDBColumn
          Caption = 'Name'
          DataBinding.FieldName = 'NAME'
        end
      end
      object lvPersonLine: TcxGridLevel
        GridView = tvPersonLine
      end
    end
  end
  object pnPersons: TPanel [3]
    Left = 243
    Top = 16
    Width = 569
    Height = 602
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object cxgPersons: TcxGrid
      Left = 0
      Top = 33
      Width = 569
      Height = 569
      Align = alClient
      TabOrder = 0
      object cvPersons: TcxGridDBCardView
        OnCustomDrawCell = cvPersonsCustomDrawCell
        DataController.DataSource = FilmsDemoDM.dsFilmsPersons
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsData.Deleting = False
        OptionsData.Inserting = False
        OptionsView.CardBorderWidth = 1
        OptionsView.CardIndent = 7
        Styles.ContentEven = stDefault
        Styles.ContentOdd = stDefault
        object cvPersonsFIRSTNAME: TcxGridDBCardViewRow
          Caption = 'Firstname'
          DataBinding.FieldName = 'FIRSTNAME'
          Position.BeginsLayer = True
        end
        object cvPersonsSECONDNAME: TcxGridDBCardViewRow
          Caption = 'Secondname'
          DataBinding.FieldName = 'SECONDNAME'
          Position.BeginsLayer = True
        end
        object cvPersonsGENDER: TcxGridDBCardViewRow
          DataBinding.FieldName = 'GENDER'
          Position.BeginsLayer = True
        end
        object cvPersonsBIRTHNAME: TcxGridDBCardViewRow
          Caption = 'Birth Name'
          DataBinding.FieldName = 'BIRTHNAME'
          Position.BeginsLayer = True
        end
        object cvPersonsDATEOFBIRTH: TcxGridDBCardViewRow
          Caption = 'Date of birth'
          DataBinding.FieldName = 'DATEOFBIRTH'
          Position.BeginsLayer = True
        end
        object cvPersonsLOCATIONOFBIRTH: TcxGridDBCardViewRow
          Caption = 'Location of Birth'
          DataBinding.FieldName = 'LOCATIONOFBIRTH'
          Position.BeginsLayer = True
        end
        object cvPersonsBIRTHCOUNTRY: TcxGridDBCardViewRow
          Caption = 'Birth Country'
          DataBinding.FieldName = 'BIRTHCOUNTRY'
          PropertiesClassName = 'TcxLookupComboBoxProperties'
          Properties.KeyFieldNames = 'ID'
          Properties.ListColumns = <
            item
              FieldName = 'NAME'
            end>
          Properties.ListSource = FilmsDemoDM.dsCountries
          Position.BeginsLayer = True
        end
        object cvPersonsBIOGRAPHY: TcxGridDBCardViewRow
          Caption = 'Biography'
          DataBinding.FieldName = 'BIOGRAPHY'
          PropertiesClassName = 'TcxBlobEditProperties'
          Properties.BlobEditKind = bekMemo
          Position.BeginsLayer = True
        end
        object cvPersonsNICKNAME: TcxGridDBCardViewRow
          Caption = 'Nickname'
          DataBinding.FieldName = 'NICKNAME'
          Position.BeginsLayer = True
        end
      end
      object lvPersons: TcxGridLevel
        GridView = cvPersons
      end
    end
    object pnPersonsCaption: TPanel
      Left = 0
      Top = 0
      Width = 569
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Persons'
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
    end
  end
  inherited sbMain: TStatusBar
    Top = 618
    Width = 812
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miFont: TMenuItem
        Caption = '&Font...'
        OnClick = miFontClick
      end
      object miSeparator4: TMenuItem
        Caption = '-'
      end
      object miCustomDrawStyles: TMenuItem
        Caption = '&Custom Draw Styles'
        object miBackgroundImage: TMenuItem
          Caption = '&Background Image'
          GroupIndex = 1
          object miTile: TMenuItem
            Caption = '&Tile'
            Checked = True
            GroupIndex = 1
            RadioItem = True
            OnClick = miTileClick
          end
          object miSky: TMenuItem
            Caption = '&Sky'
            GroupIndex = 1
            RadioItem = True
            OnClick = miSkyClick
          end
          object miEgypt: TMenuItem
            Caption = '&Egypt'
            GroupIndex = 1
            RadioItem = True
            OnClick = miEgyptClick
          end
          object miMyFace: TMenuItem
            Caption = 'My &Face'
            GroupIndex = 1
            RadioItem = True
            OnClick = miMyFaceClick
          end
          object miCar: TMenuItem
            Caption = '&Car'
            GroupIndex = 1
            RadioItem = True
            OnClick = miCarClick
          end
          object miLoadImage: TMenuItem
            Caption = '&Load Image...'
            GroupIndex = 1
            RadioItem = True
            OnClick = miLoadClick
          end
        end
        object miGradient: TMenuItem
          Caption = '&Gradient Drawing'
          GroupIndex = 1
          object miGrey: TMenuItem
            Caption = 'G&rey'
            GroupIndex = 1
            RadioItem = True
            OnClick = miGreyClick
          end
          object miGreen: TMenuItem
            Caption = '&Green'
            GroupIndex = 1
            RadioItem = True
            OnClick = miGreenClick
          end
          object miGold: TMenuItem
            Caption = 'Go&ld'
            GroupIndex = 1
            RadioItem = True
            OnClick = miGoldClick
          end
          object miBlue: TMenuItem
            Caption = '&Blue'
            GroupIndex = 1
            RadioItem = True
            OnClick = miBlueClick
          end
        end
        object miDependOnDataDrawing: TMenuItem
          Caption = 'D&epends on the data'
          GroupIndex = 1
          RadioItem = True
          OnClick = miDependOnDataDrawingClick
        end
        object miDefaultDrawing: TMenuItem
          Caption = '&Default Drawing'
          GroupIndex = 1
          RadioItem = True
          OnClick = miDefaultDrawingClick
        end
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object ilPics: TImageList
    Left = 392
    Top = 32
    Bitmap = {
      494C010102000400280010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
  object OpenDialog: TOpenDialog
    Filter = 'BMP Windows Bitmap|*.bmp'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 360
    Top = 32
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 328
    Top = 32
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 112
    Top = 24
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
    object stDefault: TcxStyle
      AssignedValues = [svColor, svTextColor]
      Color = clWindow
      TextColor = clBlack
    end
  end
end

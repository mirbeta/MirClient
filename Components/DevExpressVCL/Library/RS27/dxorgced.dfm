object frmEChartEditor: TfrmEChartEditor
  Left = 8
  Top = 8
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'TdxOrgChart editor'
  ClientHeight = 367
  ClientWidth = 594
  Color = clBtnFace
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object TreeBox: TGroupBox
    Left = 12
    Top = 10
    Width = 405
    Height = 314
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '123'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object Panel2: TPanel
      Left = 2
      Top = 276
      Width = 401
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Panel5: TPanel
        Left = 184
        Top = 0
        Width = 217
        Height = 36
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object InsButton: TSpeedButton
          Left = 0
          Top = 3
          Width = 25
          Height = 25
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FF4B9169096932086630488A64FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0C6E3750
            D0A42AC591086630FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FF0E713A56D0A72CC591096932FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0F743E5C
            D1A92FC6930B6C35FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            54A17A168049157D47137B4411784035C69532C5940C6E370B6C350969320866
            30488A63FF00FFFF00FFFF00FFFF00FF1A864F7DD8B941C7993DC6973AC69737
            C69635C69532C5932FC6932DC6912AC590086630FF00FFFF00FFFF00FFFF00FF
            1B885283D9BB7DD8B979D7B674D6B46ED5B237C69535C6945DD2AA58D1A751D0
            A4096932FF00FFFF00FFFF00FFFF00FF53A57C1B89521A864F18834C16804973
            D6B53AC6961177400F743E0E713B0C6E38468E65FF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FF18834C7AD7B73EC698137B44FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF1A864F7E
            D8BA40C699157E47FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FF1B885283D9BC7FD8B9168049FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF53A57C1B
            88521A864F4F9F76FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          ParentShowHint = False
          ShowHint = True
          OnClick = InsButtonClick
        end
        object CInsButton: TSpeedButton
          Left = 31
          Top = 3
          Width = 25
          Height = 25
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FF4E946E0E6F390C6D370B6A350A693209
            673007662E06632D05612B04602A035E28025D2744855EFF00FFFF00FFFF00FF
            10733D81DABB3FC6983DC6973AC69739C69636C59534C69532C6942FC6932DC6
            912CC591025D26FF00FFFF00FFFF00FF12764087DBBE41C6993FC6983DC6983A
            C6976AD1AD36C69533C69432C6942FC5922DC692035E28FF00FFFF00FFFF00FF
            1378428CDCC144C79A42C6993FC7999BDBC5C6BBB498DBC536C69434C69431C6
            932FC593046029FF00FFFF00FFFF00FF157A4491DDC38CDDC18ADCBFBBE4D6A1
            897C7E5741A1897CB4E3D36FD5B36BD5B166D5AF05622BFF00FFFF00FFFF00FF
            4E9A72157A4413784284B49AA28C7F9E7D6AD7BEB09D7A67A1897C80AE940867
            3007652F42875FFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA28C7F9F7F6CDA
            C0B3D9BFB2D7BDB09C7965A28C7EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFA58E81A18271E4D0C6E2CEC3E1CBC1D8BFB2DDC8BE9E7E6BA28C
            7EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC0ACA1926A539068518F66508D
            654EE2CFC48A634C88624B87614A866049B9A69BFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FF8F6750E4D1C68B644DFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF90
            6852E5D3C98C654EFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFD7CCC6926A53E4D1C78E6750FF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFA17760A0755E9D735D9C725C9A705A986E58966D57A8
            8571E1C9BE906851FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA37761F1E4DD
            F1E2DCF0E1DAEEDFD8EEDDD6ECDCD4EBDBD2D4BCB096705AFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFA47862A27761A176609F755E9E735D9C725B9A705A98
            6F589F7A66D6CBC5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          ParentShowHint = False
          ShowHint = True
          OnClick = CInsButtonClick
        end
        object DelButton: TSpeedButton
          Left = 62
          Top = 3
          Width = 25
          Height = 25
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFD5D5E0ACACC9FF00FFFF00FFFF00FFFF00FFAAAAC5D4D4DEFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD5D5E13332891F1F81A7A7C6FF
            00FFFF00FFA6A5C41616742A2979D4D4DEFF00FFFF00FFFF00FFFF00FFFF00FF
            D6D5E236358D3543C12F45D6212184A7A7C6A7A6C519197A2431CB2830B12928
            79D4D4DEFF00FFFF00FFFF00FFFF00FFAFAFCE27268B6B78E1465FED2F44D61E
            1F831B1D802838CE3B4AE36168D61A1975AAAAC5FF00FFFF00FFFF00FFFF00FF
            FF00FFAAAACC29298D6A78E1445FEE2E44D72B41D43D50E7636DD91E1F7BA6A5
            C4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFAAAACC28298D4058DD35
            52EB324DE8394BD6212181A7A6C5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFAAAACC2527913552DE4562EF445DED2E45D71D1E83A7A7C6FF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFACABCE282A953957DF4B6CF26D
            7CE46C7AE2455EED2E43D51E2082A7A7C6FF00FFFF00FFFF00FFFF00FFFF00FF
            B1B1D12D2D96536DE44F71F56E7FE42B2B9128298D6A78E0455FED4858D92221
            80ACACC9FF00FFFF00FFFF00FFFF00FFD7D6E33E3D9B5E69CE6F81E6313096AB
            AACCAAAACC28298E6B79E1565FC4333289D5D5E0FF00FFFF00FFFF00FFFF00FF
            FF00FFD7D6E33E3D9B2F3097ABABCEFF00FFFF00FFAAAACC27278B36358ED5D5
            E1FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD7D6E3B1B1D2FF00FFFF
            00FFFF00FFFF00FFAFAFCED6D5E1FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          ParentShowHint = False
          ShowHint = True
          OnClick = DelButtonClick
        end
        object ZoomButton: TSpeedButton
          Left = 107
          Top = 3
          Width = 25
          Height = 25
          AllowAllUp = True
          GroupIndex = 1
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFFF00FFFF00FFFF00FF8AA0C02A518F99A9C3FF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBECBDB2C5D9E2262
            A74D90C5234B8CFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFFF00FFAFC2D8185DA74795CF75BBE22566AA8097BBFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFCED8E11D61AA4F9BD18ED4F14998
            D123589CFF00FFFF00FFFF00FFFF00FFE2DDDAA88D7E8B664F75492E88644DA4
            897A5579A12A69A876B1D2519CD2155BA9B1C1D6FF00FFFF00FFFF00FFD0C4BD
            875E44B69171DCBB98EFD0ACDBBB97B38C6D7049355162772E6BA8195DA9A2B9
            D4FF00FFFF00FFFF00FFE5E2E08E664DD4B28EEBCBA5EBCBA4EBCBA4EBCBA5EB
            CCA5D3B18D724C3746709DC3D0DEFF00FFFF00FFFF00FFFF00FFB19788B78F6E
            E5C39AE5C49BE5C39BE5C39BE5C39BE5C39BE5C39AB08968A18575FF00FFFF00
            FFFF00FFFF00FFFF00FF956C56D3AE86DFBC91E6C9A7EAD3B8EDD9C0EAD3B8E6
            C9A8DFBB90D3AE857E553DFF00FFFF00FFFF00FFFF00FFFF00FF8E6249D6B084
            E4C9AB9B593295512C8E4C2587451D813F18E4C9AAD7B285784B30FF00FFFF00
            FFFF00FFFF00FFFF00FF997159C89F74F0E3D4F1E5D8F1E5D8F1E5D8F1E5D8F1
            E5D8F0E3D4C79E73845A42FF00FFFF00FFFF00FFFF00FFFF00FFB89F90AB7F5A
            EEE1D2F6EFE6F6EFE6F6EFE6F6EFE6F6EFE6EEE1D2A27651A78C7AFF00FFFF00
            FFFF00FFFF00FFFF00FFE3DEDB976E55C09D79F8F3EDFBF7F4FBF7F4FBF7F4F9
            F4EFBE9B77875D44E5E1DFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD6C9C2
            966B53A77E5BCCAF93E5D4BFCCAF93A37755885E43D0C4BDFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFE3DEDBB79D8F9A735C8C5F45966E57B0
            9586DFD9D5FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          ParentShowHint = False
          ShowHint = True
          OnClick = ZoomButtonClick
        end
        object RotateButton: TSpeedButton
          Left = 138
          Top = 3
          Width = 25
          Height = 25
          AllowAllUp = True
          GroupIndex = 2
          Glyph.Data = {
            36030000424D3603000000000000360000002800000010000000100000000100
            18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFA26D3695693CB39577E1DBD4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFA36E37DDC1A7BB936D926232D3C6
            B8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
            00FFA46F38B48656D5B494D9BCA1946435E1DBD4FF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFE1D8CEAE8253CDAD8FBD97
            72B49679FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC3A98FFF
            00FFFF00FFFF00FFE1D8CEB0804FE3C8AF966B3DFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFBCA2879B6833FF00FFFF00FFFF00FFFF00FFAA7641EACE
            B5905F30FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFBCA287C69D769E6A35FF
            00FFFF00FFFF00FFE1D8CEB28251DBB18C996B40FF00FFFF00FFFF00FFFF00FF
            FF00FFBBA186BF946BEABE98A06C38DED4C9FF00FFDDD3C8B28657D0A47BC494
            66B89C7EFF00FFFF00FFFF00FFFF00FFBAA185C09973EDC29DECBF9AD7A87DB0
            7E4CA06C37AC7A48D5A87EDAB390AD7B48E3DDD7FF00FFFF00FFFF00FFD3C6B8
            A77A4FF8E3D0F3D2B7EEC29DECC09AE9CDB4F4DDCBE7CBB1CFA782AF7C48DBCC
            BEFF00FFFF00FFFF00FFFF00FFFF00FFBDA389C29E79F8E2D0F3D2B7B27D45B8
            8957AE7942B58655C6A785E6E0D9FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFBFA58BC49F7CF8E2D0B37E46FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC1A78DC6A17DB57F47FF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FFC3A98EB68149FF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFD3B99DFF
            00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
          ParentShowHint = False
          ShowHint = True
          OnClick = RotateButtonClick
        end
        object btnAntialiasing: TSpeedButton
          Left = 182
          Top = 3
          Width = 25
          Height = 25
          AllowAllUp = True
          GroupIndex = 3
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFECB8
            74FFECB772FFEBB66FFFEBB46DFFEAB36BFFEAB268FFE9B066FFE9AF64FFE8AD
            61FFE8AC5FFFE7AB5CFFE7A95AFFE6A858FFE6A755FFFFFFFFFFFFFFFFFFEDBA
            78FFFCF3E6FFFBF1E3FFFBF1E1FFFBEFDFFFFBEFDDFFFBEDDBFFFAEDD9FFF9EB
            D8FFF9EBD6FFF9EAD5FFF9E9D4FFF9E9D3FFE7A959FFFFFFFFFFFFFFFFFFEEBC
            7BFFFCF4E9FFFCF4E7FFFCF2E5FFFBF1E3FFFBF0E1FFFBEFDEFFFBEDDDFF4949
            49FF404040FF373737FF2D2D2DFFF9E9D4FFE7AB5CFFFFFFFFFFFFFFFFFFEEBF
            7FFFFDF6EDFFFCF5EBFFFCF4E9FFFCF3E6FFFCF2E4FFFBF1E2FFFBEFDFFFFBEF
            DDFFFBEDDBFF424242FF393939FFF9EBD6FFE8AD60FFFFFFFFFFFFFFFFFFEFC0
            83FFFDF8F0FFFDF7EEFFFCF5ECFFFCF5EAFFFCF4E8FFFCF2E6FFFBF1E4FFFBF0
            E1FF555555FFFBEEDDFF444444FFFAECD9FFE9AF64FFFFFFFFFFFFFFFFFFF0C3
            87FFFDF9F3FFFDF8F1FFFDF7EFFFFDF6EEFFFCF5EBFFFCF5E9FFFCF3E7FF6060
            60FFFBF1E2FFFBF0E0FF4F4F4FFFFBEDDCFFEAB268FFFFFFFFFFFFFFFFFFF0C4
            8AFFFEFAF6FFFEFAF4FFFDF9F2FFFDF8F1FFFDF7EFFFFDF6ECFFFCF5EBFFFCF4
            E8FFFCF2E6FFFCF2E4FFFBF0E2FFFBEFDFFFEAB46CFFFFFFFFFFFFFFFFFFCC74
            14FFCA7213FFC97113FFC76F11FFC66D10FFC56B0FFFC46A0FFFFDF6EEFFFDF6
            ECFFFCF5E9FFFCF3E8FFFCF2E5FFFCF1E3FFEBB66FFFFFFFFFFFFFFFFFFFCE78
            16FFF8DEB8FFF7DAB0FFF6D5A6FFF5D09DFFF4CC94FFC66D10FFFDF8F1FFFDF7
            EFFFFDF6EDFFFCF5EBFFFCF4E9FFFCF3E6FFECB873FFFFFFFFFFFFFFFFFFD17C
            17FFFAE4C6FFF9E1BEFFF8DCB5FFF7D7ABFFF6D3A3FFC87012FFFEF9F4FFFDF9
            F2FFFDF8F0FFFDF7EEFFFDF6ECFFFCF5EAFFEDBA77FFFFFFFFFFFFFFFFFFD47F
            1AFFFCEBD3FFFBE7CCFFF9E4C3FFF8DFBBFFF7DAB2FFCB7314FFFEFBF6FFFEFA
            F5FFFEF9F3FFFDF8F1FFFDF7EFFFFDF7EDFFEDBC7BFFFFFFFFFFFFFFFFFFD784
            1CFFFDF1E0FFFCEDD8FFFCEAD1FFFAE6C9FFF9E1C0FFCE7715FFFEFCF9FFFEFC
            F7FFFEFBF6FFFEFAF4FFFEF9F2FFFDF8F0FFEEBE7EFFFFFFFFFFFFFFFFFFDB88
            1EFFFEF6E9FFFEF3E4FFFDF0DCFFFCEDD6FFFBE8CEFFD17B17FFFFFDFBFFFFFC
            F9FFFEFCF8FFFEFBF7FFFEFAF5FFFEF9F3FFEFC082FFFFFFFFFFFFFFFFFFDE8C
            20FFDC8A1FFFDA871DFFD9851CFFD7831BFFD5811BFFD37F19FFF2C992FFF2C8
            90FFF1C78EFFF1C68CFFF0C48AFFF0C388FFF0C285FFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
          ParentShowHint = False
          ShowHint = True
          OnClick = btnAntialiasingClick
        end
      end
    end
    object Tree: TdxOrgChart
      Left = 12
      Top = 19
      Width = 381
      Height = 254
      Anchors = [akLeft, akTop, akRight, akBottom]
      Options = [ocSelect, ocFocus, ocButtons, ocDblClick, ocEdit, ocCanDrag, ocShowDrag]
      OnChange = TreeChange
      Ctl3D = True
      Font.Style = []
      ParentCtl3D = False
    end
  end
  object PropBox: TGroupBox
    Left = 424
    Top = 10
    Width = 160
    Height = 314
    Anchors = [akTop, akRight, akBottom]
    Caption = 'Node properties'
    Enabled = False
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 19
      Width = 28
      Height = 13
      Caption = '&Width'
      FocusControl = WidthEdit
      Font.Style = []
      ParentFont = False
      OnClick = Label1Click
    end
    object Label2: TLabel
      Left = 83
      Top = 19
      Width = 31
      Height = 13
      Caption = '&Height'
      FocusControl = HeightEdit
      Font.Style = []
      ParentFont = False
      OnClick = Label1Click
    end
    object Label3: TLabel
      Left = 12
      Top = 59
      Width = 25
      Height = 13
      Caption = '&Color'
      FocusControl = ColorEdit
      Font.Style = []
      ParentFont = False
      OnClick = Label1Click
    end
    object Label4: TLabel
      Left = 12
      Top = 102
      Width = 46
      Height = 13
      Caption = 'Child&Align'
      FocusControl = AlignEdit
      Font.Style = []
      ParentFont = False
      OnClick = Label1Click
    end
    object Label5: TLabel
      Left = 12
      Top = 147
      Width = 30
      Height = 13
      Caption = '&Shape'
      FocusControl = ShapeEdit
      Font.Style = []
      ParentFont = False
      OnClick = Label1Click
    end
    object Label6: TLabel
      Left = 12
      Top = 192
      Width = 58
      Height = 13
      Caption = 'ImageIndex'
      FocusControl = IIEdit
      Font.Style = []
      ParentFont = False
      OnClick = Label1Click
    end
    object Label7: TLabel
      Left = 83
      Top = 192
      Width = 53
      Height = 13
      Caption = 'ImageAlign'
      FocusControl = IAEdit
      Font.Style = []
      ParentFont = False
      OnClick = Label1Click
    end
    object Label8: TLabel
      Left = 12
      Top = 235
      Width = 26
      Height = 13
      Caption = 'Text'
    end
    object WidthEdit: TEdit
      Left = 12
      Top = 35
      Width = 65
      Height = 21
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnExit = WidthEditExit
      OnKeyDown = WidthEditKeyDown
    end
    object HeightEdit: TEdit
      Left = 83
      Top = 35
      Width = 65
      Height = 21
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnExit = HeightEditExit
      OnKeyDown = WidthEditKeyDown
    end
    object ColorEdit: TComboBox
      Left = 12
      Top = 75
      Width = 136
      Height = 21
      Font.Style = []
      ParentFont = False
      Sorted = True
      TabOrder = 2
      OnClick = ColorEditClick
      OnExit = ColorEditExit
      OnKeyDown = ColorEditKeyDown
    end
    object AlignEdit: TComboBox
      Left = 12
      Top = 119
      Width = 136
      Height = 21
      Style = csDropDownList
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = AlignEditClick
      OnExit = AlignEditExit
      Items.Strings = (
        'caLeft'
        'caCenter'
        'caRight')
    end
    object ShapeEdit: TComboBox
      Left = 12
      Top = 163
      Width = 136
      Height = 21
      Style = csDropDownList
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = ShapeEditClick
      OnExit = ShapeEditExit
      Items.Strings = (
        'shRectangle'
        'shRoundRect'
        'shEllipse'
        'shDiamond')
    end
    object Panel3: TPanel
      Left = 2
      Top = 276
      Width = 156
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      object MultiButton: TSpeedButton
        Left = 121
        Top = 3
        Width = 25
        Height = 25
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFDAD2CD7A51317A5030D9D2CCFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFCEC3B9E3E0DCB8A696C0AB99BAA58FB7A4
          95E4E1DECDC1B8FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFC7B9AD92
          6F529371538F6E50D9CCBED7C9B98E6C4F8D6B4D8A6648C5B6ABFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFAA917BBAA490F1ECE6E8E1D7E6DED3EBE4DBE8DF
          D5E1D7C9AF967FA68C78FF00FFFF00FFFF00FFFF00FFB5CBBE228150D5DBD59B
          7D62D0C1B3ECE6DC9A7A5EA38469EBE5DBC5B19E97785FFF00FFFF00FFFF00FF
          FF00FFB5CCBE0E784148C99D94BBA59C7C61D1C3B5ECE6DD937459997A5EE6DF
          D3C9B8A597775EFF00FFFF00FFFF00FFB5CDBF117A4340C69969D0ADA19077BE
          A794F3F0EAF0EBE5F2EDE7EBE4DAE7DED5EDE8E0B59D88A78F79FF00FFB5CEBF
          137C454BC79D46C79A90E0C4ADB5A19876589A795C957456E6DDD4E1D7CC9171
          53957456916E50C6B8ACB5CEBF147F4752C79F4FC79E7EDCBC2C86559FE2CBA2
          BCA6B5D9C9B2A491C9B7A7C8B6A5BBA898E5E2DFD0C5BCFF00FF006B2C83DCBE
          52C79F83DCBE23834FB5CDBF29855385DEBF5ACDA6A4CAB5865D3A855C3ADBD4
          CEFF00FFFF00FFFF00FFB5CFC023885193E3C8238650B5CEBFFF00FFB5CDBF22
          804E7CDCBB5CCEA787D8BC70AB8CC0D2C7FF00FFFF00FFFF00FFFF00FFB5CFC0
          006B2CB5CEC0FF00FFFF00FFFF00FFB5CDBF22814D78DCB93AC69735C5940B75
          3FB5CBBDFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFB5CDBF22804D78DCB93BC69771DCB7005C24FF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFB5CDBF22814E8AE3C5207F
          4BB5CBBEFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFB5CDBF006227B5CCBEFF00FFFF00FFFF00FF}
        ParentShowHint = False
        ShowHint = True
        OnClick = MultiButtonClick
      end
    end
    object IIEdit: TEdit
      Left = 12
      Top = 208
      Width = 65
      Height = 21
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnExit = IIEditExit
      OnKeyDown = WidthEditKeyDown
    end
    object IAEdit: TComboBox
      Left = 83
      Top = 208
      Width = 65
      Height = 21
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = IAEditExit
      OnExit = IAEditExit
      Items.Strings = (
        'iaNone'
        'iaLT'
        'iaLC'
        'iaLB'
        'iaRT'
        'iaRC'
        'iaRB'
        'iaTL'
        'iaTC'
        'iaTR'
        'iaBL'
        'iaBC'
        'iaBR')
    end
    object TTEdit: TEdit
      Left = 12
      Top = 250
      Width = 136
      Height = 21
      Font.Style = []
      ParentFont = False
      TabOrder = 8
      OnExit = TTEditExit
    end
  end
  object OKButton: TButton
    Left = 403
    Top = 332
    Width = 86
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 498
    Top = 332
    Width = 86
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
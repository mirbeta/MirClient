inherited dxTokenEditDemoForm: TdxTokenEditDemoForm
  Caption = 'ExpressEditors TokenEdit Demo'
  ClientHeight = 439
  ClientWidth = 647
  Constraints.MinHeight = 470
  Constraints.MinWidth = 405
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 647
    Height = 0
    AutoSize = False
    Visible = False
  end
  object gbMain: TcxGroupBox [1]
    Left = 0
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    TabOrder = 0
    Transparent = True
    Height = 439
    Width = 647
    object gbSample: TcxGroupBox
      AlignWithMargins = True
      Left = 5
      Top = 5
      Align = alClient
      Caption = 'Sample'
      TabOrder = 0
      Transparent = True
      Height = 429
      Width = 345
      object dxTokenEdit: TdxTokenEdit
        AlignWithMargins = True
        Left = 16
        Top = 21
        Margins.Left = 14
        Margins.Right = 14
        Align = alTop
        ParentShowHint = False
        Properties.Images = ilSmall
        Properties.ImmediatePost = True
        Properties.Tokens = <
          item
            DisplayText = 'January'
            ImageIndex = 0
            Text = 'Jan'
          end
          item
            DisplayText = 'February'
            ImageIndex = 0
            Text = 'Feb'
          end
          item
            DisplayText = 'March'
            ImageIndex = 1
            Text = 'Mar'
          end
          item
            DisplayText = 'April'
            ImageIndex = 1
            Text = 'Apr'
          end
          item
            DisplayText = 'May'
            ImageIndex = 1
            Text = 'May'
          end
          item
            DisplayText = 'June'
            ImageIndex = 2
            Text = 'Jun'
          end
          item
            DisplayText = 'July'
            ImageIndex = 2
            Text = 'Jul'
          end
          item
            DisplayText = 'August'
            ImageIndex = 2
            Text = 'Aug'
          end
          item
            DisplayText = 'September'
            ImageIndex = 3
            Text = 'Sep'
          end
          item
            DisplayText = 'October'
            ImageIndex = 3
            Text = 'Oct'
          end
          item
            DisplayText = 'November'
            ImageIndex = 3
            Text = 'Nov'
          end
          item
            DisplayText = 'December'
            ImageIndex = 0
            Text = 'Dec'
          end>
        Properties.ValidateOnEnter = True
        Properties.ValidationOptions = [evoShowErrorIcon, evoAllowLoseFocus]
        Properties.OnEditValueChanged = dxTokenEditPropertiesEditValueChanged
        Properties.OnTokenClick = dxTokenEditPropertiesTokenClick
        Properties.OnTokenDelete = dxTokenEditPropertiesTokenDelete
        Properties.OnTokenGlyphClick = dxTokenEditPropertiesTokenGlyphClick
        Properties.OnValidate = dxTokenEditPropertiesValidate
        ShowHint = True
        TabOrder = 0
        Text = 'Jan; May'
        Width = 313
      end
      object lbEditValue: TcxLabel
        AlignWithMargins = True
        Left = 16
        Top = 52
        Margins.Left = 14
        Margins.Right = 14
        Margins.Bottom = 14
        Align = alClient
        Caption = 'Edit Value: Jan;May'
        Properties.WordWrap = True
        Transparent = True
        Width = 313
      end
    end
    object gbOptions: TcxGroupBox
      AlignWithMargins = True
      Left = 356
      Top = 5
      Align = alRight
      Caption = 'Options'
      TabOrder = 1
      Transparent = True
      Height = 429
      Width = 286
      object lbCloseGlyphPosition: TcxLabel
        Left = 14
        Top = 19
        Caption = 'Close Glyph Position'
        Transparent = True
      end
      object cbCloseGlyphPosition: TcxComboBox
        Left = 147
        Top = 17
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'None'
          'Left'
          'Right')
        Properties.OnEditValueChanged = cbCloseGlyphPositionPropertiesEditValueChanged
        TabOrder = 1
        Text = 'Right'
        Width = 121
      end
      object lbGlyphPosition: TcxLabel
        Left = 14
        Top = 46
        Caption = 'Glyph Position'
        Transparent = True
      end
      object cbGlyphPosition: TcxComboBox
        Left = 147
        Top = 44
        Properties.DropDownListStyle = lsFixedList
        Properties.Items.Strings = (
          'None'
          'Left'
          'Right')
        Properties.OnEditValueChanged = cbGlyphPositionPropertiesEditValueChanged
        TabOrder = 3
        Text = 'Left'
        Width = 121
      end
      object lbEditValueDelimiter: TcxLabel
        Left = 14
        Top = 72
        Caption = 'Edit Value Delimiter'
        Transparent = True
      end
      object teEditValueDelimiter: TcxTextEdit
        Left = 147
        Top = 71
        Properties.MaxLength = 1
        Properties.OnEditValueChanged = teEditValueDelimiterPropertiesEditValueChanged
        TabOrder = 5
        Text = ';'
        Width = 121
      end
      object lbInputDelimiters: TcxLabel
        Left = 14
        Top = 99
        Caption = 'Input Delimiters'
        Transparent = True
      end
      object teInputDelimiters: TcxTextEdit
        Left = 147
        Top = 98
        Properties.OnEditValueChanged = teInputDelimitersPropertiesEditValueChanged
        TabOrder = 7
        Text = ',;'
        Width = 121
      end
      object lbMaxLineCount: TcxLabel
        Left = 14
        Top = 126
        Caption = 'Max Line Count'
        Transparent = True
      end
      object seMaxLineCount: TcxSpinEdit
        Left = 147
        Top = 125
        Properties.OnChange = seMaxLineCountPropertiesChange
        TabOrder = 9
        Width = 121
      end
      object chbReadOnly: TcxCheckBox
        Left = 14
        Top = 149
        Caption = 'Read Only'
        Properties.NullStyle = nssUnchecked
        Properties.OnEditValueChanged = chbReadOnlyPropertiesEditValueChanged
        TabOrder = 10
        Transparent = True
      end
      object chbAllowCustomTokens: TcxCheckBox
        Left = 14
        Top = 176
        Caption = 'Allow Custom Tokens'
        Properties.NullStyle = nssUnchecked
        Properties.OnEditValueChanged = chbAllowCustomTokensPropertiesEditValueChanged
        State = cbsChecked
        TabOrder = 11
        Transparent = True
      end
      object chbConfirmTokenDeletion: TcxCheckBox
        Left = 14
        Top = 203
        Caption = 'Confirm Token Deletion'
        Properties.NullStyle = nssUnchecked
        TabOrder = 12
        Transparent = True
      end
      object chgbLookup: TdxCheckGroupBox
        Left = 10
        Top = 257
        Caption = 'Lookup'
        Properties.OnEditValueChanged = chgbLookupPropertiesEditValueChanged
        Style.TransparentBorder = True
        TabOrder = 13
        Transparent = True
        Height = 157
        Width = 266
        object chbLookupSorted: TcxCheckBox
          Left = 7
          Top = 127
          Caption = 'Sorted'
          Properties.NullStyle = nssUnchecked
          Properties.OnEditValueChanged = chbLookupSortedPropertiesEditValueChanged
          State = cbsChecked
          TabOrder = 0
          Transparent = True
        end
        object lbLookupDropDownRows: TcxLabel
          Left = 7
          Top = 22
          Caption = 'Drop Down Row Count'
          Transparent = True
        end
        object seLookupDropDownRows: TcxSpinEdit
          Left = 137
          Top = 21
          Properties.OnChange = seLookupDropDownRowsPropertiesChange
          TabOrder = 2
          Value = 10
          Width = 121
        end
        object lbLookupFilterMode: TcxLabel
          Left = 7
          Top = 50
          Caption = 'Filter Mode'
          Transparent = True
        end
        object cbLookupFilterMode: TcxComboBox
          Left = 137
          Top = 48
          Properties.DropDownListStyle = lsFixedList
          Properties.Items.Strings = (
            'Starts With'
            'Contains')
          Properties.OnEditValueChanged = cbLookupFilterModePropertiesEditValueChanged
          TabOrder = 4
          Text = 'Contains'
          Width = 121
        end
        object lbLookupFilterSources: TcxLabel
          Left = 7
          Top = 77
          Caption = 'Filter Sources'
          Transparent = True
        end
        object chcbLookupFilterSources: TcxCheckComboBox
          Left = 137
          Top = 75
          Properties.Items = <
            item
              Description = 'Text'
            end
            item
              Description = 'Display Text'
            end>
          Properties.OnEditValueChanged = chcbLookupFilterSourcesPropertiesEditValueChanged
          EditValue = 3
          TabOrder = 6
          Width = 121
        end
        object cbDisplayMask: TcxComboBox
          Left = 137
          Top = 102
          Properties.Items.Strings = (
            '')
          Properties.OnEditValueChanged = cbDisplayMaskPropertiesEditValueChanged
          TabOrder = 7
          Width = 121
        end
        object lbDisplayMask: TcxLabel
          Left = 7
          Top = 103
          Caption = 'Display Mask'
          Transparent = True
        end
      end
      object chbPostOnFocusLeave: TcxCheckBox
        Left = 14
        Top = 230
        Caption = 'Post On Focus Leave'
        Properties.NullStyle = nssUnchecked
        Properties.OnEditValueChanged = chbPostOnFocusLeavePropertiesEditValueChanged
        TabOrder = 14
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 16
    Top = 0
  end
  object ilSmall: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 72
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000001000000070000000D0000001100000012000000110000
          000D000000070000000100000000000000000000000000000000000000000000
          0001000000040000000D3F090069871300C2B01D00EFC02000FFAE1D00EF8413
          00C23E09006A0000000E00000004000000010000000000000000000000010000
          000511020027891700BCE02F00FFF34B17FFF65928FFF95D2DFFF55827FFF04B
          18FFDA2E00FF851600BD10010029000000050000000100000000000000041E04
          0035C22900EBF54E13FFFA5E2EFFF65D2EFFF3562BFFF0562CFFF1572DFFF45A
          2AFFF8562AFFF1430DFFB92300EC1302002B000000040000000003000010B62B
          00D7FE5418FFFF6833FFF95F2FFFF65D2EFFF55C2DFFF55B2BFFF4572CFFF356
          2BFFF4572CFFF95D2DFFF44F16FF8E1B00BB0000000D000000015512006CFF4F
          09FFFF733BFFFF652FFFFD642FFFFA5F30FFFA5F30FFF95F2FFFF95F2FFFF95D
          2DFFF55C2DFFF85C2CFFFD642FFFF84202FF480E006900000005AD2D00BDFF70
          31FFFF733BFFFF6935FFFF6833FFFF6731FFFF6731FFFD6631FFFD642FFFFD60
          2FFFFD5E2DFFFA5F30FFFF652FFFFF6123FF9D2500BF0000000AEA4200EDFF82
          45FFFF753FFFFF7139FFFF7037FFFF7037FFFF7037FFFF6935FFFF6935FFFF68
          33FFFF6833FFFF6731FFFF6833FFFF6E35FFD63300EB0000000BFF590FFFFF94
          53FFFF7B41FFFF7B41FFFF7A3FFFFF743DFFFF743DFFFF733BFFFF733BFFFF71
          39FFFF6C39FFFF7037FFFF6E35FFFF8549FFF34500FA0000000BEC5715ECFFAA
          65FFFF8549FFFF8447FFFF7E45FFFF7D43FFFF7B41FFFF7B41FFFF7B41FFFF7A
          3FFFFF743DFFFF743DFFFF783DFFFF9B59FFE94200EA0000000ABA4A12BAFFB0
          69FFFF9053FFFF884DFFFF864BFFFF864BFFFF8549FFFF8447FFFF8245FFFF7E
          45FFFF7D43FFFF7D43FFFF8447FFFFA45BFFBC3B02BC0000000762280862FF91
          47FFFFBB79FFFF9053FFFF8D4FFFFF8D4FFFFF894FFFFF884DFFFF864BFFFF86
          4BFFFF864BFFFF864BFFFFB171FFFF803BFF601D00620000000303000009D060
          23D0FFB873FFFFC17FFFFF9657FFFF9053FFFF9053FFFF9053FFFF8F51FFFF8D
          4FFFFF8F51FFFFBB79FFFFB16BFFB54710B50000000700000001000000012A10
          022AEA712FEAFFB16BFFFFD395FFFFB573FFFF9F5FFFFF9555FFFF9F5FFFFFAF
          6FFFFFD293FFFFAF67FFE96829E91C0800200000000200000000000000000000
          00011A08001BB25725B2FF954FFFFFC683FFFFD99DFFFFE0A5FFFFD89BFFFFC5
          81FFFF934BFFB3531EB31908001C000000020000000000000000000000000000
          0000000000010000000350270E509B4E209BCC662FCCF97D3AF9CC662ECC9B4B
          1F9B51240B510000000400000001000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000001000000070000000D0000001100000012000000110000
          000D000000070000000100000000000000000000000000000000000000000000
          0001000000040000000D0E241B691E4D38C2256248EF296D51FF256247EF1D4D
          38C20D241A6A0000000E00000004000000010000000000000000000000010000
          0005040907271D4D3ABC328063FF46AC8BFF4EC19DFF51CAA5FF4DC19CFF43AB
          89FF317F61FF1D4938BD04090729000000050000000100000000000000040610
          0C35296A50EB42A181FF51C9A4FF4DC8A2FF4AC7A0FF47C59FFF49C6A0FF4CC7
          A1FF4FC8A2FF3F9F7EFF29674EEC040A082B000000040000000001010110235E
          49D747AA8AFF53CBA7FF4DC8A3FF4BC8A1FF4AC7A1FF4AC7A1FF49C6A0FF49C6
          9FFF4AC7A1FF50C9A3FF43A686FF1D4C3ABB0000000D00000001122C216C3F8E
          72FF57CDAAFF4FCAA4FF4DC9A4FF4DC9A4FF4DC9A3FF4CC8A3FF4CC8A3FF4BC8
          A2FF4AC7A1FF4BC8A2FF51CAA6FF3C8B6EFF0F271E6900000005245743BD52B4
          95FF56CDA9FF50CBA7FF51CBA6FF50CBA5FF4FCAA5FF4FC9A5FF4EC9A5FF4EC9
          A4FF4DC8A3FF4CC8A3FF50CAA4FF4DB091FF215240BF0000000A2E7258ED5CC9
          A9FF55CEAAFF53CDA9FF52CDA8FF52CCA8FF52CCA7FF51CBA7FF50CAA6FF4FCB
          A6FF4FCAA6FF4FCAA5FF50CBA5FF56C6A5FF2A6E55EB0000000B368165FF68D3
          B5FF56CEABFF56CEABFF55CDABFF55CDABFF54CDAAFF53CDAAFF53CCA8FF52CC
          A8FF51CCA8FF51CBA7FF51CAA7FF61CFB0FF2F7A5BFA0000000B34765DEC73D2
          B7FF59D0AEFF58CFADFF57CFADFF57CFADFF57CFABFF55CEACFF56CEABFF55CE
          ABFF54CDAAFF53CDA9FF54CEA9FF6DCFB2FF2F735AEA0000000A275F4ABA73C3
          ACFF60D2B2FF5AD0AFFF59D0AEFF59D0AEFF59D0AEFF58D0AEFF57CFACFF57CF
          ADFF56CEABFF56CEABFF5ACFADFF6FC0A8FF265B48BC0000000715302662539F
          86FF7DDDC4FF5DD3B2FF5BD2B0FF5BD2B0FF5BD1B0FF5AD1B0FF5AD0AFFF59D0
          AEFF59D0AEFF59D1ADFF79DAC0FF509C82FF132D236200000003010201092E6F
          59D077C2ACFF84DFC7FF60D3B4FF5DD2B2FF5DD3B2FF5DD2B1FF5CD2B1FF5CD1
          B0FF5DD2B1FF81DEC4FF74BFAAFF265C48B50000000700000001000000010914
          102A3B8069EA6FBAA3FF97E4D0FF78DCC1FF66D7B7FF5FD4B3FF65D6B6FF77DB
          BFFF94E3CFFF6DB8A2FF377B65E9060D0A200000000200000000000000000000
          0001050C091B2A604DB2509E84FF84CCB7FF9AE1CFFFA5EBDAFF9AE1CFFF82CC
          B7FF4F9D83FF295F4CB3050C091C000000020000000000000000000000000000
          00000000000100000003122A22502454439B31705ACC3D8B6DF931705ACC2454
          439B122A22510000000400000001000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000001000000070000000D0000001100000012000000110000
          000D000000070000000100000000000000000000000000000000000000000000
          0001000000040000000D100C3369231A6EC22A228EEF30249BFF29218EEF2118
          6DC20F0C326A0000000E00000004000000010000000000000000000000010000
          000504040D27231D6CBC3934ABFF414CC9FF4557D8FF475ADEFF4454D8FF3F48
          C8FF3530A9FF211A6BBD04030D29000000050000000100000000000000040807
          1735352C94EB444AC3FF4A5EDEFF455ADEFF4156DDFF3F53DCFF4054DDFF4257
          DCFF4657DCFF3E44C0FF2F2890EC05040F2B000000040000000001010210332D
          89D74C55CAFF5064E1FF485DDFFF4559DEFF4459DDFF4358DDFF4257DDFF4156
          DCFF4257DDFF485BDEFF434CC7FF25206FBB0000000D0000000119163F6C4E4D
          BBFF566BE3FF4E64E0FF4C61E0FF4A5FDFFF4A5EDFFF485EDFFF485DDFFF475C
          DEFF445ADEFF465BDEFF4C61DFFF4544B5FF151138690000000534317DBD5B66
          D4FF576CE2FF5167E3FF5065E2FF4F64E1FF4E63E1FF4D62E1FF4C61E0FF4B60
          E0FF4B5FDFFF4A5EDFFF4E61E0FF525ACFFF2D2876BF0000000A4844A6ED6273
          E1FF596DE4FF556BE3FF5469E2FF5469E2FF5368E2FF5167E3FF5166E2FF5065
          E2FF4F64E2FF4E63E1FF4F64E2FF5768DDFF3F379EEB0000000B5451BCFF6B81
          E7FF5B70E5FF5A70E5FF596FE4FF586DE4FF576DE4FF566BE3FF566BE3FF556A
          E2FF5368E4FF5368E2FF5267E1FF6276E5FF4945AFFA0000000B5151B0EC7C8D
          E8FF6177E6FF5F74E6FF5E73E6FF5C72E6FF5B70E5FF5B70E5FF5A6FE5FF596E
          E4FF586DE4FF576CE4FF586DE3FF7283E5FF4844A5EA0000000A42428BBA858F
          E2FF697EE9FF6378E8FF6277E8FF6277E7FF6176E7FF6075E6FF5E74E6FF5E73
          E6FF5D72E5FF5B70E6FF5F75E6FF7C87DDFF3C3A85BC00000007232347627277
          D3FF889BEFFF697EE8FF657BE9FF657AE8FF6578E8FF6379E8FF6378E7FF6278
          E7FF6176E8FF6278E7FF8194EEFF6C6ECEFF1F1E426200000003010102095152
          A2D08E97E3FF8FA0EFFF6C82E9FF687EE9FF687DE9FF677CEAFF667CEAFF657B
          E9FF687DE8FF899BEFFF8891E1FF414185B50000000700000001000000010F0F
          1D2A5F61BAEA8992E1FFA1B1F2FF8397EEFF7389EBFF6A80EAFF7286EBFF8194
          EDFF9FAEF2FF868EDFFF5A5AB8E9090913200000000200000000000000000000
          00010909111B494B8EB2777BD7FF98A3E9FFA9B5F2FFAFBEF5FFA7B5F2FF97A2
          E9FF7478D5FF45488CB30809111C000000020000000000000000000000000000
          0000000000010000000320213E5040427B9B5556A6CC686ACBF95456A5CC3F40
          7B9B1F203D510000000400000001000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000001000000070000000D0000001100000012000000110000
          000D000000070000000100000000000000000000000000000000000000000000
          0001000000040000000D0B1C3669183B73C21B4C93EF2152A2FF1B4B93EF1738
          71C20A1B356A0000000E00000004000000010000000000000000000000010000
          000503070E27183C72BC2F6AB3FF4B98D4FF58ADE4FF5DB5EBFF57ABE3FF4895
          D4FF2D67B1FF153870BD03070E2900000005000000010000000000000004050E
          183525589CEB468ECDFF60B8EBFF5BB6ECFF58B3EAFF54B1EAFF55B1EAFF57B3
          EAFF59B3EAFF4189CAFF1F5198EC0308102B0000000400000000010102102353
          8ED74F9BD5FF64BCEEFF5EB8ECFF5BB6ECFF59B5EBFF58B4EBFF57B4EBFF57B2
          EBFF57B3EAFF5CB5EBFF4894D1FF193E72BB0000000D000000011027416C4082
          C1FF6CC1EFFF64BCEDFF60BAEDFF5FB9EDFF5EB8ECFF5DB7ECFF5CB6ECFF5BB6
          ECFF59B5EBFF5BB5EBFF60B9EBFF3B79BCFF0E213A6900000005215281BD60A9
          DDFF6CC2F0FF66BEEFFF65BEEEFF63BCEFFF62BCEDFF61BBEEFF60BAEDFF5FB9
          EDFF5DB8ECFF5CB8ECFF61BAEDFF55A0D9FF1E497CBF0000000A2E70A9ED72C1
          EBFF6EC5F1FF6BC2F0FF6AC2F0FF68C0EFFF67BFEFFF66BEEFFF64BEEFFF63BD
          EEFF62BCEEFF61BBEDFF62BBEDFF65B7E9FF2963A1EB0000000B367FBFFF81CD
          F3FF70C6F1FF70C6F1FF6EC5F1FF6DC4F0FF6CC3F1FF6AC2F0FF69C1EFFF68C1
          F0FF67BFEFFF65BFEFFF64BDEEFF72C4F0FF2D72B1FA0000000B3578B2EC89CF
          F1FF77CCF3FF74C9F3FF73C9F2FF71C8F2FF70C7F2FF6FC5F1FF6EC5F1FF6DC4
          F1FF6BC3F1FF6BC2F0FF6BC2F0FF7EC6EDFF2E6CAAEA0000000A2A628DBA85C4
          E7FF7ED0F4FF79CDF4FF78CCF4FF77CBF4FF75CBF3FF74CAF3FF72C8F3FF72C7
          F2FF70C7F1FF6FC6F2FF73C7F2FF7BBCE4FF265A88BC00000007163347625CA4
          D6FF9CDDF9FF7FD2F5FF7DCFF5FF7BCFF5FF7ACEF4FF79CDF4FF78CCF4FF76CB
          F3FF75CAF2FF76CAF3FF91D6F6FF559CD0FF142E426200000003010202093375
          A2D088C6E7FFA2E1F9FF84D4F6FF7FD3F6FF7ED2F6FF7DD1F5FF7CD0F5FF7ACF
          F5FF7CCFF4FF9ADBF8FF82C1E4FF286088B50000000700000001000000010916
          1D2A3D8BBBEA7EC1E5FFB2E6F9FF98DEF8FF8AD8F7FF82D4F6FF89D6F6FF94DB
          F7FFADE3F9FF7ABCE2FF3A84B9E9060E13200000000200000000000000000000
          0001060D111B2D698EB259A9D9FF97D2EEFFB3E4F7FFC0ECFCFFB2E3F7FF95D0
          EDFF58A7D7FF2D668CB3050C111C000000020000000000000000000000000000
          00000000000100000003142E3E50285C7B9B367CA6CC4097CBF9367CA6CC285C
          7B9B142D3D510000000400000001000000000000000000000000}
      end>
  end
end

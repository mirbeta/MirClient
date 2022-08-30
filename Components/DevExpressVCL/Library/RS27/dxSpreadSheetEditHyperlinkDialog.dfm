object dxSpreadSheetEditHyperlinkDialogForm: TdxSpreadSheetEditHyperlinkDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Insert Hyperlink'
  ClientHeight = 335
  ClientWidth = 621
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 621
    Height = 335
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    HighlightRoot = False
    object btnOK: TcxButton
      Left = 435
      Top = 300
      Width = 85
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 11
    end
    object btnCancel: TcxButton
      Left = 526
      Top = 300
      Width = 85
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 12
    end
    object edtAddress: TcxButtonEdit
      Left = 311
      Top = 80
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
        end>
      Properties.OnButtonClick = edtAddressPropertiesButtonClick
      Properties.OnChange = edtAddressPropertiesChange
      Style.HotTrack = False
      TabOrder = 5
      Width = 300
    end
    object edtEmail: TcxTextEdit
      Left = 311
      Top = 107
      Properties.OnChange = edtEmailPropertiesChange
      Style.HotTrack = False
      TabOrder = 6
      OnExit = edtEmailExit
      Width = 300
    end
    object edtTextToDisplay: TcxTextEdit
      Left = 311
      Top = 26
      Properties.ReadOnly = False
      Properties.UseNullString = True
      Properties.OnChange = EditDisplayTextChanged
      Properties.OnEditValueChanged = EditDisplayTextChanged
      Style.HotTrack = False
      TabOrder = 3
      Width = 300
    end
    object edtSubject: TcxTextEdit
      Left = 311
      Top = 134
      Properties.OnChange = EditValueChanged
      Style.HotTrack = False
      TabOrder = 7
      Width = 300
    end
    object edtCellRef: TcxTextEdit
      Left = 311
      Top = 161
      Properties.OnChange = EditValueChanged
      Style.HotTrack = False
      TabOrder = 8
      Text = 'A1'
      Width = 300
    end
    object edtScreenTip: TcxTextEdit
      Left = 311
      Top = 53
      Properties.OnChange = edtScreenTipPropertiesChange
      Style.HotTrack = False
      TabOrder = 4
      Width = 300
    end
    object tvDocumentPlace: TcxTreeView
      Left = 311
      Top = 188
      Width = 300
      Height = 94
      TabOrder = 9
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      OnChange = tvDocumentPlaceChange
    end
    object btnRemoveLink: TcxButton
      Left = 344
      Top = 300
      Width = 85
      Height = 25
      Cancel = True
      Caption = '&Remove link'
      ModalResult = 3
      TabOrder = 10
    end
    object pnPlaceInDocument: TPanel
      Tag = 1
      Left = 10
      Top = 122
      Width = 125
      Height = 86
      BevelOuter = bvNone
      TabOrder = 1
      OnClick = NavbarItemClick
      OnMouseEnter = NavbarItemMouseEnter
      OnMouseLeave = NavbarItemMouseLeave
      object cxImage2: TcxImage
        Tag = 1
        Left = 0
        Top = 0
        TabStop = False
        Align = alTop
        Enabled = False
        ParentColor = True
        Picture.Data = {
          0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000020
          000000200806000000737A7AF4000000017352474200AECE1CE9000000046741
          4D410000B18F0BFC610500000016744558745469746C6500546578743B506167
          653B5265706F727476616100000006B94944415458479D97F753D45714C5B197
          C45ED3F3AF6526BF8468229AD811A5833411A4178D1A45349AD8A2284142D488
          665226338EA8F485651BBB6CA3DD9C73DFF7CBAEB03BC9E43BF399B7B477CEB9
          EFBEF77DA488484ADCB3A8A9F5E9FDE62BCFA4F9F23369B247D2FA549A5BCDD8
          08CCD82D8D97BAA5A1A55BEA31D6B73C518AEBDA3331D752B09873963575A594
          3576A69482F847B5E30C2C028B29F85F9F68744A264251F18D8764D4E597DE01
          8FD45CF8557EEEEE95BCCADB59986F19E7E4DC239E484A614D3B3EC61E4CB1C0
          C012A6E5333D3D2BD33316D3331867640ADF2393F89A8423C680D71F96115740
          5EF6BBE5E4995F647656E44EE70BC928BE9E8D39970335915D7E1743EC496460
          294B6B0CD8A2065B74720A06A666246A118E4C4A201815972708032E29AAEF50
          0381D094DCEA782EFB73AFE460DE156089A531F72432B0AC016BC9C7249D8508
          52530CE2148C58C29128C4A3D318A751854971F942D2830AE454B4A981107E4E
          13D7DBFE96B48C0BB9987B2578C344620368223E4C4928A889272D618E203209
          71102630E11E0FCB8B5E97A417DF5403AC54584D4CCBD53B7F49EAFE330B4C24
          32B0BCEE629C010A61F2A88AD18C1155617CDF268466F4FA23D2D3EB96BDB9D7
          F4EF690245D32AF277AEDCFA5D3EDDD34013764F2C30C06FAEA8B9600C686210
          132133128A6044F3710C92F01498169F3F2A7D0E9F6497DF962F335A65E7914B
          B233FD92EC384C5A94E28607145B0D5885840656569F7F6C19B0D25A0662468C
          B86D82237B20009CDE90BC1AF0CA1FCF1DF2E4CF7E79F45B1FB6E46B697FD423
          0F1EF748516D07C5D680E406AACE3D5203F6FADA49439A145010A3822663A371
          2B72F40522DA8C4EEC88E1B1091918F6C9EB41AFF6C6CB3E971456B7536C2DE0
          2195D0C0AACAB3C6802939054D4A5B58478A8627E78C04082AE0C776E4388ED1
          3781AD8903CAE9851967409CEE80E457DFA3D83A90D4C0EA0A1E2434A0C98D40
          1DB6662D8EDB5A34284FBA6AF2ED63A9C2729D0295A85AC5D98772F29B87528E
          BF3F71FA21E8D28AB8714871697858E556A881F520A101AECB5BE5A77F560373
          E5465AA767421975730CCA883B04261487CB304C50F6218C434E12807844BC01
          56222C0154042721C53680A406DE2E6B820138A0012D2F0C68D273488AE5A920
          486AD222291337774919286DEA9212FC7D7163273ABE53D37B608204B16459E5
          3F526C23486A604D29B60A0DE8FA5AEBAA69ADA40E9CF9C3638489312229D30E
          8CDAF8A5DF82C929EE1E8FE80E3A567A9B629B005F52090DAC2D8E3BCBFD5643
          31A9595B939494349258DAE3F50F94C23A50FB400A6A3A64CC175671971A9892
          8C925B14DB0C6860D17C032CCBDAE3353FC1C02C92531C60ED4CDA38748DCD3A
          0F123BFD885F2BC0911570C2800B8CF922BA93D28B6E506C0B486A601DF72A0D
          D8E26C22AE6B8995941431AD26ED900282B4F9309E576DC8AD6A979C53ED6A80
          55A00136F4C1C27F37B03EBFAA4D6660C007612F0D600D87B0D69AD4E93730A5
          B5DE9A9A891D7EE91B193738885F46B0FD9C3C9860823D75A0E03AC5B602BE0F
          121AD8905371D718B0D2BB60C0ACAB495A80A4F956D2F8B439951CEF4B76E57D
          C9C27E270E7750463DB8AC78427A40EDCBFD9E62DB4052031BB34F18031E1C22
          ECE03174F25C67235D3FD2F631314724ED4552F21A9F9561E29357181D2E9C19
          10570358D2AF73AE516C3B486880EBB229135B6506D7307BFBD0405E95499A8B
          A484294DD258DACC93A44D32CBEFC9B1F23665D832E0C0C1C5A6DE93F51DC5DE
          01490D6C3E8AAD4203F6F6318DC4970CCCE033BF56BC6119B5182156525B900C
          8D05CD67186143EFCE5403EF025ED1DE30A0B721B0E548D14DBD88F210617A0A
          D00005B5A1D858C008C78972B484872DD1416C555681F8262665D7D1CB147B0F
          2435B0F5F0F11B6AC016E5A42A0E343184C8C2C426EDB08B04F14E08CA00CF0A
          5461109F3D68685E54A0F13E486880EBB28D5B851752939606383153BC39B9C2
          C971300D72749ACF141D18B5C1D6D5DF09EA8B6967BA1AF8002435B07D5FDE0F
          6A40536A6A2BA9A69C9F16C6D45C9C2985A662C606801B57B61DE92D14FB1024
          37B0175B85FF072C2C31042D31039A8C6210E13827688B6A35CC32103674EAA1
          8BB601DE8E131AD8FA55D655FD07C4BE05D9AF65DE80F882B25F52B1778539B2
          ED834B5FBF7A11C1CE21DC4DBA93C2927AE002C59236A19E843B0E9FEF48C376
          D97DEC8AA4815D47C965D99571599BE88B2386B95BAF0D6EBD9F1F02872F4AEA
          C1189F41D4E693B4864E68247D1BEAA514F0C6C2D38A4ED9B16C9AF9B08CF17C
          94808FE3E0D79C8BE2AB00B58C769C0156813F6025B81C2CD37C68F0FFC2BF67
          72DE3BA8056D49F90754CA858B8828C1430000000049454E44AE426082}
        Properties.GraphicClassName = 'TdxSmartImage'
        Properties.ReadOnly = True
        Style.BorderStyle = ebsNone
        Style.HotTrack = False
        StyleFocused.BorderStyle = ebsNone
        TabOrder = 0
        Transparent = True
        OnClick = NavbarItemClick
        OnMouseEnter = NavbarItemMouseEnter
        OnMouseLeave = NavbarItemMouseLeave
        Height = 48
        Width = 125
      end
      object lbPlaceInDocument: TcxLabel
        Tag = 1
        Left = 0
        Top = 48
        Align = alClient
        AutoSize = False
        Caption = '&Place in This'#13#10'Document'
        Style.HotTrack = True
        StyleFocused.BorderStyle = ebsNone
        StyleFocused.TextStyle = [fsUnderline]
        StyleHot.BorderStyle = ebsNone
        StyleHot.TextStyle = [fsUnderline]
        Properties.Alignment.Horz = taCenter
        Properties.Alignment.Vert = taVCenter
        Properties.WordWrap = True
        OnClick = NavbarItemClick
        OnMouseEnter = NavbarItemMouseEnter
        OnMouseLeave = NavbarItemMouseLeave
        Height = 38
        Width = 125
        AnchorX = 63
        AnchorY = 67
      end
    end
    object pnFileOrWebPage: TPanel
      Left = 10
      Top = 30
      Width = 125
      Height = 86
      BevelOuter = bvNone
      TabOrder = 0
      OnClick = NavbarItemClick
      OnMouseEnter = NavbarItemMouseEnter
      OnMouseLeave = NavbarItemMouseLeave
      object cxImage1: TcxImage
        Left = 0
        Top = 0
        TabStop = False
        Align = alTop
        Enabled = False
        ParentColor = True
        Picture.Data = {
          0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000020
          000000200806000000737A7AF4000000017352474200AECE1CE9000000046741
          4D410000B18F0BFC610500000013744558745469746C65005765623B576F726C
          643B4579650744383A00000B724944415458478D97075494C71A867FAFB1A126
          2637D71863CC51935C9313638CA2C1861AB10052A477105C1B45E96559840591
          45940541408A14414061A98A80748248918EB8084A918EB4202A79EFCC464C6C
          F7DEEF9CE7CCEECCFCF3BEF34DF97719754E32A3CD4D63F44FC5313A4E268C2E
          5B96D17792630C5D1418D62945C6D8CB8CB10D4964484C7B1FECC852C6EE5209
          63E2EDC11CF174664CFC729923DEB7988367321983D33744E3539DA900F00A46
          DA2E9ED17675651C420F3237AB3C99E68128A6F5491CD3367C8DF69D12F90761
          3AE1837740EB693B655A560D9FB1BEE8C068B825300A9C24669F838091B14B60
          D49DDE63E088D74126E4A63553DEE6CD14B4D830390F8C699F29E1E946273689
          C5DDB0928F4CB10966076857589CB7E870894983DBD5BB1DDCB8F20A4E5449C8
          519EBD82B4CA8679A43F352432C20E3BCEA8716318797612236B9FC8A89F4C21
          D57FC66B06B26A5D99EC262B26A5CE80C96862D176D18C374A2D9D7D41A0702C
          3C53B52BA5CC08252D1C3C188844754704426E9923BF7D083784BDB89893059B
          0BDA700AD2EC0A4E55365E2FF9E51CF23CCDCA34E7706346DD2598D9470DBC2F
          03B1E5EA4C7C853A935CAB4BDB44B3760ED8B6223055BE2ABE5807A51D36A81F
          E2A2733C01BDA375189B7882C93F263131F907FA9E3E47DDC0186EB67621342F
          1DA7A2341196A152EDE8B3E35B32CE0CC23F4E471F63345C3CDF6F20B2548189
          AB50A5F554FC83938192127E02E9AED4BB0751D1E380FA414F8C3E6FC6B31763
          9878F114CF265FE03911A73C7B49DFF83364B60C809FCA464AC55184DED8DFED
          12B06333194F6442CDF92CA346F6C354BC6620BC448ED689667ECC75EDF2B371
          3B7B53AB0C51D26185CA1E278C3E6B21224F30F6741063CF9E62FCC5E4EB3C9F
          C4EF84BEDF9F21B6B20A7EE926B8D564024E9074DF36B5EDDF9071A989692A64
          2F4CC56B064888D67C93F4E2F9DC882DD5D145AAC86D3141719B051E0E251181
          2E22D44DCA3EF48FF462E4D98BBF9878816111CF314496E3F6C34708B86E81C4
          F2E338EEC7852E37B166C51AA905647CD19EA06234DE656086CD85750E670552
          B871CF08D9CD4751F0F038FAC6AAD03DD48CB65E21DAFB84E4732F06C69FBF46
          3F99399D7DEFD8041ADAAA51D47C0EAEB15C786536C2E86C16946DA33964FC59
          047A3A44F1A681E91B65167D627751BCFBF26D65A43518E066130BF1778E40D8
          5983D6BE6E740C3F4527E1F10861F429BA4626C867CA9FF5B4FDD1C013343FAE
          864FCA3178A4C4C026481F26FE7C90B5EFFB5A5CE65F44871E515116DE3430E3
          A0DB773A2E9725105FA986A41A1D44141E00379687AA8E3E08FBC6D0DC3F8696
          81DFD13AF836B4FE013909B45FE5A3769CCDC80627EA24D22B5D11986E0E05D3
          DDD8A2B6519FE8D02CBCD3C02C166F65342F712BA2EF28E1DA5D0D5806E8E15C
          8E1069F55DA8E91C465DD730EABB292368E8A18CBE2C474475B4BDE6F190A8FF
          E9D434F005A648A93E0C41F91172228C607166FB15A22346102DC39B06E618B9
          7F536F15B0072C9E1A8CCF69E1B8AF0E7899F7E09327449EB00FA58F06514669
          1B4479FB935794B53DC11D5257FA7040D48FF6B70D73C0D5520B441629C32F43
          1A9C4809C8992E69223AF3097433BE157315CCD60CA93ABB418F9F0BD3F05238
          2454819B568FD33789895C21D2EB1E8B04F29BFB50F012FA398FDC84B9F77B91
          56D309FEAD269CCAA827E60D1159AC07BF9B7B71FA9A246C827F86146BE130D1
          F98840F7C15B314FDEF4173885EB43DBCD0907030A7122AA0CECC46AB810136E
          371AE1418C0415B520AEA21D69B58F458652AA3B905AD38E5452A655B5E1665D
          07B2EB3B90529880DF1A2F23FB6E00AE15B8C14FC08296F5069AEB4F08F44E78
          2BE64B1A3A0C9BF1F7C1C85D19DA5EC938145484E397CB617BF52E3849B57049
          AD07F77A035CD3A9A10604E50B71A3A6033944B0B0200BA5A9A1A888E1A232C4
          1C65E70FE0B773BAC8F5D246BA9F1112222C703ECA7982E82C26D0F784E86515
          BAF72B52FC19F32559614D4AD6FA30F55684AA6B00B4BD73C00A2C8209590ECB
          2B95B0BD5605B6A0064EC9D5B852DA8AACDA76E46726A122C21AADC94EE82DF4
          C1484D38C61BA230D11081D1CA00F4E5B843186D8692D38A883F2231E9BC73A9
          35D15A48984D78ED629A2BA1177055FA843BC9802C941CEDA0EC9E012DCFCBD0
          3B6586031E2770342811E6D165882A14E27AF97DE487394218730203A5FEC8B9
          C203D7C6183ACAAAD82DB91BE26BB6417697028CB4F4E16663865B616CB42558
          20CFFE5704CA2DCF58FD99189D3A3D11D355F56C450666FFACE46EF8EBB1CB90
          B10D80BCAD1D54EC5470D8733FCE25ABC1274D13667C55F062639158DC886C5F
          53B425DB2331880B4D0D23A8A81FC55E2513482998415AD51A8A7A4ED8A36A05
          49E923D8BAC708EABAD6303232C735BE156AF82A08945D5EB46CC1AC2FA92E41
          742C67FC73D9FAC512FA21FDFB1C92B0D3FA1A14ADB5E091B017EE82CDE0256F
          45488E36326E5F46B2AF19EE04B2606F6D0F2BFB33888E4F87312704DCF3F148
          2ABA8BD0A402F02E2481EB7D15279C43A074C005DB15CC1118268015DB07B6E6
          56C874D80B0FC92F8289EEA78499D4005D8F796B9478AE3B8E5E8682833FF4B9
          BB617DE9475886FD00AB4BAB70F1C6616464F3906E2305734B175C4DCC42755D
          3342E37271981D8CA2C6569477F4A2AC6700329A16D030E2C0F7523AB8FC38C8
          EA7060ED7211F9BFD520243215C74C1C11AAFED3A4DACA05524497DE0DA2CD30
          73DEC26F3E5FA779E19E9C7530744FCA82C55F81433E5F8BCAABF93CC47BEAC0
          DCD40EF9C59578D4D183DE8121B80524C3D22D02991542D40D8DC25F9009895D
          0658BB55030A5A16088ECD85092718F27A8EA8BBD78EB22A2112D30B71F8A005
          385B96D1DB916E4A91019A05B1A5E25A6BD76905F6ABDAB9C0C07D2BB44F2D81
          96DB9788CBF682034B0F571232D1DD3788C1E1718C8C3D85954734D867621091
          5884BD6A2720B1DB00E2DBB5B17A930A7E10974384A0180E9ED1D8A66801E1C3
          1ED4DDEF44794D0BFC430538AAA83442349732FA676F4D99A0B7D4BCAFB799CA
          ACD5F4EF55B260E198D76668382F41D4754FE81E8FC1E39E2764E623181819C7
          D0E838EC3CAFC0D2359CCC340F9A2C0E7E26335FBD5119DF13F15DF22C84C4E5
          C3D42918723A6C343CE826061EA3B2B11DA5952DD8A1E48359733E5EC1187AE7
          8AF89B89F95FFCA4B2FE2765AF86ED7A9A3074DD0441DE39D8F2429178BD166D
          5D43E8EA1F2646C68870360C2C7CE119988CC0E84C2892B47FBF565624EE179E
          0E5E5032E4C8A970740F47CDBD4E54D4B6A1A4B2153E2179582725FAA3B09861
          F9E689786960CAC4DC99629F7CFEEF9D0739AB767FDB6775460D9712CEC0981D
          83928A5692CE5E3CEC1840536B0F31E083C30E17C0232682AFE4215250848B24
          2354DCD0920F29256B14970B515AD58AFCDBF79170FD2E76AAF21F2F58B87203
          D1F98839EC57F08A970628744FD0F7F747733E145BBA6CC33C96315BAFD0D1C3
          B9FFB07DDC1FA959F5A86EEC44BDB00B19795550347485EA21776230086CCF18
          1CB50FC03E5D276C9133438CA0108565CDC82E6A44484C3164B5FD07177DB549
          868C4D37E04C6695B2FF5B90A026E82541B3412F8C0F098B082B972C175796D7
          3FDF78CA371B99058D24A52DC82AA807DB2312FBC9B9979031C53E1D47587082
          20C8A8C04DD2169F5A0673E7446C94F3A8FF6CE9FABD2FC77A7511FDB7F87B46
          E8A541FFFD2C9A396BEE8F5BA56D7C150D43FB2DB9C9B8105180E8C43B48CAAC
          C2F59C5A92E64A84C516C32B301B876C63B143853FB07A8BF1F90F6688AD22CF
          D39F6722F18DFBBC49F1FFC59411EA981AA1EFF62562F33E5D23BEFD88C38EFD
          EE79BBD4F80FF668058E4A6B07619786DFE8B6FD9E0F36CA9CCCFFE11703C7D9
          621FAF23FDE9F54B9FA3CF4FDB2CCF6736C911037FFF79F426EF89A98CD03D42
          97E633C232C24A029DE1EA97E57784E584CF095498CE9A2E297DFEAF7897F014
          FF23A68CD01F197470FA869B4BA0CB444BFA9DD6D376DAEF75615130CC7F0066
          0D140302F079750000000049454E44AE426082}
        Properties.GraphicClassName = 'TdxSmartImage'
        Properties.ReadOnly = True
        Style.BorderStyle = ebsNone
        Style.HotTrack = False
        StyleFocused.BorderStyle = ebsNone
        TabOrder = 0
        Transparent = True
        OnClick = NavbarItemClick
        OnMouseEnter = NavbarItemMouseEnter
        OnMouseLeave = NavbarItemMouseLeave
        Height = 48
        Width = 125
      end
      object lbFileOrWebPage: TcxLabel
        Left = 0
        Top = 48
        Align = alClient
        AutoSize = False
        Caption = 'Existing &File or'#13#10'Web Page'
        Style.HotTrack = True
        StyleFocused.BorderStyle = ebsNone
        StyleFocused.TextStyle = [fsUnderline]
        StyleHot.BorderStyle = ebsNone
        StyleHot.TextStyle = [fsUnderline]
        Properties.Alignment.Horz = taCenter
        Properties.Alignment.Vert = taVCenter
        Properties.WordWrap = True
        OnClick = NavbarItemClick
        OnMouseEnter = NavbarItemMouseEnter
        OnMouseLeave = NavbarItemMouseLeave
        Height = 38
        Width = 125
        AnchorX = 63
        AnchorY = 67
      end
    end
    object pnEMail: TPanel
      Tag = 2
      Left = 10
      Top = 214
      Width = 125
      Height = 78
      BevelOuter = bvNone
      TabOrder = 2
      OnClick = NavbarItemClick
      OnMouseEnter = NavbarItemMouseEnter
      OnMouseLeave = NavbarItemMouseLeave
      object cxImage3: TcxImage
        Tag = 2
        Left = 0
        Top = 0
        TabStop = False
        Align = alTop
        Enabled = False
        ParentColor = True
        Picture.Data = {
          0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000020
          000000200806000000737A7AF4000000017352474200AECE1CE9000000046741
          4D410000B18F0BFC61050000001E744558745469746C65004C65747465723B65
          2D6D61696C3B656D61696C3B6D61696C1A9193DA000006E2494441545847C596
          897353D51EC7ABCF05C5EDE1BEBCF777B8F07C6EA3A383CB38A3826F9E5261B0
          80CAD245109952B50E28082508585A11898502056C430BB6B4B448D7B449D3BD
          29A44DD226699BA569D374E5EBEF7B6E53E3B3F21847F1CE7C72CE3DF79CEFEF
          7B4E7EE7DC1B03E02F65C6C62BC98C8D579298633FB4C4EC395019937FC61A23
          D7557F02574731DD9E71B04A0AB96840AE48C7BF09D74C71EDFFE1BA4B70FD25
          E058C661CC6903571F32D43E9977BAA5C650D20E92573CC5E9B669724951ABE2
          7BE178216951888EE2E829D2ACC821279B14470A48230E1AEA8DE9FA92A72526
          27A94CF0E79AFDC74C6E77DF2026262715172F5E94BFE88FB926456B7C7C02A3
          6313E8EA09606B46A947627235B8EACAC0751F7C6EC09874189F984478740CE1
          91311930AECCFCDE6B52C652837A2302B543C3A348FA3457B22FE6C68801FE5C
          BF29E347141435A2B1CDAD3A8E8C8E232426C870C48CB45F6A65F88C7D38114E
          80E3387E44C68E8F4FA2AED1097D7605527614D1C06C81B9A00CCCDABB2B0FEE
          DC5C9C2A6B85FE682D02C1B05AB6D0C82886C47528AC313C8D98129384C1C272
          CFF6483F858CE564FC03217CA5FF11C70A2CB06E4BC3975B0ED3C04DD1066E38
          93900867FA6EF98FFCA8B1D8B17977312A6A6D1813139CC9A09888404391924B
          CA32BA8DE5B018E3D8B2CA0E6CD8624059955569B7C4AF46C1B27769E0E66803
          3716AF8E47A0D30E873B80AE6E1F5ACEBBB13DB3043AC1EB1B526283A11104A3
          88DC6B6558AB4B70F6EDF30E62A3EE0452B7E7C368E9529A4ED1EE6D6886E1ED
          E534708BC09DA05CCCDEB53507AEBEA074F40B01743A7C68B3F562FF912A247D
          9C83C2B32D2A8B39C381C1B0C6D0889444BBE733E64AFE690BDE5BF71DB6EF2D
          86A5A51B36A70F76C97EBBE85ABBFAB139554F03B7451BB8697D5A214E55DA54
          674203367B3FACB63E141437E083D42348DE9C0B8F98A411060F4850E60AEB6C
          73790248D87000EF7F948D7DD9E5686E778B8657D3724829BABB0FD722E1D3BC
          5F19B879C1BAE3C83F77011D5D7D382F2E3BEC7D529752EEDB65250C45CD884F
          C941EC8A4C7C7FCAAC7649E4BF673DC75083F94B74484839828C0312DCEA110D
          6D3C75A8795EEE77661BF1F2EA83343027DAC02DAFADC9514BDF2E335674FE5C
          1A1B64196516DD6E3F4E149A91B2E930E2930FAA193B5D3EC42564627D6A1672
          4FD6C1D46087B5D3ABC64C6B08D6A97A53472F5E5C914503B70B3C96958B5B13
          379E80A7D98AD60ECF2F30D63B505DEF54891592ADE6F3875056D1860C7D1116
          AD4CC78225DB90B9BF083F9436A2BED92909C93C98904062DCE2F8959EC3D880
          E5C94769E08E6803B76D4AF916D68F3E4153BB4B702B6A44A0A442B6E254F291
          D0B06C49F9CFCD4D0E14965A5054DA8073C60E995D2F8624380D04873413D526
          A79A40531B350529EB97BE83E4A4741AB833DAC0DFB35F8F457BF206C9DA1E58
          5ABB65D6769C2C6D5742140C4A508A33C89098A0192655BD64394BDE6BC1B59D
          C1E40C4B6E9456D9506DB68B26757B608E7B077B5F9A4F0377097C732A0373B2
          5E7B034E730B4C725CD6D477E190C1A2827965C92916E47613F18811EEF74819
          6439F58CDBD12FFD7D03C3E8F78594469E24708DB90BA626276C956664CE7B85
          06EE8E18E032DCBE61CD1E952095E64E64E59AD416EB760FC029FBB7C733004F
          FF20BC8161111FD6F6FE54400D9E032372E486551F8F77083DBD41358ED0544E
          7E03AA4C9DF23EE8C6DA953B69E09E680377C4AD3B84AF8F99B12FC7089F88F0
          D4E2E9E590E3D3E90AA09B62BD03EAB0728B1906E9931912D609DBF94AEFF104
          35F3AE01359EF77E5911FD711336CA4B2F36E13B1AB837DAC09D4FBD95812DFB
          2AE012E70E971F17E4E0B039BD5326B8DDC48488528CB373F50EC2C580534139
          8EF099163CA082733C0F224EC82DE6D76E2BC2A3FF512B709FC06F02CDC0130B
          F7A863D22103AD9DBDEAF0A00926188F67658288508F9BCB1B8DB6D40C1C096E
          57C1FD6AFC05390D3BE41C70CAB9D1687563EE821D11033F27E1D20F0FF53794
          54A376F1DBA85D1487D65223AA640B6DFEA61C9F7F5301A324E7F90A396816C7
          A14E6032999A5DF8ECEB72E11CEA9A7A70A1C2A49E9B44A3A3BC0ED5162752F7
          9C456A7A19CAEBBAD07CA61A556F2EC6C204BD57627217A86DC8B7E1ECC49796
          ACDDF5F8BCB19D4FBE80352BBFC4A2A42CB5548FBEBE13B18959487A4F07DD63
          CF43F7EF79487C5787D8783DE6CEDF81478485528F5FBE1D69FF7A0E69739F45
          FCB234BCB96A3F1E7A5587875ED1E18D55DF62D5D26DF8E2E167B0F5C1A7C696
          3DFDDFF512936F43F53A56DF84023F10783A3139B83CF7FF0F0FFC06FF98817F
          FE067CC6EC6770CE5E7D94F262856ED8C8FF25FA33FA72997519B01FF5198B31
          A70D5CEE1519F447A05DF22DF9973263E39564C6C62B07627E0270184D83740E
          9B220000000049454E44AE426082}
        Properties.GraphicClassName = 'TdxSmartImage'
        Properties.ReadOnly = True
        Style.BorderStyle = ebsNone
        Style.HotTrack = False
        StyleFocused.BorderStyle = ebsNone
        StyleFocused.TextStyle = [fsUnderline]
        StyleHot.BorderStyle = ebsNone
        TabOrder = 0
        Transparent = True
        OnClick = NavbarItemClick
        OnMouseEnter = NavbarItemMouseEnter
        OnMouseLeave = NavbarItemMouseLeave
        Height = 48
        Width = 125
      end
      object lbEMail: TcxLabel
        Tag = 2
        Left = 0
        Top = 48
        Align = alClient
        AutoSize = False
        Caption = '&E-mail Address'
        Style.HotTrack = True
        StyleFocused.BorderStyle = ebsNone
        StyleFocused.TextStyle = [fsUnderline]
        StyleHot.BorderStyle = ebsNone
        StyleHot.TextStyle = [fsUnderline]
        Properties.Alignment.Horz = taCenter
        Properties.Alignment.Vert = taVCenter
        Properties.WordWrap = True
        OnClick = NavbarItemClick
        OnMouseEnter = NavbarItemMouseEnter
        OnMouseLeave = NavbarItemMouseLeave
        Height = 30
        Width = 125
        AnchorX = 63
        AnchorY = 63
      end
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      ButtonOptions.Buttons = <>
      Hidden = True
      Padding.AssignedValues = [lpavBottom]
      ShowBorder = False
      Index = -1
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup3
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'Link To:'
      CaptionOptions.Visible = False
      SizeOptions.Width = 125
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object lcMainSpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 1
    end
    object lcMainGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup3
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object lclbLinkTo: TdxLayoutLabeledItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Link to:'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      Index = 0
    end
    object lcMainGroup14: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcbtnOK: TdxLayoutItem
      Parent = lcMainGroup14
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Ok'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcbtnCancel: TdxLayoutItem
      Parent = lcMainGroup14
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcEditAddress: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Address:'
      Control = edtAddress
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup2
      AlignVert = avBottom
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object lcEditEMailAddress: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'E-mail address:'
      Control = edtEmail
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lcEditSubject: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Subject:'
      Control = edtSubject
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lcEditTextToDisplay: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = '&Text to display:'
      Control = edtTextToDisplay
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcEditCellRef: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Type the cell reference:'
      SizeOptions.Width = 300
      Control = edtCellRef
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object lcEditScreenTip: TdxLayoutItem
      Parent = lcMainGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'ScreenTi&p:'
      Control = edtScreenTip
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcEditPlaceInDocument: TdxLayoutItem
      Parent = lcMainGroup2
      AlignVert = avClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = 'Or select a place in this document:'
      Control = tvDocumentPlace
      ControlOptions.AlignHorz = ahRight
      ControlOptions.OriginalHeight = 94
      ControlOptions.OriginalWidth = 300
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object lcbtnRemoveLink: TdxLayoutItem
      Parent = lcMainGroup14
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = btnRemoveLink
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcFileOrWebPage: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clBottom
      Control = pnFileOrWebPage
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 86
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcPlaceInDocument: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clBottom
      Control = pnPlaceInDocument
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 86
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clBottom
      Control = pnEMail
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 78
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 424
    Top = 232
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ChooseFile: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 520
    Top = 232
  end
end

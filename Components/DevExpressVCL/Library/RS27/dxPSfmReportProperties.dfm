object dxfmPSReportProperties: TdxfmPSReportProperties
  Left = 357
  Top = 169
  BorderStyle = bsDialog
  Caption = 'Properties'
  ClientHeight = 474
  ClientWidth = 361
  Color = clBtnFace
  Constraints.MinHeight = 502
  Constraints.MinWidth = 367
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000000000000000FFFFFFFFFF000000F00F00000F000000FFF
    FFFFFFF000000F00F00000F000000FFFFFFFFFF000000FFFFFFF0FF000000F00
    FFF080F000000F080F08080000440FF080808088804400000808088888440000
    008088888844000000088888804400000000000000440000000000000000FFFF
    0000000F0000000F0000000F0000000F0000000F0000000F0000000F0000000F
    0000000400000000000000000000F8000000FC000000FE040000FFFF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 361
    Height = 474
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    OptionsItem.FocusControlOnItemCaptionClick = True
    object edName: TcxTextEdit
      Left = 75
      Top = 73
      Anchors = [akLeft, akTop, akRight]
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Text = 'edName'
      Width = 265
    end
    object edCreator: TcxTextEdit
      Left = 97
      Top = 110
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Text = 'edCreator'
      Height = 21
      Width = 243
    end
    object edCreationDate: TcxTextEdit
      Left = 97
      Top = 137
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Text = 'edCreationDate'
      Height = 21
      Width = 243
    end
    object memDescription: TcxMemo
      Left = 32
      Top = 210
      Properties.ScrollBars = ssBoth
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Height = 203
      Width = 297
    end
    object btnOK: TcxButton
      Left = 84
      Top = 441
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object btnCancel: TcxButton
      Left = 175
      Top = 441
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object btnHelp: TcxButton
      Left = 266
      Top = 441
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 7
    end
    object btnPreview: TcxButton
      Left = 10000
      Top = 10000
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'Pre&view...'
      TabOrder = 4
      Visible = False
      OnClick = PreviewClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avBottom
      Control = edName
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 283
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblCreator: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'lblCreator'
      Control = edCreator
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 242
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblCreationDate: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Creation Date:'
      Control = edCreationDate
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 242
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = tshDescription
      AlignHorz = ahClient
      AlignVert = avClient
      Control = memDescription
      ControlOptions.OriginalHeight = 234
      ControlOptions.OriginalWidth = 338
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object bvlPreviewHost: TdxLayoutItem
      Parent = tshPreview
      AlignHorz = ahClient
      AlignVert = avClient
      ControlOptions.AutoControlAreaAlignment = False
      ControlOptions.OriginalHeight = 50
      ControlOptions.OriginalWidth = 50
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tshSummary: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Summary'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = tshSummary
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup8: TdxLayoutGroup
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 4
    end
    object tshDescription: TdxLayoutGroup
      Parent = dxLayoutGroup8
      CaptionOptions.Text = '&Description'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object tshPreview: TdxLayoutGroup
      Parent = dxLayoutGroup8
      CaptionOptions.Text = '&Preview'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutGroup9: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = tshPreview
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnPreview'
      CaptionOptions.Visible = False
      Control = btnPreview
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutImageItem1: TdxLayoutImageItem
      Parent = dxLayoutAutoCreatedGroup1
      Image.SourceDPI = 96
      Image.Data = {
        89504E470D0A1A0A0000000D49484452000000300000003008060000005702F9
        8700000CF14944415478DADD99777C14651AC77FBB3BB3494842124211140529
        963BAEA8D84220809EC6A38807162EA06785136909100434272210C4D38F9C9E
        829E8708521439F4104821A18692504293804012D2EBEE26D936E3F3BE537616
        0965F7FEF11626EF9477669EEFF3FC9E779E77C624CB327EC93FD3FF1D80C964
        6AB5F3EF9EFB025D3A75804CFFD87FC8B2DA282DDF96E5F1922C7FC0D625766D
        591E2BC9D2E7B2244196947DB22C29ADBA6FEFD23FFFEC5E57EBD85601725F1B
        0A6E251DEF3FF73BD3D55C2C61F2FA5B3AC5461E9E3AFA6ED12B9B506F6FC187
        6BF6365556D5DC95B774ECC96B70AC9C931C270F58BCF38A20AD02B86B8E408C
        EDC336B4C5AFDBA5B6E3277EF5CDF3C3EF78A4EBF5ED70A4B4091E8F8C1FCF57
        60775EE1A603CBC68CB88AEB40F11ABCEA22050CC056336626268882692175EA
        EBBBBC8C1CF79DC870F5A5AB83CB814B88365E78EC4EF4BA2916A7CA9DA875B8
        E1F6CAF04A4075553532B20F501F2F242623AF974B6858DBC3181A7354F1B26A
        87C72BE5573B9CB39E5C9A9FC936F91D2F03D11A00F77ACEEC472EF419F542A7
        A8AE3D54D728371ABEE81CDE783101E16DAC70939799A11E5A5C64ED0F154ED4
        198CF792CED931ABC58C9808014EB7170D8D4DA8ADB3212B231B2BC745E1969E
        5D99211CA4A6E8044E7EBBBA72D0BB7BBAD12D9D578AC2E5002CB9B313DD7153
        17C25B7B10B2AB0E8BB774E57D8A1BC9A0761D11131B83A8A848C04CDD4D16EE
        5D99CE9748FF2EB704A7C74DC6032E9787403D68767AE074BAE06C71E14CD119
        84914C3B86B4A07D6C672C4A72F22034D537E2F8A6ED18F86E1E5D18CD4C4A81
        03CC7AD81D979C0E4F79363FC600A68C1FCA55FADEE79BF17D7E152C61D1E810
        DB16D6102BAC56814B233434945D082EB2DE666BE2FB58EB6CA1E8D4D6A1C5DE
        88877E138EFB6E8D81288A58BB7E0752129BD0AB5B7B1D60D07BFBA2E8964D4C
        46810208B9AF3EE4BA3F251DDEF26D3AC05402D04ED996938D4D997B51DC1481
        66B7058D2E0B975943B3994B4892D87029A143B80C521B3A445AD0AD63087A77
        6983C836A110AD22ACA215ABD6E520A96F197ADDD81E1166378E6DDA89C1EFEF
        0F1A40CC7DF50FCEFB2902DE8A1C5F04C60D8176063B77E78EEDD8B973979234
        66332CA4754110A815209077D9BA20D22290B164B048068B82C83DCFD6D9BEE5
        AB32F1CC3DA5888E0C25002F4A72F761F092FC68BAA4233880990F3AEF4F59C4
        019867DFD9722326BF34849DA54781357B76EDC2FE03FB61315BC8700BCCB430
        032D82851B6B21082B8711390C375EB0F20830C8CF966FC6F3F795202A22046E
        8703A5BBF231E4A343317469178B4220006605E081162EA18A5C15E0260500B2
        210A0A447EFE7E141E3EA27ADEA2B43C12166E383356540104325E8B8A487D3E
        FEF45B8C8B2B46DBF050B81C761465EDC5884F8E74A1CBDA68B1070A60CD491D
        DC1C378D25712EB7747146371F801E0199078CB5C78E1E45D1A9535C460CC0A2
        02F068A82D335A81A4456DFFF1D106BC1C770E51E12168B1D938C0D0651CA09E
        96E6400142B6CD18D4D48F03E4703733804994034A29A418AED5439A9E4E9F39
        8DE2F3E7B8C116D55826256E2CDFA74099A915052552EF2E598749F167290256
        0E7032630F862C3D7A9D0AE00C0220A1A91FE5809B25313D94DE61007C18D58C
        36A9B58A2F22EC575A528A8A8A726EAC99E50533D4AC4645359E6FF3E3662C7E
        6F0DA6C49FA10888686AB013C06E3CB2F4684715C01D2840E8B669031C71D317
        C15396CB8F2DDE4A121A3FC42F81953C60402668C59F4C0FB2EAEA2AD4D7D771
        0033498A2536379A6DD362122CDC78B3C98CF477562139BE086D69AC6D6A6CC0
        098A40E2C7C73AD0051BA08C42F2B502581840764ABCBDDFB4B7E16639009603
        DD3179DC30DDE39AD992AC6583C9100D7A1E34DAD064B7D3F06A22C305FEC466
        1160F760869B2CAC35E1AD855F605AFF53681B21C24125C6F1ADBB91B8ECB816
        81C001B292FBD9E3A713409992C46F67B2080C577D4FF221592D5F9DE57F4535
        35D88F3FC868F152512479A826923CF0523B366930EFC28C9739C07202F80191
        612104508FA35BF7E08F9F9CE8445DEA820108CB9C1A67EB4F1262003207B819
        93FF3A5C958962A7A46B499BDCF8C4A549CBE5725269ED56A4C322C7EE410BBB
        17FB3777FEBF30830022DA50046A1B50481118F2E90FD7A900EE4001DA644EB9
        AF319E036CE7E5F2E22C06F0A8026052B4BE6275F6CFBCAF789EAA51563693F7
        3D5413B1750F5BA81A1DFFD230F0DB702F10C0BC4F3163C049448408B0D73572
        80A19F15050D109E39F9DE0606E0BAB09DFB7A51664F4CA508C87ABECACABA64
        8880210AB22132EC0FAB8B9823B8F5DA4846EB6FBCB18C0384870A242116813D
        18F6EFD39DE9686DA000028B40C6A47B08205D8900F5E3002F3FAACF8959FBC5
        9A1CFD5CA3E799EE794BDE77B3D6ED512240DB29939FD0E7D2EC2A6973976166
        3C03B02811A05168F8F233EC4156130C40F8D657FAD6F79F4111200076C7F4CC
        1E1C40538B76AEAC3FD37CDAD7F6CBDAE8A4D61CB2D64FD63305697F5B8A99FD
        4FF00830802324A1112BCE765123E00A142062CB84BBEA06A4A6C359BA831BB0
        28AB07A64C78CCF0240656AECD51A789E47149F33C79DBA3695F1979D8BA87AF
        7B913A3DC92FE9D3D23E2200CA81301681061CA6083CB6E2FCF56A04020688DC
        3CE18EDA013C0776F0B96F7A564F24BFF227757451DCA91712B27114822E332E
        7963F1678C90DAE7B5B48F312BFE18C2C32802B58D38949187912BCFDF405DAA
        830168BB69FCEF6B1228023C89C99285D9BD0860A43A0A2986AC5895AD7A5EF1
        3EF7BA471971B8FE692AC98E4D4B7E4A1F6E257E639FC45E4FFB27011C577240
        03F8B284CD5FAB820188DA34FEB7D5093308804B48C282ACDE489E34D2EF5D8D
        51F7FE2586BAADEA1E7A3E401FC1A03EB9E7BCF62166530498846C350440B3BC
        51AB4B6FA483958102882C02FF7DA94FF54006C025C422D01B53278ED4ADBC94
        1CF437749A5CB46C36ACFB97E2C09C391F6076BF634A12D7DB78041E5F732168
        80A8EF5EEC533528750125F14E3E7E2FD8D61B291347F9BCA89BE0CB07638DE4
        7BDDE3F33CD38E648C147550008E22822464AB6311D88727D696DDA402380305
        88DEF8C2ED9583672C240929AFF8E6530452A63CEE9386D1303F39F97B58962F
        1D096D7D3601CC892B5472A0BE110519FBF0D4BAF26E74A8221000939A03D11B
        9FBDAD72F04C16815D7C149A9F7D2B07D087512D114D06830C86F92E2DEB100E
        8713369B9D2A55072A2B6A70A4B00815655548BDF7204540A008D85090B50FA3
        D75574A7EEE5C100C46C78F6968A0798844AB40810C0D427F504306ADC170955
        22264DEFBE127BC9875F2384E6CBA1212277802098F95CC17E762F92FA14A38D
        D5CC73A020EB00467F5571339D581628009350CC37CFF42E7F20753E8F003DAD
        306FDB6D984E00FE137AD920213FE118461CF27C93132B577E8F89E31EE52FB9
        6A69E6554B7ADF9D771803C5F5E8DA3E94A69C260E904F00495F57060DD06EFD
        D33DCB1E4C7D8B22B09B478003D078EE5F325F3C6C1A24A48398D0489259FDE5
        563C37361185277E440D49A5B8B80CDD1B37A2478C0337748CE0A7DA697F7EF6
        018C595FD543056809142076FD989B2F3C48116829D5006EC58CE4D1978C809F
        9474E37D754F697935366FDA8511C3FAA1F0F859949456C0736E3BEE8D3D85E8
        482B3AC544F03CB3D7D9919F93CF007AD2E9178202F83AA9FB051681162621EA
        F666F6ED983E6DB4DF04C637B1BFF428A45DBDF0C8691C3C7802F1717D90B7FF
        042A8B0E6260F46EFE2A858DFFB1D161FC7C1B49E8607601C66CA80E0AC0CA00
        D63E717DE9C3B3D3D1AC4AE8CD9CDB919A92E47B8D62ACF90D52F24B70755766
        661EAAABEA101E1E8AB387762021F60025AD85EA1F9127764CDB104E6EA30814
        E41460EC869A5E7466693000ED573FDEA52471D602B414EFE10073737F4595E4
        18C337329F974DF09791DF104A4797BCBF8AC6012FCC0D4578A84B21196D4628
        19CEBE1B88B41E1926F2BE2C02F9DB0EE2E9FFD4F6A61D25C10074F87254E7E2
        C4994C42792AC0AF952F3292FAA14E92D51AE8A26DC86A1F653FFB5E269ABCB8
        23B61877B62F01D98C10C104338109D4B2D127D42AF07A8BE5C0FE9C43F8CBC6
        FF05C0C8EB8A135F9D879692BDFAECC9EE7071A3B45A997F69D485EF335A5713
        DF2FF17533FD11F90B2EBD90526A53C3786BAFD700EA7AAB126A0E34893B2E1D
        DAA178D4EB2C027BD51B7AD5FA46E6CF05F5B3AA7E73ED131407D0E6CCB2EF98
        49528B69FD3C70AF6B99EF7679D1EC68C6F6AD0598B0A521A81C60009DE62544
        15248E1D131B2554738375638C865DD4326F6BEB9264888C7411A81635FD5C05
        B8AAAA013B769FAA4DDBE1B8877A1423C06A94BD95E816778338E9EECEE2B321
        3458181EB897FF19E70AAD75697583ACF5C29157E6FE7C4F99E7EFB47906CA37
        B26B06607FD8A47A002DECF586A01DBEA8F53BF50ADB57DAAFE1B04FAB4C3AEC
        7547A9AC27D4B501B01F0DCC68474B289497BD57F5B5FE1A8C6D0D8019CCBE4E
        B2975A97AD83AE04108C2141FFAE6478AB00BFB4DF4F32BE55283AF5E2730000
        000049454E44AE426082}
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = tshSummary
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutGroup6
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = dxLayoutGroup6
      CaptionOptions.Text = 'Separator'
      Index = 3
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 208
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

object fmFlexibleLayoutDemoMain: TfmFlexibleLayoutDemoMain
  Left = 0
  Top = 0
  Caption = 'Flexible Layout Demo'
  ClientHeight = 511
  ClientWidth = 649
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 649
    Height = 511
    Align = alClient
    TabOrder = 0
    object cxDBImage1: TcxDBImage
      Left = 22
      Top = 153
      DataBinding.DataField = 'CustomerPhoto'
      DataBinding.DataSource = dmDemo.dsOrders
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 1
      Height = 113
      Width = 140
    end
    object cxDBTextEdit3: TcxDBTextEdit
      Left = 217
      Top = 175
      DataBinding.DataField = 'Prefix'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 2
      Width = 121
    end
    object cxDBTextEdit1: TcxDBTextEdit
      Left = 240
      Top = 202
      DataBinding.DataField = 'FirstName'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 3
      Width = 121
    end
    object cxDBTextEdit2: TcxDBTextEdit
      Left = 239
      Top = 229
      DataBinding.DataField = 'LastName'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 4
      Width = 121
    end
    object cxDBTextEdit4: TcxDBTextEdit
      Left = 224
      Top = 256
      DataBinding.DataField = 'Spouse'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 5
      Width = 121
    end
    object cxDBTextEdit8: TcxDBTextEdit
      Left = 215
      Top = 283
      DataBinding.DataField = 'State'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 6
      Width = 121
    end
    object cxDBTextEdit9: TcxDBTextEdit
      Left = 208
      Top = 310
      DataBinding.DataField = 'City'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 7
      Width = 121
    end
    object cxDBMaskEdit1: TcxDBMaskEdit
      Left = 231
      Top = 337
      DataBinding.DataField = 'ZipCode'
      DataBinding.DataSource = dmDemo.dsOrders
      Properties.EditMask = '00000;1;_'
      Properties.MaxLength = 0
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 8
      Width = 121
    end
    object cxDBTextEdit12: TcxDBTextEdit
      Left = 82
      Top = 406
      DataBinding.DataField = 'Trademark'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 9
      Width = 121
    end
    object cxDBTextEdit11: TcxDBTextEdit
      Left = 59
      Top = 433
      DataBinding.DataField = 'Model'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 10
      Width = 121
    end
    object cxDBHyperLinkEdit2: TcxDBHyperLinkEdit
      Left = 75
      Top = 460
      DataBinding.DataField = 'Hyperlink'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.BorderColor = clWindowFrame
      Style.BorderStyle = ebs3D
      Style.HotTrack = False
      TabOrder = 11
      Width = 121
    end
    object cxDBNavigator1: TcxDBNavigator
      Left = 10
      Top = 86
      Width = 270
      Height = 25
      Buttons.CustomButtons = <>
      DataSource = dmDemo.dsOrders
      TabOrder = 0
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'Agent Info'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'About this Demo'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Car Info'
      AllowWrapItems = True
      ButtonOptions.Buttons = <>
      Index = 3
    end
    object dxLayoutImageItem1: TdxLayoutImageItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 
        'Resize the form to see how labels are wrapped and layout items a' +
        're rearranged to best occupy the space provided by their parent ' +
        'group.'
      CaptionOptions.WordWrap = True
      Image.Data = {
        89504E470D0A1A0A0000000D49484452000000CC000000220806000000136C36
        00000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000
        00206348524D00007A26000080840000FA00000080E8000075300000EA600000
        3A98000017709CBA513C000009B849444154785EED9C3DA81B4710C7952EA5DA
        74EAD2AA721B15298C2B4170AFD7BB50157746C15542304AF93A055C249DBA04
        DC28455CA41226903222983C77790183BF7DD9FF31BBECCECDECC749BA48E60C
        8BDED3EDC7ECEEFF3733BBF7F0472FBEFAE4B3C160F08D29374CE9E2DF6F6690
        2F3FBEF7F72F5D0CD68FD1AFC04157C00073654AD571B9AAAA6AD0977E0DCE4D
        03838E4171609EDB42F5F6F67043036701CCAD5BB786A64CB472D090DB4167C6
        490D4D9914965107A67D90431CD2D97502CCCBEF6E54AFBEFFA27AF3F3BDEACD
        E6DBEADD9FBF9A39A43D9601646C4A9559B6A6DED29493171681529A062F3E48
        357730A91CADE5D63938302FBFFEB47AFDE38503E3FD8B7F0187F42F7986A188
        920B8C5F0FE00C3BD88B5643F4C0B45AB6D68D7261C8A97710605E5D7E5E478F
        7757BF6B70740D0CE041C43949687A605A6BBF55C31C10A43A669F66A62CA88C
        F63AC320CD0224EFFFF9AB0412BF6EDB08B331202CBCB28EA46C9B562B7CE446
        0A303BF3FD2652664736EB83EDBE0D30661F56A66C099AA5F9BCC6BE1547189C
        45DEFEF15311244F9E3CA91E3E7C58DDBF7FBFBAB8B8A853A79C492829592397
        C7B9C5140D9C93139A024C7F463912B2395AF3EB98FD1913201694B5F97D0E80
        B281012874584FC2F2FCF9F3EAD1A34735209AF7CF99442E30769D15687639FB
        E0DDC41D3D8D3B0760C8C6BD2E50E836709C5A7FEFD6B0D5DA93ADEA384ABA05
        2826CA33A46188F6F8446441199952258129012505890FCF9180C1F5F3B500E9
        54DA348204E9DD8EB5411F2B7EE3667E9F9A8294D02F626430756642DD09ECD8
        07184A1578EAD6981F79445EAF8EB69466F8CF96F4FD94D210FF060FA9A21AFD
        780A49FD0CE97BDB4FA33D41623DB83F1EC48974280A0F8919B6F1DB46A451C1
        78566BE67BCC0FD182B7413F18B33EA7D0FED4F3A6FAF6E79D0A0C6EBBDE6E7F
        48469367CF9ED5E9D6EDDBB78B6EB38E010C368B84CE6D91D2385C594B70F96D
        F1DC792E4AFD78DFD70A8C808AD7AD45B027301369C37D1B488C101E17A21D1F
        42F09F011E082676D50D2136442CB4199BEFF8D8C1FA9370791D3E369E37A206
        CD0DB6C46C0DF6842048CD0FFDB98843C003949929001BCFA72230B8168E5C07
        D7100194070F1E144172EC0843C0200A70BB36BEA00041062CB60F40E384627E
        C6ED1BEFBFB1B1429DB5B5611F600838C94B3A519267E4829A79E3736052E2B5
        7DADB87350BC351FDBB70D40E5BE83826003489508C1FB736B4DEB05D127C764
        E798A169332770005B0D53004C4E54C1F9E4F2F2B235285648478C30B8004801
        C3450F2890424D4C418AA64628F36C2E3C9F332025687DC18A51823607DEBE51
        580419095E1CA2C726A370000287411E5E1310448AF1358826CC96A41001B007
        2BFAE7910F82C69A709051CF6F8B794B29D894E6844880FE83FDA0EFB43101C5
        1620E668D201837729A9F72838A394A65E82B80E7E4BC6BD5E0C1802231A2104
        685C8857D2321EC1965A3A461E4F02262A3CC1B38BE2321B2F79D31113B936BE
        131A81072171BB822813F1DC680B3142CC3564826D8072CC6C83E8833394079B
        34B72002D1383C2A35A22D8703F3CD0606B0C45230A45F77EFDEDD3BAA749192
        61C112C0F0EBE74000D45E8A526E634DFF8D2B6C7FD385B46D9D2958151A0E0C
        0943F2D6FC3BE7A13DE149C004D053FF924777CE83EA483607F3F5C65D33181A
        6B0F8004086BE095083493D686AD772365CB8143AA33C02D580C96C78F1F1F2C
        AA74010CCE1B0960F8411FD100A9182FDC414CED262851AA7EAE8C1F6C2AA51F
        39A98CABA300938A548067C8DB2AE3CF9531A428E3FA54224C2D7061DCC6D986
        6CC13CFCC2EB4D08186DBE2B1EA918308864522A372B05076FDBD57F8738AB74
        9D9291F0C5338C22E6DCC8B9F08091A05C1230D2F92510AD2258D77FCA633231
        70AFED0BA3165A26305A5D9E22D5B749B64F41883B654C2972E43A8DA9379E04
        B0ED672381A34426DB064E659A0B8E080C0EF6874EC1383839062AE28F0A4B39
        945B31238AE40212BD9A16D2B22D01C3CF2FAB4CC1B6054613A228DC88A7D680
        590850C480D914409A0BCCCC030669224F3B1B077ADF86CC6BE5558E261BC000
        963B77EEB4155576BB1CE35A0223BDFFA83D94D25FAECDCECB515FB8556BBC67
        11CE2F41BB8860DB022345002B2027345F404A843B656002DBE842026998061C
        52B0919DB3F7E2726EBE97D233B75E295D06C074050B8496320CCF4B81890031
        B48B27885C3BC3F0338DEB838091D2321EC1AE0BBC6D3130CA0139B8612A38C3
        34C026B891E670613A310ACFB408235D20207AF1F38BF47BB0F62CDAA00F0902
        B79EC2FB959912A5B6295D3A60BA84E518C0D0F9642700B1F2456B9EF3437FF0
        5C12B8F69D9096F1E826F67DA8338CE9878B594A551A202AE32FF93CC9937331
        064E201718828FF7D57AED59C4445ACAFB76E04A10D0DC1AE7A12C60BA86E5D0
        C05064D90AB0000EE70D2932AC847AA2774DC1A3DC96F9699AE6B5E1451B3746
        A9F19848E0251B9E5FF19CC11A28C040708127570ECB6B6607B7418C3004CC5A
        B0795C326FADAEE03CA2C0D0B9A6713ECB02E6D8077CE9A09D322C9292C18B2F
        BC228162453BE30B1C49DB0012522ABC8319D3CF734491488491D232F7273591
        CD9580417440C4D08A8B008AE7AF05629E492071914BE343F8B001EDF17C2588
        3BB821A3F14A80D1C65DD29848DB102D506F8EB56070E239C0AEEBDB675497DB
        E1AF172209E633C5DA112C1803F30D52D8942E07F8C3C93D6E8E5AB74D191601
        2677CC062C7681CD7CF94D56AA4FB73902806B65FD5685C0A46E8C9C784830E2
        BB0A12311702BF0AD6849B6D8327D66C6022512636AE8B408A33D0DAD67B46CE
        25352FFB7C91D2E5E0507FEA520A5DCAB03D80C1394615B8078D2674099E7924
        CA48B765E8637A0C60C8034745AA086BE7895C02069E3B262C78E9219F93D026
        880A42FDA1903EC5C6758E2F12F562D1458AB8D278F5FC52BAC44D54CABB1EE5
        79CAB042607056414AA58A54122F9D41A48B027FCE78AEF64B970D7C8DAE3558
        C8EBB5F1F0B51015B1351C84906E402435F894F270D14058108D2426A4330D58
        A8AFA208E3418B944B8A84FC96CFAD3DA55E31B0D19F03CC9B6B6C1C9BE22561
        81264F1A1882C69E27EC352FCE15F0EAF6F7514C9C39CFE8CC823EFDB3517D9E
        29686FEDC1E738010CBC2CA02929759F521B692C536F24D4F5FB10533A6AB330
        9F288028BA06DA1839EB46F319D338764C7C625DD47129CAC236DB06F0A96B4E
        6716ACC7D46B538F93E3B8FD3A00E6E9FF10659E96189ABBF87DBDBC1550224C
        234AE5F576FAB54AB496AA0B606E9A72D5213418EB66CAB080EAD3DF93B3B2B0
        0726FD9F486AFA4CFE574725C23E56DDB352E31918DB03D3037306323D1D137B
        607A604E478D6760490F4C0FCC19C8F4744CA45BA68DF9F44BF466EF74AC2FB7
        E4904785FF0040EA854018BD807F0000000049454E44AE426082}
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.Text = 'Short agent data'
      CaptionOptions.WordWrap = True
      CaptionOptions.Layout = clTop
      Control = cxDBImage1
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Agent Details'
      AllowWrapItems = True
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutLabeledItem1: TdxLayoutLabeledItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000060000
        000A0000000A0000000A0000000A0000000B0000000B0000000B0000000B0000
        000B0000000B0000000B0000000B0000000B0000000700000002AA7D6EBDAF82
        72FFAF8272FFAE8172FFAE8072FFAE8071FFAE8071FFAE8070FFAD8070FFAD7F
        70FFAD7F70FFAC7F6FFFAC7F6FFFAD7E6FFFA67A6ABE00000007B28577FFFCF8
        F6FFFCF8F5FFFBF7F6FFFBF7F5FFFBF7F5FFFBF7F5FFFBF6F4FFFBF6F4FFFBF6
        F3FFFBF6F3FFFBF6F3FFFAF5F2FFFAF5F2FFAE8072FF0000000AB5897AFFFCF9
        F7FFF7EEEAFFF8EEEAFFF8EEEAFFF8EEEAFFF7EFEAFFF8EEEAFFF7EEE9FFF7EF
        EAFFF7EEEAFFF7EFE9FFF7EFEAFFFBF6F3FFB18475FF0000000AB68C7EFFFDFA
        F8FFDABEACFFA5653DFF9C562BFFA86F4EFFF7EEEAFFF7EFE9FFF7EEEAFFA263
        3AFF995427FF975125FF995B38FFFBF7F5FFB28778FF00000009B98F81FFFDFB
        F9FFF7EFEBFFF1E4DCFFAE6E40FFEEE0D8FFF7EFEBFFF7EFEAFFF7EFEAFFD4AE
        8FFFB57442FFAA6A3CFFF0E3DCFFFBF7F6FFB58A7CFF00000009BB9385FFFDFB
        FAFFF7F0EBFFF8EFEBFFBB8C6DFFD0AF9DFFF7EFEBFFF7F0EBFFF8EFEBFFBF86
        58FFB2723FFFB98A6FFFF8EFEBFFFCF9F7FFB88E7FFF00000008BE9688FFFEFC
        FBFFF7F0ECFFF7EFEBFFE2C8B8FFA25D30FFA25C2FFFA15B2FFFB57644FFB879
        47FFA66335FFE0C9BCFFF8EFEBFFFCF9F8FFBA9183FF00000007C0998BFFFEFC
        FCFFF8F0ECFFF8F0ECFFF6EFE9FFBA8053FFF3E7E0FFF8F0ECFFD3A988FFB779
        46FFAA714DFFF8EFEBFFF8EFEBFFFDFAF9FFBD9587FF00000007C29C8EFFFEFD
        FCFFF8F1EDFFF8F1EDFFF8F0ECFFC69C82FFD1B09DFFF5ECE6FFC08555FFB070
        40FFCDAA97FFF8EFEBFFF8F0ECFFFDFBFAFFC0978AFF00000006C49E91FFFEFE
        FDFFF9F0EDFFF8F1EDFFF9F0EDFFE9D4C8FFB6805FFFE4C9B6FFBD7F4EFFA766
        3BFFF1E5DEFFF8F0ECFFF8F0ECFFFDFCFBFFC29B8DFF00000006C6A093FFFFFE
        FEFFF9F1EEFFF8F1EEFFF9F1EDFFF8F1EDFFB9825AFFCA976EFFBA7B4AFFBE91
        75FFF8F0EDFFF8F1EDFFF8F0EDFFFEFCFBFFC49E90FF00000005C7A396FFFFFE
        FEFFF9F1EEFFF8F1EEFFF8F2EEFFF9F1EDFFDBB99FFFC08452FFAE6E40FFE2CD
        C0FFF8F1EDFFF9F1EDFFF8F1EDFFFEFDFCFFC5A093FF00000005C8A497FFFFFF
        FFFFF9F1EFFFF9F2EFFFF9F1EEFFF9F2EEFFF2E3DAFFC08352FFB27550FFF8F1
        EEFFF9F1EDFFF8F1EDFFF8F1EDFFFEFDFDFFC7A295FF00000004CAA699FFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFEFFFFFE
        FEFFFFFEFEFFFFFEFEFFFFFEFEFFFEFEFEFFC8A497FF00000003C9A699BECAA7
        9AFFCAA79AFFCAA79AFFCAA79AFFCAA79AFFCAA699FFCAA699FFCAA699FFCAA6
        99FFCAA699FFCAA699FFCAA699FFCAA699FFC8A496BF00000002}
      CaptionOptions.Text = 'Very long agent data'
      CaptionOptions.WordWrap = True
      LayoutLookAndFeel = dxLayoutStandardLookAndFeel1
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Prefix:'
      Control = cxDBTextEdit3
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'First Name:'
      Control = cxDBTextEdit1
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Last Name:'
      Control = cxDBTextEdit2
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Spouse:'
      Control = cxDBTextEdit4
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'State:'
      Control = cxDBTextEdit8
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'City:'
      Control = cxDBTextEdit9
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Zip Code:'
      Control = cxDBMaskEdit1
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Trademark:'
      Control = cxDBTextEdit12
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Model:'
      Control = cxDBTextEdit11
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hyperlink:'
      Control = cxDBHyperLinkEdit2
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = cxDBNavigator1
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutStandardLookAndFeel1: TdxLayoutStandardLookAndFeel
      ItemOptions.CaptionOptions.Font.Charset = DEFAULT_CHARSET
      ItemOptions.CaptionOptions.Font.Color = clWindowText
      ItemOptions.CaptionOptions.Font.Height = -11
      ItemOptions.CaptionOptions.Font.Name = 'Tahoma'
      ItemOptions.CaptionOptions.Font.Style = []
      ItemOptions.CaptionOptions.UseDefaultFont = False
    end
  end
end

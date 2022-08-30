object Form17: TForm17
  Left = 50
  Top = 50
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Group Scrolling Demo'
  ClientHeight = 350
  ClientWidth = 946
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesigned
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 946
    Height = 350
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cxDBImage1: TcxDBImage
      Left = 724
      Top = 28
      DataBinding.DataField = 'CustomerPhoto'
      DataBinding.DataSource = dmDemo.dsOrders
      Properties.GraphicClassName = 'TdxSmartImage'
      Style.HotTrack = False
      TabOrder = 21
      Height = 300
      Width = 200
    end
    object cxDBTextEdit8: TcxDBTextEdit
      Left = 89
      Top = 213
      DataBinding.DataField = 'Trademark'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 8
      Width = 213
    end
    object cxDBTextEdit144: TcxDBTextEdit
      Left = 89
      Top = 240
      DataBinding.DataField = 'Model'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 9
      Width = 213
    end
    object cxDBTextEdit6: TcxDBTextEdit
      Left = 89
      Top = 267
      DataBinding.DataField = 'HP'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 10
      Width = 213
    end
    object cxDBTextEdit1: TcxDBTextEdit
      Left = 396
      Top = 129
      DataBinding.DataField = 'LastName'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 3
      Width = 121
    end
    object cxDBTextEdit3: TcxDBTextEdit
      Left = 548
      Top = 129
      DataBinding.DataField = 'Title'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 4
      Width = 121
    end
    object cxDBTextEdit2: TcxDBTextEdit
      Left = 719
      Top = 129
      DataBinding.DataField = 'Address'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 5
      Width = 121
    end
    object cxDBNavigator1: TcxDBNavigator
      Left = 10
      Top = 80
      Width = 270
      Height = 25
      Buttons.CustomButtons = <>
      DataSource = dmDemo.dsOrders
      TabOrder = 0
    end
    object cxDBTextEdit14: TcxDBTextEdit
      Left = 55
      Top = 129
      DataBinding.DataField = 'Prefix'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 1
      Width = 121
    end
    object cxDBTextEdit11: TcxDBTextEdit
      Left = 881
      Top = 129
      DataBinding.DataField = 'HomePhone'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 6
      Width = 121
    end
    object cxDBTextEdit10: TcxDBTextEdit
      Left = 1037
      Top = 129
      DataBinding.DataField = 'Email'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 7
      Width = 121
    end
    object cxDBTextEdit5: TcxDBTextEdit
      Left = 89
      Top = 294
      DataBinding.DataField = 'Liter'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 11
      Width = 213
    end
    object cxDBDateEdit15: TcxDBDateEdit
      Left = 446
      Top = 213
      DataBinding.DataField = 'PurchaseDate'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 15
      Width = 221
    end
    object cxDBTimeEdit12: TcxDBTimeEdit
      Left = 446
      Top = 240
      DataBinding.DataField = 'Orders_Time'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 16
      Width = 221
    end
    object cxDBTextEdit13: TcxDBTextEdit
      Left = 446
      Top = 267
      DataBinding.DataField = 'PaymentType'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 17
      Width = 221
    end
    object cxDBCurrencyEdit16: TcxDBCurrencyEdit
      Left = 446
      Top = 294
      DataBinding.DataField = 'PaymentAmount'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 18
      Width = 221
    end
    object cxDBTextEdit17: TcxDBTextEdit
      Left = 446
      Top = 321
      DataBinding.DataField = 'Quantity'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 19
      Width = 221
    end
    object cxDBCurrencyEdit18: TcxDBCurrencyEdit
      Left = 446
      Top = 348
      DataBinding.DataField = 'Price'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 20
      Width = 221
    end
    object cxDBTextEdit9: TcxDBTextEdit
      Left = 89
      Top = 321
      DataBinding.DataField = 'Cyl'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 12
      Width = 213
    end
    object cxDBTextEdit7: TcxDBTextEdit
      Left = 89
      Top = 348
      DataBinding.DataField = 'TransmissSpeedCount'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 13
      Width = 213
    end
    object cxDBCheckBox1: TcxDBCheckBox
      Left = 89
      Top = 375
      DataBinding.DataField = 'TransmissAutomatic'
      DataBinding.DataSource = dmDemo.dsOrders
      Properties.ValueChecked = 'Yes'
      Properties.ValueUnchecked = 'No'
      Style.HotTrack = False
      TabOrder = 14
    end
    object cxDBTextEdit4: TcxDBTextEdit
      Left = 214
      Top = 129
      DataBinding.DataField = 'FirstName'
      DataBinding.DataSource = dmDemo.dsOrders
      Style.HotTrack = False
      TabOrder = 2
      Width = 121
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup9: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Photo'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxDBImage1'
      CaptionOptions.Visible = False
      Control = cxDBImage1
      ControlOptions.OriginalHeight = 300
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Car Details'
      ButtonOptions.Buttons = <>
      ScrollOptions.Vertical = smAuto
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Trademark'
      Control = cxDBTextEdit8
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Model'
      Control = cxDBTextEdit144
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'HP'
      Control = cxDBTextEdit6
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup10
      CaptionOptions.Text = 'Customer Details'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ScrollOptions.Horizontal = smAuto
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Last Name'
      Control = cxDBTextEdit1
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Title'
      Control = cxDBTextEdit3
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Address'
      Control = cxDBTextEdit2
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutGroup10: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Worker Data'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup13: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup1
      CaptionOptions.Text = 'About this Demo'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      Index = 0
      AutoCreated = True
    end
    object dxLayoutImageItem1: TdxLayoutImageItem
      Parent = dxLayoutGroup13
      CaptionOptions.Text = 
        'Use scrollbars in individual layout groups to scroll their conte' +
        'nt independently'
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
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      Control = cxDBNavigator1
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 270
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'Prefix'
      Control = cxDBTextEdit14
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'Phone'
      Control = cxDBTextEdit11
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'Email'
      Control = cxDBTextEdit10
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Liter'
      Control = cxDBTextEdit5
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Order Details'
      ButtonOptions.Buttons = <>
      ScrollOptions.Vertical = smAuto
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignVert = avTop
      CaptionOptions.Text = 'Purchase Date'
      Control = cxDBDateEdit15
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Order Time'
      Control = cxDBTimeEdit12
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Payment Type'
      Control = cxDBTextEdit13
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Payment Amount'
      Control = cxDBCurrencyEdit16
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Quantity'
      Control = cxDBTextEdit17
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutGroup3
      CaptionOptions.Text = 'Price'
      Control = cxDBCurrencyEdit18
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup10
      AlignVert = avClient
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Cyl'
      Control = cxDBTextEdit9
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Speed Count'
      Control = cxDBTextEdit7
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Automatic'
      Control = cxDBCheckBox1
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'Name'
      Control = cxDBTextEdit4
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 408
    Top = 24
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      LookAndFeel.NativeStyle = True
    end
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 528
    Top = 24
  end
end

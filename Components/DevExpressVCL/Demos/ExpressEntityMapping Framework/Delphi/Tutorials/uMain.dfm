object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 660
  ClientWidth = 1102
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 1102
    Height = 660
    Align = alClient
    TabOrder = 0
    AutoSize = True
    object cxTreeView1: TcxTreeView
      Left = 10
      Top = 10
      Width = 359
      Height = 640
      TabOrder = 0
      OnClick = cxTreeView1Click
      HideSelection = False
      Images = cxImageList1
      ReadOnly = True
      SortType = stText
      StateImages = cxImageList1
    end
    object pnlDemoSite: TPanel
      Left = 375
      Top = 37
      Width = 717
      Height = 613
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 2
    end
    object tsShowCode: TdxToggleSwitch
      Left = 957
      Top = 10
      Caption = 'Show Code'
      Checked = False
      Properties.OnEditValueChanged = tsShowCodePropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 1
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'Demos'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = cxTreeView1
      ControlOptions.OriginalHeight = 593
      ControlOptions.OriginalWidth = 359
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Layout = clTop
      Control = pnlDemoSite
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 41
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Control = tsShowCode
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 135
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 480
    Top = 304
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object cxImageList1: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 25166160
    ImageInfo = <
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000002B744558745469746C65004F70656E3B466F6C6465723B426172733B
          526962626F6E3B5374616E646172643B4C6F6164F1C3C4630000022249444154
          785EA593BD6B545110C57F6FF3242AA9B412144B3FB1500CA20663FC032C6C44
          B0B010041194147616366295805A88A058898D8D106C444D6C44123426120959
          36B8316FD9F8F2B15F6FDFBD7746B9EFAD82019B1CB81C6698397366E006AACA
          461000859CC959F327FC079DC1E1C4C381D785807EB21815A827E642DF8DB1E7
          E30F4E8BA2A0CA1F12A5F7FA281D8488F61FBAFC940C8A6DD5987A76F3DE9D4B
          FB468F5C7D53653D0470B94B426704DC2AA4DF517184380E9EBBB6DD99E1F2A9
          03DB4072BB0AA2608C1B1BB8F5E10C6032012BA00E5C422016D4D2250D0E5F1C
          24E8DE0104A00208AA8ED1BB57FA80AEBF02A9CB046C82DA26625BE00CD4CBA8
          CF1B54F21A1556AB75809E20080A800BDEDD3EA67D8343C8CA272AD3E3C40B15
          54404510C92EA7A28888E7A5F92AEAD38A38AD862675B4E28864A1C872D460FF
          F961BC786E1B71A85A108B3A8B8A0149894BDF78FB6868DEAFB05A2E11CFCCD2
          B3AB174D221AB32F51DF90DD47457C8C737E1D114769B2CCCA9A19094DDBB154
          9A632D5A66E7F1BDB42BD3D85ADC29CC9A54C04F17C40B0951B1C29772FD4598
          B62D4B73737477F7B0796B487D6602DB58F6D63B2E347391E71CB59536F1CFC6
          D7FBEFA362D86AA4B018B1E7C44992C529926AD14F50BF7BA749BD082A5EA8BA
          9010D7CD0860C24633FD88AB1D9D1C79C5647E5D72560001505401554421B55A
          FEFCA3F9184803600BB00928B01EC13FB10206489E9CDDED0036FC9D7F01FAB6
          A14B22EE620A0000000049454E44AE426082}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          6100000016744558745469746C6500546578743B506167653B5265706F727476
          6161000000018E49444154785E7552CD4A03410C9E192B0A3E8B480FFEE1C18A
          1705C183545B2F0A45BCF5193CFA00E24910547A522F8237919EFD7D1B7527C9
          9899A4D3955DA79BCDEC4CF2E5CB97364208C65AEBCE079F4FCEB95630C1A4C5
          2EDEE936BFE211010CFB87F3EB7C8F0D23CBB2B58EF766E3BE9224D763B0B3EB
          8F5576136C6300444A9BC293864606F28A3F79AC999A7406804645CD08C02009
          3A95B2255F3FD5223C1266A625063EF77D79FB92938380253B682FB0B3FF3040
          A9434CA1BD39376E83D8EC888D880850C3003CE6A0C1C3BB14550AA4FC7B9DC5
          74EE7D0D834245243EDFDD6AFE1923A988C048C806586190662BD538E0E6FE4D
          671EFEE8D0EB2EA73320AC30B05EC7471CD0DD6E32509E7CF4DA8ADC03841A0D
          10195DC67975F79AC728DEA6CA47FB2B090401EBA640328548B5B3A4B5AD30C8
          FF11E2BD6300AA6901A9A4BC26AA0E42C4E502885427A2683033DD9009A460A6
          AEE529B641AA035501F0C77F3FF74F1ED782642711AD0D8A9E1E63958D2FBE86
          ECA00C0017A73B1BF22DBD89AF2CE94D920B36F30B206841CBFAC5FA2D000000
          0049454E44AE426082}
      end>
  end
end

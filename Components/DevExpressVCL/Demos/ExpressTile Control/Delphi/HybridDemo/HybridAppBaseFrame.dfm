object frmBase: TfrmBase
  Left = 0
  Top = 0
  Width = 885
  Height = 567
  TabOrder = 0
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 885
    Height = 447
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = DM.dxLayoutCxLookAndFeel1
    OnClick = dxLayoutControl1Click
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
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 1
    end
    object lgBackButton: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object liiBackButton: TdxLayoutImageItem
      Parent = lgBackButton
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Image'
      CaptionOptions.Visible = False
      Image.SourceDPI = 96
      Image.Data = {
        89504E470D0A1A0A0000000D4948445200000020000000200806000000737A7A
        F40000000467414D410000B18F0BFC6105000000097048597300000EC000000E
        C0016AD689090000001A74455874536F667477617265005061696E742E4E4554
        2076332E352E313030F472A100000271494441545847CDD63BAA144114C6F151
        C14003133513849B68E6128C5C83682C98BA011F88F105357101466E434C0437
        202828A2892818188EDF6FA67B6EF5D8AFB90E941FFC679AEE3A8FAA3A75BA17
        CBE57241FE77213A08D71A0EFAC6CC61FD33ADB3E16AB8159E86D7E153F8DDE0
        DA3DCF8C3196CDB42612381DCCF07E781BBE8477E155781E9E34B876CF33638C
        65C3968F618D247022DC096FC2B7F032DC0C57C2C570269C6A70ED9E67C618CB
        862D1F7CF5AB278193C1FE1E869FC1CC6E845DC5862D1F7CF1C977573D0918F8
        227C0C0FC2A5303C8361B161CBC787C027DF5D6D25C048B6823F0EE7C2BF8A0F
        BEF8E4BB3B992201C562BF2C99ACF711BC155F7CFE08621C156691808A5534F6
        CDD28DE97CB81B1E36B8BE10C6C427DF6288B556938033EBD8A85CC533B5E7CF
        C2AFC0F87BB817A6CE3D9F7C8B21D67A7C9380C6E1EC3A3E633273C119B5C16F
        87A9844B892196989B04742F0DC4191E92E08AA89DF9D760E6BB042731C41273
        938016AA8B6924433ACEB2F7490CB1C4DC24A08F2B10DD6C5BFB58F652628825
        E64A9AC3E7A09F6BA9A5B6971DEFC3A3D09E80292E8752628825E6AA313912DE
        685E2AFA7AA972D98FCBF5504A0CB1C45C1DC7EA095806EFF33A5B50BD08FF97
        6358BD11556FC5D55F46E448547B1D53F50F12B24C0ACD37DCBE3FC9F8E4BBBB
        BD5B0950D58F522A3FCB2D997D533CBB8A0D5B3EF8E273D667792B33B05F8A46
        E53A3ECEB046A29B69A9FA3A5CBBE79931C6B261CBC7F00A8E24408A45C53A36
        CEAE06A28B99997EEEA502D7EE79668CB16CD81E155C9F261268E5CC6A1CBA97
        16AA8F7B9978A3C1B57B9E1963ECBC363D33818E1A1B7B6A86F8BBB86669B1F8
        03189E8DACDA9D0C0E0000000049454E44AE426082}
      Index = 0
    end
  end
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 447
    Align = alBottom
    PanelStyle.Active = True
    ParentFont = False
    Style.StyleController = DM.cxEditStyleController1
    TabOrder = 1
    Height = 120
    Width = 885
    object dxLayoutControl2: TdxLayoutControl
      Left = 2
      Top = 2
      Width = 881
      Height = 116
      Align = alClient
      TabOrder = 0
      LayoutLookAndFeel = DM.dxLayoutCxLookAndFeelMetropolisDark
      object dxLayoutControl2Group_Root: TdxLayoutGroup
        AlignHorz = ahClient
        AlignVert = avClient
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = -1
      end
      object dxLayoutGroup4: TdxLayoutGroup
        Parent = dxLayoutControl2Group_Root
        AlignHorz = ahCenter
        AlignVert = avClient
        CaptionOptions.Text = 'New Group'
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 0
      end
    end
  end
end

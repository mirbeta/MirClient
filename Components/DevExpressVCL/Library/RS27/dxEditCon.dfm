object FEditConnection: TFEditConnection
  Left = 389
  Top = 204
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Edit Connection'
  ClientHeight = 321
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 401
    Height = 321
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object MemoText: TcxMemo
      Left = 10
      Top = 28
      Lines.Strings = (
        'MemoText')
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Height = 57
      Width = 373
    end
    object cbSArrowStyle: TcxComboBox
      Left = 81
      Top = 122
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Width = 121
    end
    object cbDArrowStyle: TcxComboBox
      Left = 288
      Top = 122
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Width = 121
    end
    object sbFont: TcxButton
      Left = 389
      Top = 28
      Width = 29
      Height = 28
      Hint = 'Text Font'
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000002D744558745469746C6500466F6E743B436F6C6F723B4368616E6765
        3B426172733B526962626F6E3B5374616E646172643B5180D882000003144944
        4154785E7D916D4C935714C7FFCF6B29B4D452DC008515EAEB0CB200190607A6
        D1A0633420C3007580096CAB092023B0908D984517B21162AC6C3A15D1606304
        AAC41886C6C807D8D887651F741B41C5B80F6232AC66EC85DA97E71EBDF14918
        59B25F72CEB9F7E6FECF4B8E90B5FF2CA03110116E9F6E10DF3C70AE5C526287
        FF0E3C99FF6DFC943DE5ADBA2069009106D2343046B87FF963E840669128484F
        00401420D5F0B3A0AAAFD8DE28DD7DE742CB45477937E362FE4E8CE1DFC85A38
        0C468499F38D82A3B8D1A6118A448D698AA248220CFB00F8B54838CC78112E66
        84656C707BB1AEF22800489B6ABC8DAFD77E359DED393396D3E4A3B57B8E4413
        73AAD70310B184A0DF251EE56838021038522418AD0E05035785A869DAB02279
        971A639494B8A47A009F0260BA58FEA9C1ED5525A9EAE7EB531F8A7C84D94B6D
        A2A3A4DD110A2EE6071F3FBC7CFF5AEF15168D3E955515921A5BAB98938D7A55
        65CABDBBDF9260D99F5AE2B2AA5AE404528B3F03CFFAA2FDAE35EF76CF00B001
        B06E721F39BBB9FE0CD95D5D64CD74BF0720CEE7CAF3DD3DF03EFDF9ED25BAEA
        DA49F5A2D82F8381A384FE0957328D32ECAE2F022006413581310D3171262C48
        4ADD50757EA9233EAD22C9B91D13C74F62706C7CE80663CD002039DEE9702615
        B653FCDA9D5B0098B9C9B1D6A48DEEAF7FDF58758C6AF7B6B0C99E6E5AB83840
        23454EAA10A5E104205E1F0B86F492437D89B99E5F799B582236A3B8B3EF034F
        278D8E4ED2DCD367D453D742C5A2E05F0158B8F86DEE5ECD2EB7CB065305B1F0
        8F0004DD0C9F145A0ADB94FEADDB9EF42133370B9FF74EE0CB1FA49171280D7F
        007FCD9695B0450058B5FD20A5383BC996ED21F39AB22A0031AD5BCC4EFF3EC7
        E2BDD1269A3A68A76F7625506246B34F52575A795187310D790072F11213008B
        3EBBB120D5F0DA49D7CA4773531DB470CF47C70B8CD4BA599DD7B723121196C1
        1F7403AFDE55601A193FBC8DA6071BC9BBC3421D590A55A64B1FF1B196FE2D21
        6339ECD6C3A07F76E07B45C477AB431A7E098468686C8EDD04101604019CFF4B
        10197CA00D03B8A2AF48E3423D12FE0B9E03EA30520FCBA1698E000000004945
        4E44AE426082}
      ParentShowHint = False
      ShowHint = True
      SpeedButtonOptions.CanBeFocused = False
      SpeedButtonOptions.Flat = True
      SpeedButtonOptions.Transparent = True
      TabOrder = 1
      OnClick = sbFontClick
    end
    object pSourceColor: TPanel
      Left = 81
      Top = 197
      Width = 25
      Height = 25
      ParentBackground = False
      TabOrder = 5
      OnClick = pColorClick
    end
    object pColor: TPanel
      Left = 65
      Top = 290
      Width = 25
      Height = 25
      ParentBackground = False
      TabOrder = 8
      OnClick = pColorClick
    end
    object btnOK: TcxButton
      Left = 262
      Top = 333
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 13
    end
    object btnCancel: TcxButton
      Left = 343
      Top = 333
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 14
    end
    object seStrokeThickness: TcxSpinEdit
      Left = 65
      Top = 240
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Value = 1
      Width = 146
    end
    object seDArrowSize: TcxSpinEdit
      Left = 288
      Top = 147
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Value = 1
      Width = 121
    end
    object seSArrowSize: TcxSpinEdit
      Left = 81
      Top = 147
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Value = 1
      Width = 121
    end
    object seSLinkedPoint: TcxSpinEdit
      Left = 81
      Top = 172
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 121
    end
    object seDLinkedPoint: TcxSpinEdit
      Left = 288
      Top = 172
      Properties.MaxValue = 16.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Value = 1
      Width = 121
    end
    object pDestColor: TPanel
      Left = 288
      Top = 197
      Width = 25
      Height = 25
      Color = 16053234
      ParentBackground = False
      TabOrder = 12
      OnClick = pColorClick
    end
    object cbLineStyle: TcxComboBox
      Left = 65
      Top = 265
      Properties.DropDownListStyle = lsEditFixedList
      Properties.Items.Strings = (
        'Solid'
        'Dashed'
        'Dotted'
        'Dash-Dotted'
        'Dash-Double-Dotted')
      Properties.OnChange = MemoTextChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Width = 146
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object liText: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'Text'
      CaptionOptions.Layout = clTop
      Control = MemoText
      ControlOptions.OriginalHeight = 57
      ControlOptions.OriginalWidth = 345
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lgSource: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = ' Source '
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 0
    end
    object liArrowStyle: TdxLayoutItem
      Parent = lgSource
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Arrow Style'
      Control = cbSArrowStyle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 84
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgTarget: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = ' Destination '
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 1
    end
    object liArrowStyle2: TdxLayoutItem
      Parent = lgTarget
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Arrow Style'
      Control = cbDArrowStyle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 84
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignVert = avTop
      CaptionOptions.Text = ' '
      CaptionOptions.Layout = clTop
      Control = sbFont
      ControlOptions.OriginalHeight = 28
      ControlOptions.OriginalWidth = 29
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liSourceColor: TdxLayoutItem
      Parent = lgSource
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Color'
      Control = pSourceColor
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup3
      AlignHorz = ahClient
      Index = 0
      AutoCreated = True
    end
    object liColor: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Color'
      Control = pColor
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup9: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup9
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup9
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object liStrokeThickness: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      CaptionOptions.Text = 'Line Width'
      Control = seStrokeThickness
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liDArrowSize: TdxLayoutItem
      Parent = lgTarget
      AlignHorz = ahClient
      CaptionOptions.Text = 'Arrow Size'
      Control = seDArrowSize
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liSArrowSize: TdxLayoutItem
      Parent = lgSource
      AlignHorz = ahClient
      CaptionOptions.Text = 'Arrow Size'
      Control = seSArrowSize
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liSLinkedPoint: TdxLayoutItem
      Parent = lgSource
      AlignHorz = ahClient
      CaptionOptions.Text = 'Linked Point'
      Control = seSLinkedPoint
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liDLinkedPoint: TdxLayoutItem
      Parent = lgTarget
      AlignHorz = ahClient
      CaptionOptions.Text = 'Linked Point'
      Control = seDLinkedPoint
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liDestColor: TdxLayoutItem
      Parent = lgTarget
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Color'
      Control = pDestColor
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liLineStyle: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      CaptionOptions.Text = 'Line Style'
      Control = cbLineStyle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 268
    Top = 34
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 328
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ColorDialog: TdxColorDialog
    Options.ColorPicker.ShowHSLEditors = False
    Options.ColorPicker.DefaultVisible = True
    Left = 236
    Top = 34
  end
end

object FEditObject: TFEditObject
  Left = 368
  Top = 185
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Edit Object'
  ClientHeight = 414
  ClientWidth = 369
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
    Width = 369
    Height = 414
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object memoText: TcxMemo
      Left = 112
      Top = 100
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Height = 69
      Width = 203
    end
    object cbTextPosition: TcxComboBox
      Left = 112
      Top = 175
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 144
    end
    object cbShapeStyle: TcxComboBox
      Left = 112
      Top = 200
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Width = 144
    end
    object cbImagePosition: TcxComboBox
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akRight, akBottom]
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Visible = False
      Width = 175
    end
    object btnClear: TcxButton
      Left = 10000
      Top = 10000
      Width = 75
      Height = 25
      Caption = 'Clear Image'
      TabOrder = 13
      Visible = False
      OnClick = btnClearClick
    end
    object cbRaisedOut: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Raised outer edge'
      Style.HotTrack = False
      TabOrder = 14
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbRaisedIn: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Raised inner edge'
      Style.HotTrack = False
      TabOrder = 16
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbSunkenOut: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Sunken outer edge'
      Style.HotTrack = False
      TabOrder = 15
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbSunkenIn: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Sunken inner edge'
      Style.HotTrack = False
      TabOrder = 17
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbFlat: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Flat'
      Style.HotTrack = False
      TabOrder = 18
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbMono: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Mono'
      Style.HotTrack = False
      TabOrder = 19
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbLeft: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Left'
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 20
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbTop: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Top'
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 21
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbRight: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Right'
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 22
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbBottom: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Bottom'
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 23
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbSoft: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Soft'
      Style.HotTrack = False
      TabOrder = 24
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbAdjust: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Adjust'
      Style.HotTrack = False
      TabOrder = 25
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbMiddle: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Middle'
      Style.HotTrack = False
      TabOrder = 26
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object cbDiag: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Diagonal'
      Style.HotTrack = False
      TabOrder = 27
      Transparent = True
      Visible = False
      OnClick = seHeightChange
    end
    object btnOK: TcxButton
      Left = 210
      Top = 382
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 28
    end
    object btnCancel: TcxButton
      Left = 291
      Top = 382
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 29
    end
    object pBkColor: TPanel
      Left = 112
      Top = 310
      Width = 25
      Height = 25
      TabOrder = 9
      OnClick = pColorClick
    end
    object pColor: TPanel
      Left = 112
      Top = 279
      Width = 25
      Height = 25
      TabOrder = 8
      OnClick = pColorClick
    end
    object sbFont: TcxButton
      Left = 321
      Top = 100
      Width = 29
      Height = 28
      Hint = 'Font'
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
      TabOrder = 3
      OnClick = sbFontClick
    end
    object cbTransparent: TcxCheckBox
      Left = 112
      Top = 341
      Caption = 'Transparent'
      Style.HotTrack = False
      TabOrder = 10
      Transparent = True
      OnClick = seHeightChange
    end
    object seAngle: TcxSpinEdit
      Left = 112
      Top = 252
      Properties.MaxValue = 359.000000000000000000
      Properties.MinValue = -359.000000000000000000
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Width = 144
    end
    object seLineWidth: TcxSpinEdit
      Left = 112
      Top = 225
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Value = 1
      Width = 144
    end
    object seHeight: TcxSpinEdit
      Left = 112
      Top = 46
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Value = 1
      Width = 144
    end
    object seWidth: TcxSpinEdit
      Left = 112
      Top = 73
      Properties.MinValue = 1.000000000000000000
      Properties.OnChange = seHeightChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Value = 1
      Width = 144
    end
    object gcImages: TdxGalleryControl
      Left = 10000
      Top = 10000
      Width = 327
      Height = 252
      Visible = False
      OptionsBehavior.ItemCheckMode = icmSingleCheck
      OptionsView.Item.Image.ShowFrame = False
      OptionsView.Item.Text.Position = posBottom
      TabOrder = 11
      OnItemClick = gcImagesItemClick
      object gcgImages: TdxGalleryControlGroup
        ShowCaption = False
      end
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tsGeneral: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'General'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 5
      ShowBorder = False
      Index = 0
    end
    object Label1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.AlignVert = tavTop
      CaptionOptions.Text = 'Text'
      Control = memoText
      ControlOptions.OriginalHeight = 69
      ControlOptions.OriginalWidth = 203
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object Label2: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Text Layout'
      Control = cbTextPosition
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 144
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object Label3: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Shape Type'
      Control = cbShapeStyle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 144
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object tsImage: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Image'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = tsImage
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object Label5: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Text = 'Image Layout'
      Control = cbImagePosition
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnClear'
      CaptionOptions.Visible = False
      Control = btnClear
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object tsFrame: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'Frame'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutGroup7: TdxLayoutGroup
      Parent = tsFrame
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object GroupBox1: TdxLayoutGroup
      Parent = dxLayoutGroup7
      CaptionOptions.Text = ' Edge Style '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Raised outer edge'
      CaptionOptions.Visible = False
      Control = cbRaisedOut
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 109
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Raised inner edge'
      CaptionOptions.Visible = False
      Control = cbRaisedIn
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 107
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutGroup9
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Sunken outer edge'
      CaptionOptions.Visible = False
      Control = cbSunkenOut
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutGroup10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Sunken inner edge'
      CaptionOptions.Visible = False
      Control = cbSunkenIn
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object GroupBox2: TdxLayoutGroup
      Parent = dxLayoutGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = ' Border Style '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutGroup12: TdxLayoutGroup
      Parent = GroupBox2
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutGroup12
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbFlat'
      CaptionOptions.Visible = False
      Control = cbFlat
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 39
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = dxLayoutGroup12
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbMono'
      CaptionOptions.Visible = False
      Control = cbMono
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 47
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup13: TdxLayoutGroup
      Parent = GroupBox2
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutGroup13
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbLeft'
      CaptionOptions.Visible = False
      Control = cbLeft
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutGroup13
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbTop'
      CaptionOptions.Visible = False
      Control = cbTop
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 39
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutGroup13
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbRight'
      CaptionOptions.Visible = False
      Control = cbRight
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 46
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutGroup13
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbBottom'
      CaptionOptions.Visible = False
      Control = cbBottom
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 55
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup14: TdxLayoutGroup
      Parent = GroupBox2
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutGroup14
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbSoft'
      CaptionOptions.Visible = False
      Control = cbSoft
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 41
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutGroup14
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbAdjust'
      CaptionOptions.Visible = False
      Control = cbAdjust
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 52
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutGroup14
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbMiddle'
      CaptionOptions.Visible = False
      Control = cbMiddle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 51
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutGroup14
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cbDiag'
      CaptionOptions.Visible = False
      Control = cbDiag
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 62
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup3: TdxLayoutGroup
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
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object Label9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Background Color'
      Control = pBkColor
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = tsGeneral
      Index = 0
      AutoCreated = True
    end
    object Label8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Shape Color'
      Control = pColor
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'sbFont'
      CaptionOptions.Visible = False
      Control = sbFont
      ControlOptions.OriginalHeight = 28
      ControlOptions.OriginalWidth = 29
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutGroup4
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = ' '
      Control = cbTransparent
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liAngle: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Rotation Angle '
      Control = seAngle
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 144
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object liLineWidth: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Line Width'
      Control = seLineWidth
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 144
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object liHeight: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Height'
      Control = seHeight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 144
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liWidth: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Width'
      Control = seWidth
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 144
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup9: TdxLayoutGroup
      Parent = GroupBox1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup10: TdxLayoutGroup
      Parent = GroupBox1
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = tsImage
      CaptionOptions.Text = 'dxGalleryControl1'
      CaptionOptions.Visible = False
      Control = gcImages
      ControlOptions.OriginalHeight = 252
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 302
    Top = 6
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ColorDialog: TdxColorDialog
    Options.ColorPicker.DefaultVisible = True
    Left = 272
    Top = 6
  end
end

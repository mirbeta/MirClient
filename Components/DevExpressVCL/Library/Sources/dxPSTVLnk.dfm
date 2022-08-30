inherited dxfmTVReportLinkDesignWindow: TdxfmTVReportLinkDesignWindow
  Left = 358
  Top = 280
  Caption = 'dxfmTVReportLinkDesignWindow'
  ClientHeight = 336
  ClientWidth = 553
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 553
    Height = 336
    inherited btnApply: TcxButton
      Top = 282
      TabOrder = 26
    end
    inherited btnCancel: TcxButton
      Top = 282
      TabOrder = 25
    end
    inherited btnOK: TcxButton
      Top = 282
      TabOrder = 24
    end
    inherited btnHelp: TcxButton
      Top = 282
      TabOrder = 27
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 282
      TabOrder = 28
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 282
      TabOrder = 29
    end
    inherited btnTitleProperties: TcxButton
      Top = 282
      TabOrder = 30
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 282
      TabOrder = 31
    end
    object imgGrid: TcxImage [8]
      Left = 21
      Top = 84
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Height = 48
      Width = 48
    end
    object Image1: TcxImage [9]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 16
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object Image2: TcxImage [10]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object pnlPreview: TPanel [11]
      Left = 273
      Top = 45
      Width = 574
      Height = 230
      BevelOuter = bvNone
      FullRepaint = False
      ParentBackground = False
      TabOrder = 23
    end
    object lblShow: TcxLabel [12]
      Left = 21
      Top = 60
      AutoSize = False
      Caption = 'Show'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 234
    end
    object chbxShowBorders: TcxCheckBox [13]
      Left = 75
      Top = 84
      Caption = '&Border'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbxShowClick
      Width = 180
    end
    object chbxShowGrid: TcxCheckBox [14]
      Tag = 1
      Left = 75
      Top = 107
      Caption = '&Grid'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = chbxShowClick
      Width = 180
    end
    object chbxShowButtons: TcxCheckBox [15]
      Tag = 5
      Left = 75
      Top = 142
      Caption = 'Buttons'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = chbxShowClick
      Width = 180
    end
    object chbxShowTreeLines: TcxCheckBox [16]
      Tag = 4
      Left = 75
      Top = 165
      Caption = 'TreeLines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = chbxShowClick
      Width = 180
    end
    object chbxShowStateImages: TcxCheckBox [17]
      Tag = 2
      Left = 75
      Top = 200
      Caption = 'State Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = chbxShowClick
      Width = 180
    end
    object chbxShowImages: TcxCheckBox [18]
      Tag = 3
      Left = 75
      Top = 223
      Caption = 'Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = chbxShowClick
      Width = 180
    end
    object chbxTransparent: TcxCheckBox [19]
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Visible = False
      OnClick = chbxTransparentClick
      Width = 17
    end
    object stTransparent: TcxLabel [20]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' &Transparent '
      FocusControl = chbxTransparent
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stTransparentClick
      Height = 18
      Width = 211
      AnchorY = 10009
    end
    object ccbxColor: TcxColorComboBox [21]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 10
      Visible = False
      Width = 121
    end
    object ccbxGridLineColor: TcxColorComboBox [22]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 11
      Visible = False
      Width = 121
    end
    object ccbxTreeLineColor: TcxColorComboBox [23]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 12
      Visible = False
      Width = 121
    end
    object btnFont: TcxButton [24]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Fo&nt ...'
      TabOrder = 13
      Visible = False
      OnClick = btnFontClick
    end
    object edFont: TcxTextEdit [25]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      TabOrder = 14
      Text = 'edFont'
      Visible = False
      Width = 234
    end
    object lblExpanding: TcxLabel [26]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Expanding'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 234
    end
    object lblMiscellaneous: TcxLabel [27]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 234
    end
    object chbxAutoNodesExpand: TcxCheckBox [28]
      Left = 10000
      Top = 10000
      Caption = '&Auto Node Expanded'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Transparent = True
      Visible = False
      OnClick = chbxAutoNodesExpandClick
      Width = 180
    end
    object seExpandLevel: TcxSpinEdit [29]
      Left = 10000
      Top = 10000
      Properties.MaxValue = 100.000000000000000000
      Properties.MinValue = -1.000000000000000000
      Properties.OnChange = ExpandLevelChange
      Style.HotTrack = False
      TabOrder = 18
      Visible = False
      Width = 76
    end
    object chbxAutoWidth: TcxCheckBox [30]
      Left = 10000
      Top = 10000
      Caption = 'Auto &Width'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 21
      Transparent = True
      Visible = False
      OnClick = chbxAutoWidthClick
      Width = 180
    end
    object chbxNodeAutoHeight: TcxCheckBox [31]
      Left = 10000
      Top = 10000
      Caption = '&Node Auto Height'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      OnClick = chbxNodeAutoHeightClick
      Width = 180
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tshOptions: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshOptions'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object tshColors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshColors'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 1
    end
    object tshFonts: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFonts'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 2
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshBehaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 3
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Preview:'
      CaptionOptions.Layout = clTop
      SizeOptions.Height = 250
      SizeOptions.Width = 250
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 250
      ControlOptions.OriginalWidth = 250
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblShow
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      Control = imgGrid
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 56
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowGrid
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 43
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'Separator'
      Index = 5
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowButtons
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 61
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowTreeLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 70
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowStateImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 59
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = stTransparent
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = tshColors
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object lblColor: TdxLayoutItem
      Parent = tshColors
      CaptionOptions.Text = 'lblColor'
      Offsets.Left = 27
      Control = ccbxColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblGridLinesColor: TdxLayoutItem
      Parent = tshColors
      CaptionOptions.Text = 'lblGridLinesColor'
      Offsets.Left = 27
      Control = ccbxGridLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lblTreeLinesColor: TdxLayoutItem
      Parent = tshColors
      CaptionOptions.Text = 'lblTreeLinesColor'
      Offsets.Left = 27
      Control = ccbxTreeLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutSeparatorItem3: TdxLayoutSeparatorItem
      Parent = tshColors
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = tshFonts
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = tshFonts
      Control = edFont
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblExpanding
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblMiscellaneous
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      Control = Image2
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoNodesExpand
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 126
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lblExpandLevel: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblExpandLevel'
      Offsets.Left = 19
      Control = seExpandLevel
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 76
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      CaptionOptions.Visible = False
      Control = chbxNodeAutoHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 109
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ilPreview: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 16252936
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000000000000103030309000000100000001B000000220000
          001D0000000E0000000200000000000000000000000000000000000000010000
          00050000000C0000001500000020030E054C07270D8B0A4214C90D581AEF0A4B
          15DD03170674000000230000000D000000020000000000000000000000070410
          073C0A2D127F114E1EC0156726EC197A30FF1D883BFF229C4AFF26AD56FF066E
          1CFF0B641CFF0A4814DD03170671000000220000000D00000002000000102183
          37FF279647FF2BA553FF2DB35FFF2FBB64FF2EBA62FF2DB961FF2CB860FF0575
          20FF03721DFF066D1CFF0A621AFF084713DB0217066B0000000E0000000F2387
          3AFF36BF6CFF36BE69FF34BC67FF32BC66FF30BC65FF2FBA63FF2DB962FF0676
          21FF057520FF03711CFF03701CFF056B1AFF0B5D19FF0000001D0000000B258B
          3DFF3AC16EFF38BF6DFF36BE6BFF35BD69FF34BC67FF32BC66FF2FBB63FF077C
          24FF067922FF057520FF04741FFF03711BFF0C611BFF0000001E00000008278E
          40FF3CC371FF3BC26FFF3AC06EFF37BF6CFF35BE6CFF34BD68FF33BC66FF0880
          26FF077C24FF067A22FF05761FFF04721DFF0E641EFF00000019000000052890
          42FF3FC473FF3EC372FF3CC270FF3AC06FFF38BF6DFF36BE6BFF35BD68FF0981
          28FF088026FF077C25FF067A22FF06761FFF106721FF00000017000000032993
          43FF42C676FF40C574FF3EC472FF3DC371FF3BC270FF3AC06EFF38BF6CFF0B85
          2AFF098228FF088026FF077D25FF067A22FF136B24FF00000014000000022A94
          44FF44C87AFF43C677FF40C675FF3FC574FF51C981FF76D59BFFA2E1BAFF0C89
          2DFF0B862AFF09852AFF098026FF077E26FF156F27FF00000012000000012A94
          44FF5ACF88FF7CD8A0FFA6E4BEFFBCEBCFFFB3E9C8FF9CE4B8FF8FE4AEFF71D5
          95FF43B668FF1C953EFF0A822AFF098127FF18742BFF000000100000000143A1
          5BFFBBEBCEFFADE9C5FFAAE9C5FFA8EAC2FFA0E9BEFF99E8B8FF91E6B5FF8AE5
          ADFF81E3A8FF79E1A2FF61D18BFF39B261FF1A782EFF0000000E000000002A94
          44FF70C088FF9FDEB5FFB2EDCAFFADEBC5FFA7EAC1FF9FE8BEFF98E8B8FF91E6
          B2FF8AE5ADFF75D69AFF58BF7BFF3BA259FF1C7C31FF0000000A000000000617
          0A271549217F237C39D53FA259FF6DC085FF97DDAFFF94DFB0FF74C990FF4FAD
          6AFF32964CFF217734E1165625AA0C2E1466040F062900000003000000000000
          0000000000000000000006170A2915492280237B39D625813BDF1B5E2AA60E33
          1760051108240000000600000004000000030000000100000000000000000000
          0000000000000000000000000000000000010000000100000002000000020000
          0001000000010000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000003000000080000001100000019000000160000
          0009000000010000000000000000000000000000000000000000000000010000
          00030000000900000010050301232B16066A52290BAD7C3F10E964330DCD170B
          035300000016000000060000000100000000000000000000000000000004150A
          033249250A8970380FC6984E17FBA9602AFFB7723CFFC88650FFAC6027FF914A
          16FD52290BB4090501370000001000000004000000000000000000000009A652
          15FFC17941FFCE8C57FFDB9D6AFFDA9B67FFD79762FFD4925EFFBB6C33FFB766
          2DFFA3561EFF864413F63D1F0895020100230000000B0000000200000008A954
          16FFE9B07CFFE7AA78FFE3A573FFDEA16EFFDC9C69FFD79864FFBC6E35FFB96A
          31FFB5642BFFB05D24FF9A4E18FF73390FE32613056C0000000B00000006AC55
          16FFEEB684FFECB17EFFE7AC7AFFE4A874FFE1A36FFFDD9F6AFFBD7038FFBB6C
          33FFB8672EFFB36128FFB05C22FFA7551BFF894512FF0000001700000005B057
          17FFF3BB89FFF0B786FFEDB381FFE9AF7BFFE6A976FFE2A571FFBF723AFFBD6F
          37FFBA6A32FFB6652CFFB25F26FFAF5B20FF8B4612FF0000001800000004B258
          17FFF7BF90FFF4BC8BFFF0BA8AFFF1C399FFF1CAA6FFF3D3B4FFE5B994FFC781
          4DFFBB6D35FFB86830FFB46329FFB15E23FF8D4713FF0000001500000003B65A
          17FFFBD6B3FFFBDEC3FFFBE5D0FFF9E3CBFFF7DEC5FFF5DABDFFF4D5B6FFF1D1
          AFFFDCA77CFFBF723AFFB6662DFFB36127FF8F4813FF0000001100000001B85B
          18FFE3B794FFFAE7D2FFFBE7D2FFF9E2CCFFF7DEC5FFF5DABDFFF4D6B6FFF2D2
          B0FFF1CEABFFEDC6A0FFD29565FFB6652CFF934914FF0000000E000000002814
          0538763A0FA4B86427F9D69C71FFF2D6BAFFF7DEC4FFF6DABDFFF4D6B7FFF2D1
          B0FFF1CFABFFEDC8A2FFD7A77CFFBA7C4BFF964B14FF0000000A000000000000
          0000000000010F080217582B0B7EA45317E8C98754FFE7BE9AFFEBC4A1FFD097
          69FFB9753FFF9D5017F770380FBB4421097A140A032B00000003000000000000
          000000000000000000000000000002010003401F085A8C4512C9974C14DE5D2E
          0C8A2F1706480402010600000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end>
  end
end

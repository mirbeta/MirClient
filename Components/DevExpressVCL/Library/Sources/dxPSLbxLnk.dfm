inherited dxLBxReportLinkDesignWindow: TdxLBxReportLinkDesignWindow
  Left = 280
  Top = 273
  Caption = 'dxLbxReportLinkDesigner'
  ClientHeight = 274
  ClientWidth = 730
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 730
    Height = 274
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
      Properties.PopupMenuLayout.MenuItems = []
      Properties.ShowFocusRect = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Height = 48
      Width = 48
    end
    object PaintBox1: TPaintBox [9]
      Left = 21
      Top = 150
      Width = 48
      Height = 48
    end
    object Image1: TcxImage [10]
      Left = 10000
      Top = 10000
      Properties.PopupMenuLayout.MenuItems = []
      Properties.ShowFocusRect = False
      Style.TransparentBorder = False
      TabOrder = 17
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgMiscellaneous: TcxImage [11]
      Left = 10000
      Top = 10000
      Properties.PopupMenuLayout.MenuItems = []
      Properties.ShowFocusRect = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
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
      Width = 266
    end
    object chbxShowBorders: TcxCheckBox [13]
      Left = 75
      Top = 84
      Caption = 'Border'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 212
    end
    object chbxShowHorzLines: TcxCheckBox [14]
      Tag = 1
      Left = 75
      Top = 107
      Caption = 'Horizontal Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 212
    end
    object chbxPaintItemGraphics: TcxCheckBox [15]
      Left = 75
      Top = 150
      Caption = '&Paint item graphics'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = chbxPaintItemGraphicsClick
      Width = 212
    end
    object chbxTransparentGraphics: TcxCheckBox [16]
      Tag = 1
      Left = 75
      Top = 173
      Caption = '&Transparent Graphics'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = chbxTransparentGraphicsClick
      Width = 212
    end
    object pnlPreview: TPanel [17]
      Left = 305
      Top = 45
      Width = 542
      Height = 230
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      FullRepaint = False
      ParentBackground = False
      TabOrder = 23
      OnResize = pnlPreviewResize
    end
    object cbxDrawMode: TcxImageComboBox [18]
      Left = 10000
      Top = 10000
      Properties.Items = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Visible = False
      OnClick = cbxDrawModeClick
      Width = 200
    end
    object chbxTransparent: TcxCheckBox [19]
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
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
      Width = 243
      AnchorY = 10009
    end
    object ccbxColor: TcxColorComboBox [21]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 9
      Visible = False
      Width = 156
    end
    object ccbxEvenColor: TcxColorComboBox [22]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 10
      Visible = False
      Width = 156
    end
    object ccbxGridLineColor: TcxColorComboBox [23]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 11
      Visible = False
      Width = 156
    end
    object edFont: TcxTextEdit [24]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Visible = False
      Width = 266
    end
    object btnFont: TcxButton [25]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Fo&nt ...'
      TabOrder = 12
      Visible = False
      OnClick = btnFontClick
    end
    object btnEvenFont: TcxButton [26]
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'E&ven Font ...'
      TabOrder = 14
      Visible = False
      OnClick = btnFontClick
    end
    object edEvenFont: TcxTextEdit [27]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Visible = False
      Width = 266
    end
    object lblSelection: TcxLabel [28]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Selection'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 266
    end
    object chbxOnlySelected: TcxCheckBox [29]
      Left = 10000
      Top = 10000
      Caption = 'Only &selected cells'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
      Visible = False
      OnClick = chbxOnlySelectedClick
      Width = 212
    end
    object lblMiscellaneous: TcxLabel [30]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 266
    end
    object chbxAutoWidth: TcxCheckBox [31]
      Left = 10000
      Top = 10000
      Caption = 'AutoWidth'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 21
      Transparent = True
      Visible = False
      OnClick = chbxAutoWidthClick
      Width = 212
    end
    object chbxRowAutoHeight: TcxCheckBox [32]
      Left = 10000
      Top = 10000
      Caption = '&Row Auto Height'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      OnClick = chbxRowAutoHeightClick
      Width = 212
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
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
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object tshOptions: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Options'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      Control = imgGrid
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 52
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowHorzLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 95
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
      Parent = tshOptions
      CaptionOptions.Text = 'Separator'
      Index = 2
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxPaintItemGraphics
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 110
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      Control = PaintBox1
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Visible = False
      Control = chbxTransparentGraphics
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 123
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'lblPreview'
      CaptionOptions.Layout = clTop
      SizeOptions.Height = 250
      SizeOptions.Width = 250
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 250
      ControlOptions.OriginalWidth = 250
      Index = 1
    end
    object tshColor: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Color'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 1
    end
    object lblDrawMode: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblDrawMode'
      Control = cbxDrawMode
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = stTransparent
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = tshColor
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lblColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblColor'
      Offsets.Left = 27
      Control = ccbxColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblEvenColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblEvenColor'
      Offsets.Left = 27
      Control = ccbxEvenColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = tshColor
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
    object lblGridLinesColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblGridLinesColor'
      Offsets.Left = 27
      Control = ccbxGridLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object tshFont: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Font'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 2
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = tshFont
      Control = edFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnEvenFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = tshFont
      Control = edEvenFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Behaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 3
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSelection
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxOnlySelected
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblMiscellaneous
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahLeft
      Control = imgMiscellaneous
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Visible = False
      Control = chbxRowAutoHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 8
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ilPreview: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 524336
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
      end>
  end
end

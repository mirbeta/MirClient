inherited dxfmLVReportLinkDesignWindow: TdxfmLVReportLinkDesignWindow
  Left = 316
  Top = 170
  Caption = 'dxfmLVReportLinkDesignWindow'
  ClientHeight = 377
  ClientWidth = 577
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 577
    Height = 377
    inherited btnApply: TcxButton
      Top = 325
      TabOrder = 40
    end
    inherited btnCancel: TcxButton
      Top = 325
      TabOrder = 39
    end
    inherited btnOK: TcxButton
      Top = 325
      TabOrder = 38
    end
    inherited btnHelp: TcxButton
      Top = 325
      TabOrder = 41
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 325
      TabOrder = 42
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 325
      TabOrder = 43
    end
    inherited btnTitleProperties: TcxButton
      Top = 325
      TabOrder = 44
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 325
      TabOrder = 45
    end
    object imgGrid: TcxImage [8]
      Left = 21
      Top = 88
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Height = 48
      Width = 48
    end
    object Image1: TcxImage [9]
      Left = 21
      Top = 216
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Height = 48
      Width = 48
    end
    object Image2: TcxImage [10]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 26
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object Image3: TcxImage [11]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 30
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object Image4: TcxImage [12]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 34
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object pnlPreview: TPanel [13]
      Left = 270
      Top = 49
      Width = 577
      Height = 253
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 37
    end
    object pnlHint: TPanel [14]
      Left = 10
      Top = -3
      Width = 838
      Height = 27
      Alignment = taLeftJustify
      BevelOuter = bvNone
      BorderStyle = bsSingle
      Ctl3D = False
      ParentBackground = False
      ParentCtl3D = False
      TabOrder = 0
      object imgHint: TcxImage
        Left = 7
        Top = 5
        Enabled = False
        Picture.Data = {
          0B546478504E47496D61676589504E470D0A1A0A0000000D4948445200000010
          0000001008060000001FF3FF61000002A44944415478DA8D937F48535114C7CF
          7D6F732A5A8990823444FC438B4A13340C2DD42C2B2D8C1037FF51A719994659
          1AFDA0208AACBF148709922B29C35C36CB9C329222CA811AF4831493169334B5
          FC914BE7BBF776DE7220C2AA0B5FEE79E79EF3B987F3CE25E0798928254A402D
          A224145F19443C25B7968B19EA4030324C796B83EC5C3D35A2DFF93F0012154A
          FC6A0F2B7B366AF4918C01F41A8E0E6AAB9C0923DFF9049EB37F0114C632B170
          737C724DD8EE72E0A34530D03D0E9617B3E5C5B768359EFFFA1B80244492C06B
          39AA77313A43D08E7D27416076E8A821D0F3C86BBCB0DE1937FC8D7FC138EA09
          A0329E102FC6ECCCAA5027E64248E82E104415D87AB530D8791FBA2C8EDA9246
          5A817133EE862E0708054942587EEA9ABE98C2067F85D40E6999374154A8C174
          2F0BE686AEC3EBC7BE8E4B2D8EE49743BCCFDDD0E5009F07C7C5AAAD193A5D48
          EC5EE05FB570E6EA1C7624022E9F8D053A66804F6F7CE069D782A9AC991660FC
          B85C851B201E4B15366953D65A638FDC51C06C03B01F77C1D04C40546D004DA6
          1AA48976703A38BCEA5C456FB4FD3C68F9C8CC98374F96AAF033968A4DF19A8A
          3D419151C0ECD9C02409ACFD0210AF08885EEF0774CA0A0CC7C936A0800E0BB7
          9E32D2439867979395E7F68BA90792C24D5BF2F5029FAC043663760527E5C978
          6FE8AC0D0669F63370F42D2E00589F0770BD79BAF8C97B765B06F83F2C557425
          E65D890B500700B517E1EDD4159C5E82F9CA75D05A97064E5B1D50F451048C8E
          B8AA183C6DA2293260754BB1F259A2AE32DA57688245B954FCCB6C69FA95C139
          C0F17B6EB8D1E59727606A9280B95BB095B54ADB6580F7F97451B32D9CD43389
          803CFB9472E0B84B8CFC81A1CDD141D1E6E893EDB60FEC42733FAB7637D157AE
          441EA4A5D7B75264C53E8F1A434D7B7A8DFFBD7E03E1501B7D188E1D15000000
          0049454E44AE426082}
        Style.TransparentBorder = False
        TabOrder = 0
        Transparent = True
        Height = 16
        Width = 16
      end
      object lblHint: TcxLabel
        Left = 30
        Top = 3
        Caption = ' Most Options Are Being Taken Into Account Only In Detailed View'
        ParentFont = False
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -11
        Style.Font.Name = 'Tahoma'
        Style.Font.Style = [fsBold]
        Style.IsFontAssigned = True
        Transparent = True
      end
    end
    object lblShow: TcxLabel [15]
      Left = 21
      Top = 64
      AutoSize = False
      Caption = 'Show'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 231
    end
    object chbxShowBorders: TcxCheckBox [16]
      Left = 75
      Top = 88
      Caption = 'Border'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 177
    end
    object chbxShowHorzLines: TcxCheckBox [17]
      Tag = 1
      Left = 75
      Top = 111
      Caption = 'Horizontal Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 177
    end
    object chbxShowVertLines: TcxCheckBox [18]
      Tag = 2
      Left = 75
      Top = 134
      Caption = 'Vertical Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = chbxShowBordersClick
      Width = 177
    end
    object lblOnEveryPage: TcxLabel [19]
      Left = 21
      Top = 192
      AutoSize = False
      Caption = 'On Every Page'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 231
    end
    object chbxHeadersOnEveryPage: TcxCheckBox [20]
      Left = 75
      Top = 216
      Caption = 'Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      OnClick = chbxHeadersOnEveryPageClick
      Width = 177
    end
    object cbxDrawMode: TcxImageComboBox [21]
      Left = 10000
      Top = 10000
      Properties.Items = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Visible = False
      OnClick = cbxDrawModeClick
      Width = 165
    end
    object chbxTransparent: TcxCheckBox [22]
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Visible = False
      OnClick = chbxTransparentClick
      Width = 17
    end
    object stTransparent: TcxLabel [23]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' Transparent '
      FocusControl = chbxTransparent
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stTransparentClick
      Height = 19
      Width = 208
      AnchorY = 10010
    end
    object ccbxColor: TcxColorComboBox [24]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 13
      Visible = False
      Width = 121
    end
    object ccbxEvenColor: TcxColorComboBox [25]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 14
      Visible = False
      Width = 121
    end
    object chbxTransparentHeaders: TcxCheckBox [26]
      Tag = 1
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      Visible = False
      OnClick = chbxTransparentClick
      Width = 17
    end
    object stTransparentHeaders: TcxLabel [27]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Caption = ' Transaprent Headers '
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = stTransparentHeadersClick
      Height = 18
      Width = 208
      AnchorY = 10009
    end
    object ccbxHeadersColor: TcxColorComboBox [28]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 17
      Visible = False
      Width = 121
    end
    object ccbxGridLineColor: TcxColorComboBox [29]
      Tag = 3
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = ccbxColorChange
      Style.HotTrack = False
      TabOrder = 18
      Visible = False
      Width = 121
    end
    object btnHeadersFont: TcxButton [30]
      Tag = 2
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Headers Font...'
      TabOrder = 19
      Visible = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = btnFontClick
    end
    object edFixedFont: TcxTextEdit [31]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Visible = False
      Width = 231
    end
    object btnFont: TcxButton [32]
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'Fo&nt...'
      TabOrder = 21
      Visible = False
      OnClick = btnFontClick
    end
    object edFont: TcxTextEdit [33]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Visible = False
      Width = 231
    end
    object btnEvenFont: TcxButton [34]
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 100
      Height = 23
      Caption = 'E&ven Font...'
      TabOrder = 23
      Visible = False
      OnClick = btnFontClick
    end
    object edEvenFont: TcxTextEdit [35]
      Left = 10000
      Top = 10000
      TabStop = False
      Properties.ReadOnly = True
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Visible = False
      Width = 231
    end
    object lblSelection: TcxLabel [36]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Selection'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 231
    end
    object lblLookAndFeel: TcxLabel [37]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Look And Feel'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 231
    end
    object lblMiscellaneous: TcxLabel [38]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 231
    end
    object chbxOnlySelected: TcxCheckBox [39]
      Left = 10000
      Top = 10000
      Caption = 'Only &selected cells'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Transparent = True
      Visible = False
      OnClick = chbxOnlySelectedClick
      Width = 177
    end
    object chbxIncludeFixed: TcxCheckBox [40]
      Left = 10000
      Top = 10000
      Caption = '&Including fixed cells'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 28
      Transparent = True
      Visible = False
      OnClick = chbxIncludeFixedClick
      Width = 177
    end
    object chbxUse3DEffects: TcxCheckBox [41]
      Tag = 12
      Left = 10000
      Top = 10000
      Caption = '&Use 3D Effects'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 31
      Transparent = True
      Visible = False
      OnClick = chbxUse3DEffectsClick
      Width = 177
    end
    object chbxUseSoft3D: TcxCheckBox [42]
      Tag = 13
      Left = 10000
      Top = 10000
      Caption = 'Soft &3D'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 32
      Transparent = True
      Visible = False
      OnClick = chbxUseSoft3DClick
      Width = 177
    end
    object chbxAutoWidth: TcxCheckBox [43]
      Left = 10000
      Top = 10000
      Caption = 'AutoWidth'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 35
      Transparent = True
      Visible = False
      OnClick = chbxAutoWidthClick
      Width = 177
    end
    object chbxRowAutoHeight: TcxCheckBox [44]
      Left = 10000
      Top = 10000
      Caption = '&Row Auto Height'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 36
      Transparent = True
      Visible = False
      OnClick = chbxRowAutoHeightClick
      Width = 177
    end
    object chbxShowColumnHeaders: TcxCheckBox [45]
      Left = 75
      Top = 169
      Caption = '&Column Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = chbxShowColumnHeadersClick
      Width = 177
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 3
    end
    inherited dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Index = 2
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
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
    object dxLayoutItem2: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Visible = False
      Control = pnlHint
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 27
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 0
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
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      Control = imgGrid
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxShowHorzLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxShowVertLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Text = 'Separator'
      Index = 3
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object tshOptions: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Options'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object tshColor: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'Color'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 1
    end
    object tshFont: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFont'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 2
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshBehaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 3
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = tshOptions
      CaptionOptions.Visible = False
      Control = lblOnEveryPage
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxHeadersOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = tshOptions
      AlignVert = avClient
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object lblDrawMode: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblDrawMode'
      Control = cbxDrawMode
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxTransparent
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = stTransparent
      ControlOptions.OriginalHeight = 19
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
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxTransparentHeaders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 17
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = stTransparentHeaders
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshColor
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object lblHeadersColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblHeadersColor'
      Offsets.Left = 27
      Control = ccbxHeadersColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = tshColor
      AlignVert = avTop
      CaptionOptions.Text = 'Separator'
      Index = 6
    end
    object lblGridLinesColor: TdxLayoutItem
      Parent = tshColor
      CaptionOptions.Text = 'lblGridLinesColor'
      Offsets.Left = 27
      Control = ccbxGridLineColor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnHeadersFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = tshFont
      Control = edFixedFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = tshFont
      Control = edFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshFont
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnEvenFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = tshFont
      Control = edEvenFont
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSelection
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblLookAndFeel
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblMiscellaneous
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahLeft
      Control = Image2
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahLeft
      Control = Image3
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahLeft
      Control = Image4
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxOnlySelected
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 112
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Visible = False
      Control = chbxIncludeFixed
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxUse3DEffects
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxUseSoft3D
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object dxLayoutItem32: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      CaptionOptions.Visible = False
      Control = chbxRowAutoHeight
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object lichbxShowColumnHeaders: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxShowColumnHeaders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 98
      ControlOptions.ShowBorder = False
      Index = 4
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

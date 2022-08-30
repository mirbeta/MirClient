inherited frmRichEditMasterDetailMailMerge: TfrmRichEditMasterDetailMailMerge
  Caption = 'Rich Edit Master-Detail Mail Merge'
  ClientHeight = 762
  ClientWidth = 1134
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited Ribbon: TdxRibbon
    Width = 1134
    ApplicationButton.Menu = nil
    ApplicationButton.Visible = False
    object dxRibbonTabFile: TdxRibbonTab [0]
      Caption = 'File'
      Groups = <
        item
          Caption = 'Common'
          ToolbarName = 'dxBarCommon'
        end>
      Index = 0
    end
    object dxRibbonTabHome: TdxRibbonTab [1]
      Caption = 'Home'
      Groups = <
        item
          Caption = 'Clipboard'
          ToolbarName = 'dxBarClipboard'
        end
        item
          Caption = 'Font'
          ToolbarName = 'dxBarFont'
        end
        item
          Caption = 'Paragraph'
          ToolbarName = 'dxBarParagraph'
        end
        item
          Caption = 'Editing'
          ToolbarName = 'dxBarEditing'
        end>
      Index = 1
    end
    object rtMailMerge: TdxRibbonTab [2]
      Active = True
      Caption = 'Mail Merge'
      Groups = <
        item
          ToolbarName = 'bmbMailMerge'
        end
        item
          ToolbarName = 'bmbMergeToNewDocument'
        end>
      Index = 2
    end
    inherited rtAppearance: TdxRibbonTab
      Active = False
      Index = 3
    end
  end
  inherited rsbStatusBar: TdxRibbonStatusBar
    Top = 734
    Width = 1134
    Height = 28
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarToolbarPanelStyle'
      end>
  end
  object LayoutControl: TdxLayoutControl [2]
    Left = 0
    Top = 126
    Width = 1134
    Height = 608
    Align = alClient
    TabOrder = 6
    LayoutLookAndFeel = LayoutCxLookAndFeel
    object recTemplate: TdxRichEditControl
      Left = 10000
      Top = 10000
      Width = 901
      Height = 608
      Options.Fields.HighlightMode = Always
      Options.MailMerge.DataSource = dsTemplate
      TabOrder = 0
      Visible = False
      OnMailMergeStarted = TemplateMailMergeStarted
    end
    object recMaster: TdxRichEditControl
      Left = 10000
      Top = 10000
      Width = 300
      Height = 200
      Options.Fields.HighlightMode = Always
      Options.MailMerge.DataSource = dsMaster
      TabOrder = 1
      Visible = False
    end
    object recDetail: TdxRichEditControl
      Left = 10000
      Top = 10000
      Width = 300
      Height = 200
      Options.Fields.HighlightMode = Always
      Options.MailMerge.DataSource = dsDetail
      TabOrder = 2
      Visible = False
    end
    object recResultingDocument: TdxRichEditControl
      Left = 20
      Top = 44
      Width = 300
      Height = 200
      Options.DocumentSaveOptions.DefaultFormat = OpenXml
      Options.Fields.HighlightMode = Never
      TabOrder = 3
      OnCalculateDocumentVariable = ResultingDocumentCalculateDocumentVariable
    end
    object LayoutControlGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      LayoutLookAndFeel = LayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      LayoutDirection = ldTabbed
      ShowBorder = False
      OnTabChanged = LayoutControlGroup_RootTabChanged
      Index = -1
    end
    object liTemplate: TdxLayoutItem
      Parent = lgTemplate
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'RichEditControl'
      CaptionOptions.Visible = False
      Control = recTemplate
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgTemplate: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Template'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lgResultingDocument: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Resulting document'
      ButtonOptions.Buttons = <>
      Index = 3
    end
    object lgDetail: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Detail'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object lgMaster: TdxLayoutGroup
      Parent = LayoutControlGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Master'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object liMaster: TdxLayoutItem
      Parent = lgMaster
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'dxRichEditControl1'
      CaptionOptions.Visible = False
      Control = recMaster
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liDetail: TdxLayoutItem
      Parent = lgDetail
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'dxRichEditControl1'
      CaptionOptions.Visible = False
      Control = recDetail
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liResultingDocument: TdxLayoutItem
      Parent = lgResultingDocument
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'dxRichEditControl1'
      CaptionOptions.Visible = False
      Control = recResultingDocument
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  inherited bmBarManager: TdxBarManager
    Left = 464
    Top = 312
    DockControlHeights = (
      0
      0
      0
      0)
    inherited dxbQATOptions: TdxBar
      Visible = False
    end
    object bmbMailMerge: TdxBar [3]
      Caption = 'Mail Merge'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 960
      FloatTop = 8
      FloatClientWidth = 130
      FloatClientHeight = 108
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bbShowAllFieldCodes'
        end
        item
          Visible = True
          ItemName = 'bbShowAllFieldResults'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object bmbMergeToNewDocument: TdxBar [4]
      Caption = 'Merge to ...'
      CaptionButtons = <>
      DockedLeft = 167
      DockedTop = 0
      FloatLeft = 1168
      FloatTop = 8
      FloatClientWidth = 94
      FloatClientHeight = 54
      ItemLinks = <
        item
          Visible = True
          ItemName = 'bMergeToNewDocument'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = False
      WholeRow = False
    end
    object dxBarCommon: TdxBar [5]
      Caption = 'Common'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 1168
      FloatTop = 8
      FloatClientWidth = 56
      FloatClientHeight = 216
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000040000
        00130000001E0000002000000020000000200000002100000021000000210000
        002200000022000000220000002300000021000000160000000500000012281E
        16CB37291EFF463A31FFBD8150FFBC7E4DFFB97949FFB67646FFB37141FFB06D
        3DFFAD6839FFAB6535FF42362EFF3D3026FF241A13CE00000015000000193B2C
        21FF685C54FF483C34FFE8C28BFFE7C088FFE6BD85FFE5BB81FFE4B87CFFE3B5
        79FFE2B276FFE2B273FF443931FF51433AFF34261DFF0000001E000000183E2F
        24FF6C6057FF4A3F37FFD9B27DFFD8B07BFFD7AE77FFD7AB74FFD6A970FFD5A6
        6DFFD4A56AFFD4A268FF473B33FF5B4F47FF37291EFF0000001D000000164031
        26FF6F645CFF4C4038FFFFFFFFFFF7F1EBFFF7F0EBFFF7F0EBFFF7EFEBFFF6EF
        EAFFF6EFEAFFF6EFE9FF463B34FF5D5249FF3A2C21FF0000001B000000144434
        29FF73675FFF4F443CFFFFFFFFFFF8F2EDFFF8F1EDFFF7F1EDFFF7F0EDFFF8F1
        EBFFF7F0EBFFF7F0ECFF4A4037FF5F534BFF3D2E23FF00000019000000124637
        2CFF776B63FF50453DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF4E433BFF61544CFF403126FF0000001700000010493A
        2FFF796E66FF50453DFF61564EFF60564EFF60554DFF5F544CFF5E544CFF5E53
        4BFF5D524AFF5C5249FF5B5149FF61554DFF433429FF000000150000000E4C3D
        32FF7C706AFF674E44FF654B42FF634A41FF61473FFF5F473EFF5C443CFF5B43
        3AFF594139FF584038FF573F37FF63574FFF46362DFF000000130000000D4E3F
        35FF80746DFF6B5248FFF4ECE6FFE9DACEFFE9D8CDFFE9D8CCFFE9D8CBFFE8D7
        CAFFF3EAE2FFF3E9E2FF5A4139FF645850FF483A2FFF000000110000000B5142
        36FF827770FF70564DFFF9F5F2FFF4EAE4FFF1E6DEFFEBDCD2FFE9D9CCFF4E41
        3DFF60534CFFF3EAE3FF5D453CFF655951FF4C3D32FF0000000F000000095344
        39FF857A73FF755A50FFFAF6F3FFF5EDE7FFF4EDE6FFF4ECE6FFEFE2DAFF493D
        38FF5A4D46FFF4EBE4FF60483FFF655A52FF4F3F34FF0000000D000000075545
        3AFF887D76FF795E54FFFAF6F4FFF5EEE9FFF5EDE7FFF4EDE7FFF4ECE6FF473A
        36FF483D36FFE9D9CDFF644C43FF675A52FF514137FF0000000B000000065748
        3DFF898079FF7C6157FFFAF7F4FFFAF6F4FFFAF6F4FFFAF6F3FFFAF6F3FFFAF5
        F2FFF5EEE9FFF4ECE6FF695046FF82776FFF534439FF00000009000000034235
        2EC058493DFF7F645AFF998178FF967F75FF937A72FF8E786DFF8B7269FF866E
        64FF82695FFF7D645BFF6E544AFF56453BFF3F332BC200000005000000000000
        0002000000030000000400000004000000040000000400000005000000050000
        0005000000050000000500000006000000060000000400000001}
      ItemLinks = <
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonNew'
        end
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonOpen'
        end
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonSave'
        end
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonSaveAs'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarClipboard: TdxBar [6]
      Caption = 'Clipboard'
      CaptionButtons = <>
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 1168
      FloatTop = 8
      FloatClientWidth = 51
      FloatClientHeight = 102
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000030000000C00000010000000110000
        0011000000120000001200000013000000130000001000000004000000000000
        00000000000000000000000000000000000BA97563FFA87462FFA77261FFA571
        5FFFA5705EFFA36F5DFFA36D5CFFA26D5BFFA26C5AFF0000000F000000000000
        00000000000000000000000000000000000DAB7866FFFDFBF8FFF7EFE8FFF6EF
        E6FFF6EEE6FFF5EEE5FFF5EDE4FFF5EDE4FFA36D5CFF00000013000000000000
        0000265080B7336CB1FF326BB0FF2E63A6FFAD7C6AFFFDFCFAFFF7F0E9FFF7F0
        E8FFF7EFE7FFF6EFE7FFF6EEE6FFF5EEE5FFA46F5DFF00000012000000000000
        00003976B8FF91D2F4FF66BEEEFF60B5E4FFB07F6DFFFEFDFCFFF7F2EAFFF8F1
        E9FFF7F0E9FFF6F0E8FFF6EFE7FFF6EFE6FFA67260FF00000011000000000000
        00003F7DBCFF9AD6F5FF6CC3F0FF66B8E5FFB28070FFFEFEFDFFF8F3ECFFF8F2
        EBFFF7F1EAFFF7F1E9FFF7F0E8FFF7EFE8FFA87563FF00000010000000000000
        00004384C1FFA4DBF6FF73C7F1FF6CBEE8FFB48473FFFFFFFEFFF9F3EEFFF9F3
        EDFFF9F2ECFFF8F2EBFFF7F1EAFFF7F0E9FFAB7766FF0000000F000000000000
        0000488BC5FFADE1F8FF7ACCF2FF73C3E9FFB68676FFFFFFFFFFF9F5EEFFF9F4
        EEFFF9F3EDFFF8F3ECFFF8F2EBFFF7F2EBFFAC7A6AFF0000000E000000000000
        00004C92CAFFB6E6FAFF81D2F4FF7AC9EBFFB88878FFFFFFFFFFFAF5F0FFF9F5
        EFFFF9F5EFFFF9F4EEFFF9F3EDFFF9F3ECFFAF7E6CFF0000000D000000000000
        00005098CDFFBEE9FAFF88D6F6FF81CEEEFFBB8B7BFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFCFAFFB1816FFF0000000C000000000000
        0000539DD1FFC5EDFBFF8FDBF7FF89D5F2FFBD8D7DFFBB8C7CFFBA8B7BFFB989
        79FFB98977FFB78876FFB68674FFB58373FFB38371FF00000009000000000000
        000056A1D4FFCBF0FCFF95E0F8FF82CBE9FF72B7D9FF71B7D9FF70B7DAFF70B8
        DBFF6FB7DBFF4385BBFF00000009000000090000000800000002000000000000
        000058A5D7FFCBEDF8FF73B2D0FF5792B7FF5893B7FF5995BAFF5B99BDFF62A3
        C9FF69AFD4FF4184BBFF00000000000000000000000000000000000000000000
        000059A8D9FFBCDBE7FF8E7875FF9B7061FF946A5BFF8F6456FF885D50FF7B65
        63FF97BCD3FF488EC4FF00000000000000000000000000000000000000000000
        0000437EA2BD4A90BFFFB48E7FFFF3EDE7FFDEC9B8FFDDC8B5FFDBC4B2FF9770
        60FF3B7BAEFF40799EBD00000000000000000000000000000000000000000000
        00000000000000000000876B60BDB69181FFB69080FFB58E7FFFB28C7DFF7D60
        54BD000000000000000000000000000000000000000000000000}
      ItemLinks = <
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonPaste'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonCut'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonCopy'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonSelectAll'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarFont: TdxBar [7]
      Caption = 'Font'
      CaptionButtons = <>
      DockedLeft = 128
      DockedTop = 0
      FloatLeft = 1168
      FloatTop = 8
      FloatClientWidth = 145
      FloatClientHeight = 166
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000000000
        00310000003400000036000000380000003B0000003D00000040000000430000
        0044000000470000004A0000004C000000500000005200000000000000000000
        001400000016000000190000001B0000001D0000001E00000021000000230000
        0026000000280000002A0000002D0000002F0000003200000000000000000000
        00030000000400000005000000060000000700000008000000090000000B0000
        000C0000000D0000000F00000011000000130000001500000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000683C0FDC774411FF5E350DCB0000000000000000000000002B18
        05605C330AD16F3D0BFF6E3C0AFF000000000000000000000000000000000000
        000000000000010100036D3F0EE7120A03270000000000000000000000000302
        000665370CE7703E0CFF1D100342000000000000000000000000000000000000
        0000000000000000000041250A8744280A900000000000000000000000002817
        055773410EFF5E350BD500000000000000000000000000000000000000000000
        00000000000000000000100903217A4713FF794512FF784511FF774410FF7642
        10FF74410FFF311B066C00000000000000000000000000000000000000000000
        000000000000000000000000000058340EB72E1B076000000000150C032D7744
        11FF6F400DF30704010F00000000000000000000000000000000000000000000
        000000000000000000000000000028170751603810C60000000049290B997845
        12FF452709960000000000000000000000000000000000000000000000000000
        0000000000000000000000000000010100036F4011E71C100439734410F37A46
        12FF150C032D0000000000000000000000000000000000000000000000000000
        00000000000000000000000000000000000040250B815F3610C17C4814FF5D36
        0FC3000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000F09031E7D4716FC7E4915FF2B19
        0757000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000000000000000000005A3510B4784615F00302
        0006000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000}
      ItemLinks = <
        item
          UserDefine = [udWidth]
          UserWidth = 120
          ViewLevels = [ivlControlOnly]
          Visible = True
          ItemName = 'cxBarEditItemFont'
        end
        item
          Position = ipContinuesRow
          UserDefine = [udWidth]
          UserWidth = 40
          ViewLevels = [ivlControlOnly]
          Visible = True
          ItemName = 'cxBarEditItemFontSize'
        end
        item
          ButtonGroup = bgpStart
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonGrowFont'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonShrinkFont'
        end
        item
          ButtonGroup = bgpStart
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarSubItem1'
        end
        item
          ButtonGroup = bgpStart
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonBold'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonItalic'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonUnderline'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonDoubleUnderline'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonStrikethrough'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonDoubleStrikethrough'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonSubscript'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonSuperscript'
        end
        item
          ButtonGroup = bgpStart
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxRibbonColorGalleryItemTextHighlightColor'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxRibbonColorGalleryItemFontColor'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarParagraph: TdxBar [8]
      Caption = 'Paragraph'
      CaptionButtons = <>
      DockedLeft = 406
      DockedTop = 0
      FloatLeft = 1168
      FloatTop = 8
      FloatClientWidth = 105
      FloatClientHeight = 96
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000008888
        888AB8B8B8C1B6B6B6C2B6B6B6C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5
        B5C2B5B5B5C3B5B5B5C3B5B5B5C3B7B7B7C28787878A0000000000000000BBBB
        BBC15F483EFF4B3328FF493328FF483227FF463026FF452F26FF432E25FF422D
        25FF412C24FF402C22FF3F2B22FF231813FFB9B9B9C100000000000000008D8D
        8D8FB9B9B9C1B7B7B7C2B5B5B5C2B1B1B1C4AFAFAFC5AFAFAFC5AEAEAEC5AEAE
        AEC5AFAFAFC4B4B4B4C3B5B5B5C2B8B8B8C28C8C8C8F00000000000000000000
        00000000000000000000BBBBBBC160493FFF493328FF473127FF463026FF442F
        26FF291C17FFBABABAC100000000000000000000000000000000000000008888
        888ABABABAC1B8B8B8C1B7B7B7C2B2B2B2C3B1B1B1C4B0B0B0C4B0B0B0C4B0B0
        B0C4B1B1B1C4B5B5B5C2B7B7B7C2B8B8B8C18888888A0000000000000000BBBB
        BBC0685245FF4F382CFF4E372CFF4C352BFF4B342AFF4A3328FF493328FF4832
        27FF473026FF452F26FF432E25FF2C1E18FFBBBBBBC100000000000000008D8D
        8D8FBBBBBBC1B9B9B9C1B8B8B8C2B5B5B5C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2
        B2C4B3B3B3C3B6B6B6C2B8B8B8C2B9B9B9C18D8D8D8F00000000000000000000
        00000000000000000000BCBCBCC0695346FF4D372CFF4D362AFF4B3429FF4B33
        28FF35241DFFBBBBBBC000000000000000000000000000000000000000008888
        8889BBBBBBC0BBBBBBC1B9B9B9C1B6B6B6C2B5B5B5C3B5B5B5C3B4B4B4C3B4B4
        B4C3B5B5B5C3B8B8B8C2B9B9B9C1BABABAC18888888A0000000000000000BDBD
        BDC071594CFF543C2FFF533B2EFF513A2DFF50392DFF4F382CFF4E372BFF4C35
        2BFF4B342AFF4B3329FF493328FF37271EFFBBBBBBC000000000000000008D8D
        8D8EBCBCBCC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
        BBC1BABABAC1BABABAC1BABABAC1BBBBBBC18D8D8D8F00000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000}
      ItemLinks = <
        item
          ButtonGroup = bgpStart
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonBullets'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonNumbering'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonMultilevellist'
        end
        item
          ButtonGroup = bgpStart
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonDecreaseIndent'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonIncreaseIndent'
        end
        item
          ButtonGroup = bgpStart
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonShowHide'
        end
        item
          ButtonGroup = bgpStart
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonAlignTextLeft'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonCenter'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonAlignTextRight'
        end
        item
          ButtonGroup = bgpMember
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonJustify'
        end
        item
          ButtonGroup = bgpStart
          Position = ipContinuesRow
          ViewLevels = [ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarSubItem2'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    object dxBarEditing: TdxBar [9]
      Caption = 'Editing'
      CaptionButtons = <>
      DockedLeft = 565
      DockedTop = 0
      FloatLeft = 1168
      FloatTop = 8
      FloatClientWidth = 51
      FloatClientHeight = 132
      Glyph.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000020000
        000A000000160000001B000000170000000B0000000200000000000000010000
        00060000000B0000000F0000000C0000000600000001000000000000000F2616
        1072583424D8693D2AFF4D2C1FD91D110B750000001200000003000000082416
        1162513023CF613927FF45281BD01A0F0A630000000900000001291A136DA380
        70FFD4C5BDFFEDE5E1FFCFBEB7FF907163FF1F120C770000000F2C1C1663A181
        72FFD4C6BEFFEEE8E4FFCDC0B8FF866A5DFF190E096000000005734937DEE3D7
        D1FFCEAE9EFFAF7957FFD0B39EFFD7C9C3FF553324E00000001B77503FD8E2D7
        D2FFD0AD9BFFB37753FFD2B29AFFD6CBC5FF4A2B1ED90000000A8E5A47FBF8F6
        F4FFAE725BFFE7AF66FFB27F5EFFF0EBE8FF744835FF000000489A6B55FFF8F6
        F3FFB27056FFE8AF64FFB67E59FFEAE3DFFF613B29FA0000000B90604DF8F1E8
        E4FFD2B0A5FFAA6C53FFD4B6A7FFCDC0BAFF71422FFF4C2920FF80513DFFF1E8
        E3FFD3ADA1FFAE684FFFD4B2A1FFC5B6AFFF6A4231F700000009583C31A2CAAF
        A3FFF7F2EEFFFDFCFBFFEAE0DAFF9E847AFF774835FF663E36FF855944FFE8D9
        CFFFF7F0EDFFFCFAF9FFDFD4CEFF8A6657FF452D229A000000050805041D9F72
        5DF8E8DBD4FFE6D7CEFFCDB7AAFF8B6B5EFF7D4C3AFF865F56FF8B5F4BFFE9D8
        CEFFEADCD4FFCFB9ACFF997B6DFF966A54F70705041600000001000000076047
        3BA4D0B7ADFFEEE3DFFFD5C1B7FF93766CFF82533FFF734635FF926550FFECE0
        D9FFEBDED8FFC4B0A5FF9B7868FF5F45389D0000000400000000000000022219
        1541B89383FFF3EBE8FFD9C9C2FFA99389FFA67966FF00000026B48B76FFF0E7
        E3FFEADED9FFBDA79FFFA67D6CFF211915390000000100000000000000000000
        0007846554C6DDCCC4FFE6DCD8FFC2A99EFFA67E69F90000000CB38E79F8E0D1
        C9FFE9DFDAFFBCA497FF846555C2000000030000000000000000000000000000
        0002130F0D237D6152B7AF8673FA7B6050B9130E0C270000000414100E1E8365
        58B3B68F79F9806455B3130F0D1E000000010000000000000000000000000000
        0000000000010000000400000006000000050000000200000000000000000000
        0001000000010000000100000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000}
      ItemLinks = <
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonFind'
        end
        item
          ViewLevels = [ivlSmallIconWithText, ivlSmallIcon, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarButtonReplace'
        end
        item
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarLargeButtonUndo'
        end
        item
          Position = ipContinuesRow
          ViewLevels = [ivlLargeControlOnly, ivlControlOnly]
          Visible = True
          ItemName = 'dxBarLargeButtonRedo'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = False
    end
    inherited bbRibbonForm: TdxBarLargeButton
      LargeImageIndex = 42
      ImageIndex = 42
    end
    inherited bbApplicationButton: TdxBarLargeButton
      LargeImageIndex = 43
      ImageIndex = 43
    end
    inherited bbQATVisible: TdxBarLargeButton
      LargeImageIndex = 44
      ImageIndex = 44
    end
    object bbShowAllFieldCodes: TdxBarLargeButton
      Action = ShowAllFieldCodes
      Category = 0
      ScreenTip = stShowAllFieldCodes
    end
    object bbShowAllFieldResults: TdxBarLargeButton
      Action = acShowAllFieldResults
      Category = 0
      ScreenTip = stShowAllFieldResults
    end
    object bMergeToNewDocument: TdxBarLargeButton
      Caption = 'Merge To New'
      Category = 0
      Hint = 'Merge To New'
      Visible = ivAlways
      LargeImageIndex = 41
      OnClick = bMergeToNewDocumentClick
      SyncImageIndex = False
      ImageIndex = 41
    end
    object dxBarLargeButtonNew: TdxBarLargeButton
      Action = dxRichEditControlNewDocument
      Category = 0
      SyncImageIndex = False
      ImageIndex = 160
    end
    object dxBarLargeButtonOpen: TdxBarLargeButton
      Action = dxRichEditControlLoadDocument
      Category = 0
      SyncImageIndex = False
      ImageIndex = 161
    end
    object dxBarLargeButtonSave: TdxBarLargeButton
      Action = dxRichEditControlSaveDocument
      Category = 0
      SyncImageIndex = False
      ImageIndex = 162
    end
    object dxBarLargeButtonSaveAs: TdxBarLargeButton
      Action = dxRichEditControlSaveDocumentAs
      Category = 0
      SyncImageIndex = False
      ImageIndex = 163
    end
    object dxBarLargeButtonPaste: TdxBarLargeButton
      Action = dxRichEditControlPasteSelection
      Category = 0
      SyncImageIndex = False
      ImageIndex = 164
    end
    object dxBarButtonCut: TdxBarButton
      Action = dxRichEditControlCutSelection
      Category = 0
      LargeImageIndex = 165
    end
    object dxBarButtonCopy: TdxBarButton
      Action = dxRichEditControlCopySelection
      Category = 0
      LargeImageIndex = 166
    end
    object dxBarButtonSelectAll: TdxBarButton
      Action = dxRichEditControlSelectAll
      Category = 0
      LargeImageIndex = 167
    end
    object cxBarEditItemFont: TcxBarEditItem
      Action = dxRichEditControlChangeFontName
      Category = 0
      PropertiesClassName = 'TcxFontNameComboBoxProperties'
      Properties.FontPreview.ShowButtons = False
    end
    object cxBarEditItemFontSize: TcxBarEditItem
      Action = dxRichEditControlChangeFontSize
      Category = 0
      PropertiesClassName = 'TcxComboBoxProperties'
      Properties.DropDownRows = 12
      Properties.Items.Strings = (
        '8'
        '9'
        '10'
        '11'
        '12'
        '14'
        '16'
        '18'
        '20'
        '22'
        '24'
        '26'
        '28'
        '36'
        '48'
        '72')
    end
    object dxBarButtonGrowFont: TdxBarButton
      Action = dxRichEditControlIncreaseFontSize
      Category = 0
      LargeImageIndex = 170
    end
    object dxBarButtonShrinkFont: TdxBarButton
      Action = dxRichEditControlDecreaseFontSize
      Category = 0
      LargeImageIndex = 171
    end
    object dxBarSubItem1: TdxBarSubItem
      Caption = 'Change Case'
      Category = 0
      Visible = ivAlways
      ImageIndex = 12
      LargeImageIndex = 172
      ItemLinks = <
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonUPPERCASE'
        end
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonlowercase'
        end
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtontOGGLEcASE'
        end>
    end
    object dxBarLargeButtonUPPERCASE: TdxBarLargeButton
      Action = dxRichEditControlTextUpperCase
      Category = 0
      SyncImageIndex = False
      ImageIndex = -1
    end
    object dxBarLargeButtonlowercase: TdxBarLargeButton
      Action = dxRichEditControlTextLowerCase
      Category = 0
      SyncImageIndex = False
      ImageIndex = -1
    end
    object dxBarLargeButtontOGGLEcASE: TdxBarLargeButton
      Action = dxRichEditControlToggleTextCase
      Category = 0
      SyncImageIndex = False
      ImageIndex = -1
    end
    object dxBarButtonBold: TdxBarButton
      Action = dxRichEditControlToggleFontBold
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 173
    end
    object dxBarButtonItalic: TdxBarButton
      Action = dxRichEditControlToggleFontItalic
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 174
    end
    object dxBarButtonUnderline: TdxBarButton
      Action = dxRichEditControlToggleFontUnderline
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 175
    end
    object dxBarButtonDoubleUnderline: TdxBarButton
      Action = dxRichEditControlToggleFontDoubleUnderline
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 176
    end
    object dxBarButtonStrikethrough: TdxBarButton
      Action = dxRichEditControlToggleFontStrikeout
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 177
    end
    object dxBarButtonDoubleStrikethrough: TdxBarButton
      Action = dxRichEditControlToggleFontDoubleStrikeout
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 178
    end
    object dxBarButtonSubscript: TdxBarButton
      Action = dxRichEditControlToggleFontSubscript
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 179
    end
    object dxBarButtonSuperscript: TdxBarButton
      Action = dxRichEditControlToggleFontSuperscript
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 180
    end
    object dxRibbonColorGalleryItemTextHighlightColor: TdxRibbonColorGalleryItem
      Action = dxRichEditControlTextHighlight
      Category = 0
      LargeImageIndex = 181
    end
    object dxRibbonColorGalleryItemFontColor: TdxRibbonColorGalleryItem
      Action = dxRichEditControlChangeFontColor
      Category = 0
      LargeImageIndex = 182
    end
    object dxBarButtonBullets: TdxBarButton
      Action = dxRichEditControlToggleBulletedList
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 183
    end
    object dxBarButtonNumbering: TdxBarButton
      Action = dxRichEditControlToggleSimpleNumberingList
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 184
    end
    object dxBarButtonMultilevellist: TdxBarButton
      Action = dxRichEditControlToggleMultiLevelList
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 185
    end
    object dxBarButtonDecreaseIndent: TdxBarButton
      Action = dxRichEditControlDecrementIndent
      Category = 0
      LargeImageIndex = 186
    end
    object dxBarButtonIncreaseIndent: TdxBarButton
      Action = dxRichEditControlIncrementIndent
      Category = 0
      LargeImageIndex = 187
    end
    object dxBarButtonShowHide: TdxBarButton
      Action = dxRichEditControlToggleShowWhitespace
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 188
    end
    object dxBarButtonAlignTextLeft: TdxBarButton
      Action = dxRichEditControlToggleParagraphAlignmentLeft
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 189
    end
    object dxBarButtonCenter: TdxBarButton
      Action = dxRichEditControlToggleParagraphAlignmentCenter
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 190
    end
    object dxBarButtonAlignTextRight: TdxBarButton
      Action = dxRichEditControlToggleParagraphAlignmentRight
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 191
    end
    object dxBarButtonJustify: TdxBarButton
      Action = dxRichEditControlToggleParagraphAlignmentJustify
      Category = 0
      ButtonStyle = bsChecked
      LargeImageIndex = 192
    end
    object dxBarSubItem2: TdxBarSubItem
      Caption = 'Line Spacing'
      Category = 0
      Visible = ivAlways
      ImageIndex = 33
      LargeImageIndex = 193
      ItemLinks = <
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButton1'
        end
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButton2'
        end
        item
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButton3'
        end
        item
          BeginGroup = True
          Position = ipContinuesRow
          Visible = True
          ItemName = 'dxBarLargeButtonParagraph'
        end>
    end
    object dxBarLargeButton1: TdxBarLargeButton
      Action = dxRichEditControlSetSingleParagraphSpacing
      Category = 0
      ButtonStyle = bsChecked
      SyncImageIndex = False
      ImageIndex = -1
    end
    object dxBarLargeButton2: TdxBarLargeButton
      Action = dxRichEditControlSetSesquialteralParagraphSpacing
      Category = 0
      ButtonStyle = bsChecked
      SyncImageIndex = False
      ImageIndex = -1
    end
    object dxBarLargeButton3: TdxBarLargeButton
      Action = dxRichEditControlSetDoubleParagraphSpacing
      Category = 0
      ButtonStyle = bsChecked
      SyncImageIndex = False
      ImageIndex = -1
    end
    object dxBarLargeButtonParagraph: TdxBarLargeButton
      Action = dxRichEditControlShowParagraphForm
      Category = 0
      SyncImageIndex = False
      ImageIndex = 194
    end
    object dxBarButtonFind: TdxBarButton
      Action = dxRichEditControlSearchFind
      Category = 0
      LargeImageIndex = 195
    end
    object dxBarButtonReplace: TdxBarButton
      Action = dxRichEditControlSearchReplace
      Category = 0
      LargeImageIndex = 196
    end
    object dxBarLargeButtonUndo: TdxBarLargeButton
      Action = dxRichEditControlUndo
      Category = 0
      SyncImageIndex = False
      ImageIndex = 197
    end
    object dxBarLargeButtonRedo: TdxBarLargeButton
      Action = dxRichEditControlRedo
      Category = 0
      SyncImageIndex = False
      ImageIndex = 198
    end
  end
  inherited acActions: TActionList
    Left = 544
    Top = 256
    object acShowAllFieldResults: TdxRichEditControlShowAllFieldResults
      Category = 'Mail Merge'
      ImageIndex = 40
    end
    object ShowAllFieldCodes: TdxRichEditControlShowAllFieldCodes
      Category = 'Mail Merge'
      ImageIndex = 39
    end
    object dxRichEditControlNewDocument: TdxRichEditControlNewDocument
      ImageIndex = 0
    end
    object dxRichEditControlLoadDocument: TdxRichEditControlLoadDocument
      ImageIndex = 1
    end
    object dxRichEditControlSaveDocument: TdxRichEditControlSaveDocument
      ImageIndex = 2
    end
    object dxRichEditControlSaveDocumentAs: TdxRichEditControlSaveDocumentAs
      ImageIndex = 3
    end
    object dxRichEditControlPasteSelection: TdxRichEditControlPasteSelection
      ImageIndex = 4
    end
    object dxRichEditControlCutSelection: TdxRichEditControlCutSelection
      ImageIndex = 5
    end
    object dxRichEditControlCopySelection: TdxRichEditControlCopySelection
      ImageIndex = 6
    end
    object dxRichEditControlSelectAll: TdxRichEditControlSelectAll
      ImageIndex = 7
    end
    object dxRichEditControlChangeFontName: TdxRichEditControlChangeFontName
      ImageIndex = 8
    end
    object dxRichEditControlChangeFontSize: TdxRichEditControlChangeFontSize
      ImageIndex = 9
    end
    object dxRichEditControlIncreaseFontSize: TdxRichEditControlIncreaseFontSize
      ImageIndex = 10
    end
    object dxRichEditControlDecreaseFontSize: TdxRichEditControlDecreaseFontSize
      ImageIndex = 11
    end
    object dxRichEditControlTextUpperCase: TdxRichEditControlTextUpperCase
    end
    object dxRichEditControlTextLowerCase: TdxRichEditControlTextLowerCase
    end
    object dxRichEditControlToggleTextCase: TdxRichEditControlToggleTextCase
    end
    object dxRichEditControlToggleFontBold: TdxRichEditControlToggleFontBold
      ImageIndex = 13
    end
    object dxRichEditControlToggleFontItalic: TdxRichEditControlToggleFontItalic
      ImageIndex = 14
    end
    object dxRichEditControlToggleFontUnderline: TdxRichEditControlToggleFontUnderline
      ImageIndex = 15
    end
    object dxRichEditControlToggleFontDoubleUnderline: TdxRichEditControlToggleFontDoubleUnderline
      ImageIndex = 16
    end
    object dxRichEditControlToggleFontStrikeout: TdxRichEditControlToggleFontStrikeout
      ImageIndex = 17
    end
    object dxRichEditControlToggleFontDoubleStrikeout: TdxRichEditControlToggleFontDoubleStrikeout
      ImageIndex = 18
    end
    object dxRichEditControlToggleFontSubscript: TdxRichEditControlToggleFontSubscript
      ImageIndex = 19
    end
    object dxRichEditControlToggleFontSuperscript: TdxRichEditControlToggleFontSuperscript
      ImageIndex = 20
    end
    object dxRichEditControlTextHighlight: TdxRichEditControlTextHighlight
      ImageIndex = 21
    end
    object dxRichEditControlChangeFontColor: TdxRichEditControlChangeFontColor
      ImageIndex = 22
    end
    object dxRichEditControlToggleBulletedList: TdxRichEditControlToggleBulletedList
      ImageIndex = 23
    end
    object dxRichEditControlToggleSimpleNumberingList: TdxRichEditControlToggleSimpleNumberingList
      ImageIndex = 24
    end
    object dxRichEditControlToggleMultiLevelList: TdxRichEditControlToggleMultiLevelList
      ImageIndex = 25
    end
    object dxRichEditControlDecrementIndent: TdxRichEditControlDecrementIndent
      ImageIndex = 26
    end
    object dxRichEditControlIncrementIndent: TdxRichEditControlIncrementIndent
      ImageIndex = 27
    end
    object dxRichEditControlToggleShowWhitespace: TdxRichEditControlToggleShowWhitespace
      ImageIndex = 28
    end
    object dxRichEditControlToggleParagraphAlignmentLeft: TdxRichEditControlToggleParagraphAlignmentLeft
      ImageIndex = 29
    end
    object dxRichEditControlToggleParagraphAlignmentCenter: TdxRichEditControlToggleParagraphAlignmentCenter
      ImageIndex = 30
    end
    object dxRichEditControlToggleParagraphAlignmentRight: TdxRichEditControlToggleParagraphAlignmentRight
      ImageIndex = 31
    end
    object dxRichEditControlToggleParagraphAlignmentJustify: TdxRichEditControlToggleParagraphAlignmentJustify
      ImageIndex = 32
    end
    object dxRichEditControlSetSingleParagraphSpacing: TdxRichEditControlSetSingleParagraphSpacing
    end
    object dxRichEditControlSetSesquialteralParagraphSpacing: TdxRichEditControlSetSesquialteralParagraphSpacing
    end
    object dxRichEditControlSetDoubleParagraphSpacing: TdxRichEditControlSetDoubleParagraphSpacing
    end
    object dxRichEditControlShowParagraphForm: TdxRichEditControlShowParagraphForm
      ImageIndex = 34
    end
    object dxRichEditControlSearchFind: TdxRichEditControlSearchFind
      ImageIndex = 35
    end
    object dxRichEditControlSearchReplace: TdxRichEditControlSearchReplace
      ImageIndex = 36
    end
    object dxRichEditControlUndo: TdxRichEditControlUndo
      ImageIndex = 37
    end
    object dxRichEditControlRedo: TdxRichEditControlRedo
      ImageIndex = 38
    end
  end
  inherited stBarScreenTips: TdxScreenTipRepository
    Left = 544
    Top = 200
    object stShowAllFieldCodes: TdxScreenTip
      Header.Text = 'Show All Field Codes'
      Description.Text = 
        'View the document markup with dynamic elements displaying their ' +
        'rich-text codes.'
    end
    object stShowAllFieldResults: TdxScreenTip
      Header.Text = 'Show All Field Results'
      Description.Text = 
        'View the document content with dynamic elements displaying real ' +
        'data.'
    end
  end
  inherited cxLookAndFeelController: TcxLookAndFeelController
    Left = 384
    Top = 312
  end
  inherited ApplicationMenu: TdxBarApplicationMenu
    Left = 544
  end
  inherited ilSmallImages: TcxImageList
    FormatVersion = 1
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00020000000A0000000F00000011000000110000001200000012000000130000
          00130000001300000014000000130000000D0000000300000000000000000000
          000981594BC1B37B67FFB27B66FFB27A66FFB27864FFB17965FFB17965FFB177
          65FFB17763FFB07664FFB07664FF7E5548C50000000C00000000000000000000
          000DB57D6BFFFDFBF9FFFBF6F2FFFBF5F2FFFAF5F1FFFBF4EFFFF9F3EEFFF9F2
          EEFFFAF2ECFFF8F0EBFFF9F0EAFFB17866FF0000001200000000000000000000
          000DB7816DFFFDFBFAFFF7EEE7FFF8EDE7FFF7EDE7FFF7EDE6FFF6ECE5FFF6EC
          E5FFF8EBE4FFF6EBE4FFF9F1ECFFB37A68FF0000001300000000000000000000
          000DB98472FFFDFCFBFFF8EFE8FFF7EEE8FFF7EEE8FFF8EEE7FFF7EEE7FFF7EC
          E6FFF7ECE5FFF6ECE5FFFAF2EEFFB57D6BFF0000001200000000000000000000
          000CBB8775FFFEFCFBFFF9F0EAFFF8F0EAFFF8EFE9FFF8EFE8FFF8EEE9FFF8EE
          E7FFF8EEE7FFF8EDE7FFFAF4EFFFB7816EFF0000001100000000000000000000
          000BBE8A79FFFEFDFCFFF9F2EDFFF9F2EDFFF9F0EBFFF9F0EAFFF8F0EAFFF8F0
          E9FFF8EFE9FFF8EFE8FFFAF5F1FFBA8571FF0000001000000000000000000000
          000AC08F7EFFFEFDFDFFFAF3EFFFFAF4EEFFFAF3EEFFFAF1ECFFF9F1EBFFF9F0
          EBFFF9F1EBFFF8F0EAFFFBF6F3FFBB8975FF0000000F00000000000000000000
          0009C49380FFFEFEFDFFFBF5F1FFFBF5F0FFFBF4F0FFFAF3EFFFFAF3EFFFF9F3
          EDFFF9F2EDFFF9F1EBFFFCF7F4FFBE8B79FF0000000F00000000000000000000
          0009C69686FFFEFEFDFFFAF5F3FFFBF6F2FFFBF5F1FFFBF5F0FFFBF5F0FFFAF4
          EFFFFAF4EEFFFAF3EFFFFDF9F7FFC18E7DFF0000000E00000000000000000000
          0008C99B8AFFFEFEFEFFFBF6F4FFFBF6F4FFFCF6F3FFFCF6F3FFFCF4F2FFFBF5
          F1FFFBF5F0FFFAF5F0FFFDFAF8FFC39382FF0000000D00000000000000000000
          0007C99E8DFFFFFEFEFFFCF8F6FFFCF7F5FFFCF7F5FFFBF6F4FFFBF6F4FFFCF6
          F3FFFCF6F2FFFBF6F1FFFDFBF9FFC69786FF0000000C00000000000000000000
          0006CEA190FFFFFFFEFFFDF9F7FFFDF9F7FFFCF8F7FFFCF8F6FFFCF7F5FFFBF7
          F5FFFBF7F4FFFCF7F3FFFDFCFAFFC89B8AFF0000000B00000000000000000000
          0006CEA393FFFFFFFFFFFDFAF9FFFDFAF8FFFDFAF8FFFDF9F7FFFCF8F7FFFBF8
          F6FFFBF7F6FFFCF7F5FFFEFCFCFFCB9D8DFF0000000B00000000000000000000
          0005D0A696FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFEFEFFFEFEFEFFFFFE
          FEFFFEFEFEFFFEFEFEFFFEFEFDFFCDA191FF0000000900000000000000000000
          00039C7C71C0D2A898FFD1A897FFD1A897FFD1A797FFD0A696FFD0A696FFD0A6
          95FFD0A595FFCFA595FFCFA494FF98796EC20000000600000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000090000
          000E000000100000001000000010000000100000001000000011000000110000
          001100000011000000100000000B00000003000000000000000019427ACA245A
          A5FF255CA7FF255BA7FF245AA6FF2459A6FF2358A5FF2358A4FF2356A4FF2256
          A4FF2255A3FF2154A3FF2153A1FF1C468AE303080F2900000002255DA5FF316B
          AEFF6DA6D5FF86CAF0FF46A6E4FF44A3E4FF41A1E3FF3FA0E2FF3C9EE2FF3B9C
          E1FF389BE0FF369AE0FF3498DFFF2C77C1FF10284D8B000000082B68AEFF4984
          BEFF4B8BC5FFB2E3F8FF68BBECFF55B0E8FF52AEE8FF4EACE7FF4CA9E6FF49A8
          E5FF47A6E4FF44A4E4FF41A2E3FF3A92D6FF1C4885D50000000D2F6FB4FF6CA7
          D2FF3F87C4FFAED9F0FF9AD8F5FF66BDEEFF63BBEDFF60B9EBFF5DB6EBFF5BB5
          EAFF57B2EAFF55B0E9FF51AEE7FF4FABE7FF2D69B1FF040B142F3276B9FF8FC7
          E6FF509FD4FF86BCE0FFC5EFFCFF78CAF2FF74C8F1FF72C5F0FF6FC4F0FF6DC2
          EFFF69C0EEFF66BDEEFF63BBEDFF60B9EBFF448BC9FF122D4D81357CBCFFAFE3
          F5FF75C8EDFF59A2D4FFDDF7FDFFDFF8FEFFDDF7FEFFDBF7FEFFD8F5FEFFD4F4
          FDFFD0F2FDFFCCEFFCFFC7EDFBFFC1EBFBFF9ACBE9FF215187CB3882C1FFC7F5
          FEFF97E5FCFF64BAE5FF4D9FD3FF4D9DD2FF4B9BD1FF4A99CFFF4998CFFF4896
          CEFF4694CCFF4592CBFF3073B7FF3072B6FF2F71B5FF2A65A4EA3A88C5FFCDF7
          FEFFA6ECFEFF9CE8FDFF93E4FBFF8EE1FBFF89DFFBFF86DEFAFF81DAFAFF7ED8
          F9FF7BD7F9FF79D6F9FF2A6BB0FF000000140000000A000000073D8EC8FFD0F8
          FEFFAEF0FEFFAAEEFEFFA6EDFEFFA5EBFDFFBBF2FDFFD4F9FEFFD5F9FEFFD3F8
          FEFFD1F8FEFFCEF7FDFF3680BFFF0000000800000000000000003F92CBFFD3F9
          FEFFB6F3FEFFB3F1FDFFB0F1FEFFB8EDFAFF4895CBFF3B8CC6FF3B8AC6FF3A89
          C5FF3A88C5FF3A87C3FF2A6391C20000000500000000000000004197CEFFE2FC
          FEFFE2FCFEFFE1FCFEFFD4F3FAFF458FBFEC040A0E1B00000006000000060000
          000600000006000000060000000400000001000000000000000031739ABF429A
          D0FF4299D0FF4299D0FF4297CFFF153244590000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          0003000000030000000400000003000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000040000
          00130000001E0000002000000020000000200000002100000021000000210000
          002200000022000000220000002300000021000000160000000500000012281E
          16CB37291EFF463A31FFBD8150FFBC7E4DFFB97949FFB67646FFB37141FFB06D
          3DFFAD6839FFAB6535FF42362EFF3D3026FF241A13CE00000015000000193B2C
          21FF685C54FF483C34FFE8C28BFFE7C088FFE6BD85FFE5BB81FFE4B87CFFE3B5
          79FFE2B276FFE2B273FF443931FF51433AFF34261DFF0000001E000000183E2F
          24FF6C6057FF4A3F37FFD9B27DFFD8B07BFFD7AE77FFD7AB74FFD6A970FFD5A6
          6DFFD4A56AFFD4A268FF473B33FF5B4F47FF37291EFF0000001D000000164031
          26FF6F645CFF4C4038FFFFFFFFFFF7F1EBFFF7F0EBFFF7F0EBFFF7EFEBFFF6EF
          EAFFF6EFEAFFF6EFE9FF463B34FF5D5249FF3A2C21FF0000001B000000144434
          29FF73675FFF4F443CFFFFFFFFFFF8F2EDFFF8F1EDFFF7F1EDFFF7F0EDFFF8F1
          EBFFF7F0EBFFF7F0ECFF4A4037FF5F534BFF3D2E23FF00000019000000124637
          2CFF776B63FF50453DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF4E433BFF61544CFF403126FF0000001700000010493A
          2FFF796E66FF50453DFF61564EFF60564EFF60554DFF5F544CFF5E544CFF5E53
          4BFF5D524AFF5C5249FF5B5149FF61554DFF433429FF000000150000000E4C3D
          32FF7C706AFF674E44FF654B42FF634A41FF61473FFF5F473EFF5C443CFF5B43
          3AFF594139FF584038FF573F37FF63574FFF46362DFF000000130000000D4E3F
          35FF80746DFF6B5248FFF4ECE6FFE9DACEFFE9D8CDFFE9D8CCFFE9D8CBFFE8D7
          CAFFF3EAE2FFF3E9E2FF5A4139FF645850FF483A2FFF000000110000000B5142
          36FF827770FF70564DFFF9F5F2FFF4EAE4FFF1E6DEFFEBDCD2FFE9D9CCFF4E41
          3DFF60534CFFF3EAE3FF5D453CFF655951FF4C3D32FF0000000F000000095344
          39FF857A73FF755A50FFFAF6F3FFF5EDE7FFF4EDE6FFF4ECE6FFEFE2DAFF493D
          38FF5A4D46FFF4EBE4FF60483FFF655A52FF4F3F34FF0000000D000000075545
          3AFF887D76FF795E54FFFAF6F4FFF5EEE9FFF5EDE7FFF4EDE7FFF4ECE6FF473A
          36FF483D36FFE9D9CDFF644C43FF675A52FF514137FF0000000B000000065748
          3DFF898079FF7C6157FFFAF7F4FFFAF6F4FFFAF6F4FFFAF6F3FFFAF6F3FFFAF5
          F2FFF5EEE9FFF4ECE6FF695046FF82776FFF534439FF00000009000000034235
          2EC058493DFF7F645AFF998178FF967F75FF937A72FF8E786DFF8B7269FF866E
          64FF82695FFF7D645BFF6E544AFF56453BFF3F332BC200000005000000000000
          0002000000030000000400000004000000040000000400000005000000050000
          0005000000050000000500000006000000060000000400000001}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000020000
          000A0000000F0000001000000010000000100000001100000011000000110000
          001100000011000000100000000B000000030000000000000000000000094633
          2CC160453BFF644A41FFB87D4EFFB97A4AFFB47444FFB06C3DFFA76436FFA764
          36FF583D36FF5B3E37FF3B2821C20000000A00000000000000000000000D6F53
          47FF947869FF6A4F46FFD8B07BFFD7AE77FFD7AB74FFD6A970FFD5A66DFFD4A5
          6AFF5D413AFF684B41FF543931FF0000000E00000000000000000000000C7357
          4AFF987D6EFF70564BFFFFFFFFFFF6EFEAFFF6EFE9FFF6EEE9FFF5EEE9FFF6EE
          E8FF62473FFF715348FF573B33FF0000000F00000000000000000000000B785C
          4EFF9D8273FF765C50FFFFFFFFFFF7F0EBFFF7F0EBFFF7EFEBFFF6EFEAFFF6EF
          EAFF684E44FF72554BFF593E35FF0000000E00000000000000000000000A7C60
          50FFA28777FF7B6154FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFF6E5349FF74574CFF5C4138FF0000000C0000000000000000000000097F63
          54FFA78E7DFF977A6AFF967969FF957967FF82675BFF695044FF6E554BFF6E55
          4BFF6E534AFF6C5248FF583F35FF000000200000001100000005000000088367
          57FFAB9382FF634A41FF614740FF5E463DFF574139FF805E52FF805D51FF7F5C
          51FF7F5C50FF7E5B4FFF7E5A4EFFA27262FFA07061FF0000001000000007866A
          59FFAF9788FF674E44FFF3EAE4FFE7D5C8FFDAC8BBFFAA7D6DFFFDFCFAFFF7F0
          E9FFF7F0E8FFF7EFE7FFF6EFE7FFF6EEE6FFA37363FF0000001200000006886D
          5CFFB39C8CFF6B5248FFF4ECE6FFE9D9CDFFDDCDC0FFAD8070FFFEFDFCFFF7F2
          EAFFF8F1E9FFF7F0E9FFF6F0E8FFF6EFE7FFA47566FF00000010000000058B70
          5EFFB7A091FF70564DFFF6EFEAFFECDDD3FFE1D1C8FFAF8273FFFEFEFDFFF8F3
          ECFFF8F2EBFFF7F1EAFFF7F1E9FFF7F0E8FFA77869FF0000000E000000048E72
          60FFBBA595FF755A50FFF7F1ECFFF6F0EBFFECE6E1FFB18576FFFFFFFEFFF9F3
          EEFFF9F3EDFFF9F2ECFFF8F2EBFFF7F1EAFFA97B6BFF0000000C000000026A56
          49BF8F7361FF795E54FF765D52FF745A50FF6F564CFFB38879FFFFFFFFFFF9F5
          EEFFF9F4EEFFF9F3EDFFF8F3ECFFF8F2EBFFAB7E6DFF0000000A000000010000
          00020000000300000003000000030000000400000009B58A7BFFFFFFFFFFFAF5
          F0FFF9F5EFFFF9F5EFFFF9F4EEFFF9F3EDFFAD8071FF00000008000000000000
          00000000000000000000000000000000000000000004B88D7EFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAF8273FF00000007000000000000
          00000000000000000000000000000000000000000002BA8F80FFB88D7FFFB78C
          7EFFB68B7CFFB68A7AFFB48979FFB38777FFB28576FF00000004}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000030000000C00000010000000110000
          0011000000120000001200000013000000130000001000000004000000000000
          00000000000000000000000000000000000BA97563FFA87462FFA77261FFA571
          5FFFA5705EFFA36F5DFFA36D5CFFA26D5BFFA26C5AFF0000000F000000000000
          00000000000000000000000000000000000DAB7866FFFDFBF8FFF7EFE8FFF6EF
          E6FFF6EEE6FFF5EEE5FFF5EDE4FFF5EDE4FFA36D5CFF00000013000000000000
          0000265080B7336CB1FF326BB0FF2E63A6FFAD7C6AFFFDFCFAFFF7F0E9FFF7F0
          E8FFF7EFE7FFF6EFE7FFF6EEE6FFF5EEE5FFA46F5DFF00000012000000000000
          00003976B8FF91D2F4FF66BEEEFF60B5E4FFB07F6DFFFEFDFCFFF7F2EAFFF8F1
          E9FFF7F0E9FFF6F0E8FFF6EFE7FFF6EFE6FFA67260FF00000011000000000000
          00003F7DBCFF9AD6F5FF6CC3F0FF66B8E5FFB28070FFFEFEFDFFF8F3ECFFF8F2
          EBFFF7F1EAFFF7F1E9FFF7F0E8FFF7EFE8FFA87563FF00000010000000000000
          00004384C1FFA4DBF6FF73C7F1FF6CBEE8FFB48473FFFFFFFEFFF9F3EEFFF9F3
          EDFFF9F2ECFFF8F2EBFFF7F1EAFFF7F0E9FFAB7766FF0000000F000000000000
          0000488BC5FFADE1F8FF7ACCF2FF73C3E9FFB68676FFFFFFFFFFF9F5EEFFF9F4
          EEFFF9F3EDFFF8F3ECFFF8F2EBFFF7F2EBFFAC7A6AFF0000000E000000000000
          00004C92CAFFB6E6FAFF81D2F4FF7AC9EBFFB88878FFFFFFFFFFFAF5F0FFF9F5
          EFFFF9F5EFFFF9F4EEFFF9F3EDFFF9F3ECFFAF7E6CFF0000000D000000000000
          00005098CDFFBEE9FAFF88D6F6FF81CEEEFFBB8B7BFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFDFCFAFFB1816FFF0000000C000000000000
          0000539DD1FFC5EDFBFF8FDBF7FF89D5F2FFBD8D7DFFBB8C7CFFBA8B7BFFB989
          79FFB98977FFB78876FFB68674FFB58373FFB38371FF00000009000000000000
          000056A1D4FFCBF0FCFF95E0F8FF82CBE9FF72B7D9FF71B7D9FF70B7DAFF70B8
          DBFF6FB7DBFF4385BBFF00000009000000090000000800000002000000000000
          000058A5D7FFCBEDF8FF73B2D0FF5792B7FF5893B7FF5995BAFF5B99BDFF62A3
          C9FF69AFD4FF4184BBFF00000000000000000000000000000000000000000000
          000059A8D9FFBCDBE7FF8E7875FF9B7061FF946A5BFF8F6456FF885D50FF7B65
          63FF97BCD3FF488EC4FF00000000000000000000000000000000000000000000
          0000437EA2BD4A90BFFFB48E7FFFF3EDE7FFDEC9B8FFDDC8B5FFDBC4B2FF9770
          60FF3B7BAEFF40799EBD00000000000000000000000000000000000000000000
          00000000000000000000876B60BDB69181FFB69080FFB58E7FFFB28C7DFF7D60
          54BD000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00030000000E00000018000000180000000F0000000400000000000000000000
          00030000000E00000018000000180000000F0000000400000000000000030402
          011A512408A5833A0BEE81380AEF4F2105A70301001C00000003000000030302
          001A512408A5833A0BEE81380AEF4F2105A70301001C00000003000000095931
          159BB8763BFFF1BF6EFFF1BD6BFFB56F33FF52280DA10000000C00000009552C
          129BB76F34FFEEA457FFEDA354FFB3672CFF52280DA10000000C0000000CA868
          37EEF6CE8FFFC08B57FFD2A26DFFF3C983FF985525F00000001400000010A05E
          2FEFF4B775FFB57A47FFC8905BFFF0B06AFF995425EF0000001000000009B77C
          48EDFAE3BFFFA26638FFAE7545FFF8DEB5FFAC6A38FF0000002A0000002AB777
          44FFF9D4A9FF97592FFFA3663AFFF7CFA0FFA56735EE0000000D000000047251
          3290DFAE7EFFF4DDC1FFF3DDC0FFDEB58BFFCB9C76FF67372BFF603226FFD0A1
          7AFFE1B58AFFF4D5B3FFF3D4B1FFD49F6EFF68462A9300000006000000010504
          030B7356378ECD9762F2D69F69FFE0BA94FFE6CBB4FFF0DED4FF9E796DFFCAAB
          94FFDDB591FFD29964FFC28C5AF36D4E33910504020D00000001000000000000
          0000000000030000000A35241F66AD8577FBF9F4F2FFF2E2D9FFE4CDC4FF9772
          68FF865B4EFB23120D6F00000010000000040000000100000000000000000000
          000000000000000000010000000C5B3B32B2B58B7EFFFAF5F3FFF4E5DCFFB799
          8EFF43241CB40000000F00000001000000000000000000000000000000000000
          000000000000000000031E141142A07567FDDECAC2FFB89388FFF8F3F1FFF3E7
          DFFF8A6054FD150B093F00000003000000000000000000000000000000000000
          0000000000010403020F82584BD8E6D1C9FFEFE5E1FF795144E0885E53DFF6F1
          EEFFDFCFC6FF61382ED70201010E000000010000000000000000000000000000
          0000000000045339318ACCAEA3FFF0E5E1FF734D41C80805041E0A0605197550
          45C5F4EEECFFB99B93FF39201988000000040000000000000000000000000000
          00022219153BB68D7EFBEEE2DDFF68473DB00402021000000002000000010403
          020C644237ACF2EAE7FF92695DFD170D0A390000000200000000000000010403
          030B946C5FD6EBDED9FF5C413894000000070000000200000000000000000000
          00010000000454372E91ECE3E0FF683F34D50301010A00000001000000025E45
          3B85EBDCD6FF4B362E7200000005000000010000000000000000000000000000
          000000000000000000023F2A236FE9DFDCFF3E231D8400000002000000028965
          58BB3D2D27590000000300000001000000000000000000000000000000000000
          000000000000000000000000000131201B57654236BA00000001}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0002000000090000000E0000000F0000000F0000001000000010000000110000
          0011000000100000000B00000003000000000000000000000000000000000000
          00087C5345C0AD725EFFAC725DFFAC715DFFAC6F5BFFAB705CFFAB705CFFAB6E
          5CFFAB6E5AFF7A4E41C30000000B000000000000000000000000000000000000
          000BAF7462FFFDFBF9FFFBF6F2FFFBF5F2FFFAF5F1FFFBF4EFFFF9F3EEFFF9F2
          EEFFFAF2ECFFAC715DFF0000000F000000000000000000000000000000000000
          000BB17964FFFDFBFAFFF7EEE7FFF8EDE7FFF7EDE7FFF7EDE6FFF6ECE5FFF6EC
          E5FFFAF2EEFFAE7260FF01010120010101100101010B00000003000000000000
          000BB37C69FFFDFCFBFFF8EFE8FFF7EEE8FFF7EEE8FFF8EEE7FFF7EEE7FFF7EC
          E6FFFAF3EFFFB07863FFC19D92FFB57D6AFF815A4EC30101010B000000000000
          000AB57F6CFFFEFCFBFFF9F0EAFFF8F0EAFFF8EFE9FFF8EFE8FFF8EEE9FFF8EE
          E7FFFBF5F1FFB27A66FFEBE6E2FFFAF3EDFFB6806DFF0101010F000000000000
          0009B98270FFFEFDFCFFF9F2EDFFF9F2EDFFF9F0EBFFF9F0EAFFF8F0EAFFF8F0
          E9FFFBF6F3FFB37D6AFFE9E1DAFFFAF3EFFFB88170FF01010110000000000000
          0008BB8775FFFEFDFDFFFAF3EFFFFAF4EEFFFAF3EEFFFAF1ECFFF9F1EBFFF9F0
          EBFFFCF8F5FFB6806DFFEAE1DBFFFAF4F0FFB98673FF0101010F000000000000
          0007BF8B78FFFEFEFDFFFBF5F1FFFBF5F0FFFBF4F0FFFAF3EFFFFAF3EFFFF9F3
          EDFFFCF9F7FFBA8471FFECE4DDFFFBF5F2FFBB8876FF0101010E000000000000
          0007C18E7EFFFEFEFDFFFAF5F3FFFBF6F2FFFBF5F1FFFBF5F0FFFBF5F0FFFAF4
          EFFFFDFAF8FFBC8978FFEDE7E0FFFBF6F4FFBC8B7AFF0101010D000000000000
          0006C49382FFFEFEFEFFFBF6F4FFFBF6F4FFFCF6F3FFFCF6F3FFFCF4F2FFFBF5
          F1FFFDFBF9FFBF8C7CFFEFE8E3FFFCF8F5FFBF8E7CFF0101010D000000000000
          0005C49785FFFFFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFDFFFEFDFDFFFEFD
          FDFFFEFDFCFFC2907FFFF0EBE6FFFCF9F7FFC29180FF0101010C000000000000
          0003967265C0C89988FFC99887FFC79887FFC59786FFC79785FFC79784FFC596
          84FFC59683FFCDA79AFFF4EFEAFFFDFAF8FFC49686FF0101010B000000000000
          000100000003000000040000000BD8BBB0FFF8F8F8FFF5F0EFFFF5F0EFFFF5EF
          EDFFF5EFEDFFF7F0EEFFFAF4F1FFFDFBF9FFC7998AFF0000000A000000000000
          0000000000000000000000000005CCA392FFFFFEFEFFFEFEFEFFFEFEFEFFFEFE
          FEFFFEFEFDFFFEFDFDFFFEFDFDFFFEFDFCFFCA9D8DFF00000009000000000000
          00000000000000000000000000039A7B6FC0CEA495FFCFA494FFCDA494FFCCA3
          93FFCDA392FFCDA391FFCCA291FFCCA290FF97776BC200000006}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000500000009000000070000000100000000000000000000
          0000000000000000000000000000000000000000000000000001000000030000
          000100000005241C1968624D45F53328248D0000000600000000000000000000
          0000000000000000000000000000000000000000000000000005231917880000
          00090000000B57453EDBBFACA1FF5A4640E20000000900000000000000000000
          00000000000000000000000000000000000000000000000000096B554DFF231A
          178E130E0C5787736AFF99857AFF3429258B0000000600000000000000000000
          00000000000000000000000000000000000000000000000000096E574EFF9384
          7BFF3A2C26DEB2A094FF735E54FC0A08072C0000000B000000055A4139FF5A41
          38FF594038FF00000000594037FF584037FF583F36FF000000086F5850FFE2D8
          CCFF8D7A70FFAA958AFF6D564DFF6C554DFF6C544DFF352A26855C433AFF0000
          0000000000000000000000000000000000000000000000000008725B52FFECE5
          DAFFD0BEB2FFC9B6AAFFC9B6AAFF918077FF261C1988000000075F463DFF0000
          0000B2877AFFB18779FFB18679FFB18678FFB08678FF00000007735C54FFF2ED
          E2FFDDCFC5FFD0C0B5FF95847AFF271E1A870000000600000001000000000000
          0000000000000000000000000000000000000000000000000006765E55FFF4EF
          E5FFE2D7CDFF9B8C83FF281E1B86000000060000000100000000664C42FF0000
          0000B68D7FFFB58C7FFFB48C7FFFB48B7EFFB48B7DFF00000005776056FFF6F2
          E7FFA09289FF2A201C8500000005000000010000000000000000694E45FF0000
          0000000000000000000000000000000000000000000000000005796259FFA79A
          91FF2B211C8400000004000000010000000000000000000000006C5147FF0000
          0000B99285FFB99184FFB99184FFB89183FFB89083FF000000037A635AFF2C21
          1E83000000030000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002000000050000
          000300000000000000000000000000000000000000000000000071564DFF0000
          0000BD988AFFBC978AFFBC978AFFBD9689FFBC9688FFBC9688FFBC9688FF0000
          00006D5248FF000000000000000000000000000000000000000074584EFF0000
          0000000000000000000000000000000000000000000000000000000000000000
          000070554BFF0000000000000000000000000000000000000000755A50FF765A
          50FF75594FFF0000000074594EFF74594FFF74584EFF0000000073574DFF7257
          4DFF72574DFF0000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000001000000060000000C0000000E0000000B000000050000
          0002000000000000000000000000000000000000000000000000000000000000
          00000000000000000006010C1F5B031A45B4021E52D002163CA40108164E0000
          000B000000010000000000000000000000000000000000000000000000000000
          00000000000000000008082A5CAF3F83BEFF6CA9D6FF0F4491FF052F78FF010F
          27710000000700000000000000000000000000000000000000000000000C0000
          000F0000000D000000090104071F13458AE678C6E5FFA4DBF1FF093D96FF062A
          69EA0000001300000002000000000000000000000000000000007E431BFF7C41
          1AFF683213FF1C0C045800000008030E1D41225DA6F66DB5DBFF207EC6FF1248
          98FF47160DA401000011000000020000000000000000000000000D080427864E
          23F03D1F0CA00000000C00000001000000070412224816437ED71855A5FFF2E9
          E2FFBA8169FF4E1A0EAE0000011000000002000000000000000000000007502E
          149C572B11D50000000B00000000000000000000000A4C2D159C7C6866FFC3B5
          B5FFF8EEE5FF705F8CFF010132AC0000010F0000000200000000000000031F13
          0941753E18FF100803380000000D0000000D02010018885225E9925829FF705B
          5EFE8A8CC3FFA2A8F1FF4146AFFF020138AB0000010E00000002000000000000
          000978471FDD894F22FF884D20FF864C20FF9A602CFF9A612CFF925828FF4623
          0CBB273C72AB5867CDFFA8B0F2FF464CB4FF03023DA90000010C000000000000
          00044C31178675461ED8000000130000001159381B9F9D632EFF864D20FF2010
          065C0000000B233269A4656FDAFFADB6F2FF474DB3FF01013D9E000000000000
          0002130C0629834B1FFF150C053702010110935F2DED9D632FFF703E19E80101
          000E0000000200000006343A7FA36976DCFFAAB3F0FF1C1B9FEC000000000000
          0000000000066D411DC5432811893322105FA26A32FF975D2BFF3D220E8A0000
          0005000000000000000100000005353B79965760C5EE0C0B3755000000000000
          0000000000033D27126C7C4F23D6714B24B7A46D33FF8C5425FC0E0803270000
          0002000000000000000000000001000000020000000300000002000000000000
          0000000000010A0703179E6B32F7A56F35FCA26B33FF623919BA000000060000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000004714D26B1A97338FF9D662EFF2D1C0B5A000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000235251254AA7439FF8A5829E902010009000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000C0000
          000F0000000D000000060000000100000001000000040000000D000000110000
          0012000000110000000E8787878BBFBFBFBFBFBFBFBF898989897E431BFF7C41
          1AFF683213FF1C0C0457000000030000000107030119753A17FF723916FF7237
          15FF6E3414FF4A1D09F2B8B8B8C2614A41FF614941FFBFBFBFBF0D080427864E
          23F03D1F0CA00000000C00000001000000010000000720130850945827FF9153
          24FF693514EA050201268C8C8C90BFBFBFBFBFBFBFBF8E8E8E8E00000007502E
          149C572B11D50000000B000000000000000000000007502F1697965A28FF854A
          20FF2D1407905E5E5E66BEBEBEBF634D42FFB6B4B3C661616161000000031F13
          0941753E18FF100803380000000D0000000D02010018895325E9985C2BFF713B
          17FD09040129BDBDBDC0654E45FF654D44FF644D44FFBFBFBFBF000000000000
          000978471FDD894F22FF884D20FF864C20FF9A602CFF9A612CFF935828FF4924
          0DB90000000A5E5E5E5EBFBFBFBF664F45FFBFBFBFBF5E5E5E5E000000000000
          00044C31178675461ED8000000130000001159381B9F9D632EFF864D20FF2010
          065B0000000400000000BFBFBFBF685146FFBFBFBFBF00000000000000000000
          0002130C0629834B1FFF150C053702010110935F2DED9D632FFF703E19E80101
          000E0000000100000000BFBFBFBF6A5248FFBFBFBFBF00000000000000000000
          0000000000066D411DC5432811893322105FA26A32FF975D2BFF3D220E8A0000
          00050000000061616161BFBFBFBF6C5349FFBFBFBFBF61616161000000000000
          0000000000033D27126C7C4F23D6714B24B7A46D33FF8C5425FC0E0803270000
          000200000000BFBFBFBF6E564BFF6D554AFF6C554AFFBFBFBFBF000000000000
          0000000000010A0703179E6B32F7A56F35FCA26B33FF623919BA000000060000
          0000000000005E5E5E5EB7B5B4C66E574CFFB7B5B4C65E5E5E5E000000000000
          00000000000000000004714D26B1A97338FF9D662EFF867565B3BEBEBEC0BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF89898989000000000000
          0000000000000000000235251254AA7439FF8A5829E9BABABAC2745C51FF745C
          50FF735B50FF725A50FF725A4FFF715A4EFF71594EFFBFBFBFBF000000000000
          000000000000000000000000000200000004000000048D8D8D8EBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000030000000C0000000F0000000D00000006000000010000
          0001000000040000000D0000001100000012000000110000000E000000000000
          000000000000000000087E431BFF7C411AFF683213FF1C0C0457000000030000
          000107030119753A17FF723916FF723715FF6E3414FF4A1D09F2000000000000
          000000000000000000040D080427864E23F03D1F0CA00000000C000000010000
          00010000000720130850945827FF915324FF693514EA05020126000000000000
          0000000000000000000000000007502E149C572B11D50000000B000000000000
          000000000007502F1697965A28FF854A20FF2D14079000000008000000000000
          00000000000000000000000000031F130941753E18FF100803380000000D0000
          000D02010018895325E9985C2BFF713B17FD0904012900000003000000000000
          00000000000000000000000000000000000978471FDD894F22FF884D20FF864C
          20FF9A602CFF9A612CFF935828FF49240DB90000000A00000000000000000000
          0000000000000000000000000000000000044C31178675461ED8000000130000
          001159381B9F9D632EFF864D20FF2010065B0000000400000000000000000000
          000000000000000000000000000000000002130C0629834B1FFF150C05370201
          0110935F2DED9D632FFF703E19E80101000E0000000100000000000000000000
          000000000000000000000000000000000000000000066D411DC5432811893322
          105FA26A32FF975D2BFF3D220E8A0000000500000000000000008C8C8C8FB7B7
          B7C2B2B2B2C4B0B0B0C4B4B4B4C390909094000000033D27126C7C4F23D6714B
          24B7A46D33FF8C5425FC0E0803270000000200000000000000007F7F7F846F58
          4EF35C3F32FF4F3529FF40322DF37D7D7D87000000020A0703179E6B32F7A56F
          35FCA26B33FF623919BA0000000600000000000000000000000014141415A19C
          9AC367493BFE452E26FD969291C5141414180000000000000004714D26B1A973
          38FF9D662EFF2D1C0B5A00000002000000000000000000000000000000004E4E
          4E50958580DD8E817BDD4E4E4E5200000000000000000000000235251254AA74
          39FF8A5829E90201000900000000000000000000000000000000000000000000
          0000929292929191919200000000000000000000000000000000000000020000
          0004000000040000000100000000000000000000000000000000000000000000
          0000191919191919191900000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000010080324693414E7713615FF51260DC900000000000000000000
          0000653212E7703514FF6E3514FF57280FE10000000000000000000000000000
          000000000000000000000C0703157E461EF60A05021800000000000000000000
          00004F2D148A925424FF7D441CF6070301120000000000000000000000000000
          00000000000000000000000000004C270FA82B15086600000000000000000000
          0000834C22E18F5223FF421F0C9F000000000000000000000000000000000000
          00000000000000000000000000002816094B793F18FF793E18FF783D18FF9357
          27FF955A29FF81451EFF190C043C000000000000000000000000000000000000
          000000000000000000000000000002010103824C22E70A060312000000005D39
          1A9C965A29FF5F3114D200000000000000000000000000000000000000000000
          00001919191919191919000000000000000045250E902F18096905030109925A
          29F08D5425FF3018096F00000000000000000000000000000000000000000000
          0000929292929292929200000000000000001D1107365B3114C0352110579D63
          2EFF7C451DF60804021200000000000000000000000000000000000000004F4F
          4F4F8C7F7ADC897E7ADB4F4F4F4F000000000000000075441DD5825428CF9A61
          2CFF4A27109F000000000000000000000000000000000000000014141414A7A2
          A0C066483BFD553B32FDA3A09EBF14141414000000004E32177EA26B33FF8D55
          25FF1C0F063C0000000000000000000000000000000000000000828282828568
          5DF1734F40FF6C4B3BFF6F584DF18585858500000000150E0721A36B33FF6D3E
          19D80000000000000000000000000000000000000000000000008E8E8E8EBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF929292920000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000050000
          0006000000030000000100000002000000060000000700000006000000040000
          000500000007000000070000000500000006000000070000000480481DCF9653
          23EF361E0C5B00000002150B052788491EDF874A1DE0904F1EEF341C0B5E4827
          0F8085461AE0854619E047260E80653513B1834519E047250D7D2112083B8D4E
          20DF0000000700000001000000055F3415A1C08451FF774622BB22150B3CA35D
          2AFFA7622FFF2B1A0D4C3F271566A86431FF8D491AF000000009000000058F4F
          20DF160C052B00000005000000098B4C20E0B2713EFF402612690A0602189654
          24F09F5925FF1810092B21150B3BA15C29FF844519E000000006000000035831
          148DA15F2FF18D4F20DF8D4E1FDFD39D6CFF985828F00B060218000000032A17
          094B894F26D581481ED252321982B1703DFF84451AE000000005000000012C19
          0B4979441CBE000000094C2A127EBC7E4CFF5F35169E00000003000000021F11
          07392F1F1346150B052952321A80B2713EFF86461BE000000005000000000B06
          0315945728E00B060318885024D2B26F3CFF2B180A4900000001000000027F45
          1CCE88491FDF00000007150B052BA35E2BFF7B4318CF00000004000000000000
          00026F3F1CAC4D2E1775BC7C49FF9B5A29F00B06031500000000000000012011
          0737733F19BE7D451BCE7D451BCE733F18BE1F11073800000001000000000000
          0001381F0E58B07544F4C78E5DFF6236189C0000000200000000000000000000
          0001000000020000000300000003000000020000000100000000000000000000
          00000B060314AB6330FFB57340FF2C190B470000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000170401BAB915324DE000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000090908F92B7B7B7C2B5B4B4C3B4B4B4C3B4B4B4C3B5B4B4C4B4B4
          B4C3B9B9B9C25F5F5F6213131314000000000000000000000000000000000000
          000000000000BAB9B9C13E281FFF3B271FFF3B251EFF3A251DFF38241CFF3823
          1BFF6F6461E4A7A5A4C95F5F5F62000000000000000000000000000000000000
          000000000000BBBBBBC097908DD44C3329FF4B3229FFAFAFAFC6B2B2B2C47469
          64E43B251EFF716661E49B9B9BA3000000000000000000000000000000000000
          0000000000002F2F2F30B8B8B8C350372DFF432C24FFB8B8B7C35F5F5F62A19C
          9ACF442D24FF4B3A34F6999999A5000000000000000000000000000000000000
          00000000000000000000BAB9B9C2553A31FF473028FFB7B7B7C25E5E5E61A19D
          9ACE4E342AFF513F37F69A9999A4000000000000000000000000000000000000
          00000000000000000000BABABAC2593F34FF4E352CFFB4B3B3C4B6B6B6C38073
          6DE352382EFF786D68E39D9D9DA3000000000000000000000000000000000000
          00000000000000000000BABAB9C15F4339FF54392FFF51382EFF553B31FF593F
          35FF75665FEAACA9A9C95F5F5F61000000000000000000000000000000000000
          00000000000000000000BBBBBAC163483DFF62463BFFB5B4B4C4948985DB5A3F
          35FF75645EEAB0B0AFB713131313000000000000000000000000000000000000
          0000000000002F2F2F30BABABAC1684B40FF5D4238FFBABABAC2B3B1B0C65F45
          39FF5C4439FCB0B0B0B700000000000000000000000000000000000000000000
          000000000000BEBEBEC0A69F9CD16C4F43FF63463CFFB8B8B7C39A8F8ADA674A
          3FFF7B6B65E9B2B2B2B600000000000000000000000000000000000000000000
          000000000000BEBEBEC0705346FF705246FF674B40FF694C41FF6C5044FF8573
          6CE9A69F9DCF7878787900000000000000000000000000000000000000000000
          00000000000090909091BEBEBDC0BCBCBCC0BCBCBCC1BCBCBCC1BCBCBCC1BDBD
          BDC0787878792A2A2A2B00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000008787878BB5B5B5C2B3B3B3C3B2B2B2C3B2B2B2C3B5B5B5C38F8F
          8F93000000000000000000000000000000000000000000000000000000000000
          000000000000AEAEAEB6453029FC3D271FFF3B261FFF3A261EFF39241DFFB8B8
          B8C2000000000000000000000000000000000000000000000000000000000000
          0000000000008787878AB2B2B2C4776965E54F352CFF4E352BFF999695CE8E8E
          8E93000000000000000000000000000000000000000000000000000000000000
          0000000000000202020264646467A4A09ECC533930FF452E26FF908A87D55757
          575B000000000000000000000000000000000000000000000000000000000000
          000000000000000000001B1B1B1BB6B6B6C25A4137FC4D342BFF7D726FE07979
          797F000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000B0B0B0B86D5851F2533A30FF6D5D57EB9C9C
          9CA3000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000008F8F8F9582726CE65C4036FF604C44F6B6B6
          B6BF070707070000000000000000000000000000000000000000000000000000
          00000000000000000000000000006B6B6B6E948985DA61463CFF593F36FEB4B3
          B2C4282828290000000000000000000000000000000000000000000000000000
          00000000000000000000000000004949494BA6A09ECE684B40FF5D4238FFA29C
          99CF6969696B0000000000000000000000000000000000000000000000000000
          000000000000000000000000000090909092AEACAAC96B4E43FF64483DFF8475
          70E4B9B9B9C19090909200000000000000000000000000000000000000000000
          0000000000000000000000000000BDBDBDC06F5246FF6F5146FF6B4E43FF654A
          3FFF63483DFFBCBCBCC000000000000000000000000000000000000000000000
          000000000000000000000000000090909091BCBCBCC0BBBBBBC0BBBBBBC0BBBB
          BBC0BCBCBCC09090909100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00008888888AB9B9B9C1B8B8B8C2B7B7B7C2B7B7B7C2B7B7B7C2B7B7B7C2B6B6
          B6C2B6B6B6C2B6B6B6C2B8B8B8C18787878A0000000000000000000000000000
          0000BBBBBBC05F483EFF4B3328FF493328FF483227FF452F26FF432E25FF412C
          24FF402C22FF3F2B22FF231813FFBABABAC10000000000000000000000000000
          00008D8D8D8FBBBBBBC1B8B8B8C1B7B7B7C2B4B4B4C3B2B2B2C3B2B2B2C4B3B3
          B3C3B5B5B5C2B7B7B7C2B9B9B9C18C8C8C8F0000000000000000000000000000
          0000000000002B2B2B2BB9B9B9BC989593CA4F433FEC30211CF92F201BF94B41
          3DED969392CAB8B8B8BC2B2B2B2B000000000000000000000000000000000000
          0000000000009A9A9A9C9B9694D0483129FE604D45F3908A87C58D8886C54A3C
          36F32B1B16FE949190D09999999C000000000000000000000000000000000000
          000000000000B8B8B8BC644E47F44B3329FFA7A4A3CA9595959895959598A8A4
          A2CB34201AFF443632F4B7B7B7BD000000000000000000000000000000000000
          000000000000BBBBBBC0593F36FE3E2921FFBBBBBBC1191919191B1B1B1BBABA
          BAC14D332AFF33221BFEBBBBBBC1000000000000000000000000000000000000
          000000000000BCBCBCC05A3F35FF432C24FFBBBBBBC00000000000000000BBBB
          BBC1553B32FF37231CFFBBBBBBC1000000000000000000000000000000000000
          000000000000BCBCBCC05E4339FF493229FFBCBCBCC00000000000000000BBBB
          BBC05B4035FF3E2821FFBBBBBBC1000000000000000000000000000000000000
          000000000000BDBDBDC062463CFF50372EFFBCBCBCC00000000000000000BCBC
          BCC05F4439FF452E26FFBBBBBBC0000000000000000000000000000000000000
          000000000000BEBEBEC0664A3FFF573C34FFBDBDBDC00000000000000000BDBD
          BDC063473CFF4C342BFFBCBCBCC0000000000000000000000000000000000000
          00008F8F8F8FBEBEBEC06A4D43FF5E4238FFBDBDBDC08F8F8F8F8F8F8F8FBCBC
          BCC0674B40FF533930FFBBBBBBC08E8E8E8F0000000000000000000000000000
          00008F8F8F8F816C63EF6E5145FF65483DFF79645DEF8E8E8E8F8E8E8E8F7F6A
          61EF6C4F43FF5A3F35FF705C55EF8E8E8E8F0000000000000000000000000000
          00008F8F8F8FBFBFBFBFBEBEBEBFBEBEBEBFBEBEBEBF8F8F8F8F8F8F8F8FBEBE
          BEBFBEBEBEC0BEBEBEC0BEBEBEBF8F8F8F8F0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000089898989BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF898989890000000000000000000000000000
          0000BFBFBFBF5F483EFF4B3328FF493328FF463026FF452F26FF422D25FF412C
          24FF402C22FF3F2B22FF231813FFBFBFBFBF0000000000000000000000000000
          0000B1B1B1B1BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B10000000000000000000000000000
          0000BFBFBFBF634E42FF4D352AFF4B3429FF493328FF473127FF442F26FF432E
          25FF422D25FF412C23FF271B15FFBFBFBFBF0000000000000000000000000000
          00008E8E8E8EBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E0000000000000000000000000000
          0000000000002B2B2B2BBBBBBBBBA19E9DC8574B46EB362720F835251FF85448
          43EB9F9D9BC8BBBBBBBB2B2B2B2B000000000000000000000000000000000000
          0000000000009B9B9B9BA29D9BCD4B342AFD66514AF1989290C297918FC25042
          3CF12F1E19FD9E9A99CD9B9B9B9B000000000000000000000000000000000000
          000000000000BBBBBBBB69544DF34F372DFFAFACABC79797979797979797B0AC
          AAC83A241DFF4B3D37F3BBBBBBBB000000000000000000000000000000000000
          000000000000BFBFBFBF5C4138FD462F26FFBFBFBFBF191919191B1B1B1BBFBF
          BFBF50372EFF3A2620FDBFBFBFBF000000000000000000000000000000000000
          000000000000BFBFBFBF5E4339FF493229FFBFBFBFBF0000000000000000BFBF
          BFBF5A3F35FF3E2821FFBFBFBFBF000000000000000000000000000000000000
          000000000000BFBFBFBF62463CFF50372EFFBFBFBFBF0000000000000000BFBF
          BFBF5F4439FF452E26FFBFBFBFBF000000000000000000000000000000000000
          000000000000BFBFBFBF664A3FFF573C34FFBFBFBFBF0000000000000000BFBF
          BFBF63473CFF4C342BFFBFBFBFBF000000000000000000000000000000000000
          00008F8F8F8FBFBFBFBF6A4D43FF5E4238FFBFBFBFBF8F8F8F8F8F8F8F8FBFBF
          BFBF674B40FF533930FFBFBFBFBF8F8F8F8F0000000000000000000000000000
          00008F8F8F8F826D63EF6E5145FF65483DFF7A655EEF8F8F8F8F8F8F8F8F806B
          62EF6C4F43FF5A3F35FF715E57EF8F8F8F8F0000000000000000000000000000
          00008F8F8F8FBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8F8F8F8F8F8F8F8FBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF8F8F8F8F0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000007D7D7D7DA0A0A0A0B6B6B6B6BFBFBFBFBFBFBFBFBFBFBFBF9D9D
          9D9D656565650707070700000000000000000000000000000000000000000000
          000000000000BFBFBFBF615651E9463833F534241EFC2C1B15FF443732F46D65
          63E1B9B9B9C18787878700000000000000000000000000000000000000000000
          000000000000BFBFBFBF462F27FF523E37F8ADA9A8C9B5B3B2C46E605BE72D1B
          15FF534845ECBFBFBFBF07070707000000000000000000000000000000000000
          000000000000BFBFBFBF472E26FFA5A2A0CBAAAAAAAAA6A6A6A6978E8AD6442C
          23FF291914FFB9B9B9C10707070700000000000000000000000000000000BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFACA9A8C95E483FF74E34
          2BFF34241EFCBEBDBDC0BFBFBFBFBFBFBFBF000000000000000000000000BFBF
          BFBF574036FF442C25FF412B23FF3F2A21FF412B23FF50372DFF52382DFF4F35
          2CFF37231CFF34211AFF2B1A15FFBFBFBFBF000000000000000000000000BFBF
          BFBFBFBFBFBFBFBFBFBFB8B7B6C475635BED573C32FF553B30FF644F47F4AAA6
          A5CABFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000000000000000000000000000
          000000000000A4A4A4A47A6963E95B4136FF583F35FE857874E0BCBCBCC1ACAC
          ACACACACACAC0000000000000000000000000000000000000000000000000000
          000000000000A4A4A4A46D564CF65B4136FF8B807CDCBDBDBDBDBFBFBFBF4D39
          32F8BFBFBFBF0000000000000000000000000000000000000000000000000000
          000000000000A4A4A4A485746DE7563C33FF7D6F6AE4B8B7B6C3A19997D14831
          28FFBFBFBFBF0000000000000000000000000000000000000000000000000000
          00000000000077777777BABAB9C2887973E268544CF3563C32FF685047F86B57
          51EFBFBFBFBF0000000000000000000000000000000000000000000000000000
          000000000000090909096A6A6A6A9B9B9B9BBFBFBFBFBFBFBFBFBFBFBFBFACAC
          ACAC909090900000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000007D7D7D7DA0A0A0A0B6B6B6B6BFBFBFBFBFBFBFBFBFBFBFBF9D9D
          9D9D656565650707070700000000000000000000000000000000000000000000
          000000000000BFBFBFBF615551E9463732F533241DFC2B1A14FF433732F46D65
          63E1B9B9B9C18787878700000000000000000000000000000000000000000000
          000000000000BFBFBFBF493128FF543E37F8ADA9A8C9B5B3B2C46E605BE72D1A
          16FF534844ECBFBFBFBF0707070700000000000000000000000000000000BFBF
          BFBFBFBFBFBFBFBFBFBF4A3128FFA5A2A0CBBFBFBFBFBFBFBFBF978E8AD6442C
          23FF2A1A14FFB9B9B9C1BFBFBFBFBFBFBFBF000000000000000000000000BFBF
          BFBF412B22FF553C31FF422B23FF3C271FFF39251EFF402A22FF4F352BFF4E34
          2BFF342019FF311F19FF2B1A14FFBFBFBFBF000000000000000000000000BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF978D89D7583F35FC52382DFF4E36
          2DFD948A87D6BFBFBFBFBFBFBFBFBFBFBFBF000000000000000000000000BFBF
          BFBF4A332AFF483028FF462F27FF4B342BFF563B31FF563C31FF543B30FF452E
          25FF3B261FFF39251EFF34201AFFBFBFBFBF000000000000000000000000BFBF
          BFBFBFBFBFBFBFBFBFBF7E6C65E95B4136FF593F35FE877A75E0BDBCBCC1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000000000000000000000000000
          000000000000A4A4A4A46D564CF65C4136FF8C807CDCBDBDBDBDBFBFBFBF4E3B
          33F8BFBFBFBF0000000000000000000000000000000000000000000000000000
          000000000000A4A4A4A485746DE7573D34FF7E706AE4B8B7B6C3A39B98D14730
          27FFBFBFBFBF0000000000000000000000000000000000000000000000000000
          00000000000077777777BABAB9C2897A75E269554DF3583D33FF695047F86856
          4FEFBFBFBFBF0000000000000000000000000000000000000000000000000000
          000000000000090909096A6A6A6A9B9B9B9BBFBFBFBFBFBFBFBFBFBFBFBFACAC
          ACAC909090900000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000008F8F8F92B7B7B7C2B4B4B4C3B6B6B6C291919191000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000BABABAC11B1791FF18138DFF141089FFBFBFBFBF000000006C6C
          6C6FB8B8B8C2B5B5B5C3B8B8B8C16F6F6F729999999DB5B5B5C2B2B2B2C4B2B2
          B2C4B5B5B5C2BABABAC1908FADD21D1B96FF8988A5D3BFBFBFBF000000008D8D
          8D93564740F139251DFF675B57E86C6C6C748D8B8BA7473832F5321F19FF311E
          18FF50443DF0ABABABB5B8B8B8C18E8DAAD2211F9AFFBFBFBFBF000000008F8F
          8F91B3B3B3C364534CEF675B56E8949494A2B2B2B2C45C4840F44B3229FF5F4F
          49EEB2B2B2C4AFAFAFB44F51AEF02B2DA8FF494AA8F1BFBFBFBF000000000202
          020286868688908F8EA84E3D36F27D7572D2837975DD4E352CFF4C362EFBA4A2
          A2CA8080808390909091BDBDBDC0BCBCBCC0BDBDBDC091919191000000000000
          00000B0B0B0B9B9B9B9E928F8EBA533E36F8584036FB50372DFF7F7673DDADAD
          ADB4121212130000000000000000000000000000000000000000000000000000
          0000000000002F2F2F30B8B8B8C2705D56ED553C31FF5E4C45F2B5B5B5C34C4C
          4C4D000000000000000000000000000000000000000000000000000000000000
          000004040404A2A2A2A59C9693D25A3F35FF573E34FE533E37F8918E8DB69B9B
          9B9E070707070000000000000000000000000000000000000000000000000000
          000061616162B8B7B6C36B5249F75D4137FF81736EE48E8683D457433CF19393
          92A4848484860202020200000000000000000000000000000000000000006C6C
          6C6DBCBCBCC08B7C76E161463AFF624A3FF9B3B2B2C5989898A0796C67E46A58
          53EDB7B7B7C28A8A8A8C00000000000000000000000000000000000000006C6C
          6C6D897972E465493EFF63473CFF5E463BFB9F9B9AC08585858975625AED4F36
          2DFF695750EF8989898D00000000000000000000000000000000000000005252
          5252BEBEBEC0BDBDBDC0BCBCBCC0BDBDBDC0B1B1B1B286868687BDBDBDC0BBBB
          BBC0BCBCBCC06A6A6A6B00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000006C6C
          6C6FB8B8B8C2B5B5B5C3B8B8B8C16F6F6F729999999DB5B5B5C2B2B2B2C4B2B2
          B2C4B5B5B5C26868686C00000000000000000000000000000000000000008D8D
          8D93564740F139251DFF675B57E86C6C6C748D8B8BA7473832F5321F19FF311E
          18FF50443DF08787878F00000000000000000000000000000000000000008F8F
          8F91B3B3B3C364534CEF675B56E8949494A2B2B2B2C45C4840F44B3229FF5F4F
          49EEB2B2B2C48989898C00000000000000000000000000000000000000000202
          020286868688908F8EA84E3D36F27D7572D2837975DD4E352CFF4C362EFBA4A2
          A2CA808080830000000000000000000000000000000000000000000000000000
          00000B0B0B0B9B9B9B9E928F8EBA533E36F8584036FB50372DFF7F7673DDADAD
          ADB4121212130000000000000000000000000000000000000000000000000000
          0000000000002F2F2F30B8B8B8C2705D56ED553C31FF5E4C45F2B5B5B5C34C4C
          4C4D000000000000000000000000000000000000000000000000000000000000
          000004040404A2A2A2A59C9693D25A3F35FF573E34FE533E37F8918E8DB69B9B
          9B9E070707070000000000000000000000000000000000000000000000000000
          000061616162B8B7B6C36B5249F75D4137FF81736EE48E8683D457433CF19393
          92A484848486BDBDBDC0B7B7B7C2B4B4B4C3B6B6B6C291919191000000006C6C
          6C6DBCBCBCC08B7C76E161463AFF624A3FF9B3B2B2C5989898A0796C67E46A58
          53EDB7B7B7C2B8B8B8C11B1791FF18138DFF141089FFBFBFBFBF000000006C6C
          6C6D897972E465493EFF63473CFF5E463BFB9F9B9AC08585858975625AED4F36
          2DFF695750EFBABABAC1908FADD21D1B96FF8988A5D3BFBFBFBF000000006C6C
          6C6CBEBEBEC0BDBDBDC0BCBCBCC0BDBDBDC0B1B1B1B286868687BDBDBDC0BBBB
          BBC0BCBCBCC0B0B0B0B3B8B8B8C18E8DAAD2211F9AFFBFBFBFBF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000909090924F51AEF02B2DA8FF494AA8F1BFBFBFBF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000090909091BDBDBDC0BCBCBCC0BDBDBDC091919191}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00310000003400000036000000380000003B0000003D00000040000000430000
          0044000000470000004A0000004C000000500000005200000000000000000000
          001400000016000000190000001B0000001D0000001E00000021000000230000
          0026000000280000002A0000002D0000002F0000003200000000000000000000
          00030000000400000005000000060000000700000008000000090000000B0000
          000C0000000D0000000F00000011000000130000001500000000000000000000
          0000000000000000000000000000000000001721AAFF0E1385FF0505338B0000
          00150000000C0000000300000000000000000000000000000000000000000000
          0000000000000000000000000000000000002D43D4FF445FF4FF503A31FF4934
          2CFF221714990000001200000003000000000000000000000000000000060000
          000A0000000B0000000B0000000C0000000C19256F8B5D463CFF78594DFF7151
          45FF45396CFF04062FA100000019000000040000000000000000775448BDA575
          64FFA47464FFA47564FFA37463FFBA968AFFC6B0A8FF654D41FFA39596FF6C5D
          99FF5E61E3FF242792FF4E4971F3000000170000000200000000A97969FFEFE3
          DEFFEEE2DBFFEDE1DAFFEDE0D9FFECE0D9FFF0E9E5FFA4948DFF7E7EA6FF9EA7
          F2FF686CE6FF696CE6FF282B98FF070A379D0000000F00000002AD7E6EFFF1E7
          E1FFD2C1B8FF724E3CFF724C3CFF714D3BFFF1E9E4FFB2A098FF605C93FF6E78
          C6FFA7B1F4FF7279E9FF7278E9FF2B309EFF0A0D3E990000000EB18473FFF4EB
          E6FF775141FFF1E9E3FFF1E8E2FF754E40FFF0E7E0FF977B71FFF1EBE8FF8284
          BFFF747FCEFFB0BAF6FF7D85ECFF7D83ECFF3238A4FF0A0E3D8CB68979FFF5EF
          EBFFD8C8C0FF7C5646FF7A5546FF7A5444FFF4ECE6FF795543FFF5EEEBFFF3EE
          EBFF6E6BA4FF7B86D5FFBAC5F8FF8990EFFF8D95EBFF181D85F0BA8E7EFFF7F3
          F0FFF7F2EEFFF7F2EDFFF7F1EDFF7F5949FFF6F0ECFF7F5948FFF5EFEBFFF7F1
          EEFFBAA7A0FF8F93D0FF7B86D8FFC8D5FAFFA7B3EBFF161E7CCCBF9383FFFAF8
          F4FFF9F6F3FF845F4DFF835E4CFFDDD0C9FFF9F3EFFF835D4CFF825D4BFF825D
          4BFFE4DBD5FFF7F4F2FF908BBDFF5F67C9F4333B99CD0406162BC29988FFFCFA
          F7FFFBF9F5FFFBF8F5FFFBF8F5FFFAF7F5FFFAF7F4FF866050FFF9F6F3FFF9F5
          F2FFF9F4F2FFFAF7F4FFDBC5BEFF0000000A0000000400000002C69D8DFFFCFC
          FAFFFDFCFAFFFDFCFAFFFCFBFAFFFCFBF9FFFCFBF9FFA5877AFFFCFAF7FFFCF9
          F7FFFCF9F6FFFCF8F5FFC39888FF0000000500000000000000009E8476BED4B2
          A1FFD4B1A0FFD3B09FFFD2AF9EFFD1AE9DFFD1AC9CFFD0AB9AFFCEA999FFCEA8
          97FFCDA696FFCBA595FF96796DBF000000030000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00310000003400000036000000380000003B0000003D00000040000000430000
          0044000000470000004A0000004C000000500000005200000000000000000000
          001400000016000000190000001B0000001D0000001E00000021000000230000
          0026000000280000002A0000002D0000002F0000003200000000000000000000
          00030000000400000005000000060000000700000008000000090000000B0000
          000C0000000D0000000F00000011000000130000001500000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000683C0FDC774411FF5E350DCB0000000000000000000000002B18
          05605C330AD16F3D0BFF6E3C0AFF000000000000000000000000000000000000
          000000000000010100036D3F0EE7120A03270000000000000000000000000302
          000665370CE7703E0CFF1D100342000000000000000000000000000000000000
          0000000000000000000041250A8744280A900000000000000000000000002817
          055773410EFF5E350BD500000000000000000000000000000000000000000000
          00000000000000000000100903217A4713FF794512FF784511FF774410FF7642
          10FF74410FFF311B066C00000000000000000000000000000000000000000000
          000000000000000000000000000058340EB72E1B076000000000150C032D7744
          11FF6F400DF30704010F00000000000000000000000000000000000000000000
          000000000000000000000000000028170751603810C60000000049290B997845
          12FF452709960000000000000000000000000000000000000000000000000000
          0000000000000000000000000000010100036F4011E71C100439734410F37A46
          12FF150C032D0000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000040250B815F3610C17C4814FF5D36
          0FC3000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000F09031E7D4716FC7E4915FF2B19
          0757000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000005A3510B4784615F00302
          0006000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000002F19
          0C48944F27E7955026E7311A0D4B000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000009E59
          2DE4E1C3A1FFDBB88FFFA0592CE40000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8989898900000000AD65
          34E7F7EBCCFFEED9B3FFAD6533E700000000BFBFBFBF634F44FF422E24FF412D
          24FF402C24FF402C23FF3F2C22FF3F2B23FF281A14FFBFBFBFBF00000000311E
          0F3F9E5F30C99E5F30C9341F1042000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000311D
          0F489D5B30E79F5B2FE7331E104B000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000A665
          37E4E3C6A4FFDDBC92FFA76536E40000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8989898900000000B371
          3FE7F8EDCDFFF0DCB6FFB3713EE700000000BFBFBFBF6C5549FF4B3529FF4934
          29FF493329FF483328FF473227FF463127FF34231BFFBFBFBFBF000000003321
          123FA2693AC9A2693AC935221342000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000341F
          1048A56235E7A76334E73620114B000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000AD6D
          3BE4E5C9A6FFDFBE94FFAE6D3AE40000000089898989BFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8989898900000000BA79
          44E7F9EECEFFF2DEB7FFBA7943E700000000BFBFBFBF735C50FF533C2EFF523B
          2EFF513A2EFF50392DFF4F392DFF4F382CFF453025FFBFBFBFBF000000003523
          143FA8703EC9A8703EC937251442000000008E8E8E8EBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000008F8F
          8F92B7B7B7C2B8B8B8C28F8F8F92000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000B9B9
          B9C1100B97FF35319CF2B8B8B8C1000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000BCBC
          BCC09F9EADCB100C98FFB6B6B6C28787878AB7B7B7C2B5B5B5C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B3B3B3C3B5B5B5C28787878A0000000000000000BCBC
          BCC09F9EADCA110D9AFFB6B6B6C2BABABAC1634F44FF422E24FF412D24FF402C
          24FF402C23FF3F2C22FF3F2B23FF281A14FFB8B8B8C10000000000000000BABA
          BAC1130F9BFF3734A0F1BABABAC18C8C8C8FB8B8B8C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C2B5B5B5C3B5B5B5C3B7B7B7C28C8C8C8F0000000000000000A9A9
          A9ADB4B4B4C3B4B4B4C3A9A9A9AD000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000BABA
          BAC115129FFF14129FFFBBBBBBC1000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000BBBB
          BBC03C3AA5F1B4B4B4C3BCBCBCC08888888ABABABAC1B8B8B8C2B8B8B8C2B8B8
          B8C2B8B8B8C2B7B7B7C2B7B7B7C2B8B8B8C18888888A0000000000000000BDBD
          BDC0B5B5B5C21715A3FFBBBBBBC1BBBBBBC06C5549FF4B3529FF493429FF4933
          29FF483328FF473227FF463127FF34231BFFBBBBBBC10000000000000000BCBC
          BCC01817A4FF3D3CA8F1BBBBBBC08D8D8D8FBBBBBBC1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B8B8B8C1B8B8B8C1BABABAC18D8D8D8F00000000000000009090
          9091BBBBBBC1B9B9B9C1AAAAAAAD000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000BCBCBCC01B1AA9FFBCBCBCC0000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000009090
          9091BBBBBBC11C1CAAFFBBBBBBC088888889BCBCBCC0BBBBBBC0BBBBBBC0BBBB
          BBC1BBBBBBC1BBBBBBC1BBBBBBC1BBBBBBC08888888900000000000000009090
          90924243AFF01D1EABFFBCBCBCC0BEBEBEC0735C50FF533C2EFF523B2EFF513A
          2EFF50392DFF4F392DFF4F382CFF453025FFBDBDBDC000000000000000009090
          9091ACACBAC61E1FADFFBDBDBDC08D8D8D8EBDBDBDC0BCBCBCC0BCBCBCC0BCBC
          BCC0BCBCBCC0BCBCBCC0BBBBBBC0BCBCBCC08D8D8D8E00000000000000000000
          0000BEBEBEBFBEBEBEC090909091000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000091919191BFBFBFBF919191910000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000BFBFBFBF100C98FFBFBFBFBF0000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000BFBFBFBF100D9AFFBEBEBEC0B9B8
          B7C2B8B6B6C3B8B6B6C3B8B6B6C3B7B5B5C3B8B7B7C28887878A000000000000
          000000000000000000000000000000000000BFBFBFBF120E9BFFBBBAB9C1614C
          41FF402C24FF402C23FF3F2C22FF3F2B23FF281A14FFBAB9B9C1000000000000
          000050505050BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF9595B7CEBEBEBEC0BABA
          BAC2B8B7B7C2B8B7B7C2B8B8B7C3B8B8B7C3BAB9B9C28D8D8D8F000000000000
          0000BFBFBFBF7877B2DA15129FFF15129FFFBFBFBFBF14129EFFBFBFBFBF0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000BFBFBFBF1715A2FFBFBFBFBF1514A1FFBFBFBFBFBFBFBFBF919191910000
          0000000000000000000000000000000000000000000000000000000000000000
          0000BFBFBFBF7979B4DA1715A3FF1715A3FFBEBEBEC0BCBBBBC1BABABAC2BABA
          BAC2BABABAC2BABABAC2BBBABAC18888888A0000000000000000000000000000
          00008E8E8E8EBFBFBFBFBFBFBFBF1817A4FFBCBBBBC06C5549FF4B3529FF4934
          29FF493329FF483328FF36251CFFBCBCBCC10000000000000000000000000000
          00008E8E8E8E9696B9CE1919A7FF7979B4DABEBEBEC0BCBCBCC1BBBAB9C1BBBA
          B9C1BBBAB9C1BBBAB9C1BCBBBBC18D8D8D8F0000000000000000000000009191
          9191BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF5050505000000000000000000000
          000000000000000000000000000000000000000000000000000000000000BFBF
          BFBF1C1DABFFBFBFBFBF00000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000091919191BFBF
          BFBF1E1EADFFBFBFBFBF88888889BDBDBDC0BCBBBBC0BCBBBBC0BCBBBBC0BCBB
          BBC0BDBDBDC08888888900000000000000000000000000000000BFBFBFBF1F20
          AEFF1F1FADFFBFBFBFBFBEBEBEC0745D51FF553C30FF543C30FF533C2EFF523B
          2EFF483228FFBEBEBEC000000000000000000000000000000000919191919999
          BBCE2021B0FFBFBFBFBF8D8D8D8EBEBEBEC0BEBEBEC0BDBDBDC0BDBDBDC0BDBD
          BDC0BEBEBEC08D8D8D8E00000000000000000000000000000000000000009191
          9191BFBFBFBF9191919100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000089898989BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF89898989BFBFBFBF6B55
          49FF4A3429FF493328FF473228FF473128FF463126FF442F26FF432F26FF422E
          25FF412D24FF402D24FF402C23FF3E2B23FF261913FFBFBFBFBFB1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1BFBFBFBF6E57
          4AFF4C362AFF4B352AFF4A3429FF493329FF483328FF463127FF453027FF4430
          26FF432F26FF422E25FF412D24FF402D24FF271A14FFBFBFBFBF8E8E8E8EBFBF
          BFBFBDB9B7C2A17355E7BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B100000000190D
          052A85471CDA954B1BFF00000000000000000000000000000000BFBFBFBF6652
          45FF463126FF442F25FF432E26FF432E25FF291B14FFBFBFBFBF3C210E60A864
          31F7E9AD68FF9B5221FF9A5120FF984F1EFF6C3815B700000000B1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1AE6835FFF8D4
          A2FFFAC37BFFFBCE91FFF9CA88FFF9C580FFA45C2AFF00000000BFBFBFBF6953
          48FF483228FF463127FF453127FF442F26FF2A1C15FFBFBFBFBF3E231160AB6C
          40F5EFCA9DFFB06A37FFAF6936FFAE6734FF7F4B25BD00000000B1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1000000001A0F
          082790562ED6B7723EFF00000000000000000000000000000000BFBFBFBF6B55
          49FF4A3429FF483328FF473228FF463228FF2C1C16FFBFBFBFBF89898989BFBF
          BFBFBEBCB9C1BD9472E7BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1BFBFBFBF755D
          50FF553D30FF543C2FFF533B2FFF523A2EFF50392DFF4F392CFF4F382CFF4D37
          2BFF4C362BFF4B352AFF4A3429FF483428FF2E1E17FFBFBFBFBFB1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1BFBFBFBF765F
          51FF573F31FF563E31FF553C30FF543C2FFF533C2FFF523A2EFF51392DFF5039
          2DFF4E382CFF4E372CFF4C362BFF4B352AFF2F1F18FFBFBFBFBF8E8E8E8EBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000089898989BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF89898989BFBFBFBF6B55
          49FF4A3429FF493328FF473228FF473128FF463126FF442F26FF432F26FF422E
          25FF412D24FF402D24FF402C23FF3E2B23FF261913FFBFBFBFBFB1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1BFBFBFBF6E57
          4AFF4C362AFF4B352AFF4A3429FF493329FF483328FF463127FF453027FF4430
          26FF432F26FF422E25FF412D24FF402D24FF271A14FFBFBFBFBF8E8E8E8EBFBF
          BFBFBFBFBFBFA17355E7BCB8B7C2BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1000000000000
          000000000000954B1BFF733715DA1509032A0000000000000000BFBFBFBF6652
          45FF463126FF442F25FF432E26FF432E25FF291B14FFBFBFBFBF723E1BB79E55
          24FF9C5422FF9B5221FFE5AB69FF924E22F73117086000000000B1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1BA7B49FFFCE0
          B5FFFBD195FFFBCE92FFF8C074FFF5C380FFA45C2AFF00000000BFBFBFBF6953
          48FF483228FF463127FF453127FF442F26FF2A1C15FFBFBFBFBF8E603CBDBF80
          4FFFBD7F4EFFBC7D4CFFF0CDA2FFAF6F3FF53E24116000000000B1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1000000000000
          000000000000C28453FF975F34D61B1008270000000000000000BFBFBFBF6B55
          49FF4A3429FF483328FF473228FF463228FF2C1C16FFBFBFBFBF89898989BFBF
          BFBFBFBFBFBFC49F7FE7BFBCBAC1BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1BFBFBFBF755D
          50FF553D30FF543C2FFF533B2FFF523A2EFF50392DFF4F392CFF4F382CFF4D37
          2BFF4C362BFF4B352AFF4A3429FF483428FF2E1E17FFBFBFBFBFB1B1B1B1BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFB1B1B1B1BFBFBFBF765F
          51FF573F31FF563E31FF553C30FF543C2FFF533C2FFF523A2EFF51392DFF5039
          2DFF4E382CFF4E372CFF4C362BFF4B352AFF2F1F18FFBFBFBFBF8E8E8E8EBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000091919191BFBFBFBFBFBFBFBF919191919191
          9191BFBFBFBFBFBFBFBF91919191000000000000000000000000000000000000
          0000000000000000000000000000BFBFBFBF2D1B16FF251611FFBFBFBFBFBFBF
          BFBF281812FF1F120DFFBFBFBFBF000000000000000000000000000000000000
          0000000000000000000000000000BFBFBFBF412A23FF301D17FFBFBFBFBFBFBF
          BFBF3D271FFF2A1914FFBFBFBFBF000000000000000000000000000000000000
          0000000000000000000000000000BFBFBFBF4A3229FF36221CFFBFBFBFBFBFBF
          BFBF452D25FF301D17FFBFBFBFBF000000000000000000000000000000000000
          00003D3D3D3D94949494BFBFBFBFBFBFBFBF4E352DFF3B261FFFBFBFBFBFBFBF
          BFBF4A3228FF35211AFFBFBFBFBF000000000000000000000000000000003A3A
          3A3ABFBFBFBF98918ED364514CF150382EFF553A31FF412B24FFBFBFBFBFBFBF
          BFBF4F362CFF39261EFFBFBFBFBF000000000000000000000000000000009D9D
          9D9D9F9794D3553B31FF624C42F7A39C98D15A3F34FF473028FFBFBFBFBFBFBF
          BFBF543A31FF3F2A23FFBFBFBFBF00000000000000000000000000000000BBBB
          BBBB715A51F45C4137FFA29B99D0BFBFBFBF5E4338FF4E362CFFBFBFBFBFBFBF
          BFBF593E34FF462F27FFBFBFBFBF00000000000000000000000000000000BBBB
          BBBB694D42FD5D4337FFBAB9B8C2BFBFBFBF63473DFF543A31FFBFBFBFBFBFBF
          BFBF5E4438FF4C342AFFBFBFBFBF00000000000000000000000000000000BBBB
          BBBB7B645AF463473CFFA39C99D0BFBFBFBF694B40FF5A3F36FFBFBFBFBFBFBF
          BFBF64473CFF533930FFBFBFBFBF222222220000000000000000000000009D9D
          9D9DA89F9CD26C5043FF6C5449F8A59C99D36C4F44FF61463BFFA6A09ECEA9A2
          9FCE684C41FF594035FFA39E9BCEBFBFBFBF0000000000000000000000003838
          3838BFBFBFBFA9A19DD1816B61F16E5246FD715347FF6D5044FF664B3FFF694D
          41FF6D5044FF674A3FFF563D32FFBFBFBFBF0000000000000000000000000000
          00003636363694949494BBBBBBBBBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBF919191910000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000008888
          888AB8B8B8C1B6B6B6C2B6B6B6C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C3B5B5B5C3B5B5B5C3B7B7B7C28787878A0000000000000000BBBB
          BBC15F483EFF4B3328FF493328FF483227FF463026FF452F26FF432E25FF422D
          25FF412C24FF402C22FF3F2B22FF231813FFB9B9B9C10000000000000000AEAE
          AEB2B3B3B3C3AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AEAEAEC5AEAE
          AEC5AFAFAFC4B4B4B4C3B5B5B5C2B8B8B8C28C8C8C8F0000000000000000BBBB
          BBC0634E42FF4D352AFF4B3429FF4A3329FF493328FF473127FF463026FF442F
          26FF291C17FFBABABAC10000000000000000000000000000000000000000AEAE
          AEB2B5B5B5C3B2B2B2C4B2B2B2C4B1B1B1C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4B1B1B1C4B5B5B5C2B7B7B7C2B8B8B8C18888888A0000000000000000BBBB
          BBC0685245FF4F382CFF4E372CFF4C352BFF4B342AFF4A3328FF493328FF4832
          27FF473026FF452F26FF432E25FF2C1E18FFBBBBBBC10000000000000000AFAF
          AFB2B7B7B7C2B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2
          B2C4B3B3B3C3B6B6B6C2B8B8B8C2B9B9B9C18D8D8D8F0000000000000000BCBC
          BCC06E5648FF523B2DFF50392DFF4F382CFF4D372CFF4D362AFF4B3429FF4B33
          28FF35241DFFBBBBBBC00000000000000000000000000000000000000000AFAF
          AFB2B8B8B8C1B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B5B5B5C3B8B8B8C2B9B9B9C1BABABAC18888888A0000000000000000BDBD
          BDC071594CFF543C2FFF533B2EFF513A2DFF50392DFF4F382CFF4E372BFF4C35
          2BFF4B342AFF4B3329FF493328FF37271EFFBBBBBBC000000000000000008D8D
          8D8EBCBCBCC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BABABAC1BABABAC1BABABAC1BBBBBBC18D8D8D8F00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000008888
          888AB8B8B8C1B6B6B6C2B6B6B6C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C3B5B5B5C3B5B5B5C3B7B7B7C28787878A0000000000000000BBBB
          BBC15F483EFF4B3328FF493328FF483227FF463026FF452F26FF432E25FF422D
          25FF412C24FF402C22FF3F2B22FF231813FFB9B9B9C100000000000000008D8D
          8D8FB9B9B9C1B7B7B7C2B5B5B5C2B1B1B1C4AFAFAFC5AFAFAFC5AEAEAEC5AEAE
          AEC5AFAFAFC4B4B4B4C3B5B5B5C2B8B8B8C28C8C8C8F00000000000000000000
          00000000000000000000BBBBBBC160493FFF493328FF473127FF463026FF442F
          26FF291C17FFBABABAC100000000000000000000000000000000000000008888
          888ABABABAC1B8B8B8C1B7B7B7C2B2B2B2C3B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4B1B1B1C4B5B5B5C2B7B7B7C2B8B8B8C18888888A0000000000000000BBBB
          BBC0685245FF4F382CFF4E372CFF4C352BFF4B342AFF4A3328FF493328FF4832
          27FF473026FF452F26FF432E25FF2C1E18FFBBBBBBC100000000000000008D8D
          8D8FBBBBBBC1B9B9B9C1B8B8B8C2B5B5B5C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2
          B2C4B3B3B3C3B6B6B6C2B8B8B8C2B9B9B9C18D8D8D8F00000000000000000000
          00000000000000000000BCBCBCC0695346FF4D372CFF4D362AFF4B3429FF4B33
          28FF35241DFFBBBBBBC000000000000000000000000000000000000000008888
          8889BBBBBBC0BBBBBBC1B9B9B9C1B6B6B6C2B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B5B5B5C3B8B8B8C2B9B9B9C1BABABAC18888888A0000000000000000BDBD
          BDC071594CFF543C2FFF533B2EFF513A2DFF50392DFF4F382CFF4E372BFF4C35
          2BFF4B342AFF4B3329FF493328FF37271EFFBBBBBBC000000000000000008D8D
          8D8EBCBCBCC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BABABAC1BABABAC1BABABAC1BBBBBBC18D8D8D8F00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000008888
          888AB8B8B8C1B6B6B6C2B6B6B6C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C3B5B5B5C3B5B5B5C3B7B7B7C28787878A0000000000000000BBBB
          BBC15F483EFF4B3328FF493328FF483227FF463026FF452F26FF432E25FF422D
          25FF412C24FF402C22FF3F2B22FF231813FFB9B9B9C100000000000000008D8D
          8D8FB9B9B9C1B7B7B7C2B5B5B5C2B1B1B1C4AFAFAFC5AFAFAFC5AEAEAEC5AEAE
          AEC5ADADADC5ADADADC5ACACACC5B0B0B0C4ADADADB300000000000000000000
          00000000000000000000BBBBBBC160493FFF493328FF473127FF463026FF442F
          26FF432E25FF422D25FF412C23FF271B15FFBABABAC100000000000000008888
          888ABABABAC1B8B8B8C1B7B7B7C2B2B2B2C3B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4AFAFAFC4AFAFAFC5AFAFAFC5B2B2B2C4AEAEAEB30000000000000000BBBB
          BBC0685245FF4F382CFF4E372CFF4C352BFF4B342AFF4A3328FF493328FF4832
          27FF473026FF452F26FF432E25FF2C1E18FFBBBBBBC100000000000000008D8D
          8D8FBBBBBBC1B9B9B9C1B8B8B8C2B5B5B5C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2
          B2C4B2B2B2C4B1B1B1C4B1B1B1C4B4B4B4C3AEAEAEB200000000000000000000
          00000000000000000000BCBCBCC0695346FF4D372CFF4D362AFF4B3429FF4B33
          28FF493228FF473127FF473026FF32231BFFBBBBBBC000000000000000008888
          8889BBBBBBC0BBBBBBC1B9B9B9C1B6B6B6C2B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B3B3B3C3B3B3B3C3B3B3B3C3B5B5B5C2AEAEAEB20000000000000000BDBD
          BDC071594CFF543C2FFF533B2EFF513A2DFF50392DFF4F382CFF4E372BFF4C35
          2BFF4B342AFF4B3329FF493328FF37271EFFBBBBBBC000000000000000008D8D
          8D8EBCBCBCC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BABABAC1BABABAC1BABABAC1BBBBBBC18D8D8D8F00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000008888
          888AB8B8B8C1B6B6B6C2B6B6B6C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C3B5B5B5C3B5B5B5C3B7B7B7C28787878A0000000000000000BBBB
          BBC15F483EFF4B3328FF493328FF483227FF463026FF452F26FF432E25FF422D
          25FF412C24FF402C22FF3F2B22FF231813FFB9B9B9C10000000000000000AEAE
          AEB2B3B3B3C3AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AEAEAEC5AEAE
          AEC5ADADADC5ADADADC5ACACACC5B0B0B0C4ADADADB30000000000000000BBBB
          BBC0634E42FF4D352AFF4B3429FF4A3329FF493328FF473127FF463026FF442F
          26FF432E25FF422D25FF412C23FF271B15FFBABABAC10000000000000000AEAE
          AEB2B5B5B5C3B2B2B2C4B2B2B2C4B1B1B1C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4AFAFAFC4AFAFAFC5AFAFAFC5B2B2B2C4AEAEAEB30000000000000000BBBB
          BBC0685245FF4F382CFF4E372CFF4C352BFF4B342AFF4A3328FF493328FF4832
          27FF473026FF452F26FF432E25FF2C1E18FFBBBBBBC10000000000000000AFAF
          AFB2B7B7B7C2B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2
          B2C4B2B2B2C4B1B1B1C4B1B1B1C4B4B4B4C3AEAEAEB20000000000000000BCBC
          BCC06E5648FF523B2DFF50392DFF4F382CFF4D372CFF4D362AFF4B3429FF4B33
          28FF493228FF473127FF473026FF32231BFFBBBBBBC00000000000000000AFAF
          AFB2B8B8B8C1B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B3B3B3C3B3B3B3C3B3B3B3C3B5B5B5C2AEAEAEB20000000000000000BDBD
          BDC071594CFF543C2FFF533B2EFF513A2DFF50392DFF4F382CFF4E372BFF4C35
          2BFF4B342AFF4B3329FF493328FF37271EFFBBBBBBC000000000000000008D8D
          8D8EBCBCBCC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BABABAC1BABABAC1BABABAC1BBBBBBC18D8D8D8F00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        ImageClass = 'TdxPNGImage'
        Image.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000019549444154785E9593CD4AF3401486E722445444109BB449DAAAA5FE
          23D160ABB6F8036E34A9B6E9172A5DE9BE3760295610056FA1469BB87313BA11
          FD5014EFE875664C2AB831593C8490799F73664E86A4AC878046F29F4B94AA4B
          008486065D4283EAD2E9239255578B2C60011AF4F4D67F2855C7934D2708B429
          BD3F683381BA78F288FACD27D853AA389A2FE8E5F6EA482EECFC30BF0D658EB1
          85E1F10C98842826AF0AB9E2D070178972D7F305CD101D34096BB976FD41A472
          17D6D53B491CDD87D9425B99DDE0EB48A27C4FFC20CCCB37123FBC0B043D6DD7
          823453A41420658B940286C62679EB72769D0B78C52A0D02E088A53B12620B4D
          399BF705B462E5E295878ECE5FA8C08E368540201EDEE197A0A7164DC43379C4
          A7F310A718399F350C8E2A60121E6041A164C3683D13C1B0499429B080271A36
          04E316319DE3859982905EE585080DAA99E307EC9F3D61BAE63241FF475A5E2F
          41486B88A518AB10280323126F5D48AFF802FD96B0AA9B0D0F137AC79B38E884
          9A42FF2EC498E0A0A34E5A0E6858F305A1213410D0A090A8822FE6D39F2CD93A
          0FF00000000049454E44AE426082}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00002C2724FF2A2521FF0000000025201DFF231E1BFF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000312C29FF2F2A26FF000000002A2521FF28231FFF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000037322EFF342F2CFF000000002F2B27FF2D2824FF00000000000000000000
          000000000000000000000000000000000000000000001918173154504DC0514C
          48F83C3733FF3A3531FF0000000035302CFF322D29FF00000000383330FF3631
          2EFF332F2BFF312C29FF2E2A26FF2C2724FF00000000595451C046413EFF443F
          3BFF413D38FF3F3A36FF000000003A3531FF37322EFF00000000000000000000
          000000000000000000000000000000000000000000005C5956F84C4642FF8F89
          84FF47413DFF453F3BFF000000003F3B37FF3D3834FF00000000433E3BFF413C
          38FF3E3936FF3C3733FF393531FF37322EFF00000000615C59F8514B47FF948E
          89FF4C4743FF4A4440FF0000000045403CFF423D3AFF00000000000000000000
          0000000000000000000000000000000000000000000063605CC055504CFF534E
          4AFF514C48FF4F4945FF4C4743FF4A4541FF48423FFF45403CFF4E4844FF4B46
          42FF494440FF46413EFF443F3BFF413D38FF000000001E1D1B3166605DC06760
          5CF856504CFF544E4AFF514C48FF4F4A46FF4D4744FF4B4541FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000057514DFF554F
          4BFF534E49FF514B47FF4E4945FF4C4743FF4A4440FF47423EFF45403CFF423D
          3AFF403B37FF3D3834FF3B3632FF383330FF36312DFF332E2AFF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000005F5955FF5D57
          53FF5B5652FF5A5450FF58524EFF56504CFF544E4AFF514C48FF4F4A46FF4D47
          44FF4B4541FF48433FFF46403DFF433E3AFF403B37FF3E3935FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000005C5652FF5A5450FF58534FFF5650
          4DFF544F4BFF524C49FF504B47FF4D4844FF4B4642FF494340FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          000A000000160000001B000000170000000B0000000200000000000000010000
          00060000000B0000000F0000000C0000000600000001000000000000000F2616
          1072583424D8693D2AFF4D2C1FD91D110B750000001200000003000000082416
          1162513023CF613927FF45281BD01A0F0A630000000900000001291A136DA380
          70FFD4C5BDFFEDE5E1FFCFBEB7FF907163FF1F120C770000000F2C1C1663A181
          72FFD4C6BEFFEEE8E4FFCDC0B8FF866A5DFF190E096000000005734937DEE3D7
          D1FFCEAE9EFFAF7957FFD0B39EFFD7C9C3FF553324E00000001B77503FD8E2D7
          D2FFD0AD9BFFB37753FFD2B29AFFD6CBC5FF4A2B1ED90000000A8E5A47FBF8F6
          F4FFAE725BFFE7AF66FFB27F5EFFF0EBE8FF744835FF000000489A6B55FFF8F6
          F3FFB27056FFE8AF64FFB67E59FFEAE3DFFF613B29FA0000000B90604DF8F1E8
          E4FFD2B0A5FFAA6C53FFD4B6A7FFCDC0BAFF71422FFF4C2920FF80513DFFF1E8
          E3FFD3ADA1FFAE684FFFD4B2A1FFC5B6AFFF6A4231F700000009583C31A2CAAF
          A3FFF7F2EEFFFDFCFBFFEAE0DAFF9E847AFF774835FF663E36FF855944FFE8D9
          CFFFF7F0EDFFFCFAF9FFDFD4CEFF8A6657FF452D229A000000050805041D9F72
          5DF8E8DBD4FFE6D7CEFFCDB7AAFF8B6B5EFF7D4C3AFF865F56FF8B5F4BFFE9D8
          CEFFEADCD4FFCFB9ACFF997B6DFF966A54F70705041600000001000000076047
          3BA4D0B7ADFFEEE3DFFFD5C1B7FF93766CFF82533FFF734635FF926550FFECE0
          D9FFEBDED8FFC4B0A5FF9B7868FF5F45389D0000000400000000000000022219
          1541B89383FFF3EBE8FFD9C9C2FFA99389FFA67966FF00000026B48B76FFF0E7
          E3FFEADED9FFBDA79FFFA67D6CFF211915390000000100000000000000000000
          0007846554C6DDCCC4FFE6DCD8FFC2A99EFFA67E69F90000000CB38E79F8E0D1
          C9FFE9DFDAFFBCA497FF846555C2000000030000000000000000000000000000
          0002130F0D237D6152B7AF8673FA7B6050B9130E0C270000000414100E1E8365
          58B3B68F79F9806455B3130F0D1E000000010000000000000000000000000000
          0000000000010000000400000006000000050000000200000000000000000000
          0001000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          20000000000000040000000000000000000000000000000000000000000C0000
          0007000000030000000A00000010000000050000000000000000000000000000
          0000000000030000000C000000100000000F00000008000000021115A0FF0304
          1F470000000B090C467B0C0F94FF000000100000000000000000000000000000
          00000000000A00632FFF00612EFF005929FF002611860000000912168AD40709
          4A8C0000001E0F1373BA090C66BA0000000D0000000000000000000000000000
          00000000000B00622EFF00190C5A001B0D5A005929FF000B052F12186696212A
          BDFF1F28BCFF161BACFF06083F79000000070000000000000000000000000000
          000000000009006934FF001B0D57001D0F5600602DFF0006031C0A0D3451171E
          AEFF05061A43161CAAFF02031633000000020000000000000000000000000000
          000000000007007B3FFF00783EFF007138FF00361B930000000800000009242D
          C1FF1A2083B5131997E10000000A000000000000000000000000000000000000
          000000000006007B3FFF0020105000211250007037FF00070418000000041D26
          89B0242EC1FF0E11619100000005000000000000000000000000000000000000
          000000000004008444FF0022124D001B0F3D00793EFF00100828000000021419
          546B212BC3FF070A314B00000002000000000000000000000000000000000000
          000000000002009653FF009351FF008A49FF0045238C000000030000000E0000
          00140000001700000013000000070000000200000000000000020000000B0000
          0014000000170000001800000019000000190000001300000005A86D49EEB576
          4FFFB5754FFFB5754DFF3220155C0000000600000000000000065837258CB26F
          49FFB16E48FFB06E47FFAF6D46FFB06C46FFAF6B45FF00000011855A3FC0DDB9
          94FFE7C696FFC39469FF623B23B0040302190000000600000004000000135635
          219BCC9F78FFE7C493FFE4BD86FFE1B780FFB3734DFF000000144531236ACD9D
          79FFEFD6B1FFE6C495FFB9865DFF623C23AE2D1B115A0604021E2E1D125C6942
          2AB1C29069FFE6C495FFE8C597FFE3BD88FFB97B55FF000000120202010D9F76
          56D2E1C1A2FFF2DDB9FFEACBA0FFC79D73FFB27C54FFA46740FFB6815AFFCEA2
          7AFFE5C59BFFEFD5AEFFE6C6A2FFEBCDA1FFC0845EFF0000000F000000021D16
          102EB98D6AEAE0C09FFFF5E5C8FFF4E0BBFFF3DEB6FFF2DCB2FFF2DCB4FFF3DD
          B6FFF3DFBEFFDCB894FFBD8B65F5DBB691FFC58F67FF0000000D000000000000
          0002110D0A1D8F6D53B4D7AE89FFEBD4B7FFF5E5CAFFF9ECD2FFF3E2C6FFEAD0
          B0FFD3A984FF8B694EB6110D0925644C388BC9986FFF0000000A000000000000
          000000000001000000043E3125517C61499CA68163CDCEA079F9A68162CD7C60
          499D3E302453000000060000000200000006664F3A8400000004}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000002000000090000000B000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000020000000E542F18A08A491EDD000000000000
          0000000000000000000000000000000000010000000500000008000000030000
          000000000000000000030000000E553019A0C79A6AFF975125EC000000000000
          0000000000000000000000000002000000092D180B63A06136EC0000000A0000
          0004000000091009052C683C1FBAC69561FFDFC295FF7D4824C8000000000000
          000000000001000000040F09042762371BB7B07A4AFFB27243FF140C07362D1B
          0F5B5C371EA69D643BF9D2A66FFFEECD94FFCCA37AFF472B177B000000000000
          0002000000073922126B996239F3D7AE77FFF3D597FFB57A4CFFA66D42FFB47F
          51FFCEA26EFFEECC90FFEFCD92FFEEDEB6FFA36C43E90805031A00000002150E
          0829764B2CBFC19262FFF2D49AFFF5DA9FFFF6DA9DFFF4D79DFFF4D79CFFF4D6
          9AFFF3D59AFFF3D89FFFF5ECC5FFCAA177FF3825175E0000000500000003B185
          5DEBEADDBCFFFBF7D4FFFCF3CCFFFCF3CEFFFCF2CAFFFAECC0FFF8E6B6FFF6E3
          B2FFF6ECC3FFF1EBCAFFCDA981FF5139257D0000000800000001000000011C16
          0F2A947251C4DABE99FFFAF7D8FFFDFAD9FFFDF7D4FFFDFDE1FFF5EFD0FFEADB
          BAFFD6B892FFAE825CE83D2D1F5E000000070000000100000000000000000000
          0001000000054D3D2C6AC19D78F2ECDFBDFFFEFDDFFFB48359FFAB845EE18A6A
          4CBB4E3C2A700705041300000004000000010000000000000000000000000000
          0000000000000000000315110D21896E51B3DABF9AFFBC8D64FF0000000B0000
          0005000000030000000100000000000000000000000000000000000000000000
          0000000000000000000000000001000000044336285BB08865E80706040E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000200000003000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000070000
          0006000000020000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000008F522ADB5531
          1A9A0000000A0000000200000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000009E5F34EACB9D
          6AFF55321B9B0000000A00000002000000000000000000000003000000080000
          0005000000010000000000000000000000000000000000000000835332C3E4C1
          8FFFCB9D66FF673D22B7100A052800000008000000040000000A9F5F34EC2E19
          0B650000000B00000003000000010000000000000000000000004B321F73D1A6
          7BFFF5D498FFD7AE75FF9E673DF85C371FA42D1B0F59140C0736B16F40FFAF78
          48FF623619B90F08042B0000000600000002000000000000000009060414A878
          50E8F1DAAEFFF5D699FFF2D295FFCFA671FFB48152FFA66D42FFB47749FFF1D0
          94FFD3A972FF985E35F3392010710000000C0000000300000001000000033A2A
          1C57CFA67DFFFAE7BDFFF6DCA3FFF6DA9DFFF4D79DFFF4D79CFFF4D69AFFF3D5
          9AFFF2D498FFEDCB92FFBD8B5AFF734627C3140C073100000004000000000000
          0004543E2B77D2AC84FFF5E3C1FFFBEDC8FFFBF0CAFFFBF2CDFFFCF2CCFFFBF0
          C8FFF9EABEFFF6E0AEFFF7E2B7FFE8CDA8FFA9764BEC00000007000000000000
          0001000000043E2F2259B18962E7DABD9CFFECDCC2FFF6EDD8FFFEFBE9FFFDF6
          D3FFFCF7D9FFF9F3DDFFD6B590FF8F6846C81B130D3200000003000000000000
          00000000000000000002070504104F3E2C6E8B6C4EBAAB845EE1B48157FFFEFB
          E5FFEBDBC2FFBF976FF34B392870000000090000000300000000000000000000
          000000000000000000000000000100000002000000050000000BBB8B62FFD9BC
          9BFF886A4DB515100C2600000005000000010000000000000000000000000000
          000000000000000000000000000000000000000000000706040EAF8863E94235
          275D000000060000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000040000
          0003000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000002000000090000000E00000010000000110000
          0012000000130000001400000015000000150000000E00000003000000000000
          00000000000000000000000000077C6242C0AB8658FFA98356FFA78153FFA67E
          51FFA37C4FFFA1794CFF9F784AFF9E7547FF6F5331C50000000E000000000000
          000000000000000000000000000AB08B5EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E7648FF00000013000000000000
          0000000000000000000000000009B38E61FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA17A4CFF00000013000000000000
          0000000000000000000000000008B49165FFFFFFFFFFE0B693FFDDB28EFFDAAE
          89FFD7A984FFD5A57FFFD1A17AFFFFFFFFFFA47D50FF00000011000000030000
          00070000000A0000000D00000015AB8A61FFECECECFFE9E9E9FFE7E7E7FFE5E5
          E5FFE3E3E3FFE6E6E6FFF3F3F3FFFDFDFDFFA78153FF0000000F1225334E3877
          A3E93C81B2FF397FB0FF377EB0FF357BAEFF347AADFF3278ABFF3077ABFF2E76
          A9FF2D74A8FF3372A3FF87817AFFF3F3F3FFA98456FF0000000E3F81AEF17BB7
          D8FF87D3F2FF83D1F0FF7ECEF0FF7ACCEFFF76CAEFFF72C8EEFF6FC6EEFF6AC3
          EDFF67C0ECFF4A9CCCFF3477A8FFE7E7E7FFAC875AFF0000000C478BBAFFB7E6
          F7FF8ED7F3FF8BD5F3FF7D8C8EFF73675DFF7ECDF0FF675B52FF697A7EFF72C7
          EEFF6FC5EEFF6BC4ECFF2F76A9FFE5E5E5FFAF8A5DFF0000000B4B8EBCFFBFE9
          F9FF97DBF4FF92DAF4FF7D7369FF8BD5F2FF87D3F2FF83D0F0FF675B51FF7ACC
          F0FF76CAEEFF72C7EEFF3279ACFFE7E7E7FFB28D61FF000000094E93BFFFC7ED
          FAFF9DDEF5FF8C999AFF81786EFF92D9F4FF8ED7F3FF8BD5F2FF6C6056FF6F7F
          81FF7ECEF0FF7ACBF0FF357BAEFFEAEAEAFFB49164FF000000085296C1FFCFF0
          FBFFA5E2F6FFA1E0F6FF857B73FF9ADCF5FF96DBF4FF93D9F3FF71665BFF8BD5
          F3FF87D2F1FF83D1F1FF387EB0FFEDEDEDFFB69367FF000000065599C4FFD6F3
          FBFFBEEBFAFFA9E4F6FF919D9DFF857B73FF9FDEF5FF7B7267FF808D8EFF93D9
          F4FF8ED7F3FF8BD5F3FF3B81B2FFAF8E65FF896F4EC0000000035A9CC6FF9CC9
          E1FFD7F3FBFFD3F1FBFFCEF0FBFFC9EEFAFFC5ECF9FFBFEAF9FFBAE8F8FFB5E5
          F8FFAFE3F7FF77B5D7FF3F84B5FF0000000D00000002000000011727313F487C
          9DC9599BC5FF569AC4FF5498C3FF5296C1FF4F94C0FF4D91BEFF4B90BCFF498D
          BBFF478CBAFF39749AD911212D49000000040000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000002000000090000000E00000010000000110000
          0012000000130000001400000015000000150000000E00000003000000000000
          00000000000000000000000000077C6242C0AB8658FFA98356FFA78153FFA67E
          51FFA37C4FFFA1794CFF9F784AFF9E7547FF6F5331C50000000E000000000000
          000000000000000000000000000AB08B5EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E7648FF00000013000000000000
          0000000000000000000000000009B38E61FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA17A4CFF00000013000000000000
          0000000000000000000000000008B49165FFFFFFFFFFE0B693FFDDB28EFFDAAE
          89FFD7A984FFD5A57FFFD1A17AFFFFFFFFFFA47D50FF00000011000000030000
          00070000000A0000000D00000015AB8A61FFECECECFFE9E9E9FFE7E7E7FFE5E5
          E5FFE3E3E3FFE6E6E6FFF3F3F3FFFDFDFDFFA78153FF0000000F1225334E3877
          A3E93C81B2FF397FB0FF377EB0FF357BAEFF347AADFF3278ABFF3077ABFF2E76
          A9FF2D74A8FF3372A3FF87817AFFF3F3F3FFA98456FF0000000E3F81AEF17BB7
          D8FF87D3F2FF83D1F0FF7ECEF0FF7ACCEFFF76CAEFFF72C8EEFF6FC6EEFF6AC3
          EDFF67C0ECFF4A9CCCFF3477A8FFE7E7E7FFAC875AFF0000000C478BBAFFB7E6
          F7FF8ED7F3FF818F92FF786D63FF73675EFF7ECDF0FF685D52FF65584EFF6577
          7CFF6FC5EEFF6BC4ECFF2F76A9FFE5E5E5FFAF8A5DFF0000000B4B8EBCFFBFE9
          F9FF97DBF4FF80776CFF8ED7F3FF786E64FF87D3F2FF6D6357FF7ECEF0FF6558
          4EFF76CAEEFF72C7EEFF3279ACFFE7E7E7FFB28D61FF000000094E93BFFFC7ED
          FAFF9DDEF5FF8B9898FF80776DFF7B7268FF8ED7F3FF74685EFF87D3F1FF675D
          53FF7ECEF0FF7ACBF0FF357BAEFFEAEAEAFFB49164FF000000085296C1FFCFF0
          FBFFA5E2F6FFA1E0F6FF9FDEF5FF80766CFF96DBF4FF796E63FF74675EFF7785
          87FF87D2F1FF83D1F1FF387EB0FFEDEDEDFFB69367FF000000065599C4FFD6F3
          FBFFBEEBFAFF93A1A2FF867D74FF92AEB5FF9FDEF5FF7C7268FF96DBF4FF93D9
          F4FF8ED7F3FF8BD5F3FF3B81B2FFAF8E65FF896F4EC0000000035A9CC6FF9CC9
          E1FFD7F3FBFFD3F1FBFFCEF0FBFFC9EEFAFFC5ECF9FFACC7CFFFBAE8F8FFB5E5
          F8FFAFE3F7FF77B5D7FF3F84B5FF0000000D00000002000000011727313F487C
          9DC9599BC5FF569AC4FF5498C3FF5296C1FF4F94C0FF4D91BEFF4B90BCFF498D
          BBFF478CBAFF39749AD911212D49000000040000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000500000012000000190000
          001B0000001D0000001E00000020000000220000001C00000008000000000000
          0000000000000000000000000000000000000000000ECA9361FFC8905FFFC78D
          5CFFC58B58FFC28955FFC18753FFBF8451FFBF834EFF0000001A000000000000
          0000000000000000000000000000008B0FFF00280357CB9462FFFDF9F6FFFCF8
          F4FFFCF7F3FFFBF6F1FFFBF5EFFFFBF4EDFFC08451FF0000001F000000000000
          00000000000000A713FF009D12FF00920FFF008A0EFFCC9463FFFDFAF8FFFDF9
          F6FFFCF8F4FFFCF7F3FFFBF6F1FFFBF5EFFFC18653FF0000001B000000000000
          00000000000000AF17FF00000000009B13FF002D0553CD9564FFFEFBF9FFDDB2
          8FFFDBAE8AFFD8AB85FFD5A780FFFBF6F1FFC38955FF00000018000000020000
          000A0000001500B919FF000000270000002900000028CD9665FFFEFCFBFFFEFB
          F9FFFDFAF8FFFDF9F6FFFCF8F4FFFCF7F3FFC48B59FF000000140000000C0B1B
          2A65163659B11C4B7EDF1D5999F81A5797F8144478E3CE9768FFFEFDFCFFE1B8
          96FFDFB492FFDDB08EFFDAAD89FFFCF8F4FFC78E5BFF0000001100000018306E
          AAFF69A3CEFF9BCCE7FF99CFEDFF70B3E0FF408CC9FFCF9969FFFFFEFDFFFEFD
          FCFFFEFCFBFFFEFBF9FFFDFAF8FFFDF9F6FFC8905EFF0000000D000000193270
          ACFFA1D7F1FFBEEBFBFFA1D7F2FF76B9E4FF4B9CD5FFD19A6BFFFFFEFEFFFFFE
          FDFFFEFDFCFFFEFCFBFFFEFBF9FFFDFAF8FFCA9361FF0000000A000000153673
          ADFFA1D6F2FFBEEBFBFFA0D7F2FF76BAE3FF4B9DD6FFD19C6CFFD09B6BFFCF9A
          69FFCE9968FFCE9766FFCE9766FFCD9565FFCC9664FF00000007000000113976
          AFFFA1D7F2FFBEEBFBFFA1D7F2FF76B9E4FF4B9CD6FF2E88CCFF1C5CA2FF0000
          002A0000000000000002000000040000000600000008000000030000000E3D79
          B1FFA1D7F2FFBEEBFBFFA1D7F2FF76BAE4FF4C9CD5FF2E88CCFF2060A3FF0000
          00270000000000000000000000000000000000000001000000000000000B407C
          B2FFA0D7F1FFBEEBFBFFA1D7F2FF76B9E3FF4B9CD6FF2E88CCFF2462A5FF0000
          0024000000000000000000000000000000000000000000000000000000084380
          B4FFA5DCF6FFB1E9FEFFB0E8FEFFB3E6FDFF99D1F1FF5EA7DBFF2967A7FF0000
          0021000000000000000000000000000000000000000000000000000000044682
          B5FF78B5DAFF93D0EDFFABE2FAFFB4E4FAFFA3D0ECFF71A6D2FF2E6CA9FF0000
          0019000000000000000000000000000000000000000000000000000000011221
          2D442545619033618ACA3C75A7F23872A6F22C5B87D01D3D5C9E0D1C2B590000
          0009000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000036000000851E1F20F5161616F51616
          16F5181818F51B1B1BF51F2020F5262727F5000000BF0000004D000000000000
          0000000000000000000000000000000000B9494949FF494949FF616161FF9090
          90FFA9A9A9FFBCBCBCFFC8C8C8FFCAC9C9FFBDBDBCFF668080FF000000000000
          0000000000000000000000000000000000ADC8C8C8FF535353FF848484FF9595
          95FFA8A8A8FFBDBDBDFFD4D4D4FFE9E9E9FFFFFFFFFF2A2A2AF6000000000000
          0000000000000000000000000000000000ADE9E9E9FFBFBFBFFF555555FFA0A0
          A0FFB2B2B2FFC6C6C6FFDCDCDCFFEAEAEAFFFFFFFFFF303030F6000000000000
          0000000000000000000000000000000000ADCCCCCCFFE9E9E9FFAEAEAEFF6664
          62FFC6C3BFFFD6D6D6FFE9E9E9FFF4F4F4FFFFFFFFFF343434F6000000000000
          0000000000000000000000000000000000AABFBDBAFFCECCC9FFE6E2DDFF8994
          A3FF5F8EA8FFB8B5B1FFC0C0C0FFD3D3D3FFF4F4F4FFA4A4A4FF000000CE0000
          00D6000000D6000000D000000029000000B9638BA2FF4B86B2FF2F60B4FF1052
          BCFF000000B900000029000000D0000000D6000000D6000000CE1588CFFF51CF
          F9FF2AA8E6FF1993D8FF1086CFFF0664CCFF0262CDFF0682D6FF0C92E5FF14A1
          F3FF000000AA000000000000000000000000000000000000000010314EF68FEB
          FFFF52DBFFFF49D6FFFF3DD2FFFF34CDFFFF2BC6FFFF26C1FFFF23BCFFFF1DB1
          FCFF000000AD0000000000000000000000000000000000000000143552F6A8F3
          FFFF61E1FEFF55DAFEFF46D4FEFF3BCEFEFF33C8FEFF2BC2FEFF25BEFFFF27B8
          FBFF000000AD0000000000000000000000000000000000000000153856F6C8FE
          FFFF94EDFFFF83E6FFFF57DEFEFF47D7FEFF3DD1FEFF39CEFFFF35CBFFFF3FC7
          FDFF000000AD000000000000000000000000000000000000000036ADEAFF96D7
          F6FFA2E2FAFFA9EBFCFFAAF1FFFFA1F0FFFF8BE4FFFF5AD4F9FF43C1F1FF2EA4
          E0FF000000B900000000000000000000000000000000000000000000004D0000
          00BF1A3B4FF515364EF513334CF513334BF5113049F5103048F5153347F50000
          0085000000360000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFF0000F8000000F8000000F8000000F8000000F8000000F800
          000000000000001F0000001F0000001F0000001F0000001F0000001F0000FFFF
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000E0000006200000009000000000000
          00000000000000000000000000000000000000000000000000000000001E0000
          004700000000000000000000000A000000AD000000FF00000083000000000000
          0000000000000000000000000000000000000000000000000000000000690000
          00F10000004400000008000000B0000000FF000000E700000041000000000000
          0000000000000000000000000000000000000000000000000000000000AA0E0E
          0EFF000000E6000000C1000000FF000000E70000003900000000000000000000
          00000000001200000078000000C4000000D6000000D4000000BE040303ED1415
          15FF111111FF0C0C0CFF000000E9000000360000000000000000000000000000
          0030000000D1E6CBB4FFF8E9DCFFFFF7EFFFFFFEF4FFC5B9AFFF201F1FFF1E1F
          1FFF1A1A1AFF151515FF000000E9000000430000000000000000000000190000
          00D5F6DDC7FFFFF0E1FFFFF1E5FFFFF0E4FFFFF7EAFFA19A94FF282929FF2C2C
          2CFF262626FF202021FF1B1B1BFF060606F3000000350000000000000089ECC5
          A1FFFDE4CDFFFDE8D4FFFFECDDFFFFEEE1FFFEEDDFFF605D5AFF393A3AFF3738
          38FF2E2F31FF0F0F10F4000000B1000000500000001100000000020000DCF7D5
          B6FFFBDFC5FFFDE6D1FFFFEAD9FFFFF0DFFFE0D0C3FF4D4D4DFF424344FF5654
          52FF9D8E81FF000000C6000000000000000000000000000000002B1E12F0FAD9
          BBFFFADABFFFFDE3CCFFFFE7D4FFFFEFDBFFCABBB0FF826764FFB9AA9EFFEDD1
          B9FFFFDFC2FF000000D2000000000000000000000000000000002F2317F1FBE2
          CBFFFADDC3FFFCE1C9FFFEE4CEFFFEE4CEFFF8DFCAFFF6DEC8FFFFE7CEFFFFE2
          C8FFFDE4CDFF000000D600000000000000000000000000000000060401E1FAE4
          D0FFFDE8D6FFFDE9D9FFFEEADAFFFEEADAFFFFEBDBFFFFECDCFFFDE9D9FFFEEA
          D9FFF6E0CBFF000000C50000000000000000000000000000000000000099F5D9
          BEFFFDEEE1FFFDEDDFFFFEEFE2FFFEEFE3FFFEEFE3FFFEEFE2FFFDEDE0FFFFF1
          E5FFEBCBAEFF0000007700000000000000000000000000000000000000280E0A
          07E7FCEFE2FFFEF4ECFFFEF2E8FFFEF1E7FFFEF1E7FFFFF2E8FFFFF6EDFFFAE9
          DAFF000000D10000001600000000000000000000000000000000000000000000
          00450A0806E5F6E0CCFFFDF2E8FFFFF9F5FFFFF9F4FFFCF1E6FFF2D9C0FF0000
          00D20000002F0000000000000000000000000000000000000000000000000000
          00000000002600000099060402E030261CF130261BF1010000DA000000890000
          0018000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFF80000FF300000FF000000FF010000C003000080030000000100000001
          0000000F0000000F0000000F0000000F0000000F0000000F0000801F0000C03F
          0000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000002200000070000000B0000000D4000000EB000000EB0000
          00D4000000B00000007000000022000000000000000000000000000000000000
          000A00000080000000EB100F0FFF282625FF3B3A3AFF464444FF454342FF3634
          34FF1E1C1AFF000000FF000000EB000000800000000A00000000000000140000
          00B2323030FF918C87FFCCC3BCFFEADFD7FFEFD2B0FFEEBF86FFECBD83FFECCD
          ABFFE8DCD3FFC7BCB4FF866661FF1B1917FF000000B2000000140000008D8C89
          86FFE7DFD9FFFFF9F2FFFFF7F3FFF3D8B7FFE1A540FFDB9E37FFD99B34FFD89A
          33FFEECFABFFFFF3ECFFFFF5EAFFE1D6CDFF5F5A56FF0000008D00000075FFFF
          FAFFFFFAF5FFFAF4EFFFFAF4F0FFEBBF88FFE3A946FFE2A949FFDEA541FFDA9D
          35FFE1B15EFFF8EFE8FFF7EDE6FFFDF2EAFFFFF6EEFF0000007500000075FFFF
          FFFFFFFDFBFFFBF8F5FFFBF9F6FFEEC18AFFF1CD9FFFF3DBBBFFE3AB4CFFDEA1
          3BFFE4B665FFF9F4F0FFF9F2EDFFFEF7F1FFFFFAF4FF000000750000008DA3A2
          A2FFEEEDEDFFFFFFFFFFFFFFFFFFFAE3C6FFF3C993FFF1CC9BFFE7AC4AFFE5AA
          46FFF5DDBFFFFFFFFFFFFFFFFDFFEAE5E2FF8F8C8BFF0000008D000000140000
          00B2646464FFB3B3B2FFE0E0E0FFF6F7F8FFFAE6CBFFFBD29FFFFAD2A1FFF8E3
          CAFFF5F4F4FFDBDADAFFA8A7A6FF4D4D4DFF000000B200000014000000000000
          000A00000080080808EB585858FF828283FF919296FF999C9FFF989A9DFF8C8E
          91FF616262FF4B4B4BFF060606EB000000800000000A00000000000000000000
          0000000000000000002200000070000000B0000000D4080808EB080808EB0000
          00D4000000B00000007000000022000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          7E000000424D7E000000000000003E0000002800000010000000100000000100
          010000000000400000000000000000000000020000000000000000000000FFFF
          FF00FFFF0000FFFF0000FFFF0000E00700008001000000000000000000000000
          000000000000000000000000000080010000E0070000FFFF0000FFFF0000FFFF
          0000}
      end>
  end
  inherited ilLargeImages: TcxImageList
    FormatVersion = 1
    ImageInfo = <
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000010000000200000004000000060000000700000007000000070000
          0007000000070000000800000008000000080000000800000008000000080000
          0008000000080000000800000008000000080000000800000008000000080000
          0008000000070000000500000003000000010000000000000000000000000000
          0000000000020000000800000011000000180000001B0000001C0000001D0000
          001D0000001D0000001D0000001D0000001E0000001E0000001E0000001E0000
          001F0000001F0000001F0000001F0000001F0000002000000020000000200000
          001F0000001C0000001400000009000000020000000000000000000000000000
          000100000004000000117E5E52C1AF8271FFAE8172FFAE8171FFAE8070FFAD80
          70FFAD7F70FFAC7F6FFFAC7E6EFFAC7E6EFFAB7E6DFFAB7D6DFFAB7D6CFFAB7D
          6CFFAA7C6CFFAA7B6BFFA97C6BFFA97A6AFFA97A6AFFA97A6AFFA87969FFA879
          69FFA87969FF78564AC300000014000000050000000100000000000000000000
          00010000000600000016B08374FFFDFCFAFFFBF8F6FFFBF8F5FFFBF7F5FFFBF7
          F4FFFAF7F4FFFBF6F3FFFBF6F3FFFAF6F2FFFAF5F2FFFAF5F1FFFAF4F1FFF9F4
          F1FFF9F4F0FFF9F3EFFFF8F2EEFFF8F3EEFFF8F2EDFFF8F2ECFFF8F1ECFFF7F0
          ECFFF7F0EBFFA8796AFF0000001B000000070000000100000000000000000000
          00010000000600000018B18576FFFDFCFBFFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFF6EDE7FFF5EDE6FFF5ECE6FFF5ECE6FFF6ECE6FFF5ECE5FFF5EBE5FFF5EC
          E5FFF4EBE4FFF5EBE4FFF5EBE4FFF5EAE4FFF4EBE3FFF5EAE3FFF4EAE3FFF4EA
          E3FFF8F1ECFFA97B6AFF0000001E000000080000000100000000000000000000
          00010000000600000018B38777FFFEFCFBFFF6EEE9FFF7EFE8FFF6EFE9FFF6EE
          E8FFF6EDE8FFF6EDE7FFF6EDE7FFF5EDE7FFF6EDE7FFF5ECE6FFF5EDE6FFF5EC
          E6FFF5ECE5FFF5ECE5FFF5ECE5FFF4ECE5FFF4EBE4FFF5EBE4FFF5EAE4FFF5EA
          E3FFF9F2EDFFAA7D6CFF0000001E000000070000000100000000000000000000
          00010000000600000017B48979FFFEFCFBFFF6F0EAFFF7EFEAFFF6EFEAFFF6EF
          E9FFF7EEE8FFF7EEE8FFF6EEE8FFF6EEE7FFF6EEE7FFF6EDE7FFF5EDE6FFF6ED
          E6FFF6ECE6FFF5EDE6FFF5ECE5FFF5EBE5FFF5EBE5FFF4EBE5FFF5EBE4FFF5EB
          E4FFF8F2EDFFAC7D6DFF0000001D000000070000000100000000000000000000
          00010000000500000016B58B7CFFFEFDFCFFF8F0EBFFF7EFEAFFF7EFEAFFF7EF
          EAFFF7EFEAFFF7EFE9FFF6EEE9FFF6EEE9FFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFF5EDE7FFF6EDE7FFF6ECE6FFF5ECE6FFF5ECE5FFF5ECE5FFF5EBE5FFF5EC
          E4FFF9F3EEFFAC7F6FFF0000001C000000070000000100000000000000000000
          00010000000500000015B68C7EFFFEFDFCFFF7F1ECFFF7F1EBFFF8F0EBFFF8F0
          EBFFF7EFEAFFF6F0EAFFF6EFEAFFF6EFEAFFF7EFE9FFF6EFE8FFF6EEE8FFF6EE
          E8FFF6EEE7FFF6EDE8FFF6EDE7FFF6EDE6FFF5ECE7FFF5ECE6FFF5ECE6FFF5EC
          E6FFFAF4F0FFAD8070FF0000001B000000070000000100000000000000000000
          00000000000500000014B78E80FFFEFDFDFFF8F1EDFFF8F1ECFFF8F1ECFFF8F0
          EBFFF8F0ECFFF7F0EBFFF7F0EAFFF7F0EAFFF7EFEAFFF7EFE9FFF7EFE9FFF7EF
          E8FFF6EEE8FFF6EEE8FFF6EEE8FFF6EDE7FFF6EDE7FFF6EDE7FFF6EDE7FFF5EC
          E7FFF9F4F1FFAF8272FF0000001A000000070000000100000000000000000000
          00000000000500000013B88F82FFFEFDFDFFF8F2EEFFF8F1EDFFF8F2EDFFF8F1
          ECFFF8F1ECFFF8F1ECFFF8F1EBFFF7F0EBFFF7F0EBFFF7F0EAFFF7F0EAFFF6EF
          EAFFF7EFEAFFF6EFE9FFF6EEE9FFF6EEE8FFF6EEE8FFF6EEE8FFF6EEE8FFF6ED
          E7FFFAF5F2FFAF8373FF00000019000000060000000100000000000000000000
          00000000000400000012BB9184FFFEFEFDFFF8F3EEFFF8F2EEFFF8F2EDFFF8F2
          EDFFF8F2ECFFF7F1ECFFF8F1ECFFF8F1EBFFF8F1ECFFF7F0EBFFF8F0EBFFF8F0
          EAFFF7F0EAFFF7F0EAFFF7EFE9FFF6EFE9FFF7EFE9FFF6EEE9FFF6EEE8FFF6EE
          E8FFFAF5F3FFB18575FF00000018000000060000000100000000000000000000
          00000000000400000011BB9485FFFEFEFDFFF9F4F0FFF9F3EFFFF9F3EEFFF9F3
          EEFFF9F2EEFFF8F2EEFFF8F2EDFFF8F2EDFFF8F2ECFFF8F1ECFFF7F1ECFFF8F0
          EBFFF7F0EBFFF7F0EBFFF7F0EBFFF7EFEAFFF7F0EAFFF6EFE9FFF7EEE9FFF6EE
          E8FFFAF6F3FFB28677FF00000017000000060000000100000000000000000000
          00000000000400000010BC9788FFFEFEFDFFF9F3F0FFF9F4EFFFF9F3F0FFF9F4
          EFFFF9F3EEFFF8F3EEFFF8F3EEFFF8F2EEFFF9F2EDFFF8F2EDFFF8F1EDFFF8F1
          EDFFF8F0ECFFF7F1EBFFF7F1EBFFF7F0EBFFF8F0EBFFF7F0EAFFF6F0EAFFF7F0
          EAFFFBF7F4FFB48979FF00000016000000060000000100000000000000000000
          0000000000040000000FBF988AFFFEFEFEFFFAF5F1FFF9F4F1FFFAF4F0FFF9F4
          EFFFF9F4EFFFF9F3EFFFF9F3EFFFF8F3EFFFF8F2EEFFF9F3EDFFF9F2EDFFF8F2
          EDFFF8F2ECFFF8F2ECFFF7F1ECFFF7F0ECFFF8F0ECFFF8F1ECFFF7F0EBFFF8F0
          EBFFFBF8F5FFB58A7AFF00000015000000050000000100000000000000000000
          0000000000040000000EC0998BFFFEFEFEFFFAF5F2FFFAF5F1FFFAF5F2FFF9F4
          F0FFF9F4F0FFF9F4F0FFF9F4F0FFF9F3F0FFF9F3EFFFF9F3EFFFF9F3EEFFF9F2
          EEFFF9F2EDFFF8F1EDFFF8F2EDFFF8F1EDFFF8F2ECFFF8F1ECFFF7F0ECFFF8F0
          EBFFFBF8F6FFB58C7DFF00000015000000050000000100000000000000000000
          0000000000030000000EC19C8DFFFFFEFEFFFBF6F3FFFAF6F2FFFAF6F2FFFAF5
          F2FFFAF5F1FFF9F4F1FFF9F4F0FFF9F4F0FFF9F4F0FFF9F3EFFFF9F3F0FFF8F3
          EFFFF9F3EFFFF8F2EEFFF8F2EDFFF8F2EEFFF8F1EDFFF8F1EDFFF8F1EDFFF8F1
          ECFFFBF9F7FFB78E7FFF00000014000000050000000100000000000000000000
          0000000000030000000DC29D8FFFFFFEFEFFFBF7F4FFFAF6F4FFFAF6F3FFFAF6
          F2FFFAF5F3FFFAF5F2FFF9F5F1FFF9F4F1FFFAF4F1FFF9F5F1FFF9F4F1FFF9F3
          F0FFF9F3EFFFF9F3EFFFF9F3EFFFF9F3EEFFF9F3EEFFF8F2EEFFF8F2EEFFF8F1
          EDFFFCF9F7FFB88F81FF00000013000000050000000000000000000000000000
          0000000000030000000CC49F90FFFFFEFEFFFBF7F5FFFBF7F4FFFBF6F3FFFBF7
          F3FFFBF6F3FFFAF6F3FFFAF5F3FFFAF5F2FFFAF5F1FFFAF5F1FFFAF5F1FFF9F4
          F1FFFAF4F1FFF9F4F1FFF9F4F0FFF9F3EFFFF9F3EFFFF8F3EFFFF8F2EEFFF8F2
          EEFFFAF7F5FFB99283FF00000012000000040000000000000000000000000000
          0000000000030000000BC4A192FFFFFFFEFFFBF7F5FFFBF8F5FFFBF7F5FFFAF7
          F4FFFAF7F4FFFAF6F3FFFAF6F3FFFBF6F2FFFAF6F2FFFAF6F2FFFAF5F2FFFAF5
          F1FFFAF5F1FFFAF4F1FFFAF5F0FFFAF4F0FFF9F3F0FFF9F3EFFFF9F3EFFFF7F1
          ECFFF9F4F3FFBB9284FF00000011000000040000000000000000000000000000
          0000000000020000000AC6A294FFFFFFFEFFFCF8F6FFFBF8F5FFFBF8F5FFFCF8
          F5FFFBF7F4FFFBF8F4FFFBF7F4FFFBF7F4FFFBF6F3FFFAF6F3FFFAF6F2FFFAF5
          F3FFFAF6F2FFFAF5F2FFFAF5F2FFFAF5F1FFFAF5F1FFF9F4F1FFF7F2EDFFF6EF
          EAFFF7F2EFFFBD9486FF00000010000000040000000000000000000000000000
          00000000000200000009C8A495FFFFFFFFFFFCF9F6FFFBF9F6FFFCF9F6FFFBF8
          F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFAF7F4FFFBF7F4FFFBF7F4FFFBF7
          F3FFFAF6F3FFF9F5F2FFFAF6F3FFF9F5F1FFF9F4F0FFF7F2EDFFF5EFEAFFF3EA
          E6FFF3EDEAFFBD9888FF0000000F000000040000000000000000000000000000
          00000000000200000008C8A597FFFFFFFFFFFCFAF8FFFCFAF7FFFCF9F7FFFCF9
          F6FFFBF8F7FFFBF8F6FFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFBF7F4FFFAF7
          F5FFFBF7F4FFF9F4F1FFF7F1EDFFF6EFEBFFF4ECE6FFF1E7E3FFEFE4DFFFECE0
          DBFFECE1DDFFBF988AFF0000000E000000040000000000000000000000000000
          00000000000200000008C9A698FFFFFFFFFFFCFAF9FFFCFAF8FFFCF9F8FFFCF9
          F7FFFCF9F7FFFCF9F7FFFBF9F6FFFCF8F6FFFBF8F6FFFCF8F6FFFBF8F6FFFAF6
          F4FFFAF6F3FFF6EFEAFFEFE3DEFFE7D9D2FFE2D3CBFFE1CFC8FFDFCCC4FFDCC8
          BFFFDCC9C2FFC19A8CFF0000000D000000030000000000000000000000000000
          00000000000200000007CBA899FFFFFFFFFFFDFAF9FFFDFAF9FFFCFAF8FFFCFA
          F8FFFCFAF7FFFDFAF7FFFCF9F7FFFBF9F7FFFCF9F7FFFCF9F6FFFBF8F6FFFCF8
          F6FFF9F3F1FFF0E7E1FFB89284FFAC7F6FFFAB7E6DFFAB7D6DFFAB7C6CFFAA7C
          6CFFD1B8AFFFC29D8DFF0000000A000000030000000000000000000000000000
          00000000000100000006CBA99BFFFFFFFFFFFDFBFAFFFDFAFAFFFCFAF9FFFCFA
          F9FFFDFAF8FFFCFBF8FFFCFAF8FFFCFAF8FFFCF9F7FFFCF9F7FFFCF9F7FFFBF9
          F6FFF8F3F0FFEDE0DCFFB18676FFFFFFFFFFFFFEFEFFFFFDFCFFFEFCFAFFFCF9
          F7FFD1B7AEFF533C358600000006000000020000000000000000000000000000
          00000000000100000005CBAA9DFFFFFFFFFFFDFBFAFFFDFBFAFFFDFBF9FFFDFB
          FAFFFDFAF9FFFDFBF9FFFDFAF8FFFDFAF9FFFCFAF8FFFCFAF8FFFCFAF7FFFCF9
          F7FFF7F2EEFFECE0DBFFB68D7DFFFFFEFEFFFEFBFAFFFDF9F7FFFCF6F3FFD4BA
          B0FF553F38860000000800000003000000010000000000000000000000000000
          00000000000100000005CCAB9DFFFFFFFFFFFEFCFBFFFEFCFAFFFEFBFAFFFDFB
          FBFFFDFCFAFFFDFBFAFFFDFBF9FFFDFBFAFFFDFAF9FFFCFAF8FFFCFAF9FFFAF7
          F5FFF6F1EDFFEDE2DCFFBC9485FFFFFEFEFFFDF9F6FFFBF6F3FFD6BCB4FF5843
          3B86000000080000000300000001000000000000000000000000000000000000
          00000000000100000004CDAC9FFFFFFFFFFFFDFCFCFFFDFDFCFFFDFCFBFFFDFC
          FBFFFDFCFAFFFDFCFAFFFEFBFAFFFDFBFAFFFDFBF9FFFDFBF9FFFAF8F7FFF9F5
          F3FFF5EEECFFECE2DDFFC19C8CFFFFFEFEFFFBF6F3FFD9C1B7FF5B463F850000
          0007000000030000000100000000000000000000000000000000000000000000
          00000000000100000003CEAD9FFFFFFFFFFFFEFCFCFFFEFDFCFFFEFDFCFFFDFC
          FCFFFDFDFCFFFEFCFBFFFEFCFBFFFDFCFAFFFDFCFAFFFBF8F7FFF9F6F4FFF7F2
          EFFFF3ECE8FFEDE2DDFFC6A293FFFFFEFEFFDBC3BAFF5D494284000000060000
          0002000000010000000000000000000000000000000000000000000000000000
          00000000000100000002CEADA0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBF9F9FFF9F6F4FFF6F1
          F0FFF2ECE9FFEEE3E0FFE5D4CEFFE0CCC4FF5F4D458300000005000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          00000000000000000001998076BECEAEA0FFCEADA0FFCEAE9FFFCEADA0FFCEAD
          9FFFCDAC9FFFCEACA0FFCDAC9FFFCDAC9EFFCDAC9FFFCCAC9EFFCCAB9EFFCCAA
          9DFFCCAB9CFFCBAA9CFFCBAA9CFF614F48820000000400000002000000010000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000020000
          0004000000060000000600000006000000060000000600000006000000060000
          0006000000060000000600000006000000070000000700000007000000070000
          0007000000070000000700000007000000070000000700000007000000070000
          0006000000040000000200000001000000000000000000000000000000060000
          000E000000150000001700000018000000180000001800000018000000190000
          001900000019000000190000001900000019000000190000001A0000001A0000
          001A0000001A0000001A0000001A0000001A0000001B0000001B0000001A0000
          001700000010000000070000000200000000000000000000000000000010173B
          7ACD2557A4FF2859A5FF2758A6FF2757A5FF2558A5FF2656A3FF2656A4FF2456
          A4FF2455A4FF2455A3FF2354A2FF2353A2FF2252A2FF2253A2FF2151A1FF2151
          A1FF2051A1FF2050A0FF1F4FA0FF1F4F9FFF1F4E9FFF1E4D9FFF1D4D9EFF1C4B
          9CFF173F8AEE040A143D0000000B000000020000000000000000000000151D50
          9FFF2559A5FF4F95CFFF64BBEDFF47A7E7FF46A5E6FF44A4E6FF42A3E5FF40A2
          E4FF3FA1E4FF3E9FE3FF3C9EE3FF3B9DE2FF3A9CE1FF389BE1FF389AE1FF359A
          E1FF3598E0FF3497E0FF3397DEFF3296E0FF3195DEFF3095DEFF3095DEFF2E90
          DAFF297DCAFF12326BBF00000011000000040000000000000000000000162154
          A3FF3166ADFF3C7BBCFF7ECBF2FF4FADE9FF4BAAE8FF49AAE8FF47A7E6FF45A6
          E6FF43A5E5FF43A3E4FF40A2E5FF40A1E4FF3FA0E4FF3D9FE3FF3B9DE3FF3A9D
          E2FF399CE2FF379BE1FF379AE2FF3599E1FF3398E1FF3298DFFF3197DFFF3096
          DFFF3295DEFF1A4A98F80103051D00000005000000010000000000000014245C
          A9FF447ABAFF2862ACFF8CD1F2FF64BCEEFF52B0EBFF51AFEAFF50AEE9FF4DAD
          E8FF4CAAE7FF4AA9E7FF48A8E7FF47A7E6FF45A5E6FF43A4E6FF42A3E5FF41A1
          E4FF3FA1E4FF3D9FE4FF3C9FE3FF3B9EE3FF3A9DE2FF399CE1FF379AE1FF3699
          E0FF389CE1FF2363B1FF08162E5D000000080000000100000000000000132661
          AFFF548AC4FF2866AEFF7CB9E1FF82CFF3FF59B6EDFF57B5EDFF57B4ECFF54B3
          ECFF53B1EAFF51B0EAFF51AEE8FF4FADE8FF4DABE8FF4EACE8FF4CAAE8FF4BAA
          E7FF47A6E5FF44A6E6FF43A4E5FF41A3E5FF40A2E4FF3FA1E3FF3E9FE2FF3C9F
          E2FF3D9FE3FF2F7AC4FF0F2A56970000000A0000000200000000000000122A67
          B1FF649BCEFF3272B8FF619DCFFF9FDEF8FF5FBAF0FF5EBAEFFF5EB9EEFF5DBA
          EDFF62BBEEFF68BFEFFF6ABEEFFF6CC0EEFF69BDEDFF66BBEDFF63B9EDFF60B9
          ECFF5EB6ECFF5BB4EBFF56B1E9FF51ADE9FF4CA9E8FF47A6E6FF45A5E5FF43A4
          E5FF44A4E4FF4198D9FF164385D80000000D0000000300000001000000112D6C
          B5FF74AAD7FF4287C6FF4083C0FFB5E8FBFF71C6F3FF6FC4F2FF76C8F3FF7ACA
          F4FF7ACAF2FF78C8F3FF77C7F2FF73C6F1FF72C4F1FF70C2F0FF6DC0EFFF6ABF
          EEFF68BDEEFF65BAEEFF62B9ECFF5FB7EBFF5DB6EBFF5AB4EAFF54B0E9FF4EAC
          E7FF49A9E6FF50ADE8FF225EAAFF030A132C0000000400000001000000103071
          B7FF84B9DFFF5399D3FF3A7EBEFFADDCF1FFA5E1F9FF86D1F6FF85D0F6FF83CF
          F6FF82CEF5FF7FCDF3FF7DCBF3FF7CCBF3FF7ACAF2FF79C8F2FF76C6F1FF73C6
          F2FF70C4F0FF6DC2F0FF6CC1EFFF68BDEEFF66BCEDFF63B9ECFF60B8ECFF5EB6
          EBFF59B4EAFF5AB3EAFF357CC1FF0C2344740000000700000002000000103176
          BAFF93C6E7FF61ABDDFF4590CBFF8DC0E0FFC2EDFCFF8BD5F7FF8AD5F7FF89D3
          F6FF87D2F6FF86D1F6FF85D1F5FF83D0F5FF81CFF4FF7FCDF4FF7ECDF3FF7DCB
          F3FF7AC9F2FF77C8F2FF74C6F0FF72C5F0FF70C3F0FF6CC1F0FF6ABFEEFF66BD
          EEFF64BBEDFF66BCEDFF53A0D8FF153F75BA00000009000000020000000F347B
          BDFFA1D3EEFF70BBE6FF5AA8DBFF66A5D2FFD7F6FDFF95DCF9FF90D8F8FF8FD8
          F8FF8DD7F8FF8CD5F8FF8BD5F7FF88D3F6FF88D3F6FF86D1F6FF85D1F4FF82D0
          F4FF81CFF4FF80CEF4FF7DCCF4FF7BCBF3FF78C9F1FF75C7F1FF72C6F1FF70C5
          F1FF6DC1EFFF6CC1EFFF73C1EDFF215DA4F701030614000000030000000E357E
          BFFFAEDFF4FF7ECAEFFF73C2ECFF4C97CDFFCAEBF7FFDEF8FEFFDEF8FEFFDDF8
          FEFFDCF8FEFFDCF8FEFFDBF7FEFFD9F6FEFFD7F5FCFFBDECFBFF8FD7F7FF89D3
          F7FF87D2F6FF85D3F6FF84D1F5FF82D0F5FF80CEF5FF7ECDF4FF7CCBF3FF79CA
          F2FF76C8F2FF73C6F1FF7CCAF1FF3A7EBFFF091C3255000000050000000D3782
          C1FFBBE7F8FF8DD8F5FF84D2F4FF6ABDE7FF51A1D3FF4C99CCFF4B98CCFF4996
          CBFF4995CAFF4693CBFF4591CAFF4490C8FF75B0D8FFD0F0F9FFD0F4FCFF9BDE
          F9FF8DD7F8FF8CD6F7FF8AD4F7FF88D5F7FF87D3F7FF85D2F5FF83D1F5FF80CF
          F5FF7ECDF3FF7BCCF3FF81CEF3FF5CA4D5FF133A659D000000060000000C3985
          C3FFC5F0FCFF97E3FBFF93E1FAFF8DDCFAFF87D9F9FF83D7F9FF7FD5F8FF7CD2
          F8FF79D1F8FF77CEF6FF73CCF6FF6EC9F5FF5BB4E7FF559DD1FFACD6ECFFE0F9
          FEFFDFF9FEFFDEF9FEFFDEF8FEFFDDF7FEFFDDF7FEFFDBF7FEFFDAF6FEFFDAF6
          FEFFD8F5FDFFD6F5FDFFD5F4FDFFBDE2F2FF1D5897E0000000070000000B3B89
          C5FFCDF6FFFFA2ECFEFF9FEAFEFF9CE8FEFF99E7FEFF96E6FDFF93E4FDFF90E2
          FDFF8EDFFCFF8CDDFCFF89DCFCFF84D9FBFF7DD5FAFF6FCAF5FF52A7DAFF4694
          CAFF4592CAFF4492CAFF4290C9FF418FC8FF3F8DC7FF3E8BC7FF3D8AC6FF3B89
          C5FF226BB1FF226AB1FF216AB0FF2064A6F415416DA4000000050000000B3C8C
          C7FFD1F7FFFFA6EFFFFFA4EEFFFFA2ECFFFFA0EBFEFF9EE9FEFF9CE8FEFF99E7
          FEFF97E5FDFF95E3FDFF92E1FDFF8FE0FCFF8ADDFCFF83D9FBFF7DD4FAFF77D1
          F9FF73CFF9FF70CCF9FF6DC9F8FF6AC7F6FF68C5F6FF64C2F5FF62C0F4FF7CCC
          F6FF2C6CB1FF000000170000000D0000000800000006000000030000000A3D8F
          C9FFD4F9FFFFAAF1FFFFA8F0FFFFA6EFFFFFA4EEFFFFA2EDFFFFA0EBFFFF9EEA
          FEFF9CE8FEFF9AE7FEFF97E5FDFF95E3FDFF92E2FDFF8FDFFCFF8CDDFCFF89DA
          FBFF84D9FBFF81D7FBFF7ED5FAFF7CD3FAFF79D1F9FF76CEF9FF73CDF8FF8DD6
          F9FF357DBEFF0000000F00000005000000020000000100000001000000093E91
          CBFFD6FAFFFFADF3FFFFACF2FFFFAAF1FFFFA8F0FFFFA6EFFFFFA4EDFFFFA2ED
          FFFFA0EBFEFF9EEAFEFF9CE9FEFF9AE6FEFF97E6FEFF95E4FDFF93E2FDFF90E0
          FCFF8EDFFCFF8BDDFCFF88DBFBFF85D9FBFF83D7FAFF81D5FAFF7DD3F9FF95DC
          FBFF3985C4FF0000000C00000003000000000000000000000000000000084095
          CDFFDAFAFFFFB0F5FFFFAFF4FFFFADF3FFFFACF2FFFFAAF1FFFFA8F0FFFFA6EF
          FFFFA4EEFFFFABEFFEFFC0F6FEFFCEF8FEFFCDF8FEFFCDF8FEFFCDF8FEFFCCF8
          FEFFCCF8FEFFCBF8FEFFCBF8FEFFCBF8FEFFCAF8FEFFCAF8FEFFC9F8FEFFA7DE
          F1FF367CB3E90000000A00000003000000000000000000000000000000084297
          CEFFDBFBFFFFB3F6FFFFB2F6FFFFB0F5FFFFAFF4FFFFADF3FFFFACF2FFFFAAF1
          FFFFAEF2FEFF92D2ECFF3F93CBFF3F92CBFF3F92CBFF3F91CBFF3F91CAFF3F91
          CAFF3E90CAFF3D90CAFF3E8FC9FF3D8FC9FF3D8EC9FF3D8DC8FF3D8DC8FF306F
          9ECD0F23314A0000000700000002000000000000000000000000000000074299
          CFFFDDFCFFFFB5F7FFFFB4F7FFFFB3F6FFFFB1F6FFFFB0F5FFFFAFF4FFFFB3F4
          FFFF8ED2ECFF2A6082A80000000B000000090000000900000009000000090000
          0009000000090000000900000009000000090000000A0000000A0000000A0000
          0009000000060000000300000001000000000000000000000000000000053D8D
          BDE8B2E3F3FFD5F9FEFFD4F9FEFFD4F9FEFFD4F9FEFFD3F9FEFFD3F9FEFFC5F0
          FAFF4893C1EA050B0F1A00000004000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000010000000100000000000000000000000000000000000000031127
          3445357CA6CB449DD2FF449DD2FF449DD2FF449DD1FF449DD1FF449CD1FF449C
          D1FF163345590000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0003000000050000000500000005000000050000000500000006000000060000
          0005000000030000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          0001000000010000000300000005000000070000000700000007000000070000
          0007000000070000000700000007000000070000000700000007000000070000
          0007000000070000000700000007000000070000000700000007000000070000
          0007000000070000000600000004000000010000000100000000000000000000
          0001000000060000000E000000160000001A0000001B0000001B0000001B0000
          001B0000001B0000001B0000001B0000001B0000001B0000001C0000001C0000
          001C0000001C0000001C0000001C0000001C0000001C0000001D0000001D0000
          001D0000001C0000001800000010000000070000000100000000000000010000
          00030000000E2D1F198E583E33FD593D34FF583C32FF583C32FF5A3F37FFC58D
          5DFFC18656FFBD8151FFBB7D4DFFB97A4BFFB77748FFB57444FFB37141FFB06D
          3DFFAF6B3AFFAC6837FFAB6535FFA96333FFA76030FF50342DFF4F342CFF4F34
          2CFF4F342DFF4E342DFD2519158B000000100000000400000001000000010000
          0005000000145C3F36FD806357FF745449FF73534AFF735349FF61473EFFE8C5
          94FFE5BE89FFE3B981FFE3B87FFFE3B67DFFE2B57AFFE1B278FFE0B175FFDFAF
          73FFDEAD71FFDEAC6FFFDDAB6DFFDDAA6CFFDDA86AFF523731FF6B4C42FF6B4C
          42FF6B4C42FF6B4D43FF4E342BFB000000170000000600000001000000010000
          0006000000175F4337FF83655AFF77574CFF76574BFF76574BFF654B41FFEAC8
          98FFE6C08CFFE5BC85FFE3B981FFE2B77EFFE2B57CFFE1B479FFE0B277FFE0B0
          75FFDFAF72FFDEAC70FFDEAC6FFFDDAA6CFFDCA96BFF553932FF6B4C43FF6C4D
          42FF6B4C42FF6D4F45FF50362DFF0000001B0000000700000001000000010000
          000600000017624539FF87695DFF79594EFF795A4EFF795A4EFF684E44FFE1C2
          96FFDCB988FFDBB684FFD7B17BFFD7AF79FFD6AD76FFD6AC75FFD6AA72FFD5A8
          70FFD4A66EFFD4A56BFFD3A369FFD3A267FFD3A166FF583D35FF6D4F44FF6D4E
          45FF6C4D44FF6E5047FF51362FFF0000001B0000000700000001000000010000
          00060000001766483CFF8C6E63FF7D5D51FF7D5D50FF7C5C50FF6C5247FFFAF5
          F2FFF9F3F0FFF9F2EFFFF7F1ECFFF7F0EBFFF6F0EAFFF6EEE9FFF5EEE9FFF5EE
          E8FFF4EDE7FFF4ECE7FFF3ECE6FFF4EBE5FFF3EBE4FF5A3F38FF6F5046FF6E50
          46FF6E4F45FF715348FF523830FF0000001A0000000700000001000000010000
          000500000016684B3EFF917366FF816153FF816153FF7F6154FF6E554AFFFAF7
          F3FFF9F4F0FFF9F3EFFFF8F3EDFFF7F0ECFFF6F0EBFFF6EFEBFFF6EFEAFFF5EE
          E9FFF5EDE8FFF5EDE7FFF4ECE7FFF4EBE6FFF4EBE5FF5E4339FF705248FF7052
          48FF6F5147FF72554BFF523A31FF000000190000000600000001000000010000
          0005000000156C4F42FF95786AFF846456FF836557FF836456FF72584DFFFAF7
          F5FFFAF5F2FFD5B8A9FFD2B4A6FFCCA899FFC9A492FFC6A090FFC39C8BFFC097
          87FFBE9283FFBA8F7FFFB88C7BFFF4EDE6FFF4ECE6FF60463DFF72544BFF7053
          4BFF705248FF75574DFF543A32FF000000190000000600000001000000010000
          0005000000156F5346FF997D6FFF87675AFF876859FF876859FF755C50FFFBF8
          F6FFFAF6F2FFFAF5F1FFF9F5F1FFF9F3EFFFF7F2EDFFF7F1EDFFF6F0EBFFF6EF
          EAFFF5EFEAFFF5EEE9FFF5EDE8FFF5ECE8FFF4EDE6FF634940FF73564CFF7355
          4CFF72544BFF775A4FFF563D34FF000000180000000600000001000000000000
          000500000014735649FF9E8275FF8B6B5CFF8A6D5EFF8B6C5DFF796053FFFCF9
          F7FFFAF7F3FFD9BEB1FFD8BCAEFFD5BAABFFCDAC9DFFCBA797FFC8A392FFC6A0
          8FFFC39B8BFFC09787FFBD9283FFF5EEE8FFF4EDE8FF664C43FF75594EFF7458
          4DFF74574CFF795C53FF573D35FF000000180000000600000001000000000000
          00050000001376594BFFA28878FF8E7061FF8F7061FF8E7062FF7C6155FFFCF9
          F8FFFBF7F4FFFAF7F4FFF9F6F3FFF9F5F2FFF9F4F1FFF8F2EEFFF7F1EDFFF7F0
          ECFFF6F0ECFFF6F0EBFFF5EFEAFFF6EEE9FFF4EEE8FF694F46FF775B50FF7759
          4FFF75594EFF7B6056FF594037FF000000170000000600000001000000000000
          0005000000137B5D4EFFA68D7FFF937464FF937464FF917465FF7F6659FFFCFB
          F9FFFCF9F8FFFCF9F8FFFCF9F7FFFBF9F6FFFCF8F5FFF9F6F3FFF9F5F2FFF9F5
          F1FFF9F4F1FFF8F4F0FFF7F2EFFFF7F2EDFFF7F1EDFF6C5248FF785E52FF775D
          51FF775A50FF7F6359FF5C4239FF000000160000000600000001000000000000
          0005000000127E6152FFAB9182FF967767FF957A67FF957968FF876D5EFF8268
          5BFF82695AFF82685AFF81685AFF80675AFF7F6759FF7A6155FF755D51FF745B
          50FF735B50FF725A4FFF72594FFF71594EFF70574CFF72594EFF7B6055FF7A60
          54FF795E52FF81675BFF5D433AFF000000160000000500000001000000000000
          000400000012836654FFB09687FF997B6BFF9A7D6BFF9A7E6BFF9A7D6CFF997D
          6BFF997E6CFF997E6BFF997E6CFF987E6DFF997E6CFF967B6BFF82675BFF7F66
          5AFF806659FF7F6659FF7F655AFF7F6559FF7F6458FF7D6257FF7C6255FF7C61
          55FF7A6054FF826A5EFF5E463CFF000000150000000500000001000000000000
          000400000011866958FFB39B8CFF9D816FFF9D7F6EFF9D816EFF9D816FFF9D81
          70FF9D826FFF9D8270FF9D8270FF9C8170FF9B8170FF9C826FFF8D7364FF8369
          5CFF82695DFF82685CFF81675CFF81675BFF7F6659FF7F6559FF7F6458FF7D63
          58FF7D6257FF866D61FF61483EFF000000140000000500000001000000000000
          0004000000108A6C5BFFB79F91FFA08471FFA08471FFA08571FFA08573FFA085
          73FFA08574FFA08673FFA18673FFA08574FFA08673FF9F8574FF9C8271FF836A
          60FF836B60FF836A5FFF836A5DFF83695DFF82695DFF81685BFF81675AFF7F65
          59FF7F6359FF886E64FF624940FF000000140000000500000000000000000000
          0004000000108D705EFFBBA494FFA48774FFA48874FFAA927FFFAC9582FFAD94
          83FFAD9482FFAC9382FFAC9482FFAC9483FFAB9381FFAA9282FFAA9080FF9880
          71FF8E766BFF8D756AFF8D7568FF8C7468FF897167FF887065FF866E61FF8167
          5CFF806659FF8B7267FF634A40FF000000130000000500000000000000000000
          00040000000F907462FFBFA898FFA68A77FFA78B79FFB09785FF6E5449FF5439
          31FF543831FF60463EFF644A42FF634A40FF61483FFF5F473CFF5E453CFF5C44
          3BFF5B4339FF5A4139FF594137FF583F36FF563E36FF654D43FF887164FF836A
          5DFF82685CFF8D766AFF654C42FF000000120000000500000000000000000000
          00040000000E957764FFC2AD9DFFA98D7BFFAA8F7BFFB29986FF563A33FF5944
          3DFF644E47FF75594EFFE8DAD0FFDCC5B5FFDBC4B5FFDBC4B3FFDAC3B3FFDAC3
          B2FFD9C2B1FFE3D2C6FFE2D1C5FFE2D0C3FFE1CFC2FF573F37FF8A7266FF856C
          5EFF836A5EFF8F796EFF664F44FF000000120000000400000000000000000000
          00030000000E977A67FFC5B0A0FFAC907CFFAD927DFFB39C88FF573B34FF5A45
          3EFF654F48FF775B50FFEBDFD5FFDEC9BBFFDEC9B9FFDDC8B8FFDDC7B7FFDCC6
          B6FF584139FF705448FF705347FF6F5246FFE4D3C7FF594138FF8B7368FF856D
          62FF856B5FFF937B70FF685146FF000000110000000400000000000000000000
          00030000000D9B7D6AFFC8B4A3FFAE947FFFAF9480FFB79E8BFF583C34FF5B46
          3FFF655049FF795D53FFEFE4DBFFE1CEC0FFE1CDBFFFE1CDBDFFE0CBBBFFDFCA
          BBFF554038FF61473FFF654B42FF715548FFE7D8CDFF5B433AFF8C7468FF876F
          62FF876D60FF957E72FF695147FF000000100000000400000000000000000000
          00030000000C9C7F6BFFCAB6A7FFB29681FFB29782FFB89F8CFF593C35FF5B46
          40FF66514AFF7B5F55FFF2E8E0FFE5D3C5FFE4D3C4FFE3D1C2FFE3D0C1FFE2CF
          C0FF523E36FF5B413AFF5E433BFF73564BFFEBDDD4FF5E463CFF8E7469FF8871
          65FF866E64FF968075FF6B5348FF000000100000000400000000000000000000
          00030000000C9E826CFFCDBAAAFFB39983FFB49984FFBBA18DFF593D35FF5C47
          40FF68524BFF7D6157FFF4ECE5FFE8D8CAFFE8D7C9FFE7D6C8FFE6D5C7FFE6D4
          C6FF4E3C34FF553B34FF583E36FF75584DFFEEE2D9FF60483EFF8F766AFF8A72
          66FF896F65FF998377FF6C544AFF0000000F0000000400000000000000000000
          00030000000BA2836EFFCFBCACFFB69B86FFB59B86FFBBA28FFF5A3E36FF5C47
          41FF68524BFF7E6459FFF6F0EAFFEBDCCFFFEADCCEFFEADBCDFFE9DACCFFE8D9
          CBFF4B3A33FF523731FF533832FF765A4FFFF1E6DEFF624A41FF8E786AFF8A73
          67FF897165FF9B8579FF6D564BFF0000000E0000000400000000000000000000
          00030000000AA3876FFFD1BEB0FFB89D89FFB89E87FFBDA390FF5B3E37FF5C48
          42FF69524CFF80665CFFF8F3EEFFEEE1D4FFEDE0D3FFEDDFD2FFECDED1FFEBDD
          D0FF473832FF493832FF493932FF493832FFF3EBE4FF654D43FF90766CFF8D73
          68FF8A7166FF9E877DFF6D564BFF0000000D0000000300000000000000000000
          000200000008A1866EF9D2BFB0FFD3C0B2FFD3C2B1FFD6C5B6FF5B3E37FF5D48
          42FF69534CFF82675DFFF9F5F1FFF9F5F0FFF9F4EFFFF9F4EFFFF8F3EEFFF8F2
          EDFFF7F2ECFFF7F1EBFFF7F1EBFFF6F0EAFFF6EEE9FF664F46FFB4A399FFB3A0
          97FFB19D95FFB09C93FF6E564CFC0000000B0000000300000000000000000000
          00010000000552443984A48873FCA88C75FFA98D76FFA88F78FF836859FF765B
          4DFF765A4DFF80655BFFA9948BFFA89289FFA59087FFA48E85FFA28C82FFA08A
          80FF9E877EFF9C847BFF998278FF987F76FF947C73FF695148FF745F52FF745E
          51FF725B50FF6F584DFC453730A5000000070000000200000000000000000000
          0000000000020000000500000007000000090000000900000009000000090000
          000A0000000A0000000A0000000A0000000A0000000A0000000A0000000A0000
          000B0000000B0000000B0000000B0000000B0000000B0000000B0000000B0000
          000B0000000B0000000A00000007000000030000000100000000000000000000
          0000000000000000000100000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000030000000300000003000000030000000300000003000000030000
          0003000000030000000200000002000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0001000000010000000300000004000000050000000600000006000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0005000000030000000100000001000000000000000000000000000000000000
          0001000000050000000B00000011000000150000001500000016000000160000
          0016000000160000001600000017000000170000001700000017000000170000
          0018000000180000001800000018000000180000001900000019000000180000
          00150000000E0000000600000001000000000000000000000000000000000000
          00020000000B2D1F1A8B583C34FC593D34FF583D34FF5C4138FFC89263FFC084
          54FFBD8151FFBB7D4DFFB97A4BFFB77748FFB57444FFB37141FFB06D3DFFAF6B
          3AFFAC6837FFAB6535FFA96333FFA76030FFA76030FF523730FF51362EFF5035
          2EFE261915890000000E00000003000000010000000000000000000000000000
          0004000000105C4037FC876A60FF785B4FFF785A4FFF6D5246FFE8C594FFE4BC
          85FFE4BA83FFE3B981FFE2B77EFFE2B57CFFE1B479FFE0B277FFE0B075FFDFAF
          72FFDEAC70FFDEAC6FFFDDAA6CFFDCA96BFFDCA96BFF5E423AFF6C4F44FF6F50
          48FF50352EFB0000001400000005000000010000000000000000000000000000
          00050000001260443BFF8B6F64FF7B5E52FF7B5D51FF70554AFFDFBE90FFD9B3
          7FFFD8B17DFFD7B17BFFD7AF79FFD6AD76FFD6AC75FFD6AA72FFD5A870FFD4A6
          6EFFD4A56BFFD3A369FFD3A267FFD3A166FFDEAA6BFF61443CFF6E4F45FF7253
          49FF52372FFF0000001700000006000000010000000000000000000000000000
          00050000001263473DFF8F7468FF7E6154FF7D6053FF74594EFFFAF6F2FFF8F3
          EEFFF8F2EDFFF7F2ECFFF7F0ECFFF6F0EBFFF6EFEBFFF6EFEAFFF5EEE9FFF5ED
          E8FFF5EDE7FFF4ECE7FFF4EBE6FFF4EBE5FFF3EAE5FF63473EFF6E5046FF7155
          4BFF533831FF0000001700000006000000010000000000000000000000000000
          000400000012664A40FF92786CFF816357FF806356FF785C50FFF9F6F3FFF9F3
          F0FFF8F2EFFFF7F2EDFFF7F1EDFFF6F1ECFFF6F0EBFFF6F0EBFFF6EFE9FFF5EE
          E9FFF5EEE8FFF4EDE7FFF4EDE6FFF4ECE6FFF4EBE6FF674A41FF6F5147FF7456
          4DFF543932FF0000001700000006000000010000000000000000000000000000
          0004000000116A4E42FF987C71FF84675AFF836559FF7C6053FFFAF7F4FFF9F4
          F0FFF9F3EFFFF8F3EFFFF8F2EEFFF7F2EDFFF7F1EDFFF6F0EBFFF6EFEAFFF5EF
          EAFFF5EEE9FFF5EDE8FFF5ECE8FFF4EDE6FFF4EBE6FF694D43FF705248FF7558
          4EFF553A33FF0000001600000006000000010000000000000000000000000000
          0004000000106E5146FF9B8274FF866B5DFF86695CFF7F6457FFFBF8F5FFF9F5
          F1FFF9F4F1FFF8F3F0FFF8F3EEFFF7F3EEFFF7F1EDFFF7F1ECFFF6F0ECFFF6EF
          EAFFF6EFE9FFF5EEE9FFF5EEE8FFF4EDE8FFF4ECE6FF6C5046FF71534AFF765A
          50FF573C34FF0000001500000005000000010000000000000000000000000000
          000400000010715549FF9F8679FF8A6D5FFF896D5EFF836759FFFBF8F6FFFAF5
          F2FFF9F5F1FFF8F4F0FFF8F3F0FFF8F3EFFFF8F2EEFFF7F1EDFFF7F0ECFFF6F0
          ECFFF6F0EBFFF5EFEAFFF6EEE9FFF4EEE8FFF5EDE7FF6F5349FF72554BFF795D
          52FF583D35FF0000001500000005000000010000000000000000000000000000
          00040000000F75594CFFA38B7DFF8C7162FF8C7061FF866B5CFFFBFAF7FFFBF8
          F6FFFBF8F6FFFBF8F5FFFAF7F4FFFBF7F3FFF9F6F3FFF9F5F2FFF8F4F0FFF8F3
          F0FFF7F3EFFFF6F1EEFFF6F1ECFFF6F0ECFFF6EFEAFF72564BFF73564BFF7B5F
          54FF593E36FF0000001500000006000000020000000000000000000000000000
          00040000000F795C4EFFA88F82FF907465FF8F7364FF8A6F60FF886E5EFF886C
          5EFF866B5DFF856A5CFF85695BFF83685AFF7F6557FF795F53FF775D51FF765B
          4FFF755A4EFF74594EFF74584DFF73574CFF73584CFF72564CFF72554BFF7A5F
          54FF583E37FF000000190000000A000000040000000200000001000000000000
          00030000000E7C5F52FFAB9386FF937768FF927767FF917566FF907565FF8F73
          65FF8E7263FF8D7163FF8C7161FF8B7061FF896E5FFF785D53FF73584FFF7056
          4CFF6F544CFF6F544BFF6E544BFF6D534AFF6C5348FF6C5248FF6B5248FF755B
          51FF533C35FF00000026000000180000000D0000000600000002000000000000
          00030000000D806356FFB0988AFF967B6BFF957A6AFF947869FF947868FF9277
          67FF927566FF917565FF907365FF8F7363FF8B7162FF7F6558FF846156FF8A65
          59FF896458FF896457FF886357FF886256FF866255FF866155FF866154FF8A66
          58FF79554AFFA06F5FFF9F6E5FFF724F44BF0000000D00000003000000000000
          00030000000D846758FFB39C8DFF997E6DFF987D6DFF977C6CFF977C6BFF957A
          6BFF957968FF947869FF937767FF927666FF8F7263FF876C5EFF977163FFFBF8
          F5FFFAF7F4FFFAF7F4FFFAF7F4FFFAF7F4FFFAF7F4FFFAF7F4FFFAF7F3FFFAF6
          F3FFF9F6F3FFFAF6F3FFF9F6F3FFA0705FFF0000001200000005000000000000
          00030000000C886C5CFFB7A193FF9C8170FFA38979FFA58B7BFFA48A7BFFA289
          79FFA08779FFA08777FF9F8476FF9D8374FF9A8070FF93786AFF9D786AFFFCF9
          F7FFF6F0EBFFF6F0EAFFF6F0EAFFF6F0EAFFF6F0EAFFF5EFE9FFF5EFE9FFF6EF
          E8FFF5EEE8FFF5EFE8FFFAF6F3FFA17162FF0000001300000005000000000000
          00030000000C8A6F5EFFBAA696FF9F8473FFA88F80FF6C5147FF553931FF5E44
          3CFF614840FF60483FFF5F473EFF5E463DFF5C443CFF584038FF805E52FFFCFA
          F7FFF7F1ECFFF6F0EBFFF6F0EAFFF6F0EAFFF6F0EBFFF6F0EAFFF6EFEAFFF6EF
          EAFFF5EFE9FFF5EFE9FFFAF7F4FFA37363FF0000001200000005000000000000
          00030000000B8E7361FFBDA999FFA28876FFA99282FF563933FF4E423CFF664D
          44FFE9D5C9FFE8D5C9FFE8D5C8FFE6D4C8FFE5D2C5FFDAC9BDFFAB7E6EFFFCFA
          F9FFF7F2EDFFF7F1EDFFF6F1ECFFF7F1EBFFF7F0EBFFF6F1EBFFF7F0EBFFF6F0
          EAFFF6F0EAFFF5F0E9FFFAF7F4FFA47565FF0000001100000004000000000000
          00030000000B917663FFC1AC9DFFA58A79FFAC9382FF563B33FF4F433CFF6A51
          48FFF4E4D5FFEFDED0FFEAD9CBFFE8D6C9FFE5D2C6FFDCC9BEFFAD8070FFFCFB
          F9FFF8F3EEFFF7F2EDFFF7F2EDFFF7F1ECFFF7F1ECFFF7F1ECFFF7F1EBFFF6F1
          ECFFF7F0EBFFF6F0EAFFFAF7F4FFA57667FF0000001000000004000000000000
          00020000000A957967FFC4AFA1FFA78D7AFFAE9484FF573A34FF51443EFF6F55
          4CFFF7E8D9FFF6E8D8FFF5E6D7FFF2E2D4FFE9D8CDFFDFCEC2FFAE8273FFFDFC
          FAFFF8F3EEFFF8F3EEFFF8F2EEFFF8F2EDFFF8F2EDFFF8F2EDFFF7F2ECFFF7F1
          ECFFF7F1ECFFF7F1ECFFFAF8F5FFA77968FF0000000F00000004000000000000
          000200000009987C69FFC6B3A4FFAA917DFFB19686FF583C34FF52463EFF7359
          4FFFF8EADBFFF7E9DAFFF6E8D9FFF5E7D8FFF1E3D4FFE6D7CBFFB08474FFFEFC
          FBFFF9F4F0FFF9F4EFFFF8F3EFFFF8F3EEFFF8F3EEFFF7F2EEFFF8F3EDFFF8F2
          EDFFF7F1ECFFF7F1ECFFFBF8F5FFA97A6BFF0000000E00000004000000000000
          0002000000099A7F6BFFC9B7A7FFAC9280FFB29A88FF593C35FF52473FFF765D
          53FFF9ECDCFFF8EBDCFFF7EADBFFF6E8DAFFF3E5D7FFEBDDD0FFB28677FFFEFC
          FCFFFAF5F1FFF9F4F0FFF9F4F0FFF9F4F0FFF8F4EFFFF9F3EFFFF8F3EFFFF8F3
          EEFFF8F2EEFFF8F2EDFFFBF8F6FFAA7D6DFF0000000D00000003000000000000
          0002000000089D816DFFCCB8ABFFAF9481FFB49B88FF593D36FF534740FF7A60
          56FFFAEDDEFFF9ECDDFFF8EBDCFFF8EADBFFF5E7D8FFECDFD1FFB48878FFFEFD
          FDFFFAF5F1FFF9F5F1FFFAF4F1FFFAF4F1FFF9F4F0FFF9F3EFFFF9F4F0FFF9F3
          EFFFF8F3EFFFF8F2EFFFFBF9F6FFAC7F70FF0000000C00000003000000000000
          000200000008A0846FFFCEBBACFFB9A18EFFB69C89FF5B3D37FF544841FF7E64
          5AFFFBEEDEFFFAEDDEFFF9EDDDFFF8ECDDFFF6E8D9FFEFE2D4FFB58A7AFFFEFE
          FDFFFBF6F2FFFAF5F2FFFAF5F1FFFAF5F2FFFAF4F1FFF9F5F1FFF9F5F0FFF9F4
          F0FFF9F3F0FFF9F4EFFFFBF9F7FFAE8171FF0000000B00000003000000000000
          000100000006A08570FCCEBCAEFFCFBEAFFFD1C0B2FF5B3E37FF554942FF8065
          5CFFFCEFDFFFFBEEDFFFFAEEDEFFFAEDDEFFF7EADBFFF1E5D6FFB78B7DFFFFFE
          FEFFFBF6F3FFFBF6F3FFFAF6F2FFFAF6F3FFFAF6F2FFFAF5F2FFF9F5F2FFFAF4
          F1FFF9F5F0FFF9F4F0FFFCF9F7FFAF8274FF0000000A00000003000000000000
          00010000000451433983A18671FCA38772FFA38771FF6D5146FF5B3E37FF785C
          53FFA9948BFFA8938AFFA69188FFA58F86FFA28C82FF9D887EFFB88E7FFFFFFE
          FEFFFBF8F4FFFBF7F4FFFBF7F4FFFAF6F3FFFBF6F3FFFAF6F3FFFAF5F2FFFAF5
          F2FFFAF6F2FFF9F5F1FFFCF9F8FFB18575FF0000000900000002000000000000
          0000000000010000000400000006000000070000000700000007000000070000
          000800000008000000080000000800000008000000090000000DB98F80FFFFFF
          FEFFFCF8F6FFFBF7F5FFFBF8F4FFFCF7F5FFFBF7F4FFFBF7F3FFFAF6F3FFFBF6
          F3FFFAF5F3FFFAF5F2FFFCFAF8FFB38778FF0000000800000002000000000000
          0000000000000000000100000001000000020000000200000002000000020000
          0002000000020000000200000002000000000000000300000006BB9182FFFFFF
          FFFFFCF8F6FFFCF8F6FFFBF8F6FFFBF7F5FFFBF7F5FFFBF7F5FFFBF7F4FFFBF7
          F4FFFBF6F4FFFBF6F3FFFCFAF9FFB4897AFF0000000700000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000003BC9284FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFB68B7CFF0000000500000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000028C6D62BEBC93
          84FFBC9384FFBC9283FFBC9183FFBB9182FFBA9082FFBA9081FFB99080FFB98F
          80FFB98F7FFFB98E7FFFB88D7EFF88675EBF0000000300000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0002000000030000000300000003000000030000000300000003000000040000
          0004000000040000000400000004000000030000000100000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010101010302020206040202090403020A0403030B0403030B0403
          030B0403030B0503030C0503030C0503030C0503030C0503030C0503030C0503
          030D0403030C0503030B03020208010101030000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000010000020402020A0A0505180D0808230F090928100A0A2A100A0A2B100A
          0A2B110B0A2C110B0A2D110B0A2D110B0A2E110B0A2E120B0B2F120B0B30120B
          0B30120B0B30100A0A2B0C07071E0503030D0101010300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000002020206080605177B584CC6A47564FFA37464FFA37463FFA37363FFA272
          62FFA27261FFA2705FFFA26F5FFFA06F5FFFA06E5DFF9F6E5DFF9F6D5CFF9E6D
          5CFF9E6D5CFF9C6B5BFF775145CA0B07071E0302020801000001000000000000
          0000000000000000000000000001000000010000000100000001000000010100
          0002030202090C080721A67767FFFEFDFCFFFCFAF8FFFCFAF8FFFCFAF7FFFCF9
          F7FFFCF9F7FFFCF8F6FFFBF8F6FFFBF7F5FFFBF7F5FFFBF6F4FFFBF7F3FFFAF6
          F3FFFBF5F2FFF9F5F1FF9F6D5CFF100A0A2A0402020A01000001000000000000
          0000000000010000000100000002000000040000000500000005000000060000
          00070302020E0D080827A77A69FFFEFDFCFFF7F1EBFFF7F0EBFFF8F0EAFFF7F0
          EAFFF7EFEAFFF7EFEAFFF7EDE9FFF6EFE8FFF6EFE8FFF6EDE8FFF7EDE8FFF6ED
          E6FFF4EDE6FFFBF6F3FFA06F5EFF110B0A2D0503030B01000001000000000000
          000000000001000000040000000A000000100000001400000015000000150100
          00160302021D0C080734A97A6AFFFEFDFCFFF8F1ECFFF7F1ECFFF7F0EBFFF8F1
          EAFFF8F0EAFFF7F0EAFFF7F0E9FFF7F0E9FFF6EFE9FFF6EDE9FFF7EDE9FFF7EF
          E9FFF6EDE8FFFBF7F4FFA27061FF110B0A2D0403030B01000001000000000000
          000000000002000000090E1F32592B62A1EA2E6AB1FF2E6AB0FF2D69AFFF2D67
          ADFF2E66ABFF6E8CB6FFAC7D6EFFFEFDFDFFF8F2EDFFF8F1EDFFF8F1ECFFF8F1
          ECFFF8F1EBFFF8F0EBFFF8F0EAFFF7EFEAFFF7F0EAFFF7EFEAFFF7F0E9FFF7EF
          E9FFF6EDE9FFFBF7F5FFA37363FF100A0A2B0403030B01000001000000000000
          0000000000040000000F2D65A1E869AFDEFF66BAEBFF65B9ECFF62B8EBFF61B7
          EAFF60B3E6FF8CBCDBFFAD7F70FFFEFDFDFFF8F2EDFFF8F2EDFFF8F2EDFFF8F1
          ECFFF8F1EDFFF7F1ECFFF8F1ECFFF7F0EBFFF7F0EAFFF7F0EBFFF7EFEAFFF7EF
          EAFFF7EFEAFFFCF8F5FFA47564FF100A092A0403030B01000001000000000000
          000000000004000000113573B6FF82CAF1FF58B2EAFF51AEE8FF4FADE8FF4DAB
          E6FF4EA8E2FF82B6DAFFAF8172FFFEFEFDFFF8F3EFFFF8F2EFFFF9F2EFFFBB91
          7FFFBB907EFFBA8E7DFFB98E7CFFB78C7AFFB78A7AFFB68978FFF8F1EBFFF7F0
          EBFFF7F0EBFFFCF8F6FFA57667FF0F0A09280403030A01000001000000000000
          000000000004000000123777B8FF88CCF1FF54B1E9FF53B0E9FF52AFE8FF51AE
          E8FF51AAE3FF84B8DCFFB08474FFFEFEFEFFF9F3F1FFF9F3F0FFF9F3EFFFF4EB
          E6FFF4EBE6FFF3EAE5FFF4EAE5FFF2E9E5FFF2E9E3FFF2E8E2FFF8F1ECFFF7F1
          ECFFF7F0EBFFFCF9F7FFA77968FF0F0909270403030A01000001000000000000
          000000000004000000113A7BBAFF8ED0F3FF58B3EAFF56B2E9FF55B1EAFF54B1
          E8FF53ACE4FF86BBDDFFB18576FFFEFEFEFFF9F4F1FFFAF4F0FFF9F4F0FFBF96
          85FFBE9684FFBD9483FFBC9281FFBB9180FFBB8F7EFFBA8E7DFFF8F1EDFFF8F1
          ECFFF8F1ECFFFCFAF8FFA87B6AFF0E0908250402020901000001000000000000
          000000000004000000103C7EBDFF93D3F3FF5BB6EAFF59B5EAFF58B4EAFF58B2
          E8FF56AFE5FF88BEDFFFB48778FFFEFEFEFFFAF6F2FFFAF6F2FFFAF4F1FFF5ED
          E8FFF4ECE7FFF4EBE8FFF4ECE7FFF4EBE6FFF4EAE6FFF3EAE5FFF9F3EFFFF8F2
          EFFFF8F2EDFFFCFAF8FFA97C6DFF0E0908240402020901000001000000000000
          0000000000040000000F3E82BFFF98D5F3FF5EB8ECFF5DB8ECFF5BB6EBFF5BB4
          E9FF59B3E7FF8ABFE0FFB58979FFFFFEFEFFFAF7F3FFFAF6F3FFF9F6F2FFC39C
          8DFFC19B8AFFC19989FFBF9886FFBE9685FFBE9484FFBD9382FFF9F3EFFFF9F3
          EFFFF8F3EFFFFDFBF9FFAD7E6FFF0D0808220402020901000001000000000000
          0000000000040000000F4285C1FF9DD9F5FF62BBECFF62BAECFF5FB9ECFF5EB8
          ECFF5DB5E8FF8CC1E2FFB78A7BFFFFFEFEFFFAF7F3FFFAF7F3FFFAF7F3FFF6EF
          EBFFF6EFEAFFF5EFEAFFF5EEE9FFF4ECE9FFF4ECE8FFF4ECE7FFFAF4F1FFF9F3
          F0FFF9F3F0FFFDFBFAFFAE8272FF0C0807210302020801000001000000000000
          0000000000030000000E4388C3FFA3DCF5FF66BEEDFF65BDEDFF64BBEDFF62BB
          ECFF61B8E9FF8FC4E3FFB78D7CFFFFFEFEFFFAF8F4FFFBF7F4FFFAF7F4FFC7A4
          93FFC6A291FFC6A090FFC39E8EFFC29C8DFFC29A8AFFC19988FFF9F6F1FFF9F4
          F1FFF9F4F1FFFDFCFAFFAF8374FF0C07071F0302020801000001000000000000
          0000000000030000000D468BC5FFA8DEF6FF6AC1EEFF69C0EEFF68BFEEFF66BD
          EDFF65BBEAFF91C7E6FFB98F7EFFFFFFFEFFFBF8F6FFFAF8F6FFFBF8F6FFF7F1
          ECFFF6F0ECFFF7F0ECFFF6F0ECFFF6F0EAFFF5EFEAFFF5EFEAFFFAF6F2FFF9F4
          F2FFFAF4F1FFFDFCFBFFB18575FF0B07061D0302020801000001000000000000
          0000000000030000000C488FC7FFADE1F7FF6EC3EFFF6CC2EEFF6BC2EEFF6AC0
          EEFF68BDEAFF94C9E7FFBA9081FFFFFFFFFFFBF9F7FFFBF9F7FFFBF8F6FFCBA9
          9AFFCAA798FFC9A696FFC9A594FFC7A392FFC7A292FFC5A08FFFFAF7F3FFFAF7
          F2FFFAF6F2FFFEFDFBFFB28677FF0B07061C0302020701000001000000000000
          0000000000030000000C4A92C9FFB1E3F7FF72C6F0FF70C5EFFF6FC4EFFF6DC3
          EEFF6CC0EBFF96CBE7FFBB9282FFFFFFFFFFFDF9F7FFFDF9F8FFFBF9F7FFFBF9
          F7FFFBF9F6FFFBF8F6FFFBF8F6FFFBF8F6FFFBF8F4FFFBF7F4FFFAF8F4FFFAF7
          F4FFFAF6F3FFFEFDFCFFB48879FF0A06061A0302020701000001000000000000
          0000000000030000000B4D95CAFFB7E6F8FF76C9F0FF74C8F1FF74C7F0FF72C6
          EFFF70C3EDFF9ACFEAFFBD9383FFFFFFFFFFFDFAF8FFFDFAF9FFFDF9F8FFFBF9
          F7FFFBF9F8FFFBF9F7FFFBF9F7FFFBF9F6FFFBF9F6FFFAF8F6FFFBF8F4FFFBF7
          F4FFFBF8F4FFFEFDFCFFB6897AFF090606190202020601000001000000000000
          0000000000030000000A4F98CCFFBBE8F9FF7ACCF2FF78CBF2FF77CAF1FF75CA
          F0FF74C6EEFF9DD1EBFFBD9384FFFFFFFFFFFEFAF9FFFDFAF9FFFDFAF8FFFDFA
          F8FFFBFAF9FFFDFAF8FFFDF9F8FFFBF9F7FFFBF9F7FFFBF9F7FFFDF9F6FFFBF8
          F6FFFBF8F4FFFEFDFDFFB78D7CFF090605180202020601000001000000000000
          0000000000020000000A529ACEFFBFE9F9FF7ECFF2FF7CCEF2FF7BCDF2FF79CD
          F1FF78CAEFFF9FD5EDFFBE9585FFFFFFFFFFFEFBFAFFFDFBF9FFFDFBF9FFFDFA
          F9FFFDFBF9FFFDFAF8FFFDFAF8FFFDFAF8FFFDFAF8FFFBF9F8FFFDF9F7FFFBF8
          F7FFFBF8F6FFFEFEFDFFB98F7DFF080505150201010501000001000000000000
          00000000000200000009539DD0FFC4EDFAFF83D2F3FF81D1F3FF7FCFF4FF7DCF
          F3FF7CCDF0FFA3D7EFFFBF9687FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFEFFFFFE
          FEFFFFFEFEFFFFFEFEFFBA907FFF070404120201010401000001000000000000
          0000000000020000000856A0D1FFC7EEFBFF86D5F4FF85D4F4FF84D2F4FF82D2
          F4FF80D0F3FFA6DAF3FFB8A6A1FFC09688FFBF9687FFBF9686FFBF9687FFBE96
          86FFBE9586FFBE9485FFBE9584FFBD9484FFBD9484FFBD9384FFBD9384FFBD92
          83FFBD9282FFBD9282FF8D6D62C40403030C0101010300000000000000000000
          0000000000020000000858A2D3FFCBEFFBFF8AD8F6FF89D8F5FF88D6F5FF86D5
          F4FF84D3F3FFAADFF5FFA2D5EBFF9DCBE4FF9CCBE3FF9DCAE2FF9BCBE3FF9BCC
          E3FF9BCBE4FF9ACCE5FF9BCDE6FF9BCEE8FF9CCEE8FFC1E0EEFF82ACCFFF0604
          0419060404130603030F0402020B020101050000000100000000000000000000
          000000000002000000075AA4D4FFCFF1FCFF8EDBF6FF8CDBF6FF8BD8F4FF8AD5
          F3FF87D2F0FF79BFDDFF6CADCDFF6BABCBFF6BA9CAFF6AAACBFF6BACCCFF6BAC
          CEFF6CB0D2FF6EB5D6FF70B8DBFF72BEE3FF72C0E7FFADDCF1FF4D90C5FF0201
          010F010101060101010301010102000000010000000000000000000000000000
          000000000002000000065BA7D5FFD1F3FCFF92DDF7FF90DCF6FF8ED8F2FF9AD7
          ECFF8EBCD0FF7EA0B5FF7A98ACFF7D95A7FF7C94A4FF7C93A4FF7B96A8FF7F9D
          AEFF7DA2B7FF76A8C2FF6CADCBFF70B5D6FF73BFE2FFAFDDEFFF4F94C9FF0000
          0009000000020000000000000000000000000000000000000000000000000000
          000000000001000000065DA9D7FFD5F4FCFF96E1F8FF9AE0F5FFB5DDECFF99B4
          C1FF7D7D83FF7E6963FF806458FF845C4FFF84594BFF7F584BFF7A5A50FF765E
          59FF777274FF8394A1FF87A5B5FF74A9C4FF73B6D4FFB3DCEEFF5398CAFF0000
          0008000000020000000000000000000000000000000000000000000000000000
          0000000000010000000560ACD8FFD8F6FDFFACEAFAFFBEE6F3FF9AACB5FF8F71
          68FFAF8D7DFFC3A898FFCCB3A2FFD4BDACFFD7C0AFFFCFB6A5FFC3A794FFB597
          85FF9B7666FF7D5D51FF7F888EFF8FA9B8FF85BBD1FFB8DEECFF559ACBFF0000
          0007000000020000000000000000000000000000000000000000000000000000
          00000000000100000003589CC4E8BBE4F3FFD8F6FDFFDFEDF0FF9D7C6FFFE6DA
          D3FFF4EFEAFFF4EDE7FFF2EBE4FFE9DDD3FFDDC9BAFFDBC5B6FFE3D1C4FFE8DA
          D0FFE5D6CAFFD1BCAFFF825D51FFB2BCBFFFB4D1DBFFA2CEE2FF4F8EB9E80000
          0005000000010000000000000000000000000000000000000000000000000000
          00000000000000000002182B35424E88AACA62ADD8FFA9CDE3FFAC8F83FFB087
          78FFAF8877FFAD8576FFAD8475FFEDE1D9FFE4D5C8FFE0D0C2FF9E7564FF9E75
          62FF9D7462FF9B7361FF997669FF9CB7CAFF5899C5FF477DA1CD162833450000
          0003000000010000000000000000000000000000000000000000000000000000
          00000000000000000001000000020000000300000003000000060000000B0000
          000E0000000E0000001281645AC2E2D1C9FFF3EDE7FFDCCBC0FF795B4FC90000
          0017000000130000001300000011000000080000000500000004000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          00010000000100000002130F0D227E6358B7AF897CFA75584DBA120D0C230000
          0002000000010000000100000001000000010000000100000001000000010000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000030000000400000005000000050000
          0003000000020000000100000000000000000000000000000000000000000000
          0000000000000000000100000002000000030000000400000005000000050000
          0003000000020000000100000000000000000000000000000000000000000000
          00000000000100000003000000070000000D0000001200000014000000130000
          000F000000080000000300000001000000000000000000000000000000000000
          00000000000100000003000000070000000D0000001200000014000000130000
          000F000000080000000300000001000000000000000000000000000000000000
          000000000002000000080F0802303A1D088A5F2E0BCE77390FF9602D0BCF3C1C
          078D100702330000000B00000003000000010000000000000000000000000000
          000000000002000000080F0702303A1C088A5E2C0CCE76380FF95F2D0BCF3B1B
          078D0F0702330000000B00000003000000010000000000000000000000000000
          000100000005110A042D5C3917B0B47B42FFDD984BFFF1B773FFDEA56AFFB274
          3CFF53270AB21007023200000008000000020000000000000000000000000000
          0001000000051009032D583415B0AB6C35FFD08B49FFE39B55FFCF8847FFAA68
          31FF52260BB21007023200000008000000020000000000000000000000000000
          000200000008482C1486CE9C6AFFECB16AFFDE9C56FFD28E4EFFDD9C55FFEBA7
          5AFFB2723DFF411F098C0000000D000000030000000100000000000000000000
          00020000000842271186BC8756FFDE9B57FFCE9053FFC0834BFFCD8F52FFDD99
          55FFAA6C3CFF3F1E0A8B0000000D000000030000000100000000000000000000
          00020000000A81542CCBE8C18FFFD29353FF6945229D170F08396B45239FD293
          52FFDB9D5AFF713C17CE00000011000000040000000100000000000000000000
          00020000000A734927CBD7985AFFC1854DFF5E3F229D150E08395F40239FC286
          4CFFD08C4FFF6B3616CE00000010000000040000000100000000000000000000
          000200000009B27A45F9FAE3C2FFAB6830FF150D063500000015150D0638AF6A
          30FFF7CB93FF995826F900000013000000060000000200000001000000010000
          0003000000099E6A3EF9EFBB85FF9B5E30FF120C063500000015130C06389E61
          32FFEDB880FF8D4E24F900000010000000040000000100000000000000000000
          000200000007966B42CAEED6BAFFC18A59FF623C1B9A150D0634623C1C9CC487
          4FFFEAC9A3FF845228D20000001A0000000D0000000800000008000000060000
          00060000000B865E3ACEE5C19BFFB77643FF55351C9A130C063456361D9CB877
          44FFDFB790FF774725CC0000000D000000040000000000000000000000000000
          00010000000462482F81DDBA95FFEBD5BEFFC08A5FFFA45B2BFFC18757FFEDD1
          B1FFE2BE9DFF764B2FCF0000002E000000270000002B0000002D0000002A0000
          002300000023745038CEE2BF9AFFE4BE9BFFB87B4CFF9A572AFFB87C4CFFE4BE
          99FFC49974FF4F321D8500000009000000020000000000000000000000000000
          0000000000021A140D25856543AADFBD99FFF0DDC9FFFCF4EAFFF2E1CFFFECD1
          B3FFEBD2B8FFB9A093FF875849FF875648FF865446FF845244FF815042FF6D41
          35FF6C3F34FFA48271FFE2C29FFFE3C29EFFEBD4BAFFFCEEDAFFEAD2B8FFCDA7
          84FF714D2FAC150E082A00000004000000010000000000000000000000000000
          000000000001000000021B140E24654D3580A27B53C9CA9767F9A0784FCE8B65
          48CACBB1A4FFFAF9F9FFE9D7C9FFEAD8CCFFEAD8CCFFEAD8CCFFE7D8D3FF8E67
          5CFFB59D93FFCBB6ABFFA88677FF77553CCA926E4ACCB68459F88E6744CA583F
          298217100A280000000500000001000000000000000000000000000000000000
          00000000000000000001000000020000000300000005000000070000000D0000
          001950383093CFB7AFFFFFFEFEFFEBD9D0FFBE9F92FF95685BFFBD9E92FFC6B2
          ACFF97766BFF95756AFF38211B98000000130000000600000005000000050000
          0005000000030000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002000000030000
          000800000018593F36AAD1B8B1FFFFFEFEFFB18D81FFE1D0C8FFB08D81FFEEDE
          D6FF936E63FF3E251EAF00000019000000050000000200000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0003000000125F4035D0B89D92FFDECAC3FFE1D3CEFFC5A9A0FFD9C5BCFFEFE2
          D8FFDCCEC9FF58352BCF00000017000000070000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000532221D6BAA8D82FFE0D9D3FFC7B1A8FFDECBC3FFFFFFFEFFF2E4DEFFF2E6
          DEFFF2E4DCFFA68981FF2919156E0000000D0000000400000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020B08
          061F8C675BEBE6DBD4FFE9E2DCFFE1D9D4FFCEBBB3FFD8C2BBFFFFFFFFFFF5E8
          E0FFF3E9E2FFEEE2DEFF775045EE080504260000000800000002000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000075A41
          38A0CCB5ABFFF0E8E2FFEDE6E0FFF6F4F3FFB8A49DFF704F44CBD2BBB2FFFFFF
          FFFFF5EDE6FFF5EDE6FFC8B5AFFF462B24A20000000E00000005000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000032018153FAF8B
          7FFDF0E8E1FFF1E8E3FFFCFAF8FFC6B1A9FF4530288D00000016523B338BD2BB
          B2FFFFFFFFFFF8F0EAFFF7EFEAFF947065FD180F0C4500000009000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000010202010B87675AD5E3D5
          CEFFF1E9E5FFFCFAF9FFCAB6AEFF4B352D8A00000009000000050000000C533C
          3489D2BBB3FFFFFFFFFFF9F3EEFFE5DBD7FF674136D501010112000000050000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000347373072C9ADA4FFF2EB
          E6FFFCFAF9FFCFBAB3FF4E383289000000070000000100000001000000040000
          000B533C3488D3BCB3FFFFFFFFFFF9F5F3FFB49990FF33211B75000000090000
          0003000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000001130F0D22AE8D80F1EFE6DFFFFCFB
          F9FFD3BEB7FF513D368700000006000000010000000000000000000000010000
          000400000009533D3487D3BCB4FFFFFFFFFFF4EEEBFF855F52F10D0807280000
          0005000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000004775E54AADECCC3FFFDFBFAFFD7C2
          BBFF55413A860000000600000001000000000000000000000000000000000000
          00010000000300000008543D3686D3BDB4FFFFFFFFFFD3C3BDFF52362CAC0000
          0009000000030000000100000000000000000000000000000000000000000000
          00000000000000000000000000012D232042C9AA9EFFFDFBFAFFDAC6BFFF5944
          3E85000000050000000100000000000000000000000000000000000000000000
          0000000000010000000300000007543E3585D4BDB5FFFFFFFFFF9F7F73FF1E13
          1046000000050000000100000000000000000000000000000000000000000000
          000000000000000000000504030AA7887CDEF4EDEAFFDCC9C2FF5B4841840000
          0004000000010000000000000000000000000000000000000000000000000000
          000000000000000000010000000200000006543E3684D4BDB5FFEEE8E6FF7350
          42DE0302020E0000000300000001000000000000000000000000000000000000
          000000000000000000015C4B457BDECAC1FFDFCCC5FF5D4B4483000000030000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000010000000200000005553E3683D5BEB6FFBFA8
          A0FF3F2A227D0000000500000001000000000000000000000000000000000000
          0000000000001A151324C5A79DF5E1CFC8FF604E478200000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000200000004553F3782D5BE
          B6FF926E63F5130C0A2900000002000000010000000000000000000000000000
          0000000000018D746AB5E3D1CAFF625049810000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000035540
          3881D5BFB7FF604137B600000003000000010000000000000000000000000000
          000000000001CDAC9EFF63514B80000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          000256403880AD8071FF00000002000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000010000000200000004000000060000000600000006000000060000
          0007000000070000000700000007000000070000000700000007000000070000
          0007000000070000000700000007000000070000000700000004000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000002000000070000000E000000150000001800000019000000190000
          0019000000190000001A0000001A0000001A0000001A0000001B0000001B0000
          001B0000001B0000001C0000001C0000001B0000001900000011000000080000
          0002000000000000000000000000000000000000000000000000000000000000
          0001000000030000000E78564BC0A77868FFA77867FFA77666FFA67766FFA675
          65FFA67666FFA57565FFA47464FFA47464FFA47363FFA37363FFA37262FFA371
          61FFA27161FFA17061FFA17160FFA1705FFF9F705FFF725044C2000000110000
          0004000000010000000000000000000000000000000000000000000000000000
          00010000000500000013AA7A6CFFFCF9F7FFFCF8F6FFFBF8F6FFFBF8F5FFFBF7
          F5FFFBF7F4FFFAF7F3FFFAF6F3FFFAF6F3FFFAF6F2FFFAF5F2FFF9F4F1FFF9F4
          F0FFF9F3F0FFF8F3EFFFF8F3EFFFF8F2EEFFF8F2EEFFA17060FF000000180000
          0006000000010000000000000000000000000000000000000000000000000000
          00010000000500000014AB7D6DFFFCF9F8FFF7EDE9FFF6EDE8FFF6EDE6FFF6ED
          E6FFF6ECE6FFF6ECE5FFF6ECE5FFF6ECE4FFF4EBE4FFF4EBE4FFF4EBE4FFF4EA
          E3FFF4EAE3FFF4EAE3FFF4EAE3FFF3E9E3FFF8F2EEFFA37262FF0000001A0000
          0008000000020000000100000001000000000000000000000000000000000000
          00000000000500000014AB7F6FFFFDFAF8FFF7EFE9FFF7EFE9FFF7EFE8FFF6ED
          E8FFF6EDE8FFF6EDE6FFF6EDE6FFF4ECE6FFF6ECE6FFF4ECE5FFF4EBE5FFF4EB
          E4FFF4EBE4FFF4EBE4FFF4EBE4FFF4EAE3FFF9F3EFFFA37363FF0000001F0000
          000D000000080000000700000004000000020000000100000000000000000000
          00000000000500000013AD8071FFFDFBF9FFF7F0E9FFF6EFE9FFF7EFE9FFF7EF
          E8FFF7EFE9FFF6EDE8FFF7EDE8FFF7EDE6FFF6EDE6FFF6EDE6FFF6ECE5FFF4EC
          E5FFF6ECE5FFF4EBE5FFF4EBE4FFF4EBE4FFF9F4F0FFA57565FF010101310101
          01210101011C0101011901010111000000080000000200000000000000000000
          00000000000500000012AF8273FFFDFBFAFFF8F0EBFFF7F0EBFFF7F0EAFFF7F0
          EAFFF7F0E9FFF7EFE9FFF6EFE9FFF7EDE8FFF6EDE8FFF6EDE8FFF6EDE8FFF6EC
          E6FFF6EDE6FFF4ECE5FFF6EBE5FFF4EBE4FFF9F5F1FFA67667FFB89D94FFAD84
          74FFB08576FFB08676FF7F6055C2010101110000000400000001000000000000
          00000000000400000011B08375FFFDFCFAFFF8F1EBFFF7F1EBFFF7F1EBFFF7F0
          EBFFF8F0EAFFF7F0EAFFF7EFEAFFF7EFEAFFF7EDE9FFF6EFE8FFF6EFE8FFF6ED
          E8FFF7EDE8FFF6EDE6FFF4EDE6FFF6EDE6FFFAF5F2FFA87969FFE4E0DDFFF3EF
          ECFFF8F4F0FFFAF5F1FFB18677FF010101180000000600000001000000000000
          00000000000400000010B18677FFFDFCFAFFF8F2ECFFF8F1ECFFF8F1ECFFF7F1
          ECFFF7F0EBFFF8F1EAFFF8F0EAFFF7F0EAFFF7F0E9FFF7F0E9FFF6EFE9FFF6ED
          E9FFF7EDE9FFF7EFE9FFF6EDE8FFF7EDE8FFFBF6F4FFAA7A6AFFE2DAD5FFF1E9
          E3FFF4ECE7FFF9F5F2FFB38879FF010101190000000700000001000000000000
          0000000000040000000FB48878FFFDFCFBFFF8F2EDFFF8F2EDFFF8F2EDFFF8F1
          EDFFF8F1ECFFF8F1ECFFF8F1EBFFF8F0EBFFF8F0EAFFF7EFEAFFF7F0EAFFF7EF
          EAFFF7F0E9FFF7EFE9FFF6EDE9FFF7EDE8FFFBF7F4FFAA7D6DFFE2DBD6FFF1EA
          E4FFF5EDE7FFFAF5F3FFB3897AFF010101190000000600000001000000000000
          0000000000040000000FB5897AFFFEFDFCFFF9F3EFFFF8F2EFFFF8F2EDFFF8F2
          EDFFF8F2EDFFF8F1ECFFF8F1EDFFF7F1ECFFF8F1ECFFF7F0EBFFF7F0EAFFF7F0
          EBFFF7EFEAFFF7EFEAFFF7EFEAFFF7EFE9FFFBF8F5FFAC7F6FFFE3DCD8FFF1EA
          E4FFF5EEE8FFFAF5F4FFB58B7BFF010101180000000600000001000000000000
          0000000000030000000EB68D7EFFFEFDFCFFF9F3F1FFF9F3F0FFF8F3EFFFF8F2
          EFFFF9F2EFFFF8F3EFFFF8F2EFFFF8F1EDFFF8F1ECFFF8F1ECFFF8F1ECFFF8F1
          EBFFF8F1EBFFF7F0EBFFF7F0EBFFF7EFEAFFFCF8F6FFAD8071FFE4DED9FFF2EA
          E5FFF6EFE9FFFAF6F4FFB68C7DFF010101170000000600000001000000000000
          0000000000030000000DB88F80FFFEFDFDFFFAF4F1FFF9F4F1FFF9F3F1FFF9F3
          F0FFF9F3EFFFF9F3EFFFF9F3EFFFF8F2EFFFF9F2EFFFF8F2EFFFF8F2EDFFF8F1
          ECFFF8F1ECFFF7F1ECFFF7F0EBFFF7F0EBFFFAF6F3FFAE8373FFE6E0DAFFF2ED
          E7FFF7F1EBFFFBF7F5FFB88F7FFF010101160000000600000001000000000000
          0000000000030000000CB99081FFFEFEFDFFF9F6F2FFF9F4F1FFF9F4F1FFFAF4
          F0FFF9F4F0FFF9F4F0FFF9F3F0FFF9F3EFFFF9F3EFFFF9F2EFFFF9F3EDFFF9F2
          EDFFF8F1EDFFF8F1ECFFF8F1ECFFF6EFE9FFF8F4F2FFB08374FFE7E2DDFFF3ED
          E8FFF8F1ECFFFBF8F5FFB89080FF010101150000000500000001000000000000
          0000000000030000000BBC9283FFFEFEFDFFFAF6F2FFFAF7F2FFFAF6F2FFFAF6
          F2FFFAF4F1FFF9F4F1FFF9F4F0FFF9F3F1FFF9F4F0FFF9F3F0FFF9F3F0FFF9F3
          EFFFF9F3EFFFF8F2EFFFF6F0EAFFF5EDE7FFF6F1EEFFB38576FFE8E3DEFFF3ED
          E9FFF8F1ECFFFBF8F6FFB89183FF010101150000000500000001000000000000
          0000000000020000000ABD9384FFFEFEFEFFFAF7F3FFFAF7F3FFFAF7F3FFFAF6
          F3FFF9F6F2FFFAF6F2FFFAF6F2FFFAF6F1FFF9F4F1FFF8F3F0FFF9F4F1FFF8F3
          EFFFF8F2EEFFF6F0EBFFF4EDE8FFF2E9E5FFF3ECE9FFB38978FFE9E4E0FFF4EE
          EAFFF8F2EDFFFBF9F7FFBA9385FF010101140000000500000001000000000000
          00000000000200000009BE9686FFFFFEFEFFFAF7F6FFFAF7F4FFFAF7F3FFFAF7
          F3FFFAF7F3FFFAF6F3FFFAF6F2FFF9F6F3FFFAF6F2FFF8F2EFFFF6EFEBFFF5ED
          E9FFF3EAE6FFF0E5E2FFEEE2DDFFEBDED9FFECE1DDFFB5897AFFEAE6E2FFF4EF
          EBFFF8F2EEFFFCF9F7FFBB9487FF010101130000000500000000000000000000
          00000000000200000008BF9787FFFFFEFEFFFBF8F6FFFBF8F6FFFAF8F4FFFBF7
          F4FFFAF7F4FFFBF7F4FFFAF7F4FFF9F5F2FFF9F5F1FFF5EEE9FFEEE2DCFFE6D8
          D0FFE1D2CAFFE0CEC7FFDECAC2FFDBC7BEFFDCC8C2FFB78C7DFFEBE8E4FFF5F0
          ECFFF8F3EFFFFAF7F5FFBC9789FF010101120000000400000000000000000000
          00000000000200000008C09989FFFFFEFEFFFBF9F6FFFDF9F6FFFBF8F6FFFAF8
          F6FFFBF8F6FFFBF8F4FFFAF7F4FFFBF7F4FFF8F2EFFFEFE6DFFFB38B7CFFA577
          66FFA47564FFA47464FFA47363FFA37363FFCEB3AAFFB88F7EFFEFEAE7FFF6F1
          EDFFF7F2EDFFF9F5F4FFBE978AFF010101110000000400000000000000000000
          00000000000200000007C19A8BFFFFFFFEFFFDF9F7FFFBFAF7FFFBF9F7FFFBF9
          F7FFFBF8F6FFFBF8F6FFFBF8F6FFFAF8F4FFF7F2EFFFECDFDAFFAB7E6DFFFFFF
          FFFFFFFEFEFFFFFDFCFFFEFCFAFFFCF9F7FFCAAFA6FFC2A9A0FFF3EFECFFF5F1
          ECFFF6F0EBFFF7F3F0FFC0998CFF010101100000000400000000000000000000
          00000000000100000006C39B8CFFFFFFFFFFFDF9F8FFFDFAF8FFFDF9F7FFFDF9
          F8FFFBF9F7FFFBF9F7FFFBF9F6FFFBF8F6FFF6F1EDFFEBDFDBFFB08574FFFFFE
          FEFFFEFBFAFFFDF9F7FFFCF6F3FFCEB2A8FFC5ACA3FFF1EDE9FFF4F0EBFFF4EF
          EAFFF4EBE7FFF4EEEBFFC09D8EFF0101010F0000000400000000000000000000
          00000000000100000005C39D8EFFFFFFFFFFFDFBF9FFFDFAF9FFFDFAF8FFFDFA
          F9FFFDF9F8FFFBF9F7FFFBF9F8FFF9F6F4FFF6F0ECFFECE1DBFFB68C7DFFFFFE
          FEFFFDF9F6FFFBF6F3FFD1B5ACFFC8AFA5FFEEE8E5FFF2EAE4FFF1E7E3FFF0E5
          E1FFEDE1DDFFEDE2DFFFC29D90FF0101010E0000000400000000000000000000
          00000000000100000005C49E8FFFFFFFFFFFFDFBF9FFFDFBF9FFFEFAF9FFFDFA
          F9FFFDFAF8FFFDFAF8FFF9F7F6FFF9F4F2FFF5EDEBFFEBE1DDFFBC9584FFFFFE
          FEFFFBF6F3FFD4BAAFFFCBB2A7FFE9DEDAFFE5D8D2FFE2D4CCFFE2D1CBFFE1CE
          C7FFDECBC2FFDECCC5FFC49F91FF0101010D0000000300000000000000000000
          00000000000100000004C59F90FFFFFFFFFFFDFDFBFFFEFBFAFFFEFBFAFFFDFB
          F9FFFDFBF9FFFBF7F6FFF9F5F3FFF7F1EEFFF3EBE7FFEDE1DCFFC19B8BFFFFFE
          FEFFD6BCB2FFD1B8B0FFEBE3DDFFBA9689FFAF8476FFAF8474FFAF8374FFAF82
          73FFAE8273FFD1B8AFFFC49F91FF0000000A0000000300000000000000000000
          00000000000100000003C6A191FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFBF9F9FFF9F6F4FFF6F1F0FFF2ECE9FFEEE3E0FFE3D2CBFFDBC5
          BDFFD4BFB5FFF3EFECFFECDFDCFFB48B7CFFFFFFFFFFFFFEFEFFFFFDFCFFFEFC
          FAFFFCF9F7FFD3BAB2FF55403886000000060000000200000000000000000000
          0000000000000000000293776CBEC6A191FFC59F91FFC69F92FFC59F91FFC59F
          90FFC59F91FFC49F90FFC49E90FFC49D8FFFC49E8EFFC39D8EFFC39D8EFFD8C3
          BAFFF8F5F3FFF5F1EDFFECE0DCFFB99283FFFFFEFEFFFEFBFAFFFDF9F7FFFCF6
          F4FFD6BDB4FF57423B8600000008000000030000000100000000000000000000
          000000000000000000010000000100000002000000030000000400000008DAC6
          BDFFFCFCFCFFFAF9F7FFF9F7F6FFF9F7F5FFF9F7F6FFF9F6F5FFF8F6F4FFF9F7
          F6FFF9F6F4FFF6F2EEFFEEE3DEFFBF998BFFFFFEFEFFFDF9F6FFFBF6F4FFD8BF
          B8FF5A463F860000000800000003000000010000000000000000000000000000
          000000000000000000000000000000000000000000010000000200000006CDAE
          A1FFFEFEFEFFFCFBF9FFFCFBF9FFFDFAF9FFFCFAF9FFFCFAF8FFFCFAF8FFF9F7
          F6FFF9F5F4FFF5EFEDFFEDE3DFFFC4A191FFFFFEFEFFFBF6F4FFDBC4BAFF5D49
          4285000000070000000300000001000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000100000004CFB0
          A3FFFFFFFFFFFDFDFCFFFEFCFBFFFEFCFBFFFDFCFAFFFDFCFAFFFBF8F7FFF9F6
          F5FFF7F3F0FFF4EDE9FFEEE3DFFFC9A698FFFFFEFEFFDDC6BDFF5F4C45840000
          0006000000020000000100000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000100000003D0B1
          A4FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBF9F9FFF9F6
          F5FFF6F2F1FFF3EDEAFFEFE4E1FFE5D4CFFFE1CEC7FF604F4883000000050000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000029A83
          7ABED0B1A4FFCFB0A4FFD0B0A4FFCFB0A4FFCFB0A3FFCFB0A4FFCEB0A3FFCEAF
          A3FFCEAEA2FFCEAFA1FFCDAEA1FFCDAEA1FF63524A8200000004000000020000
          0001000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          0004000000030000000100000001000000030000000A00000011000000130000
          000D000000050000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002000000080907
          06230000000A0000000500000003000000080202021C4E3B35DE45342FC81A14
          125E0000000C0000000300000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000040000000F523F
          39DC0A0807310000000E0000000800000010221917738D7B73FFB2A095FF5943
          3DF5000000130000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000010000000600000015634C
          45FF62514AE20A080736000000140000001B4A3832CFC2B3ACFFA59188FF3E2F
          2AB4000000120000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000010000000600000017654E
          46FFD2C7C1FF614E48E30B08073E120D0C507A655DFFD5C6BEFF7B665EFF1712
          10560000000D0000000300000001000000010000000000000000000000000000
          000000000000452D24FF442C23FF432C23FF432C23FF432B23FF000000000000
          0000412A22FF402A22FF402A22FF402A21FF402921FF0000000600000017674F
          48FFE7DED9FFC0AFA4FF63504AE640312CB6AFA098FFBEACA2FF523F39DB0000
          001C0000000D0000000700000005000000030000000100000001000000000000
          000000000000594137FF584037FF573F36FF573F36FF442C23FF000000000000
          0000553E34FF553D33FF543D33FF543C33FF402A22FF00000005000000166951
          4AFFE9E0DBFFC8B5A8FFC3B1A8FF766159FECEBDB3FF927E76FF2A201D880000
          00240000001A00000016000000120000000A0000000400000001000000000000
          000000000000594238FF5A4138FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000005000000156B53
          4CFFEBE4DFFFC6B3A7FFC8B6AAFFCBBBB2FFCEC0B6FF6C564EFF665048FF664F
          48FF654F47FF664E47FF654D46FF2E24207E0000000600000001000000000000
          0000000000005B4339FF472F27FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000005000000146D56
          4EFFF0EAE5FFCDBDB1FFC8B4A8FFC8B5AAFFC8B5A8FFD5C6BDFFD5C7BDFFD5C7
          BDFFD5C5BCFF9E8D86FF2F2421850000000E0000000400000001000000000000
          0000000000005C443AFF483027FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000005000000136F57
          50FFF2EDE9FFD9CBC0FFD1C0B4FFC9B6A9FFC8B5A9FFC7B4A9FFC8B4A8FFCFBF
          B5FFA18F87FF3026238600000011000000060000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000400000012705A
          52FFF3EFEBFFDDD0C4FFDBCEC2FFD1C1B6FFC9B6ABFFCAB6AAFFD2C2B8FFA391
          8AFF312723860000001000000006000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000400000011735C
          54FFF4F1EDFFDFD3C7FFDED1C7FFDBCEC2FFD0BFB2FFD3C3BAFFA6958EFF3429
          25870000000F0000000600000002000000000000000000000000000000000000
          0000000000004C332AFF4C332AFF000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000400000010745E
          56FFF6F2EEFFE2D6CBFFE0D5CAFFE0D5CAFFDFD3CBFFA89891FF352A26860000
          000E000000063F2A21FF412A22FF000000000000000000000000000000000000
          00000000000061493FFF4D342AFF000000000000000000000000B08375FFAF83
          75FFAF8374FFAE8273FFAE8273FFAD8273FFAD8173FFAA7E70FF0000000F775F
          58FFF7F3F0FFE4D9CEFFE3D8CDFFE8E0D7FFB0A099FF362C28850000000D0000
          000500000002563E35FF432B23FF000000000000000000000000000000000000
          000000000000634A40FF4F352BFF000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000030000000D7962
          59FFF7F5F1FFE6DCD1FFEBE3DAFFB2A39CFF372D29840000000C000000050000
          000200000000563F35FF432C23FF000000000000000000000000000000000000
          000000000000644B41FF50352DFF000000000000000000000000B3887AFFB287
          7AFFB18779FFB18678FFB18678FFB18678FFB08577FFAE8375FF0000000D7A63
          5BFFF9F5F3FFEDE6DDFFB3A59EFF392E2A830000000B00000004000000010000
          000000000000584037FF442D24FF000000000000000000000000000000000000
          000000000000664C42FF51372DFF000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000030000000B7C65
          5CFFF9F7F4FFB5A7A0FF3A2F2B820000000A0000000400000001000000000000
          0000000000005A4137FF462E25FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000B68D7EFFB58C
          7EFFB48B7EFFB48B7CFFB48B7CFFB48B7CFFB38A7CFFB1877AFF0000000A7E66
          5DFFB8AAA4FF3B302C81000000095C463F89AF8477FFB08577FF000000000000
          0000000000000000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002000000077F68
          60FF3B302C7F0000000800000003000000010000000000000000000000000000
          0000000000000000000100000000000000000000000000000000000000000000
          000000000000553A30FF553A30FF000000000000000000000000B89183FFB890
          83FFB89082FFB89082FFB89082FFB78E81FFB78E80FFB68D7FFF000000043C32
          2E7C000000055E494188B38A7DFFB38A7CFFB48A7CFFB38A7BFF000000000000
          000000000000493028FF493028FF000000000000000000000000000000000000
          0000000000006B5147FF563C31FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0002000000020000000100000000000000000000000000000000000000000000
          0000000000005F463BFF4A3228FF000000000000000000000000000000000000
          0000000000006D5247FF573C32FF000000000000000000000000BB9588FFBC95
          87FFBB9586FFBB9486FFBB9486FFBA9385FFB99385FFB99285FFB99284FFB891
          84FFB89183FFB89082FFB79082FFB78F81FFB78E80FFB68E80FF000000000000
          00000000000060473DFF4C3229FF000000000000000000000000000000000000
          0000000000006E5348FF583D33FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000061483FFF4D332AFF000000000000000000000000000000000000
          0000000000006F544AFF5A3E33FF000000000000000000000000BF998CFFBE99
          8CFFBE988BFFBD988AFFBD988AFFBD978AFFBD9789FFBC9689FFBC9689FFBB95
          88FFBB9587FFBB9587FFBA9486FFBA9386FFBA9285FFB99384FF000000000000
          00000000000063493FFF4E352BFF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000C19D90FFC19D
          8FFFC09C8FFFC09C8FFFC09C8FFFC09C8EFFBF9B8EFFBF9A8DFFBE9A8CFFBF9A
          8CFFBE998BFFBE988BFFBE978AFFBD978AFFBC9789FFBC9688FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000005E4136FF5D4136FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000053372DFF52372DFF000000000000000000000000000000000000
          00000000000074584DFF5F4136FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000684E44FF53382EFF000000000000000000000000000000000000
          00000000000074594EFF5F4137FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000006A5045FF543A2FFF000000000000000000000000000000000000
          000000000000765A4FFF604337FF604237FF5F4237FF5F4237FF000000000000
          00005E4136FF5D4136FF5D4036FF5C4035FF5C4035FF00000000000000005B3F
          34FF5B3E33FF5A3E34FF5A3D32FF593D33FF0000000000000000573C32FF573C
          32FF6C5146FF6A5046FF563B31FF000000000000000000000000000000000000
          000000000000765A50FF765A4FFF755A4FFF755A4FFF604238FF000000000000
          000074584DFF73584DFF73574DFF73574CFF5D4036FF00000000000000007156
          4BFF71554BFF70554BFF70544AFF5A3E34FF00000000000000006E5248FF6D53
          48FF6D5247FF6C5147FF563C32FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0002000000040000000500000005000000040000000300000002000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          00070000000D0000001100000012000000110000000E00000009000000040000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000052B18
          0D526C3A1EC67D4020EA84411FFF6A3117D92A1E28B1010713460000000E0000
          0006000000020000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000001000000010000000100000001000000010000000200000006331E
          105B9C663CF0E2C296FFCE9D5DFF48606FFF073688FF072B6FFF0315348D0000
          0010000000070000000200000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0001000000030000000500000005000000050000000500000004000000060000
          000A22140B48A77148FB95CEDCFF3590DBFF1E69BFFF093E9AFF08307DFF0416
          3386000000120000000A00000008000000070000000700000007000000060000
          0005000000020000000100000000000000000000000000000000000000010000
          00060000000D00000012000000130000001400000014000000110000000C0000
          00080000000D2B272E8B2E79BAFF80D5F2FF44A5E2FF2C7FCBFF073C98FF0B34
          7AFB01060E43000000200000001C0000001A0000001A0000001A000000180000
          0012000000080000000200000001000000000000000000000000000000030000
          000A713A17FF713916FF713816FF6F3715FF6C3716FF643013FF3417099E0000
          00090000000700020315103E7CDD57BDE4FF8DDFF4FF44A5E3FF1860BAFF0B3C
          91FF422E3CFF5B2A0FFF5E2C10FF5F2C10FF5F2B10FF5F2B10FF5E2B10FF471C
          09FF0000000F0000000400000001000000000000000000000000000000030000
          000A6F411DC9935727FF975A29FF965A29FF925625FF693717DB2A15087A0000
          00080000000400000008040E1D3E2465A4F961CEF1FF94E2F4FF328BD9FF2648
          9BFF4B3C36FF5A3E30FF824B24FF8B4E23FF8E5023FF8F5123FF85471DFF431D
          0ACB0000000F0000000400000001000000000000000000000000000000010000
          00050000000F301D0D67945928FF945929FF6C3B17E40000001A0000000C0000
          0004000000020000000300000008081B3560256FB6FF3D9CDCFF93DAEBFFA67A
          6AFFCAAA90FF99795EFF604638FF834C24FF8C4F23FF7B401AFF241107720000
          0015000000080000000200000001000000000000000000000000000000000000
          000200000007000000127E4B22DD965A29FF6B3515F701010018000000070000
          00020000000000000001000000030000000807183053343455C7644D45FEF9F9
          F8FFB08777FFD6BAA1FFA68466FF654A3CFF7D4624FF4F250ED90000001B0000
          000A000000020000000100000000000000000000000000000000000000000000
          0000000000030000000A472B1388965C2AFF773D19FF13090442000000080000
          000100000000000000000000000100000002000000060000001158443CCED5C6
          BDFFFFFFFFFFB08777FFDBC3ACFF514D8DFF180F62FF220F0E91000000160000
          0006000000020000000000000000000000000000000000000000000000000000
          00000000000100000007160D0637975E2AFF81491FFF31190A860000000B0000
          000300000001000000000000000000000000000000030000000C110C0A3E8769
          5BFEE2D3C9FFFFFFFFFF514A8EFF4A5DDDFF1D29B9FF0B0762E2010009280000
          0008000000020000000100000000000000000000000000000000000000000000
          000000000001000000040000000D7C4E23D7905626FF552C11CA000000100000
          000700000005000000040000000400000005000000060000000F28160A648D55
          2CFF927160FF9B9CD3FFA3B7F0FF4042B9FF5165DFFF202CBDFF0A0965DC0101
          0921000000060000000200000001000000000000000000000000000000000000
          0000000000000000000200000009472D1484985F2CFF77401AFC080401290000
          0012000000100000001000000011000000110000001300000019593416B2955A
          29FF8D5731FF413497FF5B6BDBFFA8BAF1FF4346B9FF5568DFFF2330BEFF0E0D
          69DC01010B220000000500000002000000010000000000000000000000000000
          0000000000000000000100000005150D06339C632EFF925927FF81481FFF7F46
          1EFF7D451EFF7C441CFF7B431BFF79421BFF79411BFF854A20FF955A2AFF9960
          2CFF985E2BFF8B5636FF443799FF6272DDFFADBFF2FF474ABAFF5669DFFF2836
          C0FF12106DDC02010B2100000005000000020000000000000000000000000000
          00000000000000000001000000030000000C7D5025D1A06830FFA16831FF9C62
          2DFF99612CFF9A602CFF99602BFF985F2BFF985E2BFF9B612DFF9D622EFF9C61
          2DFF9A612DFF995F2CFF814C31FF3B3398F4697ADFFFB0C2F3FF4B4EBBFF576A
          DFFF2D3CC4FF161470DC02020C21000000040000000200000000000000000000
          000000000000000000000000000200000008492F1683A06832FF8E5727FC110A
          0435000000120000000F0000000F0000001100000019643E1CB59C632EFF9E64
          2EFF9D632EFF9C622DFF7F461EFF180E155D373299E46F7EE0FFB5C7F3FF4E51
          BCFF5467DEFF3343C7FF191776DD02020C1F0000000400000001000000000000
          000000000000000000000000000100000005120C052AA06730FF8D5426FF321C
          0B750000000B0000000500000005000000080503011B905A2AF7A06730FF9F66
          30FF9E652FFF9A612CFF643515DA000000130706142839379CE47484E2FFB9CA
          F4FF5053BBFF5164DDFF3849CBFF1D1B7ADD03020D1F00000003000000000000
          0000000000000000000000000001000000030000000A7E5327CD98612CFF5A32
          16BA0000000A000000030000000200000008321F0E649E652FFFA16931FFA168
          32FFA06830FF905728FF341C0B800000000B00000005070614223A389EE37889
          E3FFBECEF5FF5154BBFF4C62DCFF3D4FCEFF211F7FDD03030D1D000000000000
          000000000000000000000000000000000002000000064A31177EA06A31FF804D
          20F70403011600000004000000030000000A66411DB3A26A32FFA36B33FFA26A
          33FFA26932FF824C20FC0B060228000000060000000200000002070715223C3A
          A0E37A8BE4FFC1D1F6FF4C4EB5FF475CDAFF404FCAFF1E1B6BBF000000000000
          00000000000000000000000000000000000100000004130C0629A46D34FF9059
          27FF29180A5B00000007000000050503011696632EF7A56E35FFA46D34FFA46D
          33FF9E652FFF5C3316BE0000000C000000030000000000000001000000020807
          15223D3BA3E57788E2FFC5D4F7FF3E3FA7FF4A5DD9FF403AABF6000000000000
          0000000000000000000000000000000000000000000200000009805628CA9C66
          2FFF573215AB000000090000000836231065A36D33FFA67036FFA66F36FFA56E
          35FF945C29FF2C180A6400000008000000020000000000000000000000000000
          0001080715213D3DA5E56675DBFFC7D7F7FF3434A3FF2A266E9E000000000000
          00000000000000000000000000000000000000000001000000054932177AA36E
          34FF804E22EE0201000F0000000B6C4720B4A67136FFA87137FFA77137FFA670
          35FF824E21F20403011700000004000000010000000000000000000000000000
          0000000000010808152037348FCA4542B7FF2B28709E02020408000000000000
          0000000000000000000000000000000000000000000100000003130D0627A670
          36FF945E2AFF201308480705021A9C6931F7AA7439FFAA7338FFA97338FFA16B
          31FF543215A40000000900000003000000000000000000000000000000000000
          0000000000000000000100000001000000010000000100000001000000000000
          0000000000000000000000000000000000000000000000000002000000078058
          2AC79E692FFF4C2F159039261167A87337FFAC763AFFAB763AFFAA7539FF9660
          2CFF2214084A0000000600000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000054932
          1876A66F35FF8B5D2AE1734E26B8AC763AFFAC783BFFAC773AFFA97438FF7C4E
          22DE0000000A0000000300000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000100000003140D
          0625A87437FCAB7739FFAB7739FFAF7A3CFFAE7A3CFFAD7A3BFFA36E34FF472C
          1386000000070000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          0006825A2BC6B07B3DFFB07C3EFFAF7C3DFFAF7B3DFFAF7B3CFF9D652DFF130B
          052B000000040000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          000448321872AF7A3BFFB07E3FFFB07D3EFFB17D3EFFAD773AFF734A20C40000
          0007000000020000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0002100B051EAB783AFCB27F3FFFB17E3FFFB17E3EFFA67135FF3A26116A0000
          0005000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000004835C2CC2B17E3FFFB28040FFB37F3FFF9B662FF6090603160000
          0003000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000024732186DAE7A3BFFAE793BFFAB7738FF67441FAB000000040000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0001000000010000000100000001000000010000000100000001000000000000
          0000000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000010000
          0003000000050000000500000005000000050000000400000003000000010000
          0001000000000000000000000000000000010000000200000004000000060000
          0006000000060000000700000007000000070000000700000006000000050000
          0002000000010000000000000000000000000000000000000000000000060000
          000D00000012000000130000001400000014000000110000000B000000040000
          0001000000000000000000000001000000020000000700000010000000160000
          001800000019000000190000001A0000001A0000001A00000018000000128989
          898DBFBFBFC0BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF898989890000000A7138
          16FF703816FF703816FF6E3715FF6C3515FF632F12FF3417099E000000080000
          0002000000010000000000000001000000030000000D632F11FF632E12FF612E
          11FF622D10FF612D10FF602C10FF5F2B10FF5E2B10FF5E2B10FF471C09FFBFBF
          BFC35E473FFF5E473FFF5E473FFF5E473FFF5E473FFFBFBFBFBF0000000A6E41
          1DC9925627FF955928FF955928FF905425FF673616DB2A14087A000000080000
          0002000000010000000000000001000000030000000D6A3A1ACD81491FEE9152
          24FF905224FF905224FF905224FF8F5124FF8F5123FF85481DFF421D0ACB8E8E
          8E95BFBFBFC0BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000050000
          000F301C0D67935727FF935828FF6A3916E40000001A0000000C000000040000
          00010000000000000000000000010000000200000007000000152213085D8C50
          22FF915324FF915324FF905225FF915224FF7C411AFF24120771000000150000
          00080000000261616162B6B3B3C65F483FFFB6B3B3C661616161000000020000
          0007000000127C4A21DD945828FF693513F70101001800000007000000020000
          000000000000000000000000000000000001000000040000000F30190B748F51
          24FF925425FF925425FF925425FF8D4F21FF51250ED800000019000000090000
          000200000001BFBFBFBF5F4941FF5F4840FF5F4840FFBFBFBFBF000000000000
          00030000000A462A1288955A29FF733C19FF1309044200000008000000010000
          00000000000000000000000000000000000000000005000000105E3416BF9255
          26FF935727FF935627FF925526FF7E431BFF271207800000000F000000040000
          0001000000005E5E5E5EBFBFBFBF604941FFBFBFBFBF5E5E5E5E000000000000
          000100000007160D0637965B29FF80471EFF301809860000000B000000030000
          000100000000000000000000000000000001000000070402011D864C21F89558
          28FF945827FF945827FF935527FF693514F80603012900000008000000020000
          00000000000000000000BFBFBFBF614A41FFBFBFBFBF00000000000000000000
          0001000000040000000D7B4C22D78D5425FF542C11CA00000010000000070000
          0005000000040000000400000005000000060000000D28160A61935727FF975A
          29FF965A28FF955928FF8B5022FF42200DB80000001100000004000000010000
          00000000000000000000BFBFBFBF624A42FFBFBFBFBF00000000000000000000
          00000000000200000009462B1484965C2AFF733E19FC08040129000000120000
          001000000010000000110000001100000013000000195A3316B2965B29FF975B
          2AFF975B29FF975B29FF7D431BFF1E0E05610000000B00000002000000010000
          00000000000000000000BFBFBFBF624C43FFBFBFBFBF00000000000000000000
          00000000000100000005150D06339A602CFF905726FF7F461DFF7D451DFF7C43
          1CFF7A431CFF7A411BFF78411BFF78401AFF84491FFF945928FF9A5F2BFF995E
          2BFF985D2AFF965A29FF663315ED010100170000000600000001000000000000
          00000000000000000000BFBFBFBF634C44FFBFBFBFBF00000000000000000000
          000000000001000000030000000C7B4F24D19E662FFF9F6530FF9A602CFF985F
          2BFF975E2AFF985E2AFF975D2AFF965B2AFF9A5F2CFF9B612DFF9B612CFF9B60
          2CFF9A5F2BFF8D5325FF3C1F0C9D0000000D0000000300000000000000000000
          00000000000000000000BFBFBFBF654E44FFBFBFBFBF00000000000000000000
          0000000000000000000200000008482E15839E6530FF8D5527FC100A04350000
          00120000000F0000000F0000001100000019633E1CB59B602DFF9C632DFF9B61
          2EFF9C612DFF7F461DFF150A0445000000080000000200000000000000000000
          00000000000000000000BFBFBFBF664E45FFBFBFBFBF00000000000000000000
          0000000000000000000100000005120B052A9D642EFF8A5224FF311B0B750000
          000B0000000500000005000000080503011B8F5829F79F6430FF9E642FFF9E63
          2FFF995F2BFF623515DA00000010000000050000000100000000000000000000
          00000000000000000000BFBFBFBF674F46FFBFBFBFBF00000000000000000000
          00000000000000000001000000030000000A7D5125CD965E2BFF583214BA0000
          000A000000030000000200000008311E0D649C642EFFA06730FF9F6630FF9E66
          2FFF8F5626FF341B0B800000000A000000030000000000000000000000000000
          00000000000000000000BFBFBFBF675147FFBFBFBFBF00000000000000000000
          000000000000000000000000000200000006482F167E9E6730FF7F4A1FF70402
          011600000004000000030000000A653F1DB3A06731FFA16931FFA06831FFA067
          31FF814A20FC0B06022800000006000000010000000000000000000000000000
          00000000000000000000BFBFBFBF695147FFBFBFBFBF00000000000000000000
          000000000000000000000000000100000004120C0629A26A32FF8E5625FF2817
          095B00000007000000050503011694602CF7A36B33FFA26B32FFA26A32FF9C63
          2EFF5A3316BE0000000C00000003000000000000000000000000000000000000
          00000000000000000000BFBFBFBF6A5348FFBFBFBFBF00000000000000000000
          000000000000000000000000000000000002000000097E5428CA9A632DFF5431
          15AB000000090000000835220F65A16A32FFA56E35FFA46D34FFA46C34FF925A
          29FF2B180A640000000800000002000000000000000000000000000000000000
          00000000000000000000BFBFBFBF6B534AFFBFBFBFBF00000000000000000000
          000000000000000000000000000000000001000000054830177AA16B31FF7E4D
          20EE0201000F0000000B6B4520B4A56E35FFA66F35FFA56F35FFA56D34FF814B
          20F2040201170000000400000001000000000000000000000000000000000000
          00000000000000000000BFBFBFBF6C554AFFBFBFBFBF00000000000000000000
          00000000000000000000000000000000000100000003130D0627A46E35FF915B
          29FF201308480705021A9A662FF7A87237FFA87136FFA77037FF9F6830FF5230
          14A4000000090000000300000000000000000000000000000000000000000000
          00000000000000000000BFBFBFBF6D564CFFBFBFBFBF00000000000000000000
          00000000000000000000000000000000000000000002000000077E5729C79B65
          2FFF4C2E159038251167A67036FFA97338FFA97338FFA97238FF955E2AFF2113
          084A000000060000000100000000000000000000000000000000000000000000
          00000000000000000000BFBFBFBF6F574CFFBFBFBFBF00000000000000000000
          000000000000000000000000000000000000000000010000000548311776A56D
          33FF895A29E1724C24B8A97438FFAB7539FFAA7539FFA77136FF7C4B21DE0000
          000A000000030000000100000000000000000000000000000000000000000000
          00000000000000000000BFBFBFBF6F584DFFBFBFBFBF00000000000000000000
          0000000000000000000000000000000000000000000100000003130D0625A671
          36FCA97437FFA97438FFAD783AFFAC773BFFAC763AFFA16B32FF462B12860000
          0007000000020000000000000000000000000000000000000000000000000000
          00000000000000000000BFBFBFBF70584EFFBFBFBFBF00000000000000000000
          0000000000000000000000000000000000000000000000000002000000068159
          2BC6AE783BFFAE7A3CFFAE793CFFAD783BFFAD783BFF99622CFF120B052B0000
          0004000000010000000000000000000000000000000000000000000000000000
          00000000000061616161BFBFBFBF71594EFFBFBFBFBF61616161000000000000
          0000000000000000000000000000000000000000000000000001000000044831
          1772AD7839FFAF7B3CFFAF7B3CFFAF7A3CFFAB7539FF71471FC4000000070000
          0002000000000000000000000000000000000000000000000000000000000000
          000000000000BFBFBFBF735B50FF725B50FF725A4FFFBFBFBFBF000000000000
          000000000000000000000000000000000000000000000000000000000002100B
          051EAA7638FCB07C3DFFB07C3DFFAF7C3DFFA56F34FF3925116A000000050000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000005E5E5E5EB8B6B4C6745B51FFB8B6B4C65E5E5E5E000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0004815A2BC2B07C3EFFB17D3EFFB07D3EFF98632DF68D8C8A93BFBFBFC0BFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF89898989000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          00024732186DAC783AFFAB773AFFA87437FF67431EABBFBFBFC0786054FF7860
          54FF786054FF786054FF786053FF776054FF776053FF775F53FF765F53FF765E
          53FF765E53FF765E52FF765D52FF755D51FF745D51FFBFBFBFBF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000002000000040000000500000005000000038E8E8E8FBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF
          BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF8E8E8E8E000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000010000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000100000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000000000000000000000000
          0000000000000000000000000001000000030000000500000005000000050000
          0005000000040000000300000001000000010000000000000000000000000000
          0001000000020000000400000006000000060000000600000007000000070000
          0007000000070000000600000005000000020000000100000000000000000000
          00000000000000000001000000060000000D0000001200000013000000140000
          0014000000110000000B00000004000000010000000000000000000000010000
          00020000000700000010000000160000001800000019000000190000001A0000
          001A0000001A0000001800000012000000080000000200000001000000000000
          000000000000000000030000000A713A17FF713916FF713816FF6F3715FF6C37
          16FF643013FF3417099E00000008000000020000000100000000000000010000
          00030000000D643012FF632E11FF612E11FF622D10FF612D10FF602C10FF5F2B
          10FF5F2B10FF5E2B10FF471C09FF0000000F0000000400000001000000000000
          000000000000000000030000000A6F411DC9935727FF975A29FF965A29FF9256
          25FF693717DB2A15087A00000008000000020000000100000000000000010000
          00030000000D6B3B1ACD81491FEE915324FF905224FF905224FF905124FF9051
          23FF905123FF85471DFF431D0ACB0000000F0000000400000001000000000000
          00000000000000000001000000050000000F301D0D67945928FF945929FF6C3B
          17E40000001A0000000C00000004000000010000000000000000000000010000
          000200000007000000152213085D8D5023FF925324FF915325FF915224FF9152
          24FF7C411AFF2411077100000015000000080000000200000001000000000000
          000000000000000000000000000200000007000000127E4B22DD965A29FF6B35
          15F7010100180000000700000002000000000000000000000000000000000000
          0001000000040000000F301A0B748F5124FF935526FF925526FF925425FF8C4E
          22FF51250ED80000001900000009000000020000000100000000000000000000
          0000000000000000000000000000000000030000000A472B1388965C2AFF773D
          19FF130904420000000800000001000000000000000000000000000000000000
          000000000005000000105E3416BF935526FF945727FF935626FF935626FF7E44
          1BFF271207800000000F00000004000000010000000000000000000000000000
          00000000000000000000000000000000000100000007160D0637975E2AFF8149
          1FFF31190A860000000B00000003000000010000000000000000000000000000
          0001000000070402011D874D22F8965928FF955927FF955827FF945628FF6935
          14F8060301290000000800000002000000000000000000000000000000000000
          000000000000000000000000000000000001000000040000000D7C4E23D79056
          26FF552C11CA0000001000000007000000050000000400000004000000050000
          00060000000D29160A61935728FF975B29FF965B28FF965A28FF8C5123FF4320
          0DB8000000110000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000200000009472D1484985F
          2CFF77401AFC0804012900000012000000100000001000000011000000110000
          0013000000195A3416B2975B2AFF985D2AFF985C2AFF985C2AFF7E441DFF1E0E
          05610000000B0000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000005150D06339C63
          2EFF925927FF81481FFF7F461EFF7D451EFF7C441CFF7B431BFF79421BFF7941
          1BFF854A20FF955A2AFF9A602CFF9A5F2CFF9A5E2BFF975C2AFF683515ED0101
          0017000000060000000100000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000030000000C7D50
          25D1A06830FFA16831FF9C622DFF99612CFF9A602CFF99602BFF985F2BFF985E
          2BFF9B612DFF9D622EFF9C612DFF9B612DFF9B612DFF8E5325FF3D1F0C9D0000
          000D000000030000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000200000008492F
          1683A06832FF8E5727FC110A0435000000120000000F0000000F000000110000
          0019643E1CB59C632EFF9E642EFF9D632EFF9D622DFF81471EFF150B04450000
          0008000000020000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000100000005120C
          052AA06730FF8D5426FF321C0B750000000B0000000500000005000000080503
          011B905A2AF7A06730FF9F6630FF9E652FFF9A612CFF643515DA000000100000
          0005000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000030000
          000A7E5327CD98612CFF5A3216BA0000000A000000030000000200000008321F
          0E649E652FFFA16931FFA16832FFA06830FF905728FF341C0B800000000A0000
          0003000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          00064A31177EA06A31FF804D20F70403011600000004000000030000000A6641
          1DB3A26A32FFA36B33FFA26A33FFA26932FF824C20FC0B060228000000060000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0004130C0629A46D34FF905927FF29180A5B0000000700000005050301169663
          2EF7A56E35FFA46D34FFA46D33FF9E652FFF5C3316BE0000000C000000030000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000200000009805628CA9C662FFF573215AB000000090000000836231065A36D
          33FFA67036FFA66F36FFA56E35FF945C29FF2C180A6400000008000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000054932177AA36E34FF804E22EE0201000F0000000B6C4720B4A671
          36FFA87137FFA77137FFA67035FF824E21F20403011700000004000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000003130D0627A67036FF945E2AFF201308480705021A9C6931F7AA74
          39FFAA7338FFA97338FFA16B31FF543215A40000000900000003000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000020000000780582AC79E692FFF4C2F159039261167A87337FFAC76
          3AFFAB763AFFAA7539FF96602CFF2214084A0000000600000001000000000000
          0000000000000000000000000000000000000000000000000000000000000202
          02029999999CB7B7B7C2B2B2B2C3B1B1B1C4B0B0B0C4AFAFAFC4B3B3B3C39999
          999D020202030000000549321876A66F35FF8B5D2AE1734E26B8AC763AFFAC78
          3BFFAC773AFFA97438FF7C4E22DE0000000A0000000300000001000000000000
          0000000000000000000000000000000000000000000000000000000000000909
          0909A5A5A5AB7E6457F7755348FF715345FF6F5043FF66473BFF43312BF89F9F
          9FAA0909090A00000003140D0625A87437FCAB7739FFAB7739FFAF7A3CFFAE7A
          3CFFAD7A3BFFA36E34FF472C1386000000070000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00003737373AA59B97D5836353FF825F52FF7E5E4FFF5D3F32FF8C8481D73737
          373D000000000000000200000006825A2BC6B07B3DFFB07C3EFFAF7C3DFFAF7B
          3DFFAF7B3CFF9D652DFF130B052B000000040000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000084848489978076EB886556FF79594BFF76615BEC8181818C0000
          000100000000000000010000000448321872AF7A3BFFB07E3FFFB07D3EFFB17D
          3EFFAD773AFF734A20C400000007000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001B1B1B1CB2AEADC48F6C5EFC7A5748FCABA7A5C51B1B1B1D0000
          0000000000000000000000000002100B051EAB783AFCB27F3FFFB17E3FFFB17E
          3EFFA67135FF3A26116A00000005000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000065656566A5948CDE9E8E86DE64646467000000000000
          000000000000000000000000000100000004835C2CC2B17E3FFFB28040FFB37F
          3FFF9B662FF60906031600000003000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000009090909A5A5A5A6A3A3A3A409090909000000000000
          0000000000000000000000000001000000024732186DAE7A3BFFAE793BFFAB77
          38FF67441FAB0000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000002F2F2F2F2F2F2F2F00000000000000000000
          0000000000000000000000000000000000010000000200000004000000050000
          0005000000030000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0001000000010000000100000001000000010000000000000000000000000000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002000000040000
          0005000000050000000500000004000000020000000100000000000000010000
          0002000000040000000500000006000000060000000600000006000000060000
          0005000000020000000100000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000060000000C0000
          001100000013000000120000000D000000060000000200000001000000010000
          00050000000B0000001200000016000000180000001800000018000000160000
          000F000000070000000200000001000000000000000000000000000000000000
          00000000000000000000000000000000000000000003070402187C401AFF7A40
          1AFF7A3F19FF7A3E19FF653012FF0000000A0000000300000001000000020000
          0008592D11C8753917FF743916FF733816FF733816FF733715FF723716FF5B27
          0EFF0000000C0000000300000001000000000000000000000000000000000000
          000000000000000000000000000000000000000000020201000B2A180B5A7E48
          20E38F5224FF603315CA150A0445000000080000000200000001000000020000
          0007140A04373E230F878D5022FF915223FF8F5224FF8B4E22FF6C3717E41D0C
          05600000000A0000000200000001000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000040000000D371F
          0E71884D23FF512910C400000013000000070000000100000001000000020000
          0005000000102C190B698D5023FF925425FF915324FF7D411CFF2310066E0000
          0010000000050000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002000000070905
          0221864D24F76C3716F70402011D0000000A0000000600000005000000050000
          000800000012553116A8915425FF935626FF905325FF612F13EE0101001A0000
          0008000000020000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000030000
          000D643B1AB77A411BFF21110760000000160000001200000012000000130000
          001502010021854C22F0955728FF955727FF874C1FFF391A0A9D0000000F0000
          0004000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          0008331F0E658B5224FF854C20FF83491EFF83481EFF81481DFF80471DFF8046
          1DFF905425FF965B29FF965A29FF965928FF78401AFF13090442000000090000
          0002000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0005050301188F5928F79C612DFF975D29FF955A28FF945A28FF945928FF9358
          27FF985D2BFF985C2BFF985C2AFF945727FF592E12D300000011000000050000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00030000000B623D1CAE905625FF28170A600000001600000012000000140503
          01268A5326F09A5F2CFF9A5F2BFF894F22FF2F180A7C0000000B000000030000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000072F1E0E5A8C5425FF4A2911A00000000E000000080000000C301D
          0D62985F2BFF9C622DFF9B612DFF7B441DFA0804022500000007000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000004050302168B5627EF78441DEF02010011000000070000000C653E
          1DB19C632EFF9E632FFF965C2AFF512C12B50000000D00000003000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000200000009623E1DA7885022FF1F1207490000000B05030218945F
          2CF7A06730FF9F6631FF8C5324FF2514085A0000000800000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000002F2F2F2F2F2F2F2F000000000000
          000000000001000000062F1E0E56935B28FF482A12900000000F372411689E66
          30FFA26A33FFA06830FF77451DEA010100120000000500000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000909090BA1A1A1A99F9F9FA70909090C0000
          000000000001000000030201010E905D2AEE79491FE400000015704922BCA26A
          33FFA46C34FF9A622CFF472912950000000A0000000300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000016161616A7E7470E1776D69E26161616C0000
          000100000000000000020000000861401EA0905928FF27190C529D6932FAA670
          35FFA66E35FF905A28FF150C0535000000060000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000001B1B1B1EA5A2A1C7735950FD5D473EFD9E9B9BC81B1B
          1B1F0000000000000001000000052A1C0D4C9E652FFF8C5C2DDCA77237FFA872
          36FFA46E34FF6F431DCE0000000B000000030000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000018383838B88756DED83665AFF795E53FF6F5E59EE8080
          808D0000000100000000000000030201010C956530E9A97438FFAA7438FFA974
          39FF9F6830FF3B24107300000007000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003838383AA79F9BD4886B5EFF8A6C60FF876A5DFF74584DFF958E
          8AD63737373C0000000000000002000000065F411F9AAA7539FFAC773AFFAB76
          3AFF925E2BF70A06031D00000004000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000009090909A8A8A8AA957B71F6927366FF907165FF8D7062FF896C5FFF7861
          56F7A2A2A2A8090909090000000100000004271B0D44A97437FFAD793BFFA774
          37FF633F1CAE0000000800000002000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000020202029B9B9B9BBEBEBEBFBEBEBEC0BCBCBCC0BBBBBBC0BBBBBBC1BBBB
          BBC09A9A9A9C02020202000000000000000200000006966632E3A87437FFA16A
          31FF291B0C4C0000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000005000000070000
          0006000000040000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000020000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000000000
          0000000000000000000000000000000000010000000100000000000000000000
          0000000000010000000100000000000000000000000000000000000000000000
          0001000000030000000400000004000000030000000200000001000000000000
          0000000000010000000300000004000000040000000400000004000000040000
          0002000000010000000200000003000000040000000400000004000000030000
          0003000000040000000400000003000000020000000100000000000000010000
          00040000000A0000000D0000000D0000000B0000000600000002000000010000
          0001000000050000000B000000100000001100000011000000100000000C0000
          000700000004000000070000000C00000010000000100000000D0000000B0000
          000C00000010000000100000000C000000060000000200000000000000020000
          0007A45D28FFA25A25FFA05924FF9F5722FF2A17094B00000003000000010000
          00020000000899521FFF99511EFF98511EFF97501EFF964F1DFF964F1DFF2815
          074E0000000A0A060221683714B39B5320FF9A521FFF713B17C2281608563D20
          0C778B491BF0964F1DFF804519E01E10063C0000000300000001000000010000
          000638200E60AE6F3DF5B3723EFF4026126C0B06021900000003000000010000
          000200000008331C0A63AD6B39FFD59D6CFFCD8E58FFB16D39FF46260D830A05
          021D0000000D693A16B3C28654FFC07E49FF79431CC848260F8581461DD5AA6C
          3AF7CE9461FF9A5421FF0A0502260A0502190000000200000001000000000000
          00030000000A59331691AF6B36FF201207410000000800000002000000010000
          0002000000060A0602239E5824FFD39A67FFC68751FF83461AE1000000100000
          00080000000E9F5722FFD6A16FFFA05925FF0A060228000000120A0602279D57
          24FFD39D69FF97511EFF00000013000000060000000100000000000000000000
          00010000000638210F5EB77642FF4C2A12800000000800000003000000020000
          000300000008341C0B62AF6D3BFFD29964FFB6733EFF512B10930000000A0000
          00050000000C8C4C1FE0D09967FF9E5722FF000000150000000D000000139A53
          20FFD59D6BFF98511EFF00000011000000050000000100000000000000000000
          0000000000030000000A9D5A26EF6E3D1BB00000000C00000008000000070000
          00080000000D542D1292B97A47FFD09560FFA55F2CFF1F100640000000060000
          0003000000074C291181B3713EFFBA7B48FF4A2810830000000F000000149C54
          20FFD49C6AFF99521FFF00000010000000040000000100000000000000000000
          00000000000200000006663C1B9FB87B4BF64C2B12804C2A117E4B2A117E4B29
          117F4A281081AD7140EECB8D59FFC98B56FF86471BE00000000B000000030000
          00010000000300000009361E0C608D4C20E0B3713EFF4A281081150B0535A55E
          2CFFCF955FFF9B5320FF0000000F000000030000000100000000000000000000
          000000000001000000043A22105CBA7A46FFBE814DFFA35B25FFA25A25FFA259
          24FFBA7C49FFD09B68FFD59D6AFFB97743FF522D129000000007000000020000
          000000000002000000050000000A0000000D371E0D5E5630148F7F471DD0CC94
          62FFD6A171FF9E5623FF0000000D000000030000000100000000000000000000
          000000000000000000020C070318AF6934FF5A34188F0000000B000000090000
          000C4B291181BC7C4AFFD49966FFA8632FFF1F11073D00000004000000010000
          0000000000020B0603184E2D147D160D062C000000090000000A00000010A25C
          27FFD7A373FFA15925FF0000000B000000030000000000000000000000000000
          0000000000000000000100000005754521AD894E24CE00000007000000050000
          000882471CCFCF9665FFCC8F5AFF894A1FDF0000000800000002000000000000
          000100000004502F167CC28552FF7E481FBF0000000A000000090000000FA65F
          2AFFCB915DFF834A21D000000009000000020000000000000000000000000000
          0000000000000000000100000003472A156AB46F3BFF170D062800000005160C
          0529AB6531FFD8A16FFFB97742FF4B29107D0000000500000001000000000000
          0001000000032F1C0D4ABB7A45FFC0824FFF4528136E0000000B502F157FC183
          50FFBD7D4AFF4F2D157E00000006000000010000000000000000000000000000
          0000000000000000000000000002180F0726B6723EFF512F167B000000064D2C
          137CBE814EFFD79F6CFFAA632EFF150C05280000000200000001000000000000
          000000000001000000042F1C0E4A754723AEA36130EFAF6832FFAE6832FF8952
          28CF4529146D0000000600000002000000000000000000000000000000000000
          0000000000000000000000000001000000038D572BCD754520AC000000077A46
          20BDCD9665FFCB905BFF83491FCE000000050000000100000000000000000000
          0000000000000000000100000003000000040000000500000006000000060000
          0005000000040000000200000001000000000000000000000000000000000000
          00000000000000000000000000000000000254341B79B46F3AFF0C070318AD68
          33FFD9A36FFFBC7B46FF4D2B127B000000030000000100000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000001180F0825B97741FF7E4D28B2CC8F
          5CFFD79F6BFFAE6832FF160C0525000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000029E6232DED19967FFDCA6
          76FFD09460FF884E23CD00000002000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000623D2188CB925FFFDEAC
          7EFFC3824EFF502F167800000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000225170D39BF7E4AFFE0B0
          82FFB6733EFF170E062700000002000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000004AC6D39EFB671
          3BFF90562CCE0000000400000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002000000040000
          0004000000030000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000300000004000000050000
          0005000000050000000500000005000000050000000500000005000000050000
          0005000000050000000400000003000000030000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001B1B1B1B5B8B8B8C2B5B5B5C3B3B3B3C3B2B2
          B2C3B2B2B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B1B1B1C4B2B2
          B2C4B2B2B2C4B2B2B2C3949494A3848484916666666F2E2E2E33000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000002AEAEAEB63B2B25FB33211CFF32211CFF3121
          1AFF2F201AFF2F201AFF2F1F19FF2F1E19FF2D1E19FF2C1E18FF2B1E18FF2B1D
          16FF281914FF3D312CF5483F39F05E5754E68A8886D4B2B2B2C3979797A02727
          272B000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000002AFAFAFB66E625CE957453FF64D372DFF533A
          30FF533A30FF523930FF513930FF51392FFF51392FFF483229FF443026FF4630
          28FF442D26FF452E26FF402B22FF35231DFF291A15FF352C29F58F8D8CD1B4B4
          B4C03939393E0000000100000000000000000000000000000000000000000000
          0000000000000000000000000001767676799A9A9AA3B2B2B2C37E7570E04E38
          2EFF543D32FF533C31FF533C31FF523B30FF3B2720FF898482D7AFAFAFC5A2A0
          9FCB857E7BDA56453DF54D352CFF4F372EFF4A3229FF37231CFF221612FE827E
          7DD7ADADADB91414141700000001000000000000000000000000000000000000
          000000000000000000000000000000000001000000035252525AA7A5A4CA4D37
          2DFF563E34FF563D34FF563D33FF543C32FF271A15FFAFAFAFC43434343E4141
          4148949494A0ACABABC754423AF74E372CFF50382EFF50382EFF3B2720FF2219
          17FCA19F9FCB6B6B6B7200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000112121217AEACACC74E37
          2EFF584036FF583F35FF583F35FF563E35FF251914FFB2B2B2C3000000050000
          00020707070B9A9A9AA6847C78DB4B342CFF513930FF51392FFF4E362EFF2C1D
          16FF5A5251E79E9E9EAA00000003000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0FAEACACC65039
          30FF5A4138FF594137FF594137FF584036FF281A15FFB3B3B3C3000000040000
          0001000000014646464DA5A2A1CB49342AFF533C31FF533C31FF523A30FF412D
          26FF2F2622F8B2B2B2C100000004000000010000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0FAFAEADC6513B
          31FF5B4339FF5C4238FF5B4238FF5A4137FF2B1C17FFB4B4B4C3000000040000
          00010000000114141419B2B2B2C34B372EFE563D33FF543D32FF533C32FF4F39
          2FFF211410FEB2B2B2C30909090D000000010000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0FAFAEADC6533C
          34FF5D463BFF5D453BFF5D453BFF5C443AFF2C1F1AFFB4B4B4C3000000040000
          00010000000102020206B1B1B1C1523E35FB573F35FF563E35FF553E33FF543D
          33FF211611FFACACACC60909090E000000010000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0FB0AFAEC6553E
          36FF61473DFF60473DFF5F473CFF5F463CFF2F201CFFB5B5B5C3000000040000
          0000000000012727272CB1B1B1C4513A32FE594136FF584036FF584036FF563D
          33FF2B211CFCB4B4B4C30909090D000000010000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0FB0AFAEC65740
          37FF624A3FFF62493EFF61493EFF61483EFF33231CFFB5B5B5C3000000040000
          0001000000027A7A7A829A9694D1523A33FF5B4238FF5B4238FF594137FF4B34
          2BFF544B47ECACACACB800000003000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB0AFAEC65841
          39FF654B41FF644B40FF634A40FF634A3FFF36251EFFB4B4B4C3000000050909
          090C6868686EB4B4B4C370615AEB5A4137FF5D443BFF5C4339FF5C4339FF3C2A
          23FE999695CF7C7C7C8300000002000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB2B0B0C65B43
          3BFF674C43FF654D43FF654C42FF654B41FF382721FFB2B2B2C46E6E6E79A4A4
          A4AFAEADADC6786C66E55B4439FF60473CFF5E463DFF5A4239FF48342DFE847F
          7CD9B4B4B4BE2424242700000001000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB2B0B0C65D46
          3CFF695045FF694F45FF684F44FF674E43FF443129FF817A78DC827876DE7164
          60E859453EF95D453BFF5D453BFF5A4338FF523B32FF645650EE9D9999CFB4B4
          B4BD424242450000000100000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB2B0B0C55F48
          3DFF6B5147FF6B5146FF6A5046FF6A4F46FF5F463CFF574136FF594238FF6148
          3DFF5E453CFE756863E9746762E88B8380D9A5A2A2CAB5B5B5C28383838A2424
          2427000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB2B1B0C5614A
          40FF6D5349FF6C5349FF6C5247FF6B5247FF60473DFF7A6A67E87D6D69E86752
          49F75A4238FF584137FF5F4E49F4756C68E3A5A2A2CAB3B3B3BE616161660202
          0204000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB3B2B1C5644C
          41FF6F564BFF6E554BFF6D554AFF6D534AFF4C3630FFB3B3B3C39F9F9FAAB5B5
          B5C2938C88D8604741FD5E453BFF513B33FF45312AFE756B68E3B6B6B6C37575
          7579000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB3B2B1C5664E
          43FF71574CFF71574CFF70564CFF6F564CFF48352BFFB6B6B6C2000000054040
          4044B2B2B2BB908783DB634B41FF6B5046FF5F483EFF4A352EFF685C58E9B8B8
          B8C23939393C0000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0EB4B2B2C5684F
          44FF73594EFF73594EFF72584DFF72574CFF4B362DFFB8B8B8C2000000030000
          00014949494DB4B4B4C3654E46FB6C5348FF6C5248FF654A41FF452F28FF918B
          88D58484848A0000000100000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0DB4B2B2C56950
          46FF755A4FFF745A4FFF74594FFF74584EFF4F3930FFB8B8B8C2000000030000
          000102020204ACACACB57A6762ED6A5145FF6D5449FF6D5349FF563F37FF6254
          50EE9F9F9FA70000000200000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0DB4B2B2C56B52
          49FF765D51FF765C52FF755B51FF755B51FF523B32FFB8B8B8C1000000030000
          0000000000028585858D7F6D68EA6C5248FF6F554BFF6F554AFF644C41FF5041
          3CF7A8A8A8B20000000200000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000B0B0B0DB4B2B2C56C53
          4AFF775E53FF785E52FF775E52FF775D52FF553F34FFB8B8B8C1000000020000
          000100000002A4A4A4AC7A6961EE6D534AFF71564DFF70564CFF6C5248FF4C3B
          35FAA8A8A8B20000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000012121214B5B2B2C56E55
          4BFF795F54FF7A5F54FF795E54FF785E53FF574038FFB8B8B8C1000000030000
          000145454549B8B8B8C2725A51F871584DFF73584DFF73584DFF6A5047FF6251
          4CF1AAAAAAB20000000200000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000014949494CB2AFAEC77055
          4BFF7B6055FF7A6054FF7A5F55FF7A5F54FF594239FFB8B8B8C21D1D1D214949
          494DB6B6B6BD988E8BD96E5449FF755B50FF74594FFF745A4FFF5F483FFF8A81
          7DDC8F8F8F940000000100000000000000000000000000000000000000000000
          00000000000000000000000000008D8D8D8E9999999DBBBBBBC19C908DDA745A
          4FFF7C6257FF7B6256FF7C6155FF7B6055FF60483EFFA9A5A3CCB8B8B8C2B5B3
          B3C4978C89DA71584DFD785D52FF775C52FF765C51FF6F554AFF6A5650F2B6B5
          B5C3505050530000000100000000000000000000000000000000000000000000
          0000000000000000000000000001B2B2B2B5816E65EF7C675EF3755B50FF7D63
          58FF7D6258FF7D6257FF7C6257FF7C6258FF745B50FF684E44FF664D43FF684E
          45FF71584EFF785C51FF765D52FF72584DFF695045FF7A6964ECB3B2B2C59595
          9599040404050000000000000000000000000000000000000000000000000000
          0000000000000000000000000001B2B2B2B5745D53FB70574CFF70574CFF7057
          4CFF70574CFF70574CFF70574CFF70574CFF70564BFF6F564BFF6F554AFF7059
          4EFB79665DF379665CF37B675FF18D807BE2A7A2A0CEBBBBBBC1808080830B0B
          0B0C000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000B3B3B3B4BDBDBDC0BCBCBCC0BBBBBBC0BBBB
          BBC0BBBBBBC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1B0B0B0B69595959B9595959B909090946262626526262628000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000200000002000000010000
          0001000000010000000100000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          0003000000030000000300000003000000030000000300000003000000030000
          0003000000030000000100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001A0A0A0A3BABABAC1B8B8
          B8C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B6B6
          B6C2B8B8B8C1BBBBBBC01D1D1D1F000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000019E9E9EA4473A35F63322
          1CFF32211CFF32221BFF31211BFF31201BFF302019FF2E2019FF2E1F19FF2E1F
          18FF261A15FFA2A09FCB1F1F1F22000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000019F9F9FA48A817DDB6351
          49F2563E35FB543A31FF543B32FF543B31FF543B31FF523A30FF4F382FFF5742
          3AF84D403BF2A09E9DCC1F1F1F22000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000004E4E4E5094949499ADAD
          ADB7AEACACC65C4940F7553C32FF553C32FF553B32FF553B32FF463129FEACAC
          ABC7A2A2A2AC959595991F1F1F20000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010909
          090D9E9E9EA7887F7ADD563C32FF573E34FF563D34FF563D33FF35231EFFAFAF
          AFC51E1E1E240000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00014F4F4F54A29E9CCE563D33FF594036FF594035FF583F35FF392720FF9E9B
          9ACD404040450000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000024242427B4B4B3C3594138FE5B4238FF5B4237FF5B4135FF402D24FF8782
          7FD8616161670000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000004040406B4B4B4BD68534AF55D4238FF5D4339FF5C4339FF48332BFF7168
          64E2818181880000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000002999999A0796A62EA5E4439FF5F453BFF5F443BFF513931FF5D52
          4DEDA1A1A1A90000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000017979797F897E79DF5F453AFF61473DFF61463BFF593F36FF4C3B
          35F7B8B8B8C10909090B00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000015959595D9B9390D461463CFF634A3EFF63493DFF60463BFF412D
          26FFB2B1B1C52A2A2A2D00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000013737373AADAAA9C961473CFF664B3FFF654A3FFF644A3FFF4834
          2BFF9C9796D04B4B4B4F00000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000016161618B9B9B9C1674D44FB684C42FF684D41FF664C41FF503A
          30FF87807DDB6C6C6C7100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000002AFAFAFB676625AF1694E43FF694E42FF684D42FF5840
          36FF736864E58D8D8D9300000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000018F8F8F9585746FE6694E43FF6C5044FF6B4F44FF6147
          3BFF63544EF0ADADADB400000002000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000016E6E6E72948884DC694F43FF6E5146FF6D5146FF694E
          41FF554039FBB9B9B9C114141416000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000150505053A49E9CD16A4F44FF6F5448FF6F5347FF6D51
          46FF523C32FFADAAA9C835353538000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000002F2F2F31B3B2B1C56C5145FF705549FF705448FF7053
          49FF594137FF999492D35757575B000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000D0D0D0FBBBBBBC1765C53F8715549FF72564AFF7256
          4AFF61473DFF887D7BDE7777777B000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001A8A8A8AD836F67EE72564BFF74584CFF7457
          4BFF684C42FF796A64E99898989D000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000018787878B90807AE373564BFF76594DFF7458
          4DFF6E5146FF6A564FF3B7B7B7BD020202030000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001666666699E948FD873564BFF775A4FFF7659
          4FFF75574CFF5E453DFDB9B9B9C11F1F1F210000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000045454547ACA6A5CD74574BFF785B50FF785B
          4FFF775A4FFF61483DFFAAA6A4CB4747474A0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000124242427B4B2B2C574574CFF7A5C51FF7A5C
          50FF795C50FF684D42FF958C88D99999999D0B0B0B0D00000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000002F2F2F309898989AB3B3B3B7B3B1B0C676574DFF7B5E51FF7A5D
          51FF7A5C51FF71564AFF705B54F3B5B3B2C5B2B2B2B79797979A636363640000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000002F2F2F30A8A29ECF7B655DF2755B50FC7C5E53FF7C5F53FF7B5E
          52FF7A5E52FF795C50FF705449FF684E46FC715D57F3897D79E1A8A8A8AA0000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000002F2F2F30B1ADACCA76594DFF77594DFF765A4EFF76594DFF7559
          4DFF74594DFF74584CFF74574CFF71544BFF6C5146FF6A574DF8A8A8A8AA0000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001D1D1D1DBEBEBEBFBDBDBDC0BCBCBCC0BCBCBCC0BCBCBCC0BCBC
          BCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BDBDBDC0A8A8A8A90000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000300000004000000050000
          0005000000050000000500000005000000050000000500000005000000050000
          0005000000050000000500000005000000050000000500000005000000050000
          0005000000030000000100000001000000000000000000000000000000000000
          00000000000000000000000000018686868BB7B7B7C2B4B4B4C3B3B3B3C3B2B2
          B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B2B2
          B2C4B5B5B5C28686868C00000001000000000000000000000000000000000000
          0000000000000000000000000003B8B8B8C244352CFF35261FFF34251FFF3325
          1FFF32241EFF32241EFF32231EFF32231EFF32231EFF31231DFF30221DFF3022
          1CFF30211CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F
          1BFF2D1F1BFFB5B5B5C200000003000000000000000000000000000000000000
          0000000000000000000000000002B8B8B8C254433AFF544339FF544339FF5442
          39FF534139FF534039FF524039FF514038FF504038FF503F38FF4F3E38FF4F3D
          36FF4E3D36FF4E3D36FF4E3C34FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4D3B
          34FF3C2C29FFB6B6B6C200000003000000000000000000000000000000000000
          00000000000000000000000000018C8C8C90B8B8B8C2B5B5B5C3B4B4B4C3B3B3
          B3C3B2B2B2C4AFAFAFC4ACACACC5AAAAAAC6A8A8A8C7A7A7A7C7A6A6A6C7A6A6
          A6C7A7A7A7C7A8A8A8C7AAAAAAC6ADADADC5AFAFAFC4B1B1B1C4B2B2B2C4B2B2
          B2C3B6B6B6C28B8B8B9100000001000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000003000000054848
          4850ABABABB8A9A9A8C87A726EDF594D48ED44352FF836241EFF35231CFF3423
          1CFF382821FC473B35F46B6360E39A9998CEB1B1B1C157575760000000060000
          0005000000030000000100000001000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000014E4E4E52B7B7
          B7C2837B79DB49352EFC4C342BFF513830FF543B33FF533C32FF503930FF4E36
          2CFF4A332BFF442E27FF3D2922FF34231DFE706967E0B5B5B5C35C5C5C610000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000014141416B0B0B0B9837B
          78DC523A30FF5A4036FF5A4036FF583F36FF543C33FF65544DF0938D8BD4AFAE
          AEC6B2B2B2C39F9C9ACE786A65E5473129FE37241EFF6F6764E2B3B3B3BE1616
          1619000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000015858585DA9A6A6C95941
          38FC5C4239FF5C4238FF5C4338FF563D33FF6C5E58EBB2B2B2C4898989923333
          33381E1E1E236767676FB3B3B3C189827DDB483229FF392621FEA2A1A0CA5F5F
          5F65000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000028585858C867A76E05C43
          39FF5E453AFF5F4439FF5D4338FF534037FAADACACC679797980020202040000
          000100000001000000024646464CB4B4B4C367554DF1402B24FF706764E28989
          8992000000020000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000029E9E9EA7705F57EE5F46
          3CFF60473CFF60463BFF594136FF746763E5A6A6A6B00909090C000000000000
          00000000000000000000000000028B8B8B94887F7BDD49342CFF554843F09E9E
          9EAA000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002ADADADB76A534AF76248
          3EFF63483EFF62483EFF523B31FF928D8AD46767676E00000001000000000000
          00000000000000000000000000014D4D4D53A29E9CCD4F382EFF493933F7A9A9
          A9B6000000030000000100000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003B5B5B5C0644B42FB664B
          40FF654B40FF654A3FFF4E382FFFA7A5A4CA3535353A00000001000000000000
          00000000000000000000000000011E1E1E23AEACACC6543C32FF44332BFBB1B1
          B1BF000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003B7B7B7C2634840FE674C
          43FF684C42FF674C41FF4E372FFFB2B2B2C41616161A00000000000000000000
          00000000000000000000000000000B0B0B0FB2B2B2C4573E35FF443028FEB5B5
          B5C3000000040000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003B7B7B7C2644A40FF6A4E
          44FF6A4D43FF684E43FF50382FFFB6B6B6C20404040700000000000000000000
          000000000000000000000000000004040407B5B5B5C25A4136FF432E27FFB5B5
          B5C2000000040000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003B8B8B8C2664C41FF6B50
          46FF6B5045FF6B4F45FF523A31FFB7B7B7C20000000300000000000000000000
          000000000000000000000000000000000003B6B6B6C25C4238FF453129FFB5B5
          B5C2000000040000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003B8B8B8C2694D42FF6D52
          47FF6D5247FF6D5146FF543B33FFB8B8B8C20000000300000000000000000000
          000000000000000000000000000000000003B6B6B6C25E453AFF48322BFFB5B5
          B5C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003B8B8B8C16A4F44FF6F54
          49FF6F5348FF6E5348FF583D35FFB8B8B8C20000000300000000000000000000
          000000000000000000000000000000000003B6B6B6C260463CFF4A352CFFB6B6
          B6C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B8B8B8C16C5046FF7255
          4AFF71554AFF70544AFF594037FFB8B8B8C10000000300000000000000000000
          000000000000000000000000000000000003B7B7B7C261473EFF4D362EFFB6B6
          B6C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B8B8B8C16E5347FF7357
          4CFF73564BFF72564BFF5C413AFFB8B8B8C10000000200000000000000000000
          000000000000000000000000000000000003B7B7B7C263493FFF50382FFFB7B7
          B7C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B9B9B9C16F5348FF7558
          4DFF74584DFF74584DFF5E443BFFB8B8B8C10000000200000000000000000000
          000000000000000000000000000000000003B8B8B8C2664A41FF523A32FFB7B7
          B7C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B9B9B9C172554AFF775A
          4FFF77594EFF75594DFF61463DFFB9B9B9C10000000200000000000000000000
          000000000000000000000000000000000003B8B8B8C2674C43FF553D35FFB8B8
          B8C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002BABABAC172554BFF795B
          50FF785B4FFF775A4FFF63473DFFB9B9B9C10000000200000000000000000000
          000000000000000000000000000000000002B8B8B8C1694E44FF563E35FFB8B8
          B8C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002BABABAC174574CFF7A5C
          51FF795C50FF795C4FFF654B3EFFBABABAC10000000200000000000000000000
          000000000000000000000000000000000002B8B8B8C16A4F46FF5B4037FFB8B8
          B8C1000000030000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000D0D0D0FBABABAC175584DFF7A5E
          52FF7A5D51FF7A5D51FF664B42FFB9B9B9C11010101200000000000000000000
          000000000000000000000000000112121215B8B8B8C16D5147FF5C433AFFB8B8
          B8C1121212150000000100000000000000000000000000000000000000000000
          00000000000000000000000000000000000150505053B4B2B2C5765A4EFF7C5E
          52FF7B5E52FF7A5E51FF694E44FFB3B0AFC65B5B5B5F00000001000000000000
          00000000000000000000000000025959595DB0ADADC86E5448FF5E463BFFAFAD
          ACC86565656A0000000200000001000000000000000000000000000000000000
          000000000000000000007979797A9E9E9EA1BBBBBBC09B8F8ADB795C51FF7C5F
          53FF7C5F53FF7B5F52FF705548FF8F827DDFBABABAC19F9F9FA3767676780000
          00000000000081818183A1A1A1A5B9B9B9C1948781DF72574BFF664B41FF8477
          71E3B8B8B8C1A1A1A1A681818183000000010000000000000000000000000000
          00000000000000000000B3B3B3B58B7971E87F665CF5795B50FF7E6054FF7D60
          54FF7D5F54FF7D5F53FF785B50FF70544AFF745D55F686756FE8B2B2B2B50000
          000100000001BDBDBDC07F6E65EB735D52F672574BFF765B4FFF71564BFF674B
          41FF6B544DF6796862EBBCBCBCC0000000010000000000000000000000000000
          00000000000000000000B3B3B3B5795E53FB775A4EFF775A4EFF775A4EFF775A
          4EFF765A4EFF765A4EFF765A4EFF75594DFF73554BFF6F574CFBB2B2B2B50000
          000100000001BDBDBDC0715549FF72564BFF71574BFF71564AFF71564AFF7053
          49FF6C5146FF61473DFFBCBCBCC0000000010000000000000000000000000000
          0000000000000000000088888889BEBEBEC0BDBDBDC0BCBCBCC0BCBCBCC0BCBC
          BCC0BCBCBCC0BCBCBCC0BBBBBBC0BBBBBBC0BCBCBCC0BDBDBDC0888888890000
          00000000000090909092BCBCBCC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BBBBBBC1BCBCBCC090909092000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000000000
          0000000000000000000000000001000000010000000200000002000000020000
          0002000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          00000000000000000000000000018686868BB7B7B7C2B4B4B4C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B1B1B1C4B1B1
          B1C4B1B1B1C4B1B1B1C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0B0C4B1B1B1C4B2B2
          B2C4B5B5B5C38686868C00000001000000000000000000000000000000000000
          0000000000000000000000000003B8B8B8C244352CFF35261FFF34251FFF3325
          1FFF32241EFF32241EFF32231EFF32231EFF32231EFF31231EFF31231DFF3022
          1CFF30211CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F
          1BFF2D1F1BFFB5B5B5C200000003000000000000000000000000000000000000
          0000000000000000000000000003B7B7B7C254433AFF544339FF544339FF5442
          39FF534139FF534039FF524039FF514038FF504038FF503F38FF503F38FF4F3D
          36FF4E3D36FF4E3D36FF4E3C34FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4D3B
          34FF3C2C29FFB5B5B5C300000003000000000000000000000000000000000000
          0000000000000000000000000002AAAAAAB4AFAFAFC4AAAAAAC6A8A8A8C7A8A8
          A8C7A7A7A7C7A7A7A7C7A7A7A7C7A6A6A6C7A6A6A6C7A6A6A6C8A6A6A6C8A6A6
          A6C8A5A5A5C8A5A5A5C8A5A5A5C8A4A4A4C8A4A4A4C8A4A4A4C8A3A3A3C8A6A6
          A6C8ACACACC6A9A9A9B500000003000000000000000000000000000000000000
          0000000000000000000000000002B8B8B8C247382FFF382921FF372821FF3727
          20FF372720FF37271FFF37271FFF35271FFF35271FFF35261FFF34251FFF3324
          1FFF33241FFF32231EFF32231EFF32231EFF32231EFF31231DFF30221DFF3022
          1CFF30221CFFB5B5B5C200000003000000000000000000000000000000000000
          0000000000000000000000000002B8B8B8C15A473CFF59473CFF58463CFF5846
          3CFF57453CFF57443BFF56433BFF56433AFF54433AFF54433AFF54433AFF5441
          39FF534139FF524039FF524039FF514038FF513F38FF503F38FF503F38FF503E
          36FF3F302BFFB6B6B6C200000003000000000000000000000000000000000000
          00000000000000000000000000018C8C8C90B8B8B8C1B5B5B5C2B5B5B5C3B4B4
          B4C3B2B2B2C3B0B0B0C4AEAEAEC5ACACACC6A9A9A9C6A9A9A9C7A8A8A8C7A8A8
          A8C7A8A8A8C7A9A9A9C6ACACACC6AEAEAEC5B0B0B0C4B2B2B2C4B2B2B2C3B3B3
          B3C3B6B6B6C28B8B8B9000000001000000000000000000000000000000000000
          0000000000000000000000000000000000010000000200000003000000054848
          484FACACACB8AAA9A9C779716EDE5A4D4AED473631F838251FFF37241EFF3624
          1DFF382821FC483B37F46B6360E39A9897CEB2B2B2C157575760000000060000
          0005000000030000000100000001000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000014E4E4E52B8B8
          B8C2837D7ADB49362EFC4E372DFF533A32FF583F34FF583F34FF553C32FF5239
          30FF4E362EFF463129FF3F2C22FF33231DFE706967E0B5B5B5C25C5C5C610000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000014141416B0B0B0B8867D
          79DC543C32FF5E4338FF5E4439FF5D4338FF573E34FF65554FF0948E8CD4AFAE
          AEC5B3B3B3C3A19D9CCE7A6E68E54B362CFE38251EFF6E6664E1B4B4B4BE1616
          1619000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000015959595DAAA8A7C95B43
          3AFC60473CFF60463BFF60463BFF5A4035FF6E5F58EBB3B3B3C48A8A8A923333
          33381E1E1E236767676EB3B3B3C08D8380DB4B362CFF372720FEA3A1A1CA5F5F
          5F65000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000028585858C867B76E06046
          3CFF63483EFF62483EFF60473BFF554239FAAEADACC679797980020202040000
          000100000001000000024747474CB5B5B5C36B5950F1422E26FF706764E28989
          8992000000020000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000029F9F9FA774605AEE644A
          3FFF654B40FF654A3FFF5B4339FF746964E5A6A6A6B00909090C000000000000
          00000000000000000000000000028B8B8B948A807CDC4E372DFF554844F09F9F
          9FAA000000020000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002AEAEAEB76C544DF7664B
          42FF684C42FF674C41FF553D33FF938D8BD46868686E00000001000000000000
          00000000000000000000000000014D4D4D53A29E9DCD543B32FF4A3934F7AAAA
          AAB6000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B6B6B6C0674E46FB6A4E
          44FF6A4D43FF684E43FF513931FFA8A6A5CA3535353900000001000000000000
          00000000000000000000000000011F1F1F23AFADADC6573E35FF47332DFBB1B1
          B1BE000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003B8B8B8C2674C42FE6B50
          46FF6B5045FF6B4F45FF503A30FFB4B3B3C41616161900000000000000000000
          00000000000000000000000000000B0B0B0EB3B2B2C45B4137FF443229FEB5B5
          B5C2000000040000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B8B8B8C1694D42FF6D52
          47FF6D5247FF6D5146FF533932FFB7B7B7C20404040700000000000000000000
          000000000000000000000000000004040407B6B6B6C25E453AFF443028FFB6B6
          B6C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B8B8B8C16A4F44FF6F54
          49FF6F5348FF6E5348FF553B33FFB8B8B8C20000000300000000000000000000
          000000000000000000000000000000000003B6B6B6C260463CFF47322AFFB6B6
          B6C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B9B9B9C16C5046FF7255
          4AFF71554AFF70544AFF563E36FFB8B8B8C10000000300000000000000000000
          000000000000000000000000000000000003B7B7B7C261473EFF4A342CFFB6B6
          B6C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B9B9B9C16E5347FF7357
          4CFF73564BFF72564BFF594038FFB8B8B8C10000000200000000000000000000
          000000000000000000000000000000000003B7B7B7C263493FFF4D362DFFB7B7
          B7C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002B9B9B9C16F5348FF7558
          4DFF74584DFF74584DFF5C4339FFB8B8B8C10000000200000000000000000000
          000000000000000000000000000000000003B8B8B8C2664A41FF4F382FFFB7B7
          B7C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002BABABAC172554AFF775A
          4FFF77594EFF75594DFF5F443BFFB9B9B9C10000000200000000000000000000
          000000000000000000000000000000000003B8B8B8C2674C43FF523B33FFB8B8
          B8C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002BABABAC172554BFF795B
          50FF785B4FFF775A4FFF60453CFFB9B9B9C10000000200000000000000000000
          000000000000000000000000000000000002B8B8B8C1694E44FF543C33FFB8B8
          B8C2000000030000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002BBBBBBC174574CFF7A5C
          51FF795C50FF795C4FFF63493EFFBABABAC10000000200000000000000000000
          000000000000000000000000000000000002B8B8B8C16A4F46FF583E36FFB8B8
          B8C1000000030000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000D0D0D0FBBBBBBC175584DFF7A5E
          52FF7A5D51FF7A5D51FF654A40FFB9B9B9C11010101200000000000000000000
          000000000000000000000000000112121215B8B8B8C16D5147FF5A4138FFB8B8
          B8C1121212150000000100000000000000000000000000000000000000000000
          00000000000000000000000000000000000150505053B4B2B2C5765A4EFF7C5E
          52FF7B5E52FF7A5E51FF684C43FFB2B0AFC65B5B5B5F00000001000000000000
          00000000000000000000000000025959595DB0ADADC86E5448FF5C443AFFAEAC
          ABC86565656A0000000200000001000000000000000000000000000000000000
          000000000000000000007979797A9E9E9EA1BBBBBBC09B8F89DB795C51FF7C5F
          53FF7C5F53FF7B5F52FF705447FF8E817CDFBABABAC19F9F9FA3767676780000
          00000000000081818183A1A1A1A5B9B9B9C1928681DF72574BFF664B40FF8275
          6FE3B8B8B8C1A1A1A1A681818183000000010000000000000000000000000000
          00000000000000000000B3B3B3B58B7871E87E655BF5785A4FFF7E6054FF7D60
          54FF7D5F54FF7D5F53FF785C50FF6F5248FF735C54F684746DE8B2B2B2B50000
          000100000001BDBDBDC07D6C65EB715B50F671564AFF765B4FFF72564BFF654A
          3FFF69524AF676655EEBBCBCBCC0000000010000000000000000000000000000
          00000000000000000000B3B3B3B5795E53FB775A4EFF775A4EFF775A4EFF775A
          4EFF765A4EFF765A4EFF765A4EFF75594DFF73554AFF6D544BFBB2B2B2B50000
          000100000001BDBDBDC0705449FF71554AFF71574BFF71564AFF71564AFF7053
          49FF6B5045FF5E443BFFBCBCBCC0000000010000000000000000000000000000
          0000000000000000000088888889BEBEBEC0BDBDBDC0BCBCBCC0BCBCBCC0BCBC
          BCC0BCBCBCC0BCBCBCC0BBBBBBC0BBBBBBC0BCBCBCC0BDBDBDC0888888890000
          00000000000090909092BCBCBCC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BBBBBBC1BCBCBCC090909092000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000000000
          0000000000000000000000000001000000010000000200000002000000020000
          0002000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0001000000020000000300000004000000050000000500000005000000050000
          0004000000030000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000011919191C4848
          484E797979839191919F9C9C9CACB2B2B2C4B1B1B1C4B1B1B1C4B1B1B1C4B3B3
          B3C38F8F8F9C6666666F2727272C000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000010101012B9B9B9C19D9B
          9BCB736E6EDC4C4443EC372F2BF42F2522F81D110EFF1C110DFF19100BFF332C
          2BF355514FE68B8A8AD2B3B3B3C3898989921919191C00000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000130303035A7A5A5C82B1C
          16FF37251DFF432E26FF49332AFF4B342AFF4D352CFF4C352CFF473129FF3E2A
          22FF2F1F19FF1E130EFF383231F19A9998CCACACACB723232327000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002505050588F8A88D4533A
          31FF573E34FF4F382EFF564742F2918D8BD2AFAFAFC4AFAFAFC49E9A99CE6B5C
          57EB50382FFF4B342BFF32211BFF1E1615FC908F8FD0A4A4A4AF0909090C0000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000027070707A7C7370DF563D
          33FF4F382FFF584B46EFAFAEAEC58C8C8C972A2A2A311A1A1A2077777781B0B0
          B0C46A5A54ED513830FF51392FFF39271FFF251C19F9AAAAA9C75C5C5C620000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000039090909C6C5F5AE9583F
          35FF3E2C23FE9A9898CD8282828B020202050000000100000001000000037B7B
          7B859F9D9CCC4F382EFF553B31FF51382FFF2E1D18FF5F5A5AE29B9B9BA60000
          0003000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000100000003969696A2604F47F3543C
          33FF574B48EDB3B3B3C11B1B1B1F000000010000000000000000000000011515
          151BB0B0B0C4533E35FD563E32FF553D32FF442F26FF302926F6B2B2B2C10404
          0408000000010000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002979797A0645147F54531
          29FF817C7BD97E7E7E8600000002000000000000000000000000000000013939
          393FAEADADC5533C33FE584034FF573F34FF533B30FF261915FEAEADADC50909
          090E000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000019B9B9B9FB8B8B8C1AAA9
          A8C8B4B3B3C342424245000000010000000000000000000000010707070A9999
          99A4888280D7573E34FF5A4136FF5A4036FF584035FF2A1B16FFAAAAAAC60909
          090E000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001121212151212
          121512121214070707080000000000000000000000010B0B0B0E8F8F8F97ACAC
          ACC652413BF65C4138FF5C4238FF5C4238FF5A4136FF30221CFDB3B3B3C30909
          090D000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          000100000000000000010000000100000001232323279C9C9CA5A9A9A9C75243
          3EF25A4136FF5E453BFF5E443AFF5D433AFF543C32FF4E443FF0AFAFAFBC0000
          0003000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000300000004000000040000
          00040000000400000004000000044F4F4F55B0B0B0BB989594CF4D3C36F65A41
          38FF62483CFF60463CFF60463CFF60453BFF463029FF827E7DD8848484900000
          0005000000030000000100000001000000000000000000000000000000000000
          000000000000000000018F8F8F93B8B8B8C1B6B6B6C2B5B5B5C2B5B5B5C3B5B5
          B5C3B5B5B5C3B5B5B5C3B4B4B4C3B2B2B2C47B7270DE49362EFD5F463BFF654B
          3FFF644A3EFF63493DFF63483DFF573F34FF5D514DECABABABC6B0B0B0C4B4B4
          B4C3B7B7B7C28E8E8E9300000001000000000000000000000000000000000000
          00000000000000000002B9B9B9C1543B33FF533B32FF533B31FF523A31FF5138
          31FF503830FF503730FF4F372FFF573E35FF533B33FF674C41FF664C42FF664C
          41FF664C40FF654A3FFF654A3FFF5C4238FF4A342BFF463027FF453027FF442F
          26FF301F1AFFB8B8B8C200000003000000000000000000000000000000000000
          00000000000000000002B9B9B9C16A4E44FF6A4E43FF694D43FF694C42FF674C
          42FF684E43FF6D5146FF6C5046FF6C4F46FF6B4F44FF6B4F44FF6A4E44FF684E
          43FF684D42FF62493DFF60473CFF60463BFF5E453BFF5E453AFF5D443AFF5C43
          38FF32221AFFB8B8B8C200000003000000000000000000000000000000000000
          000000000000000000018F8F8F93B9B9B9C1B7B7B7C2B6B6B6C2B4B4B4C3AFAF
          AFC4968F8BD66C5247FD6E5247FF6D5247FF6C5146FF6D5046FF6C5046FF674D
          42FF5B473CFA938D8BD5B3B3B3C3B4B4B4C3B5B5B5C3B5B5B5C3B5B5B5C3B5B5
          B5C2B8B8B8C28F8F8F9300000001000000000000000000000000000000000000
          00000000000000000000000000010000000200000003020202069C9C9CA59690
          8ED25B4238FE6F5349FF6F5449FF6F5349FF6E5248FF6E5348FF654A3FFF6C5B
          53EFA9A8A7C8A9A9A9B23535353A000000040000000400000004000000040000
          0003000000020000000100000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000012C2C2C2FB6B6B6C26751
          49F672554AFF72564BFF71554BFF71554AFF70554AFF624A3FFE847975DFB6B6
          B6C28686868C1010101200000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000015757575B9D9895D16C4F
          46FF74584DFF74584DFF73574CFF72554AFF624A42FC9A9493D1B4B4B4BD5757
          575B000000010000000100000001000000020000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001616161678C807BDF7457
          4CFF77594EFF75594DFF75584EFF62483EFE9D9997D0AEAEAEB62C2C2C2F0000
          000100000000262626289D9D9DA19C9C9CA39C9C9CA27B7B7B7E000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000161616167897C77E27658
          4DFF785B4FFF775A4FFF6B4F45FF796C66E5B4B4B4BE2828282B000000010000
          0000000000015959595D9E9A99CE4C3C37F55F5350EAA2A2A2A8000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000161616166938883DD7659
          4EFF795C50FF795C4FFF5F463BFF928C89D56969697000000002000000000000
          0000000000028787878E887B77DF61473CFF4D3B36F7A0A0A0A9000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000152525255AAA5A2CD7559
          4DFF7A5D51FF7A5D51FF5D4239FF8D8684D77C7C7C8300000002000000010000
          000125252529B8B8B8C276625BEF6A4F46FF62524DEFA0A0A0A9000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000022222224BABABAC1856F
          67EF795C50FF7A5E51FF664B41FF665854EBB7B7B7C27474747A1B1B1B1F3C3C
          3C40A6A6A6AE9F9A99CF6A4F44FF684B42FF6F635DE78787878F000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000018C8C8C90B0AD
          ACC87B6156F9795D50FF765A4DFF594036FF6B5E5AE8A5A2A1CBB7B7B7C2B0AF
          AFC6938B89D6685048F9775A4FFF64493FFF7E7370E070707076000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000014141415AAAA
          AAAEB0ADADC8877169EF775A4FFF73564CFF60473DFF513831FF4E362EFF533A
          32FF62483EFF75584EFF765A4EFF5E453BFF908986D75B5B5B5F000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000001414
          14158989898CBBBBBBC1A59E9CD191827AE4826B62F27A6056F973574CFF7459
          4FFC7D685FF37B665EF28C7E79E29E9895D2B9B9B9C140404042000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000012F2F2F316969696C94949499A9A9A9AFBABABAC1B9B9B9C1B9B9
          B9C1B1B1B1B89595959C91919197616161653131313302020203000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000200000002000000020000
          0002000000020000000100000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0002000000030000000500000005000000060000000700000007000000060000
          0006000000040000000300000002000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000011919191D4848
          4850777777858E8E8EA0989898AEADADADC5ACACACC6ACACACC6ACACACC6AEAE
          AEC58C8C8C9E656565712727272E000000030000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000110101013B7B7B7C29998
          98CC6B6867DD3F3B3AED2A2522F4221B19F80E0705FF0E0705FF0D0605FF2823
          22F44A4848E7838383D4AEAEAEC5878787931919191E00000001000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000130303036A2A2A2C92014
          10FF312019FF3F2B23FF463127FF4B342AFF4D352CFF4C352CFF442E25FF3523
          1CFF21140FFF0F0806FF2E2B2BF2919191CEA9A9A9B923232329000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000100000002505050598B8786D5533A
          31FF573E34FF4F382EFF564741F38D8988D4AAAAAAC6AAAAAAC6989494D06859
          54ED50382FFF463027FF211410FF130E0DFC888787D2A1A1A1B00909090D0000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000001000000036F6F6F7B78706CE0563D
          33FF50392FFF534743EFA9A9A9C68A8A8A98292929321A1A1A2375757583ABAB
          ABC6685751EE513830FF50382EFF2A1A14FF191514FAA2A2A2C95B5B5B640000
          0002000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000048F8F8F9D6A5D57EA583F
          35FF3A2720FE949292CE8080808D020202060000000200000001000000047979
          7987999695CE4F382EFF553B31FF4F362EFF1D100DFF585554E4989898A80000
          0003000000010000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003949494A35E4D46F3543D
          33FF4D4340EDB0B0B0C21A1A1A1F000000010000000000000000000000011515
          151DABABABC6513D34FD563E32FF553D32FF3A2620FF241F1CF7ADADADC30404
          0409000000010000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003969696A1635048F53F2C
          25FF7A7675D97D7D7D8700000003000000010000000000000001000000023838
          3841A9A8A8C7533B32FE584034FF573F34FF50382EFF170D0AFEA9A9A9C70909
          0910000000010000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000019A9A9A9FB7B7B7C2A8A6
          A5C8B2B2B1C442424246000000010000000000000000000000010707070B9797
          97A5847E7BD9573E34FF5A4136FF5A4036FF584035FF1A0F0DFFA5A4A4C80909
          090F000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002121212161212
          121612121215070707080000000100000001000000020B0B0B0F8E8E8E99A7A7
          A7C7513F3AF65C4138FF5C4238FF5C4238FF5A4136FF201613FDAEAEAEC50909
          090F000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000001000000030000000400000004000000050000
          0005000000050000000500000005000000052323232A9A9A9AA6A5A4A4C94F41
          3BF35A4136FF5E453BFF5E443AFF5D433AFF533C33FF3F3734F1A7A7A7BE0000
          000A000000050000000200000001000000000000000000000000000000000000
          00000000000000000001BBBBBBC0B8B8B8C2B5B5B5C3B3B3B3C3B3B3B3C3B2B2
          B2C3B2B2B2C3B2B2B2C3B2B2B2C4B1B1B1C4AFAFAFC58F8C8BD1493933F75A41
          37FF62483CFF60463CFF60463CFF60453BFF412C26FF726F6EDBA6A6A6C7AEAE
          AEC5B3B3B3C3BABABAC100000002000000010000000000000000000000000000
          00000000000000000002B8B8B8C1402923FF402923FF3E2922FF3D2722FF3D27
          20FF3C2620FF3A2620FF3A261EFF39251EFF412A24FF432E26FF5F463BFF654B
          3FFF644A3EFF63493DFF63483DFF61473CFF422D26FF311F18FF2F1D18FF2F1D
          17FF1E120EFFB4B4B4C300000004000000010000000000000000000000000000
          00000000000000000002B8B8B8C2684C43FF674C42FF674C41FF664B41FF654A
          41FF654A40FF664B41FF694E43FF694E43FF694E43FF684D42FF664C42FF664C
          41FF664C40FF654A3FFF62483DFF5E4439FF5D4439FF5C4238FF5B4338FF5B41
          37FF20130FFFB3B3B3C300000004000000010000000000000000000000000000
          00000000000000000002B9B9B9C1B2B2B2C4ACACACC5ABABABC6AAAAAAC6A9A9
          A9C6A7A7A7C7958F8CD56C544BF96B4E45FF6B4F44FF6B4F44FF6A4E44FF684E
          43FF684D42FF5A4337FF6F635EE79F9F9FCAA1A1A1C9A1A1A1C9A1A1A1C9A3A3
          A3C9A9A9A9C6B5B5B5C300000003000000010000000000000000000000000000
          00000000000000000002B8B8B8C1473028FF462F26FF452E26FF452E26FF432E
          26FF4B342BFF4F382FFF6A4E44FF6D5247FF6C5146FF6D5046FF6C5046FF6B50
          44FF6A4F43FF563E34FF3C2820FF39251EFF38251DFF37231DFF36231CFF3523
          1BFF251711FFB4B4B4C300000004000000010000000000000000000000000000
          00000000000000000002BABABAC16E5147FF6D5147FF6D5047FF6C4F46FF7154
          49FF71554AFF70544AFF6F5449FF6F5349FF6E5248FF6E5348FF6B4F44FF674B
          41FF654B40FF644A40FF64493FFF64493FFF63473EFF61473EFF61473DFF6047
          3CFF261913FFB5B5B5C200000003000000010000000000000000000000000000
          00000000000000000001BDBDBDC0B9B9B9C1B7B7B7C2B5B5B5C3AFAEAEC5755E
          55F672554AFF72564BFF71554BFF71554AFF70554AFF624A3FFE847975E0B0B0
          B0C4B2B2B2C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C3B5B5B5C2BBBBBBC100000001000000010000000000000000000000000000
          000000000000000000000000000100000002000000035656565D9B9593D16C50
          46FF74584DFF74584DFF73574CFF72554AFF624941FC989390D2B3B3B3BE5656
          565C000000050000000500000006000000070000000600000005000000050000
          0004000000030000000100000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001616161678C807BDF7457
          4CFF77594EFF75594DFF75584EFF62473DFE999492D0AEAEAEB72C2C2C300000
          000100000001262626289C9C9CA29A9A9AA39B9B9BA37A7A7A7E000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000161616167897B77E27658
          4DFF785B4FFF775A4FFF6A4F44FF726661E6B3B3B3BE2727272B000000010000
          0000000000015858585D999695CE3E322FF5544C48EAA1A1A1A9000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000161616166948884DD7659
          4EFF795C50FF795C4FFF594035FF8D8786D56969697000000002000000000000
          0000000000028787878F857975E05B4138FF3F2F2DF79F9F9FAA000000030000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000152525255AAA5A2CC7559
          4DFF7A5D51FF7A5D51FF513931FF898380D77C7C7C8400000002000000010000
          000125252529B6B6B6C275625BEF684D44FF554843EF9F9F9FAA000000030000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000022222224BBBBBBC1856F
          67EF795C50FF7A5E51FF5C4138FF5D524DECB7B7B7C27474747A1B1B1B1F3C3C
          3C41A5A5A5AE9D9997CF6A4E45FF63473EFF665A56E88686868F000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000018C8C8C8FB2AF
          AEC87B6156F9795D50FF715549FF4B332AFF635854E8A3A19FCBB7B7B7C2AEAC
          ACC68D8584D6664F46F9775A4FFF5D423AFF766D69E070707077000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000014141415AAAA
          AAAEB2AFAEC8877169EF775A4FFF6D5047FF543B32FF422C25FF3F2A23FF4731
          29FF60463DFF76584EFF765A4EFF573E34FF898380D75A5A5A5F000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000001414
          14158989898CBBBBBBC1A7A09DD192837BE4816B62F27A6056F973574CFF7459
          4FFC7E685FF37C665FF28B7D78E29B9593D2B9B9B8C140404042000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000012F2F2F316969696C94949499A9A9A9AFBABABAC1B9B9B9C1B9B9
          B9C1B0B0B0B89595959C91919197616161653131313302020203000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000002000000020000
          0002000000020000000200000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000959595B6B9B9B9C1B7B7
          B7C2B6B6B6C2B5B5B5C2B5B5B5C2B7B7B7C29E9E9EC000000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000100000001000000010000
          00010000000100000001000000010000000100000001ADADADB4221F92FB1815
          8EFF17128DFF16128BFF151089FF140F88FFB8B8B8C200000000000000000000
          0000000000000000000100000002000000040000000500000006000000060000
          0005000000030000000100000001000000010000000300000005000000060000
          00070000000700000007000000070000000600000005AEAEAEB46C69A2DF3835
          98F3B2B2B2C4B5B5B5C28A89A6D316128BFFB8B8B8C100000000000000000000
          000000000001000000029C9C9CC1B5B5B5C3B0B0B0C4AFAFAFC5AFAFAFC4B2B2
          B2C4B7B7B7C2616161850000000230303052B8B8B8C1B2B2B2C3AEAEAEC5ACAC
          ACC5ACACACC6ACACACC6ACACACC5AEAEAEC5B3B3B3C39A9A9AC19494949C4342
          94DF525092D28888888FBBBBBBC0BBBBBBC1BEBEBEC000000000000000000000
          00000000000100000003B7B7B7C239261EFF38251DFF38251DFF38251DFF3221
          19FF615853E76C6C6C74000000032C2C2C3195918FD135231BFF35231BFF3423
          1BFF34221BFF34221BFF34221AFF33221AFF241712FFB5B5B5C27D7D7D818888
          8890413F9AE9605F99D2A2A2A2A85959595A0000000000000000000000000000
          00000000000000000002B8B8B8C18B8480D9523E37F94C352CFF452F26FF5E53
          4EEB9E9D9CCB6C6C6C74000000022C2C2C31AEACACC6817975DC483229FF4C34
          2BFF4C352BFF4B342BFF462F26FF54473FF17E7A77D9B7B7B7C2000000038B8B
          8B8D9898A0AE34339EF76E6DA7DFBBBBBBC01919191900000000000000000000
          0000000000000000000140404044A0A0A0AD9E9B99CE513D35FA412D24FF574C
          4AEBAEAEAEC53F3F3F47000000030909090D89898999908C89D4493328FF4D35
          2BFF4C362CFF483228FF43352EF6A3A2A2C9868686949898989E8787878B8787
          878AA3A3A3A78584AED622209BFFA4A4B5CA4C4C4C4D00000000000000000000
          00000000000000000000000000011B1B1B1FA5A5A5B1948F8DD34A342AFE3423
          1BFF7F7977DAAAAAAAB81818181E41414149AFAFAFC45C4D44F24D352CFF4E36
          2DFF4E372CFF3B2822FE959291CF979797A50909090EB6B6B6B95152AAEC5051
          A8ECA2A2A2A6A6A5B5CA2525A0FF7B7AACDA4B4B4B4D00000000000000000000
          0000000000000000000000000000000000012C2C2C31B1B1B1BF817773DF422D
          26FF382A24FC9D9B9ACD8F8F8F9CA1A1A1B0867E7CDB4B362BFF4F382EFF4F37
          2EFF442F25FF655D5AE5AFAFAFC01C1C1C2100000001B7B7B7B93335ACFC4142
          ACF7B9B9B9C17475AEDE2829A4FF8888B1D64B4B4B4D00000000000000000000
          0000000000000000000000000000000000000000000151515157B3B3B3C36B5D
          57EC402C25FF493D37F3AAAAAAC7A6A4A4C9554038F951392FFF513A2FFF4D36
          2EFF42332DF8A9A8A7C86262626A0000000200000001B7B7B7B89C9DB5C75357
          B1EE2E31ACFF2D2FAAFF6365AEE6BDBDBDC04040404000000000000000000000
          00000000000000000000000000000000000000000000000000027B7B7B82ADAC
          ACC65C4841F63B2921FF6A605CE6746963E650392FFF533B31FF533B31FF412D
          25FF85817ED79E9E9EAA0909090C0000000100000000262626268C8C8C8CBEBE
          BEBFBEBEBEC0BDBDBDC0BEBEBEC0737373740000000000000000000000000000
          00000000000000000000000000000000000000000000000000000909090C9A9A
          9AA3A09D9BCE4C392FFC4B352BFF553E33FE543D33FF543D33FF4D382EFF5549
          45EFB2B2B2C33737373D00000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000011D1D
          1D21ADADADBA8C8582D9533C32FF573F35FF573F34FF563F34FF432F27FE9B99
          98CD8282828B0000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000404
          04079C9C9CA8908986D7553D33FF584136FF594036FF4E382EFF6B625EE5B0B0
          B0BF1A1A1A1F0000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000014D4D
          4D52B1B1B0C5614D45F75A4137FF5A4237FF5A4237FF422E26FF94908FD19A9A
          9AA70909090D0000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000D0D0D0FACAC
          ACB58B807DDD5A4137FF5C4439FF5C4439FF5C4338FF4C352CFF4A3A35F6ACAB
          ABC67C7C7C830000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000016E6E6E73ACAB
          AAC75F4840FB5F473CFF5E463BFF5A4239FF67564EF273645EEB48342BFF6155
          51ECB5B5B5C35353535800000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000124242427B6B6B6C07D6F
          69E75E463CFF61483DFF61483DFF4F3B31FE9B9795D0ABAAA9C7614F45F64631
          29FF7C7371DFB3B3B3BE2C2C2C30000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000018F8F8F95A39E9DCE6048
          3EFE644A40FF634A40FF5B4339FF716560E7B4B4B4C09C9C9CA6A39F9ECE5742
          39FD46332AFE96918FD2A7A7A7B0101010130000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000015959595DB7B7B7C2725F58F1644B
          40FF654C41FF644B41FF533F36FCACABAAC76D6D6D7321212125B1B1B1BB8F86
          81DC533D34FF4C3B34F9A9A8A7C89292929A0909090C00000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000222222235F5F5F63B6B6B6BF8A7F7ADF654C40FF674E
          44FF684D43FF5E463CFF756A64E6ACACACB712121216000000024A4A4A50B4B4
          B4C36E5B53F3543D35FF5B4B44F3AEADACC69393939A3C3C3C3F000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000064646466A9A5A3CC8A7D78E1684E44FE6A5045FF6A50
          45FF695045FF5D453BFF736762E7ADABAAC74E4E4E51000000028F8F8F949F98
          96D275625BF1644A40FF553E35FF5E4E47F3928B89D6A6A6A6AB000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000001646464678B7E79E2664C42FF654C42FF654C42FF664B
          42FF654B42FF61473EFF533C34FF8D8481DB4E4E4E51000000028F8F8F947361
          59F1634A40FF634A40FF60483EFF574037FF53413AF8A5A5A5AB000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000065656566BDBDBDC0BBBBBBC1BABABAC1B9B9B9C1B9B9
          B9C1B8B8B8C1B9B9B9C1BABABAC1BCBCBCC04F4F4F500000000191919193BBBB
          BBC1B8B8B8C1B7B7B7C2B7B7B7C2B8B8B8C2BABABAC18D8D8DAF000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000002000000020000
          0002000000020000000200000002000000010000000000000000000000010000
          0001000000020000000300000003000000020000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000040000000500000006000000060000
          0005000000030000000100000001000000010000000300000005000000060000
          0007000000070000000700000007000000060000000500000002000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000001000000028E8E8E94B5B5B5C3B0B0B0C4AFAFAFC5AFAFAFC4B2B2
          B2C4B7B7B7C2535353580000000222222225B8B8B8C1B2B2B2C3AEAEAEC5ACAC
          ACC5ACACACC6ACACACC6ACACACC5AEAEAEC5B3B3B3C38C8C8C94000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          00000000000100000003B7B7B7C239261EFF38251DFF38251DFF38251DFF3221
          19FF615853E76C6C6C74000000032C2C2C3195918FD135231BFF35231BFF3423
          1BFF34221BFF34221BFF34221AFF33221AFF241712FFB5B5B5C2000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          00000000000000000002B8B8B8C18B8480D9523E37F94C352CFF452F26FF5E53
          4EEB9E9D9CCB6C6C6C74000000022C2C2C31AEACACC6817975DC483229FF4C34
          2BFF4C352BFF4B342BFF462F26FF54473FF17E7A77D9B7B7B7C2000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000140404044A0A0A0AD9E9B99CE513D35FA412D24FF574C
          4AEBAEAEAEC53F3F3F47000000030909090D89898999908C89D4493328FF4D35
          2BFF4C362CFF483228FF43352EF6A3A2A2C98686869440404045000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000011B1B1B1FA5A5A5B1948F8DD34A342AFE3423
          1BFF7F7977DAAAAAAAB81818181E41414149AFAFAFC45C4D44F24D352CFF4E36
          2DFF4E372CFF3B2822FE959291CF979797A50909090E00000001000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000012C2C2C31B1B1B1BF817773DF422D
          26FF382A24FC9D9B9ACD8F8F8F9CA1A1A1B0867E7CDB4B362BFF4F382EFF4F37
          2EFF442F25FF655D5AE5AFAFAFC01C1C1C210000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000151515157B3B3B3C36B5D
          57EC402C25FF493D37F3AAAAAAC7A6A4A4C9554038F951392FFF513A2FFF4D36
          2EFF42332DF8A9A8A7C86262626A000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000027B7B7B82ADAC
          ACC65C4841F63B2921FF6A605CE6746963E650392FFF533B31FF533B31FF412D
          25FF85817ED79E9E9EAA0909090C000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000909090C9A9A
          9AA3A09D9BCE4C392FFC4B352BFF553E33FE543D33FF543D33FF4D382EFF5549
          45EFB2B2B2C33737373D00000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000011D1D
          1D21ADADADBA8C8582D9533C32FF573F35FF573F34FF563F34FF432F27FE9B99
          98CD8282828B0000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000404
          04079C9C9CA8908986D7553D33FF584136FF594036FF4E382EFF6B625EE5B0B0
          B0BF1A1A1A1F0000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000014D4D
          4D52B1B1B0C5614D45F75A4137FF5A4237FF5A4237FF422E26FF94908FD19A9A
          9AA70909090D0000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000D0D0D0FACAC
          ACB58B807DDD5A4137FF5C4439FF5C4439FF5C4338FF4C352CFF4A3A35F6ACAB
          ABC67C7C7C830000000200000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000016E6E6E73ACAB
          AAC75F4840FB5F473CFF5E463BFF5A4239FF67564EF273645EEB48342BFF6155
          51ECB5B5B5C35353535800000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000124242427B6B6B6C07D6F
          69E75E463CFF61483DFF61483DFF4F3B31FE9B9795D0ABAAA9C7614F45F64631
          29FF7C7371DFB3B3B3BE2C2C2C30000000010000000087878789B9B9B9C1B7B7
          B7C2B6B6B6C2B5B5B5C2B5B5B5C2B7B7B7C28F8F8F9200000000000000000000
          000000000000000000000000000000000000000000018F8F8F95A39E9DCE6048
          3EFE644A40FF634A40FF5B4339FF716560E7B4B4B4C09C9C9CA6A39F9ECE5742
          39FD46332AFE96918FD2A7A7A7B01010101300000001AEAEAEB4221F92FB1815
          8EFF17128DFF16128BFF151089FF140F88FFB8B8B8C200000000000000000000
          0000000000000000000000000000000000015959595DB7B7B7C2725F58F1644B
          40FF654C41FF644B41FF533F36FCACABAAC76D6D6D7321212125B1B1B1BB8F86
          81DC533D34FF4C3B34F9A9A8A7C89292929A0909090CAFAFAFB46C69A2DF3835
          98F3B2B2B2C4B5B5B5C28A89A6D316128BFFB8B8B8C100000000000000000000
          00000000000000000000222222235F5F5F63B6B6B6BF8A7F7ADF654C40FF674E
          44FF684D43FF5E463CFF756A64E6ACACACB712121216000000024A4A4A50B4B4
          B4C36E5B53F3543D35FF5B4B44F3AEADACC69393939A7777777B9595959C4342
          95DF525092D28888888FBBBBBBC0BBBBBBC1BEBEBEC000000000000000000000
          0000000000000000000064646466A9A5A3CC8A7D78E1684E44FE6A5045FF6A50
          45FF695045FF5D453BFF736762E7ADABAAC74E4E4E51000000028F8F8F949F98
          96D275625BF1644A40FF553E35FF5E4E47F3928B89D6A6A6A6AB7E7E7E808888
          888F413F9AE9605F99D2A2A2A2A85959595A0000000000000000000000000000
          00000000000000000001646464678B7E79E2664C42FF654C42FF654C42FF664B
          42FF654B42FF61473EFF533C34FF8D8481DB4E4E4E51000000028F8F8F947361
          59F1634A40FF634A40FF60483EFF574037FF53413AF8A5A5A5AB000000018B8B
          8B8C9898A0AE34339EF76E6DA7DFBBBBBBC01919191900000000000000000000
          0000000000000000000065656566BDBDBDC0BBBBBBC1BABABAC1B9B9B9C1B9B9
          B9C1B8B8B8C1B9B9B9C1BABABAC1BCBCBCC04F4F4F500000000191919193BBBB
          BBC1B8B8B8C1B7B7B7C2B7B7B7C2B8B8B8C2BABABAC1B6B6B6BA8787878B8787
          878AA3A3A3A78584AED622209BFFA4A4B5CA4C4C4C4D00000000000000000000
          0000000000000000000000000000000000010000000100000002000000020000
          0002000000020000000200000002000000010000000000000000000000010000
          00010000000200000003000000030000000200000002B6B6B6B95152ABEC5051
          A8ECA2A2A2A6A6A5B5CA2525A0FF7B7AACDA4B4B4B4D00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000B7B7B7B93335ACFC4142
          ACF7B9B9B9C17475AEDE2829A4FF8888B1D64B4B4B4D00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000B7B7B7B89C9DB5C75357
          B1EE2E31ACFF2D2FAAFF6365AEE6BDBDBDC04040404000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000262626268C8C8C8CBEBE
          BEBFBEBEBEC0BDBDBDC0BEBEBEC0737373740000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000010000
          0002000000050000000700000008000000080000000800000009000000090000
          000900000009000000090000000A0000000A0000000A0000000A0000000A0000
          000B0000000B0000000B0000000B0000000B0000000B0000000C0000000C0000
          000C0000000C0000000C0000000B000000090000000400000001000000020000
          000800000012000000180000001B0000001C0000001D0000001D0000001E0000
          001F0000001F0000002000000021000000220000002300000023000000240000
          00250000002500000026000000270000002800000028000000290000002A0000
          002B0000002B0000002B000000290000001F0000000E00000003000000030000
          000E000000320000003300000034000000350000003600000037000000380000
          00390000003A0000003B0000003C0000003D0000003E0000003F000000400000
          0041000000420000004300000044000000450000004600000047000000480000
          00490000004A0000004B0000004C0000004D0000001C00000007000000030000
          000E000000230000002400000025000000260000002700000028000000280000
          00290000002A0000002B0000002C0000002D0000002E0000002F000000300000
          0031000000310000003200000033000000340000003500000036000000370000
          0038000000390000003A0000003B0000003C0000002000000008000000020000
          000900000016000000170000001700000018000000190000001A0000001A0000
          001B0000001C0000001D0000001E0000001E0000001F00000020000000210000
          0022000000230000002300000024000000250000002600000027000000280000
          0028000000290000002A0000002B0000002C0000001C00000007000000010000
          00040000000B0000000B0000000C0000000C0000000D0000000E0000000E0000
          0010000000110000001100000011000000120000001200000013000000140000
          00140000001500000016000000170000001700000018000000190000001A0000
          001A0000001B0000001C0000001D0000001E0000001100000005000000000000
          0001000000030000000400000004000000040000000500000007000000090000
          000D000000100000000E0000000B0000000A000000090000000A0000000B0000
          000B0000000C0000000C0000000D0000000E0000000E0000000F000000100000
          00100000001100000012000000110000000D0000000600000002000000000000
          00000000000000000000000000000000000000000000000000020000000B0000
          001A000000220000001C0000000E000000050000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000005000000191E21
          5A9D2E30A3FF03022CA20000002A000000140000000800000004000000020000
          0001000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000070000001E4951
          BEFF6E88F7FF2836ACFF03022CA90000003500000021000000160000000F0000
          000C000000080000000300000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000010000000500000017494C
          719C6F81E9FF4E6BF3FF37326AFF382526F3211513A70A07055D000000330000
          002C000000210000001200000007000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000020000000B0000
          0022494C70A15D5B8AFF684C41FF7B5C4FFF64473DFF463029FF3E2822FF3C27
          21FF2E1B1AD90201013C00000016000000070000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000040000
          000F0000002B725D58F0B0988BFF876558FF745346FF5B3B30FF56382DFF5737
          2EFF2F1A15FF110B2CDD0000063D000000160000000700000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000001000000010000000100000001000000020000
          00070000001A3E312A9E806254FFBAA193FF82604FFF67473BFF634338FF3A21
          1AFF483977FF1F1E9FFF080550DC0000063B0000001500000007000000020000
          0000000000000000000000000000000000000000000000000000000000010000
          0002010100050101010501010106010101060101010601010106010101060101
          010901010115443F3BEC7C6153FFBAA08EFFA17D67FF876655FF4D3027FF5545
          85FF4647CDFF3636C4FF1E1CA1FF0A0755DE0000063A00000014000000060000
          0001000000000000000000000000000000000000000000000000010100040101
          010A030202110302021503030216030302160303021603030216030302160303
          021803030220363535E67F6556FFAC8D7FFFA88672FF755243FF5D4C8CFF4F50
          CFFF4040CAFF4041CCFF3A3AC6FF2120A4FF0A0858DD01000638000000130000
          0006000000010000000000000000000000000000000000000000010101092F21
          1C59926554EAA06F5EFFA0705EFFA06F5EFFA06F5EFF9F6F5FFFA06F5EFF9F6F
          5EFF9D6E5DFFB8A39CFF816657FF9C7D6FFF886756FF645492FF575BD2FF4243
          CCFF4242CBFF4343CBFF4445CDFF3E3EC6FF2323A8FF0A0959DC010006370000
          00130000000700000001000000000000000000000000000000000202010F9265
          55E7DFCCC4FFF3EBE4FFF4EBE5FFF4EBE4FFF4EBE4FFF4EBE5FFF3EAE4FFF3EB
          E4FFEFE6E0FFE3DFDCFF8F796CFF826355FF70629DFF6164D6FF4546CDFF4546
          CCFF4546CDFF4546CCFF4647CDFF4949CFFF4142CAFF2626AAFF0B095BDC0101
          083700000012000000060000000100000000000000000000000002020211A475
          64FFF8F2EEFFF4ECE5FFF5ECE5FFF5EDE8FFF9F5F2FFF9F5F2FFFAF5F2FFF9F5
          F2FFF2E8E2FFF1EFEDFFD8D6D7FF686296FF8189DAFF5256D1FF474ACEFF474A
          CFFF474ACEFF474ACEFF474ACEFF484BD0FF4C50D1FF4347CBFF282AACFF0F0B
          61DC01010835000000110000000600000001000000000000000002020210A778
          67FFF8F4F0FFF5EDE7FFD4C7C0FF684D42FF684D41FF674C41FF674C41FFFAF5
          F3FF674B40FF6A5046FF9C8D86FF9B8F92FF6064BCFF7B82D9FF585CD5FF4B4E
          D1FF4B4ED1FF4B4ED1FF4B4ED1FF4B4ED2FF4D51D2FF5156D4FF484CCEFF2C2D
          B0FF0F0F64DB01010833000000100000000600000001000000000202020FAA7C
          6CFFFAF5F2FFF6EFE9FF6D5145FFFAF6F4FFF5EEE9FFF6EEE9FF694E43FFFAF6
          F3FF694D43FFF9F5F2FFF3EBE6FFEFEAE8FF9C9295FF696FC6FF858DDEFF5C63
          D7FF4F53D2FF4F53D3FF4F53D3FF4E53D3FF4F53D3FF5056D4FF555BD6FF4B50
          D1FF2F32B5FF120F67DA010108320000000F00000005000000010201010DAE81
          70FFFBF7F5FFF7F0ECFF705448FFF8F2EEFFFAF7F5FFFAF7F5FF6D5247FFFAF7
          F5FF6B5145FFFBF7F4FFF5EEE9FFF4EDE8FF9F9089FFDCDBE1FF6F76CAFF9099
          E2FF6A71DCFF5F67DBFF6169DCFF636BDDFF6169DCFF5E66DBFF5C63D9FF5C63
          D9FF5056D2FF3437B7FF15136CDA020109300000000E000000050201010CB285
          74FFFCF9F7FFF8F2EDFFD8CCC7FF72564BFF71564AFF71554AFF715549FFFCF8
          F7FF6F5448FFFBF8F6FFF9F1EDFFF7F1ECFF72574CFFF4F1F1FFDDDCE1FF777E
          CFFF9EAAE8FF7782E3FF6972DFFF6972DFFF6872DFFF6872DFFF6872DFFF6973
          E0FF666FDEFF555DD4FF383DBBFF171771DB0202092D0000000C0201010BB589
          79FFFCFBF9FFFAF5F1FFFAF5F2FFFAF5F2FFFCFAF8FFFCFAF8FF73574BFFFAF5
          F3FF73564BFFFCFAF7FFFCF9F8FFFCF9F7FF715549FFF7F2EFFFF3F0EEFFE0DE
          E5FF777BCAFFA7B4ECFF7F8BE6FF707BE2FF6F7BE2FF6F7BE2FF6F7BE2FF6F7B
          E2FF707CE2FF707CE2FF5B63D7FF3D42BFFF1A1972D80101031C0101010ABE94
          83FFFEFCFBFFFBF8F5FFFBF7F4FF795C51FF775B50FF775B50FFDAD1CCFFFBF6
          F4FF765A4EFF755A4DFF74594DFF74594DFFDAD0CBFFFAF5F2FFF8F4F0FFF5F2
          F0FFCAB9B9FF5961AFD7B0BDEFFF8795E9FF7885E5FF7785E6FF7885E6FF7885
          E6FF7885E5FF7986E6FF7885E6FF6069D9FF363AB0FF1212489101010108C79F
          8DFFFEFDFCFFFCF9F6FFFCF9F6FFFCF8F6FFFCF9F6FFFBF8F6FFFBF8F6FFFBF8
          F5FF7A5D52FFFDFBFAFFFBF8F5FFFBF8F5FFFBF8F5FFFBF7F5FFFBF8F4FFF8F6
          F2FFD8C0B7FF090910285D66B2D5B6C4F1FF8F9EEBFF7F8DE8FF7F8DE8FF7F8E
          E8FF7F8EE8FF7F8EE8FF808FE8FF7A88E6FF5E66D5FF2E2E93EB01010107CBA5
          94FFFEFEFDFFFDFAF9FFFCFAF8FFFCFAF8FFFDFAF8FFFDFAF8FFFCFAF8FFFDFA
          F8FF7C5F54FFFDFCFBFFFCF9F7FFFCF9F7FFFCF9F6FFFCF9F7FFFBF9F6FFFCF9
          F7FFCBA697FF0101010E08091020606AB4D4BCCAF3FF96A6EDFF8797EAFF8797
          EBFF8797EAFF8797EAFF8697EAFF8899EAFF8591E6FF393AA5F701010106CEAA
          99FFFFFEFEFFFDFCFAFFFDFBF9FFFDFBF9FFFDFBF9FFFDFBF9FFFDFBF9FFFDFB
          F9FF7E6356FFFDFBF9FFFDFAF8FFFDFBF9FFFCFAF8FFFCFAF8FFFDFAF8FFFCFB
          F8FFCFAA99FF01010109000000070809101C636FB7D3C1CFF5FF9EAFF0FF8E9F
          EDFF8EA0EDFF8EA0EDFF8EA0EDFF98AAEFFF8D99E2FF333688C901000004C2A1
          92EEF4E9E5FFFFFFFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFE
          FEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFFFFEFEFFF4E9
          E5FFC3A192EE0101010500000002000000040809101B6772B8D2C6D4F6FFB9CA
          F6FFA0B3F2FF98ABF1FFA3B6F3FFBECEF6FF717ACEFD1516325100000002352D
          2842A88E7FCAD5B3A1FFD5B3A2FFD5B4A1FFD5B4A1FFD5B3A2FFD6B3A1FFD5B3
          A1FFD5B4A1FFD5B3A1FFD5B4A1FFD5B3A1FFD5B4A1FFD5B3A1FFD5B3A2FFA88D
          7FCB352D284400000003000000010000000100000003090A1019636DB2CDABB9
          EFFFCCDBF7FFD5E4FAFFBDCBF2FF7D8AD8FD282B526F00000008000000010000
          0001000000020000000300000003000000030100000401000004010000040100
          0004010000040100000401000004010000040100000401000004010000040000
          00030000000200000001000000000000000000000001000000020303050B3E45
          71836570BAD4727BD0EE5760A5C0191B2E3E0000000600000002000000000000
          0000000000000000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000010000
          0003000000040000000400000004000000030000000100000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000010000
          0002000000050000000700000008000000080000000800000009000000090000
          000900000009000000090000000A0000000A0000000A0000000A0000000A0000
          000B0000000B0000000B0000000B0000000B0000000B0000000C0000000C0000
          000C0000000C0000000C0000000B000000090000000400000001000000020000
          000800000012000000180000001B0000001C0000001D0000001D0000001E0000
          001F0000001F0000002000000021000000220000002300000023000000240000
          00250000002500000026000000270000002800000028000000290000002A0000
          002B0000002B0000002B000000290000001F0000000E00000003000000030000
          000E000000320000003300000034000000350000003600000037000000380000
          00390000003A0000003B0000003C0000003D0000003E0000003F000000400000
          0041000000420000004300000044000000450000004600000047000000480000
          00490000004A0000004B0000004C0000004D0000001C00000007000000030000
          000E000000230000002400000025000000260000002700000028000000280000
          00290000002A0000002B0000002C0000002D0000002E0000002F000000300000
          0031000000310000003200000033000000340000003500000036000000370000
          0038000000390000003A0000003B0000003C0000002000000008000000020000
          000900000016000000170000001700000018000000190000001A0000001A0000
          001B0000001C0000001D0000001E0000001E0000001F00000020000000210000
          0022000000230000002300000024000000250000002600000027000000280000
          0028000000290000002A0000002B0000002C0000001C00000007000000010000
          00040000000B0000000B0000000D0000000D0000000E0000000F0000000F0000
          0010000000110000001100000011000000120000001200000013000000140000
          001400000015000000170000001800000018000000190000001A0000001B0000
          001B0000001C0000001D0000001E0000001F0000001100000005000000000000
          000200000005000000080000000A0000000B0000000C0000000D0000000D0000
          000D0000000D0000000D0000000C0000000A000000090000000A0000000C0000
          000E000000110000001300000015000000170000001700000018000000190000
          00190000001B0000001B00000017000000100000000700000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000007A411AFF79411AFF77411AFF773F1AFF763F
          19FF5C2C12F0000000000000000000000000000000000000000000000000723A
          18FF723A17FF713917FF713817FF6F3717FF6F3716FF6F3715FF6F3615FF5B28
          0EFF000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000004A2C1481835024E4975D2BFF925626FF6137
          18B7291408600000000000000000000000000000000000000000000000003B21
          0E6C70401CC6935626FF945727FF935627FF945626FF915325FF783F1AE73418
          0984000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000002A190B48955B29FF844B20FF1D0F
          063F000000000000000000000000000000000000000000000000000000000000
          000020120839915425FF955928FF945827FF945828FF7F441CFF1D0E06420000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000834D24DE81491EFF331A
          0A78000000000000000000000000000000000000000000000000000000000000
          00004A2A1387935726FF965A29FF965A28FF915525FF582C0FCF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000004F3016878D5325FF572F
          11C9000000000000000000000000000000000000000000000000000000000000
          00007C4721E1975B2AFF975C29FF965A29FF854A1FFF2E16096F000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000001A10072D955C29FF7840
          1BFF0D07031E0000000000000000000000000000000000000000000000002113
          083C935828FF995D2AFF985D2AFF985C2AFF703D16F907040112000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000007F4F24D5854C
          20FF331B0B720000000000000000000000000000000000000000000000005531
          1696975C2AFF9A5E2BFF9A5E2BFF915627FF4C260FAB00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000004A2E157B945B
          29FF8A5022FF824A1FFF82491FFF82491FFF80471EFF7E471EFF895022FF965B
          29FF9C612DFF9C612DFF9B602CFF834B20FF1F10064800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000150D06249C63
          2EFFA06730FF9C632FFF99602CFF995F2CFF985F2BFF985E2BFF9C622EFF9E63
          2EFF9D632EFF9D632EFF985F2CFF6A3815E40101000300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000794F
          23C9955B2AFF3A220F6900000000000000000000000000000000623D1CA59D63
          2EFF9E652FFF9D632FFF915827FF3D210D870000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000462D
          1572945C29FF583114B400000000000000000000000005030109925B29F3A067
          30FFA06630FF9E6530FF844C20FF120904270000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000000120C
          051E97612AFC7E491FFC090502120000000000000000301E0E519D652FFFA168
          31FFA06731FF9B622DFF5F3615C3000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000784E25C08B5324FF2C190A5A000000000000000065401EA8A06831FFA26A
          33FFA26932FF915827FF301B0A63000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000412B1469965E2BFF5C3516B4000000000503010996602BF6A46C34FFA36C
          33FFA36B33FF804A20F60603010C000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000B07031299652CFC804A20F60503010935221057A16B32FFA56D35FFA46D
          34FF9D662FFF522F149F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000744E25B7905927FF311E0E576C4821AEA56E35FFA67036FFA66F
          36FF925B28FF2013083F00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000003B27135D9A642DFF714B22B79D6932F6A87137FFA87136FFA46D
          34FF77461FDB0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000A06030F9A662EF6A77136FFAA7338FFA97338FFA87338FF9E67
          30FF4328117E0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000714C23AEA97438FFAB7539FFAB7439FFAA7438FF8F5C
          29FC100A041E0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000036251154A87437FFAB763AFFAB7639FFA67136FF683F
          1CBA000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000060402099E6D33F3AD783BFFAC773AFF9E6930FF311E
          0D57000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000006D4A24A5A87236FFA77135FF8A5825ED0503
          0109000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000003000000050000000500000003000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000002000000060000000B00000011000000110000000D000000070000
          0002000000010000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          0001000000050402011454250F9B853A18E8863C17EA59270FA50502011A0000
          0007000000020000000100000001000000030000000400000004000000040000
          0004000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000004000000040000000100000001000000000000
          000200000008592C1499B77950FFE7CDA5FFDEB98EFFB26F43FF5D2D14A20000
          000C00000003000000018686868BB7B7B7C2B4B4B4C3B3B3B3C3B3B3B3C3B3B3
          B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B3B3B3C3B6B6B6C28686868C00000001000000000000
          0002000000089D562CECF0DFC3FFF2E0BEFFEEDBB6FFE5C7A1FFA55B2EF70000
          000E0000000400000003B8B8B8C243332CFF33241FFF32241FFF32231EFF3224
          1EFF32231EFF32231EFF31231EFF31221DFF30221DFF30221CFF30221CFF2F21
          1CFF2F211CFF2F211CFF2F201CFF2E201CFFB7B7B7C200000003000000000000
          000100000006AD6939F4F6EBD4FFF8F0D4FFF7EDCEFFF2E1C1FFB06B3BF70000
          000C0000000300000003B8B8B8C1544339FF544239FF534139FF524139FF5140
          38FF514038FF513F38FF503F38FF503F38FF4F3E38FF4F3E36FF4E3D36FF4E3D
          35FF4E3C34FF4D3C34FF4D3C34FF3E2F29FFB7B7B7C200000003000000000000
          0001000000046E482994D19C72FFF1E0C6FFEFDEC0FFD09B6EFF794E2DA50000
          000800000002000000018B8B8B90B8B8B8C2B5B5B5C2B4B4B4C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3
          B3C3B3B3B3C3B3B3B3C3B4B4B4C3B7B7B7C28B8B8B9000000001000000000000
          0000000000010705030D7951319CBB7E4DEEBD804DF17952319E070503110000
          0003000000010000000100000001000000030000000300000003000000030000
          0003000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000004000000030000000100000001000000000000
          0000000000000000000200000005000000080000000900000006000000040000
          0001000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          000000000002000000060000000B00000011000000120000000E000000070000
          0002000000010000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          000100000005040201145A2C139B8E441DE88F461EEA5E2E14A50603011A0000
          0007000000020000000000000001000000020000000300000003000000030000
          0003000000030000000300000003000000030000000300000003000000030000
          0003000000030000000300000003000000030000000100000001000000000000
          0002000000085E331999BC8157FFE7CEA6FFDFBA8FFFB87849FF62341AA20000
          000C00000003000000018787878BB9B9B9C1B7B7B7C2B6B6B6C2B6B6B6C2B6B6
          B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C3B5B5B5C3B5B5B5C2B8B8B8C28686868B00000001000000000000
          000200000008A56136ECF1E0C4FFF2E0BEFFEEDBB6FFE6C8A2FFAC6738F70000
          000E0000000400000003BABABAC14A3A30FF3B2B22FF3B2B22FF3A2A21FF392A
          21FF392A21FF382921FF372821FF372821FF372821FF372720FF37271FFF3727
          1FFF35271FFF35271FFF35261FFF35261FFFB8B8B8C100000002000000000000
          000100000006B37444F4F6ECD5FFF8F0D4FFF7EDCEFFF2E2C2FFB77546F70000
          000C0000000300000003BABABAC15D493EFF5C493EFF5B493DFF5B483DFF5B48
          3DFF5A473CFF5A473CFF5A473CFF59473CFF58463CFF58453CFF57453CFF5744
          3BFF56443AFF56433AFF54433AFF44352CFFB9B9B9C100000002000000000000
          000100000004714D3094D5A47BFFF2E3C8FFF0E0C3FFD5A377FF7D5535A50000
          000800000002000000018C8C8C8FBABABAC1B8B8B8C2B7B7B7C2B7B7B7C2B7B7
          B7C2B7B7B7C2B7B7B7C2B7B7B7C2B7B7B7C2B6B6B6C2B6B6B6C2B6B6B6C2B6B6
          B6C2B6B6B6C2B6B6B6C2B7B7B7C2B9B9B9C18C8C8C9000000001000000000000
          0000000000010705030D7C57389CBF8758EEC28959F17C57389E070503110000
          0003000000010000000000000001000000020000000200000002000000020000
          0002000000030000000300000003000000030000000300000003000000030000
          0003000000030000000300000003000000020000000100000000000000000000
          0000000000000000000200000005000000080000000900000006000000040000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000002000000060000000B00000011000000120000000E000000070000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000005040201145F33189B965025E8985226EA643619A50603011A0000
          0007000000020000000000000001000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000100000000000000000000
          000200000008643A1F99C48B5EFFE8CFA7FFE0BB90FFBF8150FF683C20A20000
          000C00000003000000018787878ABBBBBBC0BABABAC1B9B9B9C1B9B9B9C1B9B9
          B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C2B8B8B8C2B8B8B8C2B8B8B8C1BABABAC18787878A00000001000000000000
          000200000008AC6D40ECF2E1C5FFF2E0BEFFEEDBB6FFE7C9A3FFB47344F70000
          000E0000000400000002BBBBBBC0503F35FF413027FF412F27FF412F26FF402F
          25FF402F26FF402F25FF3F2F25FF3F2F24FF3E2E24FF3E2E24FF3E2D23FF3D2D
          23FF3D2D23FF3D2D23FF3D2C23FF3C2C23FFBBBBBBC100000002000000000000
          000100000006BB7F50F4F7EDD6FFF8F0D4FFF7EDCEFFF3E3C4FFBE8152F70000
          000C0000000300000002BCBCBCC0634F43FF624E43FF624E43FF624E42FF624E
          42FF624D42FF624D42FF624D41FF614D41FF604D40FF604D40FF5F4D40FF5F4B
          40FF5F4B40FF5E4A3FFF5E4A3FFF4C3C31FFBBBBBBC100000001000000000000
          00010000000474543794DAAD85FFF3E5CBFFF1E3C5FFD9AC81FF815D3DA50000
          000800000002000000018D8D8D8FBCBCBCC0BBBBBBC1BABABAC1BABABAC1BABA
          BAC1BABABAC1BABABAC1BABABAC1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1BBBBBBC18C8C8C8F00000001000000000000
          0000000000010706040D7F5E409CC59264EEC79365F17F5E409E070604110000
          0003000000010000000000000000000000010000000100000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000100000000000000000000
          0000000000000000000100000002000000030000000400000003000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          00000000000000000000BEBEBEC0BABABAC1B8B8B8C1BCBCBCC02E2E2E2F0000
          0001000000010000000300000004000000040000000400000004000000040000
          0004000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000001000000010000000000000000000000000000
          00000000000000000000BCBCBCC015108AFF140F88FF8F8EAAD1BDBDBDC00000
          00018686868BB7B7B7C2B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2
          B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C4B2B2
          B2C4B3B3B3C3B6B6B6C28686868C000000010000000000000000000000000000
          00000000000000000000BDBDBDC0B7B7B7C28E8CA9D216128AFFBBBBBBC10000
          0002B8B8B8C243332CFF33241FFF32241FFF32231EFF32241EFF32231EFF3223
          1EFF31231EFF31221DFF30221DFF30221CFF30221CFF2F211CFF2F211CFF2F21
          1CFF2F201CFF2E201CFFB7B7B7C2000000030000000000000000000000000000
          00000000000000000000BEBEBEC01E1C96FF1C1A93FF7372A5DCBBBBBBC00000
          0002B8B8B8C1544339FF544239FF534139FF524139FF514038FF514038FF513F
          38FF503F38FF503F38FF4F3E38FF4F3E36FF4E3D36FF4E3D35FF4E3C34FF4D3C
          34FF4D3C34FF3E2F29FFB7B7B7C2000000030000000000000000000000000000
          00000000000000000000BEBEBEBFBBBBBBC19494B1D11F1D97FFBCBCBCC00000
          00018B8B8B90B8B8B8C2B5B5B5C2B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3
          B3C3B4B4B4C3B7B7B7C28B8B8B90000000010000000000000000000000000000
          00000000000000000000BEBEBEBF292AA5FF2728A2FF9797B4CFBEBEBEBF0000
          0001000000010000000300000003000000030000000300000003000000040000
          0004000000040000000400000004000000040000000400000004000000040000
          0004000000040000000300000001000000010000000000000000000000000000
          00000000000000000000BFBFBFBFBEBEBEBFBEBEBEBFBEBEBEBF2E2E2E2E0000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          00000000000000000000BEBEBEC0B9B9B9C1B6B6B6C2B7B7B7C2BDBDBDC00000
          0000000000010000000200000003000000030000000300000003000000030000
          0003000000030000000300000003000000030000000300000003000000030000
          0003000000030000000300000001000000010000000000000000000000000000
          00000000000000000000BBBBBBC018158DFF17128BFF15108AFFB9B9B9C10000
          00018787878BB9B9B9C1B7B7B7C2B6B6B6C2B6B6B6C2B6B6B6C2B5B5B5C2B5B5
          B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5
          B5C3B5B5B5C2B8B8B8C28686868B000000010000000000000000000000000000
          00000000000000000000BCBCBCC01D1B95FF7270A3DDB5B5B5C2BDBDBDC00000
          0002BABABAC14A3A30FF3B2B22FF3B2B22FF3A2A21FF392A21FF392A21FF3829
          21FF372821FF372821FF372821FF372720FF37271FFF37271FFF35271FFF3527
          1FFF35261FFF35261FFFB8B8B8C1000000020000000000000000000000000000
          00000000000000000000BEBEBEBF9696B3D021209AFF9493B0D1BDBDBDC00000
          0002BABABAC15D493EFF5C493EFF5B493DFF5B483DFF5B483DFF5A473CFF5A47
          3CFF5A473CFF59473CFF58463CFF58453CFF57453CFF57443BFF56443AFF5643
          3AFF54433AFF44352CFFB9B9B9C1000000020000000000000000000000000000
          00000000000000000000BFBFBFBFBDBDBDC09595B3D023239EFFBCBCBCC00000
          00018C8C8C8FBABABAC1B8B8B8C2B7B7B7C2B7B7B7C2B7B7B7C2B7B7B7C2B7B7
          B7C2B7B7B7C2B7B7B7C2B6B6B6C2B6B6B6C2B6B6B6C2B6B6B6C2B6B6B6C2B6B6
          B6C2B7B7B7C2B9B9B9C18C8C8C90000000010000000000000000000000000000
          00000000000000000000BFBFBFBF2C2FAAFF2B2CA7FF9898B6CFBEBEBEC00000
          0000000000010000000200000002000000020000000200000002000000030000
          0003000000030000000300000003000000030000000300000003000000030000
          0003000000030000000200000001000000000000000000000000000000000000
          00000000000000000000BFBFBFBFBFBFBFBFBEBEBEBFBEBEBEBF2E2E2E2E0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000BFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBFBF0000
          0000000000010000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000001000000000000000000000000000000000000
          00000000000000000000BFBFBFBF1B1791FF19168FFF18148EFFBFBFBFBF0000
          00018787878ABBBBBBC0BABABAC1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C2B8B8B8C2B8B8
          B8C2B8B8B8C1BABABAC18787878A000000010000000000000000000000000000
          00000000000000000000BFBFBFBF9898B5CE1E1C95FF9797B4CEBFBFBFBF0000
          0001BBBBBBC0503F35FF413027FF412F27FF412F26FF402F25FF402F26FF402F
          25FF3F2F25FF3F2F24FF3E2E24FF3E2E24FF3E2D23FF3D2D23FF3D2D23FF3D2D
          23FF3D2C23FF3C2C23FFBBBBBBC1000000020000000000000000000000000000
          00000000000000000000BFBFBFBFBFBFBFBF22229CFFBFBFBFBF2E2E2E2E0000
          0001BCBCBCC0634F43FF624E43FF624E43FF624E42FF624E42FF624D42FF624D
          42FF624D41FF614D41FF604D40FF604D40FF5F4D40FF5F4B40FF5F4B40FF5E4A
          3FFF5E4A3FFF4C3C31FFBBBBBBC1000000010000000000000000000000000000
          00000000000000000000BFBFBFBF282AA5FF2728A2FFBFBFBFBF000000000000
          00018D8D8D8FBCBCBCC0BBBBBBC1BABABAC1BABABAC1BABABAC1BABABAC1BABA
          BAC1BABABAC1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1BBBBBBC18C8C8C8F000000010000000000000000000000000000
          00000000000000000000BFBFBFBF9B9CB9CE2C2EA8FFBFBFBFBF000000000000
          0000000000000000000100000001000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000001000000000000000000000000000000000000
          000000000000000000002E2E2E2EBFBFBFBFBFBFBFBFBFBFBFBF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          0005000000050000000500000002000000010000000000000000000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000028D8D8D94B4B4
          B4C3B1B1B1C4B4B4B4C38D8D8D94000000030000000100000003000000040000
          0004000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000004000000040000000100000001000000000000
          0000000000000000000000000000000000000000000100000003B6B6B6C21712
          8BFF15108AFF140F88FFB6B6B6C2000000048686868CB7B7B7C2B4B4B4C3B2B2
          B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C4B2B2B2C4B2B2B2C4B2B2B2C3B6B6B6C28686868C00000001000000000000
          0000000000000000000000000000000000000000000100000003B6B6B6C28888
          A5D419168FFF8886A3D4B7B7B7C200000004B8B8B8C244352CFF35271FFF3526
          1FFF34251FFF34241FFF33241FFF32241EFF32241EFF32231EFF32231EFF3123
          1EFF31221DFF30221DFF30221DFF3F2F29FFB7B7B7C200000003000000000000
          0000000000000000000000000000000000000000000100000002B7B7B7C2ACAC
          ACC51E1C96FFAFAFAFC42D2D2D3400000003B8B8B8C254433AFF54433AFF5443
          3AFF54433AFF544239FF534139FF534139FF524039FF514038FF504038FF503F
          38FF503F38FF4F3F38FF4F3E36FF3F312AFFB7B7B7C200000003000000000000
          0000000000000000000000000000000000000000000100000003B8B8B8C22627
          A1FF23239EFFB4B4B4C300000004000000028B8B8B90B8B8B8C2B5B5B5C3B4B4
          B4C3B4B4B4C3B4B4B4C3B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3
          B3C3B3B3B3C3B3B3B3C3B4B4B4C3B7B7B7C28B8B8B9000000001000000000000
          00000000000000000000000000000000000000000000000000018E8E8E94B5B5
          B5C3B2B2B2C3B8B8B8C200000003000000020000000100000003000000030000
          0004000000040000000400000004000000040000000400000004000000040000
          0004000000040000000400000004000000030000000100000001000000000000
          000000000000000000000000000000000000000000000000000100000003B9B9
          B9C12D2FABFFBABABAC100000002000000010000000000000000000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          0000000000000000000000000001000000020000000300000003000000048E8E
          8E93BBBBBBC18F8F8F9300000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000014F4F4F52BABABAC1B8B8B8C2B7B7B7C2B8B8B8C18E8E
          8E93000000030000000200000002000000030000000300000003000000030000
          0003000000030000000300000003000000030000000300000003000000030000
          0003000000030000000100000001000000000000000000000000000000000000
          00000000000000000002BBBBBBC17371A3DC17128BFF5A579BE5140F88FFB9B9
          B9C1000000038787878BB9B9B9C1B7B7B7C2B6B6B6C2B6B6B6C2B6B6B6C2B5B5
          B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5
          B5C2B8B8B8C18787878B00000001000000000000000000000000000000000000
          00000000000000000002B9B9B9C11D1B95FFADADADC519168FFF8E8DAAD2BABA
          BAC100000004BABABAC14D3C32FF3D2D23FF3D2D23FF3D2D23FF3D2D23FF3C2C
          23FF3C2B22FF3B2B22FF3B2B22FF3A2B22FF3A2A22FF392A21FF392921FF3829
          21FF372921FFB8B8B8C100000002000000000000000000000000000000000000
          00000000000000000002BBBBBBC17676AADC21209AFF1E1C96FFB5B5B5C22D2D
          2D3200000003BABABAC15F4C40FF5F4B3FFF5F4B3FFF5E4A3FFF5D4A3FFF5D4A
          3FFF5D493FFF5C493EFF5B483DFF5B483DFF5B483DFF5A483DFF5A473DFF5947
          3CFF48392FFFB9B9B9C100000002000000000000000000000000000000000000
          000000000000000000018B8B8B90B7B7B7C2B2B2B2C323239EFFB8B8B8C10000
          0003000000018C8C8C8FBABABAC1B8B8B8C2B7B7B7C2B7B7B7C2B7B7B7C2B7B7
          B7C2B7B7B7C2B7B7B7C2B7B7B7C2B6B6B6C2B6B6B6C2B6B6B6C2B6B6B6C2B7B7
          B7C2B9B9B9C18C8C8C9000000001000000000000000000000000000000000000
          000000000000000000018C8C8C905052ADEF2B2CA7FF7B7CAFDBBBBBBBC10000
          0002000000000000000100000002000000020000000200000002000000020000
          0003000000030000000300000003000000030000000300000003000000030000
          0003000000020000000100000000000000000000000000000000000000000000
          000000000000000000018D8D8D8FBBBBBBC0BBBBBBC1BBBBBBC04F4F4F510000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000200000002000000030000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000190909092BBBBBBC0BBBBBBC1BBBBBBC08F8F8F9200000001000000010000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0001BDBDBDC01D1B95FF1B1892FF19168FFFBCBCBCC0000000028888888ABBBB
          BBC0BABABAC1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8
          B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B9B9B9C1BBBBBBC18787878A0000
          0001000000000000000000000000000000000000000000000000000000000000
          0001BDBDBDC09595B3D021209AFF9494B1D08F8F8F9200000002BCBCBCC05341
          36FF443227FF443227FF443227FF433227FF433227FF423127FF423127FF4231
          27FF413027FF413027FF413027FF413026FF412F26FF412F26FFBBBBBBC10000
          0001000000000000000000000000000000000000000000000000000000000000
          0001BDBDBDC0BABABAC12627A1FFBBBBBBC12222222400000001BCBCBCC06652
          45FF665145FF655145FF655144FF655045FF655043FF655043FF645043FF6450
          43FF634F43FF634F43FF624F43FF624E43FF624E42FF503E34FFBBBBBBC00000
          0001000000000000000000000000000000000000000000000000000000000000
          0001BDBDBDC02C2FAAFF2B2CA7FFBCBCBCC000000001000000008D8D8D8FBCBC
          BCC0BBBBBBC1BBBBBBC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABA
          BAC1BABABAC1BABABAC1B9B9B9C1B9B9B9C1BABABAC1BBBBBBC08C8C8C8F0000
          0001000000000000000000000000000000000000000000000000000000000000
          0000BEBEBEC09A9BB8CF2E31ACFFBDBDBDC00000000100000000000000000000
          0001000000010000000100000001000000010000000200000002000000020000
          0002000000020000000200000002000000020000000200000001000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          000022222223BEBEBEBFBEBEBEC0909090910000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000010000000100000003000000040000000400000004000000040000
          0004000000040000000400000004000000040000000400000004000000050000
          0005000000050000000500000005000000050000000500000005000000050000
          0005000000050000000500000005000000040000000200000001000000000000
          0000000000018686868BB7B7B7C2B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1
          B1C4B0B0B0C4B0B0B0C4B2B2B2C4B5B5B5C28686868C00000002000000000000
          000000000003B8B8B8C241312BFF31231DFF30221CFF30221CFF2F211CFF2F21
          1CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F1BFF2D1F1BFF2D1F
          1AFF2C1F1AFF2C1F1AFF2B1F1AFF2B1F1AFF2B1E19FF2A1E19FF2A1E19FF2A1E
          19FF291D19FF291D19FF281C19FF271C19FFB6B6B6C200000003000000000000
          000000000002B8B8B8C2503F38FF503F38FF4F3F38FF4F3E36FF4E3D36FF4E3D
          35FF4E3D35FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4C3B34FF4B3A34FF4B3A
          33FF4A3A33FF4A3933FF4A3933FF493932FF493932FF483932FF483832FF4838
          31FF473831FF473830FF463630FF362A25FFB6B6B6C200000003000000000000
          0000000000028B8B8B91B5B5B5C3B2B2B2C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAF
          AFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAE
          AEC5AEAEAEC5AEAEAEC5AFAFAFC4B3B3B3C38A8A8A9100000002000000000000
          0000000000028686868CB5B5B5C2B2B2B2C3B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4B0B0B0C4B0B0B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAF
          AFC5AEAEAEC5AFAFAFC5AFAFAFC4B3B3B3C38585858C00000002000000000000
          000000000002B8B8B8C144342CFF35261FFF34251FFF33241FFF33241EFF3224
          1EFF32231EFF32231EFF32231EFF31231EFF30221EFF30221CFF30221CFF2F21
          1CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D1F1CFF2D1F1CFF2D1F
          1AFF2D1F1AFF2D1F1AFF2B1F1AFF2B1F1AFFB7B7B7C200000003000000000000
          000000000002B8B8B8C154433AFF54433AFF544239FF544239FF534139FF5240
          39FF514039FF513F38FF504038FF503F38FF503F38FF4F3F38FF4F3D36FF4E3D
          36FF4E3D35FF4E3D34FF4D3C34FF4D3C34FF4D3C34FF4D3B34FF4C3B34FF4B3B
          34FF4B3A33FF4A3A33FF4A3933FF3C2C26FFB7B7B7C200000003000000000000
          0000000000018C8C8C90B8B8B8C1B6B6B6C2B4B4B4C3B3B3B3C3B2B2B2C4B1B1
          B1C4B3B3B3C3B4B4B4C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4
          B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C3B3B3B3C3B4B4B4C3B6B6B6C28B8B8B9000000001000000000000
          000000000001000000010000000400000004000000070000000D000000140000
          00140000000C0000000500000004000000030000000300000004000000050000
          0006000000070000000800000008000000080000000800000008000000080000
          0008000000080000000800000008000000060000000300000001000000000000
          0000000000000000000100000002000000070000000F05030124573019AE9657
          2FFF0000001400000007000000020000000200000001000000028787878BB8B8
          B8C2B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B3B3B3C3B3B3
          B3C3B3B3B3C3B4B4B4C3B5B5B5C3B7B7B7C28686868B00000001000000000000
          000000000001000000020000000700000010170D0744784425DEBB8859FF9858
          31FF0000001B0000000C00000007000000040000000200000003B8B8B8C14233
          2CFF33241FFF32241EFF32241EFF32231EFF32231EFF31231EFF31231EFF3023
          1DFF30221CFF30221CFF2F211CFF2F211CFFB8B8B8C200000002000000000000
          0001000000030000000800000012351D0F74955D38F9D0A572FFDCB47EFF9959
          33FF000000270000001B000000160000000F0000000700000004B9B9B9C15442
          39FF534139FF534139FF524039FF524039FF513F38FF503F38FF503F38FF503F
          38FF4F3E36FF4E3D36FF4E3D35FF3F3029FFB8B8B8C100000002000000010000
          0004000000090503011B58301BABAD7B52FFDDBA87FFDEB57FFFDFBB88FF9B5C
          35FF9A5B35FF9A5B34FF995B33FF6E4124C00000000E000000048B8B8B91B7B7
          B7C2B4B4B4C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C3B3B3B3C3B6B6B6C28B8B8B9100000002000000030000
          0008160C07377A4628DBC59E71FFE4C492FFE1B986FFE0B885FFE4C491FFE2C2
          90FFE2C18EFFE1BF8CFFE0BE8AFF9B5D35FF00000012000000068686868CB7B7
          B7C2B5B5B5C3B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2
          B2C3B2B2B2C3B2B2B2C3B3B3B3C3B6B6B6C28686868B0000000200000004321C
          1062966340F6DDC69BFFE8CC9DFFE9C99AFFEACD9DFFEACD9DFFE9CC9BFFE6C5
          93FFE3BD89FFE1BB87FFE5C694FF9E6039FF0000001200000007BABABAC14636
          2FFF372720FF37271FFF37271FFF35271FFF35271FFF35261FFF35261FFF3425
          1FFF33251FFF32241FFF32241EFF32231EFFB9B9B9C100000002000000043C28
          1961AF8058F6E3D2A9FFF2E7C0FFF0DCB1FFEED6AAFFEDD5A7FFEDD4A6FFECD2
          A5FFECD0A2FFE6C493FFE7CE9EFFA0653DFF0000001000000006BABABAC15846
          3CFF58453CFF57453CFF57443BFF56443AFF56433AFF54433AFF544339FF5443
          39FF544139FF544139FF524139FF42322CFFB9B9B9C100000002000000020000
          000519110B2D936A45D5D9C29CFFF4EECBFFF2E5BDFFF0DDB2FFF3E6BFFFF3E6
          BEFFF4EDC8FFF4EBC6FFEEE1B7FFA36941FF0000000C000000048C8C8C90B8B8
          B8C1B6B6B6C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B5B5B5C3B7B7B7C28B8B8B9000000001000000000000
          000100000004060503116D4F369FCDAB83FFF4EECCFFF5EBC7FFF5EDCAFFB07A
          53FFB07B53FFB07A51FFB07951FF825A3CC100000007000000038787878BB8B8
          B8C1B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B5B5
          B5C3B5B5B5C3B5B5B5C3B5B5B5C2B8B8B8C28686868B00000001000000000000
          000000000001000000020000000541302060BB936BF6EDE4C2FFF8F5D7FFB47F
          58FF0000000E0000000A00000008000000060000000300000002BBBBBBC14A3A
          30FF3B2B22FF3B2B22FF3A2B22FF392A21FF392A21FF382921FF382921FF3728
          20FF372720FF372720FF372720FF35271FFFBABABAC100000002000000000000
          0000000000000000000000000001000000031D160F2DA17A54D8E1CEABFFB784
          5CFF000000070000000300000002000000010000000100000001BBBBBBC15D4A
          3EFF5B493EFF5B483EFF5B483DFF5A483DFF5A473DFF5A473CFF59473CFF5946
          3CFF57463CFF57453CFF57443BFF46352DFFBABABAC100000002000000000000
          000000000000000000000000000000000001000000020705040D76593DA1B988
          60FF0000000300000001000000000000000000000000000000018C8C8C8FBBBB
          BBC1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C2B8B8B8C2B8B8B8C2B8B8
          B8C2B8B8B8C2B8B8B8C2B8B8B8C1BABABAC18C8C8C8F00000001000000000000
          0000000000000000000100000001000000020000000200000003000000040000
          0004000000030000000200000002000000020000000200000002000000030000
          0004000000040000000400000004000000040000000500000005000000050000
          0005000000050000000500000005000000030000000200000000000000000000
          0000000000008888888ABCBCBCC0BBBBBBC1BBBBBBC1BBBBBBC1BABABAC1BABA
          BAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C1B9B9B9C1BABABAC18787878A00000001000000000000
          000000000001BCBCBCC0524135FF433227FF423127FF433127FF423127FF4130
          27FF413027FF413026FF413026FF412F26FF412F26FF412F25FF402F25FF3F2F
          24FF3F2F24FF3E2F24FF3E2D24FF3E2D24FF3D2D23FF3D2D23FF3D2D23FF3C2D
          23FF3C2C23FF3B2B22FF3B2B22FF3B2B22FFBBBBBBC100000001000000000000
          000000000001BDBDBDC0655144FF655144FF655043FF645043FF645043FF634F
          43FF634F43FF634F43FF624E43FF624E43FF624E43FF624E42FF624D42FF624D
          42FF614D40FF614D41FF604C40FF604C40FF5F4C40FF5F4B3FFF5F4A3FFF5E4A
          3FFF5D4A3FFF5D4A3FFF5C493EFF4A3B30FFBBBBBBC000000001000000000000
          0000000000018D8D8D8FBCBCBCC0BBBBBBC1BBBBBBC1BABABAC1BABABAC1BABA
          BAC1BABABAC1BABABAC1BABABAC1BABABAC1B9B9B9C1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C1B8B8B8C1BABABAC18C8C8C8F00000001000000000000
          0000000000008888888ABCBCBCC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BBBBBBC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABA
          BAC1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8
          B8C1B8B8B8C1B8B8B8C1B9B9B9C1BBBBBBC18787878A00000001000000000000
          000000000001BEBEBEC0544338FF45342AFF453329FF453329FF453328FF4532
          28FF443227FF443227FF443227FF433227FF433227FF433227FF423127FF4131
          27FF413127FF413027FF413027FF413026FF412F26FF412F25FF412F25FF402F
          24FF3F2F24FF3F2F24FF3E2E24FF3E2D24FFBCBCBCC000000001000000000000
          000000000001BEBEBEC0675447FF675447FF675346FF675347FF675346FF6652
          46FF665246FF665245FF655144FF655044FF655044FF655044FF645044FF6350
          43FF644F43FF634F43FF624E43FF624E43FF624E43FF624E43FF624D42FF624D
          41FF614D42FF604D41FF604D40FF4E3E32FFBCBCBCC000000001000000000000
          0000000000008D8D8D8EBEBEBEC0BDBDBDC0BDBDBDC0BDBDBDC0BCBCBCC0BCBC
          BCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BBBB
          BBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBB
          BBC0BBBBBBC0BBBBBBC1BBBBBBC0BCBCBCC08D8D8D8F00000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000010000000100000003000000040000000400000004000000040000
          0004000000040000000400000004000000040000000400000004000000050000
          0005000000050000000500000005000000050000000500000005000000050000
          0005000000050000000500000005000000040000000200000001000000000000
          0000000000018686868BB7B7B7C2B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1
          B1C4B0B0B0C4B0B0B0C4B2B2B2C4B5B5B5C28686868C00000002000000000000
          000000000003B8B8B8C241312BFF31231DFF30221CFF30221CFF2F211CFF2F21
          1CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F1BFF2D1F1BFF2D1F
          1AFF2C1F1AFF2C1F1AFF2B1F1AFF2B1F1AFF2B1E19FF2A1E19FF2A1E19FF2A1E
          19FF291D19FF291D19FF281C19FF271C19FFB6B6B6C200000003000000000000
          000000000002B8B8B8C2503F38FF503F38FF4F3F38FF4F3E36FF4E3D36FF4E3D
          35FF4E3D35FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4C3B34FF4B3A34FF4B3A
          33FF4A3A33FF4A3933FF4A3933FF493932FF493932FF483932FF483832FF4838
          31FF473831FF473830FF463630FF362A25FFB6B6B6C200000003000000000000
          0000000000028B8B8B91B5B5B5C3B2B2B2C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAF
          AFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAE
          AEC5AEAEAEC5AEAEAEC5AFAFAFC4B3B3B3C38A8A8A9100000002000000000000
          0000000000028686868CB5B5B5C2B2B2B2C3B1B1B1C4B0B0B0C4B0B0B0C4B0B0
          B0C4B0B0B0C4B0B0B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAF
          AFC5AEAEAEC5AFAFAFC5AFAFAFC4B3B3B3C38585858C00000002000000000000
          000000000002B8B8B8C144342CFF35261FFF34251FFF33241FFF33241EFF3224
          1EFF32231EFF32231EFF32231EFF31231EFF30221EFF30221CFF30221CFF2F21
          1CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D1F1CFF2D1F1CFF2D1F
          1AFF2D1F1AFF2D1F1AFF2B1F1AFF2B1F1AFFB7B7B7C200000003000000000000
          000000000002B8B8B8C154433AFF54433AFF544239FF544239FF534139FF5240
          39FF514039FF513F38FF504038FF503F38FF503F38FF4F3F38FF4F3D36FF4E3D
          36FF4E3D35FF4E3D34FF4D3C34FF4D3C34FF4D3C34FF4D3B34FF4C3B34FF4B3B
          34FF4B3A33FF4A3A33FF4A3933FF3C2C26FFB7B7B7C200000003000000000000
          0000000000018C8C8C90B8B8B8C2B5B5B5C3B1B1B1C4B2B2B2C4B3B3B3C3B4B4
          B4C3B5B5B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4
          B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C3B3B3B3C3B4B4B4C3B6B6B6C28B8B8B9000000001000000000000
          00000000000100000002000000050000000B00000013000000130000000D0000
          0007000000040000000400000003000000030000000300000004000000050000
          0006000000070000000800000008000000080000000800000008000000080000
          0008000000080000000800000008000000060000000300000001000000000000
          0000000000010000000100000005000000129C5C34FF4E2814AE050201240000
          000F0000000700000003000000020000000100000001000000028787878BB8B8
          B8C2B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B3B3B3C3B3B3
          B3C3B3B3B3C3B4B4B4C3B5B5B5C3B7B7B7C28686868B00000001000000010000
          000200000003000000060000000A000000199D5E36FFB8895CFF6F3B1FDE150B
          0645000000110000000800000003000000010000000000000002B8B8B8C14233
          2CFF33241FFF32241EFF32241EFF32231EFF32231EFF31231EFF31231EFF3023
          1DFF30221CFF30221CFF2F211CFF2F211CFFB8B8B8C200000002000000010000
          00050000000C0000001200000018000000249E6038FFDEBB86FFCDA16EFF8E54
          32F931190D76000000140000000A000000040000000100000003B9B9B9C15442
          39FF534139FF534139FF524039FF524039FF513F38FF503F38FF503F38FF503F
          38FF4F3E36FF4E3D36FF4E3D35FF3F3029FFB8B8B8C100000002000000030000
          000B764B30BEA1643DFFA0633CFFA0623BFF9F623AFFE0BC88FFDCB37EFFDBB3
          7FFFA87149FF532B18AD0502011F0000000C00000005000000028B8B8B91B7B7
          B7C2B4B4B4C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C3B3B3B3C3B6B6B6C28B8B8B9100000002000000030000
          000DA86F47FFE9D0A1FFE5C796FFE5C694FFE4C392FFE3C291FFE0B782FFDEB6
          81FFE0BB87FFBF9265FF754225DC150B063B0000000B000000058686868CB7B7
          B7C2B5B5B5C3B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2
          B2C3B2B2B2C3B2B2B2C3B3B3B3C3B6B6B6C28686868B00000002000000030000
          000DAD744EFFECD8ABFFE9CA9BFFEBCE9FFFEACE9EFFEACC9CFFE7C795FFE3BE
          8BFFE2BB87FFE2BE8BFFD5AF7FFF935C39F7311B0F6500000008B9B9B9C14636
          2FFF372720FF37271FFF37271FFF35271FFF35271FFF35261FFF35261FFF3425
          1FFF33251FFF32241FFF32241EFF32231EFFB9B9B9C100000002000000030000
          000BB27D54FFF2E6C0FFEFD7ACFFEED7AAFFEED6AAFFEDD5A7FFEDD4A6FFECD1
          A4FFE7C594FFE5C393FFD9B889FFA57048F63923156400000007B9B9B9C15846
          3CFF58453CFF57453CFF57443BFF56443AFF56433AFF54433AFF544339FF5443
          39FF544139FF544139FF524139FF42322CFFB9B9B9C100000002000000020000
          0008B9875FFFF4ECC8FFF4EBC7FFF4EAC5FFF3E9C3FFF3E8C2FFF0DAAFFFF0DA
          AEFFF0DDB3FFD0AA80FF895A38D6170F093200000008000000048C8C8C90B8B8
          B8C1B6B6B6C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B5B5B5C3B7B7B7C28B8B8B9000000001000000010000
          00058C694BC0BD8D65FFBC8B64FFBB8A63FFBA8961FFF5EDC9FFF2E1BAFFF0E1
          BBFFC59E77FF66432BA0060402150000000600000002000000028787878BB8B8
          B8C1B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B5B5
          B5C3B5B5B5C3B5B5B5C3B5B5B5C2B8B8B8C28686868B00000001000000000000
          00020000000400000006000000080000000CBE8F67FFF6F0CEFFEADAB6FFB286
          60F63E2A1C620000000700000003000000010000000000000001BBBBBBC14A3A
          30FF3B2B22FF3B2B22FF3A2B22FF392A21FF392A21FF382921FF382921FF3728
          20FF372720FF372720FF372720FF35271FFFBABABAC100000002000000000000
          000000000001000000010000000200000006C1946CFFDFC6A3FF986D4BD81B13
          0C2E000000050000000200000001000000000000000000000001BBBBBBC15D4A
          3EFF5B493EFF5B483EFF5B483DFF5A483DFF5A473DFF5A473CFF59473CFF5946
          3CFF57463CFF57453CFF57443BFF46352DFFBABABAC100000002000000000000
          000000000000000000000000000100000002C49970FF715036A10605030D0000
          00020000000100000000000000000000000000000000000000018C8C8C8FBBBB
          BBC1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C2B8B8B8C2B8B8B8C2B8B8
          B8C2B8B8B8C2B8B8B8C2B8B8B8C1BABABAC18C8C8C8F00000001000000000000
          0000000000000000000100000001000000030000000300000004000000030000
          0002000000020000000200000002000000020000000200000002000000030000
          0004000000040000000400000004000000040000000500000005000000050000
          0005000000050000000500000005000000030000000200000000000000000000
          0000000000008888888ABCBCBCC0BBBBBBC1BBBBBBC1BBBBBBC1BABABAC1BABA
          BAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C1B9B9B9C1BABABAC18787878A00000001000000000000
          000000000001BCBCBCC0524135FF433227FF423127FF433127FF423127FF4130
          27FF413027FF413026FF413026FF412F26FF412F26FF412F25FF402F25FF3F2F
          24FF3F2F24FF3E2F24FF3E2D24FF3E2D24FF3D2D23FF3D2D23FF3D2D23FF3C2D
          23FF3C2C23FF3B2B22FF3B2B22FF3B2B22FFBBBBBBC100000001000000000000
          000000000001BDBDBDC0655144FF655144FF655043FF645043FF645043FF634F
          43FF634F43FF634F43FF624E43FF624E43FF624E43FF624E42FF624D42FF624D
          42FF614D40FF614D41FF604C40FF604C40FF5F4C40FF5F4B3FFF5F4A3FFF5E4A
          3FFF5D4A3FFF5D4A3FFF5C493EFF4A3B30FFBBBBBBC000000001000000000000
          0000000000018D8D8D8FBCBCBCC0BBBBBBC1BBBBBBC1BABABAC1BABABAC1BABA
          BAC1BABABAC1BABABAC1BABABAC1BABABAC1B9B9B9C1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C1B8B8B8C1BABABAC18C8C8C8F00000001000000000000
          0000000000008888888ABCBCBCC0BBBBBBC0BBBBBBC1BBBBBBC1BBBBBBC1BBBB
          BBC1BBBBBBC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABABAC1BABA
          BAC1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8
          B8C1B8B8B8C1B8B8B8C1B9B9B9C1BBBBBBC18787878A00000001000000000000
          000000000001BEBEBEC0544338FF45342AFF453329FF453329FF453328FF4532
          28FF443227FF443227FF443227FF433227FF433227FF433227FF423127FF4131
          27FF413127FF413027FF413027FF413026FF412F26FF412F25FF412F25FF402F
          24FF3F2F24FF3F2F24FF3E2E24FF3E2D24FFBCBCBCC000000001000000000000
          000000000001BEBEBEC0675447FF675447FF675346FF675347FF675346FF6652
          46FF665246FF665245FF655144FF655044FF655044FF655044FF645044FF6350
          43FF644F43FF634F43FF624E43FF624E43FF624E43FF624E43FF624D42FF624D
          41FF614D42FF604D41FF604D40FF4E3E32FFBCBCBCC000000001000000000000
          0000000000008D8D8D8EBEBEBEC0BDBDBDC0BDBDBDC0BDBDBDC0BCBCBCC0BCBC
          BCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BCBCBCC0BBBB
          BBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBBBBC0BBBB
          BBC0BBBBBBC0BBBBBBC1BBBBBBC0BCBCBCC08D8D8D8F00000000000000000000
          0000000000000000000000000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000020000000400000005000000060000000600000005000000040000
          0002000000020000000400000006000000060000000600000006000000050000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0002BBBBBBC1B5B5B5C3B1B1B1C4AFAFAFC4AFAFAFC5B0B0B0C4B5B5B5C3BABA
          BAC1BABABAC1B4B4B4C3AFAFAFC4AEAEAEC5ADADADC5AFAFAFC5B3B3B3C3BABA
          BAC1000000020000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0003B8B8B8C233211BFF31211AFF31201AFF31201AFF301F19FF301F18FFB5B5
          B5C3B5B5B5C32E1D17FF2E1D17FF2D1D17FF2C1D16FF2C1D16FF2C1C16FFB6B6
          B6C2000000030000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0002B8B8B8C1979290D4533B31FF523A31FF523A30FF523A30FF8D8988D4B6B6
          B6C2B6B6B6C2948F8DD44F382EFF4F372EFF4E372EFF4E372DFF8A8785D4B7B7
          B7C2000000020000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00012D2D2D34AFAFAFC5553D32FF543C32FF543C32FF3A2820FFAEAEAEC52D2D
          2D352D2D2D35ADADADC551392FFF51382FFF51382EFF36241DFFACACACC62D2D
          2D35000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000005B1B1B1C4563E34FF563D34FF563D33FF35241CFFB0B0B0C40000
          000600000006AFAFAFC4533B30FF523A30FF513A30FF301F19FFAEAEAEC50000
          0006000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000004B2B2B2C3584035FF573F35FF573E34FF38251EFFB2B2B2C40000
          000500000005B1B1B1C4543B32FF543C32FF533B31FF32211BFFAFAFAFC40000
          0005000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000004B3B3B3C35A4237FF5A4037FF594036FF3A2721FFB2B2B2C30000
          000500000005B1B1B1C4553D33FF563D33FF553D33FF34221CFFAFAFAFC40000
          0005000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000004B3B3B3C35B4338FF5B4239FF5B4238FF3C2922FFB2B2B2C30000
          000500000005B2B2B2C4573F35FF573E34FF573E34FF36241EFFB0B0B0C40000
          0005000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000004B4B4B4C35E453AFF5D443AFF5D4439FF3F2B24FFB3B3B3C30000
          000400000005B2B2B2C3594137FF594036FF583F35FF38261FFFB1B1B1C40000
          0005000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000002000000020000
          000300000007B3B3B3C360463DFF5F463BFF5F453BFF422D25FFB4B4B4C30000
          000400000004B3B3B3C35B4238FF5B4238FF5A4137FF3A2821FFB1B1B1C40000
          0005000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000011919191B5757575C7C7C7C8387878790B7B7
          B7C2B5B5B5C3AFAFAFC462483EFF61473DFF60473DFF432F28FFB5B5B5C30000
          000400000004B4B4B4C35D4439FF5C443AFF5C4339FF3C2A22FFB2B2B2C40000
          0005000000000000000000000000000000000000000000000000000000000000
          000000000000000000015858585CACACACB4A7A4A3CA847A77E06E5F59EB6758
          52F04B352EFF644B40FF634A3FFF63493FFF62493FFF47322AFFB5B5B5C20000
          000400000004B4B4B4C35F453BFF5F453BFF5E453AFF3F2B25FFB2B2B2C30000
          0004000000000000000000000000000000000000000000000000000000000000
          00000000000164646468B8B8B8C1877C77DF624B42FB61493EFF62483EFF674D
          43FF674D43FF756057F4654C41FF654C41FF644A41FF48332CFFB5B5B5C20000
          000400000004B5B5B5C361483DFF60473CFF60473CFF412E27FFB3B3B3C30000
          0004000000000000000000000000000000000000000000000000000000000000
          000126262628B9B9B9C1867974E36A5045FF6A5145FF6A5045FF694F45FF8170
          68EBA8A5A3CBB2B2B2C4674E43FF674D43FF664C42FF4C362DFFB6B6B6C20000
          000300000003B5B5B5C263493FFF62493FFF62483EFF453027FFB3B3B3C30000
          0004000000000000000000000000000000000000000000000000000000000000
          000164646468A6A19ECE6D5349FF6D5348FF6C5247FF6C5247FF735F57F3B7B7
          B7C27B7B7B83B6B6B6C2694F44FF684E44FF684E44FF4E392FFFB7B7B7C20000
          000300000003B6B6B6C2644B40FF654A40FF634A40FF46322BFFB4B4B4C30000
          0004000000000000000000000000000000000000000000000000000000000000
          00029595959B8F817CE36F554AFF6F544AFF6E5449FF694E44FF908683DB9595
          959B00000004B8B8B8C26B5146FF6A5046FF6A5045FF523A31FFB8B8B8C20000
          000300000003B6B6B6C2674D43FF664D42FF654C42FF4A342DFFB5B5B5C30000
          0004000000000000000000000000000000000000000000000000000000000000
          0002B9B9B9C17E695FF371564BFF70564BFF6F554AFF624941FFACA8A7CA4B4B
          4B4F00000003B9B9B9C16C5348FF6C5247FF6B5147FF543C34FFB8B8B8C20000
          000300000003B7B7B7C2684F44FF684F43FF674E43FF4C362FFFB5B5B5C30000
          0004000000000000000000000000000000000000000000000000000000000000
          0002B9B9B9C173574DFF72574DFF72574CFF72574CFF61473DFFB9B9B9C11919
          191B00000002B9B9B9C16E5449FF6E5349FF6E5349FF563F36FFB8B8B8C10000
          000300000003B7B7B7C26B5145FF6A5045FF6A4F44FF4E3930FFB5B5B5C20000
          0003000000000000000000000000000000000000000000000000000000000000
          0002BABABAC174594EFF74584EFF73594DFF72584DFF60483EFFBABABAC10D0D
          0D0F00000002BABABAC170554BFF70554BFF6F544AFF5A4137FFB9B9B9C10000
          000200000003B8B8B8C26C5247FF6B5147FF6B5146FF523B33FFB5B5B5C20000
          0003000000000000000000000000000000000000000000000000000000000000
          0002BBBBBBC17E675EF7755A4FFF745A4FFF74594EFF624941FFB4B2B2C53F3F
          3F4100000002BBBBBBC171574CFF71574CFF71564BFF5B433BFFB9B9B9C10000
          000200000002B8B8B8C16D5449FF6D5349FF6C5248FF553C35FFB6B6B6C20000
          0003000000000000000000000000000000000000000000000000000000000000
          0001A3A3A3A791817AE7765B50FF755B50FF755A50FF674D42FF9E9591D68A8A
          8A8D00000002BBBBBBC173594DFF72584DFF72584DFF5F463DFFBABABAC10000
          000200000002B8B8B8C16F554AFF6F554AFF6E5549FF564037FFB6B6B6C20000
          0003000000010000000000000000000000000000000000000000000000000000
          000172727275A9A19ED2775C51FF775C51FF765C50FF6B5147FF7C6962EEBBBB
          BBC07D7D7D81BBBBBBC1745A4EFF74594FFF74594EFF62473EFFB9B9B9C10000
          000400000004B8B8B8C271564CFF70564CFF70554BFF5A4137FFB5B5B5C21919
          191E000000020000000100000000000000000000000000000000000000000000
          000033333334BCBCBCC092827BE6775D51FF785C51FF74594FFF684E44FF8271
          6AEAB6B3B3C5BABABAC1755B50FF755B50FF745A4FFF634940FFB8B8B8C2B9B9
          B9C1B9B9B9C1B5B5B5C272574DFF72584DFF72574DFF5C453BFFA6A3A2CB9494
          949D5757575C4B4B4B4D00000001000000000000000000000000000000000000
          00000000000073737374BDBDBDC0988983E2785D52FF785D51FF74584DFF6A50
          46FF684E44FF846F66F3775B51FF765C50FF765B50FF654B42FF654B41FF6349
          40FF634940FF74594FFF73594EFF74594EFF72584EFF62483FFF6E5B52F38B80
          7BDF938986DA8989898E00000001000000000000000000000000000000000000
          0000000000000000000066666667BEBEBEC0B4B0AFC89C908BDD8A766EEE856F
          66F2755B50FF785D52FF785C51FF775C51FF775C51FF775C51FF765B50FF765B
          50FF765B50FF755B50FF755A50FF755A4FFF745A4FFF71564CFF695045FF694F
          43FF76655DEF8989898E00000001000000000000000000000000000000000000
          00000000000000000000000000001919191A5959595A8B8B8B8C9898989ABEBE
          BEC0BEBEBEC0BDBDBDC0BDBDBDC0BDBDBDC0BCBCBCC0BCBCBCC0BBBBBBC0BBBB
          BBC0BBBBBBC1BBBBBBC1BABABAC1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1BBBBBBC18A8A8A8D00000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000200000002000000020000000200000002000000030000
          0002000000020000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000040000000500000005000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000040000000200000001000000000000000000000000000000000000
          000000000000000000028686868CB5B5B5C3B1B1B1C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAF
          AFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5ADADADC5AFAF
          AFC5B3B3B3C38585858D00000002000000000000000000000000000000000000
          00000000000000000003B6B6B6C244352CFF35261FFF34251FFF33251FFF3224
          1EFF32241EFF32231EFF32231EFF32231EFF31231EFF31231DFF30221DFF3022
          1CFF30211CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F
          1BFF2D1F1BFFB4B4B4C300000003000000000000000000000000000000000000
          00000000000000000003B6B6B6C254433AFF544339FF544339FF544239FF5341
          39FF534039FF524039FF514038FF504038FF503F38FF503F38FF4F3E38FF4F3D
          36FF4E3D36FF4E3D36FF4E3C34FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4D3B
          34FF3C2C29FFB5B5B5C300000003000000000000000000000000000000000000
          000000000000000000028A8A8A91B3B3B3C3AFAFAFC4AEAEAEC5AEAEAEC5ADAD
          ADC5ADADADC5ADADADC5ADADADC5ACACACC5ACACACC5ACACACC5ACACACC5ACAC
          ACC6ACACACC6ACACACC6ACACACC6ACACACC6ACACACC5AEAEAEC5AFAFAFC5B0B0
          B0C4B4B4B4C38A8A8A9100000002000000000000000000000000000000000000
          000000000000000000028585858CB4B4B4C3AFAFAFC4AFAFAFC5AFAFAFC5AEAE
          AEC5AEAEAEC5AEAEAEC5AEAEAEC5ADADADC5ADADADC5ADADADC5ADADADC5ACAC
          ACC5ACACACC5ACACACC5ACACACC5ADADADC5B1B1B1C48383838F000000070000
          0005000000040000000200000001000000000000000000000000000000000000
          00000000000000000002B8B8B8C247382FFF382921FF372821FF372720FF3727
          20FF37271FFF37271FFF35271FFF35271FFF35261FFF34251FFF34251FFF3324
          1FFF33241FFF32231EFF32231EFF32231EFF32231EFFB5B5B5C2000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          00000000000000000002B8B8B8C25A473CFF59473CFF58463CFF58463CFF5745
          3CFF57443BFF56433BFF56433AFF54433AFF54433AFF54433AFF544239FF5441
          39FF534139FF524039FF524039FF514038FF41312BFFB5B5B5C2000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000028B8B8B91B5B5B5C2B2B2B2C4B0B0B0C4B0B0B0C4B0B0
          B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAF
          AFC5AFAFAFC5AFAFAFC5AEAEAEC5AFAFAFC4B2B2B2C388888893000000060000
          0005000000030000000200000001000000000000000000000000000000000000
          000000000000000000028686868CB6B6B6C2B2B2B2C3B1B1B1C4B1B1B1C4B1B1
          B1C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC4AFAFAFC4B1B1B1C4B2B2B2C4B2B2
          B2C3B5B5B5C28686868C00000002000000000000000000000000000000000000
          00000000000000000002B8B8B8C14C3B31FF3C2C23FF3B2C22FF3B2B22FF3B2B
          22FF3A2A21FF3A2A21FF3A2A21FF392A21FF382921FF382821FF372821FF3727
          20FF37271FFF37271FFF35271FFF35271FFF35271FFF35251FFF35251FFF3325
          1FFF33241FFFB7B7B7C200000003000000000000000000000000000000000000
          00000000000000000002B9B9B9C15D4A3FFF5D4A3FFF5D4A3FFF5C493EFF5C48
          3DFF5B483DFF5B483DFF5A483DFF5A473DFF59473CFF59463CFF58463CFF5746
          3CFF57453CFF57443BFF56443BFF56433AFF54433AFF544339FF544339FF5442
          39FF43332CFFB7B7B7C200000002000000000000000000000000000000000000
          000000000000000000018B8B8B90B7B7B7C2B4B4B4C3B3B3B3C3B3B3B3C3B2B2
          B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B2B2B2C4B2B2B2C3B3B3B3C3B4B4
          B4C3B7B7B7C28B8B8B9000000001000000000000000000000000000000000000
          000000000000000000018686868BB8B8B8C2B5B5B5C3B4B4B4C3B3B3B3C3B3B3
          B3C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C3B5B5B5C38484848D000000050000
          0004000000030000000100000000000000000000000000000000000000000000
          00000000000000000002BABABAC14F3E32FF3F2E24FF3E2E24FF3E2E24FF3E2E
          23FF3E2D23FF3D2D23FF3D2D23FF3D2D23FF3D2D23FF3B2C23FF3B2B23FF3B2B
          22FF3A2B22FF3A2A22FF3A2A21FF392A21FF392921FFB8B8B8C1000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000001BBBBBBC1624D41FF614D41FF604D41FF604D40FF604D
          40FF5F4C40FF5F4B3FFF5F4B3FFF5E4A3FFF5D4A3FFF5D4A3EFF5C4A3EFF5C49
          3EFF5C483DFF5B483DFF5B483DFF5B483CFF49392FFFB8B8B8C1000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000018C8C8C90B9B9B9C1B6B6B6C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B3B3B3C3B4B4B4C3B6B6B6C28A8A8A91000000040000
          0003000000020000000100000000000000000000000000000000000000000000
          000000000000000000018787878BB9B9B9C1B7B7B7C2B6B6B6C2B6B6B6C2B6B6
          B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5
          B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B5B5B5C3B5B5B5C2B5B5B5C2B6B6
          B6C2B8B8B8C18787878B00000001000000000000000000000000000000000000
          00000000000000000001BBBBBBC0514035FF413027FF413027FF413026FF4130
          26FF412F26FF412F26FF412F25FF402F25FF3F2F24FF3F2F24FF3E2F24FF3E2D
          24FF3E2D24FF3D2D23FF3D2D23FF3D2D23FF3C2D23FF3C2C23FF3B2B22FF3B2B
          22FF3B2B22FFB9B9B9C100000002000000000000000000000000000000000000
          00000000000000000001BBBBBBC0645043FF634F43FF634F43FF634F43FF624E
          43FF624E43FF624E43FF624E42FF624D42FF624D42FF614D40FF614D41FF604C
          40FF604C40FF5F4C40FF5F4B3FFF5F4A3FFF5E4A3FFF5D4A3FFF5D4A3FFF5C49
          3EFF4A3B30FFBABABAC100000002000000000000000000000000000000000000
          000000000000000000018D8D8D8FBBBBBBC0BABABAC1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8
          B8C2BABABAC18C8C8C8F00000001000000000000000000000000000000000000
          0000000000000000000000000001000000010000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000030000
          0002000000020000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000040000000500000005000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000040000000200000001000000000000000000000000000000000000
          000000000000000000028686868CB5B5B5C3B1B1B1C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAF
          AFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5ADADADC5AFAF
          AFC5B3B3B3C38585858D00000002000000000000000000000000000000000000
          00000000000000000003B6B6B6C244352CFF35261FFF34251FFF33251FFF3224
          1EFF32241EFF32231EFF32231EFF32231EFF31231EFF31231DFF30221DFF3022
          1CFF30211CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F
          1BFF2D1F1BFFB4B4B4C300000003000000000000000000000000000000000000
          00000000000000000003B6B6B6C254433AFF544339FF544339FF544239FF5341
          39FF534039FF524039FF514038FF504038FF503F38FF503F38FF4F3E38FF4F3D
          36FF4E3D36FF4E3D36FF4E3C34FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4D3B
          34FF3C2C29FFB5B5B5C300000003000000000000000000000000000000000000
          000000000000000000028B8B8B91B6B6B6C2B2B2B2C3B1B1B1C4B0B0B0C4AFAF
          AFC5AEAEAEC5ADADADC5ADADADC5ACACACC5ACACACC5ACACACC5ACACACC5ACAC
          ACC6ACACACC6ACACACC6ACACACC6ACACACC6ACACACC5AEAEAEC5AFAFAFC5B0B0
          B0C4B4B4B4C38A8A8A9100000002000000000000000000000000000000000000
          00000000000000000000000000010000000300000005000000068484848EB2B2
          B2C3AFAFAFC4AEAEAEC5AEAEAEC5ADADADC5ADADADC5ADADADC5ADADADC5ACAC
          ACC5ACACACC5ACACACC5ACACACC5ADADADC5B1B1B1C48383838F000000070000
          0005000000040000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000003B7B7B7C24635
          2DFF37271FFF37271FFF35271FFF35271FFF35261FFF34251FFF34251FFF3324
          1FFF33241FFF32231EFF32231EFF32231EFF32231EFFB5B5B5C2000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000003B7B7B7C25745
          3CFF57443BFF56433BFF56433AFF54433AFF54433AFF54433AFF544239FF5441
          39FF534139FF524039FF524039FF514038FF41312BFFB5B5B5C2000000030000
          0001000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000100000003000000040000000589898992B4B4
          B4C3B1B1B1C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAF
          AFC5AFAFAFC5AFAFAFC5AEAEAEC5AFAFAFC4B2B2B2C388888893000000060000
          0005000000030000000200000001000000000000000000000000000000000000
          000000000000000000018686868BB8B8B8C2B5B5B5C2B4B4B4C3B3B3B3C3B2B2
          B2C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC4AFAFAFC4B1B1B1C4B2B2B2C4B2B2
          B2C3B5B5B5C28686868C00000002000000000000000000000000000000000000
          00000000000000000002B8B8B8C14C3B31FF3C2C23FF3B2C22FF3B2B22FF3B2B
          22FF3A2A21FF3A2A21FF3A2A21FF392A21FF382921FF382821FF372821FF3727
          20FF37271FFF37271FFF35271FFF35271FFF35271FFF35251FFF35251FFF3325
          1FFF33241FFFB7B7B7C200000003000000000000000000000000000000000000
          00000000000000000002B9B9B9C15D4A3FFF5D4A3FFF5D4A3FFF5C493EFF5C48
          3DFF5B483DFF5B483DFF5A483DFF5A473DFF59473CFF59463CFF58463CFF5746
          3CFF57453CFF57443BFF56443BFF56433AFF54433AFF544339FF544339FF5442
          39FF43332CFFB7B7B7C200000002000000000000000000000000000000000000
          000000000000000000018C8C8C90B8B8B8C1B6B6B6C2B5B5B5C2B5B5B5C3B4B4
          B4C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B2B2B2C4B2B2B2C3B3B3B3C3B4B4
          B4C3B7B7B7C28B8B8B9000000001000000000000000000000000000000000000
          00000000000000000000000000010000000200000003000000048585858CB6B6
          B6C2B4B4B4C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C3B5B5B5C38484848D000000050000
          0004000000030000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002B9B9B9C14D3D
          31FF3E2D23FF3D2D23FF3D2D23FF3D2D23FF3D2D23FF3B2C23FF3B2B23FF3B2B
          22FF3A2B22FF3A2A22FF3A2A21FF392A21FF392921FFB8B8B8C1000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002BABABAC1604D
          40FF5F4C40FF5F4B3FFF5F4B3FFF5E4A3FFF5D4A3FFF5D4A3EFF5C4A3EFF5C49
          3EFF5C483DFF5B483DFF5B483DFF5B483CFF49392FFFB8B8B8C1000000020000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000010000000200000003000000038B8B8B91B8B8
          B8C2B6B6B6C2B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B3B3B3C3B4B4B4C3B6B6B6C28A8A8A91000000040000
          0003000000020000000100000000000000000000000000000000000000000000
          000000000000000000018787878ABBBBBBC1B8B8B8C1B8B8B8C2B8B8B8C2B7B7
          B7C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5
          B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B5B5B5C3B5B5B5C2B5B5B5C2B6B6
          B6C2B8B8B8C18787878B00000001000000000000000000000000000000000000
          00000000000000000001BBBBBBC0514035FF413027FF413027FF413026FF4130
          26FF412F26FF412F26FF412F25FF402F25FF3F2F24FF3F2F24FF3E2F24FF3E2D
          24FF3E2D24FF3D2D23FF3D2D23FF3D2D23FF3C2D23FF3C2C23FF3B2B22FF3B2B
          22FF3B2B22FFB9B9B9C100000002000000000000000000000000000000000000
          00000000000000000001BBBBBBC0645043FF634F43FF634F43FF634F43FF624E
          43FF624E43FF624E43FF624E42FF624D42FF624D42FF614D40FF614D41FF604C
          40FF604C40FF5F4C40FF5F4B3FFF5F4A3FFF5E4A3FFF5D4A3FFF5D4A3FFF5C49
          3EFF4A3B30FFBABABAC100000002000000000000000000000000000000000000
          000000000000000000018D8D8D8FBBBBBBC0BABABAC1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8
          B8C2BABABAC18C8C8C8F00000001000000000000000000000000000000000000
          0000000000000000000000000001000000010000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000030000
          0002000000020000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000040000000500000005000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000040000000200000001000000000000000000000000000000000000
          000000000000000000028686868CB5B5B5C3B1B1B1C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAF
          AFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5ADADADC5AFAF
          AFC5B3B3B3C38585858D00000002000000000000000000000000000000000000
          00000000000000000003B6B6B6C244352CFF35261FFF34251FFF33251FFF3224
          1EFF32241EFF32231EFF32231EFF32231EFF31231EFF31231DFF30221DFF3022
          1CFF30211CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F
          1BFF2D1F1BFFB4B4B4C300000003000000000000000000000000000000000000
          00000000000000000003B6B6B6C254433AFF544339FF544339FF544239FF5341
          39FF534039FF524039FF514038FF504038FF503F38FF503F38FF4F3E38FF4F3D
          36FF4E3D36FF4E3D36FF4E3C34FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4D3B
          34FF3C2C29FFB5B5B5C300000003000000000000000000000000000000000000
          000000000000000000028B8B8B91B6B6B6C2B2B2B2C3B1B1B1C4B0B0B0C4AFAF
          AFC5AEAEAEC5ADADADC5ADADADC5ACACACC5ACACACC5ACACACC5ACACACC5ACAC
          ACC6ACACACC6ACACACC6ACACACC6ACACACC6ABABABC6ABABABC6ABABABC6ACAC
          ACC5B1B1B1C48989899200000003000000000000000000000000000000000000
          00000000000000000000000000010000000300000005000000068484848EB2B2
          B2C3AFAFAFC4AEAEAEC5AEAEAEC5ADADADC5ADADADC5ADADADC5ADADADC5ACAC
          ACC5ACACACC5ACACACC5ACACACC5ACACACC6ACACACC6ACACACC6ACACACC6ACAC
          ACC5B2B2B2C48484848D00000002000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000003B7B7B7C24635
          2DFF37271FFF37271FFF35271FFF35271FFF35261FFF34251FFF34251FFF3324
          1FFF33241FFF32231EFF32231EFF32231EFF32231EFF31231DFF30221DFF3022
          1CFF30221CFFB5B5B5C200000003000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000003B7B7B7C25745
          3CFF57443BFF56433BFF56433AFF54433AFF54433AFF54433AFF544239FF5441
          39FF534139FF524039FF524039FF514038FF513F38FF503F38FF503F38FF503E
          36FF3F302BFFB5B5B5C200000003000000000000000000000000000000000000
          000000000000000000000000000100000003000000040000000589898992B4B4
          B4C3B1B1B1C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAF
          AFC5AFAFAFC5AFAFAFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AFAF
          AFC5B3B3B3C38A8A8A9200000002000000000000000000000000000000000000
          000000000000000000018686868BB8B8B8C2B5B5B5C2B4B4B4C3B3B3B3C3B2B2
          B2C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AEAEAEC5AFAF
          AFC4B3B3B3C38585858C00000002000000000000000000000000000000000000
          00000000000000000002B8B8B8C14C3B31FF3C2C23FF3B2C22FF3B2B22FF3B2B
          22FF3A2A21FF3A2A21FF3A2A21FF392A21FF382921FF382821FF372821FF3727
          20FF37271FFF37271FFF35271FFF35271FFF35271FFF35251FFF35251FFF3325
          1FFF33241FFFB7B7B7C200000003000000000000000000000000000000000000
          00000000000000000002B9B9B9C15D4A3FFF5D4A3FFF5D4A3FFF5C493EFF5C48
          3DFF5B483DFF5B483DFF5A483DFF5A473DFF59473CFF59463CFF58463CFF5746
          3CFF57453CFF57443BFF56443BFF56433AFF54433AFF544339FF544339FF5442
          39FF43332CFFB7B7B7C200000002000000000000000000000000000000000000
          000000000000000000018C8C8C90B8B8B8C1B6B6B6C2B5B5B5C2B5B5B5C3B4B4
          B4C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B0B0B0C4B0B0B0C4B1B1
          B1C4B5B5B5C38A8A8A9100000002000000000000000000000000000000000000
          00000000000000000000000000010000000200000003000000048585858CB6B6
          B6C2B4B4B4C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B1B1B1C4B1B1B1C4B1B1B1C4B2B2
          B2C4B5B5B5C28686868C00000002000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002B9B9B9C14D3D
          31FF3E2D23FF3D2D23FF3D2D23FF3D2D23FF3D2D23FF3B2C23FF3B2B23FF3B2B
          22FF3A2B22FF3A2A22FF3A2A21FF392A21FF392921FF372921FF372821FF3727
          20FF372720FFB8B8B8C100000002000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002BABABAC1604D
          40FF5F4C40FF5F4B3FFF5F4B3FFF5E4A3FFF5D4A3FFF5D4A3EFF5C4A3EFF5C49
          3EFF5C483DFF5B483DFF5B483DFF5B483CFF5A473CFF59473CFF58463CFF5846
          3CFF47362FFFB8B8B8C100000002000000000000000000000000000000000000
          00000000000000000000000000010000000200000003000000038B8B8B91B8B8
          B8C2B6B6B6C2B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3
          B3C3B6B6B6C28B8B8B9000000002000000000000000000000000000000000000
          000000000000000000018787878ABBBBBBC1B8B8B8C1B8B8B8C2B8B8B8C2B7B7
          B7C2B6B6B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5
          B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B3B3B3C3B4B4
          B4C3B7B7B7C28686868B00000002000000000000000000000000000000000000
          00000000000000000001BBBBBBC0514035FF413027FF413027FF413026FF4130
          26FF412F26FF412F26FF412F25FF402F25FF3F2F24FF3F2F24FF3E2F24FF3E2D
          24FF3E2D24FF3D2D23FF3D2D23FF3D2D23FF3C2D23FF3C2C23FF3B2B22FF3B2B
          22FF3B2B22FFB9B9B9C100000002000000000000000000000000000000000000
          00000000000000000001BBBBBBC0645043FF634F43FF634F43FF634F43FF624E
          43FF624E43FF624E43FF624E42FF624D42FF624D42FF614D40FF614D41FF604C
          40FF604C40FF5F4C40FF5F4B3FFF5F4A3FFF5E4A3FFF5D4A3FFF5D4A3FFF5C49
          3EFF4A3B30FFBABABAC100000002000000000000000000000000000000000000
          000000000000000000018D8D8D8FBBBBBBC0BABABAC1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8
          B8C2BABABAC18C8C8C8F00000001000000000000000000000000000000000000
          0000000000000000000000000001000000010000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000020000
          0002000000020000000200000002000000020000000200000002000000030000
          0002000000020000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000040000000500000005000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000060000000600000006000000060000000600000006000000060000
          0006000000040000000200000001000000000000000000000000000000000000
          000000000000000000028686868CB5B5B5C3B1B1B1C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAF
          AFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5ADADADC5AFAF
          AFC5B3B3B3C38585858D00000002000000000000000000000000000000000000
          00000000000000000003B6B6B6C244352CFF35261FFF34251FFF33251FFF3224
          1EFF32241EFF32231EFF32231EFF32231EFF31231EFF31231DFF30221DFF3022
          1CFF30211CFF2F211CFF2F211CFF2F211CFF2F211CFF2E201CFF2D201CFF2D1F
          1BFF2D1F1BFFB4B4B4C300000003000000000000000000000000000000000000
          00000000000000000003B6B6B6C254433AFF544339FF544339FF544239FF5341
          39FF534039FF524039FF514038FF504038FF503F38FF503F38FF4F3E38FF4F3D
          36FF4E3D36FF4E3D36FF4E3C34FF4D3C34FF4D3C34FF4D3C34FF4D3C34FF4D3B
          34FF3C2C29FFB5B5B5C300000003000000000000000000000000000000000000
          000000000000000000028A8A8A91B3B3B3C3AFAFAFC4AEAEAEC5AEAEAEC5ADAD
          ADC5ADADADC5ADADADC5ADADADC5ACACACC5ACACACC5ACACACC5ACACACC5ACAC
          ACC6ACACACC6ACACACC6ACACACC6ACACACC6ABABABC6ABABABC6ABABABC6ACAC
          ACC5B1B1B1C48989899200000003000000000000000000000000000000000000
          000000000000000000028585858CB4B4B4C3AFAFAFC4AFAFAFC5AFAFAFC5AEAE
          AEC5AEAEAEC5AEAEAEC5AEAEAEC5ADADADC5ADADADC5ADADADC5ADADADC5ACAC
          ACC5ACACACC5ACACACC5ACACACC5ACACACC6ACACACC6ACACACC6ACACACC6ACAC
          ACC5B2B2B2C48484848D00000002000000000000000000000000000000000000
          00000000000000000002B8B8B8C247382FFF382921FF372821FF372720FF3727
          20FF37271FFF37271FFF35271FFF35271FFF35261FFF34251FFF34251FFF3324
          1FFF33241FFF32231EFF32231EFF32231EFF32231EFF31231DFF30221DFF3022
          1CFF30221CFFB5B5B5C200000003000000000000000000000000000000000000
          00000000000000000002B8B8B8C25A473CFF59473CFF58463CFF58463CFF5745
          3CFF57443BFF56433BFF56433AFF54433AFF54433AFF54433AFF544239FF5441
          39FF534139FF524039FF524039FF514038FF513F38FF503F38FF503F38FF503E
          36FF3F302BFFB5B5B5C200000003000000000000000000000000000000000000
          000000000000000000028B8B8B91B5B5B5C2B2B2B2C4B0B0B0C4B0B0B0C4B0B0
          B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC4AFAFAFC5AFAF
          AFC5AFAFAFC5AFAFAFC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AEAEAEC5AFAF
          AFC5B3B3B3C38A8A8A9200000002000000000000000000000000000000000000
          000000000000000000028686868CB6B6B6C2B2B2B2C3B1B1B1C4B1B1B1C4B1B1
          B1C4B1B1B1C4B0B0B0C4B0B0B0C4B0B0B0C4B0B0B0C4AFAFAFC4AFAFAFC4AFAF
          AFC4AFAFAFC4AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AFAFAFC5AEAEAEC5AFAF
          AFC4B3B3B3C38585858C00000002000000000000000000000000000000000000
          00000000000000000002B8B8B8C14C3B31FF3C2C23FF3B2C22FF3B2B22FF3B2B
          22FF3A2A21FF3A2A21FF3A2A21FF392A21FF382921FF382821FF372821FF3727
          20FF37271FFF37271FFF35271FFF35271FFF35271FFF35251FFF35251FFF3325
          1FFF33241FFFB7B7B7C200000003000000000000000000000000000000000000
          00000000000000000002B9B9B9C15D4A3FFF5D4A3FFF5D4A3FFF5C493EFF5C48
          3DFF5B483DFF5B483DFF5A483DFF5A473DFF59473CFF59463CFF58463CFF5746
          3CFF57453CFF57443BFF56443BFF56433AFF54433AFF544339FF544339FF5442
          39FF43332CFFB7B7B7C200000002000000000000000000000000000000000000
          000000000000000000018B8B8B90B7B7B7C2B4B4B4C3B3B3B3C3B3B3B3C3B2B2
          B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2
          B2C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B1B1B1C4B0B0B0C4B0B0B0C4B1B1
          B1C4B5B5B5C38A8A8A9100000002000000000000000000000000000000000000
          000000000000000000018686868BB8B8B8C2B5B5B5C3B4B4B4C3B3B3B3C3B3B3
          B3C3B3B3B3C3B3B3B3C3B3B3B3C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2B2C3B2B2
          B2C3B2B2B2C4B2B2B2C4B2B2B2C4B2B2B2C4B1B1B1C4B1B1B1C4B1B1B1C4B2B2
          B2C4B5B5B5C28686868C00000002000000000000000000000000000000000000
          00000000000000000002BABABAC14F3E32FF3F2E24FF3E2E24FF3E2E24FF3E2E
          23FF3E2D23FF3D2D23FF3D2D23FF3D2D23FF3D2D23FF3B2C23FF3B2B23FF3B2B
          22FF3A2B22FF3A2A22FF3A2A21FF392A21FF392921FF372921FF372821FF3727
          20FF372720FFB8B8B8C100000002000000000000000000000000000000000000
          00000000000000000001BBBBBBC1624D41FF614D41FF604D41FF604D40FF604D
          40FF5F4C40FF5F4B3FFF5F4B3FFF5E4A3FFF5D4A3FFF5D4A3EFF5C4A3EFF5C49
          3EFF5C483DFF5B483DFF5B483DFF5B483CFF5A473CFF59473CFF58463CFF5846
          3CFF47362FFFB8B8B8C100000002000000000000000000000000000000000000
          000000000000000000018C8C8C90B9B9B9C1B6B6B6C2B5B5B5C2B5B5B5C2B5B5
          B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4
          B4C3B4B4B4C3B4B4B4C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3B3C3B3B3
          B3C3B6B6B6C28B8B8B9000000002000000000000000000000000000000000000
          000000000000000000018787878BB9B9B9C1B7B7B7C2B6B6B6C2B6B6B6C2B6B6
          B6C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C2B5B5B5C3B5B5B5C3B5B5
          B5C3B5B5B5C3B5B5B5C3B4B4B4C3B4B4B4C3B4B4B4C3B4B4B4C3B3B3B3C3B4B4
          B4C3B7B7B7C28686868B00000002000000000000000000000000000000000000
          00000000000000000001BBBBBBC0514035FF413027FF413027FF413026FF4130
          26FF412F26FF412F26FF412F25FF402F25FF3F2F24FF3F2F24FF3E2F24FF3E2D
          24FF3E2D24FF3D2D23FF3D2D23FF3D2D23FF3C2D23FF3C2C23FF3B2B22FF3B2B
          22FF3B2B22FFB9B9B9C100000002000000000000000000000000000000000000
          00000000000000000001BBBBBBC0645043FF634F43FF634F43FF634F43FF624E
          43FF624E43FF624E43FF624E42FF624D42FF624D42FF614D40FF614D41FF604C
          40FF604C40FF5F4C40FF5F4B3FFF5F4A3FFF5E4A3FFF5D4A3FFF5D4A3FFF5C49
          3EFF4A3B30FFBABABAC100000002000000000000000000000000000000000000
          000000000000000000018D8D8D8FBBBBBBC0BABABAC1B9B9B9C1B9B9B9C1B9B9
          B9C1B9B9B9C1B9B9B9C1B9B9B9C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8B8C1B8B8
          B8C1B8B8B8C1B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8B8C2B8B8
          B8C2BABABAC18C8C8C8F00000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000003000000060000000400000002000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000001000000040000000A4F2B12800000000B00000004000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000010000000100000001000000010000000000000000000000000000
          000000000002000000092816094CB36A36FF2816094D0000000A000000030000
          0001000000010000000200000004000000040000000400000004000000050000
          0005000000050000000500000005000000050000000500000005000000050000
          0005000000050000000600000005000000030000000200000001000000000000
          0001000000050000000F7E461DC5C78955FF7C441DC600000011000000060000
          0002000000040000000A0000000E000000100000001100000011000000120000
          0012000000130000001300000013000000140000001400000014000000150000
          00150000001500000015000000130000000E0000000600000002000000000000
          0002000000092A170A50B46C38FFD29A66FFB36B37FF2A1709530000000A0000
          00050000000A664F42BD8C6E5CFF8B6E5AFF8A6D5AFF896B59FF886B58FF886B
          58FF876956FF866856FF856754FF856752FF836551FF826551FF816350FF8163
          4FFF7F604EFF7F604DFF7E5F4CFF584337BF0000000E00000003000000010000
          00040000000E7F461DC5C98C57FFD39C67FFC88A56FF7E441DC6000000110000
          00080000000C8E705DFFD2C4BBFFC3B1A4FFC1B0A3FFC1AEA2FFC0AC9FFFBFAC
          9FFFBEAA9EFFBDAA9CFFBDA89BFFBBA89AFFBBA798FFBAA697FFB9A597FFB8A4
          95FFB8A394FFB8A293FFB6A093FF7E604CFF0000001100000005000000010000
          00062D190B50B6703AFFD39D69FFD39C68FFD39C67FFB56D38FF2C180A520000
          000A0000000A6A5444C28E715EFF8E705DFF8C6E5CFF8B6E5BFF8A6C5AFF8A6C
          59FF896C58FF886B57FF876956FF866956FF866755FF856753FF846752FF8265
          51FF826450FF81634FFF7F624EFF5D493AC50000000D00000003000000010000
          000680471EC2AD5F2AFFAC5F2AFFD39D69FFAB5E28FFAB5D27FF7F461DC50000
          0009000000070000000B00000010000000130000001300000014000000140000
          0015000000150000001600000016000000170000001700000018000000180000
          00180000001700000015000000130000000D0000000600000002000000010000
          00040000000B00000017AD5F29FFD59F6BFFAC5F29FF0000001B0000000D0000
          0006000000050000000B00000010000000120000001300000013000000140000
          0014000000150000001500000016000000160000001700000017000000170000
          0016000000110000000A00000006000000030000000200000001000000000000
          00010000000500000011AD602AFFD69F6BFFAB5F29FF00000013000000070000
          000400000008695345BC917460FF90725FFF90725EFF8F715DFF8E715DFF8D6F
          5CFF8B6E5BFF8B6D5BFF8A6D59FF896C59FF886B57FF886A57FF866956FF8668
          55FF60493DBF0000000C00000003000000010000000100000000000000000000
          0000000000040000000FAE602BFFD6A06CFFAC6029FF00000011000000050000
          00030000000A937562FFD4C8C0FFC6B6AAFFC5B5A9FFC5B4A8FFC4B3A6FFC4B2
          A6FFC3B1A4FFC1B0A3FFC1AEA2FFC0AEA0FFBFAB9FFFBEAB9EFFBDAA9CFFBCA9
          9BFF876855FF0000000E00000004000000000000000000000000000000000000
          0000000000030000000EAE612BFFD7A16EFFAC602AFF00000010000000040000
          0002000000086D574AC2927562FF917560FF917461FF91745FFF90725FFF8F72
          5DFF8E715EFF8D705CFF8C6E5CFF8B6D5AFF8A6C5AFF8A6C59FF896B58FF886A
          57FF634F3FC30000000C00000003000000000000000000000000000000000000
          0000000000030000000CAE612BFFD7A370FFAD602AFF0000000E000000040000
          0001000000040000000A0000000E000000100000001100000012000000120000
          0012000000130000001300000014000000140000001500000015000000150000
          0014000000100000000A00000005000000030000000100000000000000000000
          00000000000200000009AF622CFFAE612CFFAE612BFF0000000A000000030000
          000100000004000000090000000E000000100000001100000011000000120000
          0012000000120000001300000013000000140000001400000015000000150000
          00150000001400000013000000110000000B0000000500000001000000000000
          00000000000100000004000000090000000C0000000A00000005000000010000
          0002000000076B5648BC957864FF947764FF937664FF927662FF927462FF9274
          61FF91735FFF90735EFF8F725EFF8E725DFF8E705DFF8D705CFF8D6E5AFF8B6D
          5AFF8A6D59FF896C58FF886A57FF614B3EBE0000000B00000003000000000000
          0000000000010000000200000004000000050000000500000002000000010000
          000200000008957966FFD7CDC5FFCABCB1FFCABBB0FFC9B9AEFFC8B9AEFFC7B8
          ABFFC6B6AAFFC5B5A9FFC5B4A8FFC4B3A6FFC3B2A5FFC2B0A4FFC2B0A3FFC1AE
          A2FFC0AE9FFFBEAB9EFFBEAB9DFF886B58FF0000000E00000004000000000000
          00000000000100000004000000080000000B0000000900000004000000010000
          0002000000076F594CC1967966FF957965FF957865FF947664FF937663FF9376
          63FF927562FF927560FF917461FF90735FFF90725EFF8E715DFF8F715DFF8D70
          5CFF8D6F5AFF8C6D5AFF8B6C5AFF665040C30000000B00000003000000000000
          00000000000200000008B0642EFFB0632EFFAF632EFF00000009000000020000
          000100000003000000090000000C0000000E0000000F0000000F000000100000
          0010000000110000001100000012000000120000001200000013000000130000
          001300000013000000110000000F0000000B0000000500000001000000000000
          0000000000020000000AB1652FFFDBAA78FFB0632DFF0000000B000000030000
          000100000003000000080000000C0000000E0000000F0000000F0000000F0000
          0010000000100000001100000011000000120000001200000012000000130000
          00110000000D0000000800000005000000030000000100000000000000000000
          0000000000020000000AB2662FFFDCAB7BFFB1642FFF0000000C000000030000
          0001000000066D594CBB977B68FF977B67FF977B67FF967A66FF957866FF9578
          65FF947764FF947763FF937663FF927663FF927561FF917460FF917361FF9072
          5FFF665243BD0000000A00000003000000000000000000000000000000000000
          0000000000020000000AB36630FFDDAC7CFFB1652FFF0000000C000000030000
          000200000007987C69FFDBD1CAFFCEC2B8FFCEC0B6FFCEBFB5FFCCBEB4FFCCBD
          B2FFCBBCB2FFCABAB0FFC9BAAFFFC8B9AEFFC7B8ACFFC6B6AAFFC5B5A9FFC5B4
          A8FF90725FFF0000000C00000003000000000000000000000000000000000000
          0001000000030000000AB36630FFDEAE7EFFB2652FFF0000000C000000040000
          000200000006725D4EC0987C6AFF987D69FF977B68FF977B67FF977A68FF967A
          67FF967966FF957866FF947864FF947763FF937663FF927662FF927562FF9174
          61FF6B5446C20000000A00000003000000000000000000000000000000000000
          0002000000060000000DB46731FFDEAE7FFFB26630FF00000010000000080000
          000400000004000000070000000B0000000C0000000D0000000D0000000E0000
          000E0000000F0000000F0000000F000000100000001000000011000000110000
          00100000000D0000000800000004000000020000000100000000000000010000
          0003854C25BFB46932FFB46732FFDFAF80FFB36631FFB36630FF844B23C00000
          000600000004000000070000000A0000000C0000000C0000000D0000000D0000
          000E0000000E0000000F0000000F0000000F0000001000000010000000110000
          0011000000100000000F0000000D000000090000000400000001000000000000
          00032D1A0D45C07A46FFDFB081FFDFB080FFDFAF80FFBE7844FF2C190C480000
          0005000000066F5A4DBA997E6CFF997E6BFF997D6BFF987C6AFF987C69FF987B
          69FF977B68FF977B67FF967A67FF967966FF957866FF957865FF947764FF9376
          63FF937662FF927562FF917561FF685245BD0000000900000002000000000000
          000200000006874E26C0D59E6EFFE0B081FFD59D6CFF844C24C1000000080000
          0004000000079B7F6CFFDED4CEFFD3C7BEFFD3C5BCFFD1C5BCFFD0C4BAFFCFC2
          B9FFCEC1B8FFCEC0B6FFCDBFB5FFCDBEB4FFCBBDB2FFCBBCB1FFCABAB0FFC9B9
          AFFFC8B8AEFFC6B7ABFFC6B7AAFF927561FF0000000B00000003000000000000
          0001000000032B190C43BF7A46FFE1B282FFBF7A46FF2D1A0C47000000040000
          000200000005725F51C09A7F6CFF9A7E6DFF9A7E6CFF9A7D6BFF997E6AFF997D
          69FF987D69FF987C68FF977B68FF977B68FF967A67FF967A67FF957866FF9578
          65FF947764FF937664FF937663FF6D5748C20000000900000002000000000000
          00000000000100000005874E26C0D5A070FF854E26C000000007000000020000
          0001000000020000000500000007000000080000000900000009000000090000
          000A0000000A0000000A0000000B0000000B0000000B0000000C0000000C0000
          000C0000000D0000000D0000000C000000090000000400000001000000000000
          000000000001000000032B190C41C07B47FF2B190C4200000004000000010000
          0000000000010000000100000002000000020000000200000002000000020000
          0002000000020000000200000003000000030000000300000003000000030000
          0003000000030000000300000003000000020000000100000000000000000000
          00000000000000000001000000035632197B0000000400000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000400000004000000030000
          0003000000040000000400000002000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000020000000800000010000000100000000A0000
          000B000000110000001100000009000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000400000011181410FF16120EFF000000160000
          0017130F0BFF130F0CFF00000013000000050000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000005000000161C1814FF1A1612FF0000001D0000
          001E15110EFF14110DFF00000019000000060000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000000000000000000
          000000000000000000010000000600000017201C18FF1E1915FF0000001E0000
          001F191511FF171310FF0000001A000000070000000100000000000000000000
          0001000000030000000300000004000000040000000400000004000000040000
          0004000000050000000500000005000000050000000400000004000000020000
          00010000000000000001000000060000001724201CFF221E1AFF0000001E0000
          001F1C1814FF1B1613FF0000001A000000070000000100000000000000010000
          0004000000090000000D0000000E0000000F0000001000000010000000100000
          001100000011000000110000001200000012000000120000000D000000060000
          000200000001000000020000000800000019292521FF27221FFF0000001D0000
          001E201C18FF1E1A16FF00000019000000070000000100000000000000020000
          000824120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF0000000C0000
          0004000000040000000900000012000000202E2A26FF2C2724FF0000001C0000
          001D25201DFF231E1BFF00000018000000060000000100000000000000020000
          0007342521FF332421FF332420FF32231FFF31221FFF31231EFF31211EFF3122
          1DFF30211DFF2F201CFF2F201CFF2F201BFF2F1F1BFF2E1F1BFF0000000C0000
          00070000000B0F0E0D50211F1DA7302A28E6342F2BFF312C29FF0000001C0000
          001C2A2521FF28231FFF00000018000000060000000100000000000000010000
          00050000000A0000000E00000010000000110000001100000011000000120000
          001300000013000000140000001400000015000000130000000F000000080000
          000911100F50383430D83E3936FF3C3733FF393531FF37322EFF0000001B0000
          001B2F2B27FF2D2824FF00000017000000060000000100000000000000010000
          00040000000A0000000E0000000F000000100000001100000011000000120000
          001200000012000000130000001400000014000000130000000F000000090000
          000D2B2726A546413DFF736C68FFADA6A1FF3F3A36FF3C3733FF0000001B0000
          001B35302CFF322D29FF00000017000000060000000100000000000000020000
          000724120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF0000000E0000
          0011453F3CE44B4642FFB3ACA6FFD5CDC8FF443F3BFF413D38FF0000001A0000
          001A3A3531FF37322EFF00000016000000060000000100000000000000020000
          00063D2E2AFF3C2C29FF3C2C28FF3B2B27FF3B2B27FF3A2A26FF392A25FF3829
          25FF372824FF372824FF362723FF352622FF352622FF342621FF0000000D0000
          001048443FE4504B47FFB9B0ABFFDAD2CCFF494440FF47413DFF0000001C0000
          001C3F3B37FF3D3834FF00000018000000070000000100000000000000010000
          0004000000090000000C0000000E0000000E0000000F0000000F000000100000
          001000000011000000110000001200000012000000110000000D000000080000
          000C33312EA4554F4BFF827C76FFBDB4AEFF4E4945FF4C4743FF000000230000
          002445403CFF423D3AFF0000001D0000000A0000000200000000000000010000
          0004000000080000000B0000000D0000000D0000000E0000000E0000000F0000
          000F00000010000000110000001100000011000000100000000D000000070000
          00081715144D4C4843D757524EFF55504CFF534E4AFF514C48FF4F4945FF4C47
          43FF4A4541FF48423FFF45403CFF0000000E0000000400000000000000010000
          000524120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF0000000A0000
          0005000000081716154D363431A3504947E458524EFF56504CFF544E4AFF514C
          48FF4F4A46FF4D4744FF4B4541FF0000000D0000000300000000000000010000
          0005483A35FF473935FF473833FF463732FF453531FF443431FF433330FF4233
          2FFF41322EFF40312DFF3F302BFF3F2F2CFF3E2F2BFF3E2E2AFF000000090000
          000300000003000000070000000B0000000E0000001000000011000000120000
          001200000013000000120000000E000000070000000200000000000000010000
          0003000000070000000A0000000B0000000C0000000C0000000C0000000D0000
          000D0000000E0000000E0000000F0000000F0000000F0000000C000000080000
          0005000000040000000500000006000000070000000800000008000000080000
          0009000000090000000900000008000000050000000200000001000000010000
          000300000007000000090000000A0000000B0000000B0000000C0000000C0000
          000D0000000D0000000E0000000E0000000E0000000F0000000F0000000E0000
          000D0000000D0000000E0000000F0000000F0000001000000010000000100000
          00110000001100000012000000110000000D0000000600000001000000010000
          000424120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF0000000C00000003000000010000
          0004564743FF554541FF554541FF53453FFF52433EFF51423DFF50403CFF4F3F
          3AFF4E3E39FF4D3E39FF4C3D38FF4B3C37FF4A3B37FF493A36FF483935FF4738
          34FF463733FF453532FF443430FF433530FF42332FFF42332FFF41322EFF4032
          2EFF40312CFF3F302CFF3E2E2BFF3D2D2AFF0000000B00000003000000010000
          0002000000060000000800000008000000090000000A0000000A0000000A0000
          000B0000000B0000000C0000000C0000000D0000000D0000000E0000000E0000
          000F0000000F0000001000000010000000110000001100000012000000120000
          00120000001300000014000000130000000E0000000700000002000000010000
          00020000000500000007000000080000000800000009000000090000000A0000
          000A0000000A0000000C0000000C0000000C0000000C0000000D0000000D0000
          000E0000000E0000000F0000000F000000100000001000000011000000110000
          00120000001200000013000000120000000E0000000700000002000000010000
          000324120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF0000000A00000002000000010000
          0003655650FF635550FF635450FF62534EFF60514DFF5F504BFF5E4F4AFF5D4E
          49FF5C4D48FF5A4C47FF5A4A46FF594A46FF584843FF574742FF564642FF5445
          41FF534441FF52423FFF51423EFF51403CFF4F403BFF4E3F3AFF4D3F3AFF4C3D
          39FF4C3C39FF4B3B37FF4A3A36FF493935FF0000000A00000002000000000000
          0001000000030000000500000005000000050000000600000006000000060000
          000700000007000000070000000800000008000000090000000A0000000B0000
          000C0000000C0000000D0000000D0000000E0000000E0000000F0000000F0000
          00100000001100000011000000100000000C0000000600000002000000000000
          0000000000010000000100000001000000010000000100000001000000020000
          0002000000020000000200000002000000020000000300000005000000080000
          000B0000000C0000000D0000000D0000000D0000000E0000000E0000000F0000
          000F0000001000000010000000100000000C0000000600000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000010000000524120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF24120FFF2412
          0FFF24120FFF24120FFF24120FFF24120FFF0000000800000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000005655652FF6354
          4FFF62534DFF61524DFF60524DFF5F4F4BFF5E4F4AFF5C4D49FF5B4D48FF5A4C
          47FF594A45FF594944FF584944FF564843FF0000000800000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000002000000050000
          000700000008000000080000000900000009000000090000000A0000000A0000
          000A0000000B0000000B0000000B000000080000000400000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0002000000020000000200000002000000020000000200000002000000020000
          0003000000030000000300000003000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000040000000600000007000000070000
          0006000000040000000200000001000000000000000000000000000000000000
          0000000000000000000100000002000000040000000600000008000000080000
          0007000000040000000200000001000000000000000000000000000000000000
          000000000001000000040000000900000011000000170000001B0000001C0000
          0018000000130000000B00000005000000020000000100000000000000000000
          000100000002000000050000000A000000130000001A0000001F000000200000
          001B000000140000000B00000005000000020000000100000000000000000000
          0001000000060000000E05040326392A2495584138D46F5246FF6F5145FF573F
          36D5372822970504032A00000011000000070000000200000001000000000000
          000200000007000000110504032A34262198513A32D664473DFF62463BFF4C36
          2ED630221D9A0403032D00000013000000080000000200000001000000010000
          0006000000111F17145D6B5147E5A1897EFFB9A498FFCCB8ACFFCBB7ABFFB7A2
          95FF9E8579FF664B42E71F171465000000150000000700000002000000020000
          0007000000131F171463654B42E79C8277FFB59E91FFC9B2A5FFC8B1A4FFB29B
          8DFF967C70FF594137E719120F65000000170000000800000002000000040000
          000D281E1A6991776BFFC5B1A4FFC8AE99FFBB966FFFB68B5DFFB68B5DFFBD97
          71FFC7AD97FFC0AB9FFF8A7065FF251B186F0000001200000005000000050000
          0010271E196C8F7568FFC1AB9EFFBFA590FFA88761FF9E794DFF9F7B4EFFAA8A
          63FFBEA58EFFBCA498FF7D6358FF1F1613720000001300000006000000070A08
          072A80655AF6CFBCB2FFC2A086FFB58457FFD9B789FFE9CFA3FFEAD1A5FFDBBC
          8EFFBC8E5FFFC4A387FFC9B6A9FF775C50F7080605310000000C0000000B0A08
          062E7E6458F6CCB7ACFFB4957BFF9E7647FFCEB079FFE4CC94FFE4CE96FFD1B5
          7EFFA57E4FFFB5987BFFC5B0A2FF674D42F7070504330000000B0000000B4234
          2C93AF998DFFCCB2A0FFB58157FFE3B987FFE8BF8DFFEAC292FFEAC99AFFEDCF
          A5FFE9C89EFFBF8F63FFCAB19DFFA68E82FF3C2D279800000014000000124234
          2D96AD9689FFC3AA96FFA07548FFDBB573FFE2BD7AFFE4C17FFFE5C788FFE8CE
          94FFE3C58DFFAA8253FFC1A994FF9B8377FF33241F9A000000120000000E6D58
          4CD6CAB8AFFFB28363FFCF9868FFE3B07DFFE4B585FFE7B989FFE8BA8BFFE7B9
          8CFFEBC39AFFD8AD84FFBF9071FFC2AEA3FF634C42DA00000019000000176C57
          4CD9C8B4A9FFA07858FFC39055FFDBAC68FFDEB270FFE0B774FFE2B876FFE1B7
          77FFE5C188FFCDA772FFAA8462FFBCA69AFF533D36D9000000170000000F856B
          5FF2D7C9C0FFA66A4BFFDCA06FFFE7B894FFF1D3C2FFEECAB1FFE7B38BFFE4AD
          81FFE5B287FFE6B891FFB8805FFFD3C1B6FF7B6153F90000001C00000019886E
          62F9D8C7BDFF91603FFFD29A59FFE0B582FFEDD1B7FFE9C8A3FFE1B177FFDDAA
          6BFFDFAE70FFE0B47CFF9F734DFFCCB8ACFF674D43F30000001A0000000F9177
          69FFE1D3CAFFA26448FFDA9767FFF7E4E0FFF6E1DBFFF2D4C7FFEDC3ACFFE2A4
          79FFE1A377FFE6B18BFFB87E60FFD9C9C0FF83675AFF0000001C000000198F75
          67FFDFCEC4FF8E5C3BFFD09052FFF5E3DBFFF4DFD5FFEED2BDFFE8C09DFFDA9F
          62FFD99E5FFFDEAD77FF9F704FFFD6C3B9FF73584CFF0000001A0000000E9078
          68FBE8DED7FFAC765FFFC67E56FFF9ECEEFFF9EBEDFFF5DFD9FFEFCBBBFFE19B
          70FFE09A6FFFD59977FFBF8A74FFE2D8D1FF816659FF0000001B000000188C73
          64FFE8DCD5FF9D6F55FFBA7843FFF8EBEBFFF8EAEAFFF3DDD3FFECC9B0FFD995
          58FFD89458FFC89163FFAB7F65FFDED1C9FF755A4FFB000000170000000B8069
          5CE0DFD5D0FFD4B9ADFFA25C40FFDEA384FFF6DFDBFFF3D8D0FFE3A583FFDF93
          67FFDC956CFFBA785DFFD5BBAEFFE4DCD8FF755C4DFF442F27C6564237C5866C
          5DFFE0D7D1FFCDB5A7FF915635FFD79D74FFF4DED6FFF1D6C9FFDC9F70FFD68C
          51FFD38E57FFA56C4CFFCDB4A5FFD1C4BDFF654E43E300000013000000074739
          3186A79387FFECE4DEFFC39B8CFF9F553CFFC57752FFD5865CFFD7875EFFCB7F
          5AFFB46C55FFCCA496FFE7DDD6FFA2938DFF63493BFFA18D82FFBBADA6FF7D62
          53FFAFA19BFFEBE2DBFFB99786FF8D4F32FFB96F40FFCB7D47FFCC7E49FFBD75
          47FF9C6145FFBF9C8BFFE5DAD2FF958178FF3327218D0000000E00000004110E
          0C2A957B6BFFEAE6E3FFEFE7E1FFD7BDB2FFB47F6EFFA86752FFAC6B57FFBC88
          76FFD8BFB4FFEBE1DBFFE5E0DEFF5C4239FF53382CFF7D6054FF967F71FF775B
          4BFF7D6559FFEFEBE8FFEFE5DFFFD2BAAEFFA67865FF945E47FF98614AFFAB7F
          6BFFD1B9ACFFEADFD7FFE8E3E0FF80685AFF0B08073300000008000000020000
          0008725D4ECA8E796DFFEBE8E5FFF7F4F1FFF1EAE5FFEBE3DCFFEBE3DBFFEFE8
          E2FFF6F2EEFFE5E1DEFF745D54FF61463DFF563B2EFF69483CFF846656FF785D
          4EFF8A7367FFB1A29BFFF7F5F4FFF8F3F0FFF1EAE4FFEBE2DBFFEBE2DAFFEFE7
          E1FFF5F1ECFFEAE5E3FF846D62FF554036CD0000000F00000004000000010000
          0004110E0C28947968FE917D70FFD1C8C3FFEFECEBFFFEFEFEFFFDFDFCFFE3DD
          DAFFAFA19BFF765F53FF796055FF6D5348FF5A4032FF6A493DFF856758FF7A5F
          4FFF998376FFC5B8B1FFD8D3D1FFDFDBD8FFE8E3E1FFFCFBFBFFFBFBFAFFDED8
          D5FFC0B2ABFF977F70FF876E5EFE0C0907300000000800000002000000000000
          000200000007675547B19B8577FFB9A79DFFC5B8B2FFD5CFCCFFBCADA7FFA289
          7EFF877066FF7E685FFF927C6FFF7A6157FF614537FF6A493DFF866858FF7C61
          51FFA18C81FFD4C9C3FFFDFDFCFFDED8D4FFAB968BFF897167FF6C564DFF9278
          6EFFA28A7DFF977D6EFF624F43B50000000D0000000400000001000000000000
          0001000000042C24204DA58C7CFFC1AEA3FFE0D5D0FFF8F6F5FFEAE3DFFFC5B1
          A6FFA18980FF877267FFAD988BFF897267FF674B3CFF604238FF7B5D50FF7E63
          52FFAA968AFFDBD1CBFFFFFFFFFFF0EBE8FFC0AA9EFF947C71FF7A6459FFBBA7
          99FFB59F91FF866D5DFF28211C53000000070000000200000000000000000000
          0000000000020202020A9A8475E6B5A69BFFDDD3CCFFF2EEEBFFF2EEECFFD1C2
          B8FFB29D96FF907B71FFBEADA0FFA9958BFF6B5141FF17100D4A1D1713477F64
          54FFB7A599FFE2D9D4FFFFFFFFFFEFE9E6FFBEA79CFF8F786FFF8B746BFFC6B6
          ACFFB7A293FF7C6452E702020110000000040000000100000000000000000000
          0000000000010000000463564D8FBCAB9FFFD8CBC2FFEEE9E6FFF7F5F4FFDCCF
          C8FFC0ABA2FF99877DFFBEABA1FFD1BFB6FF7D6252F80000000D0000000AA28B
          7CF8C9B7ACFFE6DFDAFFFFFFFFFFEDE6E3FFBBA59CFF8C776FFFA79488FFCCBC
          B0FFA48E7EFF4E3F359200000008000000020000000000000000000000000000
          00000000000000000002221E1B33C2AFA2FDD5C8BFFFEBE3DFFFFBF9F8FFE6DD
          D7FFCAB7B0FFA59288FFBCA8A1FFD7C8BFFF8B7161F20000000A00000007AC96
          87F1D0C3B8FFEBE3DFFFFFFFFFFFEBE3DFFFBAA59DFF8C7970FFC6B6A9FFD5C7
          BDFF977D6FFD1B16133700000004000000010000000000000000000000000000
          00000000000000000001000000049A8A81C5D1C6BEFFE9E1DCFFFAF8F6FFEBE4
          E1FFD9C9C1FFB19F95FFB8A69EFFD6C8BDFF756051C400000008000000068E7D
          71C2D9CBC3FFECE5E1FFFFFFFFFFE8DFDAFFB5A097FF947F75FFD5C7BDFFD8C9
          C0FF7A6558C70000000700000002000000000000000000000000000000000000
          0000000000000000000000000002332F2C43D2C3BAFDEDE5DFFFF8F5F4FFF3EE
          EBFFE2D8D0FFC5B4ADFFC8BBB5FFBDA79BFD261F1A4400000004000000032D28
          2540C8B6AAFDF3EFEDFFFFFFFFFFE7DDD8FFAD9B92FFB4A39AFFE6DDD6FFBEAA
          9DFD29231F470000000400000001000000000000000000000000000000000000
          000000000000000000000000000100000002645C587BD9CCC2FFECE6E1FFF5F1
          EFFFEEE6E2FFD7C9C2FFBFAA9EFF5548427D0000000500000002000000010000
          00025C524B7BCFBEB3FFECE5E1FFEBE1DDFFC8BAB3FFD4C6BEFFC6B2A5FF574B
          437D000000050000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000235312E4291857DB3B6A5
          9AE5B19F93E585766CB32E282544000000040000000200000000000000000000
          000100000002302B284287786FB3AC988BE5AA9589E5837268B32E2825440000
          0004000000020000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          0003000000030000000300000002000000010000000000000000000000000000
          0000000000010000000100000002000000020000000300000003000000020000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000010000
          0003000000040000000400000002000000010000000000000000000000010000
          0002000000040000000400000003000000020000000000000000000000000000
          0000000000000000000000000000000000010000000300000004000000040000
          0005000000040000000400000003000000010000000000000000000000040000
          000B0000000F0000000D00000007000000020000000100000001000000030000
          00080000000F000000100000000C000000050000000200000000000000000000
          0000000000000000000000000001000000050000000B0000000F000000100000
          0010000000100000000E0000000B000000070000000300000001000000091113
          80D613179CFF131698FC01020B21000000050000000100000001000000070304
          1B3D111497FF111397FF0E107CD60000000A0000000200000000000000000000
          00000000000000000000000000030000000B005023FF005022FF004D22FF004C
          22FF00461FF2003919D20026119E0008032D0000000900000003000000091417
          6292252CB9FF161AA0FF0708386A0000000A00000003000000030000000B1012
          55862126B4FF1D22AFFF090B51940000000A0000000200000000000000000000
          00000000000000000000000000030000000D00622FFF006733FF006632FF0060
          2DFF005D2BFF005C2AFF005326FF00421DF2000E064500000007000000070A0B
          2C4B2830BCFF1D23ABFF0F1168B00000000F0000000700000008000000111C20
          8DCB242AB7FF1D21AAFF0506254E000000080000000100000000000000000000
          00000000000000000000000000030000000D006832FF006C34FF005224E50000
          00170002011A002C1680006530FF005A29FF003C1AD90000000C000000040101
          02122931B2EF252CB6FF181C9AF00000021C000000120000001304051536282D
          B8FC272DBAFF181C9AEF00000215000000050000000100000000000000000000
          00000000000000000000000000030000000C006D34FF007036FF00481FE40000
          000F0000000A00000011004C25BF006A34FF005022FF000A042D000000030000
          000B1F257EAE2E36C0FF1B22AAFF1A20A8FF1A20A7FF1C22AAFF2C34BEFF2B32
          BEFF2931BBFF10136AAF0000000C000000030000000100000000000000000000
          00000000000000000000000000030000000B007339FF00753AFF004B23E40000
          000E000000090000000F004B24B6007139FF005626FF000A042A000000020000
          000812154667333CC5FF323BC4FF323BC4FF313AC3FF3038C3FF2F37C2FF2F37
          C1FF262EB8FF090B3C6800000009000000020000000000000000000000000000
          00000000000000000000000000030000000B00793DFF00793EFF005025E40000
          00110000000F001D0E51007238F700783BFF004921D60000000A000000010000
          000504050E1F3641C6FC3640C8FF21287CAB000000180000001929309FD3323B
          C4FF242BB3FC02030C2200000005000000010000000000000000000000000000
          00000000000000000000000000020000000A007F41FF008245FF006931F40046
          22B0006531E9007B3FFF007D41FF006230E90013083E00000006000000000000
          00020000000A2B359AC6313CC4FF1F259FE10000001303040C223640C7FC3640
          C8FF192088C90000000C00000003000000010000000000000000000000000000
          000000000000000000000000000200000009018445FF018848FF018445FF0181
          43FF018545FF008243FF005D2DD1000A05280000000900000003000000000000
          0001000000071A1F56733A46CCFF252FBAFF0506163011143A573A46CCFF343F
          C6FF101456830000000800000002000000000000000000000000000000000000
          000000000000000000000000000200000008018A48FF018F4DFF01763BE9001F
          0F490143238901723BE1017B3DFF015728D50007031B00000003000000000000
          00010000000408091929404FD2FF2B35C2FF1015456921276C903D4BCFFF313C
          C5FF060821390000000500000001000000000000000000000000000000000000
          00000000000000000000000000020000000702904DFF02924EFF016933E30000
          000B0000000900120A2D018948FF017B3AFF0135188100000005000000000000
          000000000002000000093844AFD53340C8FF2027759E323C9FC7414FD2FF2631
          ADE40000000B0000000300000000000000000000000000000000000000000000
          000000000000000000000000000200000006039551FF039853FF026C34E30000
          0009000000070003021102904EFF028747FF014C24AC00000005000000000000
          00000000000100000006252E728E3B4AD1FF3642B3DC4351CEF74150D4FF1921
          739D000000070000000200000000000000000000000000000000000000000000
          000000000000000000000000000100000006039A54FF039B56FF026F35E30000
          000B0011092A025A329E039A56FF038E49FF013F1E8900000004000000000000
          00000000000100000004111533444554D7FF4857D9FF4858DAFF3C4BD1FF0D11
          3A53000000050000000100000000000000000000000000000000000000000000
          00000000000000000000000000010000000403A05BFF03A25DFF029650FF0292
          4CFF039951FF03A05BFF039D58FF027039D40007041500000002000000000000
          000000000001000000030101030A4757CCEE4B5BDBFF4B5BDAFF3644C8F60202
          0711000000020000000000000000000000000000000000000000000000000000
          00000000000100000001000000020000000402814ACD03834ACE039857EE04A0
          5BFF039152EB027943CB0144247A000A05180000000200000001000000000000
          00010000000200000004000000070000000B0000000D0000000D0000000B0000
          0006000000020000000100000000000000000000000000000000000000010000
          0002000000040000000600000008000000090000000A0000000B0000000C0000
          000C0000000D0000000C00000009000000050000000200000000000000000000
          0002000000060000000D00000014000000170000001900000018000000130000
          000D000000040000000100000000000000000000000000000000000000010000
          00040000000C000000150000001C0000001E0000001F00000020000000210000
          0021000000210000001F000000170000000B0000000300000001000000000000
          00020000000A8D5E3ED9A9714AFFA9714AFFA9714AFFA9714AFFA9724BFF3222
          165D0000000A0000000300000001000000000000000000000000000000010000
          000753382488A9714AFFA97049FFA87049FFA87048FFA86F48FFA86F48FFA86E
          47FFA76E47FFA76D46FFA66D46FF000000170000000600000001000000000000
          00020000000B75543CAFDCBD9DFFEDCFA6FFE7C190FFE7C18EFFCFA478FF7051
          39B2000000120000000600000001000000010000000000000000000000010000
          00050000001155392590CEA885FFEDD2ACFFE5BF8CFFE1B57EFFE1B57DFFE0B5
          7CFFE0B37BFFE0B37AFFA56C45FF0000001C0000000700000001000000000000
          00010000000843332665D3B08DFFF0DBB7FFE9C797FFE8C594FFE5C190FFB081
          5CFD221811490000000C00000005000000020000000100000001000000010000
          00020000000800000015563B2893CFAB89FFEFD4B0FFE6C291FFE3B983FFE2B8
          82FFE2B880FFE1B67FFFA87049FF0000001D0000000700000001000000000000
          00010000000407060417C09772F7F3E4C8FFF1DBB4FFEDD1A5FFEBCDA0FFD4AC
          81FF916546E0100B082D0000000D000000060000000300000002000000020000
          0002000000050000000E0000001F5F422DA2D0AB87FFEAC99CFFE4BD88FFE4BC
          87FFE4BC86FFE4BB85FFAB744DFF0000001B0000000700000001000000000000
          000000000002000000086A543F90DFC5A4FFF8ECCDFFF4E1BAFFF2DFB7FFF3DE
          B5FFCDA982FF986A48ED35251962000000100000000B00000008000000070000
          00090000000D0000001534241969966847EECAA27CFFE9C596FFE6C08DFFE6C0
          8DFFE6BF8CFFE5BF8BFFAF7851FF0000001A0000000700000001000000000000
          000000000001000000040E0C091DBD9973EFF2E5C9FFF8ECCBFFF5E5BFFFF4E3
          BCFFF4E2BBFFDBBD97FFAC7C58FF745036BD432E2078231810470403021B2218
          1048432E1F79734F35BFAA7955FFD5B189FFEAC99BFFE8C695FFE8C494FFE7C3
          95FFE7C493FFE7C391FFB17E56FF000000180000000600000001000000000000
          00000000000000000002000000064235295AD2AE87FFF6EED3FFF8EFCDFFF6E8
          C4FFF6E7C1FFF5E6C1FFF1DFBAFFD2B28DFFB9916CFFAC7C59FFA26E4AFFAB79
          56FFB88A66FFCDA881FFE8CB9FFFEBCB9DFFEACA9BFFEBCA9AFFEACB9BFFECD3
          AFFFF1D9B6FFECCDA1FFB4815AFF000000160000000600000001000000000000
          00000000000000000000000000020000000667534085D8B892FFF8F0D5FFFAF2
          D3FFF8ECC9FFF8EBC7FFF7EAC5FFF7E9C4FFF7E8C3FFF6E7C2FFF5E6C1FFF5E4
          BDFFF0D8AEFFEDCFA4FFEDD0A3FFEDCEA2FFECCEA1FFEED1A6FFEDD5B2FFC59B
          76FFD8B899FFF3E0C2FFB7865FFF000000140000000500000001000000000000
          0000000000000000000000000000000000020000000667544184D5B38DFFF4EA
          CEFFFCF6DBFFFAF2D1FFF9EECCFFF9EDCAFFF9EDC9FFF8ECC8FFF8EBC7FFF7EA
          C5FFF7E9C4FFF5E3BDFFEFD5ACFFEED5A9FFF2DCB9FFEBD5B7FFC49973FF5F48
          34915D46328FD9BC9EFFBA8B64FF000000110000000500000000000000000000
          000000000000000000000000000000000001000000020000000546392C5AC4A1
          7BEEDDC3A1FFF9F0D6FFFDF8DCFFFBF5D7FFFBF2D3FFFAF1D0FFF9EFCDFFF9EF
          CDFFF9EFCEFFF9F1D0FFFAF1D3FFF3E4C9FFD6B797FFB58C66F23F3024610000
          000F0000000F5E473489BD8F68FF0000000C0000000300000000000000000000
          0000000000000000000000000000000000000000000000000001000000030C0A
          081575604A91CCA882F6DEC29FFFEBDABBFFF3EACDFFF8F1D5FFFDF9DFFFF8F0
          D3FFF3E7CAFFE8D5B5FFD6B894FFBF9873F76C5540950B09071A000000070000
          000400000005000000095F483683000000060000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0001000000030A08061143382B56887056A8A78969CEB69673E3CEA880FFB593
          70E3A38364CF836A51AA40332759090806150000000600000004000000020000
          0001000000010000000200000003000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000002000000030000000400000004000000050000
          0005000000050000000400000003000000020000000100000001000000000000
          0000000000000000000100000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000010000
          0001000000010000000100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000002000000030000000400000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000100000002000000070000000E0000000F00000008000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000100000005000000114B2C1A8C956242E200000011000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000030000000B0B07042E965A37F5A36E48F500000018000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000007000000165A3721A6C0997BFFAB724CFF0000001A000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          00040000000F21140C5299623DFADFC09BFFA16D48F300000018000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000010000000100000000000000000000
          0000000000000000000000000000000000000000000000000001000000040000
          000C110A06358B5635E9CDA989FFDCB688FF956645E400000016000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000200000003000000030000000200000001000000000000
          00000000000000000000000000000000000100000001000000040000000C0503
          02227A4B2ED3BC9373FFDCAF77FFD9B68DFF7C5539C300000013000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000100000003000000070000000C0000000C0000000600000002000000010000
          000000000000000000010000000100000002000000050000000D0E090630784C
          2FD2B58A69FFDFB37FFFDDAF79FFD1B18CFF5C3F2C980000000E000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          00060000000C0A06032655311DA6AC744EFF0000000C00000003000000000000
          0000000000010000000200000004000000090000001020140D4C895735EBB78C
          68FFDFB382FFDEAF79FFE3BC8BFFBF9571FF2B1E15530000000A000000000000
          0000000000000000000000000000000000000000000200000004000000090000
          0010362013708B593BF1C69C77FFAE7951FF0000001100000005000000020000
          000300000005000000080000000E0B070426563621A194603EFAC69B72FFE2BA
          88FFDFB27DFFDFB27DFFEBD2ABFF9E6D4AEB0201011600000006000000000000
          000000000000000000000000000100000003000000060000000C1A100A3D7145
          2BCDB48866FFE5C499FFEACBA1FFB07C55FF000000150000000B000000090000
          000C00000010110B07314B2E1D8F8C5738F2B58863FFDEB98CFFE3BC8AFFE1B6
          82FFE0B681FFE4BF8FFFD8BC9BFF5B402C920000000C00000003000000000000
          0000000000010000000200000004000000090705031B53352298A67554FDD9B8
          92FFEBCCA1FFE7C595FFECCFA8FFB38059FF0F0A06341D120B452D1C115E4A2E
          1C8E784B2ED8955F3DFFB1825EFFDBB68DFFE6C295FFE2BC8AFFE3BA87FFE2B9
          86FFE2BA86FFF0DEBAFFAA7D59F10806041E0000000600000001000000000000
          000100000003000000060000000C33221661906142E8CBA583FFEDD2ABFFEACB
          9EFFE9C799FFE8C797FFEED3ACFFB6845DFF945D3BFFA3714EFFA67551FFB88C
          67FFCEA983FFE6C69FFFE9C89DFFE6C291FFE5BF8DFFE4BE8CFFE4BD8BFFE4BC
          8AFFEBCFA6FFD4B795FF4D37277A0000000B0000000300000000000000020000
          00040000000817100B30744F35BDBC9472FFE8CDAAFFEDD2A9FFEBCC9EFFEACB
          9DFFEBCA9CFFEAC99BFFEFD6B1FFEFD5B0FFEED4AEFFEDD1A9FFECCFA7FFEBCD
          A2FFE9C99CFFE8C595FFE7C393FFE7C292FFE6C291FFE6C191FFE6C18FFFEAC9
          9BFFEEE1C3FF936C4ED004030214000000050000000100000000000000050604
          0313533A2887AD8361F8DFC7A6FFF5E5C2FFF4E0BAFFF3E1BAFFF3E0B9FFF2DD
          B6FFF2DCB3FFF0DAB1FFEED4A9FFEDD0A3FFEBCB9EFFEACA9CFFEAC99BFFEAC8
          9AFFE9C899FFE9C698FFE8C697FFE8C596FFE7C595FFE7C494FFEBC99CFFF4E9
          CCFFB08A69EA140F0A29000000060000000200000000000000002F23184C9970
          51DED3B696FFF6EACAFFF7EAC6FFF6E7C1FFF6E6C0FFF5E6BFFFF5E5BFFFF5E5
          BEFFF5E4BDFFF4E3BCFFF4E2BCFFF4E1BBFFF3E1B9FFF0DAB0FFEDD1A5FFEACB
          9EFFEBCA9DFFEBCA9CFFEACA9BFFE9C89AFFE9C79AFFEED5ABFFF7F0D6FFC5A2
          82F9281E16430000000700000002000000010000000000000000C1956DFFF5EE
          DCFFFFFFF2FFFCF7DFFFF8ECC9FFF8EBC6FFF8EAC6FFF7EAC5FFF7E9C4FFF7E8
          C4FFF6E8C3FFF6E7C2FFF6E7C1FFF6E6C1FFF5E6C0FFF5E5BFFFF5E4BEFFF3DF
          B7FFEDD1A6FFEBCD9FFFECCC9FFFECCFA5FFF8ECCDFFF1E7CFFFB6906FEB291F
          17420000000700000003000000010000000000000000000000002E241B42A481
          60D7DFC7ADFFFCFAECFFFFFEEEFFFBF3D7FFF9EECAFFF9EDC9FFF9EDC9FFF8EC
          C8FFF8ECC8FFF8EBC7FFF7EAC7FFF7EAC6FFF7EAC5FFF7E9C4FFF7E9C3FFF7E8
          C3FFF6E7C1FFF4E1BBFFF9ECCEFFFBF6E0FFDBC2A6FF9A795BCE120E0A230000
          0006000000020000000100000000000000000000000000000000000000030202
          01095A48367AC3A07FF5EDDFCAFFFFFFF2FFFEFBE9FFFBF3D3FFFAF0CDFFF9F0
          CDFFF9EFCCFFF9EFCBFFF9EFCBFFF9EECAFFF9EFCDFFFBF2D4FFFCF5D8FFFDF9
          DFFFFFFEE8FFF8F3E0FFE1CBB1FFBB9473EF534130730504030E000000040000
          0002000000000000000000000000000000000000000000000000000000010000
          00020000000417130E24896D52B1D7BB9CFFF8F3E2FFFFFFF1FFFDF9E1FFFBF3
          D1FFFBF2D0FFFAF2D0FFFCF6D9FFFFFEE0FFF9F5E5FFF4EBD9FFEEE1CCFFE2CD
          B3FFD0AE8CFFAF8B68E3624E3A840C0907170000000500000003000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000100000002000000043B2F2450B29170E0E5D2B8FFFEFDEFFFFFFE
          EEFFFDF8DCFFFBF5D3FFFDF8DBFFC8A17AFFB3906EE5A18161CE8C6F54B46350
          3C83231C16340000000600000004000000020000000100000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000001000000020706040D69554187CFAF8CFAF2E8
          D4FFFFFFF2FFFEFDE9FFFDFADEFFCBA47DFF0000000800000005000000040000
          0003000000020000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000031F1A142A977C
          5FBFDDC5A7FFFAF7E7FFFFFFF1FFCCA780FF0000000500000002000000010000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0003473A2C5ABF9F7CE9EADBC3FFCEAA82FF0000000300000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010C0A081178634D94CEAA82FC0000000200000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000010000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          0003000000030000000100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000070000
          000D0000000C0000000600000001000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000F9867
          44E24A2C19890000000F00000005000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000014A571
          4AF5935834F50B07042B0000000A000000030000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000016AE76
          4FFFB6855CFF593620A400000013000000060000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000000000000000000014A470
          49F3DEBD91FF995F3AFA21140C4E0000000E0000000400000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000129767
          45E3E3C89EFFC79B6FFF8B5635E9100A06310000000B00000003000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000F7D56
          3AC1DCBE95FFEACA9DFFB7865EFF7A4C30D10603021E0000000A000000040000
          0001000000010000000000000000000000000000000000000000000000010000
          0002000000030000000300000002000000010000000000000000000000000000
          00000000000000000000000000000000000000000000000000000000000B5D40
          2B95D0AE87FFEDD3A8FFE6C497FFB4825CFF7B4D30D10F09062D0000000C0000
          0005000000020000000000000001000000000000000000000001000000020000
          00060000000C0000000C00000008000000040000000200000001000000000000
          0000000000000000000000000000000000000000000000000000000000082C1E
          1450BD916AFFF2E1B8FFE7C192FFE7C69AFFB98A64FF8C5A38EA21150D4A0000
          000F000000080000000400000002000000010000000000000000000000030000
          000DA97149FF522F1BA70A0603260000000D0000000600000003000000010000
          0001000000000000000000000000000000000000000000000000000000040201
          01129E6C48EAEFDFBAFFECD0A3FFE6C292FFEACB9FFFCCA47DFF9A6542FA5939
          249F0B0704250000000D00000008000000040000000300000002000000050000
          0011AB744DFFC7A37EFF855232F1331E1171000000120000000A000000040000
          0002000000000000000000000000000000000000000000000000000000020000
          000A5B3F2B90D6B992FFF4E4BCFFE9C898FFE7C495FFEACA9EFFE5C8A1FFBA8E
          69FF925E3DF24F32208E120C07300000000F0000000B000000090000000B0000
          0016AE7851FFF1DFB7FFE1C095FFAE7E59FF6C3F25CD190E09400000000E0000
          0007000000030000000100000000000000000000000000000000000000010000
          00050806041AAA7B57F0F4E9C4FFF0D8ADFFE9C899FFE8C798FFE9C899FFECD0
          A7FFE1C39EFFB88B67FF9D6744FF7F5334D74E32208E301E145E1F140D45100A
          0735B17C55FFF4E3BCFFE5BE8EFFE7C79AFFD5B189FF9C6945FD4E2F1C9A0704
          021E0000000A0000000500000002000000010000000000000000000000000000
          0002000000084C372677D2B38DFFF8EDC7FFECD0A4FFEBCA9CFFEAC99BFFEAC8
          9AFFEACB9EFFEED3ABFFEAD1ACFFD5B38EFFBF9572FFAE7E5BFFAB7A56FF9D67
          44FFB28058FFEDD3ACFFE5C190FFE5C08FFFE7C494FFEACCA2FFC39A75FF8756
          37E8301D12630000000F00000008000000040000000100000000000000000000
          00010000000404030211926C4ECFECE0BBFFF6E9C2FFEDD0A3FFEBCC9FFFEBCC
          9EFFEACB9DFFEBCA9CFFEACA9DFFEBCFA4FFEDD3ABFFEFD5AFFFEFD6AFFFEFD7
          B3FFEFD8B3FFEFD8B3FFE8C494FFE7C393FFE7C292FFE6C291FFE9C99EFFE4C6
          9FFFB18460FF6A432ABF150D08340000000B0000000500000002000000000000
          00000000000100000005140E0A26AF8965E9F5ECC9FFF6E8C0FFEDD1A6FFECCE
          A2FFECCDA1FFECCDA0FFEBCDA0FFEBCB9EFFEBCB9DFFEBCB9DFFEACA9CFFEAC9
          9BFFEAC89AFFE9C899FFE9C698FFE8C697FFE8C596FFE7C595FFE7C494FFE9C5
          96FFEBCFA7FFD6B590FF9F704EF84B311F8A0503021700000008000000000000
          0000000000000000000200000006281E1640C3A07CF9F6F0CCFFF9EFC8FFF0D7
          ADFFF0D6AAFFF1D9B0FFF2DDB4FFF2DDB6FFF2DEB6FFF3E1B8FFF3E0B7FFF2DD
          B4FFF0DAB1FFF0D8AEFFEED3A8FFECCFA1FFE9C89BFFE9C79AFFE9C798FFE8C7
          97FFE8C697FFE9CB9FFFECD2ADFFC6A07BFF8A5E40DF2A1C1350000000000000
          000000000000000000000000000200000005281F163FB58F6BEBF0E6C1FFFDFB
          D7FFFAF2CDFFF6E8C3FFF6E6C1FFF6E6C0FFF6E5BFFFF5E5BEFFF5E4BEFFF5E3
          BDFFF4E3BCFFF4E2BBFFF3E2BAFFF4E1BAFFF3DFB7FFF0D8AFFFECCFA2FFEACA
          9CFFEAC89AFFE9C899FFEACA9DFFF3E0BAFFF0E2BEFFBC8E67FF000000000000
          00000000000000000000000000000000000200000005120E0A209A7858CDDAC1
          9BFFFAF7D2FFFDFBD7FFFAF3CFFFF8EBC8FFF7E9C5FFF6E9C4FFF6E8C3FFF6E8
          C2FFF6E7C2FFF6E6C1FFF5E6C0FFF5E5BFFFF5E4BEFFF5E4BEFFF4E3BDFFF2DE
          B5FFEDCFA3FFF0D9B3FFF6EDC9FFDBC19CFF9F7B5BD82D221945000000000000
          00000000000000000000000000000000000000000001000000030504030D5240
          2F72BA9370EFE0CAA5FFF7F2CEFFFEFEDAFFFDFBD8FFFBF6D3FFFBF3D0FFF9EF
          CCFFF8ECC8FFF8EAC6FFF7EAC5FFF7E9C4FFF7E9C4FFF6E8C3FFF7E8C2FFF8EB
          C6FFFBF6D4FFEADBB6FFBF9B77F55945337D0202010D00000005000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          00040C090716614C3983AE8966E3CFAC85FFE1CBA6FFECDFBBFFF2EAC5FFF8F3
          CFFFFEFEDAFFFDFDDAFFF8EECAFFF8EDCAFFF8EDC9FFF8EDC8FFFBF6D3FFF5EF
          CCFFD4B791FF86684EB317120D27000000060000000300000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000020000000300000005231C1533624E3B838A6D52B49F7E5FCEB18D
          6AE6C69D76FFFEFDDBFFFAF1CEFFFAF0CEFFFBF4D3FFFCFBD8FFE3CFA9FFAE8D
          6BE13A2E23520000000600000003000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000100000001000000020000000300000004000000050000
          0009C9A179FFFEFEDBFFFBF6D3FFFDFBDAFFF0E6C2FFCCAC85FA68523F890706
          0410000000040000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000001000000020000
          0005CAA37CFFFEFFDBFFF9F5D2FFDCC29CFF96795CBF1F19132C000000040000
          0002000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          0003CCA680FFE9D9B4FFBD9D7AE946392C5B0000000400000002000000010000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0002CCA780FC77624A950C0A0812000000020000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001000000010000000100000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000004000000060000
          0007000000070000000700000008000000080000000800000008000000080000
          0008000000080000000900000009000000090000000900000009000000090000
          0009000000080000000600000002000000010000000000000000000000000000
          0000000000000000000000000000000000010000000600000010000000170000
          001B0000001C0000001C0000001D0000001E0000001E0000001F0000001F0000
          0020000000200000002100000021000000220000002300000023000000240000
          002300000020000000160000000A000000020000000000000000000000000000
          0000000000000000000000000000000000030000000F8F6642C1C78D5BFFC68C
          5BFFC68C5AFFC58B59FFC58B59FFC48A58FFC48957FFC38956FFC38855FFC288
          55FFC28754FFC28753FFC18653FFC08652FFC18552FFC18451FFC08451FFBF83
          50FFBF8450FF895E39C500000017000000050000000100000000000000000000
          00000000000000000000000000000000000500000015C88E5DFFF9F4EFFFF9F3
          EDFFF8F2ECFFF8F2EBFFF8F0E9FFF6EFE7FFF6EEE6FFF5EDE4FFF4ECE3FFF5EB
          E1FFF3EAE0FFF3E8DDFFF2E8DCFFF1E6DAFFF0E5D8FFF0E4D7FFF0E3D6FFEFE2
          D4FFEEE0D3FFBF8450FF0000001F000000080000000100000000000000000000
          00000000000000000000000000000000000500000016C98F5FFFFCF7F4FFFCF9
          F4FFFCF8F3FFFBF6F2FFFBF6F1FFFBF5EFFFFBF4EEFFFAF4EDFFFAF3ECFFF9F3
          EAFFF9F2E9FFF9F1E8FFF9F1E8FFF9F0E7FFF8F0E6FFF8EFE5FFF8EFE5FFF8EE
          E4FFF8EEE3FFC08450FF00000023000000080000000100000000000000000000
          00000000000000000000000000000000000500000016C99060FFFCFAF7FFFDF9
          F5FFFCF8F5FFFCF7F3FFFCF7F2FFFBF6F1FFFBF6EFFFFAF5EEFFFAF4EDFFFAF3
          ECFFFAF3EBFFFAF2EAFFF9F2E9FFF9F1E8FFF9F0E7FFF9F0E6FFF8EFE5FFF8EF
          E5FFF8EFE4FFBF8451FF00000022000000090000000100000000000000000000
          00000000000000000000000000000000000500000014CA9160FFFDFBF8FFFDF9
          F7FFFDFAF6FFFDF8F4FFFCF8F3FFFCF7F2FFFBF6F1FFFBF5F0FFFBF5EEFFFAF4
          EDFFFAF3ECFFFAF2EBFFF9F2EAFFF9F2E9FFF9F1E8FFF9F0E7FFF8F0E6FFF8F0
          E6FFF8EFE5FFC18552FF00000021000000090000000100000000000000000000
          00000000000000000000000000000000000500000013CA9261FFFDFBF9FFFDFB
          F7FFFDFAF7FFD9AD89FFD8AC87FFD7AA86FFD7A884FFD6A782FFD4A580FFD3A4
          7FFFD2A37DFFD1A17CFFD0A07AFFD09E78FFCF9E77FFCE9C76FFF9F0E7FFF9F0
          E7FFF8EFE6FFC18652FF00000021000000080000000100000000000000000000
          00000000000000000000000000000000000500000012CB9362FFFEFCFAFFFEFB
          F9FFFDFBF9FFFDFAF7FFFDFAF6FFFCF9F5FFFDF8F4FFFCF8F2FFFCF7F1FFFBF6
          F0FFFBF5EEFFFAF4EEFFFAF4EDFFFAF3ECFFF9F2EAFFF9F2EAFFF9F1E8FFF9F0
          E8FFF9F0E7FFC18653FF0000001F000000080000000100000000000000000000
          00000000000000000000000000000000000400000011CC9363FFFEFDFBFFFEFC
          FAFFFEFCFAFFFDFBF9FFFDFAF7FFFCF9F6FFFCF9F5FFFCF8F4FFFCF8F3FF47B4
          E6FF0096DEFF0094D9FF0091D6FF008FD2FF008DCEFF46A8D4FFF9F2E9FFF9F1
          E8FFF9F1E7FFC28754FF0000001F000000080000000100000000000000000000
          00000000000000000000000000000000000400000011CD9464FFFEFDFCFFFEFD
          FBFFFEFCFBFFDDB18FFFDBB18DFFDBAF8BFFD9AD8AFFD9AC88FFFCF9F4FF009C
          E6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008ED1FFFAF3EBFFF9F2
          EAFFF9F1E9FFC28755FF0000001E000000080000000100000000000000000000
          00000000000000000000000000000000000400000011CD9565FFFDFDFCFFFDFC
          FBFFFDFCFBFFFDFBFAFFFDFBF9FFFDFAF8FFFCFAF7FFFBF9F6FFFCF8F5FF41B5
          EAFF009DE6FF009AE4FF0099E1FF0097DDFF0094D9FF41ABDDFFF9F3EBFFFAF3
          EBFFF9F2EAFFC38955FF0000001D000000070000000100000000000000000000
          00000000000100000003000000030000000800000013CA9365FFFAF9F9FFFAF9
          F8FFF8F7F6FFF8F7F6FFF8F7F5FFF7F5F3FFF7F5F3FFF6F4F2FFF5F2EFFFF5F1
          EEFFF3F1EDFFF3EFEBFFF2EEEAFFF2EEE8FFF2EDE7FFF3EDE8FFF5EFE9FFF8F2
          EAFFF9F2EAFFC48957FF0000001C000000070000000100000000000000000000
          0002000000060000000A0000000D000000110000001DC08C60FFEDECECFFECEB
          EBFFEAE9E9FFE8E8E7FFE6E6E5FFE5E4E2FFE4E2E1FFE2E0DFFFE1DEDCFFDFDC
          D9FFDCDAD7FFDBD8D5FFD9D6D2FFD8D4D1FFD9D4CFFFDDD8D3FFE8E2DDFFF4ED
          E7FFF8F1EAFFC48A57FF0000001B000000070000000100000000000000010000
          0005112533513775A2E9397FB0FF377EB0FF357BAEFF347AADFF3278ABFF3077
          ABFF2E76A9FF2D74A8FF2C72A8FF2A71A6FF2870A5FF276FA4FF266EA3FF256E
          A3FF256DA3FF256DA3FF256DA3FF256DA3FF256DA3FF2A70A4FF9FB0BCFFE8E4
          DDFFF6F0E9FFC58B58FF0000001A000000070000000000000000000000020000
          00083E7EACF26DB2D6FF92D8F3FF8DD6F2FF89D4F2FF87D2F2FF83D0F0FF80CD
          F0FF7BCBF0FF79C8EFFF75C8EEFF71C5EEFF6DC3EEFF6AC1EDFF67BFEBFF65BD
          EBFF61BCEAFF5DB8EAFF59B7EAFF56B5E9FF55B3E8FF3C90C6FF2D72A5FFDED9
          D4FFF4EEE9FFC48A59FF00000019000000060000000000000000000000020000
          0008458AB8FF9ADBF4FF9ADBF5FF96DAF4FF91D8F3FF8DD5F2FF8BD4F2FF87D2
          F2FF79AABFFF615549FF7DCCEFFF79C9EEFF75C7EEFF5D5144FF68A1BAFF6AC1
          EDFF66BFEBFF65BDEAFF60BCEAFF5DB8EAFF5BB7EAFF55B4E8FF256DA3FFDBD7
          D2FFF2EDE8FFC58B5AFF00000018000000060000000000000000000000020000
          0008488DBBFFA5DFF6FFA0DEF6FF9BDCF5FF9ADBF4FF96DAF4FF92D7F3FF8DD6
          F3FF665A4EFF87D1F1FF83CEF1FF7FCDF0FF7DCCEFFF77C9EFFF605448FF71C5
          EEFF6DC3EEFF6AC0ECFF67BFEBFF65BDEAFF61BCEAFF5DB8EAFF256DA3FFDBD8
          D4FFF3EFEAFFC68C5BFF00000017000000060000000000000000000000020000
          00074C91BDFFA9E1F6FFA9E2F7FFA5DFF6FFA0DDF6FF9CDCF5FF9ADBF4FF96DA
          F3FF6A5E52FF8DD6F3FF89D3F2FF87D1F1FF83CEF1FF7FCDF0FF64584BFF77C9
          EFFF75C8EEFF71C5EEFF6CC3EEFF6AC0ECFF67BFEBFF64BBE9FF256DA3FFDDDA
          D7FFF4F0ECFFC78D5CFF00000016000000060000000000000000000000010000
          00065093C0FFACE2F5FFADE5F8FFACE3F7FFA7E1F6FFA4DFF6FFA1DDF5FF7D88
          87FF6E6356FF96D9F3FF92D8F3FF8ED6F3FF89D3F2FF87D1F1FF685C4FFF6E7D
          80FF7BCCF0FF77C9EFFF75C9EEFF71C5EEFF6CC3EEFF67BDEAFF276FA4FFE0DC
          DAFFF4F1EEFFC78E5DFF00000015000000050000000000000000000000010000
          00055397C2FFB5E7F9FFB3E7F8FFB1E6F8FFADE5F8FFACE3F7FFA7E1F6FFA4DF
          F6FF73665AFF9BDCF5FF9ADBF5FF96DAF4FF92D8F3FF8ED6F2FF6C6054FF87D0
          F2FF83CDF0FF80CDF0FF7DCBEFFF77C9EEFF75C8EEFF6FC3ECFF2970A6FFE3E0
          DDFFF6F3F0FFC88F5EFF00000014000000050000000000000000000000010000
          0005589BC5FFBDE9F9FFB9EAFAFFB6E8FAFFB2E7F9FFB1E6F8FFADE5F8FFABE3
          F7FF766A5FFFA4DFF6FFA0DDF6FF9CDCF5FF9ADBF4FF96DAF4FF706458FF8ED5
          F3FF89D3F2FF87D0F2FF83CDF0FF80CDF0FF7DCBF0FF76C8EDFF2B72A7FFE4E2
          E2FFF7F4F1FFC89060FF00000013000000050000000000000000000000010000
          00045B9DC7FFD6F2FBFFBEECFAFFBCEBFAFFB9EAFAFFB6E8FAFFB3E7F9FFB1E6
          F8FF9EC1CBFF796E61FFA7E1F6FFA4DFF6FFA0DEF5FF756A5DFF8FB8C7FF96DA
          F4FF92D8F3FF8ED6F3FF89D4F2FF87D1F1FF83CDF0FF85CEF0FF2D75A9FFE8E8
          E6FFF7F6F3FFC99160FF00000012000000050000000000000000000000000000
          00025998BEF1AACFE4FFEFFBFEFFECF9FDFFE9F8FDFFE6F7FDFFE1F6FDFFDEF5
          FDFFD9F3FCFFD4F1FBFFD0F0FBFFCCEEFAFFC8ECF9FFC1EAF9FFBCE7F8FFB6E5
          F8FFB1E3F7FFABE2F6FFA6DFF5FFA0DCF4FF99DAF4FF64A8D0FF3A7DAFFFEDEE
          ECFFF9F8F7FFCA9261FF00000011000000050000000000000000000000000000
          0001182832414C7F9FCA5EA0C9FF5C9EC8FF5A9CC6FF589AC5FF5598C3FF5397
          C2FF5194C0FF4F92BFFF4C90BDFF4A8EBCFF488DBAFF468AB8FF4388B7FF4186
          B6FF3F84B4FF3D82B3FF3B81B1FF397EB1FF377DAEFF4484B1FFBCCDD8FFF6F5
          F4FFFCFCFAFFCB9262FF00000010000000040000000000000000000000000000
          00000000000100000002000000030000000500000007CF9B6EFFF9F9F9FFF9F9
          F9FFF8F8F8FFF7F7F7FFF7F7F7FFF6F6F6FFF5F5F5FFF4F4F4FFF4F4F4FFF3F3
          F3FFF2F2F2FFF1F1F1FFF0F0F0FFF0F0F0FFF0F0F0FFF2F2F1FFF7F6F6FFFCFB
          FBFFFEFDFCFFCC9463FF0000000F000000040000000000000000000000000000
          00000000000000000000000000010000000200000003D39D70FFFEFEFEFFFEFE
          FEFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFCFC
          FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFCFCFCFFFCFCFCFFFDFDFCFFFEFD
          FDFFFFFEFEFFCC9564FF0000000C000000030000000000000000000000000000
          000000000000000000000000000000000000000000019E7654BFD49E70FFD49E
          70FFD39D6FFFD39D6FFFD39D6EFFD39D6EFFD29C6DFFD29C6DFFD29B6CFFD29A
          6BFFD19A6BFFD19A6BFFD0996AFFD09869FFCF9869FFCF9867FFCE9667FFCE96
          67FFCD9666FF976E4BC200000008000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          0002000000030000000300000004000000040000000500000005000000060000
          00060000000700000007000000080000000800000009000000090000000A0000
          000A0000000A0000000700000003000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000200000002000000020000000200000002000000020000
          0002000000020000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000001000000010000000100000001000000010000
          0001000000010000000100000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000100000004000000060000
          0007000000070000000700000008000000080000000800000008000000080000
          0008000000080000000900000009000000090000000900000009000000090000
          0009000000080000000600000002000000010000000000000000000000000000
          0000000000000000000000000000000000010000000600000010000000170000
          001B0000001C0000001C0000001D0000001E0000001E0000001F0000001F0000
          0020000000200000002100000021000000220000002300000023000000240000
          002300000020000000160000000A000000020000000000000000000000000000
          0000000000000000000000000000000000030000000F8F6642C1C78D5BFFC68C
          5BFFC68C5AFFC58B59FFC58B59FFC48A58FFC48957FFC38956FFC38855FFC288
          55FFC28754FFC28753FFC18653FFC08652FFC18552FFC18451FFC08451FFBF83
          50FFBF8450FF895E39C500000017000000050000000100000000000000000000
          00000000000000000000000000000000000500000015C88E5DFFF9F4EFFFF9F3
          EDFFF8F2ECFFF8F2EBFFF8F0E9FFF6EFE7FFF6EEE6FFF5EDE4FFF4ECE3FFF5EB
          E1FFF3EAE0FFF3E8DDFFF2E8DCFFF1E6DAFFF0E5D8FFF0E4D7FFF0E3D6FFEFE2
          D4FFEEE0D3FFBF8450FF0000001F000000080000000100000000000000000000
          00000000000000000000000000000000000500000016C98F5FFFFCF7F4FFFCF9
          F4FFFCF8F3FFFBF6F2FFFBF6F1FFFBF5EFFFFBF4EEFFFAF4EDFFFAF3ECFFF9F3
          EAFFF9F2E9FFF9F1E8FFF9F1E8FFF9F0E7FFF8F0E6FFF8EFE5FFF8EFE5FFF8EE
          E4FFF8EEE3FFC08450FF00000023000000080000000100000000000000000000
          00000000000000000000000000000000000500000016C99060FFFCFAF7FFFDF9
          F5FFFCF8F5FFFCF7F3FFFCF7F2FFFBF6F1FFFBF6EFFFFAF5EEFFFAF4EDFFFAF3
          ECFFFAF3EBFFFAF2EAFFF9F2E9FFF9F1E8FFF9F0E7FFF9F0E6FFF8EFE5FFF8EF
          E5FFF8EFE4FFBF8451FF00000022000000090000000100000000000000000000
          00000000000000000000000000000000000500000014CA9160FFFDFBF8FFFDF9
          F7FFFDFAF6FFFDF8F4FFFCF8F3FFFCF7F2FFFBF6F1FFFBF5F0FFFBF5EEFFFAF4
          EDFFFAF3ECFFFAF2EBFFF9F2EAFFF9F2E9FFF9F1E8FFF9F0E7FFF8F0E6FFF8F0
          E6FFF8EFE5FFC18552FF00000021000000090000000100000000000000000000
          00000000000000000000000000000000000500000013CA9261FFFDFBF9FFFDFB
          F7FFFDFAF7FFD9AD89FFD8AC87FFD7AA86FFD7A884FFD6A782FFD4A580FFD3A4
          7FFFD2A37DFFD1A17CFFD0A07AFFD09E78FFCF9E77FFCE9C76FFF9F0E7FFF9F0
          E7FFF8EFE6FFC18652FF00000021000000080000000100000000000000000000
          00000000000000000000000000000000000500000012CB9362FFFEFCFAFFFEFB
          F9FFFDFBF9FFFDFAF7FFFDFAF6FFFCF9F5FFFDF8F4FFFCF8F2FFFCF7F1FFFBF6
          F0FFFBF5EEFFFAF4EEFFFAF4EDFFFAF3ECFFF9F2EAFFF9F2EAFFF9F1E8FFF9F0
          E8FFF9F0E7FFC18653FF0000001F000000080000000100000000000000000000
          00000000000000000000000000000000000400000011CC9363FFFEFDFBFFFEFC
          FAFFFEFCFAFFFDFBF9FFFDFAF7FFFCF9F6FFFCF9F5FFFCF8F4FFFCF8F3FF47B4
          E6FF0096DEFF0094D9FF0091D6FF008FD2FF008DCEFF46A8D4FFF9F2E9FFF9F1
          E8FFF9F1E7FFC28754FF0000001F000000080000000100000000000000000000
          00000000000000000000000000000000000400000011CD9464FFFEFDFCFFFEFD
          FBFFFEFCFBFFDDB18FFFDBB18DFFDBAF8BFFD9AD8AFFD9AC88FFFCF9F4FF009C
          E6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF008ED1FFFAF3EBFFF9F2
          EAFFF9F1E9FFC28755FF0000001E000000080000000100000000000000000000
          00000000000000000000000000000000000400000011CD9565FFFDFDFCFFFDFC
          FBFFFDFCFBFFFDFBFAFFFDFBF9FFFDFAF8FFFCFAF7FFFBF9F6FFFCF8F5FF41B5
          EAFF009DE6FF009AE4FF0099E1FF0097DDFF0094D9FF41ABDDFFF9F3EBFFFAF3
          EBFFF9F2EAFFC38955FF0000001D000000070000000100000000000000000000
          00000000000100000003000000030000000800000013CA9365FFFAF9F9FFFAF9
          F8FFF8F7F6FFF8F7F6FFF8F7F5FFF7F5F3FFF7F5F3FFF6F4F2FFF5F2EFFFF5F1
          EEFFF3F1EDFFF3EFEBFFF2EEEAFFF2EEE8FFF2EDE7FFF3EDE8FFF5EFE9FFF8F2
          EAFFF9F2EAFFC48957FF0000001C000000070000000100000000000000000000
          0002000000060000000A0000000D000000110000001DC08C60FFEDECECFFECEB
          EBFFEAE9E9FFE8E8E7FFE6E6E5FFE5E4E2FFE4E2E1FFE2E0DFFFE1DEDCFFDFDC
          D9FFDCDAD7FFDBD8D5FFD9D6D2FFD8D4D1FFD9D4CFFFDDD8D3FFE8E2DDFFF4ED
          E7FFF8F1EAFFC48A57FF0000001B000000070000000100000000000000010000
          0005112533513775A2E9397FB0FF377EB0FF357BAEFF347AADFF3278ABFF3077
          ABFF2E76A9FF2D74A8FF2C72A8FF2A71A6FF2870A5FF276FA4FF266EA3FF256E
          A3FF256DA3FF256DA3FF256DA3FF256DA3FF256DA3FF2A70A4FF9FB0BCFFE8E4
          DDFFF6F0E9FFC58B58FF0000001A000000070000000000000000000000020000
          00083E7EACF26DB2D6FF92D8F3FF8DD6F2FF89D4F2FF87D2F2FF83D0F0FF80CD
          F0FF7BCBF0FF79C8EFFF75C8EEFF71C5EEFF6DC3EEFF6AC1EDFF67BFEBFF65BD
          EBFF61BCEAFF5DB8EAFF59B7EAFF56B5E9FF55B3E8FF3C90C6FF2D72A5FFDED9
          D4FFF4EEE9FFC48A59FF00000019000000060000000000000000000000020000
          0008458AB8FF9ADBF4FF9ADBF5FF96DAF4FF91D8F3FF8DD5F2FF8BD4F2FF87D2
          F2FF83CEF1FF80CCF0FF7DCCEFFF79C9EEFF75C7EEFF71C5EEFF6DC4EDFF6AC1
          EDFF66BFEBFF65BDEAFF60BCEAFF5DB8EAFF5BB7EAFF55B4E8FF256DA3FFDBD7
          D2FFF2EDE8FFC58B5AFF00000018000000060000000000000000000000020000
          0008488DBBFFA5DFF6FFA0DEF6FF9BDCF5FF9ADBF4FF8CB8C8FF756A61FF7368
          5FFF72675EFF87D1F1FF70635BFF6E6359FF6D6158FF74A9C1FF75C8EEFF6FA6
          BFFF685C53FF685B52FF67BFEBFF65BDEAFF61BCEAFF5DB8EAFF256DA3FFDBD8
          D4FFF3EFEAFFC68C5BFF00000017000000060000000000000000000000020000
          00074C91BDFFA9E1F6FFA9E2F7FFA5DFF6FFA0DDF6FF776D64FF9ADBF4FF96DA
          F3FF736860FF8DD6F3FF71655CFF87D1F1FF83CEF1FF6D6158FF7DCBEFFF6A5F
          55FF75C8EEFF71C5EEFF6CC3EEFF6AC0ECFF67BFEBFF64BBE9FF256DA3FFDDDA
          D7FFF4F0ECFFC78D5CFF00000016000000060000000000000000000000010000
          00065093C0FFACE2F5FFADE5F8FFACE3F7FFA7E1F6FF97BDCBFF776D65FF756B
          63FF756961FF96D9F3FF71665DFF8ED6F3FF89D3F2FF6E6259FF83D0F0FF6C5F
          56FF7BCCF0FF77C9EFFF75C9EEFF71C5EEFF6CC3EEFF67BDEAFF276FA4FFE0DC
          DAFFF4F1EEFFC78E5DFF00000015000000050000000000000000000000010000
          00055397C2FFB5E7F9FFB3E7F8FFB1E6F8FFADE5F8FFACE3F7FFA7E1F6FFA4DF
          F6FF756B62FF9BDCF5FF73685FFF96DAF4FF92D8F3FF6F635AFF8BD3F2FF6D61
          57FF83CDF0FF80CDF0FF7DCBEFFF77C9EEFF75C8EEFF6FC3ECFF2970A6FFE3E0
          DDFFF6F3F0FFC88F5EFF00000014000000050000000000000000000000010000
          0005589BC5FFBDE9F9FFB9EAFAFFB6E8FAFFB2E7F9FFB1E6F8FF797067FF786F
          65FF99BECAFFA4DFF6FF746A60FF72685FFF71675DFF8BB7C6FF92D8F3FF84B2
          C4FF6D6057FF6B5F56FF83CDF0FF80CDF0FF7DCBF0FF76C8EDFF2B72A7FFE4E2
          E2FFF7F4F1FFC89060FF00000013000000050000000000000000000000010000
          00045B9DC7FFD6F2FBFFBEECFAFFBCEBFAFFB9EAFAFFB6E8FAFFB3E7F9FFB1E6
          F8FFADE5F8FFABE3F7FF756B62FFA4DFF6FFA0DEF5FF9DDCF5FF9ADBF5FF96DA
          F4FF92D8F3FF8ED6F3FF89D4F2FF87D1F1FF83CDF0FF85CEF0FF2D75A9FFE8E8
          E6FFF7F6F3FFC99160FF00000012000000050000000000000000000000000000
          00025998BEF1AACFE4FFEFFBFEFFECF9FDFFE9F8FDFFE6F7FDFFE1F6FDFFDEF5
          FDFFD9F3FCFFD4F1FBFF929592FFCCEEFAFFC8ECF9FFC1EAF9FFBCE7F8FFB6E5
          F8FFB1E3F7FFABE2F6FFA6DFF5FFA0DCF4FF99DAF4FF64A8D0FF3A7DAFFFEDEE
          ECFFF9F8F7FFCA9261FF00000011000000050000000000000000000000000000
          0001182832414C7F9FCA5EA0C9FF5C9EC8FF5A9CC6FF589AC5FF5598C3FF5397
          C2FF5194C0FF4F92BFFF4C90BDFF4A8EBCFF488DBAFF468AB8FF4388B7FF4186
          B6FF3F84B4FF3D82B3FF3B81B1FF397EB1FF377DAEFF4484B1FFBCCDD8FFF6F5
          F4FFFCFCFAFFCB9262FF00000010000000040000000000000000000000000000
          00000000000100000002000000030000000500000007CF9B6EFFF9F9F9FFF9F9
          F9FFF8F8F8FFF7F7F7FFF7F7F7FFF6F6F6FFF5F5F5FFF4F4F4FFF4F4F4FFF3F3
          F3FFF2F2F2FFF1F1F1FFF0F0F0FFF0F0F0FFF0F0F0FFF2F2F1FFF7F6F6FFFCFB
          FBFFFEFDFCFFCC9463FF0000000F000000040000000000000000000000000000
          00000000000000000000000000010000000200000003D39D70FFFEFEFEFFFEFE
          FEFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFCFC
          FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFCFCFCFFFCFCFCFFFDFDFCFFFEFD
          FDFFFFFEFEFFCC9564FF0000000C000000030000000000000000000000000000
          000000000000000000000000000000000000000000019E7654BFD49E70FFD49E
          70FFD39D6FFFD39D6FFFD39D6EFFD39D6EFFD29C6DFFD29C6DFFD29B6CFFD29A
          6BFFD19A6BFFD19A6BFFD0996AFFD09869FFCF9869FFCF9867FFCE9667FFCE96
          67FFCD9666FF976E4BC200000008000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000020000
          0002000000030000000300000004000000040000000500000005000000060000
          00060000000700000007000000080000000800000009000000090000000A0000
          000A0000000A0000000700000003000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000010000000100000001000000010000
          0001000000010000000200000002000000020000000200000002000000020000
          0002000000020000000200000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000010000000300000006000000090000000A0000000A0000
          000A0000000A0000000B0000000B0000000B0000000B0000000C0000000C0000
          000C0000000C0000000C0000000B000000090000000400000001000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000020000000B000000180000002100000025000000270000
          0028000000280000002A0000002A0000002B0000002C0000002D0000002E0000
          002E0000002F0000002F0000002C000000220000001000000004000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000600000017C88F5DFFC88E5CFFC78E5CFFC68D5BFFC58C
          59FFC58B58FFC48A57FFC48956FFC38855FFC28753FFC18652FFC08651FFC084
          51FFBF8450FFBE844EFFBE824EFFBD814CFF0000002200000009000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000001000000070000001DCA915FFFFBF6F1FFFBF5F0FFFBF5EEFFFAF4
          EDFFFAF3ECFFFAF2EAFFF9F1E9FFF9F0E8FFF9EFE6FFF8EFE5FFF8EEE4FFF8ED
          E2FFF7ECE1FFF7EBE0FFF7EBDFFFBE824DFF0000002C0000000B000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000001000000070000001DCA9261FFFCF7F2FFFBF6F1FFFBF5F0FFFBF5
          EFFFFAF4EDFFFAF3ECFFFAF2EAFFF9F1E9FFF9F0E8FFF9F0E6FFF8EFE5FFF8EE
          E4FFF8EDE2FFF7ECE1FFF7EBE0FFBF834FFF0000002F0000000C000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000001000000070000001CCA9361FFF7F3EFFFEFEAE6FFF6F1ECFFFBF6
          F0FFFBF5EFFFFAF4EDFFFAF3ECFFFAF2EBFFF9F1E9FFF9F0E8FFF9F0E6FFF8EE
          E5FFF8EEE4FFF8EDE2FFF7ECE1FFBF8350FF0000002E0000000C000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000001000000060000001ACB9363FFEAE6E3FF438B49FFDCD7D4FFD8AE
          8BFFDAAD89FFD8AA86FFD6A682FFD3A47EFFD1A17AFFCF9E76FFCE9C74FFCC99
          72FFF8EFE5FFF8EEE4FFF8EDE2FFC08551FF0000002D0000000C000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000030000000F00000028B9875BFFCECBC8FF0B7317FF3B8342FFDDD9
          D5FFF7F2EEFFFBF6F0FFFBF5EFFFFBF4EDFFFAF3ECFFFAF2EBFFF9F1E9FFF9F0
          E8FFF9F0E6FFF8EFE5FFF8EEE4FFC18552FF0000002C0000000B000000000000
          0000000000000000000000000000000000010000000100000001000000010000
          00030001001305300A7A085613C6197D1DFF1A7E26FF0B7518FF109536FF3D85
          43FFDEDAD7FFF8F3EEFF47B3E5FF0095DBFF0090D5FF008CCEFF0089C8FF46A5
          D0FFF9F0E8FFF9F0E7FFF8EFE5FFC28752FF0000002B0000000B000000000000
          000000000001000000020000000300000006000000080000000A0000000C0001
          001A0748109B25A13EFF23AF50FF44C676FF67CD8EFF81D9A3FF87DCA9FF1F8D
          36FF448C4BFFF1ECE8FF009CE5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0088
          C8FFF9F1E9FFF9F0E8FFF9F0E7FFC28754FF0000002A0000000B000000000000
          0001000000040000000900000011000000190000002100000028000000320636
          0C8A2DA746FF33BF65FF2DB052FF219634FF0C831CFF0C801AFF49AD61FF418C
          48FFE3E1DDFFF8F4F0FF41B5EBFF009BE4FF0097DEFF0093D8FF008FD1FF41A6
          D4FFFAF2EBFFF9F1E9FFF9F0E8FFC38955FF000000290000000A000000010000
          0005000000100000001F0A162059152F438C1F4361B5265479D32A5C89EA1884
          45FD59C678FF17B048FF147838FA8C884DFFDBD9D7FF0C831CFF44954DFFE8E4
          E1FFFAF6F3FFFCF9F5FFFCF8F4FFFCF7F3FFFCF7F2FFFBF6F0FFFBF5EFFFFBF4
          EEFFFAF3ECFFFAF2EBFFF9F1E9FFC48A57FF000000280000000A000000030000
          000E0C182251224563AF4D85B2F86BA3C9FF85B9D8FF95C9E4FF82BEDEFF19A3
          35FF7ADDA2FF13A12FFF28828BFFCB9667FFF3F1EEFF4CA056FFEBE9E6FFE0B6
          94FFE0B693FFDEB491FFDDB28EFFDBAF8CFFDAAE8AFFD9AC88FFD8AB87FFD7AA
          85FFFBF4EEFFFAF3ECFFFAF2EBFFC48B58FF000000260000000A000000060000
          0017285172BD77B4D7FFA7DEF4FFB8E9FAFFBEEBFBFFB8E8FAFF90CCE8FF1BA6
          39FF7ADFA4FF24AA3CFF3789BEFFD09A6BFFFAF7F4FFF7F4F0FFFBF8F5FFFDFA
          F8FFFDFAF7FFFDFAF6FFFDF9F6FFFDF9F5FFFCF8F4FFFCF7F3FFFCF7F2FFFBF6
          F0FFFBF5EFFFFBF4EEFFFAF3ECFFC58B58FF0000002500000009000000070000
          001E3D7AADFFA1DFF6FFAEE4F8FFB9E9FAFFBEEBFBFFB8E8FBFF91D0ECFF1CAA
          3CFF12A528FF11A226FF388DC4FFD19C6DFFFCF7F3FFFCF7F3FF47B2E4FF0094
          DAFF0092D6FF008FD1FF008CCEFF47A9D6FFFDF9F6FFFCF8F4FFFCF7F3FFFCF7
          F2FFFBF6F0FFFBF5EFFFFBF4EEFFC68C59FF0000002300000009000000070000
          001E3F7CADFFA1DFF5FFADE4F7FFB8E9FAFFBEEBFBFFB9E8FBFF95D1EEFF7FC5
          E7FF62B3DEFF4CA1D5FF3990CAFFD29C6DFFFBF4EDFFFBF4EEFF009AE1FFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF008CCEFFFDF9F6FFDFB593FFDFB592FFDFB4
          91FFFCF7F2FFFBF6F0FFFBF5EFFFC68D5BFF0000002200000009000000070000
          001C407DAEFFA1DFF6FFA3DCF2FF9AD3ECFF8CC5E3FF76B6D9FF60A5CEFF529A
          C6FF4C94C2FF4891BFFF418AB9FFD39D6EFFF9EDE5FFF8ECE2FF40B0E3FF0099
          E1FF0096DDFF0093D9FF0091D4FF41A9DAFFFDFAF7FFFDF9F6FFFDF9F6FFFCF8
          F5FFFCF8F3FFFCF7F2FFFBF6F1FFC78E5CFF0000002100000008000000060000
          001A4D8BB9FF85C5E4FF74B6D9FF83C0E0FF93CBE6FF9DD3EDFF89CAE9FF7EC6
          E9FF64B6E3FF4CA3D8FF3A91CAFFD49E6FFFF6E7D9FFF6E3D6FFF5E4D5FFF6E6
          D9FFF8ECE1FFFAF2ECFFFCF8F3FFFDFAF7FFFDFAF7FFFDFAF7FFFDFAF6FFFDF9
          F6FFFCF8F4FFFCF8F3FFFCF7F2FFC88F5DFF0000001F00000008000000050000
          00175D9EC9FF86C7E5FFA9E0F6FFB9E9FBFFBEEBFBFFB8E8FAFF94D2EFFF81C8
          EBFF64B6E2FF4EA5DAFF3B94D0FFD49F71FFD7A375FFD39D6DFFCC9464FFC68D
          5AFFF6E6D9FFE7C1A0FFE7C0A0FFE6BF9EFFE5BF9EFFE5BE9DFFE5BD9CFFE4BD
          9CFFFDF9F6FFFCF8F5FFFCF8F4FFC9905FFF0000001D00000008000000050000
          00154580B3FFA1DFF6FFADE4F9FFB8E9FBFFBEEBFBFFB8E8FAFF94D2EFFF81C8
          EBFF64B6E3FF4EA6DAFF3C97D2FF82949DFFE9CEB7FFFFFFFFFFFFFFFFFFC890
          5EFFF5E3D5FFF8ECE2FFFBF5EFFFFCF8F4FFFDFAF8FFFDFBF8FFFDFAF8FFFDFA
          F7FFFDFAF7FFFDF9F6FFFCF9F5FFCA9260FF0000001C00000007000000040000
          00134682B4FFA1DFF6FFAEE4F8FFB9E9FAFFBEEBFBFFB9E8FAFF95D2EEFF81C8
          EAFF64B6E3FF4EA6DBFF3C97D4FF308BCBFF7F919BFFE9CEB7FFFFFFFFFFCB93
          62FFF6E4D6FFF8ECE2FFFBF4EEFFFDF8F4FFFDFAF7FFFDFBF9FFFDFBF8FFFDFA
          F8FFFDFAF7FFFDFAF7FFFDF9F6FFCA9362FF0000001A00000007000000040000
          00104784B4FFA1DFF5FFA3DCF2FF9BD3ECFF8CC5E3FF77B6D9FF60A4CDFF5299
          C6FF4C94C2FF4992C1FF448FC1FF3D8BC0FF3686BEFF839097FFE9CEB7FFCD97
          66FFF6E7D9FFF9EEE5FFFBF3EEFFFCF7F3FFFDFAF7FFFEFBF9FFFDFBF9FFFDFB
          F8FFFDFAF8FFFDFAF7FFFDFAF7FFCB9363FF0000001700000006000000030000
          000E5290BDFF85C5E4FF74B6D9FF83C0E0FF93CBE6FF9DD3ECFF8AC9E8FF7EC5
          E9FF64B6E3FF4DA4D9FF3D95D1FF338BC8FF3083C1FF307FB8FF808B90FFD49F
          70FFD49E70FFD39E6FFFD29D6DFFD19C6DFFD19B6BFFD09A6AFFD0996AFFCF98
          68FFCE9767FFCE9666FFCD9565FFCC9464FF0000001000000004000000030000
          000C5FA0CAFF86C7E5FFA9E0F5FFB9E9FAFFBEEBFBFFB8E8FAFF94D2EFFF81C8
          EBFF64B7E2FF4EA6DBFF3D98D5FF328ECFFF2E88CCFF3387CAFF3788C8FF3382
          BCFF256DA3FF0000002B000000120000000C0000000C0000000D0000000E0000
          000F0000001000000011000000110000000E0000000700000002000000020000
          000A4B88B7FFA1E0F6FFADE5F8FFB8E9FAFFBEEBFBFFB8E8FAFF94D2EEFF81C8
          EAFF64B6E2FF4FA6DAFF3C98D4FF318ECFFF2E88CCFF3388CBFF398BCCFF4292
          CFFF3875A7FF000000230000000B000000030000000300000003000000030000
          0003000000040000000400000004000000030000000100000001000000020000
          00084D89B9FFA1E0F6FFADE4F9FFB8E9FBFFBEEBFBFFB8E8FAFF94D2EEFF82C8
          EAFF65B7E3FF4EA6DAFF3C98D5FF318ECFFF2E88CCFF3388CBFF398BCCFF4293
          D0FF3B77AAFF0000001F00000008000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000020000
          00074F89BAFFA1DFF6FFA7DFF6FFA4DAF2FF99D1EBFF88C5E3FF73B6DAFF68AC
          D3FF5FA4CDFF579DC9FF4D97C6FF4592C5FF3E8DC5FF3A8AC6FF3B8AC9FF4292
          D0FF3C79ABFF0000001D00000008000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          00065C97C4FF93D1EDFF90CEEBFF9BD8F2FFA4E0F7FFA7E4FBFFA5E4FCFFA6E6
          FEFFA9E8FFFFA6E5FDFF9FDEF9FF93D2F2FF82C3E7FF6FB1DAFF569BC9FF3987
          BFFF3A7AADFF0000001A00000007000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000010000
          000473B0D6FF9DDDF5FFA8E8FFFFA8E8FFFFA8E8FFFFA8E8FFFFA8E8FFFFAAE8
          FFFFADE8FFFFB0E9FFFFB5E9FFFFBAEAFFFFBFEBFFFFC4EDFFFFCAEDFFFF84BB
          E1FF2E77ADFF0000001700000006000000010000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0002518BBDFF94D2EFFFA8E8FFFFA8E8FFFFA8E8FFFFA9E8FFFFACE8FFFFAFE8
          FFFFB2EAFFFFB7EBFFFFBCECFFFFC2ECFFFFC7EDFFFFCCEDFFFFD2EEFFFF8CBD
          E1FF407DAFFF0000001100000004000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0001111C263530526D966FAACDF68EC8E6FF9FD9F3FFA9E2F9FFACE5FCFFB3E8
          FEFFBBECFFFFBBE9FDFFB3E1F9FFA6D4F1FF93C5E7FF7EB1D8FF6195C0F72749
          669F0D1923420000000800000002000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000100000002101B243321384B692E516C973A6689BF42769FDD4982
          AFF34C87B6FE4780ADF33F739DDE356185C1294C699C1C3448700D19223D0000
          000B000000060000000300000001000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000C18F67FFC18F
          67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F
          67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F
          67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F
          67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFC18F67FFFFE9
          D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9
          D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9
          D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9
          D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8E9798FF6D7272FF595B5CFF4C4D
          4DFF444444FF3F3F3FFF3F3F3FFF474747FF555656FF6B6D6DFF888D8DFFA9B1
          B3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF878E8FFF525353FF4F4F4FFF5A5A5AFF676767FF7777
          77FF8C8C8CFF9F9F9FFFAEAEAEFFB2B2B2FFB1B1B1FFA5A5A5FF949494FF7575
          75FF696A6AFFA9B0B1FFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF7A7A7AFF707070FF696969FF717171FF7A7A7AFF8686
          86FF929292FF9D9D9DFFABABABFFB9B9B9FFC8C8C8FFD9D9D9FFE4E4E4FFEEEE
          EEFFCCCCCCFF484848FFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF767676FFF1F1F1FF626262FF757575FF7E7E7EFF8B8B
          8BFF979797FFA3A3A3FFB1B1B1FFC0C0C0FFCDCDCDFFDBDBDBFFE5E5E5FFEDED
          EDFFF6F6F6FF525252FFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF737373FFE7E7E7FFEFEFEFFF545454FF868686FF9191
          91FF9D9D9DFFAAAAAAFFB8B8B8FFC6C6C6FFD5D5D5FFE1E1E1FFE9E9E9FFF0F0
          F0FFF8F8F8FF5B5B5BFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF717171FFDCDCDCFFE4E4E4FFEBEBEBFF494949FF9797
          97FFA3A3A3FFB0B0B0FFBFBFBFFFCDCDCDFFDBDBDBFFE6E6E6FFEDEDEDFFF4F4
          F4FFFAFAFAFF656565FFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF717171FFCECECEFFD7D7D7FFDFDFDFFFE5E5E5FF4141
          41FFAAAAAAFFB8B8B8FFC6C6C6FFD3D3D3FFE0E0E0FFE9E9E9FFF1F1F1FFF6F6
          F6FFFCFCFCFF6D6D6DFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF717171FFC1C1C1FFCACACAFFD2D2D2FFD9D9D9FFDEDE
          DEFF414141FFBFBFBFFFCCCCCCFFD9D9D9FFE4E4E4FFEEEEEEFFF4F4F4FFF8F8
          F8FFFCFCFCFF757575FFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF727272FFB4B4B4FFBCBCBCFFC4C4C4FFCACACAFFD0D0
          D0FF85A7CBFF1653A1FF8E8E8EFF939393FF9D9D9DFFABABABFFBDBDBDFFD4D4
          D4FFF0F0F0FF7C7C7CFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFF0554AEFF1A6DBEFF2876C0FF4A91CDFF6FAAD7FF97C2
          DFFFFFFFFFFFFFFFFFFF707578FF8E9DABFF7A98B5FF5C88B8FF3D73B5FF1E58
          A8FF114FA6FF01328DFFFFFFFFFFFFFFFFFFCED9DBFFC5CDCEFFBABEBFFFA9AA
          ABFFA2A2A2FF838383FFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFF065CB6FF63C9F7FF43A9E5FF2C8DD4FF1C75C4FF1061
          B5FF0951A8FF04469FFF04439CFF0748A0FF0C51A9FF1360B6FF1B72C7FF2589
          DCFF30A4F4FF012F8AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFC18F67FFFFE9
          D9FFFFFFFFFFFFFFFFFF0664BDFF88E0FEFF6BD6FEFF65D2FEFF5FCEFEFF58CA
          FEFF50C7FEFF49C3FEFF41BFFEFF3ABAFEFF39B8FFFF37B6FFFF36B4FFFF35B3
          FFFF35B1FFFF01328DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9D9FFC18F67FFDA9451FFDA94
          51FFDA9451FFDA9450FF076BC4FFA2E9FEFF74DCFEFF6AD8FEFF5FD2FEFF58CE
          FEFF51CAFEFF4BC7FEFF45C3FEFF40C0FEFF3DBDFEFF3ABAFEFF39B8FFFF37B6
          FFFF36B4FFFF023792FFDA9450FFDA9550FFDA9450FFDA9450FFDA9450FFDA94
          50FFDA9450FFDA9451FFDA9450FFDA9450FFDA9450FFDA9450FFDC9857FFFBEA
          D9FFFBEAD9FFFBEAD9FF0873CCFFB9F1FFFF83E4FEFF78DFFEFF6EDBFEFF64D6
          FEFF5BD2FEFF54CEFEFF4ECAFEFF48C6FEFF43C2FEFF3FBFFEFF3CBDFEFF3ABA
          FEFF3EBAFFFF023D97FFFBEAD9FFFBEAD9FFFBEAD9FFFBEAD9FFFBEAD9FFFBEA
          D9FFFBEAD9FFFBEAD9FFFBEAD9FFFBEAD9FFFBEAD9FFDC9856FFDF9D5DFFFAE3
          CCFFFAE3CCFFFAE3CCFF097BD4FFCCF5FEFF92EAFEFF87E7FFFF7EE3FEFF73DD
          FEFF69DAFEFF5ED5FEFF57D1FEFF51CDFEFF4BC9FEFF46C5FEFF41C2FEFF3DBF
          FEFF4EC2FEFF03439DFFFAE3CCFFFAE3CCFFFAE3CCFFFAE3CCFFFDF1E6FFFDF1
          E6FFFAE3CCFFFDF1E6FFFDF1E6FFFAE3CCFFFAE3CCFFDF9D5DFFE1A163FFF9DB
          BEFFF9DBBDFFF9DABDFF0982DAFFDBF9FFFF9CEDFFFF93EAFEFF8AE8FEFF82E5
          FEFF76E1FEFF6CDBFEFF63D8FEFF5AD3FEFF54CFFEFF4ECCFEFF49C8FEFF44C4
          FEFF62CCFEFF044AA4FFF9DBBDFFF9DBBDFFF9DBBDFFF9DABDFFC4803EFFC581
          40FFFCEDDEFFC88544FFCA8746FFF9DBBEFFF9DBBDFFE1A163FFE4A669FFF8D2
          AFFFF8D2AFFFF8D2AFFF0A89E1FFA8DDF7FFD0F7FFFFBDF3FFFFABEFFFFF9AEB
          FEFF8BE8FEFF7CE4FEFF72E0FEFF6DDCFEFF6BD9FEFF6CD8FEFF6FD7FEFF72D6
          FEFF56B5EBFF0450AAFFF8D2AFFFF8D2AFFFF8D2AFFFF8D2AFFFF8D2AFFFBE78
          37FFBF7A39FFC07C3AFFF8D2AFFFF8D2AFFFF8D2AFFFE4A669FFE7AB70FFF6CA
          A0FFF6CAA1FFF6CAA1FF8ACCEFFF36A4E9FF43AAEAFF69BEEFFF8DCFF3FFACE0
          F7FFC0ECFBFFCCF4FEFFC4F1FEFFACE5FAFF8CD3F4FF66BCEAFF42A3DEFF2586
          CEFF2079C6FF6DAFDBFFF6CAA0FFF6CAA0FFF6CAA0FFF6CAA0FFF6CAA0FFB873
          32FFB87332FFB97534FFF6CAA0FFF6CAA1FFF6CAA1FFE7AB70FFE9AF76FFFBE2
          CAFFFBE2CAFFFBE2CAFFFBE2CAFFFBE2CAFF92CFEFFF66BDEDFF44ABE9FF299A
          E4FF168CDDFF0C82D8FF0C7ED5FF1382D4FF218AD5FF3597D8FF52A7DDFF7CBE
          E3FFFBE2CAFFFBE2CAFFFBE2CAFFFBE2CAFFFBE2CAFFFBE2CAFFB3702FFFB370
          31FFFBE2CAFFB47130FFB57131FFFBE2CAFFFBE2CAFFE9AF76FFECB37CFFFBE8
          D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8
          D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8
          D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8
          D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFFBE8D4FFECB37CFFD3AA84EFF9E1
          CBFFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEF
          E2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEF
          E2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEF
          E2FFFCEFE2FFFCEFE2FFFCEFE2FFFCEFE2FFF9E1CBFFD3AA84EF4B40368ED5AE
          89EFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB
          88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB
          88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFF0BB
          88FFF0BB88FFF0BB88FFF0BB88FFF0BB88FFD5AE89EF4B40368E000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          BE000000424DBE000000000000003E0000002800000020000000200000000100
          010000000000800000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000300000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000093000000F90000005400000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000305050575000000000000000000000000000000000000
          00000000000001010193030303FF010101FF000000F900000051000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000101013C1B1B1BFF0606067E0000000000000000000000000000
          000004040496080808FF060606FF040404FF020202F600000046000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000A0A0A87202020FF1D1D1DFF0606067E00000000000000000606
          06960F0F0FFF0C0C0CFF0A0A0AFF080808F60000004800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001B1B1BD2252525FF222222FF1F1F1FFF0606067E080808961616
          16FF131313FF111111FF0D0D0DF6010101480000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000001E2D2D2DFF2A2A2AFF272727FF242424FF212121FF1E1E1EFF1B1B
          1BFF181818FF141414F601010148000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000009070635514136A092745BDCAD8462F7AD7C56FEAB7B55FEA98160F78E70
          59DC493F37C7323232FF2F2F2FFF2C2C2CFF292929FF262626FF232323FF2020
          20FF1B1B1BF60202024800000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000025442
          34A2AD8460F2D0AA8BFFE2C7B2FFF1DFD2FFFBF0E8FFFBF0E8FFF0DED1FFDEC4
          B1FF635850FF363636FF343434FF313131FF2E2E2EFF2B2B2BFF282828FF2525
          25FF0D0D0DA20000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000040302237F6147C8D3A9
          85FFEDD4BFFFFEF1E6FFFFF2E7FFFFF3E8FFFFF2E7FFFFF2E7FFFFF3E8FFF7EC
          E1FF464545FF3D3D3DFF393939FF353535FF333333FF303030FF2D2D2DFF2A2A
          2AFF272727FF0808087E00000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000002816044C8DFB591FFFCE7
          D7FFFCEADAFFFEECDFFFFFEEE2FFFFF0E5FFFFF0E5FFFFF0E5FFFFF0E5FFC9BE
          B5FF474747FF444444FF414141FF3D3D3DFF3A3A3AFF363636FF343434FF3030
          30FF2C2C2CFF292929FF0909097E000000000000000000000000000000000000
          00000000000000000000000000000000000057412CA2DAA777FFFAE2CCFFFBE3
          CDFFFCE7D4FFFEECDCFFFFEDDFFFFFEFE2FFFFEFE2FFFFEFE2FFFFEFE2FF9992
          8CFF4E4E4EFF4C4C4CFF4A4A4AFF454545FF434343FF404040FF3C3C3CFF3939
          39FF353535FF303030FF262626EA020202410000000000000000000000000000
          000000000000000000000000000009070435BE8652F2EEC9A7FFF9DEC5FFFBE1
          C9FFFCE6D2FFFEEADAFFFFEBDDFFFFEDE0FFFFEDE0FFFFEDE0FFFFEDE0FF6B69
          68FF575757FF545454FF525252FF4F4F4FFF4C4C4CFF494949FF454545FF4242
          42FF2A2A2AD20808086300000009000000000000000000000000000000000000
          0000000000000000000000000000583F29A0DFA671FFF9DCC1FFF9DABEFFFBE0
          C8FFFCE5D0FFFEE9D7FFFFEADAFFFFECDDFFFFECDDFFFFECDDFFE3D3C7FF5E5E
          5EFF5C5C5CFF5C5C5CFF595959FF585858FF545454FF515151FF524B44E30505
          0542000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000A57548DCEABB8EFFF9D8BCFFF9DABDFFFBDF
          C6FFFCE4CEFFFEE8D5FFFFE9D8FFFFEBDBFFFFEBDBFFFFEBDBFFB7ADA4FF6464
          64FF636363FF626262FF616161FF666564FF9E9287FFC8A88BFF906A4EDC0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000CE8E52F7F3CBA6FFF8D6B7FFF9D9BBFFFBDD
          C4FFFCE2CBFFFEE6D3FFFFE8D5FFFFE9D8FFFFE9D8FFFFE9D8FF908B87FF6C6C
          6CFF6A6A6AFF797674FFB8A99CFFF0D3BBFFF9D8BCFFECC9ABFFAE8058F70000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000D7924FFEF8D9BDFFF8D3B2FFF9D7B9FFFBDC
          C1FFFCE0C8FFFEE4CFFFFFE6D2FFFFE7D4FFFFE7D4FFFEE7D4FF9A938EFF918C
          88FFCEBDAFFFFCE0C9FFFCDFC6FFFADABEFFF9D6B7FFF7DAC1FFB37F55FE0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000D89453FEF8E0C9FFF9DBBFFFFADABEFFFADB
          BFFFFCDEC5FFFDE2CBFFFEE4CEFFFEE5D0FFFEE5D0FFFEE5D0FFFCE4CFFFFEE4
          CDFFFDE2CAFFFCE0C7FFFBDEC3FFFADCC2FFFADEC3FFF7E1CCFFB68156FE0000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000D1965CF7F4D6BAFFFAE3CDFFFBE4CEFFFBE4
          CFFFFDE4CFFFFDE4CFFFFEE4CEFFFEE3CDFFFEE2CCFFFEE2CCFFFEE3CDFFFEE4
          CEFFFDE4CEFFFDE4CFFFFCE6D1FFFBE5D1FFFBE4D0FFEFD4BBFFB7865CF70000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000A98055DCEEC7A3FFFBE6D3FFFCE5D2FFFCE8
          D6FFFDE9D9FFFDECDDFFFEECDEFFFEEDDFFFFEEDDFFFFEEDDFFFFEEDDFFFFEEC
          DEFFFDEBDCFFFDEADBFFFDE9D8FFFCE7D5FFFBE8D6FFE5C2A2FF987453DC0000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000005B4733A0E8B687FFFCEADAFFFCE8D7FFFDEA
          DAFFFDEBDDFFFEEDDFFFFEEEE0FFFEEEE1FFFEEEE1FFFEEEE1FFFEEEE1FFFEEE
          E0FFFEEDDFFFFDECDDFFFDEBDCFFFCE9D9FFFCEBDCFFD9AB82FF544132A00000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000A080635CC9764F2F5DAC0FFFCECDEFFFCEC
          DDFFFDEDDFFFFDEEE2FFFEEFE2FFFEEFE3FFFEEFE3FFFEEFE3FFFEEFE3FFFEEF
          E2FFFDEEE2FFFDEDE0FFFDEDDFFFFCEDDFFFF1D6BEFFBB8859F2090706350000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000005E4A38A2E9BB8FFFFDF0E5FFFCEF
          E2FFFDEFE3FFFDF0E5FFFEF0E5FFFEF1E6FFFEF1E6FFFEF1E6FFFEF1E6FFFEF0
          E5FFFDF0E4FFFDEFE4FFFDF0E4FFFDF1E6FFDFB083FF594433A2000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000028E7052C8EFC9A6FFFDF3
          EAFFFDF2E9FFFDF2E8FFFEF2E8FFFEF2E9FFFEF2E9FFFEF2E9FFFEF2E9FFFEF2
          E8FFFDF2E7FFFDF3E9FFFDF4EBFFE9C09BFF876748C800000002000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000040303238E7153C8EABE
          94FFF6E0CCFFFDF5EEFFFEF5EDFFFEF5EDFFFEF4ECFFFEF4ECFFFEF5EDFFFEF5
          EDFFFDF5EEFFF4DDC8FFE5B587FF8B6948C80403022300000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000025E4C
          3BA2CD9B6AF2EABE94FFF2D4B8FFF8E7D6FFFCF3EBFFFCF3EBFFF7E6D5FFF0D1
          B4FFE6B88BFFC8925CF25C4835A2000000020000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000A0807355C4B3AA0AB8661DCD49D68F7DB9A5CFEDB985AFED29A63F7A981
          5BDC5B4737A00A08063500000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          BE000000424DBE000000000000003E0000002800000020000000200000000100
          010000000000800000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end
      item
        Image.Data = {
          36100000424D3610000000000000360000002800000020000000200000000100
          2000000000000010000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00080101014204040476070707A40A0A0AC90C0C0CE60E0E0EF90D0D0DF90A0A
          0AE6070707C9030303A401010176000000420000000800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000026050505751212
          12C5191919FF171717FF171717FF151515FF141414FF131313FF111111FF1010
          10FF0F0F0FFF0E0E0EFF0D0D0DFF0C0C0CFF0A0909C502020275000000260000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000B0606066D201F1FD6212121FF2020
          20FF1F1F1FFF1D1D1DFF1C1C1CFF1B1B1BFF191919FF181818FF171717FF1616
          16FF141414FF131313FF121212FF111111FF0F0F0FFF0F0F0FFF121110D60202
          026D0000000B0000000000000000000000000000000000000000000000000000
          000000000000000000000101012B141414A22A2A2AFF292929FF272727FF3F3D
          3CFF6A6562FF908883FFB1A7A0FFCCC1B8FFE2D4CBFFEFE1D6FFEFE1D6FFE0D4
          CAFFCABEB5FFADA39BFF89827AFF5F5A56FF302E2DFF141414FF121212FF1111
          11FF0A0909A20000002B00000000000000000000000000000000000000000000
          00000000000001010130212121B9323232FF313131FF6C6966FFA7A09AFFDDD2
          CAFFF5E9E0FFF5E9DFFFF5E8DEFFECCEADFFE3B780FFDBA45CFFDAA25BFFE0B2
          7AFFE8CAA7FFF4E6DAFFF4E6DBFFF3E5DBFFD9CCC3FF9D958EFF5C5753FF1818
          18FF151515FF111010B900000030000000000000000000000000000000000000
          0000000000191B1B1BA93A3A3AFF6B6967FFB3ACA8FFF7EDE5FFF7ECE4FFF6EC
          E4FFF6EBE3FFF6EBE2FFEAC89EFFDEA458FFDCA155FFDA9F52FFD99D50FFD79C
          4EFFD69B4CFFE5C095FFF5E8DDFFF4E7DEFFF4E7DDFFF4E7DDFFF4E6DCFFA79E
          98FF55524FFF191919FF0C0C0CA9000000190000000000000000000000000000
          00000C0C0C6E424242FE9F9B98FFE2DBD5FFF8F0E9FFF8EFE9FFF8EFE8FFF8EF
          E8FFF7EEE7FFF0D6B7FFE2A95EFFE0A65BFFDEA559FFDDA356FFDBA054FFDA9E
          51FFD89C4FFFD79A4CFFEACDABFFF5EAE1FFF5E9E0FFF5E9E0FFF5E9DFFFF5E8
          DFFFDBD0C7FF8C8581FF1E1E1EFE0505056E0000000000000000000000000000
          00000A0A0A61696765D2FAF3EEFFFAF3EEFFF9F2EDFFF9F2EDFFF9F2ECFFF9F1
          ECFFF9F1EBFFECC491FFE4AC62FFE3AA5FFFE1A75DFFDFA55AFFDEA457FFDCA2
          55FFDA9F52FFD99E50FFE2B680FFF7ECE4FFF6ECE4FFF6EBE3FFF6EBE3FFF6EB
          E2FFF6EAE2FFF6EAE1FF5A5654D2050505610000000000000000000000000000
          000000000000CDCAC7E7FBF6F2FFFBF6F2FFFBF5F1FFFAF5F1FFFAF4F0FFFAF4
          EFFFFAF4EFFFEAB776FFE7AF66FFE5AD63FFE3AB61FFE2A95EFFE0A75BFFDEA5
          59FFDDA256FFDBA053FFDCA660FFF8EFE8FFF8EFE8FFF7EEE7FFF7EEE7FFF7ED
          E6FFF7EDE5FFF7ECE5FFCAC2BBE7000000000000000000000000000000000000
          000000000000CECCCAE7FCF8F6FFFCF8F5FFFCF8F5FFFCF7F4FFFBF7F4FFFBF7
          F3FFFBF6F2FFEDBA79FFE9B26AFFF0CA9AFFF6E3CAFFEDC796FFE2A95FFFE1A7
          5CFFDFA55AFFDDA457FFDFA964FFF9F2ECFFF9F1ECFFF9F1EBFFF8F0EAFFF8F0
          EAFFF8F0E9FFF8EFE9FFCBC4BEE7000000000000000000000000000000000000
          00000D0D0D61727170D2FDFBF9FFFDFAF8FFFDFAF8FFFDFAF7FFFCF9F7FFFCF9
          F6FFFCF9F6FFF2CC9DFFECB56EFFF8E5CDFFF9EBD9FFF7E4CBFFE5AC63FFE3AB
          60FFE1A85EFFE0A65AFFE7BF8BFFFAF4F0FFFAF4EFFFFAF4EFFFFAF3EEFFF9F3
          EEFFF9F2EDFFF9F2ECFF625F5ED2070707610000000000000000000000000000
          00001212126E606060FEB1B0B0FFEBE9E8FFFEFCFBFFFEFCFAFFFDFBFAFFFDFB
          FAFFFDFBF9FFF8E4C9FFEEB771FFF2CE9EFFF8E5CDFFF0CB9BFFE7B067FFE6AD
          64FFE4AC62FFE2A95FFFF2DABEFFFBF7F4FFFBF7F3FFFBF6F2FFFBF6F2FFFBF5
          F1FFE4DFDCFF9F9C9AFF3E3E3EFE0B0B0B6E0000000000000000000000000000
          0000000000192E2E2EA9646464FF8E8E8EFFC7C7C6FFFEFEFDFFFEFDFDFFFEFD
          FCFFFEFDFCFFFEFCFAFFF7DBB8FFEEB972FFEDB76FFFEBB56DFFEAB36BFFE8B0
          68FFE6AF65FFF1D3AEFFFDFAF6FFFCF9F7FFFCF9F6FFFCF9F6FFFCF8F6FFBCBA
          B8FF787676FF464646FF1F1F1FA9000000190000000000000000000000000000
          000000000000030303303B3B3BB9686868FF686868FF969696FFC2C2C2FFECEC
          EBFFFFFEFEFFFFFEFEFFFEFEFCFFF9E6CCFFF4CFA1FFF0BE80FFEEBC7CFFF1CC
          9CFFF6E1C7FFFEFCFAFFFEFCFAFFFDFBFAFFE8E7E6FFB9B8B7FF868685FF5050
          50FF4D4D4DFF2D2C2CB902020230000000000000000000000000000000000000
          000000000000000000000303032B2E2E2EA26C6C6CFF6B6B6BFF696969FF7A7A
          7AFF9A9A9AFFB6B6B6FFCECECEFFE2E2E2FFF2F2F2FFFCFCFCFFFCFBFBFFF1F0
          F0FFE1E0E0FFCBCBCAFFB0B0AFFF919191FF6D6D6DFF575757FF565656FF5555
          55FF242424A20202022B00000000000000000000000000000000000000000000
          00000000000000000000000000000000000B1414146D535353D66E6E6EFF6D6D
          6DFF6C6C6CFF6B6B6BFF6A6A6AFF696969FF686868FF676767FF666666FF6565
          65FF636363FF626262FF616161FF5F5F5FFF5F5F5FFF5D5D5DFF474747D61010
          106D0000000B0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000002020226181818754545
          45C5707070FF6F6F6FFF6F6F6FFF6E6E6EFF6D6D6DFF6B6B6BFF6A6A6AFF6969
          69FF686868FF676767FF666666FF656565FF3D3D3DC515151575020202260000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000807070742181818762F2F2FA4464646C95B5B5BE66A6A6AF96A6A6AF95959
          59E6444444C92C2C2CA417171776070707420000000800000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
        Mask.Data = {
          BE000000424DBE000000000000003E0000002800000020000000200000000100
          010000000000800000000000000000000000020000000000000000000000FFFF
          FF00000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000}
      end>
  end
  inherited ilLargeColorSchemesGlyphs: TcxImageList
    FormatVersion = 1
  end
  inherited ilSmallColorSchemesGlyphs: TcxImageList
    FormatVersion = 1
  end
  object dsTemplate: TDataSource
    DataSet = cdsTemplate
    Left = 624
    Top = 200
  end
  object cdsTemplate: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    Left = 688
    Top = 200
    Data = {
      270000009619E0BD01000000180000000100000000000300000027000466616B
      6504000100000000000000}
    object cdsTemplatefake: TIntegerField
      FieldName = 'fake'
    end
  end
  object cdsMaster: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 688
    Top = 256
  end
  object cdsDetail: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 688
    Top = 312
  end
  object cdsCategories: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 776
    Top = 200
  end
  object dsMaster: TDataSource
    DataSet = cdsMaster
    Left = 624
    Top = 256
  end
  object dsDetail: TDataSource
    DataSet = cdsDetail
    Left = 624
    Top = 312
  end
  object LayoutLookAndFeels: TdxLayoutLookAndFeelList
    Left = 776
    Top = 256
    object LayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
    end
  end
end

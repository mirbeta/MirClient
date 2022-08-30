object dxRibbonCustomizationForm: TdxRibbonCustomizationForm
  Left = 246
  Top = 141
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Ribbon Customization'
  ClientHeight = 582
  ClientWidth = 739
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 618
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 739
    Height = 582
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    CustomizeFormTabbedView = True
    object cbCommands: TcxComboBox
      Left = 10
      Top = 28
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbCommandsPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 326
    end
    object btnMoveUp: TcxButton
      Left = 704
      Top = 241
      Width = 25
      Height = 25
      Action = acMoveUp
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E0000000000000000000000000000000000000000
        000000000000000000020000000A0000000F00000010000000100000000F0000
        000A000000020000000000000000000000000000000000000000000000000000
        000000000000000000097E5237C1A8663FFFA7633DFFA6623BFFA5603AFF7544
        29C1000000090000000000000000000000000000000000000000000000000000
        0000000000000000000CB57852FFE6C69AFFE2BE8EFFE2BE8EFFE2BE8DFFA661
        3CFF0000000D0000000000000000000000000000000000000000000000000000
        0000000000000000000CB67B55FFE6C79DFFDAAD73FFD9AD73FFE3C08FFFA865
        3EFF0000000D0000000000000000000000000000000000000000000000020000
        00080000000B00000013B87F59FFE7C79DFFDBAF77FFDBAE77FFE4C191FFA968
        42FF000000150000000C0000000A00000003000000000000000000000007B77D
        56FFB67B54FFB47953FFB37751FFE5C495FFDCB279FFDCB17AFFE4C295FFAB6B
        46FFAA6A44FFAA6842FFA86640FF000000080000000000000000000000057E57
        3DB5E7D3C4FFEACEA8FFE6C69AFFE5C598FFDDB37DFFDDB37DFFE5C497FFE5C5
        97FFE7C79CFFDDC5B7FF673923B600000006000000000000000000000002140E
        0A24B78869F2F2E5D2FFE6C799FFE7C997FFEACF9FFFEACF9EFFE7C997FFE3C1
        8FFFEFDFCBFF9D6549F310090626000000020000000000000000000000000000
        00034E372871D7B7A0FFF5E9CDFFEEDAABFFEDD7A8FFEDD7A8FFEED7A8FFF4E5
        C4FFC7A08CFF4126187200000004000000000000000000000000000000000000
        00010000000590684BC7EEE0D3FFF3E5BFFFF1DFB2FFF1DFB2FFF2E2B7FFE8D8
        CBFF7B4C30C70000000600000001000000000000000000000000000000000000
        0000000000011D15102DC4997AFAFBF4E4FFF5E8C0FFF4E6BBFFF9F2E0FFB07D
        5FFA19100A2F0000000100000000000000000000000000000000000000000000
        000000000000000000025F453383DFC5B1FFFBF2D7FFFAF2D4FFD3B6A2FF5335
        2383000000030000000000000000000000000000000000000000000000000000
        0000000000000000000005030309A37C5CD8F4EBDFFFF1E6DAFF936245D80403
        020A000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000012E231A3FCFAB8EFDC09679FD291C13400000
        0001000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000017458429867473195000000010000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000100000001000000000000
        00000000000000000000000000000000000000000000}
      PaintStyle = bpsGlyph
      TabOrder = 11
    end
    object btnMoveDown: TcxButton
      Left = 704
      Top = 272
      Width = 25
      Height = 25
      Action = acMoveDown
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E0000000000000000000000000000000000000000
        000000000000000000000000000000000003000000090000000A000000030000
        0000000000000000000000000000000000000000000000000000000000000000
        00000000000000000000000000010000000D552C19A64E2916A40000000D0000
        0001000000000000000000000000000000000000000000000000000000000000
        000000000000000000000000000623130B54A77053FD9E6443FD20110A540000
        0006000000000000000000000000000000000000000000000000000000000000
        0000000000000000000204020117814A2EDEDFBF9BFFD4AC7EFF794226DF0302
        0118000000020000000000000000000000000000000000000000000000000000
        000000000001000000094D2D1C90C69D7FFFDFB987FFDFB785FFBA875FFF4726
        17910000000A0000000100000000000000000000000000000000000000000000
        000000000004180F0A3AA87252FBE5C89FFFDCB27AFFDCB17BFFE0BC8EFF9C61
        40FB160C073B0000000400000000000000000000000000000000000000000000
        00010000000C7E4E32CCDDBFA0FFDFB783FFDDB37DFFDDB37DFFDFB682FFD2AB
        80FF714027CD0000000D00000002000000000000000000000000000000000000
        0006452C1E78C69D7FFFE4C394FFDEB680FFDFB680FFDEB67FFFDEB57FFFE3BF
        8FFFB8855FFF3E2416790000000700000000000000000000000000000002120C
        0828AA7758F3E9CEAAFFE4C290FFE5C794FFE8CA98FFE8CA99FFE6C593FFE3C1
        8DFFE2C193FF996141F31009062A000000030000000000000000000000067953
        39B6E0C7ADFFF9F1E0FFFAF2E2FFFAF2E2FFEBD3A3FFECD2A3FFF1DDB7FFF0DD
        B7FFF9F1E0FFDBBEA6FF6B4028B800000007000000000000000000000006C190
        6AFFC18E68FFC08C66FFBE8A64FFFBF5E5FFEFDAACFFEFD9ACFFF3E2BEFFB980
        5AFFB77E58FFB77C55FFB57A54FF000000080000000000000000000000020000
        0006000000070000000DC08D68FFFBF7E9FFF2E1B4FFF2E1B4FFF5E9C5FFB277
        53FF000000100000000900000007000000020000000000000000000000000000
        00000000000000000007C3926CFFFCF8EBFFF5E8BCFFF5E7BCFFF8EECCFFB47C
        56FF000000080000000000000000000000000000000000000000000000000000
        00000000000000000005C59570FFFDFAEDFFFDFAEDFFFDFAEDFFFDFAEDFFB67F
        5AFF000000060000000000000000000000000000000000000000000000000000
        0000000000000000000393755CBACDA37EFFCBA17DFFCAA07BFFC99D79FF8C6A
        50BB000000040000000000000000000000000000000000000000000000000000
        0000000000000000000100000002000000040000000400000004000000040000
        00030000000100000000000000000000000000000000}
      PaintStyle = bpsGlyph
      TabOrder = 12
    end
    object btnRemove: TcxButton
      Left = 342
      Top = 272
      Width = 25
      Height = 25
      Action = acRemove
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E0000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000020000
        0008000000120000001200000005000000000000000000000000000000000000
        0000000000000000000000000000000000000000000200000006000000133218
        0C6E813C1CE0AC5A2DFF00000013000000000000000000000000000000000000
        0000000000000000000000000001000000050000000F231209527E4020D4B670
        42FFDAA66DFFAE5E30FF00000017000000000000000000000000000000000000
        000000000001000000030000000C180E083C733E24BFB66F42FFD6A46DFFE4B7
        7BFFE4B77BFFB06034FF00000016000000000000000000000000000000010000
        0002000000090F090529673E25A8B57548FDD7A674FFE7BF89FFE6BB80FFE5B8
        7CFFE6B97FFFB26537FF00000015000000000000000000000001000000070906
        041C5B3B2590B4774EF7D8A87CFFECCA9AFFEAC38EFFE9C089FFE8BD84FFE7BA
        81FFE7BE86FFB4693AFF0000001400000000000000000000000651392978B17B
        54ECD7AA82FFEFD1A9FFEFCFA0FFEDC996FFECC793FFEBC490FFE9C18AFFE7BE
        86FFE9C38FFFB56B3FFF00000013000000000000000000000009CB976FFFF1DC
        C1FFF3DCB8FFF3D8AEFFF0D2A4FFF0CFA0FFEFD0A1FFEFCF9FFFEDCC9BFFEDC9
        96FFEECDA0FFB87043FF0000001200000000000000000000000444332661AF81
        5BE1DDB693FFF5E0C3FFF8E3C3FFF6DFB9FFF4DAAFFFF2D5AAFFF0D2A4FFEECE
        9DFFF1D5ABFFBA7648FF00000011000000000000000000000001000000040504
        0311533D2B78BF8D67F1E3C2A2FFF8EACEFFF8E6C5FFF5DDB6FFF2D6ABFFF0D2
        A4FFF3DBB5FFBD784CFF00000010000000000000000000000000000000000000
        0001000000050C09061B6B4E3894C79772F8E6C9AAFFFAEBCEFFF6E2BEFFF2D7
        ACFFF5E0BCFFBF7C52FF0000000F000000000000000000000000000000000000
        000000000000000000010000000618120C2B7C583FA7D2A17BFFEBD2B4FFF8E6
        C8FFF8E7C9FFC18456FF0000000E000000000000000000000000000000000000
        00000000000000000000000000000000000200000007261C133E91674AC0D5A8
        81FFEFDABDFFC3875BFF0000000E000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000002000000093628
        1C54A37453D5C68C63FF0000000A000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000010000
        00030000000800000008000000020000000000000000}
      PaintStyle = bpsGlyph
      TabOrder = 3
    end
    object btnAdd: TcxButton
      Left = 342
      Top = 241
      Width = 25
      Height = 25
      Action = acAdd
      OptionsImage.Glyph.SourceDPI = 96
      OptionsImage.Glyph.Data = {
        424D360400000000000036000000280000001000000010000000010020000000
        000000000000C40E0000C40E0000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000040000
        000F0000000F0000000700000001000000000000000000000000000000000000
        00000000000000000000000000000000000000000000000000000000000EB068
        3FFF834324E131190D6A00000011000000060000000100000000000000000000
        000000000000000000000000000000000000000000000000000000000011B370
        45FFDEAF77FFB47247FF773C20D2201009500000000F00000005000000010000
        000000000000000000000000000000000000000000000000000000000010B676
        4BFFECC38FFFE8BA7DFFD8A66FFFAD693FFF6A371CBE170C06400000000D0000
        00040000000100000000000000000000000000000000000000000000000FBB7C
        51FFEFCD9CFFE9BB7FFFE8BC80FFEABE83FFD39D6CFFA7623BFD5B301AA90D07
        032C0000000B00000003000000010000000000000000000000000000000EBE82
        57FFF1D5ACFFEBC087FFE9BF85FFE8BD81FFE9BF82FFE9C087FFCD9765FF9F59
        33F74E29169408040221000000090000000200000000000000000000000DC085
        5EFFF5DEBBFFEDC68FFFEDC58DFFEBC289FFEAC084FFE8BC80FFE9BD82FFE8BB
        87FFC78E60FF965430EE4223137E0000000900000000000000000000000CC590
        67FFF9E9CFFFF3D4A3FFF2D09FFFF0CC98FFEEC890FFEBC088FFE9BE83FFEAC3
        8AFFECC693FFE3B889FFA65B33FF0000000D00000000000000000000000BC995
        6EFFFAEFDAFFF4DAAEFFF5D8AAFFF2D5A6FFF1D1A1FFF1D0A2FFF1D2A8FFEDCD
        A1FFCC956BFF9A5D39E33A2314680000000700000000000000000000000ACC9B
        73FFFCF4E3FFF8E0B7FFF6DDB4FFF6DEB4FFF7E2C0FFF6E1C1FFD9B28CFFB075
        4EF14D301F7E040302160000000600000001000000000000000000000009CDA0
        77FFFEF7E9FFFBE7C2FFFBEBCCFFFBEFD6FFE3C5A7FFC08B65F965442E960B07
        051E00000006000000020000000000000000000000000000000000000008D0A4
        7CFFFEFBEFFFFDF4E0FFEEDAC1FFD1A07DFF78553CA717100B2C000000070000
        000200000000000000000000000000000000000000000000000000000007D2A7
        7FFFF5EADBFFD8B291FF916D50BE261C143C0000000700000002000000000000
        000000000000000000000000000000000000000000000000000000000005D5AA
        83FFA98462D3372B204F00000007000000020000000000000000000000000000
        0000000000000000000000000000000000000000000000000000000000010000
        0004000000050000000200000001000000000000000000000000000000000000
        00000000000000000000000000000000000000000000}
      PaintStyle = bpsGlyph
      TabOrder = 2
    end
    object btnNewElement: TcxButton
      Left = 373
      Top = 504
      Width = 75
      Height = 25
      Caption = '&Add'
      DropDownMenu = bpmNewElement
      Kind = cxbkOfficeDropDown
      OptionsImage.Layout = blGlyphTop
      TabOrder = 8
    end
    object btnRename: TcxButton
      Left = 454
      Top = 504
      Width = 75
      Height = 25
      Action = acRename
      TabOrder = 9
    end
    object btnImportExport: TcxButton
      Left = 603
      Top = 504
      Width = 95
      Height = 25
      Caption = 'Im&port/Export'
      DropDownMenu = bpmImportExport
      Kind = cxbkOfficeDropDown
      OptionsImage.Layout = blGlyphTop
      TabOrder = 10
    end
    object btnReset: TcxButton
      Left = 10
      Top = 547
      Width = 75
      Height = 25
      Caption = 'R&eset'
      DropDownMenu = bpmReset
      Kind = cxbkOfficeDropDown
      OptionsImage.Layout = blGlyphTop
      TabOrder = 13
    end
    object btnOK: TcxButton
      Left = 573
      Top = 547
      Width = 75
      Height = 25
      Caption = '&OK'
      Enabled = False
      ModalResult = 1
      TabOrder = 14
    end
    object btnCancel: TcxButton
      Left = 654
      Top = 547
      Width = 75
      Height = 25
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 15
    end
    object tlCommands: TcxTreeList
      Left = 10
      Top = 53
      Width = 326
      Height = 476
      Bands = <
        item
        end>
      DragMode = dmAutomatic
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.IncSearchItem = tlCommandsMainColumn
      OptionsData.Editing = False
      OptionsData.Deleting = False
      OptionsSelection.HideFocusRect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.Headers = False
      OptionsView.ShowRoot = False
      OptionsView.TreeLineStyle = tllsNone
      TabOrder = 1
      OnBeginDragNode = tlCommandsBeginDragNode
      OnClick = acUpdateActionsStateExecute
      OnCollapsing = tlCommandsCollapsing
      OnCustomDrawDataCell = tlCommandsCustomDrawDataCell
      OnDeletion = tlCommandsDeletion
      OnDragOver = tlCommandsDragOver
      OnEndDrag = tlCommandsEndDrag
      OnFocusedNodeChanged = tlCommandsFocusedNodeChanged
      object tlCommandsMainColumn: TcxTreeListColumn
        DataBinding.ValueType = 'String'
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object tlRibbon: TcxTreeList
      Left = 373
      Top = 99
      Width = 325
      Height = 399
      Bands = <
        item
        end>
      DragMode = dmAutomatic
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.DragFocusing = True
      OptionsData.Editing = False
      OptionsData.Deleting = False
      OptionsSelection.HideFocusRect = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.CheckGroups = True
      OptionsView.Headers = False
      OptionsView.ShowRoot = False
      OptionsView.TreeLineStyle = tllsNone
      PopupMenu = bpmRibbon
      Styles.OnGetContentStyle = tlRibbonStylesGetContentStyle
      TabOrder = 7
      OnBeginDragNode = tlCommandsBeginDragNode
      OnClick = acUpdateActionsStateExecute
      OnCollapsing = tlCommandsCollapsing
      OnCustomDrawDataCell = tlCommandsCustomDrawDataCell
      OnDeletion = tlCommandsDeletion
      OnDragOver = tlRibbonDragOver
      OnEndDrag = tlCommandsEndDrag
      OnFocusedNodeChanged = tlCommandsFocusedNodeChanged
      OnMoveTo = tlRibbonMoveTo
      OnNodeCheckChanged = tlRibbonNodeCheckChanged
      object tlRibbonMainColumn: TcxTreeListColumn
        DataBinding.ValueType = 'String'
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
    end
    object chbShowQATBelowRibbon: TcxCheckBox
      Left = 373
      Top = 76
      AutoSize = False
      Caption = 'Show Quick Access Toolbar below the Ribbon'
      Properties.OnChange = chbShowQATBelowRibbonPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Height = 17
      Width = 325
    end
    object lbRibbonQAT: TcxLabel
      Left = 373
      Top = 53
      AutoSize = False
      Caption = 'Customize &Quick Access Toolbar:'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Transparent = True
      Height = 17
      Width = 325
      AnchorY = 62
    end
    object cbRibbon: TcxComboBox
      Left = 373
      Top = 28
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbRibbonPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Width = 325
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object lciCommandsSource: TdxLayoutItem
      Parent = lcgCommands
      AlignVert = avTop
      CaptionOptions.Text = 'C&hoose commands from:'
      CaptionOptions.Layout = clTop
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = cbCommands
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 326
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciMoveUp: TdxLayoutItem
      Parent = lcgReordering
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnMoveUp
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciMoveDown: TdxLayoutItem
      Parent = lcgReordering
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnMoveDown
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciRemove: TdxLayoutItem
      Parent = lcgActions
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnRemove
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciAdd: TdxLayoutItem
      Parent = lcgActions
      CaptionOptions.Text = 'cxButton4'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnAdd
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 25
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciNewElement: TdxLayoutItem
      Parent = lcgRibbonActions
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton5'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnNewElement
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRename: TdxLayoutItem
      Parent = lcgRibbonActions
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton6'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnRename
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciImportExport: TdxLayoutItem
      Parent = lcgRibbonActions
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton7'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Visible = False
      Control = btnImportExport
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciReset: TdxLayoutItem
      Parent = lcgControlling
      AlignHorz = ahLeft
      CaptionOptions.Text = 'cxButton8'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnReset
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciOK: TdxLayoutItem
      Parent = lcgControlling
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton9'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 1
    end
    object lciCancel: TdxLayoutItem
      Parent = lcgControlling
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton10'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcgCommands: TdxLayoutGroup
      Parent = lcgEditing
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lcgActions: TdxLayoutGroup
      Parent = lcgEditing
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcgControlling: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object lcgEditing: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcgRibbon: TdxLayoutGroup
      Parent = lcgEditing
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 2
    end
    object lcgRibbonActions: TdxLayoutGroup
      Parent = lcgRibbon
      AlignHorz = ahClient
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lciSeparator: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Separator'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 1
    end
    object lcgReordering: TdxLayoutGroup
      Parent = lcgEditing
      AlignHorz = ahRight
      AlignVert = avCenter
      CaptionOptions.Text = 'New Group'
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 3
    end
    object lciCommands: TdxLayoutItem
      Parent = lcgCommands
      AlignVert = avClient
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = tlCommands
      ControlOptions.OriginalHeight = 474
      ControlOptions.OriginalWidth = 326
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciRibbon: TdxLayoutItem
      Parent = lcgRibbon
      AlignVert = avClient
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Control = tlRibbon
      ControlOptions.OriginalHeight = 397
      ControlOptions.OriginalWidth = 325
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lciShowQATBelowRibbon: TdxLayoutItem
      Parent = lcgRibbon
      CaptionOptions.Text = 'lciShowQATBelowRibbon'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = chbShowQATBelowRibbon
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 325
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciRibbonQAT: TdxLayoutItem
      Parent = lcgRibbon
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbRibbonQAT
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 325
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lciRibbonSource: TdxLayoutItem
      Parent = lcgRibbon
      AlignVert = avTop
      CaptionOptions.Text = 'Customize the Ri&bbon:'
      CaptionOptions.Layout = clTop
      LayoutLookAndFeel = dxLayoutCxLookAndFeel
      Visible = False
      Control = cbRibbon
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 325
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object alActions: TActionList
    Left = 208
    object acAddNewContext: TAction
      Caption = 'Add New &Context'
      OnExecute = acAddNewContextExecute
    end
    object acAddNewTab: TAction
      Caption = 'Add New &Tab'
      OnExecute = acAddNewTabExecute
    end
    object acAddNewGroup: TAction
      Caption = 'Add New &Group'
      OnExecute = acAddNewGroupExecute
    end
    object acAdd: TAction
      Caption = '&Add'
      OnExecute = acAddExecute
    end
    object acRemove: TAction
      Caption = '&Remove'
      ShortCut = 46
      OnExecute = acRemoveExecute
    end
    object acRename: TAction
      Caption = 'Rena&me...'
      ShortCut = 113
      OnExecute = acRenameExecute
    end
    object acShowTab: TAction
      AutoCheck = True
      Caption = '&Show Tab'
      OnExecute = acShowTabExecute
    end
    object acResetAllCustomizations: TAction
      Caption = 'Reset a&ll customizations'
      OnExecute = acResetAllCustomizationsExecute
    end
    object acResetSelectedTab: TAction
      Caption = 'Reset Ta&b'
      OnExecute = acResetSelectedTabExecute
    end
    object acMoveUp: TAction
      Caption = 'Move &Up'
      OnExecute = acMoveUpExecute
    end
    object acMoveDown: TAction
      Caption = 'Move &Down'
      OnExecute = acMoveDownExecute
    end
    object acUpdateActionsState: TAction
      Caption = 'UpdateActionsState'
      OnExecute = acUpdateActionsStateExecute
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 240
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object bmMain: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    PopupMenuLinks = <>
    Style = bmsUseLookAndFeel
    UseSystemFont = True
    Left = 272
    PixelsPerInch = 96
    object bbtnNewContext: TdxBarButton
      Action = acAddNewContext
      Category = 0
    end
    object bbtnNewTab: TdxBarButton
      Action = acAddNewTab
      Category = 0
    end
    object bbtnNewGroup: TdxBarButton
      Action = acAddNewGroup
      Category = 0
    end
    object bsUpperSeparator: TdxBarSeparator
      Caption = 'Upper separator'
      Category = 0
      Hint = 'Upper separator'
      Visible = ivAlways
      ShowCaption = False
    end
    object bbtnRemove: TdxBarButton
      Action = acRemove
      Category = 0
    end
    object bbtnRename: TdxBarButton
      Action = acRename
      Category = 0
    end
    object bbtnResetTab: TdxBarButton
      Action = acResetSelectedTab
      Category = 0
    end
    object bbtnShowTab: TdxBarButton
      Action = acShowTab
      Category = 0
      ButtonStyle = bsChecked
    end
    object bsLowerSeparator: TdxBarSeparator
      Caption = 'Lower separator'
      Category = 0
      Hint = 'Lower separator'
      Visible = ivAlways
      ShowCaption = False
    end
    object bbtnMoveUp: TdxBarButton
      Action = acMoveUp
      Category = 0
    end
    object bbtnMoveDown: TdxBarButton
      Action = acMoveDown
      Category = 0
    end
    object bbtnResetOnlySelectedTab: TdxBarButton
      Action = acResetSelectedTab
      Caption = 'Reset only &selected tab'
      Category = 0
    end
    object bbtnResetAllCustomizations: TdxBarButton
      Action = acResetAllCustomizations
      Category = 0
    end
    object bbtnImportCustomizationFile: TdxBarButton
      Caption = 'Import customization file'
      Category = 0
      Hint = 'Import customization file'
      Visible = ivAlways
    end
    object bbtnExportAllCustomizations: TdxBarButton
      Caption = 'Export all customizations'
      Category = 0
      Hint = 'Export all customizations'
      Visible = ivAlways
    end
  end
  object bpmRibbon: TdxBarPopupMenu
    BarManager = bmMain
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbtnNewTab'
      end
      item
        Visible = True
        ItemName = 'bbtnNewGroup'
      end
      item
        Visible = True
        ItemName = 'bsUpperSeparator'
      end
      item
        Visible = True
        ItemName = 'bbtnRename'
      end
      item
        Visible = True
        ItemName = 'bbtnShowTab'
      end
      item
        Visible = True
        ItemName = 'bbtnResetTab'
      end
      item
        Visible = True
        ItemName = 'bbtnRemove'
      end
      item
        Visible = True
        ItemName = 'bsLowerSeparator'
      end
      item
        Visible = True
        ItemName = 'bbtnMoveUp'
      end
      item
        Visible = True
        ItemName = 'bbtnMoveDown'
      end>
    UseOwnFont = False
    OnPopup = acUpdateActionsStateExecute
    Left = 384
    Top = 112
    PixelsPerInch = 96
  end
  object bpmReset: TdxBarPopupMenu
    BarManager = bmMain
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbtnResetOnlySelectedTab'
      end
      item
        Visible = True
        ItemName = 'bbtnResetAllCustomizations'
      end>
    UseOwnFont = False
    OnPopup = acUpdateActionsStateExecute
    Left = 84
    Top = 547
    PixelsPerInch = 96
  end
  object bpmNewElement: TdxBarPopupMenu
    BarManager = bmMain
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbtnNewTab'
      end
      item
        Visible = True
        ItemName = 'bbtnNewGroup'
      end>
    UseOwnFont = False
    OnPopup = acUpdateActionsStateExecute
    Left = 420
    Top = 528
    PixelsPerInch = 96
  end
  object bpmImportExport: TdxBarPopupMenu
    BarManager = bmMain
    ItemLinks = <
      item
        Visible = True
        ItemName = 'bbtnImportCustomizationFile'
      end
      item
        Visible = True
        ItemName = 'bbtnExportAllCustomizations'
      end>
    UseOwnFont = False
    OnPopup = acUpdateActionsStateExecute
    Left = 670
    Top = 528
    PixelsPerInch = 96
  end
  object cxStyleRepository: TcxStyleRepository
    Left = 304
    PixelsPerInch = 96
    object stNodeDisabled: TcxStyle
    end
  end
end

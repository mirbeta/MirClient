object frmSpreadSheetConditionalFormattingRulesManagerDialog: TfrmSpreadSheetConditionalFormattingRulesManagerDialog
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Conditional Formatting Rules Manager'
  ClientHeight = 382
  ClientWidth = 684
  Color = clBtnFace
  Constraints.MinHeight = 420
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 684
    Height = 382
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object btnCancel: TcxButton
      Left = 498
      Top = 347
      Width = 85
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 8
    end
    object btnOk: TcxButton
      Left = 407
      Top = 347
      Width = 85
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 7
    end
    object btnApply: TcxButton
      Left = 589
      Top = 347
      Width = 85
      Height = 25
      Cancel = True
      Caption = '&Apply'
      TabOrder = 9
      OnClick = btnApplyClick
    end
    object cbDisplayMode: TcxComboBox
      Left = 82
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Selected Area'
        'Sheet')
      Properties.OnChange = cbDisplayModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Text = 'Selected Area'
      Width = 143
    end
    object btnEdit: TcxButton
      Left = 108
      Top = 51
      Width = 92
      Height = 25
      Action = acEdit
      Cancel = True
      OptionsImage.Images = ilImages
      TabOrder = 2
    end
    object btnRemove: TcxButton
      Left = 206
      Top = 51
      Width = 92
      Height = 25
      Action = acDelete
      Cancel = True
      OptionsImage.Images = ilImages
      TabOrder = 3
    end
    object btnCreate: TcxButton
      Left = 10
      Top = 51
      Width = 92
      Height = 25
      Action = acCreate
      Cancel = True
      OptionsImage.Images = ilImages
      TabOrder = 1
    end
    object tlRules: TcxTreeList
      Left = 10
      Top = 82
      Width = 664
      Height = 259
      Bands = <
        item
        end>
      DefaultRowHeight = 30
      DragMode = dmAutomatic
      Navigator.Buttons.CustomButtons = <>
      OptionsBehavior.ConfirmDelete = False
      OptionsBehavior.Sorting = False
      OptionsCustomizing.BandCustomizing = False
      OptionsCustomizing.BandHorzSizing = False
      OptionsCustomizing.BandMoving = False
      OptionsCustomizing.BandVertSizing = False
      OptionsCustomizing.ColumnCustomizing = False
      OptionsCustomizing.ColumnMoving = False
      OptionsCustomizing.ColumnVertSizing = False
      OptionsCustomizing.NestedBands = False
      OptionsCustomizing.StackedColumns = False
      OptionsView.CellTextMaxLineCount = 1
      OptionsView.ColumnAutoWidth = True
      OptionsView.DropNodeIndicator = True
      OptionsView.TreeLineStyle = tllsNone
      TabOrder = 6
      OnContextPopup = tlRulesContextPopup
      OnCustomDrawDataCell = tlRulesCustomDrawDataCell
      OnDblClick = tlRulesDblClick
      OnDragOver = tlRulesDragOver
      OnEditing = tlRulesEditing
      OnKeyDown = tlRulesKeyDown
      OnMoveTo = tlRulesMoveTo
      object tlcRuleName: TcxTreeListColumn
        Caption.Text = 'Name'
        DataBinding.ValueType = 'String'
        Width = 321
        Position.ColIndex = 0
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlcRuleFormat: TcxTreeListColumn
        Caption.Text = 'Format'
        DataBinding.ValueType = 'String'
        Width = 132
        Position.ColIndex = 1
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlcRuleArea: TcxTreeListColumn
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.OnEditValueChanged = tlcRuleAreaPropertiesEditValueChanged
        Caption.Text = 'Area'
        DataBinding.ValueType = 'String'
        Width = 130
        Position.ColIndex = 2
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
      end
      object tlcRuleStopIfTrue: TcxTreeListColumn
        Caption.Text = 'Stop If True'
        DataBinding.ValueType = 'String'
        Width = 123
        Position.ColIndex = 3
        Position.RowIndex = 0
        Position.BandIndex = 0
        Summary.FooterSummaryItems = <>
        Summary.GroupFooterSummaryItems = <>
        OnGetEditProperties = tlcRuleStopIfTrueGetEditProperties
      end
    end
    object btnMoveUp: TcxButton
      Left = 608
      Top = 51
      Width = 30
      Height = 25
      Action = acMoveUp
      Cancel = True
      OptionsImage.Images = ilImages
      TabOrder = 4
    end
    object btnMoveDown: TcxButton
      Left = 644
      Top = 51
      Width = 30
      Height = 25
      Action = acMoveDown
      Cancel = True
      OptionsImage.Images = ilImages
      TabOrder = 5
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lcMainItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnApply
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lciDisplayMode: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Display Mode:'
      Control = cbDisplayMode
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 143
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnEdit
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcMainGroup1
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnRemove
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 2
      AutoCreated = True
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Control = btnCreate
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainSpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 1
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignVert = avClient
      Control = tlRules
      ControlOptions.OriginalHeight = 298
      ControlOptions.OriginalWidth = 708
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnMoveUp
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 30
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem8: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnMoveDown
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 30
      ControlOptions.ShowBorder = False
      Index = 4
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 368
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object ilImages: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 400
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000030000
          000B000000110000001200000012000000120000001200000012000000120000
          00120000001200000012000000110000000B00000003000000000000000A714F
          44C29E6D5DFF9E6C5DFF9D6C5CFF9D6B5BFF9C6B5BFF9C6B5BFF9C6A5AFF9B69
          59FF9B695AFF9A6959FF9B6959FF6F4A3FC30000000B000000000000000EA171
          62FFF9F2EFFFF7EEE9FFF7EEEAFFC29F93FFF7EEE9FFF7EEE9FFF7EEE9FFC29F
          93FFF7EDE9FFF7EDE8FFF7EEE9FF9C6B5BFF0000000F000000000000000EA475
          67FFF9F2F0FFF7EEEBFFF7EEEAFFC3A095FFF7EEEAFFF8EEEAFFF8EEEAFFC2A0
          94FFF7EEE9FFF7EEE9FFF9F2EFFF9F6E5FFF0000000F000000000000000DAD72
          4AFFD2A881FFC48F62FFC48F62FFC48F62FFC38F62FFC38E61FFC38E60FFC38D
          61FFC3A196FFC3A095FFD1B5ACFFA17263FF0000000E000000000000000CB277
          4EFFEDD8B6FFE7CCA0FFE8CCA1FFC69265FFE7CCA1FFE7CCA0FFE7CCA0FFC490
          64FFF9F3F0FFF9F3F0FFFBF6F4FFBC988DFF0000000D000000000000000AB57D
          53FFEED9B6FFE7CDA1FFE8CCA1FFC79569FFE7CDA1FFE8CCA2FFE7CCA1FFC693
          66FFFBF8F6FF89ADD7FFFDF9F8FF7590B8FF0000000C0000000000000009B983
          59FFD8B08CFFCB996EFFCA9A6DFFCA996DFFC9986CFFC9976CFFD7B190FFE4CB
          B5FFE3D3CDFF1664B9FF4D88C9FF1462B7FF0000000F0000000000000008BD88
          5FFFEEDAB8FFE9CEA3FFE8CDA3FFCC9D70FFE8CEA3FFE8CDA3FFEEDABAFF7D98
          B8FF1C6ABEFF80D2F8FF4EAEE9FF5CC0F8FF1967BCFF0C335C7E00000007C08E
          64FFEFDAB9FFEEDAB8FFEEDAB8FFDAB591FFEEDAB8FFEFDAB8FFF2E3CAFFECDA
          C7FF5B96D3FF6DBDEDFF6ECBF8FF58B1EAFF185290C00000000000000004916D
          4DC1C29267FFC29167FFC29066FFC18F65FFC08E64FFBF8C63FFCFA989FF7A94
          B5FF2879CAFFABE4FCFF89C9EFFFA7E2FBFF2576C7FF133B6481000000010000
          0003000000050000000500000005000000050000000500000005000000050000
          0006000000082C7FCEFF215F9AC12B7ECDFF0000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000001842687E000000001841687E0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
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
          2771000000070000000000000000000000000000000000000000000000000000
          000000000000000000030104071E13458AE678C6E5FFA4DBF1FF093D96FF062A
          69EA0000001300000002000000000000000000000000000000000000000B0000
          0011000000120000001300000017030E1D4D225DA5F66DB5DBFF207EC6FF1248
          98FF47160DA40100001100000002000000000000000000000000714F44C29E6D
          5DFF9E6C5DFF9D6C5CFFA6786AFFC6ADA4FF9C97A2FF3E67A4FF1855A6FFF2E9
          E2FFBA8169FF4E1A0EAE00000110000000020000000000000000A17162FFF9F2
          EFFFF7EEE9FFF7EEEAFFC29F93FFF8F0EBFFF7F1EEFFEFEAE9FF9792A1FFC3B5
          B5FFF8EEE5FF705F8CFF010132AC0000010F0000000200000000A47567FFF9F2
          F0FFF7EEEBFFF7EEEAFFC3A095FFF7EEEAFFF8EEEAFFF8F1EEFFD8C8C2FF9E9A
          A8FF8A8CC3FFA2A8F1FF4146AFFF020138B00000010E00000002AD724AFFD2A8
          81FFC48F62FFC48F62FFC48F62FFC38F62FFC38E61FFC38E60FFD5B092FFD9C9
          C4FF7383B7FF5867CDFFA8B0F2FF464CB4FF03023DA90000010CB2774EFFEDD8
          B6FFE7CCA0FFE8CCA1FFC69265FFE7CCA1FFE7CCA0FFE7CCA0FFC49064FFF9F3
          F0FFF4F0EEFF7E8AC2FF656FDAFFADB6F2FF474DB3FF01013D9EB57D53FFEED9
          B6FFE7CDA1FFE8CCA1FFC79569FFE7CDA1FFE8CCA2FFE7CCA1FFC69366FFF7F0
          ECFFF9F3F1FFF7F3F2FF817EC1FF6976DCFFAAB3F0FF1C1B9FECB98359FFD8B0
          8CFFCB996EFFCA9A6DFFCA996DFFC9986CFFC9976CFFC9976BFFC9976BFFC7A6
          9BFFC6A69AFFE0CDC7FFD2BCB4FF353B789A5760C5EE0C0B3755BD885FFFEEDA
          B8FFE9CEA3FFE8CDA3FFCC9D70FFE8CEA3FFE8CDA3FFE8CDA3FFCB9B6FFFF7F0
          EEFFF8F0EDFFFAF4F2FFC5A69CFF0000000B0000000300000002C08E64FFEFDA
          B9FFEEDAB8FFEEDAB8FFDAB591FFEEDAB8FFEFDAB8FFEEDAB8FFD9B48EFFFAF4
          F2FFFAF5F2FFFAF5F2FFB48A7CFF000000080000000000000000916D4DC1C292
          67FFC29167FFC29066FFC18F65FFC08E64FFBF8C63FFBF8C62FFBE8B61FFBA91
          83FFB89083FFB88F82FF876A5FC1000000050000000000000000000000030000
          0005000000050000000500000005000000050000000500000005000000060000
          0006000000060000000500000004000000010000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000001000000050000000A0000
          00070000000200000002000000070000000A0000000600000001000000000000
          000000000000000000000000000000000000000000050C07315E221491EA0201
          27870000000C0000000D020128901D0F8DE809052C5900000005000000000000
          000000000000000000000000000000000000000000092B1D9EF2606FEBFF252E
          A3FF02012A9002012C98212BA1FF5767E8FF231696F000000009000000000000
          000000000000000000000000000000000000000000061C155C936165D2FF697D
          F5FF2A34ABFF2933ABFF647BF3FF595CCFFF1610548F000000060000000A0000
          000F000000100000001000000010000000100000001100000019201B63A2575F
          D6FF526AF3FF5167F4FF5459D5FF1B165C970000000B00000001856156C1B886
          75FFB88575FFB78574FFB58374FFB58373FFC6A194FFD1BAB2FF6A5E9CFF404A
          C7FF788FF8FF778CF7FF3A46C5FF0D0A46910000000900000001C09181FFFFFF
          FFFFFBF5F2FFFAF4F1FFCFAEA2FFFAF4F0FFF8F5F3FF8582C2FF4753CCFF839A
          FBFF636BCDFF6269CCFF7E95FAFF424DC7FF0E0D498600000005C29485FFFFFF
          FFFFF8F2EDFFF8F1ECFFC7A295FFF8F0EBFFF5F1EEFF5B5BC9FFBCC9FAFF8A90
          E2FF9694D1FF918FD0FF878CE0FFB8C4F9FF4441B1EC00000006BB845AFFBB84
          59FFBA8359FFB98258FFBA8258FFB98157FFD7BBA5FFAA99B1FF6363CCFF9487
          B7FFD5BAA5FFD5B9A5FF8F82B5FF4E4DB9ED18183B5500000003BD875DFFEDD6
          B2FFE8CA9DFFE7C99DFFCA9A71FFE7C79AFFEBD0AAFFEFDDC6FFDEC5AFFFEEDD
          C5FFEAD1ADFFEAD1AFFFD6B7A1FF000000120000000300000001C08B60FFEED8
          B6FFEED8B6FFEED8B6FFCB9C73FFEDD8B5FFEDD7B5FFEDD7B5FFCA9B72FFEDD7
          B5FFEDD7B5FFEDD7B5FFBA8359FF0000000C0000000000000000D0A378FFD0A3
          77FFCFA276FFCFA175FFCEA074FFCD9F73FFCD9E72FFCC9D71FFCB9C70FFCA9A
          6FFFC9996DFFC8976CFFC7966AFF0000000B0000000000000000C8A093FFFFFF
          FFFFFCF8F7FFFCF8F5FFCDAB9EFFFBF7F5FFFBF7F4FFFBF7F3FFCDAA9EFFFBF6
          F2FFFAF5F2FFFCF8F7FFC69C8EFF0000000B0000000000000000C8A194FFFFFF
          FFFFFFFFFFFFFFFFFFFFD9BEB3FFFFFFFFFFFFFFFFFFFFFFFFFFD9BDB3FFFFFF
          FFFFFFFFFFFFFFFFFFFFC69E90FF0000000900000000000000009E8177C2D6AF
          A1FFD5AEA1FFD5AEA0FFD5AEA0FFD4ADA0FFD4AD9FFFD4AD9FFFD3AC9FFFD3AC
          9EFFD3AB9EFFD2AB9DFF9B7F74C3000000050000000000000000000000040000
          0006000000070000000700000007000000070000000700000007000000070000
          0008000000080000000700000005000000010000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000020000000A0000000F00000010000000100000
          000F0000000A0000000200000000000000000000000000000000000000000000
          00000000000000000000000000097E5237C1A8663FFFA7633DFFA6623BFFA560
          3AFF754429C10000000900000000000000000000000000000000000000000000
          000000000000000000000000000CB57852FFE6C69AFFE2BE8EFFE2BE8EFFE2BE
          8DFFA6613CFF0000000D00000000000000000000000000000000000000000000
          000000000000000000000000000CB67B55FFE6C79DFFDAAD73FFD9AD73FFE3C0
          8FFFA8653EFF0000000D00000000000000000000000000000000000000000000
          0002000000080000000B00000013B87F59FFE7C79DFFDBAF77FFDBAE77FFE4C1
          91FFA96842FF000000150000000C0000000A0000000300000000000000000000
          0007B77D56FFB67B54FFB47953FFB37751FFE5C495FFDCB279FFDCB17AFFE4C2
          95FFAB6B46FFAA6A44FFAA6842FFA86640FF0000000800000000000000000000
          00057E573DB5E7D3C4FFEACEA8FFE6C69AFFE5C598FFDDB37DFFDDB37DFFE5C4
          97FFE5C597FFE7C79CFFDDC5B7FF673923B60000000600000000000000000000
          0002140E0A24B78869F2F2E5D2FFE6C799FFE7C997FFEACF9FFFEACF9EFFE7C9
          97FFE3C18FFFEFDFCBFF9D6549F3100906260000000200000000000000000000
          0000000000034E372871D7B7A0FFF5E9CDFFEEDAABFFEDD7A8FFEDD7A8FFEED7
          A8FFF4E5C4FFC7A08CFF41261872000000040000000000000000000000000000
          0000000000010000000590684BC7EEE0D3FFF3E5BFFFF1DFB2FFF1DFB2FFF2E2
          B7FFE8D8CBFF7B4C30C700000006000000010000000000000000000000000000
          000000000000000000011D15102DC4997AFAFBF4E4FFF5E8C0FFF4E6BBFFF9F2
          E0FFB07D5FFA19100A2F00000001000000000000000000000000000000000000
          00000000000000000000000000025F453383DFC5B1FFFBF2D7FFFAF2D4FFD3B6
          A2FF533523830000000300000000000000000000000000000000000000000000
          000000000000000000000000000005030309A37C5CD8F4EBDFFFF1E6DAFF9362
          45D80403020A0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000012E231A3FCFAB8EFDC09679FD291C
          1340000000010000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000174584298674731950000
          0001000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000001000000010000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000003000000090000000A0000
          0003000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000010000000D552C19A64E2916A40000
          000D000000010000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000623130B54A77053FD9E6443FD2011
          0A54000000060000000000000000000000000000000000000000000000000000
          000000000000000000000000000204020117814A2EDEDFBF9BFFD4AC7EFF7942
          26DF030201180000000200000000000000000000000000000000000000000000
          00000000000000000001000000094D2D1C90C69D7FFFDFB987FFDFB785FFBA87
          5FFF472617910000000A00000001000000000000000000000000000000000000
          00000000000000000004180F0A3AA87252FBE5C89FFFDCB27AFFDCB17BFFE0BC
          8EFF9C6140FB160C073B00000004000000000000000000000000000000000000
          0000000000010000000C7E4E32CCDDBFA0FFDFB783FFDDB37DFFDDB37DFFDFB6
          82FFD2AB80FF714027CD0000000D000000020000000000000000000000000000
          000000000006452C1E78C69D7FFFE4C394FFDEB680FFDFB680FFDEB67FFFDEB5
          7FFFE3BF8FFFB8855FFF3E241679000000070000000000000000000000000000
          0002120C0828AA7758F3E9CEAAFFE4C290FFE5C794FFE8CA98FFE8CA99FFE6C5
          93FFE3C18DFFE2C193FF996141F31009062A0000000300000000000000000000
          0006795339B6E0C7ADFFF9F1E0FFFAF2E2FFFAF2E2FFEBD3A3FFECD2A3FFF1DD
          B7FFF0DDB7FFF9F1E0FFDBBEA6FF6B4028B80000000700000000000000000000
          0006C1906AFFC18E68FFC08C66FFBE8A64FFFBF5E5FFEFDAACFFEFD9ACFFF3E2
          BEFFB9805AFFB77E58FFB77C55FFB57A54FF0000000800000000000000000000
          000200000006000000070000000DC08D68FFFBF7E9FFF2E1B4FFF2E1B4FFF5E9
          C5FFB27753FF0000001000000009000000070000000200000000000000000000
          0000000000000000000000000007C3926CFFFCF8EBFFF5E8BCFFF5E7BCFFF8EE
          CCFFB47C56FF0000000800000000000000000000000000000000000000000000
          0000000000000000000000000005C59570FFFDFAEDFFFDFAEDFFFDFAEDFFFDFA
          EDFFB67F5AFF0000000600000000000000000000000000000000000000000000
          000000000000000000000000000393755CBACDA37EFFCBA17DFFCAA07BFFC99D
          79FF8C6A50BB0000000400000000000000000000000000000000000000000000
          0000000000000000000000000001000000020000000400000004000000040000
          0004000000030000000100000000000000000000000000000000}
      end>
  end
  object alActions: TActionList
    Images = ilImages
    Left = 432
    object acMoveUp: TAction
      ImageIndex = 3
      ShortCut = 16422
      OnExecute = acMoveUpExecute
      OnUpdate = acMoveUpUpdate
    end
    object acMoveDown: TAction
      Tag = 1
      ImageIndex = 4
      ShortCut = 16424
      OnExecute = acMoveDownExecute
      OnUpdate = acMoveDownUpdate
    end
    object acDelete: TAction
      Caption = 'acDelete'
      ImageIndex = 2
      OnExecute = acDeleteExecute
      OnUpdate = acDeleteUpdate
    end
    object acEdit: TAction
      Caption = 'acEdit'
      ImageIndex = 1
      OnExecute = acEditExecute
      OnUpdate = acEditUpdate
    end
    object acCreate: TAction
      Caption = 'acCreate'
      ImageIndex = 0
      OnExecute = acCreateExecute
    end
    object acClone: TAction
      Caption = 'Clone'
      OnExecute = acCloneExecute
      OnUpdate = acCloneUpdate
    end
  end
  object cxEditRepository: TcxEditRepository
    Left = 336
    PixelsPerInch = 96
    object cxEditRepositoryCheckBoxItem: TcxEditRepositoryCheckBoxItem
      Properties.NullStyle = nssInactive
      Properties.ValueChecked = '1'
      Properties.ValueGrayed = '-1'
      Properties.ValueUnchecked = '0'
      Properties.OnEditValueChanged = tlcRuleStopIfTruePropertiesEditValueChanged
    end
  end
  object pmRules: TPopupMenu
    Left = 640
    Top = 112
    object miEdit: TMenuItem
      Action = acEdit
      Default = True
    end
    object miClone: TMenuItem
      Action = acClone
    end
    object miLine2: TMenuItem
      Caption = '-'
    end
    object miDelete: TMenuItem
      Action = acDelete
    end
  end
end

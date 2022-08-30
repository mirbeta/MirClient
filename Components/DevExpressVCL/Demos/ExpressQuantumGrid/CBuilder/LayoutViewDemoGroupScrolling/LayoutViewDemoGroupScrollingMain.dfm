inherited frmMain: TfrmMain
  Left = 300
  Top = 120
  Caption = 'ExpressQuantumGrid LayoutView Group Scrolling Demo'
  ClientHeight = 606
  ClientWidth = 822
  Constraints.MinHeight = 660
  Constraints.MinWidth = 700
  Position = poScreenCenter
  WindowState = wsMaximized
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 822
    Caption = 
      'This demo shows how to limit the card/record size in Layout View' +
      's and enable content scrolling using scrollbars.'
  end
  object Grid: TcxGrid [1]
    Left = 0
    Top = 16
    Width = 822
    Height = 571
    Align = alClient
    TabOrder = 0
    LevelTabs.Slants.Positions = []
    object LayoutView: TcxGridDBLayoutView
      Navigator.Buttons.CustomButtons = <>
      FilterBox.Visible = fvNever
      DataController.DataSource = dmGridCars.dsModels
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      Images = Images
      OptionsCustomize.RecordExpanding = True
      OptionsView.MinValueWidth = 40
      OptionsView.ShowItemFilterButtons = sfbAlways
      OptionsView.ShowOnlyEntireRecords = False
      OptionsView.ViewMode = lvvmSingleRow
      Styles.Content = stValues
      Styles.Item = stItems
      Styles.RecordCaption = stRecordCaption
      Styles.Selection = stRecordSelected
      object LayoutViewRecId: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'RecId'
        Visible = False
        LayoutItem = LayoutViewLayoutItem1
      end
      object LayoutViewID: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'ID'
        Visible = False
        LayoutItem = LayoutViewLayoutItem2
      end
      object LayoutViewTrademark: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'TrademarkID'
        RepositoryItem = dmGridCars.EditRepositoryTrademarkLookup
        LayoutItem = LayoutViewLayoutItem3
      end
      object LayoutViewModel: TcxGridDBLayoutViewItem
        Caption = 'Model'
        DataBinding.FieldName = 'Name'
        LayoutItem = LayoutViewLayoutItem4
      end
      object LayoutViewHP: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Horsepower'
        LayoutItem = LayoutViewLayoutItem5
      end
      object LayoutViewCyl: TcxGridDBLayoutViewItem
        Caption = 'Cylinder'
        DataBinding.FieldName = 'Cilinders'
        LayoutItem = LayoutViewLayoutItem7
      end
      object LayoutViewTransmissSpeedCount: TcxGridDBLayoutViewItem
        Caption = 'Speed Count'
        DataBinding.FieldName = 'Transmission Speeds'
        LayoutItem = LayoutViewLayoutItem8
      end
      object LayoutViewTransmissAutomatic: TcxGridDBLayoutViewItem
        Caption = 'Automatic'
        DataBinding.FieldName = 'Transmission Type'
        RepositoryItem = dmGridCars.EditRepositoryTransmissionTypeCheckBox
        LayoutItem = LayoutViewLayoutItem9
      end
      object LayoutViewMPG_City: TcxGridDBLayoutViewItem
        Caption = 'City (mpg)'
        DataBinding.FieldName = 'MPG City'
        LayoutItem = LayoutViewLayoutItem10
      end
      object LayoutViewMPG_Highway: TcxGridDBLayoutViewItem
        Caption = 'Highway (mpg)'
        DataBinding.FieldName = 'MPG Highway'
        LayoutItem = LayoutViewLayoutItem11
      end
      object LayoutViewCategory: TcxGridDBLayoutViewItem
        Caption = 'Category'
        DataBinding.FieldName = 'CategoryID'
        RepositoryItem = dmGridCars.EditRepositoryCategoryLookup
        LayoutItem = LayoutViewLayoutItem12
      end
      object LayoutViewDescription: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Description'
        RepositoryItem = dmGridCars.EditRepositoryMemo
        LayoutItem = LayoutViewLayoutItem13
      end
      object LayoutViewHyperlink: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Hyperlink'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        LayoutItem = LayoutViewLayoutItem14
        Options.Filtering = False
      end
      object LayoutViewPicture: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Image'
        RepositoryItem = dmGridCars.EditRepositoryImage
        LayoutItem = LayoutViewLayoutItem15
      end
      object LayoutViewPrice: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Price'
        RepositoryItem = dmGridCars.EditRepositoryPrice
        LayoutItem = LayoutViewLayoutItem16
        Options.Filtering = False
      end
      object LayoutViewGroup_Root: TdxLayoutGroup
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'Template Card'
        ButtonOptions.Buttons = <>
        Hidden = True
        ItemIndex = 2
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = -1
      end
      object LayoutViewGroup1: TdxLayoutGroup
        Parent = LayoutViewGroup3
        CaptionOptions.Text = 'Engine'
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        Index = 1
      end
      object LayoutViewGroup2: TdxLayoutGroup
        Parent = LayoutViewGroup3
        CaptionOptions.Text = 'Transmission'
        ButtonOptions.Buttons = <>
        Index = 0
      end
      object LayoutViewGroup3: TdxLayoutGroup
        CaptionOptions.Text = 'New Group'
        ButtonOptions.Buttons = <>
        LayoutDirection = ldTabbed
        ShowBorder = False
        Index = -1
      end
      object LayoutViewLayoutItem1: TcxGridLayoutItem
        Index = -1
      end
      object LayoutViewLayoutItem2: TcxGridLayoutItem
        Index = -1
      end
      object LayoutViewLayoutItem3: TcxGridLayoutItem
        Parent = LayoutViewGroup11
        AlignHorz = ahClient
        AlignVert = avTop
        CaptionOptions.Visible = False
        SizeOptions.Width = 151
        Index = 0
      end
      object LayoutViewLayoutItem4: TcxGridLayoutItem
        Parent = LayoutViewGroup11
        AlignHorz = ahClient
        AlignVert = avTop
        CaptionOptions.AlignVert = tavBottom
        CaptionOptions.Visible = False
        SizeOptions.Width = 151
        Index = 1
      end
      object LayoutViewLayoutItem5: TcxGridLayoutItem
        Parent = LayoutViewGroup8
        AlignHorz = ahClient
        SizeOptions.Width = 64
        Index = 3
      end
      object LayoutViewLayoutItem7: TcxGridLayoutItem
        Parent = LayoutViewGroup8
        AlignHorz = ahLeft
        SizeOptions.Width = 118
        Index = 0
      end
      object LayoutViewLayoutItem8: TcxGridLayoutItem
        Parent = LayoutViewGroup10
        AlignHorz = ahLeft
        SizeOptions.Width = 145
        Index = 0
      end
      object LayoutViewLayoutItem9: TcxGridLayoutItem
        Parent = LayoutViewGroup10
        AlignHorz = ahClient
        Index = 2
      end
      object LayoutViewLayoutItem10: TcxGridLayoutItem
        Parent = LayoutViewGroup9
        AlignHorz = ahClient
        Index = 0
      end
      object LayoutViewLayoutItem11: TcxGridLayoutItem
        Parent = LayoutViewGroup9
        AlignHorz = ahClient
        Index = 2
      end
      object LayoutViewLayoutItem12: TcxGridLayoutItem
        Parent = LayoutViewGroup5
        SizeOptions.Width = 89
        Index = 0
      end
      object LayoutViewLayoutItem13: TcxGridLayoutItem
        Parent = LayoutViewGroup12
        AlignHorz = ahClient
        AlignVert = avTop
        CaptionOptions.ImageIndex = 0
        CaptionOptions.Visible = False
        SizeOptions.Height = 353
        Index = 0
      end
      object LayoutViewLayoutItem14: TcxGridLayoutItem
        Parent = LayoutViewGroup7
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.ImageIndex = 1
        CaptionOptions.VisibleElements = [cveImage]
        SizeOptions.Width = 299
        Index = 2
      end
      object LayoutViewLayoutItem15: TcxGridLayoutItem
        Parent = LayoutViewGroup11
        CaptionOptions.Visible = False
        SizeOptions.Height = 94
        SizeOptions.Width = 212
        Index = 2
      end
      object LayoutViewLayoutItem16: TcxGridLayoutItem
        Parent = LayoutViewGroup5
        AlignVert = avBottom
        CaptionOptions.ImageIndex = 3
        SizeOptions.Width = 78
        Index = 2
      end
      object LayoutViewGroup4: TdxLayoutGroup
        Parent = LayoutViewGroup_Root
        AlignHorz = ahLeft
        AlignVert = avClient
        CaptionOptions.Text = 'Product'
        SizeOptions.Width = 201
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        ScrollOptions.Horizontal = smAuto
        Index = 0
      end
      object LayoutViewGroup5: TdxLayoutGroup
        Parent = LayoutViewGroup4
        CaptionOptions.Text = 'Hidden Group'
        SizeOptions.Width = 176
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 2
      end
      object LayoutViewGroup7: TdxLayoutGroup
        Parent = LayoutViewGroup_Root
        AlignHorz = ahLeft
        AlignVert = avClient
        CaptionOptions.ImageIndex = 2
        CaptionOptions.Text = 'Information'
        SizeOptions.Height = 190
        SizeOptions.Width = 323
        ButtonOptions.Buttons = <>
        ItemIndex = 2
        ScrollOptions.Horizontal = smAuto
        Index = 1
      end
      object LayoutViewGroup8: TdxLayoutGroup
        Parent = LayoutViewGroup13
        CaptionOptions.Text = 'Engine'
        SizeOptions.Width = 299
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        Index = 0
      end
      object LayoutViewSpaceItem1: TdxLayoutEmptySpaceItem
        Parent = LayoutViewGroup4
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 11
        Index = 1
      end
      object LayoutViewGroup10: TdxLayoutGroup
        Parent = LayoutViewGroup7
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'Transmission'
        SizeOptions.Width = 283
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        Index = 1
      end
      object LayoutViewSeparatorItem1: TdxLayoutSeparatorItem
        Parent = LayoutViewGroup5
        CaptionOptions.Text = 'Separator'
        SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
        SizeOptions.SizableHorz = False
        SizeOptions.SizableVert = False
        Index = 1
      end
      object LayoutViewGroup9: TdxLayoutGroup
        Parent = LayoutViewGroup13
        CaptionOptions.Text = 'Fuel economy'
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        Index = 1
      end
      object LayoutViewSpaceItem2: TdxLayoutEmptySpaceItem
        Parent = LayoutViewGroup9
        AlignHorz = ahClient
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 22
        Index = 1
      end
      object LayoutViewSpaceItem3: TdxLayoutEmptySpaceItem
        Parent = LayoutViewGroup8
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 21
        Index = 1
      end
      object LayoutViewSpaceItem4: TdxLayoutEmptySpaceItem
        Parent = LayoutViewGroup8
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 15
        Index = 2
      end
      object LayoutViewSpaceItem5: TdxLayoutEmptySpaceItem
        Parent = LayoutViewGroup10
        CaptionOptions.Text = 'Empty Space Item'
        SizeOptions.Height = 10
        SizeOptions.Width = 30
        Index = 1
      end
      object LayoutViewGroup11: TdxLayoutGroup
        Parent = LayoutViewGroup4
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 0
      end
      object LayoutViewGroup12: TdxLayoutGroup
        Parent = LayoutViewGroup_Root
        AlignHorz = ahClient
        AlignVert = avTop
        CaptionOptions.Text = 'Description'
        SizeOptions.Height = 196
        SizeOptions.Width = 274
        ButtonOptions.Buttons = <>
        LayoutDirection = ldHorizontal
        ScrollOptions.Vertical = smAuto
        Index = 2
      end
      object LayoutViewGroup13: TdxLayoutAutoCreatedGroup
        Parent = LayoutViewGroup7
        LayoutDirection = ldHorizontal
        Index = 0
        AutoCreated = True
      end
    end
    object GridLevel1: TcxGridLevel
      GridView = LayoutView
    end
  end
  inherited sbMain: TStatusBar
    Top = 587
    Width = 822
  end
  object cxGroupBox1: TcxGroupBox [3]
    Left = 0
    Top = 16
    Align = alTop
    TabOrder = 2
    Height = 34
    Width = 822
    object seRecordWidth: TcxSpinEdit
      Left = 81
      Top = 9
      Properties.AssignedValues.MinValue = True
      Properties.Increment = 100.000000000000000000
      Properties.OnChange = seRecordWidthPropertiesChange
      TabOrder = 0
      Width = 80
    end
    object lbRecordWidth: TcxLabel
      Left = 8
      Top = 10
      Caption = 'Record Width:'
      Transparent = True
    end
    object lbrecordHeight: TcxLabel
      Left = 168
      Top = 10
      Caption = 'Record Height:'
      Transparent = True
    end
    object seRecordHeight: TcxSpinEdit
      Left = 247
      Top = 9
      Properties.AssignedValues.MinValue = True
      Properties.Increment = 50.000000000000000000
      Properties.OnChange = seRecordHeightPropertiesChange
      TabOrder = 3
      Width = 80
    end
  end
  inherited mmMain: TMainMenu
    Left = 44
    Top = 138
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 156
    Top = 139
    PixelsPerInch = 96
    object stValues: TcxStyle [24]
      AssignedValues = [svFont, svTextColor]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clNavy
    end
    object stItems: TcxStyle [25]
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object stHeader: TcxStyle [26]
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
    object stRecordCaption: TcxStyle [27]
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
    end
    object stRecordSelected: TcxStyle [28]
      AssignedValues = [svFont, svTextColor]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold, fsItalic]
      TextColor = clNavy
    end
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object Images: TcxImageList
    FormatVersion = 1
    DesignInfo = 12058672
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000101010108282
          82FF828282FF828282FF818181FF818181FF818181FFBBB8B4FFBBB8B4FFBBB8
          B4FFBBB8B4FFBBB8B4FFBBB8B4FFBBB8B4FFBBB8B4FFBBB8B4FF404040408F8F
          8FFFFDFDFDFFADBBD7FFF9F9F9FFF9F9F9FFF9F9F9FFBBB8B4FFFEFDFCFFFDFB
          F8FFEDD8CBFFC1805CFFF5E9E0FFFAF3ECFFFAF2E9FFBBB8B4FF404040409999
          99FF959596FFADBBD7FFE2C1A3FFE2C1A3FFE2C1A3FFBBB8B4FFFDFBF8FFFDF9
          F5FFC6855BFFC57941FFCE997AFFFAF2E9FFF9F0E6FFBBB8B4FF40404040A2A2
          A2FFFAFAFAFFADBBD7FFEBEBEBFFEBEBEBFFEBEBEBFFBBB8B4FFFDF9F5FFE9D2
          C4FFD38F55FFD28C4FFFBE6B30FFF3E4D8FFF8EEE2FFBBB8B4FF40404040AAAA
          AAFFE2C1A3FFADBBD7FFE2C1A3FFE2C1A3FFE2C1A3FFBBB8B4FFFCF7F2FFDFBA
          A2FFD1966BFFCD9675FFD08442FFCD9775FFF7ECDFFFBBB8B4FF40404040AFAF
          AFFFFBFBFBFFADBBD7FFF0F0F0FFF0F0F0FFF0F0F0FFBBB8B4FFFBF5EFFFFAF3
          ECFFFAF2E9FFF9F0E6FFCF9060FFB66028FFF0DFCEFFBBB8B4FF40404040B5B5
          B5FFE2C1A3FFADBBD7FFE2C1A3FFE2C1A3FFE2C1A3FFBBB8B4FFFAF3ECFFFAF2
          E9FFF9F0E6FFF8EEE2FFD7AC90FFB6622DFFC88E6DFFBBB8B4FF40404040B8B8
          B8FF959596FFADBBD7FFF4F4F4FFF5F5F5FFF5F5F5FFBBB8B4FFFAF2E9FFF9F0
          E6FFF8EEE2FFF7ECDFFFF6EADCFFBE7A55FFC38561FFBBB8B4FF40404040BBBB
          BBFFE2C1A3FFADBBD7FFE2C1A3FFE2C1A3FFE2C1A3FFBBB8B4FFBBB8B4FFBBB8
          B4FFBBB8B4FFBBB8B4FFBBB8B4FFBBB8B4FFBBB8B4FFBBB8B4FF40404040BDBD
          BDFFFDFDFDFFADBBD7FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FFF7F7F7FFF6F6
          F6FFF5F5F5FFF4F4F4FFFCFCFCFFBDBDBDFF000000000000000040404040BEBE
          BEFFE2C1A3FFADBBD7FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
          A3FFE2C1A3FFE2C1A3FFE2C1A3FFBEBEBEFF000000000000000040404040BFBF
          BFFFFDFDFDFFADBBD7FFFAFAFAFFFAFAFAFFFBFBFBFFFAFAFAFFFAFAFAFFF9F9
          F9FFF8F8F8FFF6F6F6FFFDFDFDFFBFBFBFFF000000000000000040404040C0C0
          C0FFE2C1A3FFADBBD7FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1A3FFE2C1
          A3FFC0C0C0FFCBCBCBFFDBDBDBFFB3B3B3EF000000000000000040404040C0C0
          C0FF959596FFADBBD7FFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFAFA
          FAFFCBCBCBFFE2E2E2FFB4B4B4EF24242430000000000000000040404040C0C0
          C0FFFFFFFFFFADBBD7FFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFEFEFEFFFAFA
          FAFFDBDBDBFFB4B4B4EF2424243000000000000000000000000010101010C0C0
          C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
          C0FFB4B4B4EF2424243000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000006D6D6DBFB2B2B2FF41414170000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000003E3E3E70BDBEBEFF949595FF44444470000000000000
          0000000000001005042042241F8F613D36BF7F6559EF8A8877FF6F645ABF4C3A
          3580080302101B1B1B30A1A1A1EF999A9AFF7F8080CF00000000000000000000
          0000290E0760602915EF6A3926FF744939FF687158FF698B71FF7D9C84FF94AD
          9AFF938C81F08A8787E1A0A0A0FF808282CF0909091000000000000000002E11
          086075310BFF6C2600FF6D2702FF60421CFF63836DFF747B75FF7F827FFF8990
          8AFF8C908AFF9FA0A0FF838281E1090909100000000000000000180904408B41
          0EFF814004FF4D611BFF50631AFF758274FF7B9279FF8CB289FF8FAF8EFF95AD
          97FF9A9F99FF918F90FF886C67FF1005042000000000000000005C2B0C9FA354
          05FF4B8223FF238514FF688C60FF76906CFF8DBB80FF8BB57FFF89AE7FFF87A8
          81FF89A98BFFA0A19DFF78645FFF492B269F00000000000000009D5410EFB96B
          01FF469927FF2A9D0DFF73816CFF92C571FF90C076FF8EBA77FF8DB97CFF89B3
          7FFF89AE84FF90A58BFF8A7B78FF5A2F26EF0000000000000000B46C13FFCC8A
          0FFF60B241FF3BB11EFF7A8C6FFF97CE70FF95CE80FF93CA80FF8FBE7BFF86C4
          8AFF89B07AFF84A377FF8D8782FF452A13FF0000000000000000BE7E1FFFDBA6
          2CFFD7B848FF6CD170FF7B8B77FFA0DD95FFBED894FFAFD493FFA9C686FF8CC1
          81FF8BAE73FF87AA83FF81887BFF2C461BFF0000000000000000B87F2BEFE8BC
          4FFFF0D075FFCCE19DFF87A291FF9CC68EFF9CDF99FF9BDFA6FF8FCE96FF92C2
          81FF8FBD78FF94A18AFF44653FFF273A14EF00000000000000007650259FEECA
          6AFFF7DF9AFFF4EEBFFFC8E6C3FF89908BFF96C995FF9CD481FF8FD191FF82C9
          87FF8BAF8BFF7E8D7AFF0E570EFF262C139F00000000000000001E11093089B2
          3FFFFAE3A9FFFEF1CAFFD9EDCBFFB5D7B0FF87A290FF8F9C88FF95A48BFF8F9E
          8EFF6E936CFF2A7324FF246023FF120A0430000000000000000000000000182B
          06606EC44DFFCFE498FFC1DD89FFD4E0A1FF48DA8BFF4BCE85FF58BC73FF46AE
          6CFF308E39FF2C7B3AFF1D1D1160000000000000000000000000000000000000
          0000182B066018A00FEF1DC221FFA1C759FFB4C971FF8DC065FF5FC478FF54BA
          82FF5E8D69EF24281E6000000000000000000000000000000000000000000000
          0000000000001005042021370980426D35BF438836EF557F30EF426B3EBF2F2A
          1E70090F08200000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00000000000000090B1016A7C4FF16A7C4FF001C223000000000000000000138
          456016A7C4FF16A7C4FF00090B10000000000000000000000000000000000000
          00000000000016A7C4FF73D8E6FF77E0ECFF16A7C4FF16A7C4FF16A7C4FF1BAE
          C9FF37D5E6FF27C8DCFF16A7C4FF000000000000000000000000000000000000
          00000000000016A7C4FF98EAF3FF89E7F1FF74DFECFF6DE1EDFF59D9E8FF50DB
          EAFF41D8E8FF33D4E6FF16A7C4FF000000000000000000000000000000000000
          00000000000016A7C4FF9FEBF4FF8BE4EEFF6AD3E3FF74D6E5FF6DD5E4FF4CCD
          DFFF47D5E6FF37CFE1FF16A7C4FF000000000000000000000000012F395016A7
          C4FF16A7C4FF6BD3E3FF87DFEBFF8EDFEAFF16A7C4FF16A7C4FF16A7C4FF16A7
          C4FF61D5E5FF3FD0E2FF24BAD2FF16A7C4FF16A7C4FF0013172016A7C4FF50CD
          E0FF61D4E4FF84E6F0FF7DD8E6FF16A7C4FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7
          A7FF16A7C4FF5AD0E1FF45D8E8FF2BC5DBFF22C1D7FF16A7C4FF16A7C4FF62DF
          ECFF6FE1EEFF64D5E4FF16A7C4FFA7A7A7FFFEFEFEFFFDFDFDFFF7F7F7FFE5E5
          E5FFA7A7A7FF16A7C4FF45D2E3FF42D8E8FF33D4E6FF16A7C4FF16A7C4FF44CB
          DEFF65DFECFF54CEE0FF16A7C4FFA7A7A7FFF9F9F9FFF9F9F9FFF4F4F4FFE5E5
          E5FFA7A7A7FF16A7C4FF43CBDEFF4DDAE9FF27BED5FF16A7C4FF0000000016A7
          C4FF47CBDEFF4DCDDFFF16A7C4FFA7A7A7FFE8E8E8FFEEEEEEFFE9E9E9FFDADA
          DAFFA7A7A7FF16A7C4FF4CCCDFFF34BDD4FF16A7C4FF00000000000000000013
          172016A7C4FF5FDEECFF16A7C4FFA7A7A7FFBCBCBCFFD5D5D5FFD2D2D2FFB9B9
          B9FFA7A7A7FF16A7C4FF71E1EEFF16A7C4FF000000000000000000000000012F
          395016A7C4FF55DCEAFF4FD1E2FF16A7C4FFA7A7A7FFA7A7A7FFA7A7A7FFA7A7
          A7FF16A7C4FF70D7E6FF7CE4EFFF16A7C4FF001C2230000000000000000016A7
          C4FF40D7E8FF4CDAE9FF58DDEBFF52D1E2FF16A7C4FF16A7C4FF16A7C4FF16A7
          C4FF8BE0ECFF95E9F2FF86E6F0FF78E3EFFF16A7C4FF000000000000000016A7
          C4FF33CEE1FF3BD0E2FF16A7C4FF5CDDEBFF68E0EDFF74E2EEFF81E5F0FF8DE8
          F1FF90E5EFFF16A7C4FF91E8F2FF7BE0ECFF16A7C4FF00000000000000000009
          0B1016A7C4FF16A7C4FF012F395016A7C4FF16A7C4FF6BE0EDFF77E3EFFF16A7
          C4FF16A7C4FF012F395016A7C4FF16A7C4FF00090B1000000000000000000000
          00000000000000000000000000000000000016A7C4FF62DEECFF6EE1EDFF16A7
          C4FF000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000C6F84AF16A7C4FF16A7C4FF0141
          5070000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000503
          020C3A1D1988793C1ADA934811ED7F3E16E046221A9D09040517010305070537
          6882075DB0D50263C4E70056B3D8073A6E8C01070D11000000000603020B6A38
          1DBFC4680FFFD16700FFD77A1AFFD6740EFFC96102FF6B3921D3145191C02D97
          F1FF3B9BF3FF2E91F0FF4A9CF1FF0373E6FF0756A5CC01070E1249260F7BD17D
          2BFFDF882DFFFCF4ECFFFDF9F4FFF8E6D4FFE2852BFF776957FF279FFBFF45A9
          F7FFEFF2F4FFEFF2F3FFE9EEF3FF4097F0FF006EE6FF073D74949E5A21DBE89D
          4FFFFDF7F0FFE5AD74FFE09E5BFFDB8E3FFFC8690BFF4B8BBDFF45B7FFFF30A1
          F5FF79C0F8FFE5EDF3FF007AECFF1783ECFF0074EBFF0661BEE3BF7530F0F2B7
          79FFFFFEFCFFFCF5EEFFFBF3EAFFE29A50FFBD640DFF4AA1E2FF52C0FFFF49B2
          F8FFEEF2F4FFF2F3F4FFD9E7F2FF0E83EDFF097DECFF0669CCEFAF6826DAF5B7
          78FFFEFAF4FFE8AA6AFFE59E55FFE49E56FFD7883CFF60A2D4FF5FCEFFFF4CB9
          FDFF9DD6FCFFEAF0F4FF2A9AF2FF2391F0FF1688F0FF0C65BFE35B300677F0B0
          70FFF9C791FFFEFBF7FFFEF8F2FFFAE9D8FFF4BB89FF7A7F78FF4DB5E1FF59BF
          FAFF7FD1FFFFEAF0F4FFE6EFF3FF4CAAF6FF1D8DEEFF09407696070300099757
          18BDF3B779FFFBC48AFFFFCA9AFFA3975EFF2A7210FF0C860BFF048A17FF127E
          30FF3996A3FF6FC9FFFF6DC4FDFF3AA4F3FF1363AED301080F14000000000A05
          000D6A360A88BB6F36D8857936FA008711FF04B41CFF6FD87AFFC8ECC4FF0AB3
          12FF008802FF2A868BFC3089DDDD114A86990109121700000000000000000000
          0000000000000B0B00240A761DF223C54BFFB6EABDFFEEF4E6FFEEF4E5FFA5E4
          ABFF13BC20FF017708F70311122F000000000000000000000000000000000000
          0000000000000027055A23B252FF3CC96BFF59CD76FF4AC862FFBEE6BBFFF3F6
          EAFF35C342FF01A30FFF0121006A000000000000000000000000000000000000
          00000000000006400B7A2EBE63FF29C265FF41C76BFFF0F5E9FFF5F7ECFF79D6
          86FF08B317FF07AD17FF0231058B000000000000000000000000000000000000
          0000000000000431075525B755FF3DC976FFF3F6EDFF98E1B4FFC3E9C7FF19B9
          3AFF12BA29FF09A318FF02210366000000000000000000000000000000000000
          000000000000010D0215169B2FE839C773FFBAECCFFFEDF4E8FFF0F5EAFFACE7
          BEFF1FC245FF097215F10008001B000000000000000000000000000000000000
          000000000000000000000430074C19AB3BF63FC773FF91E1B2FFD3EFD9FF3EC7
          70FF138E2EFA011C025900000000000000000000000000000000000000000000
          0000000000000000000000000000032005330A7118AD0E972BE70E912AE80860
          15B20218033B0000000000000000000000000000000000000000}
      end>
  end
end

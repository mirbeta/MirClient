inherited frmMain: TfrmMain
  Left = 294
  Top = 129
  Caption = 'ExpressQuantumGrid LayoutView Carousel Mode Demo'
  ClientHeight = 606
  ClientWidth = 1034
  Constraints.MinHeight = 660
  Constraints.MinWidth = 1050
  Position = poScreenCenter
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 1034
    Height = 32
    Caption = 
      'This demo illustrates the Layout View'#39's capabilities in Carou' +
      'sel mode. In this mode, cards are arranged in an ellipse with tra' +
      'nsparency and animation effects that mimic a rolling carousel.'
    Transparent = False
  end
  inherited sbMain: TStatusBar
    Top = 587
    Width = 1034
  end
  object lcMain: TdxLayoutControl [2]
    Left = 0
    Top = 32
    Width = 1034
    Height = 137
    Align = alTop
    TabOrder = 1
    AutoSize = True
    LayoutLookAndFeel = dxLayoutSkinLookAndFeel1
    object btnCustomize: TcxButton
      Left = 916
      Top = 10
      Width = 108
      Height = 29
      Anchors = [akTop, akRight]
      Caption = 'Customize Layout'
      TabOrder = 11
      OnClick = btnCustomizeClick
    end
    object cbExpandableRecords: TcxCheckBox
      Left = 665
      Top = 35
      Caption = 'Record Expand Button'
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 8
      Transparent = True
      OnClick = cbExpandableRecordsClick
      Width = 137
    end
    object cbRecordCaptions: TcxCheckBox
      Left = 665
      Top = 62
      Caption = 'Record Captions'
      State = cbsChecked
      Style.HotTrack = False
      TabOrder = 9
      Transparent = True
      OnClick = cbRecordCaptionsClick
      Width = 137
    end
    object cbMultiSelectRecords: TcxCheckBox
      Left = 665
      Top = 89
      Caption = 'Multi-select Records'
      Style.HotTrack = False
      TabOrder = 10
      Transparent = True
      OnClick = cbMultiSelectRecordsClick
      Width = 145
    end
    object tbPitchAngle: TcxTrackBar
      Left = 85
      Top = 64
      Properties.Frequency = 10
      Properties.Max = 180
      Properties.PageSize = 30
      Properties.ShowChangeButtons = True
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Height = 21
      Width = 196
    end
    object tbEndRecordScale: TcxTrackBar
      Left = 340
      Top = 82
      Properties.Frequency = 10
      Properties.Max = 100
      Properties.PageSize = 10
      Properties.ShowChangeButtons = True
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      Height = 21
      Width = 196
    end
    object tbStartRecordScale: TcxTrackBar
      Left = 340
      Top = 55
      Properties.Frequency = 10
      Properties.Max = 100
      Properties.PageSize = 10
      Properties.ShowChangeButtons = True
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Height = 21
      Width = 196
    end
    object tbRollAngle: TcxTrackBar
      Left = 85
      Top = 10
      Properties.Frequency = 20
      Properties.Max = 360
      Properties.PageSize = 30
      Properties.ShowChangeButtons = True
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
      Height = 21
      Width = 196
    end
    object tbBackgroundAlphaLevel: TcxTrackBar
      Left = 455
      Top = 10
      Properties.Frequency = 10
      Properties.Max = 255
      Properties.PageSize = 10
      Properties.ShowChangeButtons = True
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      Height = 21
      Width = 204
    end
    object cbInterpolationMode: TcxComboBox
      Left = 774
      Top = 10
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'Default'
        'Low Quality'
        'High Quality'
        'Bilinear'
        'Bicubic'
        'Nearest Neighbor'
        'High Quality Bilinear'
        'High Quality Bicubic')
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Width = 121
    end
    object tsAutoPitchAngle: TdxToggleSwitch
      Left = 94
      Top = 37
      Checked = False
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      TabOrder = 1
      Transparent = True
    end
    object tbRecordCount: TcxTrackBar
      Left = 94
      Top = 91
      Position = 1
      Properties.Frequency = 5
      Properties.Max = 21
      Properties.Min = 1
      Properties.PageSize = 10
      Properties.ShowChangeButtons = True
      Properties.OnChange = CarouselModePropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Height = 21
      Width = 204
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCustomize'
      CaptionOptions.Visible = False
      Control = btnCustomize
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cbExpandableRecords'
      CaptionOptions.Visible = False
      Control = cbExpandableRecords
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cbRecordCaptions'
      CaptionOptions.Visible = False
      Control = cbRecordCaptions
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cbMultiSelectRecords'
      CaptionOptions.Visible = False
      Control = cbMultiSelectRecords
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 2
    end
    object liPitchAngle: TdxLayoutItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Pitch Angle:'
      Control = tbPitchAngle
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'End:'
      Control = tbEndRecordScale
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcMainGroup3
      CaptionOptions.Text = 'Start:'
      Control = tbStartRecordScale
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem8: TdxLayoutItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Roll Angle:'
      Control = tbRollAngle
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Background Record Scale'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object lcMainItem9: TdxLayoutItem
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Background Record Alpha Level:'
      Control = tbBackgroundAlphaLevel
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem11: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Interpolation Mode:'
      Control = cbInterpolationMode
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup7: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lcMainGroup5: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Auto Pitch Angle'
      Control = tsAutoPitchAngle
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem10: TdxLayoutItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Record Count:'
      Control = tbRecordCount
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
  object Grid: TcxGrid [3]
    Left = 0
    Top = 169
    Width = 1034
    Height = 418
    Align = alClient
    TabOrder = 2
    LevelTabs.Slants.Positions = []
    object LayoutView: TcxGridDBLayoutView
      Navigator.Buttons.CustomButtons = <>
      FilterBox.Visible = fvNever
      DataController.DataSource = dsHomes
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.RecordExpanding = True
      OptionsView.FocusRect = False
      OptionsView.CarouselMode.PitchAngle = 80.000000000000000000
      OptionsView.MinValueWidth = 40
      OptionsView.ViewMode = lvvmCarousel
      object LayoutViewRecId: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'RecId'
        Visible = False
        LayoutItem = LayoutViewLayoutItem1
      end
      object LayoutViewAddress: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Address'
        LayoutItem = LayoutViewLayoutItem3
      end
      object LayoutViewBeds: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Beds'
        LayoutItem = LayoutViewLayoutItem4
      end
      object LayoutViewBaths: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Baths'
        LayoutItem = LayoutViewLayoutItem5
      end
      object LayoutViewHouseSize: TcxGridDBLayoutViewItem
        Caption = 'Size'
        DataBinding.FieldName = 'HouseSize'
        LayoutItem = LayoutViewLayoutItem6
      end
      object LayoutViewPrice: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Price'
        RepositoryItem = EditRepositoryPrice
        LayoutItem = LayoutViewLayoutItem8
      end
      object LayoutViewFeatures: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Features'
        RepositoryItem = EditRepositoryMemo
        LayoutItem = LayoutViewLayoutItem9
      end
      object LayoutViewYearBuilt: TcxGridDBLayoutViewItem
        Caption = 'Year Built'
        DataBinding.FieldName = 'YearBuilt'
        RepositoryItem = EditRepositorySpinItem
        LayoutItem = LayoutViewLayoutItem10
      end
      object LayoutViewPhoto: TcxGridDBLayoutViewItem
        DataBinding.FieldName = 'Photo'
        RepositoryItem = EditRepositoryImage
        LayoutItem = LayoutViewLayoutItem13
      end
      object dxLayoutGroup1: TdxLayoutGroup
        AlignHorz = ahLeft
        AlignVert = avTop
        CaptionOptions.Text = 'Template Card'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = -1
      end
      object LayoutViewLayoutItem1: TcxGridLayoutItem
        Index = -1
      end
      object LayoutViewLayoutItem3: TcxGridLayoutItem
        Parent = LayoutViewGroup13
        AlignHorz = ahClient
        CaptionOptions.Visible = False
        SizeOptions.Width = 226
        Index = 0
      end
      object LayoutViewLayoutItem4: TcxGridLayoutItem
        Parent = LayoutViewGroup3
        Index = 2
      end
      object LayoutViewLayoutItem5: TcxGridLayoutItem
        Parent = LayoutViewGroup3
        Index = 3
      end
      object LayoutViewLayoutItem6: TcxGridLayoutItem
        Parent = LayoutViewGroup3
        Index = 1
      end
      object LayoutViewLayoutItem8: TcxGridLayoutItem
        Parent = LayoutViewGroup13
        CaptionOptions.ImageIndex = 3
        CaptionOptions.Visible = False
        SizeOptions.Width = 139
        Index = 1
      end
      object LayoutViewLayoutItem9: TcxGridLayoutItem
        Parent = LayoutViewGroup3
        CaptionOptions.Visible = False
        SizeOptions.Width = 175
        Index = 4
      end
      object LayoutViewLayoutItem10: TcxGridLayoutItem
        Parent = LayoutViewGroup3
        Index = 0
      end
      object LayoutViewLayoutItem13: TcxGridLayoutItem
        Parent = LayoutViewGroup4
        AlignVert = avClient
        CaptionOptions.Visible = False
        SizeOptions.Width = 251
        Index = 0
      end
      object LayoutViewGroup4: TdxLayoutGroup
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        AlignVert = avTop
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 1
      end
      object LayoutViewGroup13: TdxLayoutGroup
        Parent = dxLayoutGroup1
        AlignHorz = ahClient
        AlignVert = avTop
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        LayoutDirection = ldHorizontal
        ShowBorder = False
        Index = 0
      end
      object LayoutViewGroup3: TdxLayoutGroup
        Parent = LayoutViewGroup4
        AlignHorz = ahClient
        CaptionOptions.Text = 'Hidden Group'
        ButtonOptions.Buttons = <>
        Hidden = True
        ShowBorder = False
        Index = 1
      end
    end
    object GridLevel1: TcxGridLevel
      GridView = LayoutView
    end
  end
  inherited mmMain: TMainMenu
    Left = 44
    Top = 138
    object miView: TMenuItem [1]
      Caption = '&View'
      object miCustomize: TMenuItem
        Caption = 'Customize...'
        OnClick = miCustomizeClick
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    Left = 156
    Top = 139
    PixelsPerInch = 96
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 464
    Top = 68
  end
  object dsHomes: TDataSource
    DataSet = mdHomes
    Left = 72
    Top = 138
  end
  object EditRepository: TcxEditRepository
    Left = 100
    Top = 138
    object EditRepositoryImage: TcxEditRepositoryImageItem
      Properties.FitMode = ifmProportionalStretch
      Properties.GraphicClassName = 'TdxSmartImage'
    end
    object EditRepositoryMemo: TcxEditRepositoryMemoItem
      Properties.VisibleLineCount = 10
    end
    object EditRepositoryPrice: TcxEditRepositoryCurrencyItem
      Properties.AutoSelect = False
      Properties.HideSelection = False
    end
    object EditRepositorySpinItem: TcxEditRepositorySpinItem
    end
  end
  object mdHomes: TdxMemData
    Active = True
    Indexes = <>
    Persistent.Data = {
      5665728FC2F5285C8FFE3F08000000000000000E000800416464726573730002
      0000000200050042656473000200000002000600426174687300080000000600
      0A00486F75736553697A65000800000006000600507269636500000000000E00
      0900466561747572657300000000000E000A00596561724275696C7400000000
      000D00060050686F746F00}
    SortOptions = []
    Left = 128
    Top = 138
    object mdHomesAddress: TMemoField
      FieldName = 'Address'
      BlobType = ftMemo
    end
    object mdHomesBeds: TSmallintField
      FieldName = 'Beds'
    end
    object mdHomesBaths: TSmallintField
      FieldName = 'Baths'
    end
    object mdHomesHouseSize: TFloatField
      FieldName = 'HouseSize'
      DisplayFormat = '#.00 Sq Ft'
    end
    object mdHomesPrice: TFloatField
      FieldName = 'Price'
    end
    object mdHomesFeatures: TMemoField
      FieldName = 'Features'
      BlobType = ftMemo
    end
    object mdHomesYearBuilt: TMemoField
      FieldName = 'YearBuilt'
      BlobType = ftMemo
    end
    object mdHomesPhoto: TBlobField
      FieldName = 'Photo'
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 184
    Top = 139
    object dxLayoutSkinLookAndFeel1: TdxLayoutSkinLookAndFeel
    end
  end
end

inherited dxfmTreeListReportLinkDesignWindow: TdxfmTreeListReportLinkDesignWindow
  Left = 232
  Top = 184
  Caption = 'Property Sheets'
  ClientHeight = 449
  ClientWidth = 713
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 713
    Height = 449
    inherited btnApply: TcxButton
      Top = 432
      TabOrder = 61
    end
    inherited btnCancel: TcxButton
      Top = 432
      TabOrder = 60
    end
    inherited btnOK: TcxButton
      Top = 432
      TabOrder = 59
    end
    inherited btnHelp: TcxButton
      Top = 432
      TabOrder = 62
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 432
      TabOrder = 63
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 432
      TabOrder = 64
    end
    inherited btnTitleProperties: TcxButton
      Top = 432
      TabOrder = 65
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 432
      TabOrder = 66
    end
    object imgShow: TcxImage [8]
      Left = 21
      Top = 68
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Height = 48
      Width = 48
    end
    object imgOnEveryPage: TcxImage [9]
      Left = 21
      Top = 230
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Height = 48
      Width = 48
    end
    object imgSelection: TcxImage [10]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 14
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgExpanding: TcxImage [11]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgGridSize: TcxImage [12]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgSeparators: TcxImage [13]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 25
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgLookAndFeel: TcxImage [14]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 29
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgRefinements: TcxImage [15]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 32
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgPreview: TcxImage [16]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 54
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object pnlPreview: TPanel [17]
      Left = 357
      Top = 29
      Width = 490
      Height = 380
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 58
      object PreviewTreeList: TcxTreeList
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 484
        Height = 374
        Align = alClient
        Bands = <
          item
            Caption.Text = 'Manufacturer Data'
            Width = 187
          end
          item
            Caption.Text = 'Car Data'
            Width = 201
          end>
        DefaultRowHeight = 17
        Enabled = False
        Navigator.Buttons.CustomButtons = <>
        OptionsView.CellAutoHeight = True
        OptionsView.ScrollBars = ssNone
        OptionsView.Bands = True
        OptionsView.ColumnAutoWidth = True
        OptionsView.GridLines = tlglBoth
        OptionsView.UseNodeColorForIndent = False
        Preview.Column = colManufacturerCountry
        TabOrder = 0
        OnCustomDrawBandHeaderCell = PreviewTreeListCustomDrawBandHeaderCell
        OnCustomDrawDataCell = PreviewTreeListCustomDrawDataCell
        OnCustomDrawFooterCell = PreviewTreeListCustomDrawFooterCell
        OnCustomDrawHeaderCell = PreviewTreeListCustomDrawHeaderCell
        object colManufacturerName: TcxTreeListColumn
          Caption.Text = 'Name'
          DataBinding.ValueType = 'String'
          Width = 135
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object colManufacturerLogo: TcxTreeListColumn
          Caption.Text = 'Logo'
          DataBinding.ValueType = 'String'
          Width = 76
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object colManufacturerCountry: TcxTreeListColumn
          Caption.Text = 'Country'
          DataBinding.ValueType = 'String'
          Width = 167
          Position.ColIndex = 2
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object colCarModel: TcxTreeListColumn
          Caption.Text = 'Model'
          DataBinding.ValueType = 'String'
          Width = 89
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 1
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object colCarIsSUV: TcxTreeListColumn
          Caption.Text = 'SUV'
          DataBinding.ValueType = 'String'
          Width = 35
          Position.ColIndex = 1
          Position.RowIndex = 0
          Position.BandIndex = 1
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object colSpeedCount: TcxTreeListColumn
          PropertiesClassName = 'TcxTrackBarProperties'
          Properties.Max = 8
          Caption.Text = 'Speed count'
          DataBinding.ValueType = 'String'
          Width = 103
          Position.ColIndex = 2
          Position.RowIndex = 0
          Position.BandIndex = 1
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
      end
    end
    object lblShow: TcxLabel [18]
      Left = 21
      Top = 44
      AutoSize = False
      Caption = 'Show'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 318
    end
    object chbxShowBands: TcxCheckBox [19]
      Left = 75
      Top = 68
      Caption = 'Ba&nds'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object chbxShowHeaders: TcxCheckBox [20]
      Tag = 1
      Left = 75
      Top = 91
      Caption = '&Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object chbxShowFooters: TcxCheckBox [21]
      Tag = 2
      Left = 75
      Top = 114
      Caption = 'Foo&ters'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object chbxShowBorders: TcxCheckBox [22]
      Tag = 5
      Left = 75
      Top = 137
      Caption = 'Borders'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object chbxShowExpandButtons: TcxCheckBox [23]
      Tag = 3
      Left = 75
      Top = 160
      Caption = 'Expand Buttons'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object chbxShowTreeLines: TcxCheckBox [24]
      Tag = 4
      Left = 75
      Top = 183
      Caption = 'TreeLines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object lblOnEveryPage: TcxLabel [25]
      Left = 21
      Top = 206
      AutoSize = False
      Caption = 'On Every Page'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 318
    end
    object chbxBandsOnEveryPage: TcxCheckBox [26]
      Left = 75
      Top = 230
      Caption = 'Ba&nds'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      OnClick = OptionsOnEveryPageClick
      Width = 264
    end
    object chbxHeadersOnEveryPage: TcxCheckBox [27]
      Tag = 1
      Left = 75
      Top = 253
      Caption = 'Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      OnClick = OptionsOnEveryPageClick
      Width = 264
    end
    object chbxFootersOnEveryPage: TcxCheckBox [28]
      Tag = 2
      Left = 75
      Top = 276
      Caption = 'Footers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      OnClick = OptionsOnEveryPageClick
      Width = 264
    end
    object lblSelection: TcxLabel [29]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Selection'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object lblExpanding: TcxLabel [30]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Expanding'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object lblSize: TcxLabel [31]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Size'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object lblSeparators: TcxLabel [32]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Separators'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object chbxProcessSelection: TcxCheckBox [33]
      Left = 10000
      Top = 10000
      Caption = 'Process Selection'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      Visible = False
      OnClick = OptionsSelectionClick
      Width = 264
    end
    object chbxProcessExactSelection: TcxCheckBox [34]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Process Exact Selection'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Transparent = True
      Visible = False
      OnClick = OptionsSelectionClick
      Width = 264
    end
    object chbxExpandNodes: TcxCheckBox [35]
      Left = 10000
      Top = 10000
      Caption = 'Nodes'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      OnClick = OptionsExpandingClick
      Width = 264
    end
    object chbxExplicitlyExpandNodes: TcxCheckBox [36]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Explicitly Expand Nodes'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      OnClick = OptionsExpandingClick
      Width = 264
    end
    object chbxAutoWidth: TcxCheckBox [37]
      Left = 10000
      Top = 10000
      Caption = '&Auto Width'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      OnClick = OptionsSizeClick
      Width = 264
    end
    object seSeparatorThickness: TcxSpinEdit [38]
      Left = 10000
      Top = 10000
      Properties.MaxValue = 16.000000000000000000
      Properties.OnChange = SeparatorThicknessChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 26
      Visible = False
      Width = 150
    end
    object ccbxSeparatorColor: TcxColorComboBox [39]
      Left = 10000
      Top = 10000
      Properties.AllowSelectColor = True
      Properties.CustomColors = <>
      Properties.OnChange = SeparatorColorChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Visible = False
      Width = 150
    end
    object lblLookAndFeel: TcxLabel [40]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Look and Feel'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object lblRefinements: TcxLabel [41]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Refinements'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object cbxLookAndFeel: TcxComboBox [42]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = LookAndFeelChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 30
      Visible = False
      Width = 264
    end
    object chbxTransparentGraphics: TcxCheckBox [43]
      Left = 10000
      Top = 10000
      Caption = 'Transparent &Graphics'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 33
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxDisplayGraphicsAsText: TcxCheckBox [44]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Display Graphics As &Text'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 34
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxFlatCheckMarks: TcxCheckBox [45]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Flat Check &Marks'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 35
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxDisplayTrackBarsAsText: TcxCheckBox [46]
      Tag = 4
      Left = 10000
      Top = 10000
      Caption = 'Display Track &Bars As Text'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 36
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxSuppressBackgroundBitmaps: TcxCheckBox [47]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Suppress Background Textures'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 37
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingClick
      Width = 264
    end
    object chbxConsumeSelectionStyle: TcxCheckBox [48]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Consume Selection Style'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 38
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingClick
      Width = 264
    end
    object lblPreviewOptions: TcxLabel [49]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object chbxPreviewVisible: TcxCheckBox [50]
      Left = 10000
      Top = 10000
      Caption = 'Visible'
      Style.HotTrack = False
      TabOrder = 55
      Transparent = True
      Visible = False
      OnClick = PreviewVisibleClick
      Width = 264
    end
    object chbxPreviewAutoHeight: TcxCheckBox [51]
      Left = 10000
      Top = 10000
      Caption = 'Auto Height'
      Style.HotTrack = False
      TabOrder = 56
      Transparent = True
      Visible = False
      OnClick = PreviewAutoHeightClick
      Width = 264
    end
    object sePreviewMaxLineCount: TcxSpinEdit [52]
      Left = 10000
      Top = 10000
      Properties.MaxValue = 1000.000000000000000000
      Properties.OnChange = PreviewMaxLineCountChanged
      Style.HotTrack = False
      TabOrder = 57
      Visible = False
      Width = 124
    end
    object chbxUseNativeStyles: TcxCheckBox [53]
      Left = 10000
      Top = 10000
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 39
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingClick
      Height = 21
      Width = 21
    end
    object lblUseNativeStyles: TcxLabel [54]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = '&Use Native Styles'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      OnClick = lblUseNativeStylesClick
      Height = 18
      Width = 291
      AnchorY = 10009
    end
    object btnStyleFont: TcxButton [55]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Font...'
      TabOrder = 43
      Visible = False
      OnClick = StyleFontClick
    end
    object btnStyleColor: TcxButton [56]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Co&lor...'
      TabOrder = 44
      Visible = False
      OnClick = StyleColorClick
    end
    object btnStyleBackgroundBitmap: TcxButton [57]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Texture...'
      TabOrder = 45
      Visible = False
      OnClick = StyleBackgroundBitmapClick
    end
    object btnStyleBackgroundBitmapClear: TcxButton [58]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Clear'
      TabOrder = 46
      Visible = False
      OnClick = StyleBackgroundBitmapClearClick
    end
    object btnStyleRestoreDefaults: TcxButton [59]
      Left = 10000
      Top = 10000
      Width = 116
      Height = 23
      Caption = 'Restore Defaults'
      TabOrder = 41
      Visible = False
      OnClick = StyleRestoreDefaultsClick
    end
    object btnStylesSaveAs: TcxButton [60]
      Left = 10000
      Top = 10000
      Width = 115
      Height = 23
      Caption = 'Save &As...'
      TabOrder = 42
      Visible = False
      OnClick = StylesSaveAsClick
    end
    object lblStyleSheets: TcxLabel [61]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Style Sheets'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object cbxStyleSheets: TcxComboBox [62]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnDrawItem = cbxStyleSheetsPropertiesDrawItem
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 48
      Visible = False
      OnClick = cbxStyleSheetsClick
      OnKeyDown = cbxStyleSheetsKeyDown
      Width = 318
    end
    object btnStyleSheetNew: TcxButton [63]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&New...'
      TabOrder = 50
      Visible = False
      OnClick = btnStyleSheetNewClick
    end
    object btnStyleSheetCopy: TcxButton [64]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Copy...'
      TabOrder = 51
      Visible = False
      OnClick = btnStyleSheetCopyClick
    end
    object btnStyleSheetDelete: TcxButton [65]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Delete...'
      TabOrder = 52
      Visible = False
      OnClick = btnStyleSheetDeleteClick
    end
    object btnStyleSheetRename: TcxButton [66]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Rename...'
      TabOrder = 49
      Visible = False
      OnClick = btnStyleSheetRenameClick
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
    end
    inherited dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Index = 1
    end
    object lblPreviewWindow: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'lblPreviewWindow'
      CaptionOptions.Layout = clTop
      SizeOptions.Height = 400
      SizeOptions.Width = 400
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 400
      ControlOptions.OriginalWidth = 400
      Index = 1
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
      Index = 0
    end
    object tshView: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshView'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 0
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshBehaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 7
      Index = 1
    end
    object tshFormatting: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFormatting'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 2
    end
    object tshStyles: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshStyles'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 3
    end
    object tshPreview: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshPreview'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 4
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = tshView
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
      Control = imgShow
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxShowBands
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 53
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowHeaders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowFooters
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 61
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 61
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowExpandButtons
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowTreeLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 70
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = tshView
      CaptionOptions.Visible = False
      Control = lblOnEveryPage
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = imgOnEveryPage
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxBandsOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 53
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Visible = False
      Control = chbxHeadersOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Visible = False
      Control = chbxFootersOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 61
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = tshView
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = tshView
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object bvlWarningHost: TdxLayoutItem
      Parent = tshView
      AlignVert = avClient
      SizeOptions.Height = 66
      Index = 4
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
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblExpanding
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSize
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSeparators
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      Control = imgSelection
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxProcessSelection
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 107
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      CaptionOptions.Visible = False
      Control = chbxProcessExactSelection
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 137
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahLeft
      Control = imgExpanding
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxExpandNodes
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 54
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Visible = False
      Control = chbxExplicitlyExpandNodes
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 137
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahLeft
      Control = imgGridSize
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahLeft
      AlignVert = avTop
      Control = imgSeparators
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object lblSeparatorsThickness: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      CaptionOptions.Text = 'lblSeparatorsThickness'
      Control = seSeparatorThickness
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 7
      AutoCreated = True
    end
    object lblSeparatorsColor: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      CaptionOptions.Text = 'lblSeparatorsColor'
      Control = ccbxSeparatorColor
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblLookAndFeel
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblRefinements
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahLeft
      Control = imgLookAndFeel
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup14
      AlignHorz = ahLeft
      Control = imgRefinements
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahClient
      Control = cbxLookAndFeel
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem32: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentGraphics
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 127
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup14: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem33: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Visible = False
      Control = chbxDisplayGraphicsAsText
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 142
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup14
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem34: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Visible = False
      Control = chbxFlatCheckMarks
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem35: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Visible = False
      Control = chbxDisplayTrackBarsAsText
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 151
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem36: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Visible = False
      Control = chbxSuppressBackgroundBitmaps
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem37: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Visible = False
      Control = chbxConsumeSelectionStyle
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 141
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup15
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
    object dxLayoutItem38: TdxLayoutItem
      Parent = tshPreview
      CaptionOptions.Visible = False
      Control = lblPreviewOptions
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem39: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup16
      AlignHorz = ahLeft
      Control = imgPreview
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem40: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup17
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxPreviewVisible
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 53
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup16: TdxLayoutAutoCreatedGroup
      Parent = tshPreview
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem41: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup17
      CaptionOptions.Visible = False
      Control = chbxPreviewAutoHeight
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup17: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup16
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object lblPreviewMaxLineCount: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup17
      CaptionOptions.Text = 'lblPreviewMaxLineCount'
      Offsets.Left = 19
      Control = sePreviewMaxLineCount
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem42: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup18
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxUseNativeStyles
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem43: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup18
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = lblUseNativeStyles
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup18: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem44: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem45: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleColor
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem46: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleBackgroundBitmap
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem47: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleBackgroundBitmapClear
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup19
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      ShowBorder = False
      Index = 1
    end
    object bvlStylesHost: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup20
      AlignHorz = ahClient
      AlignVert = avClient
      SizeOptions.Height = 200
      SizeOptions.Width = 200
      Index = 0
    end
    object dxLayoutAutoCreatedGroup19: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem49: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup21
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStyleRestoreDefaults
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup20: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup19
      AlignHorz = ahClient
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem50: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup21
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStylesSaveAs
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup21: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup20
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem51: TdxLayoutItem
      Parent = tshStyles
      CaptionOptions.Visible = False
      Control = lblStyleSheets
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem52: TdxLayoutItem
      Parent = tshStyles
      Control = cbxStyleSheets
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem53: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup22
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStyleSheetNew
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem54: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup22
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetCopy
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutAutoCreatedGroup22: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object dxLayoutItem55: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup22
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem56: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup22
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetRename
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 4
    Top = 362
    PixelsPerInch = 96
    object styleBandHeaders: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
    end
    object styleStandard: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object stylePreview: TcxStyle
      AssignedValues = [svFont]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object styleCardShadow: TcxStyle
    end
  end
  object pmStyles: TPopupMenu
    Images = ilStylesPopup
    OnPopup = pmStylesPopup
    Left = 32
    Top = 362
    object miStyleFont: TMenuItem
      Caption = '&Font...'
      ImageIndex = 0
      ShortCut = 16454
      OnClick = StyleFontClick
    end
    object miStyleColor: TMenuItem
      Caption = '&Color...'
      OnClick = StyleColorClick
    end
    object miLine3: TMenuItem
      Caption = '-'
    end
    object miStyleBackgroundBitmap: TMenuItem
      Caption = '&Texture...'
      ImageIndex = 1
      OnClick = StyleBackgroundBitmapClick
    end
    object miStyleBackgroundBitmapClear: TMenuItem
      Caption = 'Clear'
      ImageIndex = 3
      ShortCut = 16430
      OnClick = StyleBackgroundBitmapClearClick
    end
    object milLine: TMenuItem
      Caption = '-'
    end
    object miStylesSelectAll: TMenuItem
      Caption = 'Select A&ll'
      ShortCut = 16449
      OnClick = miStylesSelectAllClick
    end
    object miLine2: TMenuItem
      Caption = '-'
    end
    object miStyleRestoreDefaults: TMenuItem
      Caption = 'Restore Defaults'
      OnClick = StyleRestoreDefaultsClick
    end
    object miLine4: TMenuItem
      Caption = '-'
    end
    object miStylesSaveAs: TMenuItem
      Caption = 'Save &As...'
      ImageIndex = 2
      ShortCut = 16467
      OnClick = StylesSaveAsClick
    end
  end
  object ilStylesPopup: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 23593024
  end
end

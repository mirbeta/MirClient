inherited cxfmVerticalGridReportLinkDesignWindow: TcxfmVerticalGridReportLinkDesignWindow
  Left = 373
  Top = 217
  Caption = 'cxfmVerticalGridReportLinkDesignWindow'
  ClientHeight = 465
  ClientWidth = 705
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 705
    Height = 465
    inherited btnApply: TcxButton
      Top = 411
      TabOrder = 47
    end
    inherited btnCancel: TcxButton
      Top = 411
      TabOrder = 46
    end
    inherited btnOK: TcxButton
      Top = 411
      TabOrder = 45
    end
    inherited btnHelp: TcxButton
      Top = 411
      TabOrder = 48
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 411
      TabOrder = 49
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 411
      TabOrder = 50
    end
    inherited btnTitleProperties: TcxButton
      Top = 411
      TabOrder = 51
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 411
      TabOrder = 52
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
    object imgExpanding: TcxImage [9]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgGridSize: TcxImage [10]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgLookAndFeel: TcxImage [11]
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
    object imgRefinements: TcxImage [12]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object imgPagination: TcxImage [13]
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
    object pnlPreview: TPanel [14]
      Left = 357
      Top = 29
      Width = 490
      Height = 359
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 44
      object PreviewVGrid: TcxVerticalGrid
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 484
        Height = 353
        BorderStyle = cxcbsNone
        Align = alClient
        OptionsView.ScrollBars = ssNone
        OptionsView.RowHeaderWidth = 112
        OptionsView.ValueWidth = 150
        TabOrder = 0
        OnDrawValue = PreviewVGridDrawValue
        Version = 1
        object rowLuxurySedan: TcxCategoryRow
          Properties.Caption = 'Luxury sedans'
          ID = 0
          ParentID = -1
          Index = 0
          Version = 1
        end
        object rowManufacturer: TcxEditorRow
          Properties.Caption = 'Manufacturer'
          Properties.EditPropertiesClassName = 'TcxTextEditProperties'
          Properties.EditProperties.MaxLength = 0
          Properties.DataBinding.ValueType = 'String'
          Properties.Value = 'BMW'
          ID = 1
          ParentID = 0
          Index = 0
          Version = 1
        end
        object rowModel: TcxEditorRow
          Properties.Caption = 'Model'
          Properties.EditPropertiesClassName = 'TcxTextEditProperties'
          Properties.EditProperties.MaxLength = 0
          Properties.DataBinding.ValueType = 'String'
          Properties.Value = '760 Li V12'
          ID = 2
          ParentID = 0
          Index = 1
          Version = 1
        end
        object rowPicture: TcxEditorRow
          Height = 116
          Properties.Caption = 'Picture'
          Properties.EditPropertiesClassName = 'TcxImageProperties'
          Properties.DataBinding.ValueType = 'String'
          Properties.Value = Null
          ID = 3
          ParentID = 0
          Index = 2
          Version = 1
        end
        object rowEngine: TcxEditorRow
          Height = 57
          Properties.Caption = 'Engine'
          Properties.EditPropertiesClassName = 'TcxMemoProperties'
          Properties.EditProperties.MaxLength = 0
          Properties.DataBinding.ValueType = 'String'
          Properties.Value = 
            '6.0L DOHC V12 438 HP 48V DI Valvetronic 12-cylinder engine with ' +
            '6.0-liter displacement, dual overhead cam valvetrain'
          ID = 4
          ParentID = 0
          Index = 3
          Version = 1
        end
        object rowTransmission: TcxEditorRow
          Height = 17
          Properties.Caption = 'Transmission'
          Properties.DataBinding.ValueType = 'String'
          Properties.Value = 'Elec 6-Speed Automatic w/Steptronic'
          ID = 5
          ParentID = 0
          Index = 4
          Version = 1
        end
        object rowTires: TcxEditorRow
          Height = 44
          Properties.Caption = 'Tires'
          Properties.EditPropertiesClassName = 'TcxMemoProperties'
          Properties.EditProperties.MaxLength = 0
          Properties.DataBinding.ValueType = 'String'
          Properties.Value = 
            'P245/45R19 Fr - P275/40R19 Rr Performance. Low Profile tires wit' +
            'h 245mm width, 19.0" rim'
          ID = 6
          ParentID = 0
          Index = 5
          Version = 1
        end
      end
    end
    object lblShow: TcxLabel [15]
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
    object chbxShowHeaders: TcxCheckBox [16]
      Left = 75
      Top = 68
      Caption = '&Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object chbxShowBorders: TcxCheckBox [17]
      Tag = 2
      Left = 75
      Top = 91
      Caption = 'Borders'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object chbxShowExpandButtons: TcxCheckBox [18]
      Tag = 1
      Left = 75
      Top = 114
      Caption = 'Expand Buttons'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = OptionsViewClick
      Width = 264
    end
    object cbxPrintMode: TcxComboBox [19]
      Left = 138
      Top = 149
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbxPrintModeChange
      Style.HotTrack = False
      TabOrder = 5
      Width = 201
    end
    object lblExpanding: TcxLabel [20]
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
    object lblSize: TcxLabel [21]
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
    object chbxExpandRows: TcxCheckBox [22]
      Left = 10000
      Top = 10000
      Caption = 'Rows'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      Visible = False
      OnClick = OptionsExpandingClick
      Width = 46
    end
    object chbxAutoWidth: TcxCheckBox [23]
      Left = 10000
      Top = 10000
      Caption = '&Auto Width'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Visible = False
      OnClick = OptionsSizeClick
      Width = 74
    end
    object chbxBestFit: TcxCheckBox [24]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'BestFit'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Visible = False
      OnClick = OptionsSizeClick
      Width = 53
    end
    object chbxKeepSameRecordWidths: TcxCheckBox [25]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Keep Equal Record Widths'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      Visible = False
      OnClick = OptionsSizeClick
      Width = 146
    end
    object chbxWrapRecords: TcxCheckBox [26]
      Tag = 3
      Left = 10000
      Top = 10000
      Caption = '&Wrap Records'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Transparent = True
      Visible = False
      OnClick = OptionsSizeClick
      Width = 88
    end
    object lblLookAndFeel: TcxLabel [27]
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
    object lblRefinements: TcxLabel [28]
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
    object lblPagination: TcxLabel [29]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Pagination'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
    end
    object cbxLookAndFeel: TcxComboBox [30]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = LookAndFeelChange
      Style.HotTrack = False
      TabOrder = 17
      Visible = False
      Width = 264
    end
    object chbxTransparentGraphics: TcxCheckBox [31]
      Left = 10000
      Top = 10000
      Caption = 'Transparent &Graphics'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxDisplayGraphicsAsText: TcxCheckBox [32]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Display Graphics As &Text'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 21
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxFlatCheckMarks: TcxCheckBox [33]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Flat Check &Marks'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxSuppressBackgroundBitmaps: TcxCheckBox [34]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Suppress Background Textures'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingClick
      Width = 264
    end
    object chbxDisplayTrackBarsAsText: TcxCheckBox [35]
      Tag = 4
      Left = 10000
      Top = 10000
      Caption = 'Display Track &Bars As Text'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
      Visible = False
      OnClick = OptionsRefinementsClick
      Width = 264
    end
    object chbxPaginateByRows: TcxCheckBox [36]
      Left = 10000
      Top = 10000
      Caption = 'By Rows'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Transparent = True
      Visible = False
      OnClick = OptionsPaginationClick
      Width = 264
    end
    object chbxPaginateByWrapping: TcxCheckBox [37]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'By Wrapping'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 28
      Transparent = True
      Visible = False
      OnClick = OptionsPaginationClick
      Width = 264
    end
    object chbxOneWrappingPerPage: TcxCheckBox [38]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'One Wrapping Per Page'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 29
      Transparent = True
      Visible = False
      OnClick = OptionsPaginationClick
      Width = 264
    end
    object chbxUseNativeStyles: TcxCheckBox [39]
      Left = 10000
      Top = 10000
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 30
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingClick
      Height = 21
      Width = 21
    end
    object lblUseNativeStyles: TcxLabel [40]
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
    object btnStyleFont: TcxButton [41]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Font...'
      TabOrder = 34
      Visible = False
      OnClick = StyleFontClick
    end
    object btnStyleColor: TcxButton [42]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Co&lor...'
      TabOrder = 35
      Visible = False
      OnClick = StyleColorClick
    end
    object btnStyleBackgroundBitmap: TcxButton [43]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Bitmap...'
      TabOrder = 36
      Visible = False
      OnClick = StyleBackgroundBitmapClick
    end
    object btnStyleBackgroundBitmapClear: TcxButton [44]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Clear'
      TabOrder = 37
      Visible = False
      OnClick = StyleBackgroundBitmapClearClick
    end
    object btnStyleRestoreDefaults: TcxButton [45]
      Left = 10000
      Top = 10000
      Width = 116
      Height = 23
      Caption = 'Restore Defaults'
      TabOrder = 32
      Visible = False
      OnClick = StyleRestoreDefaultsClick
    end
    object btnStylesSaveAs: TcxButton [46]
      Left = 10000
      Top = 10000
      Width = 115
      Height = 23
      Caption = 'Save &As...'
      TabOrder = 33
      Visible = False
      OnClick = StylesSaveAsClick
    end
    object lblStyleSheets: TcxLabel [47]
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
    object cbxStyleSheets: TcxComboBox [48]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 18
      Properties.OnDrawItem = cbxStyleSheetsPropertiesDrawItem
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 39
      Visible = False
      OnClick = cbxStyleSheetsClick
      OnKeyDown = cbxStyleSheetsKeyDown
      Width = 318
    end
    object btnStyleSheetNew: TcxButton [49]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&New...'
      TabOrder = 40
      Visible = False
      OnClick = btnStyleSheetNewClick
    end
    object btnStyleSheetCopy: TcxButton [50]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Copy...'
      TabOrder = 41
      Visible = False
      OnClick = btnStyleSheetCopyClick
    end
    object btnStyleSheetDelete: TcxButton [51]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Delete...'
      TabOrder = 42
      Visible = False
      OnClick = btnStyleSheetDeleteClick
    end
    object btnStyleSheetRename: TcxButton [52]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Rename...'
      TabOrder = 43
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
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Preview:'
      CaptionOptions.Layout = clTop
      Control = pnlPreview
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 300
      ControlOptions.OriginalWidth = 300
      Index = 1
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
      CaptionOptions.Visible = False
      Control = chbxShowHeaders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 61
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Visible = False
      Control = chbxShowExpandButtons
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblPrintMode: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'lblPrintMode'
      Control = cbxPrintMode
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object bvlMultipleRecords: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup3
      CaptionOptions.Text = 'Separator'
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblExpanding
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblSize
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      Control = imgExpanding
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxExpandRows
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 50
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      Control = imgGridSize
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxAutoWidth
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Visible = False
      Control = chbxBestFit
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 57
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Visible = False
      Control = chbxKeepSameRecordWidths
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 150
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Visible = False
      Control = chbxWrapRecords
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 92
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblLookAndFeel
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblRefinements
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblPagination
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      Control = cbxLookAndFeel
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignVert = avBottom
      Control = imgLookAndFeel
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahLeft
      Control = imgRefinements
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahLeft
      Control = imgPagination
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
      Control = chbxTransparentGraphics
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 127
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Visible = False
      Control = chbxDisplayGraphicsAsText
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 142
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Visible = False
      Control = chbxFlatCheckMarks
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Visible = False
      Control = chbxSuppressBackgroundBitmaps
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      CaptionOptions.Visible = False
      Control = chbxDisplayTrackBarsAsText
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 151
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxPaginateByRows
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 65
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxPaginateByWrapping
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxOneWrappingPerPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 139
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxUseNativeStyles
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = lblUseNativeStyles
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem32: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStyleFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem33: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStyleColor
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem34: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStyleBackgroundBitmap
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem35: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStyleBackgroundBitmapClear
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object bvlStylesHost: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup14
      AlignHorz = ahClient
      AlignVert = avClient
      SizeOptions.Height = 200
      SizeOptions.Width = 200
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tshView: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshView'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshBehaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 1
    end
    object tshFormatting: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFormatting'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 2
    end
    object tshStyles: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshStyles'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 3
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
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 5
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem37: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStyleRestoreDefaults
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup14: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahClient
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem36: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStylesSaveAs
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup14
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem38: TdxLayoutItem
      Parent = tshStyles
      CaptionOptions.Visible = False
      Control = lblStyleSheets
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem39: TdxLayoutItem
      Parent = tshStyles
      Control = cbxStyleSheets
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem40: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup16
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStyleSheetNew
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem41: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup16
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetCopy
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup16: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object dxLayoutItem42: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup16
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem43: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup16
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetRename
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 4
    Top = 373
    PixelsPerInch = 96
    object styleCategory: TcxStyle
      AssignedValues = [svFont, svTextColor]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      TextColor = clWindowText
    end
    object styleHeader: TcxStyle
      AssignedValues = [svFont, svTextColor]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWindowText
    end
    object styleContent: TcxStyle
      AssignedValues = [svFont, svTextColor]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      TextColor = clWindowText
    end
  end
  object pmStyles: TPopupMenu
    Images = ilStylesPopup
    OnPopup = pmStylesPopup
    Left = 32
    Top = 373
    object miStyleFont: TMenuItem
      Caption = '&Font...'
      ImageIndex = 0
      ShortCut = 16454
      OnClick = StyleFontClick
    end
    object miStyleColor: TMenuItem
      Caption = '&Color...'
      ShortCut = 16451
      OnClick = StyleColorClick
    end
    object miLine3: TMenuItem
      Caption = '-'
    end
    object miStyleBackgroundBitmap: TMenuItem
      Caption = '&Bitmap...'
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
    DesignInfo = 24444992
  end
end

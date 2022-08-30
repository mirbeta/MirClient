inherited cxfmPivotGridReportLinkDesignWindow: TcxfmPivotGridReportLinkDesignWindow
  Left = 349
  Top = 253
  Caption = 'cxfmPivotGridReportLinkDesignWindow'
  ClientHeight = 481
  ClientWidth = 657
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 657
    Height = 481
    inherited btnApply: TcxButton
      Top = 432
      TabOrder = 48
    end
    inherited btnCancel: TcxButton
      Top = 432
      TabOrder = 47
    end
    inherited btnOK: TcxButton
      Top = 432
      TabOrder = 46
    end
    inherited btnHelp: TcxButton
      Top = 432
      TabOrder = 49
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 432
      TabOrder = 50
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 432
      TabOrder = 51
    end
    inherited btnTitleProperties: TcxButton
      Top = 432
      TabOrder = 52
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 432
      TabOrder = 53
    end
    object Image1: TcxImage [8]
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
      Top = 311
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 12
      Transparent = True
      Height = 48
      Width = 48
    end
    object imgExpanding: TcxImage [10]
      Left = 10000
      Top = 10000
      Enabled = False
      Style.TransparentBorder = False
      TabOrder = 17
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
      TabOrder = 21
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
      TabOrder = 24
      Transparent = True
      Visible = False
      Height = 48
      Width = 48
    end
    object pnlPreview: TPanel [13]
      Left = 357
      Top = 29
      Width = 490
      Height = 380
      BevelOuter = bvNone
      ParentBackground = False
      TabOrder = 45
      object PreviewPivotGrid: TcxPivotGrid
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 484
        Height = 374
        Align = alClient
        Enabled = False
        Groups = <
          item
            IsCaptionAssigned = True
            Caption = 'FieldsGroup 1'
            UniqueName = ''
          end>
        LookAndFeel.SkinName = ''
        TabOrder = 0
        object pgfPurchaseQuarter: TcxPivotGridField
          Area = faColumn
          AreaIndex = 0
          IsCaptionAssigned = True
          Caption = 'Purchase Quarter'
          DataBinding.ValueType = 'DateTime'
          GroupIndex = 0
          GroupInterval = giDateQuarter
          Visible = True
          UniqueName = 'Purchase Quarter'
        end
        object pgfPurchaseMonth: TcxPivotGridField
          Area = faColumn
          AreaIndex = 1
          IsCaptionAssigned = True
          Caption = 'Purchase Month'
          DataBinding.ValueType = 'DateTime'
          GroupIndex = 0
          GroupExpanded = False
          GroupInterval = giDateMonth
          Visible = True
          UniqueName = 'Purchase Month'
        end
        object pgfPaymentType: TcxPivotGridField
          Tag = 1
          Area = faRow
          AreaIndex = 0
          IsCaptionAssigned = True
          Caption = 'Payment Type'
          DataBinding.ValueType = 'String'
          Visible = True
          Width = 90
          UniqueName = 'Payment Type'
        end
        object pgfQuantity: TcxPivotGridField
          Tag = 2
          Area = faData
          AreaIndex = 0
          IsCaptionAssigned = True
          Caption = 'Quantity'
          DataBinding.ValueType = 'Integer'
          Visible = True
          Width = 62
          UniqueName = 'Quantity'
        end
        object pgfCarName: TcxPivotGridField
          Tag = 3
          AreaIndex = 1
          IsCaptionAssigned = True
          Caption = 'Car Name'
          DataBinding.ValueType = 'String'
          Visible = True
          UniqueName = 'Car Name'
        end
        object pgfUnitPrice: TcxPivotGridField
          Tag = 4
          AreaIndex = 0
          IsCaptionAssigned = True
          Caption = 'Unit Price'
          DataBinding.ValueType = 'Currency'
          Visible = True
          UniqueName = 'Unit Price'
        end
        object pgfCompanyName: TcxPivotGridField
          Tag = 5
          Area = faRow
          AreaIndex = 1
          IsCaptionAssigned = True
          Caption = 'Company Name'
          DataBinding.ValueType = 'String'
          Visible = True
          Width = 90
          UniqueName = 'Company Name'
        end
        object pgfPaymentAmount: TcxPivotGridField
          Tag = 6
          Area = faData
          AreaIndex = 1
          IsCaptionAssigned = True
          Caption = 'Payment Amount'
          DataBinding.ValueType = 'Currency'
          Visible = True
          UniqueName = 'Payment Amount'
        end
      end
    end
    object lblShow: TcxLabel [14]
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
    object chbxColumnFields: TcxCheckBox [15]
      Left = 75
      Top = 68
      Caption = '&Column Fields'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxDataFields: TcxCheckBox [16]
      Left = 75
      Top = 91
      Caption = '&Data Fields'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxFilterFields: TcxCheckBox [17]
      Left = 75
      Top = 114
      Caption = '&Filter Fields'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxRowFields: TcxCheckBox [18]
      Left = 75
      Top = 137
      Caption = '&Row Fields'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxShowExpandButtons: TcxCheckBox [19]
      Tag = 4
      Left = 75
      Top = 160
      Caption = '&Expand Buttons'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxPrefilter: TcxCheckBox [20]
      Tag = 5
      Left = 75
      Top = 183
      Caption = '&Prefilter'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxHorizontalLines: TcxCheckBox [21]
      Tag = 1
      Left = 75
      Top = 218
      Caption = '&Horizontal Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxVerticalLines: TcxCheckBox [22]
      Tag = 2
      Left = 75
      Top = 241
      Caption = '&Vertical Lines'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object chbxBorders: TcxCheckBox [23]
      Tag = 3
      Left = 75
      Top = 264
      Caption = '&Borders'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      OnClick = chbxOptionsViewClick
      Width = 264
    end
    object lblOnEveryPage: TcxLabel [24]
      Left = 21
      Top = 287
      AutoSize = False
      Caption = 'On Every Page'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 318
    end
    object chbxColumnHeadersOnEveryPage: TcxCheckBox [25]
      Tag = 1
      Left = 75
      Top = 311
      Caption = 'Column Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      OnClick = chbxOptionsOnEveryPageClick
      Width = 102
    end
    object chbxRowHeadersOnEveryPage: TcxCheckBox [26]
      Tag = 2
      Left = 75
      Top = 334
      Caption = 'Row Headers'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Transparent = True
      OnClick = chbxOptionsOnEveryPageClick
      Width = 102
    end
    object chbxFilterBarOnEveryPage: TcxCheckBox [27]
      Tag = 3
      Left = 75
      Top = 357
      Caption = 'Filter Bar'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 15
      Transparent = True
      OnClick = chbxOptionsOnEveryPageClick
      Width = 102
    end
    object lblExpanding: TcxLabel [28]
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
    object chbxExpandColumns: TcxCheckBox [29]
      Left = 10000
      Top = 10000
      Caption = 'Columns'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
      Visible = False
      OnClick = chbxExpandOptionsClick
      Width = 264
    end
    object chbxExpandRows: TcxCheckBox [30]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Rows'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      OnClick = chbxExpandOptionsClick
      Width = 264
    end
    object lblLookAndFeel: TcxLabel [31]
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
    object lblRefinements: TcxLabel [32]
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
    object cbxLookAndFeel: TcxComboBox [33]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbxLookAndFeelChange
      Style.HotTrack = False
      TabOrder = 22
      Visible = False
      Width = 264
    end
    object chbxTransparentGraphics: TcxCheckBox [34]
      Tag = 3
      Left = 10000
      Top = 10000
      Caption = 'Transparent &Graphics'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 25
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingChanged
      Width = 264
    end
    object chbxDisplayGraphicsAsText: TcxCheckBox [35]
      Tag = 4
      Left = 10000
      Top = 10000
      Caption = 'Display Graphics As &Text'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 26
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingChanged
      Width = 264
    end
    object chbxFlatCheckMarks: TcxCheckBox [36]
      Tag = 5
      Left = 10000
      Top = 10000
      Caption = 'Flat Check &Marks'
      Properties.OnChange = OptionsFormattingChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingChanged
      Width = 264
    end
    object chbxDisplayTrackBarsAsText: TcxCheckBox [37]
      Tag = 6
      Left = 10000
      Top = 10000
      Caption = 'Display Track &Bars As Text'
      Properties.OnChange = OptionsFormattingChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 28
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingChanged
      Width = 264
    end
    object chbxSuppressBackgroundBitmaps: TcxCheckBox [38]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Suppress Background Textures'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 29
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingChanged
      Width = 264
    end
    object chbxSuppressContentColoration: TcxCheckBox [39]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Suppress Content Coloration'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 30
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingChanged
      Width = 264
    end
    object chbxUseNativeStyles: TcxCheckBox [40]
      Left = 10000
      Top = 10000
      TabStop = False
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 31
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingChanged
      Height = 21
      Width = 21
    end
    object lblUseNativeStyles: TcxLabel [41]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = '&Use Native Styles'
      FocusControl = chbxUseNativeStyles
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
    object btnStyleFont: TcxButton [42]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Font...'
      TabOrder = 35
      Visible = False
      OnClick = btnStyleFontClick
    end
    object btnStyleColor: TcxButton [43]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Co&lor...'
      TabOrder = 36
      Visible = False
      OnClick = btnStyleColorClick
    end
    object btnStyleBackgroundBitmap: TcxButton [44]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Bitmap...'
      TabOrder = 37
      Visible = False
      OnClick = btnStyleBackgroundBitmapClick
    end
    object btnStyleBackgroundBitmapClear: TcxButton [45]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Clear'
      TabOrder = 38
      Visible = False
      OnClick = btnStyleClearClick
    end
    object btnStyleRestoreDefaults: TcxButton [46]
      Left = 10000
      Top = 10000
      Width = 116
      Height = 23
      Caption = 'Restore Defaults'
      TabOrder = 33
      Visible = False
      OnClick = btnStyleRestoreDefaultsClick
    end
    object btnStylesSaveAs: TcxButton [47]
      Left = 10000
      Top = 10000
      Width = 115
      Height = 23
      Caption = 'Save &As...'
      TabOrder = 34
      Visible = False
      OnClick = btnStylesSaveAsClick
    end
    object lblStyleSheets: TcxLabel [48]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Style Sheets'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 318
      AnchorY = 10009
    end
    object cbxStyleSheets: TcxComboBox [49]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 18
      Properties.OnDrawItem = cbxStyleSheetsPropertiesDrawItem
      Style.HotTrack = False
      TabOrder = 40
      Visible = False
      OnClick = cbxStyleSheetsClick
      OnKeyDown = cbxStyleSheetsKeyDown
      Width = 318
    end
    object btnStyleSheetNew: TcxButton [50]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&New...'
      TabOrder = 41
      Visible = False
      OnClick = btnStyleSheetNewClick
    end
    object btnStyleSheetCopy: TcxButton [51]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Copy...'
      TabOrder = 42
      Visible = False
      OnClick = btnStyleSheetCopyClick
    end
    object btnStyleSheetDelete: TcxButton [52]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Delete...'
      TabOrder = 43
      Visible = False
      OnClick = btnStyleSheetDeleteClick
    end
    object btnStyleSheetRename: TcxButton [53]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Rename...'
      TabOrder = 44
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
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'lbPreview'
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
      Parent = dxLayoutAutoCreatedGroup2
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
      Index = 0
      AutoCreated = True
    end
    object tshBehaviors: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshBehaviors'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 1
    end
    object tshStyles: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshStyles'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 3
    end
    object tshFormatting: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFormatting'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 2
    end
    object tshView: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshView'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = tshView
      CaptionOptions.Visible = False
      Control = lblShow
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = tshView
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      Control = Image1
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxColumnFields
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxDataFields
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 73
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxFilterFields
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 74
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxRowFields
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 71
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxShowExpandButtons
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 96
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxPrefilter
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 58
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Text = 'Separator'
      Index = 6
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxHorizontalLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 95
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxVerticalLines
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 82
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      CaptionOptions.Visible = False
      Control = chbxBorders
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 57
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = tshView
      CaptionOptions.Visible = False
      Control = lblOnEveryPage
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahLeft
      Control = imgOnEveryPage
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Visible = False
      Control = chbxColumnHeadersOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 102
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = tshView
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem15: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Visible = False
      Control = chbxRowHeadersOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup5
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      CaptionOptions.Visible = False
      Control = chbxFilterBarOnEveryPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 67
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = tshBehaviors
      CaptionOptions.Visible = False
      Control = lblExpanding
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahLeft
      Control = imgExpanding
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      CaptionOptions.Visible = False
      Control = chbxExpandColumns
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshBehaviors
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      CaptionOptions.Visible = False
      Control = chbxExpandRows
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 50
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblLookAndFeel
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblRefinements
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahLeft
      Control = imgLookAndFeel
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      Control = cbxLookAndFeel
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahLeft
      Control = imgRefinements
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 0
    end
    object dxLayoutItem26: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxTransparentGraphics
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 127
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxDisplayGraphicsAsText
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 142
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxFlatCheckMarks
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 105
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxDisplayTrackBarsAsText
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 151
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxSuppressBackgroundBitmaps
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Visible = False
      Control = chbxSuppressContentColoration
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = dxLayoutAutoCreatedGroup11
      CaptionOptions.Text = 'Separator'
      Index = 4
    end
    object dxLayoutItem32: TdxLayoutItem
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
    object dxLayoutItem33: TdxLayoutItem
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
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem34: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem35: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleColor
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem36: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleBackgroundBitmap
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem37: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleBackgroundBitmapClear
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      ShowBorder = False
      Index = 1
    end
    object bvlStylesHost: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup14
      AlignHorz = ahClient
      AlignVert = avClient
      SizeOptions.Height = 200
      SizeOptions.Width = 200
      Index = 0
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem39: TdxLayoutItem
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
    object dxLayoutItem38: TdxLayoutItem
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
    object dxLayoutItem40: TdxLayoutItem
      Parent = tshStyles
      CaptionOptions.Visible = False
      Control = lblStyleSheets
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem41: TdxLayoutItem
      Parent = tshStyles
      Control = cbxStyleSheets
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem42: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup16
      AlignHorz = ahLeft
      CaptionOptions.Visible = False
      Control = btnStyleSheetNew
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem43: TdxLayoutItem
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
    object dxLayoutItem44: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup16
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem45: TdxLayoutItem
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
    Top = 368
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
    Top = 368
    object miStyleFont: TMenuItem
      Caption = '&Font...'
      ImageIndex = 0
      ShortCut = 16454
      OnClick = btnStyleFontClick
    end
    object miStyleColor: TMenuItem
      Caption = '&Color...'
      ShortCut = 16451
      OnClick = btnStyleColorClick
    end
    object miLine3: TMenuItem
      Caption = '-'
    end
    object miStyleBackgroundBitmap: TMenuItem
      Caption = '&Bitmap...'
      ImageIndex = 1
      OnClick = btnStyleBackgroundBitmapClick
    end
    object miStyleBackgroundBitmapClear: TMenuItem
      Caption = 'Clear'
      ImageIndex = 3
      ShortCut = 16430
      OnClick = btnStyleClearClick
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
      OnClick = btnStyleRestoreDefaultsClick
    end
    object miLine4: TMenuItem
      Caption = '-'
    end
    object miStylesSaveAs: TMenuItem
      Caption = 'Save &As...'
      ImageIndex = 2
      ShortCut = 16467
      OnClick = btnStylesSaveAsClick
    end
  end
  object ilStylesPopup: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 24117312
  end
end

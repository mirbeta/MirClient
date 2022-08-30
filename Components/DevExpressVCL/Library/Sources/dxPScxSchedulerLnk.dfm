inherited cxfmSchedulerReportLinkDesignWindow: TcxfmSchedulerReportLinkDesignWindow
  Left = 364
  Top = 183
  Caption = 'cxfmSchedulerReportLinkDesignWindow'
  ClientHeight = 545
  ClientWidth = 900
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 900
    Height = 545
    inherited btnApply: TcxButton
      Top = 499
      TabOrder = 104
    end
    inherited btnCancel: TcxButton
      Top = 499
      TabOrder = 103
    end
    inherited btnOK: TcxButton
      Top = 499
      TabOrder = 102
    end
    inherited btnHelp: TcxButton
      Top = 499
      TabOrder = 105
    end
    inherited btnRestoreOriginal: TcxButton
      Top = 499
      TabOrder = 106
    end
    inherited btnRestoreDefaults: TcxButton
      Top = 499
      TabOrder = 107
    end
    inherited btnTitleProperties: TcxButton
      Top = 499
      TabOrder = 108
    end
    inherited btnFootnoteProperties: TcxButton
      Top = 499
      TabOrder = 109
    end
    object imgLookAndFeel: TcxImage [8]
      Left = 21
      Top = 68
      Properties.PopupMenuLayout.MenuItems = []
      Properties.ShowFocusRect = False
      Style.TransparentBorder = False
      TabOrder = 81
      Transparent = True
      Height = 48
      Width = 48
    end
    object imgRefinements: TcxImage [9]
      Left = 21
      Top = 146
      Properties.PopupMenuLayout.MenuItems = []
      Properties.ShowFocusRect = False
      Style.TransparentBorder = False
      TabOrder = 84
      Transparent = True
      Height = 48
      Width = 48
    end
    object pnlPreview: TPanel [10]
      Left = 529
      Top = 29
      Width = 318
      Height = 447
      BevelOuter = bvNone
      Color = clWhite
      FullRepaint = False
      ParentBackground = False
      TabOrder = 101
      object pbPreview: TPaintBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 312
        Height = 441
        Align = alClient
        OnPaint = pbPreviewPaint
      end
      object pbxPrintStylesPreview: TPaintBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 312
        Height = 441
        Align = alClient
        OnPaint = pbxPrintStylesPreviewPaint
      end
    end
    object dePrintRangeStart: TcxDateEdit [11]
      Left = 10000
      Top = 10000
      EditValue = 38187d
      Properties.OnEditValueChanged = dePrintRangeStartEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Visible = False
      Width = 200
    end
    object dePrintRangeEnd: TcxDateEdit [12]
      Left = 10000
      Top = 10000
      EditValue = 38187d
      Properties.OnEditValueChanged = dePrintRangeEndEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Visible = False
      Width = 200
    end
    object lblPrintRangesMiscellaneous: TcxLabel [13]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Miscellaneous'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 490
    end
    object chbxHideDetailsOfPrivateAppointments: TcxCheckBox [14]
      Left = 10000
      Top = 10000
      Caption = 'Hide Details of Private Appointments'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      Visible = False
      Width = 195
    end
    object lblLookAndFeel: TcxLabel [15]
      Left = 21
      Top = 44
      AutoSize = False
      Caption = 'Look and Feel'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 490
    end
    object lblRefinements: TcxLabel [16]
      Left = 21
      Top = 122
      AutoSize = False
      Caption = 'Refinements'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 18
      Width = 490
    end
    object cbxLookAndFeel: TcxComboBox [17]
      Left = 75
      Top = 68
      AutoSize = False
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.OnChange = cbxLookAndFeelPropertiesChange
      Style.HotTrack = False
      TabOrder = 82
      OnClick = LookAndFeelChange
      Height = 23
      Width = 221
    end
    object chbxSuppressBackgroundBitmaps: TcxCheckBox [18]
      Tag = 1
      Left = 75
      Top = 146
      Caption = 'Suppress Background Textures'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 85
      Transparent = True
      OnClick = OptionsFormattingClick
      Width = 436
    end
    object chbxSuppressContentColoration: TcxCheckBox [19]
      Tag = 2
      Left = 75
      Top = 169
      Caption = 'Suppress Content Coloration'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 86
      Transparent = True
      OnClick = OptionsFormattingClick
      Width = 436
    end
    object chbxUseNativeStyles: TcxCheckBox [20]
      Left = 10000
      Top = 10000
      AutoSize = False
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 87
      Transparent = True
      Visible = False
      OnClick = OptionsFormattingClick
      Height = 21
      Width = 21
    end
    object lblUseNativeStyles: TcxLabel [21]
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
    object btnStyleFont: TcxButton [22]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Font...'
      TabOrder = 91
      Visible = False
      OnClick = btnStyleFontClick
    end
    object btnStyleColor: TcxButton [23]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Co&lor...'
      TabOrder = 92
      Visible = False
      OnClick = btnStyleColorClick
    end
    object btnStyleBackgroundBitmap: TcxButton [24]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Bitmap...'
      TabOrder = 93
      Visible = False
      OnClick = btnStyleBackgroundBitmapClick
    end
    object btnStyleBackgroundBitmapClear: TcxButton [25]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = 'Cle&ar'
      TabOrder = 94
      Visible = False
      OnClick = btnStyleBackgroundBitmapClearClick
    end
    object btnStyleRestoreDefaults: TcxButton [26]
      Left = 10000
      Top = 10000
      Width = 116
      Height = 23
      Caption = 'Rest&ore Defaults'
      TabOrder = 89
      Visible = False
      OnClick = btnStyleRestoreDefaultsClick
    end
    object btnStylesSaveAs: TcxButton [27]
      Left = 10000
      Top = 10000
      Width = 115
      Height = 23
      Caption = 'Save &As...'
      TabOrder = 90
      Visible = False
      OnClick = btnStylesSaveAsClick
    end
    object lblStyleSheets: TcxLabel [28]
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
    object cbxStyleSheets: TcxComboBox [29]
      Left = 10000
      Top = 10000
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 20
      Properties.OnDrawItem = cbxStyleSheetsPropertiesDrawItem
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 96
      Visible = False
      OnClick = cbxStyleSheetsClick
      OnKeyDown = cbxStyleSheetsKeyDown
      Width = 318
    end
    object btnStyleSheetNew: TcxButton [30]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&New...'
      TabOrder = 97
      Visible = False
      OnClick = btnStyleSheetNewClick
    end
    object btnStyleSheetCopy: TcxButton [31]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Copy...'
      TabOrder = 98
      Visible = False
      OnClick = btnStyleSheetCopyClick
    end
    object btnStyleSheetDelete: TcxButton [32]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Delete...'
      TabOrder = 99
      Visible = False
      OnClick = btnStyleSheetDeleteClick
    end
    object btnStyleSheetRename: TcxButton [33]
      Left = 10000
      Top = 10000
      Width = 75
      Height = 23
      Caption = '&Rename...'
      TabOrder = 100
      Visible = False
      OnClick = btnStyleSheetRenameClick
    end
    object cbxPrintStyles: TcxImageComboBox [34]
      Left = 10000
      Top = 10000
      AutoSize = False
      Properties.Items = <>
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 4
      Visible = False
      OnClick = cbxPrintStylesClick
      Height = 40
      Width = 490
    end
    object lbOptionsYear: TcxLabel [35]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 468
    end
    object cbxPrintStyleYearlyLayout: TcxComboBox [36]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        '1 page/month'
        '2 pages/month')
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 6
      Text = '1 page/month'
      Visible = False
      OnClick = cbxPrintStyleYearlyLayoutClick
      Width = 243
    end
    object cbxPrintStyleYearlyMonthPerPage: TcxComboBox [37]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        '1 page/year'
        '2 pages/year'
        '3 pages/year'
        '4 pages/year'
        '6 pages/year'
        '12 pages/year')
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 7
      Text = '12 pages/year'
      Visible = False
      OnClick = cbxPrintStyleYearlyMonthPagesPerYearClick
      Width = 243
    end
    object sePrintStyleYearlyResourceCountPerPage: TcxSpinEdit [38]
      Tag = 3
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.OnChange = sePrintStyleResourceCountPerPagePropertiesChanged
      Properties.OnEditValueChanged = sePrintStyleResourceCountPerPagePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 8
      Visible = False
      Width = 121
    end
    object chbxPrintStyleYearlyTaskPad: TcxCheckBox [39]
      Left = 10000
      Top = 10000
      Caption = 'Task&Pad'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 60
    end
    object chbxPrintStyleYearlyNotesAreaBlank: TcxCheckBox [40]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Blank)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object chbxPrintStyleYearlyNotesAreaLined: TcxCheckBox [41]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Lined)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object Label9: TcxLabel [42]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'View'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 468
      AnchorY = 10009
    end
    object chbxPrimaryPageHeadersOnly: TcxCheckBox [43]
      Left = 10000
      Top = 10000
      Caption = 'Primary Page Headers Only'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Transparent = True
      Visible = False
      OnClick = chbxPrimaryPageHeadersOnlyClick
      Width = 151
    end
    object Label10: TcxLabel [44]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Images '
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 468
      AnchorY = 10009
    end
    object chbxPrintStyleYearlyShowEventImages: TcxCheckBox [45]
      Left = 10000
      Top = 10000
      Caption = 'Show Event Images'
      Style.HotTrack = False
      TabOrder = 15
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowEventImagesClick
      Width = 119
    end
    object sePrintStyleTimeLineResourceCountPerPage: TcxSpinEdit [46]
      Tag = 4
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.OnChange = sePrintStyleResourceCountPerPagePropertiesChanged
      Properties.OnEditValueChanged = sePrintStyleResourceCountPerPagePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Visible = False
      Width = 121
    end
    object lbOptionsTimeLine: TcxLabel [47]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
      AnchorY = 10009
    end
    object chbxPrintStyleTimeLineTaskPad: TcxCheckBox [48]
      Left = 10000
      Top = 10000
      Caption = 'Task&Pad'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 60
    end
    object chbxPrintStyleTimeLineNotesAreaBlank: TcxCheckBox [49]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Blank)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object chbxPrintStyleTimeLineNotesAreaLined: TcxCheckBox [50]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Lined)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object tePrintStyleTimeLinePrintFrom: TcxTimeEdit [51]
      Left = 10000
      Top = 10000
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = tePrintStylePrintRangePropertiesChange
      Properties.OnEditValueChanged = tePrintStylePrintRangePropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 21
      Visible = False
      Width = 81
    end
    object tePrintStyleTimeLinePrintTo: TcxTimeEdit [52]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.ImmediatePost = True
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = tePrintStylePrintRangePropertiesChange
      Properties.OnEditValueChanged = tePrintStylePrintRangePropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 22
      Visible = False
      Width = 81
    end
    object Label2: TcxLabel [53]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'View'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
      AnchorY = 10009
    end
    object chbxPrintStyleTimeLinePrimaryPageScalesOnly: TcxCheckBox [54]
      Left = 10000
      Top = 10000
      Caption = 'Primary Page Scales Only'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
      Visible = False
      OnClick = chbxPrimaryPageScalesOnlyClick
      Width = 141
    end
    object chbxPrintStyleTimeLinePrimaryPageHeadersOnly: TcxCheckBox [55]
      Left = 10000
      Top = 10000
      Caption = 'Primary Page Headers Only'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 25
      Transparent = True
      Visible = False
      OnClick = chbxPrimaryPageHeadersOnlyClick
      Width = 151
    end
    object chbxPrintStyleTimeLineDontPrintWeekends: TcxCheckBox [56]
      Left = 10000
      Top = 10000
      Caption = 'Don'#39't Print &Weekends'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 26
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleDontPrintWeekEndsClick
      Width = 123
    end
    object chbxPrintStyleTimeLineWorkTimeOnly: TcxCheckBox [57]
      Left = 10000
      Top = 10000
      Caption = 'W&ork Time Only'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 27
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleWorkTimeOnlyClick
      Width = 95
    end
    object lbTimeLineImages: TcxLabel [58]
      Left = 10000
      Top = 10000
      AutoSize = False
      Caption = 'Images '
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
      AnchorY = 10009
    end
    object chbxPrintStyleTimeLineShowEventImages: TcxCheckBox [59]
      Left = 10000
      Top = 10000
      Caption = 'Show Event Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 29
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowEventImagesClick
      Width = 115
    end
    object chbxPrintStyleTimeLineShowResourceImages: TcxCheckBox [60]
      Left = 10000
      Top = 10000
      Caption = 'Show Resource Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 30
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowResourceImagesClick
      Width = 132
    end
    object lbOptionsMemo: TcxLabel [61]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
      AnchorY = 10009
    end
    object chbxPrintStyleMemoStartEachItemOnNewPage: TcxCheckBox [62]
      Left = 10000
      Top = 10000
      Caption = 'Start Each Item On New Page'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 32
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleMemoStartEachItemOnNewPageClick
      Width = 163
    end
    object chbxPrintStyleMemoPrintOnlySelectedEvents: TcxCheckBox [63]
      Left = 10000
      Top = 10000
      Caption = 'Print Only Selected Events'
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 33
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleMemoPrintOnlySelectedEventsClick
      Width = 147
    end
    object lbOptionsDetails: TcxLabel [64]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object chbxPrintStyleDetailsUsePagination: TcxCheckBox [65]
      Left = 10000
      Top = 10000
      Caption = 'Start a New Page Each:'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 35
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleDetailsUsePaginationClick
      Width = 134
    end
    object cbxPrintStyleDetailsPagination: TcxComboBox [66]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        'Day'
        'Week'
        'Month')
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 36
      Visible = False
      OnClick = cbxPrintStyleDetailsPaginationClick
      Width = 197
    end
    object lbOptionsTrifold: TcxLabel [67]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object cbxPrintStyleTrifoldSectionLeft: TcxComboBox [68]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 38
      Visible = False
      OnClick = cbxPrintStyleTrifoldSectionModeClick
      Width = 181
    end
    object cbxPrintStyleTrifoldSectionMiddle: TcxComboBox [69]
      Tag = 1
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 39
      Visible = False
      OnClick = cbxPrintStyleTrifoldSectionModeClick
      Width = 181
    end
    object cbxPrintStyleTrifoldSectionRight: TcxComboBox [70]
      Tag = 2
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 40
      Visible = False
      OnClick = cbxPrintStyleTrifoldSectionModeClick
      Width = 181
    end
    object lbOptionsMonthly: TcxLabel [71]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object cbxPrintStyleMonthlyLayout: TcxComboBox [72]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        '1 page/month'
        '2 pages/month')
      Style.HotTrack = False
      TabOrder = 42
      Visible = False
      OnClick = cbxPrintStyleMonthlyLayoutClick
      Width = 124
    end
    object sePrintStyleMonthlyResourceCountPerPage: TcxSpinEdit [73]
      Tag = 2
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.OnChange = sePrintStyleResourceCountPerPagePropertiesChanged
      Properties.OnEditValueChanged = sePrintStyleResourceCountPerPagePropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 43
      Visible = False
      Width = 100
    end
    object chbxPrintStyleMonthlyTaskPad: TcxCheckBox [74]
      Left = 10000
      Top = 10000
      Caption = 'Task&Pad'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 44
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 60
    end
    object chbxPrintStyleMonthlyNotesAreaBlank: TcxCheckBox [75]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Blank)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 45
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object chbxPrintStyleMonthlyNotesAreaLined: TcxCheckBox [76]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Lined)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 46
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object Label7: TcxLabel [77]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'View'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object chbxPrintStyleMonthlyDontPrintWeekends: TcxCheckBox [78]
      Left = 10000
      Top = 10000
      Caption = 'Don'#39't Print &Weekends'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 48
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleDontPrintWeekEndsClick
      Width = 123
    end
    object chbxPrintStyleMonthlyPrintExactlyOneMonthPerPage: TcxCheckBox [79]
      Left = 10000
      Top = 10000
      Caption = 'Print Exactly One Month Per Page'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 49
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleMonthlyPrintExactlyOneMonthPerPageClick
      Width = 182
    end
    object Label8: TcxLabel [80]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Images '
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object chbxPrintStyleMonthlyShowResourceImages: TcxCheckBox [81]
      Left = 10000
      Top = 10000
      Caption = 'Show Resource Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 51
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowResourceImagesClick
      Width = 132
    end
    object chbxPrintStyleMonthlyShowEventImages: TcxCheckBox [82]
      Left = 10000
      Top = 10000
      Caption = 'Show Event Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 52
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowEventImagesClick
      Width = 115
    end
    object lbOptionsWeekly: TcxLabel [83]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object cbxPrintStyleWeeklyArrange: TcxComboBox [84]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        'Top to Bottom'
        'Left to Right')
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 54
      Visible = False
      OnClick = cbxPrintStyleWeeklyArrangeClick
      Width = 127
    end
    object cbxPrintStyleWeeklyLayout: TcxComboBox [85]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        '1 page/day'
        '2 pages/day')
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 55
      Visible = False
      OnClick = cbxPrintStyleWeeklyLayoutClick
      Width = 127
    end
    object cbxPrintStyleWeeklyDaysLayout: TcxComboBox [86]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        'Two columns'
        'One column')
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 56
      Visible = False
      OnClick = cbxPrintStyleWeeklyDaysLayoutClick
      Width = 127
    end
    object sePrintStyleWeeklyResourceCountPerPage: TcxSpinEdit [87]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.OnChange = sePrintStyleResourceCountPerPagePropertiesChanged
      Properties.OnEditValueChanged = sePrintStyleResourceCountPerPagePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 57
      Visible = False
      Width = 121
    end
    object chbxPrintStyleWeeklyTaskPad: TcxCheckBox [88]
      Left = 10000
      Top = 10000
      Caption = 'TaskPad'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 58
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 60
    end
    object chbxPrintStyleWeeklyNotesAreaBlank: TcxCheckBox [89]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Blank)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 59
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object chbxPrintStyleWeeklyNotesAreaLined: TcxCheckBox [90]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Lined)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 60
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object tePrintStyleWeeklyPrintFrom: TcxTimeEdit [91]
      Left = 10000
      Top = 10000
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = tePrintStylePrintRangePropertiesChange
      Properties.OnEditValueChanged = tePrintStylePrintRangePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 61
      Visible = False
      Width = 121
    end
    object tePrintStyleWeeklyPrintTo: TcxTimeEdit [92]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = tePrintStylePrintRangePropertiesChange
      Properties.OnEditValueChanged = tePrintStylePrintRangePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 62
      Visible = False
      Width = 121
    end
    object Label5: TcxLabel [93]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'View'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object chbxPrintStyleWeeklyDontPrintWeekends: TcxCheckBox [94]
      Left = 10000
      Top = 10000
      Caption = 'Don'#39't Print &Weekends'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 64
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleWeeklyDontPrintWeekendsClick
      Width = 123
    end
    object Label6: TcxLabel [95]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Images '
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object chbxPrintStyleWeeklyShowResourceImages: TcxCheckBox [96]
      Left = 10000
      Top = 10000
      Caption = 'Show Resource Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 66
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowResourceImagesClick
      Width = 132
    end
    object chbxPrintStyleWeeklyShowEventImages: TcxCheckBox [97]
      Left = 10000
      Top = 10000
      Caption = 'Show Event Images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 67
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowEventImagesClick
      Width = 115
    end
    object lbOptionsDaily: TcxLabel [98]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Options'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object cbxPrintStyleDailyLayout: TcxComboBox [99]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.ItemHeight = 15
      Properties.Items.Strings = (
        '1 page/day'
        '2 pages/day')
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 69
      Visible = False
      OnClick = cbxPrintStyleDailyLayoutClick
      Width = 139
    end
    object sePrintStyleDailyResourceCountPerPage: TcxSpinEdit [100]
      Left = 10000
      Top = 10000
      Properties.AssignedValues.MinValue = True
      Properties.OnChange = sePrintStyleResourceCountPerPagePropertiesChanged
      Properties.OnEditValueChanged = sePrintStyleResourceCountPerPagePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 70
      Visible = False
      Width = 99
    end
    object chbxPrintStyleDailyTaskPad: TcxCheckBox [101]
      Left = 10000
      Top = 10000
      Caption = 'Task&Pad'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 71
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 60
    end
    object chbxPrintStyleDailyNotesAreaBlank: TcxCheckBox [102]
      Tag = 1
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Blank)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 72
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object chbxPrintStyleDailyNotesAreaLined: TcxCheckBox [103]
      Tag = 2
      Left = 10000
      Top = 10000
      Caption = 'Notes Area (&Lined)'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 73
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleOptionsViewClick
      Width = 110
    end
    object tePrintStyleDailyPrintFrom: TcxTimeEdit [104]
      Left = 10000
      Top = 10000
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = tePrintStylePrintRangePropertiesChange
      Properties.OnEditValueChanged = tePrintStylePrintRangePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 74
      Visible = False
      Width = 99
    end
    object tePrintStyleDailyPrintTo: TcxTimeEdit [105]
      Tag = 1
      Left = 10000
      Top = 10000
      Properties.ImmediatePost = True
      Properties.TimeFormat = tfHourMin
      Properties.OnChange = tePrintStylePrintRangePropertiesChange
      Properties.OnEditValueChanged = tePrintStylePrintRangePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 75
      Visible = False
      Width = 99
    end
    object lbViewDaily: TcxLabel [106]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'View'
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object chbxPrintStyleDailyShowResourceImages: TcxCheckBox [107]
      Left = 10000
      Top = 10000
      Caption = 'Show resource images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 77
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowResourceImagesClick
      Width = 127
    end
    object lbImagesDaily: TcxLabel [108]
      Left = 10000
      Top = 10000
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Images '
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Visible = False
      Height = 18
      Width = 357
    end
    object chbxPrintStyleDailyShowEventImages: TcxCheckBox [109]
      Left = 10000
      Top = 10000
      Caption = 'Show event images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 79
      Transparent = True
      Visible = False
      OnClick = chbxPrintStyleShowEventImagesClick
      Width = 113
    end
    inherited dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Index = 2
    end
    inherited dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Index = 1
    end
    object lblPreviewWindow: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'lblPreviewWindow'
      CaptionOptions.Layout = clTop
      SizeOptions.Height = 450
      SizeOptions.Width = 320
      Control = pnlPreview
      ControlOptions.OriginalHeight = 380
      ControlOptions.OriginalWidth = 318
      Index = 1
    end
    object pcMain: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = 'New Group'
      SizeOptions.Width = 400
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      OnTabChanged = PageControl1Change
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object tshPrintRange: TdxLayoutGroup
      Parent = pcMain
      AlignVert = avClient
      CaptionOptions.Text = 'tshPrintRange'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lblPrintRangeStart: TdxLayoutItem
      Parent = tshPrintRange
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintRangeStart'
      Offsets.Left = 27
      Control = dePrintRangeStart
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPrintRangeEnd: TdxLayoutItem
      Parent = tshPrintRange
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintRangeEnd'
      Offsets.Left = 27
      Control = dePrintRangeEnd
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 200
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liPrintRangesMiscellaneous: TdxLayoutItem
      Parent = tshPrintRange
      CaptionOptions.Visible = False
      Control = lblPrintRangesMiscellaneous
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liHideDetailsOfPrivateAppointments: TdxLayoutItem
      Parent = tshPrintRange
      CaptionOptions.Visible = False
      Offsets.Left = 27
      Control = chbxHideDetailsOfPrivateAppointments
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 199
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object tshFormatting: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshFormatting'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 2
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblLookAndFeel
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = tshFormatting
      CaptionOptions.Visible = False
      Control = lblRefinements
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      AlignHorz = ahLeft
      Control = imgLookAndFeel
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahLeft
      Control = imgRefinements
      ControlOptions.OriginalHeight = 48
      ControlOptions.OriginalWidth = 48
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup3
      Control = cbxLookAndFeel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 221
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = chbxSuppressBackgroundBitmaps
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 173
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = tshFormatting
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup5
      CaptionOptions.Visible = False
      Control = chbxSuppressContentColoration
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 162
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup5: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup4
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object tshStyles: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshStyles'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 3
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = chbxUseNativeStyles
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 21
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup6
      AlignHorz = ahClient
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Control = lblUseNativeStyles
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup6: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleFont
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleColor
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem14: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Visible = False
      Control = btnStyleBackgroundBitmap
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem15: TdxLayoutItem
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
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      ShowBorder = False
      Index = 1
    end
    object bvlStylesHost: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup8
      AlignHorz = ahClient
      AlignVert = avClient
      SizeOptions.Height = 225
      SizeOptions.Width = 225
      Index = 0
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem17: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStyleRestoreDefaults
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup8: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup7
      AlignHorz = ahClient
      Index = 0
      AutoCreated = True
    end
    object dxLayoutItem16: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStylesSaveAs
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup9: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup8
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = tshStyles
      CaptionOptions.Visible = False
      Control = lblStyleSheets
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = tshStyles
      Control = cbxStyleSheets
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetNew
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem22: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetCopy
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = tshStyles
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object dxLayoutItem21: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem20: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup10
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = btnStyleSheetRename
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object tshPrintStyles: TdxLayoutGroup
      Parent = pcMain
      CaptionOptions.Text = 'tshPrintStyles'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 1
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = tshPrintStyles
      Control = cbxPrintStyles
      ControlOptions.OriginalHeight = 40
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object pcPrintStyleOptions: TdxLayoutGroup
      Parent = tshPrintStyles
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.HideTabs = True
      TabbedOptions.ShowFrame = True
      Index = 1
    end
    object tshYearly: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 0
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Visible = False
      Control = lbOptionsYear
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPrintStyleYearlyLayout: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Text = 'lblPrintStyleYearlyLayout'
      Offsets.Left = 20
      Control = cbxPrintStyleYearlyLayout
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPrintStyleYearlyMonthPerPage: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Text = 'lblPrintStyleYearlyMonthPerPage'
      Offsets.Left = 20
      Control = cbxPrintStyleYearlyMonthPerPage
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblPrintStyleYearlyResourceCountPerPage: TdxLayoutItem
      Parent = tshYearly
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleYearlyResourceCountPerPage'
      Offsets.Left = 20
      Control = sePrintStyleYearlyResourceCountPerPage
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lblPrintStyleYearlyInclude: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Text = 'lblPrintStyleYearlyInclude'
      Offsets.Left = 20
      Control = chbxPrintStyleYearlyTaskPad
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem27: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleYearlyNotesAreaBlank
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleYearlyNotesAreaLined
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Visible = False
      Control = Label9
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem30: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrimaryPageHeadersOnly
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 155
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object dxLayoutItem31: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Visible = False
      Control = Label10
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem32: TdxLayoutItem
      Parent = tshYearly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleYearlyShowEventImages
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 119
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object tshTimeLine: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 1
    end
    object lblPrintStyleTimeLineResourceCountPerPage: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Text = 'lblPrintStyleTimeLineResourceCountPerPage'
      Offsets.Left = 20
      Control = sePrintStyleTimeLineResourceCountPerPage
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem33: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Control = lbOptionsTimeLine
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPrintStyleTimeLineInclude: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Text = 'lblPrintStyleTimeLineInclude'
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLineTaskPad
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem35: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLineNotesAreaBlank
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem36: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLineNotesAreaLined
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lblPrintStyleTimeLinePrintFrom: TdxLayoutItem
      Parent = tshTimeLine
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleTimeLinePrintFrom'
      Offsets.Left = 20
      Control = tePrintStyleTimeLinePrintFrom
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lblPrintStyleTimeLinePrintTo: TdxLayoutItem
      Parent = tshTimeLine
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleTimeLinePrintTo'
      Offsets.Left = 20
      Control = tePrintStyleTimeLinePrintTo
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 81
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem37: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Control = Label2
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem38: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLinePrimaryPageScalesOnly
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 145
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object dxLayoutItem39: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLinePrimaryPageHeadersOnly
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 155
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem40: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLineDontPrintWeekends
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 127
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object dxLayoutItem41: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLineWorkTimeOnly
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 99
      ControlOptions.ShowBorder = False
      Index = 11
    end
    object dxLayoutItem42: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Control = lbTimeLineImages
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 12
    end
    object dxLayoutItem43: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLineShowEventImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 119
      ControlOptions.ShowBorder = False
      Index = 13
    end
    object dxLayoutItem44: TdxLayoutItem
      Parent = tshTimeLine
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleTimeLineShowResourceImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Index = 14
    end
    object tshMemo: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 2
    end
    object dxLayoutItem45: TdxLayoutItem
      Parent = tshMemo
      CaptionOptions.Visible = False
      Control = lbOptionsMemo
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem46: TdxLayoutItem
      Parent = tshMemo
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleMemoStartEachItemOnNewPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 167
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem47: TdxLayoutItem
      Parent = tshMemo
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleMemoPrintOnlySelectedEvents
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 151
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object tshDetails: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      Index = 3
    end
    object dxLayoutItem48: TdxLayoutItem
      Parent = tshDetails
      CaptionOptions.Visible = False
      Control = lbOptionsDetails
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem49: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleDetailsUsePagination
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 134
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem50: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      AlignVert = avCenter
      Control = cbxPrintStyleDetailsPagination
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = tshDetails
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object tshTrifold: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 4
    end
    object dxLayoutItem51: TdxLayoutItem
      Parent = tshTrifold
      CaptionOptions.Visible = False
      Control = lbOptionsTrifold
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPrintStyleTrifoldSectionLeft: TdxLayoutItem
      Parent = tshTrifold
      CaptionOptions.Text = 'lblPrintStyleTrifoldSectionLeft'
      Offsets.Left = 20
      Control = cbxPrintStyleTrifoldSectionLeft
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPrintStyleTrifoldSectionMiddle: TdxLayoutItem
      Parent = tshTrifold
      CaptionOptions.Text = 'lblPrintStyleTrifoldSectionMiddle'
      Offsets.Left = 20
      Control = cbxPrintStyleTrifoldSectionMiddle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblPrintStyleTrifoldSectionRight: TdxLayoutItem
      Parent = tshTrifold
      CaptionOptions.Text = 'lblPrintStyleTrifoldSectionRight'
      Offsets.Left = 20
      Control = cbxPrintStyleTrifoldSectionRight
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object tshMonthly: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 3
      Index = 5
    end
    object dxLayoutItem52: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Visible = False
      Control = lbOptionsMonthly
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPrintStyleMonthlyLayout: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Text = 'lblPrintStyleMonthlyLayout'
      Offsets.Left = 20
      Control = cbxPrintStyleMonthlyLayout
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPrintStyleMonthlyResourceCountPerPage: TdxLayoutItem
      Parent = tshMonthly
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleMonthlyResourceCountPerPage'
      Offsets.Left = 20
      Control = sePrintStyleMonthlyResourceCountPerPage
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblPrintStyleMonthlyInclude: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Text = 'lblPrintStyleMonthlyInclude'
      Offsets.Left = 20
      Control = chbxPrintStyleMonthlyTaskPad
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem54: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleMonthlyNotesAreaBlank
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem55: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleMonthlyNotesAreaLined
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem56: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Visible = False
      Control = Label7
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem57: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleMonthlyDontPrintWeekends
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 127
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem58: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleMonthlyPrintExactlyOneMonthPerPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 186
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object dxLayoutItem59: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Visible = False
      Control = Label8
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem60: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleMonthlyShowResourceImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object dxLayoutItem61: TdxLayoutItem
      Parent = tshMonthly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleMonthlyShowEventImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 119
      ControlOptions.ShowBorder = False
      Index = 11
    end
    object tshWeekly: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 5
      Index = 6
    end
    object dxLayoutItem62: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Visible = False
      Control = lbOptionsWeekly
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPrintStyleWeeklyArrange: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Text = 'lblPrintStyleWeeklyArrange'
      Offsets.Left = 20
      Control = cbxPrintStyleWeeklyArrange
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPrintStyleWeeklyLayout: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Text = 'lblPrintStyleWeeklyLayout'
      Offsets.Left = 20
      Control = cbxPrintStyleWeeklyLayout
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblPrintStyleWeeklyDaysLayout: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Text = 'lblPrintStyleWeeklyDaysLayout'
      Offsets.Left = 20
      Control = cbxPrintStyleWeeklyDaysLayout
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lblPrintStyleWeeklyResourceCountPerPage: TdxLayoutItem
      Parent = tshWeekly
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleWeeklyResourceCountPerPage'
      Offsets.Left = 20
      Control = sePrintStyleWeeklyResourceCountPerPage
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lblPrintStyleWeeklyInclude: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Text = 'lblPrintStyleWeeklyInclude'
      Offsets.Left = 20
      Control = chbxPrintStyleWeeklyTaskPad
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutItem64: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleWeeklyNotesAreaBlank
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutItem65: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleWeeklyNotesAreaLined
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object lblPrintStyleWeeklyPrintFrom: TdxLayoutItem
      Parent = tshWeekly
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleWeeklyPrintFrom'
      Offsets.Left = 20
      Control = tePrintStyleWeeklyPrintFrom
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object lblPrintStyleWeeklyPrintTo: TdxLayoutItem
      Parent = tshWeekly
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleWeeklyPrintTo'
      Offsets.Left = 20
      Control = tePrintStyleWeeklyPrintTo
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem66: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Visible = False
      Control = Label5
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object dxLayoutItem67: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleWeeklyDontPrintWeekends
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 127
      ControlOptions.ShowBorder = False
      Index = 11
    end
    object dxLayoutItem68: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Visible = False
      Control = Label6
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 12
    end
    object dxLayoutItem69: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleWeeklyShowResourceImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 136
      ControlOptions.ShowBorder = False
      Index = 13
    end
    object dxLayoutItem70: TdxLayoutItem
      Parent = tshWeekly
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleWeeklyShowEventImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 119
      ControlOptions.ShowBorder = False
      Index = 14
    end
    object tshDaily: TdxLayoutGroup
      Parent = pcPrintStyleOptions
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ItemIndex = 11
      Index = 7
    end
    object dxLayoutItem71: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Visible = False
      Control = lbOptionsDaily
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPrintStyleDailyLayout: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Text = 'lblPrintStyleDailyLayout'
      Offsets.Left = 20
      Control = cbxPrintStyleDailyLayout
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPrintStyleDailyResourceCountPerPage: TdxLayoutItem
      Parent = tshDaily
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleDailyResourceCountPerPage'
      Offsets.Left = 20
      Control = sePrintStyleDailyResourceCountPerPage
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 99
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lblPrintStyleDailyInclude: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Text = 'lblPrintStyleDailyInclude'
      Offsets.Left = 20
      Control = chbxPrintStyleDailyTaskPad
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 64
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem73: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleDailyNotesAreaBlank
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem74: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Text = ' '
      Offsets.Left = 20
      Control = chbxPrintStyleDailyNotesAreaLined
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 114
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lblPrintStyleDailyPrintFrom: TdxLayoutItem
      Parent = tshDaily
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleDailyPrintFrom'
      Offsets.Left = 20
      Control = tePrintStyleDailyPrintFrom
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 99
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object lblPrintStyleDailyPrintTo: TdxLayoutItem
      Parent = tshDaily
      AlignHorz = ahLeft
      CaptionOptions.Text = 'lblPrintStyleDailyPrintTo'
      Offsets.Left = 20
      Control = tePrintStyleDailyPrintTo
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 99
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object dxLayoutItem72: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Visible = False
      Control = lbViewDaily
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object dxLayoutItem75: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleDailyShowResourceImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 131
      ControlOptions.ShowBorder = False
      Index = 9
    end
    object dxLayoutItem76: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Visible = False
      Control = lbImagesDaily
      ControlOptions.OriginalHeight = 18
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 10
    end
    object dxLayoutItem77: TdxLayoutItem
      Parent = tshDaily
      CaptionOptions.Visible = False
      Offsets.Left = 20
      Control = chbxPrintStyleDailyShowEventImages
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 11
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
  object pmStyles: TPopupMenu
    Images = ilStylesPopup
    OnPopup = pmStylesPopup
    Left = 46
    Top = 473
    object miStyleFont: TMenuItem
      Caption = '&Font...'
      ImageIndex = 0
      OnClick = btnStyleFontClick
    end
    object miStyleColor: TMenuItem
      Caption = '&Color...'
      OnClick = btnStyleColorClick
    end
    object miLine3: TMenuItem
      Caption = '-'
    end
    object miStyleBackgroundBitmap: TMenuItem
      Caption = '&Bitmap...'
      ImageIndex = 1
      ShortCut = 16463
      OnClick = btnStyleBackgroundBitmapClick
    end
    object miStyleBackgroundBitmapClear: TMenuItem
      Caption = 'Clear'
      ImageIndex = 3
      ShortCut = 16430
      OnClick = btnStyleBackgroundBitmapClearClick
    end
    object miLine2: TMenuItem
      Caption = '-'
    end
    object miStyleRestoreDefaults: TMenuItem
      Caption = 'Restore Defaults'
      OnClick = btnStyleRestoreDefaultsClick
    end
    object milLine: TMenuItem
      Caption = '-'
    end
    object miStylesSelectAll: TMenuItem
      Caption = 'Select A&ll'
      ShortCut = 16449
      OnClick = miStylesSelectAllClick
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
  object cxEditStyleController1: TcxEditStyleController
    OnStyleChanged = StyleController1StyleChanged
    Left = 80
    Top = 464
    PixelsPerInch = 96
  end
  object ilStylesPopup: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 30408712
  end
end

inherited dxPDFPrintDialog: TdxPDFPrintDialog
  Caption = 'dxPDFPrintDialog'
  ClientHeight = 650
  ClientWidth = 1000
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    Width = 1000
    Height = 650
    inherited pbxCollate: TPaintBox
      Top = 296
    end
    inherited btnPreview: TcxButton
      Top = 622
      TabOrder = 33
    end
    inherited btnPageSetup: TcxButton
      Top = 622
      TabOrder = 32
    end
    inherited btnOK: TcxButton
      Left = 739
      Top = 622
      TabOrder = 34
    end
    inherited btnCancel: TcxButton
      Left = 830
      Top = 622
      TabOrder = 35
    end
    inherited btnHelp: TcxButton
      Left = 921
      Top = 622
      TabOrder = 36
    end
    inherited lbxPrintStyles: TcxListBox
      Top = 539
      Height = 65
      TabOrder = 28
    end
    inherited btnDefineStyles: TcxButton
      Top = 568
      TabOrder = 30
    end
    inherited btnPageSetup2: TcxButton
      Top = 539
      TabOrder = 29
    end
    inherited seCopies: TcxSpinEdit
      Top = 248
      TabOrder = 17
    end
    inherited cbxNumberOfPages: TcxComboBox
      Top = 223
      TabOrder = 16
    end
    inherited chbxCollate: TcxCheckBox
      Top = 273
      TabOrder = 18
    end
    inherited rbtnAllPages: TcxRadioButton
      Top = 223
      TabOrder = 11
    end
    inherited rbtnCurrentPage: TcxRadioButton
      Top = 246
      TabOrder = 12
    end
    inherited rbtnPageRanges: TcxRadioButton
      Top = 269
      TabOrder = 14
    end
    inherited edPageRanges: TcxTextEdit
      Top = 269
      TabOrder = 15
    end
    inherited rbtnSelection: TcxRadioButton
      Top = 246
      TabOrder = 13
    end
    inherited btnPrinterProperties: TcxButton
      TabOrder = 9
    end
    inherited btnNetwork: TcxButton
      TabOrder = 10
    end
    inherited chbxPrintToFile: TcxCheckBox
      Top = 164
      Style.TransparentBorder = True
      TabOrder = 6
      Width = 76
    end
    inherited cbxFileName: TcxComboBox
      Left = 104
      Top = 164
      TabOrder = 7
      Width = 319
    end
    inherited btnBrowse: TcxButton
      Top = 164
      TabOrder = 8
    end
    object rbtnAdjustTo: TcxRadioButton [27]
      Left = 22
      Top = 386
      Width = 226
      Height = 17
      Caption = '&Adjust To:'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 19
      OnClick = rbtnAdjustToClick
      GroupIndex = 1
      ParentBackground = False
      Transparent = True
    end
    object seScaleFactor: TcxSpinEdit [28]
      Left = 42
      Top = 409
      Enabled = False
      Properties.MaxValue = 500.000000000000000000
      Properties.MinValue = 10.000000000000000000
      Properties.OnEditValueChanged = seScaleFactorPropertiesEditValueChanged
      Style.HotTrack = False
      TabOrder = 20
      Value = 10
      OnExit = seScaleFactorExit
      Width = 55
    end
    object rbtnFitToWidth: TcxRadioButton [29]
      Left = 22
      Top = 436
      Width = 226
      Height = 17
      Caption = '&Fit To Page Width'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 21
      OnClick = rbtnFitToWidthClick
      GroupIndex = 1
      ParentBackground = False
      Transparent = True
    end
    object lblCenterOnPage: TcxLabel [30]
      Left = 22
      Top = 459
      AutoSize = False
      Caption = 'Center on page '
      Style.HotTrack = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 17
      Width = 226
    end
    object cbCenterHorz: TcxCheckBox [31]
      Left = 22
      Top = 482
      Caption = 'Hori&zontaly'
      ParentColor = False
      Style.HotTrack = False
      TabOrder = 23
      Transparent = True
      OnClick = cbCenterHorzClick
      Width = 120
    end
    object cbCenterVert: TcxCheckBox [32]
      Tag = 1
      Left = 148
      Top = 482
      Caption = '&Verticaly'
      Style.HotTrack = False
      TabOrder = 24
      Transparent = True
      OnClick = cbCenterVertClick
      Width = 100
    end
    object rbtnAutoOrientation: TcxRadioButton [33]
      Left = 278
      Top = 386
      Width = 226
      Height = 17
      Caption = 'Auto portrait/landscape'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 25
      OnClick = rbtnAutoOrientationClick
      GroupIndex = 2
      ParentBackground = False
      Transparent = True
    end
    object rbtnPortrait: TcxRadioButton [34]
      Left = 278
      Top = 409
      Width = 226
      Height = 17
      Caption = 'Portrait'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 26
      OnClick = rbtnPortraitClick
      GroupIndex = 2
      ParentBackground = False
      Transparent = True
    end
    object rbtnLandscape: TcxRadioButton [35]
      Left = 278
      Top = 432
      Width = 226
      Height = 17
      Caption = 'Landscape'
      Color = clBtnFace
      ParentColor = False
      TabOrder = 27
      OnClick = rbtnLandscapeClick
      GroupIndex = 2
      ParentBackground = False
      Transparent = True
    end
    object Preview: TdxPSPreviewWindow [36]
      Left = 534
      Top = 28
      Width = 460
      Height = 576
      BorderStyle = cxcbsNone
      OnInitContent = PreviewInitContent
    end
    object cbPrintAsImage: TcxCheckBox [37]
      Left = 22
      Top = 137
      Caption = 'Print as Image'
      Style.HotTrack = False
      TabOrder = 5
      Transparent = True
      OnClick = cbPrintAsImageClick
      Width = 401
    end
    inherited lcMainGroup_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
    end
    inherited gbxPrintStyles: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup7
      Top = 475
      Index = 3
    end
    inherited dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Index = 1
    end
    inherited dxLayoutItem3: TdxLayoutItem
      ControlOptions.OriginalHeight = 65
    end
    inherited gbxCopies: TdxLayoutGroup
      Top = 10
    end
    inherited gbxPageRange: TdxLayoutGroup
      Top = 10
    end
    inherited dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup7
    end
    inherited gbxPrinter: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup7
    end
    inherited lichbxPrintToFile: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
      AlignHorz = ahLeft
      AlignVert = avTop
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 76
    end
    inherited licbxFileName: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup15
    end
    inherited dxLayoutAutoCreatedGroup10: TdxLayoutAutoCreatedGroup
      Parent = nil
      LayoutDirection = ldVertical
      Index = -1
      Special = True
    end
    object dxLayoutAutoCreatedGroup7: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup11
      Index = 0
      AutoCreated = True
    end
    object dxLayoutAutoCreatedGroup11: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup7
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object gbxPlacementAndScaling: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Placement and Scaling'
      SizeOptions.Width = 250
      ButtonOptions.Buttons = <>
      ItemIndex = 4
      Index = 0
    end
    object Orientation: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Orientation'
      SizeOptions.Width = 250
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = gbxPlacementAndScaling
      CaptionOptions.Visible = False
      Control = rbtnAdjustTo
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblPercentOfNormalSize: TdxLayoutItem
      Parent = gbxPlacementAndScaling
      AlignHorz = ahLeft
      CaptionOptions.Text = '% normal size'
      CaptionOptions.Layout = clRight
      Offsets.Left = 20
      Control = seScaleFactor
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 55
      ControlOptions.ShowBorder = False
      Enabled = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = gbxPlacementAndScaling
      CaptionOptions.Visible = False
      Control = rbtnFitToWidth
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = gbxPlacementAndScaling
      CaptionOptions.Visible = False
      Control = lblCenterOnPage
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      CaptionOptions.Visible = False
      Control = cbCenterHorz
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 78
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup12
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = cbCenterVert
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 65
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup12: TdxLayoutAutoCreatedGroup
      Parent = gbxPlacementAndScaling
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = Orientation
      CaptionOptions.Visible = False
      Control = rbtnAutoOrientation
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = Orientation
      CaptionOptions.Visible = False
      Control = rbtnPortrait
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem12: TdxLayoutItem
      Parent = Orientation
      CaptionOptions.Visible = False
      Control = rbtnLandscape
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 113
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object gbxPreview: TdxLayoutGroup
      Parent = dxLayoutAutoCreatedGroup13
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'Preview'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutAutoCreatedGroup13: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup11
      AlignHorz = ahClient
      Index = 1
      AutoCreated = True
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = gbxPreview
      AlignHorz = ahClient
      AlignVert = avClient
      Control = Preview
      ControlOptions.OriginalHeight = 100
      ControlOptions.OriginalWidth = 460
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem13: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      LayoutLookAndFeel = dxLayoutCxLookAndFeel1
      Control = cbPrintAsImage
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 91
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object dxLayoutAutoCreatedGroup15: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutAutoCreatedGroup9
      AlignHorz = ahClient
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 6
      AutoCreated = True
    end
  end
  inherited pmPrintStyles: TPopupMenu
    Left = 375
    Top = 336
  end
  inherited ilPrinters: TcxImageList
    FormatVersion = 1
    DesignInfo = -195988
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 472
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

object dxPSPDFExportDialogForm: TdxPSPDFExportDialogForm
  Left = 397
  Top = 229
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'PDF Export Options'
  ClientHeight = 361
  ClientWidth = 305
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 305
    Height = 361
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cbCompressed: TcxCheckBox
      Left = 33
      Top = 62
      Caption = 'Compressed'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      Transparent = True
      Width = 237
    end
    object cbEmbedFonts: TcxCheckBox
      Left = 33
      Top = 85
      Caption = 'Embed Fonts'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      Width = 237
    end
    object cbUseCIDFonts: TcxCheckBox
      Left = 33
      Top = 108
      Caption = 'Use CID Fonts'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      Width = 237
    end
    object cbJpgCompress: TcxCheckBox
      Left = 33
      Top = 131
      Caption = 'Use JPEG Compression for images'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 3
      Transparent = True
      OnClick = cbJpgCompressClick
      Width = 237
    end
    object tbJpgCompression: TcxTrackBar
      Left = 33
      Top = 154
      Anchors = [akLeft, akTop, akRight]
      Properties.Frequency = 5
      Properties.Max = 100
      Properties.TickMarks = cxtmBoth
      Style.HotTrack = False
      TabOrder = 4
      Transparent = True
      Height = 28
      Width = 237
    end
    object cbOpenAfterExport: TcxCheckBox
      Left = 21
      Top = 291
      Caption = 'Open after export'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 5
      Transparent = True
      Width = 108
    end
    object rbtnAllPages: TcxRadioButton
      Left = 10000
      Top = 10000
      Width = 58
      Height = 17
      Caption = '&All'
      Checked = True
      TabOrder = 6
      TabStop = True
      Visible = False
      OnClick = rbtnPageRangesClick
      Transparent = True
    end
    object rbtnCurrentPage: TcxRadioButton
      Tag = 1
      Left = 10000
      Top = 10000
      Width = 104
      Height = 17
      Caption = 'Curr&ent page'
      TabOrder = 7
      Visible = False
      OnClick = rbtnPageRangesClick
      Transparent = True
    end
    object rbtnPageRanges: TcxRadioButton
      Tag = 2
      Left = 10000
      Top = 10000
      Width = 67
      Height = 17
      Caption = 'Pa&ges: '
      TabOrder = 8
      Visible = False
      OnClick = rbtnPageRangesClick
      Transparent = True
    end
    object edPageRanges: TcxTextEdit
      Left = 10000
      Top = 10000
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 9
      Visible = False
      OnKeyPress = edPageRangesKeyPress
      Width = 177
    end
    object teTitle: TcxTextEdit
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 10
      Visible = False
      Width = 209
    end
    object teAuthor: TcxTextEdit
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 11
      Visible = False
      Width = 209
    end
    object teSubject: TcxTextEdit
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 12
      Visible = False
      Width = 209
    end
    object teKeywords: TcxTextEdit
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 13
      Visible = False
      Width = 209
    end
    object teCreator: TcxTextEdit
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 14
      Visible = False
      Width = 209
    end
    object cbSecurityEnable: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Enable'
      Style.HotTrack = False
      TabOrder = 15
      Transparent = True
      Visible = False
      OnClick = cbSecurityEnableClick
      Width = 56
    end
    object edUserPassword: TcxTextEdit
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Properties.EchoMode = eemPassword
      Properties.PasswordChar = '*'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 16
      Visible = False
      Width = 147
    end
    object edOwnerPassword: TcxTextEdit
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Properties.EchoMode = eemPassword
      Properties.PasswordChar = '*'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 17
      Visible = False
      Width = 147
    end
    object cbxMethod: TcxComboBox
      Left = 10000
      Top = 10000
      Anchors = [akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.Items.Strings = (
        'RC4: 40 Bit '
        'RC4: 128 Bit ')
      Properties.OnChange = cbxMethodPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 18
      Text = 'RC4: 40 Bit '
      Visible = False
      Width = 147
    end
    object cbAllowChanging: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Allow Changing the document'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 20
      Transparent = True
      Visible = False
      Width = 121
    end
    object cbAllowContentCopying: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Allow Content copying and extraction'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 21
      Transparent = True
      Visible = False
      Width = 121
    end
    object cbAllowComments: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Allow Comments'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 22
      Transparent = True
      Visible = False
      Width = 121
    end
    object cbAllowDocumentAssembly: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Allow Document assembly'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 23
      Transparent = True
      Visible = False
      Width = 121
    end
    object cbAllowPrintingHiResolution: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Allow Printing with high resolution'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 24
      Transparent = True
      Visible = False
      Width = 121
    end
    object btnOk: TcxButton
      Left = 63
      Top = 325
      Width = 85
      Height = 23
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 25
    end
    object btnCancel: TcxButton
      Left = 154
      Top = 325
      Width = 85
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 26
    end
    object cbAllowPrinting: TcxCheckBox
      Left = 10000
      Top = 10000
      Caption = 'Allow Printing'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 19
      Transparent = True
      Visible = False
      Width = 121
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      TabbedOptions.ShowFrame = True
      Index = 0
    end
    object tbsExport: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = '&Export'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object gbExportSettings: TdxLayoutGroup
      Parent = tbsExport
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = ' Export Settings '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutGroup6: TdxLayoutGroup
      Parent = gbExportSettings
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 5
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'cbCompressed'
      CaptionOptions.Visible = False
      Control = cbCompressed
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 83
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'cbEmbedFonts'
      CaptionOptions.Visible = False
      Control = cbEmbedFonts
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 86
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'cbUseCIDFonts'
      CaptionOptions.Visible = False
      Control = cbUseCIDFonts
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 93
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'cbJpgCompress'
      CaptionOptions.Visible = False
      Control = cbJpgCompress
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 186
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      Control = tbJpgCompression
      ControlOptions.OriginalHeight = 28
      ControlOptions.OriginalWidth = 237
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = tbsExport
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cbOpenAfterExport'
      CaptionOptions.Visible = False
      Control = cbOpenAfterExport
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 108
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object tbsPageRange: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = '&Pages'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutGroup10: TdxLayoutGroup
      Parent = tbsPageRange
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup11: TdxLayoutGroup
      Parent = dxLayoutGroup10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup11
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'rbtnAllPages'
      CaptionOptions.Visible = False
      Control = rbtnAllPages
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 58
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem9: TdxLayoutItem
      Parent = dxLayoutGroup11
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'rbtnCurrentPage'
      CaptionOptions.Visible = False
      Control = rbtnCurrentPage
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 104
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup13: TdxLayoutGroup
      Parent = dxLayoutGroup10
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem10: TdxLayoutItem
      Parent = dxLayoutGroup13
      CaptionOptions.Text = 'rbtnPageRanges'
      CaptionOptions.Visible = False
      Control = rbtnPageRanges
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 67
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem11: TdxLayoutItem
      Parent = dxLayoutGroup13
      Control = edPageRanges
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 177
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbDescription: TdxLayoutLabeledItem
      Parent = dxLayoutGroup10
      AlignHorz = ahClient
      CaptionOptions.Text = 
        'Enter page number and/or page ranges'#13#10'separated by commes. For e' +
        'xample : 1,3,5-12'
      CaptionOptions.WordWrap = True
      Index = 2
    end
    object tbsDocInfo: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = '&Document Information'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 2
    end
    object dxLayoutGroup16: TdxLayoutGroup
      Parent = tbsDocInfo
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lbTitle: TdxLayoutItem
      Parent = dxLayoutGroup16
      AlignHorz = ahClient
      CaptionOptions.Text = 'teTitle'
      Control = teTitle
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 175
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbAuthor: TdxLayoutItem
      Parent = dxLayoutGroup16
      AlignHorz = ahClient
      CaptionOptions.Text = 'teAuthor'
      Control = teAuthor
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 175
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbSubject: TdxLayoutItem
      Parent = dxLayoutGroup16
      AlignHorz = ahClient
      CaptionOptions.Text = 'teSubject'
      Control = teSubject
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 175
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lbKeywords: TdxLayoutItem
      Parent = dxLayoutGroup16
      AlignHorz = ahClient
      CaptionOptions.Text = 'Keywords'
      Control = teKeywords
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 175
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lbCreator: TdxLayoutItem
      Parent = dxLayoutGroup16
      AlignHorz = ahClient
      CaptionOptions.Text = 'teCreator'
      Control = teCreator
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 175
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object tbsSecurity: TdxLayoutGroup
      Parent = dxLayoutGroup2
      CaptionOptions.Text = '&Security'
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 3
    end
    object dxLayoutItem18: TdxLayoutItem
      Parent = tbsSecurity
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'cbSecurityEnable'
      CaptionOptions.Visible = False
      Control = cbSecurityEnable
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 56
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object gbSecuritySettings: TdxLayoutGroup
      Parent = tbsSecurity
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = ' Security Settings '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 1
    end
    object dxLayoutGroup8: TdxLayoutGroup
      Parent = gbSecuritySettings
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 6
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup17: TdxLayoutGroup
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 0
    end
    object lbUserPassword: TdxLayoutItem
      Parent = dxLayoutGroup17
      AlignHorz = ahClient
      CaptionOptions.Text = 'User Password:'
      Control = edUserPassword
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lbOwnerPassword: TdxLayoutItem
      Parent = dxLayoutGroup17
      AlignHorz = ahClient
      CaptionOptions.Text = 'Owner Password:'
      Control = edOwnerPassword
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbMethod: TdxLayoutItem
      Parent = dxLayoutGroup17
      AlignHorz = ahClient
      CaptionOptions.Text = 'cbxMethod'
      Control = cbxMethod
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 117
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem23: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'cbAllowChanging'
      CaptionOptions.Visible = False
      Control = cbAllowChanging
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 166
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem24: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'cbAllowContentCopying'
      CaptionOptions.Visible = False
      Control = cbAllowContentCopying
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 204
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem25: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'cbAllowComments'
      CaptionOptions.Visible = False
      Control = cbAllowComments
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 102
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object licbAllowDocumentAssembly: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'cbAllowDocumentAssembly'
      CaptionOptions.Visible = False
      Control = cbAllowDocumentAssembly
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 147
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object licbAllowPrintingHiResolution: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'cbAllowPrintingHiResolution'
      CaptionOptions.Visible = False
      Control = cbAllowPrintingHiResolution
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 184
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object dxLayoutGroup18: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahCenter
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem28: TdxLayoutItem
      Parent = dxLayoutGroup18
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem29: TdxLayoutItem
      Parent = dxLayoutGroup18
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem19: TdxLayoutItem
      Parent = dxLayoutGroup8
      AlignHorz = ahClient
      CaptionOptions.Text = 'Method:'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = cbAllowPrinting
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 88
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutGroup6
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 8
      Offsets.Right = 8
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 5
    end
    object lbMaxQuality: TdxLayoutLabeledItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'MaxQuality'
      Index = 0
    end
    object lbMaxCompression: TdxLayoutLabeledItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'MaxCompression'
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 264
    Top = 40
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

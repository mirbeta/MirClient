object dxPDFViewerDocumentPropertiesDialogForm: TdxPDFViewerDocumentPropertiesDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Document Properties'
  ClientHeight = 498
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 426
    Height = 498
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnOk: TcxButton
      Left = 341
      Top = 458
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object lbFile: TcxLabel
      Left = 110
      Top = 40
      Caption = 'FileName'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 47
    end
    object lbTitle: TcxLabel
      Left = 110
      Top = 63
      Caption = 'Title'
      Style.HotTrack = False
      Transparent = True
    end
    object lbAuthor: TcxLabel
      Left = 110
      Top = 86
      Caption = 'Author'
      Style.HotTrack = False
      Transparent = True
    end
    object lbSubject: TcxLabel
      Left = 110
      Top = 109
      Caption = 'Subject'
      Style.HotTrack = False
      Transparent = True
    end
    object lbKeywords: TcxLabel
      Left = 110
      Top = 132
      Caption = 'Keywords'
      Style.HotTrack = False
      Transparent = True
    end
    object lbFileSize: TcxLabel
      Left = 110
      Top = 377
      Caption = 'FileSize'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 39
    end
    object lbLocation: TcxLabel
      Left = 110
      Top = 354
      Cursor = crHandPoint
      Caption = 'FileLocation'
      Style.HotTrack = False
      Transparent = True
      OnClick = lbLocationClick
    end
    object lbPageCount: TcxLabel
      Left = 110
      Top = 400
      Caption = 'PageCount'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 57
    end
    object lbProducer: TcxLabel
      Left = 110
      Top = 308
      Caption = 'Producer'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 47
    end
    object lbVersion: TcxLabel
      Left = 110
      Top = 331
      Caption = 'Version'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 39
    end
    object lbPageSize: TcxLabel
      Left = 110
      Top = 423
      Caption = 'PageSize'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 47
    end
    object lbCreated: TcxLabel
      Left = 110
      Top = 197
      Caption = 'Created'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 43
    end
    object lbModified: TcxLabel
      Left = 110
      Top = 220
      Caption = 'Modified'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 44
    end
    object lbApplication: TcxLabel
      Left = 110
      Top = 243
      Caption = 'Application'
      Style.HotTrack = False
      Properties.WordWrap = True
      Transparent = True
      Width = 56
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahParentManaged
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 3
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lgDescription: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Description'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object liFile: TdxLayoutItem
      Parent = lgDescription
      CaptionOptions.Text = 'File:'
      Control = lbFile
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 47
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liTitle: TdxLayoutItem
      Parent = lgDescription
      CaptionOptions.Text = 'Title:'
      Control = lbTitle
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 32
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liAuthor: TdxLayoutItem
      Parent = lgDescription
      CaptionOptions.Text = 'Author:'
      Control = lbAuthor
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 24
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liSubject: TdxLayoutItem
      Parent = lgDescription
      CaptionOptions.Text = 'Subject:'
      Control = lbSubject
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 40
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liKeywords: TdxLayoutItem
      Parent = lgDescription
      CaptionOptions.Text = 'Keywords:'
      Control = lbKeywords
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 51
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object lgAdvanced: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Advanced'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 3
    end
    object liFileSize: TdxLayoutItem
      Parent = lgAdvanced
      CaptionOptions.Text = 'File Size:'
      Control = lbFileSize
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 39
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object liLocation: TdxLayoutItem
      Parent = lgAdvanced
      CaptionOptions.Text = 'Location:'
      Control = lbLocation
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object liPageCount: TdxLayoutItem
      Parent = lgAdvanced
      CaptionOptions.Text = 'Number of Pages:'
      Control = lbPageCount
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 57
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object liProducer: TdxLayoutItem
      Parent = lgAdvanced
      CaptionOptions.Text = 'Producer:'
      Control = lbProducer
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 47
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liVersion: TdxLayoutItem
      Parent = lgAdvanced
      CaptionOptions.Text = 'Version:'
      Control = lbVersion
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 39
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liPageSize: TdxLayoutItem
      Parent = lgAdvanced
      CaptionOptions.Text = 'Page Size:'
      Control = lbPageSize
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 47
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lgRevision: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Revision'
      ButtonOptions.Buttons = <>
      ItemIndex = 2
      Index = 2
    end
    object liCreated: TdxLayoutItem
      Parent = lgRevision
      CaptionOptions.Text = 'Created:'
      Control = lbCreated
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 43
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object liModified: TdxLayoutItem
      Parent = lgRevision
      CaptionOptions.Text = 'Modified:'
      Control = lbModified
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 44
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liApplication: TdxLayoutItem
      Parent = lgRevision
      CaptionOptions.Text = 'Application:'
      Control = lbApplication
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 56
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 248
    Top = 80
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end

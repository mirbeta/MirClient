object StandarddxReportLinkDesignWindow: TStandarddxReportLinkDesignWindow
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'StandarddxReportLinkDesignWindow'
  ClientHeight = 70
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 563
    Height = 70
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object btnApply: TcxButton
      Left = 692
      Top = 26
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
      OnClick = ApplyClick
    end
    object btnCancel: TcxButton
      Left = 611
      Top = 26
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnOK: TcxButton
      Left = 530
      Top = 26
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnHelp: TcxButton
      Left = 773
      Top = 26
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 3
    end
    object btnRestoreOriginal: TcxButton
      Left = 10
      Top = 26
      Width = 120
      Height = 25
      Caption = 'Restore Original'
      TabOrder = 4
      OnClick = RestoreOriginalClick
    end
    object btnRestoreDefaults: TcxButton
      Left = 136
      Top = 26
      Width = 120
      Height = 25
      Caption = 'Restore Defaults'
      TabOrder = 5
      OnClick = RestoreDefaultsClick
    end
    object btnTitleProperties: TcxButton
      Left = 262
      Top = 26
      Width = 120
      Height = 25
      Caption = 'Title Properties'
      TabOrder = 6
      OnClick = TitlePropertiesClick
    end
    object btnFootnoteProperties: TcxButton
      Left = 388
      Top = 26
      Width = 120
      Height = 25
      Caption = 'Footnotes Properties'
      TabOrder = 7
      OnClick = FootnotesPropertiesClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = -1
    end
    object libtnApply: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = btnApply
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object libtnCancel: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object libtnOk: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object libtnRestoreOriginal: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnRestoreOriginal
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object libtnRestoreDefaults: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnRestoreDefaults
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 6
    end
    object libtnTitleProperties: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      CaptionOptions.Visible = False
      Control = btnTitleProperties
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 7
    end
    object libtnFootnoteProperties: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignVert = avClient
      CaptionOptions.Visible = False
      Control = btnFootnoteProperties
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 120
      ControlOptions.ShowBorder = False
      Index = 8
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      Visible = False
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 0
    end
    object dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 0
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 584
    Top = 264
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

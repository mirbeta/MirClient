object dxSpreadSheetProtectWorkbookDialogForm: TdxSpreadSheetProtectWorkbookDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  ClientHeight = 169
  ClientWidth = 274
  Color = clBtnFace
  Constraints.MinWidth = 280
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 274
    Height = 169
    Align = alTop
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object edPassword: TcxTextEdit
      Left = 10
      Top = 89
      Properties.EchoMode = eemPassword
      Style.HotTrack = False
      TabOrder = 2
      Width = 254
    end
    object lbHeader: TcxLabel
      Left = 10
      Top = 10
      AutoSize = False
      Caption = 'lbHeader'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 16
      Width = 254
    end
    object cbProtectStructure: TcxCheckBox
      Left = 22
      Top = 32
      Caption = 'Protect'
      State = cbsChecked
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Transparent = True
      OnClick = cbProtectStructureClick
      Width = 242
    end
    object btnOK: TcxButton
      Left = 108
      Top = 116
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      TabOrder = 3
      OnClick = btnOKClick
    end
    object btnCancel: TcxButton
      Left = 189
      Top = 116
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object liPassword: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'Password:'
      CaptionOptions.Layout = clTop
      Control = edPassword
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'lbHeader'
      CaptionOptions.Visible = False
      Control = lbHeader
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 137
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cbProtected'
      CaptionOptions.Visible = False
      Offsets.Left = 12
      Control = cbProtectStructure
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 55
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 2
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 344
    Top = 104
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

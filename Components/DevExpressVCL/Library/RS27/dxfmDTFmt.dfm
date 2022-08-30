object dxfmDateTimeFormats: TdxfmDateTimeFormats
  Left = 350
  Top = 153
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Change Date and Time Formats'
  ClientHeight = 380
  ClientWidth = 300
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 300
    Height = 380
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lbxDateFormats: TcxListBox
      Left = 22
      Top = 46
      Width = 253
      Height = 150
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 0
      OnClick = lbxDateFormatsClick
      OnDblClick = lbxDTFormatsDblClick
    end
    object lbxTimeFormats: TcxListBox
      Left = 22
      Top = 220
      Width = 253
      Height = 59
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 1
      OnClick = TimeFormatsChanged
      OnDblClick = lbxDTFormatsDblClick
    end
    object chbxAutoUpdate: TcxCheckBox
      Left = 22
      Top = 288
      Caption = '&Update Automatically '
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
      OnClick = chbxAutoUpdateClick
      Width = 125
    end
    object btnDefault: TcxButton
      Left = 189
      Top = 285
      Width = 86
      Height = 23
      Caption = '&Default ...'
      TabOrder = 3
      OnClick = btnDefaultClick
    end
    object btnOK: TcxButton
      Left = 14
      Top = 326
      Width = 87
      Height = 23
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 4
    end
    object btnCancel: TcxButton
      Left = 107
      Top = 326
      Width = 87
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 5
    end
    object btnHelp: TcxButton
      Left = 200
      Top = 326
      Width = 87
      Height = 23
      Caption = '&Help'
      TabOrder = 6
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
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      ItemIndex = 1
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      ShowBorder = False
      Index = 0
    end
    object lblAvailableDateFormats: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Available &Date Formats:'
      CaptionOptions.Layout = clTop
      Control = lbxDateFormats
      ControlOptions.OriginalHeight = 150
      ControlOptions.OriginalWidth = 253
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lblAvailableTimeFormats: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Available Time &Formats:'
      CaptionOptions.Layout = clTop
      Control = lbxTimeFormats
      ControlOptions.OriginalHeight = 59
      ControlOptions.OriginalWidth = 253
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignVert = avCenter
      CaptionOptions.Text = 'chbxAutoUpdate'
      CaptionOptions.Visible = False
      Control = chbxAutoUpdate
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 125
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object libtnDefault: TdxLayoutItem
      Parent = dxLayoutGroup3
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnDefault'
      CaptionOptions.Visible = False
      Control = btnDefault
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 86
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 87
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 87
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 87
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

object dxfmPageNumberFormat: TdxfmPageNumberFormat
  Left = 434
  Top = 210
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Change Page Number Format'
  ClientHeight = 202
  ClientWidth = 321
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
    Width = 321
    Height = 202
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object rbtnContinueFromPrevSection: TcxRadioButton
      Left = 10
      Top = 60
      Width = 297
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Continue from Previous Section'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = rbtnContinueFromPrevSectionClick
      Transparent = True
    end
    object rbtnStartAt: TcxRadioButton
      Left = 10
      Top = 83
      Width = 90
      Height = 17
      Caption = 'Start &at:'
      TabOrder = 4
      OnClick = rbtnStartAtClick
      Transparent = True
    end
    object btnDefault: TcxButton
      Left = 226
      Top = 118
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Default...'
      TabOrder = 5
      OnClick = btnDefaultClick
    end
    object btnOK: TcxButton
      Left = 44
      Top = 169
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 6
    end
    object btnCancel: TcxButton
      Left = 135
      Top = 169
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 7
    end
    object btnHelp: TcxButton
      Left = 226
      Top = 169
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Caption = '&Help'
      TabOrder = 8
      OnClick = btnHelpClick
    end
    object cbxPageNumberingFormat: TcxComboBox
      Left = 106
      Top = 10
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbxPageNumberingFormatChange
      Style.HotTrack = False
      TabOrder = 1
      Width = 205
    end
    object lblPageNumbering: TcxLabel
      Left = 10
      Top = 37
      AutoSize = False
      Caption = 'Page Numbering'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 17
      Width = 301
    end
    object lblPageNumberFormat: TcxLabel
      Left = 10
      Top = 10
      AutoSize = False
      Caption = 'Number &Format:'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.Alignment.Vert = taVCenter
      Transparent = True
      Height = 21
      Width = 90
      AnchorY = 21
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Page Numbering'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = rbtnContinueFromPrevSection
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 297
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahLeft
      AlignVert = avCenter
      CaptionOptions.Text = 'rbtnStartAt'
      CaptionOptions.Visible = False
      Control = rbtnStartAt
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = lgDefault
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'btnDefault'
      CaptionOptions.Visible = False
      Control = btnDefault
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 6
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup4
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Separator'
      Index = 5
    end
    object dxLaoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignHorz = ahClient
      Control = cbxPageNumberingFormat
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 205
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lgDefault
      CaptionOptions.Text = 'Separator'
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Visible = False
      Control = lblPageNumbering
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 7
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object bvlStartAtHolder: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'New Item'
      CaptionOptions.Visible = False
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object lgDefault: TdxLayoutGroup
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 4
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup2
      AlignVert = avClient
      CaptionOptions.Text = 'lblPageNumberFormat'
      CaptionOptions.Visible = False
      Control = lblPageNumberFormat
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 90
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup
      Parent = lcMainGroup_Root
      LayoutDirection = ldHorizontal
      Index = 0
      AutoCreated = True
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    Top = 8
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

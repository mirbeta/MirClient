object dxfmAutoText: TdxfmAutoText
  Left = 324
  Top = 212
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'AutoText'
  ClientHeight = 370
  ClientWidth = 377
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
    Width = 377
    Height = 370
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object pbxPreview: TPaintBox
      Left = 22
      Top = 239
      Width = 331
      Height = 74
      Anchors = [akLeft, akBottom]
      Color = clBtnFace
      ParentColor = False
      OnPaint = pbxPreviewPaint
    end
    object cbxAutoEntries: TcxTextEdit
      Left = 22
      Top = 28
      Properties.OnChange = cbxAutoEntriesPropertiesChange
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      OnEnter = cbxAutoEntriesEnter
      OnExit = cbxAutoEntriesExit
      OnKeyDown = cbxAutoEntriesKeyDown
      Width = 240
    end
    object cbxAutoEntriesList: TcxListBox
      Left = 22
      Top = 53
      Width = 240
      Height = 128
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 1
      OnClick = cbxAutoEntriesListClick
      OnKeyDown = cbxAutoEntriesListKeyDown
    end
    object btnAdd: TcxButton
      Left = 268
      Top = 28
      Width = 85
      Height = 23
      Caption = '&Add'
      Default = True
      TabOrder = 3
      OnClick = btnAddClick
    end
    object btnDelete: TcxButton
      Left = 268
      Top = 57
      Width = 85
      Height = 23
      Caption = '&Delete'
      TabOrder = 4
      OnClick = btnDeleteClick
    end
    object btnOK: TcxButton
      Left = 98
      Top = 331
      Width = 85
      Height = 23
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object btnCancel: TcxButton
      Left = 189
      Top = 331
      Width = 85
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object btnHelp: TcxButton
      Left = 280
      Top = 331
      Width = 85
      Height = 23
      Caption = '&Help'
      TabOrder = 7
    end
    object ToolBar: TPanel
      Left = 22
      Top = 187
      Width = 240
      Height = 28
      BevelOuter = bvNone
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = -1
    end
    object gbxEnterAutoTextEntriesHere: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = ' Enter A&utoText Entries Here: '
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = gbxEnterAutoTextEntriesHere
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup3: TdxLayoutGroup
      Parent = dxLayoutGroup2
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup4
      Control = cbxAutoEntries
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 240
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup4
      Control = cbxAutoEntriesList
      ControlOptions.OriginalHeight = 128
      ControlOptions.OriginalWidth = 240
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup5: TdxLayoutGroup
      Parent = dxLayoutGroup3
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup5
      AlignHorz = ahLeft
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblPreview: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Preview:'
      CaptionOptions.Layout = clTop
      Control = pbxPreview
      ControlOptions.OriginalHeight = 74
      ControlOptions.OriginalWidth = 243
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup7: TdxLayoutGroup
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
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object libtnHelp: TdxLayoutItem
      Parent = dxLayoutGroup7
      CaptionOptions.Text = 'btnHelp'
      CaptionOptions.Visible = False
      Control = btnHelp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Visible = False
      Control = ToolBar
      ControlOptions.OriginalHeight = 28
      ControlOptions.OriginalWidth = 240
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 312
    Top = 104
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

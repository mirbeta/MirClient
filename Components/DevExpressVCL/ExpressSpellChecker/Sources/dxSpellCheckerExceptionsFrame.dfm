object frmSpellCheckerExceptions: TfrmSpellCheckerExceptions
  Left = 0
  Top = 0
  Width = 320
  Height = 169
  TabOrder = 0
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 320
    Height = 169
    Align = alClient
    TabOrder = 0
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object teCandidate: TcxTextEdit
      Left = 10
      Top = 10
      Anchors = [akLeft, akTop, akRight]
      Properties.OnChange = teCandidatePropertiesEditValueChanged
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 0
      OnKeyDown = teCandidateKeyDown
      Width = 209
    end
    object lbxList: TcxListBox
      Left = 10
      Top = 35
      Width = 209
      Height = 101
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      Style.TransparentBorder = False
      TabOrder = 1
      OnClick = lbxListClick
    end
    object cbAutoInclude: TcxCheckBox
      Left = 10
      Top = 142
      Anchors = [akLeft, akBottom]
      Caption = 'cbAutoInclude'
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 2
      Transparent = True
    end
    object btnAdd: TcxButton
      Left = 225
      Top = 10
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'btnAdd'
      TabOrder = 3
      OnClick = btnAddClick
    end
    object btnDelete: TcxButton
      Left = 225
      Top = 39
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'btnDelete'
      TabOrder = 4
      OnClick = btnDeleteClick
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      Control = teCandidate
      ControlOptions.OriginalHeight = 19
      ControlOptions.OriginalWidth = 211
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      AlignVert = avClient
      Control = lbxList
      ControlOptions.OriginalHeight = 110
      ControlOptions.OriginalWidth = 211
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahLeft
      AlignVert = avBottom
      CaptionOptions.Text = 'cbAutoInclude'
      CaptionOptions.Visible = False
      Control = cbAutoInclude
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 89
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnAdd'
      CaptionOptions.Visible = False
      Control = btnAdd
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 8
    Top = 32
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

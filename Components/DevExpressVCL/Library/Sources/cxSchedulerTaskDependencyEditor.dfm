object fmSchedulerTaskDependencyEditor: TfmSchedulerTaskDependencyEditor
  Left = 494
  Top = 254
  AutoSize = True
  BorderIcons = [biSystemMenu]
  Caption = 'fmSchedulerTaskDependencyEditor'
  ClientHeight = 116
  ClientWidth = 300
  Color = clBtnFace
  Constraints.MinHeight = 154
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object lcMain: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 300
    Height = 113
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object cbTypeRelation: TcxComboBox
      Left = 57
      Top = 50
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbTypeRelationPropertiesChange
      Style.HotTrack = False
      TabOrder = 2
      OnKeyDown = FormKeyDown
      Width = 233
    end
    object btnDelete: TcxButton
      Left = 23
      Top = 77
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'btnDelete'
      ModalResult = 1
      TabOrder = 3
      OnClick = btnDeleteClick
      OnKeyDown = FormKeyDown
    end
    object btnOk: TcxButton
      Left = 114
      Top = 77
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'btnOk'
      Default = True
      ModalResult = 1
      TabOrder = 4
      OnKeyDown = FormKeyDown
    end
    object btnCancel: TcxButton
      Left = 205
      Top = 77
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'btnCancel'
      ModalResult = 2
      TabOrder = 5
      OnKeyDown = FormKeyDown
    end
    object lbToName: TcxLabel
      Left = 57
      Top = 30
      Caption = 'lbToName'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Transparent = True
    end
    object lbFromName: TcxLabel
      Left = 57
      Top = 10
      Caption = 'cxLabel1'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Transparent = True
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = -1
    end
    object lbType: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'lbType'
      Control = cbTypeRelation
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 214
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
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnDelete'
      CaptionOptions.Visible = False
      Control = btnDelete
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnOk'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lbTo: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'cxLabel1'
      Control = lbToName
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 46
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lbFrom: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'asd'
      Control = lbFromName
      ControlOptions.OriginalHeight = 13
      ControlOptions.OriginalWidth = 42
      ControlOptions.ShowBorder = False
      Index = 0
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 208
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end

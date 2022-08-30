object fmResourcesLayoutEditor: TfmResourcesLayoutEditor
  Left = 249
  Top = 230
  BorderIcons = [biSystemMenu]
  Caption = 'Resource editor'
  ClientHeight = 252
  ClientWidth = 305
  Color = clBtnFace
  Constraints.MinHeight = 224
  Constraints.MinWidth = 220
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
    Width = 305
    Height = 252
    Align = alClient
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object clbResources: TcxCheckListBox
      Left = 10
      Top = 10
      Width = 194
      Height = 232
      Anchors = [akLeft, akTop, akRight, akBottom]
      EditValueFormat = cvfIndices
      Items = <>
      TabOrder = 0
      OnClick = clbResourcesClick
      OnDragOver = clbResourcesDragOver
      OnEditValueChanged = clbResourcesEditValueChanged
      OnKeyDown = clbResourcesKeyDown
      OnMouseDown = clbResourcesMouseDown
    end
    object btnUp: TcxButton
      Left = 210
      Top = 10
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Up'
      TabOrder = 1
      OnClick = BtnClick
    end
    object btnDown: TcxButton
      Tag = 1
      Left = 210
      Top = 39
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Down'
      TabOrder = 2
      OnClick = BtnClick
    end
    object btnSelectAll: TcxButton
      Tag = 2
      Left = 210
      Top = 68
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Select all'
      TabOrder = 3
      OnClick = BtnClick
    end
    object btnSelectNone: TcxButton
      Tag = 3
      Left = 210
      Top = 97
      Width = 85
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Select none'
      TabOrder = 4
      OnClick = BtnClick
    end
    object btnClose: TcxButton
      Left = 210
      Top = 219
      Width = 85
      Height = 23
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      ModalResult = 2
      TabOrder = 5
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 1
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = lcMainGroup_Root
      AlignHorz = ahClient
      AlignVert = avClient
      Control = clbResources
      ControlOptions.OriginalHeight = 212
      ControlOptions.OriginalWidth = 195
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 4
      ShowBorder = False
      Index = 1
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnUp'
      CaptionOptions.Visible = False
      Control = btnUp
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnDown'
      CaptionOptions.Visible = False
      Control = btnDown
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnSelectAll'
      CaptionOptions.Visible = False
      Control = btnSelectAll
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'btnSelectNone'
      CaptionOptions.Visible = False
      Control = btnSelectNone
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object dxLayoutItem6: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahRight
      AlignVert = avBottom
      CaptionOptions.Text = 'btnClose'
      CaptionOptions.Visible = False
      Control = btnClose
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 4
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 16
    Top = 16
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
    end
  end
end

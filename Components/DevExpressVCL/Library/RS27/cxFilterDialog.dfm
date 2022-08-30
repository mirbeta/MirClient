object cxFilterDialog: TcxFilterDialog
  Left = 421
  Top = 148
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  AutoSize = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Custom Filter'
  ClientHeight = 209
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PopupMode = pmAuto
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 337
    Height = 209
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel1
    object lblColumnCaption: TcxLabel
      Left = 10
      Top = 30
      AutoSize = False
      Caption = 'ColumnCaption'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 16
      Width = 308
    end
    object cbOperator1: TcxComboBox
      Left = 10
      Top = 52
      Properties.DropDownListStyle = lsFixedList
      Style.HotTrack = False
      TabOrder = 1
      OnClick = cbOperator1Click
      Width = 302
    end
    object rbAnd: TcxRadioButton
      Tag = 1
      Left = 10
      Top = 79
      Width = 60
      Height = 17
      Caption = '&And'
      Checked = True
      TabOrder = 2
      TabStop = True
      Transparent = True
    end
    object rbOr: TcxRadioButton
      Tag = 1
      Left = 76
      Top = 79
      Width = 65
      Height = 17
      Caption = '&Or'
      TabOrder = 3
      Transparent = True
    end
    object cbOperator2: TcxComboBox
      Left = 10
      Top = 102
      Properties.DropDownListStyle = lsFixedList
      Properties.OnChange = cbOperator2PropertiesChange
      Style.HotTrack = False
      TabOrder = 4
      Width = 302
    end
    object btnOK: TcxButton
      Left = 162
      Top = 169
      Width = 75
      Height = 23
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object btnCancel: TcxButton
      Left = 243
      Top = 169
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Show rows where:'
      CaptionOptions.Visible = False
      CaptionOptions.Layout = clTop
      Control = lblColumnCaption
      ControlOptions.OriginalHeight = 16
      ControlOptions.OriginalWidth = 308
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      Control = cbOperator1
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'rbAnd'
      CaptionOptions.Visible = False
      Control = rbAnd
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 60
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem4: TdxLayoutItem
      Parent = dxLayoutGroup2
      CaptionOptions.Text = 'rbOr'
      CaptionOptions.Visible = False
      Control = rbOr
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 65
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutItem5: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      Control = cbOperator2
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 100
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup4: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avTop
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 7
    end
    object dxLayoutItem7: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnOK'
      CaptionOptions.Visible = False
      Control = btnOK
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem8: TdxLayoutItem
      Parent = dxLayoutGroup4
      CaptionOptions.Text = 'btnCancel'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblSeries: TdxLayoutLabeledItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Use * to represent any series of characters'
      Index = 5
    end
    object lblSingle: TdxLayoutLabeledItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Use ? to represent any single character'
      Index = 6
    end
    object lblTitle: TdxLayoutLabeledItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahLeft
      CaptionOptions.Text = 'Show rows where:'
      Index = 0
    end
    object lblEdit1PlaceHolder: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      Index = 1
    end
    object lblEdit2PlaceHolder: TdxLayoutItem
      Parent = dxLayoutAutoCreatedGroup1
      AlignHorz = ahClient
      Index = 1
    end
    object dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      AlignVert = avTop
      LayoutDirection = ldHorizontal
      Index = 4
      AutoCreated = True
    end
  end
  object dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    Left = 128
    object dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

object dxSpreadSheetCellsModificationDialogForm: TdxSpreadSheetCellsModificationDialogForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'dxSpreadSheetCellsModificationDialogForm'
  ClientHeight = 193
  ClientWidth = 201
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
    Width = 201
    Height = 193
    TabOrder = 0
    AutoSize = True
    LayoutLookAndFeel = dxLayoutCxLookAndFeel
    object lbCaption: TcxLabel
      Left = 10
      Top = 10
      AutoSize = False
      Caption = 'lbCaption'
      Style.HotTrack = False
      Style.TransparentBorder = False
      Properties.LineOptions.Visible = True
      Transparent = True
      Height = 17
      Width = 176
    end
    object btnCancel: TcxButton
      Left = 101
      Top = 141
      Width = 85
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 6
    end
    object btnOk: TcxButton
      Left = 10
      Top = 141
      Width = 85
      Height = 25
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 5
    end
    object rbShiftHorizontally: TcxRadioButton
      Left = 10
      Top = 33
      Width = 176
      Height = 17
      Caption = 'rbShiftHorizontally'
      Checked = True
      TabOrder = 1
      TabStop = True
      Transparent = True
    end
    object rbShiftVertically: TcxRadioButton
      Left = 10
      Top = 56
      Width = 176
      Height = 17
      Caption = 'rbShiftVertically'
      TabOrder = 2
      Transparent = True
    end
    object rbShiftRow: TcxRadioButton
      Left = 10
      Top = 79
      Width = 176
      Height = 17
      Caption = 'rbShiftRow'
      TabOrder = 3
      Transparent = True
    end
    object rbShiftColumn: TcxRadioButton
      Left = 10
      Top = 102
      Width = 176
      Height = 17
      Caption = 'rbShiftColumn'
      TabOrder = 4
      Transparent = True
    end
    object lcMainGroup_Root: TdxLayoutGroup
      AlignHorz = ahLeft
      AlignVert = avTop
      ButtonOptions.Buttons = <>
      Hidden = True
      ItemIndex = 5
      ShowBorder = False
      Index = -1
    end
    object lcMainItem3: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxLabel1'
      CaptionOptions.Visible = False
      Control = lbCaption
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 176
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root
      AlignHorz = ahCenter
      AlignVert = avBottom
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 6
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup1
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 85
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbShiftHorizontally
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 176
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbShiftVertically
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 176
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbShiftRow
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 176
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbShiftColumn
      ControlOptions.AutoColor = True
      ControlOptions.OriginalHeight = 17
      ControlOptions.OriginalWidth = 176
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Empty Space Item'
      SizeOptions.Height = 10
      SizeOptions.Width = 10
      Index = 5
    end
  end
  object dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList
    Left = 152
    object dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

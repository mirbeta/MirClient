inherited dxRichEditNumberingListDialogForm: TdxRichEditNumberingListDialogForm
  Caption = 'Bullets and Numbering'
  ClientHeight = 370
  ClientWidth = 576
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 576
    Height = 370
    object lbBulleted: TdxContainerListBox [0]
      Left = 20
      Top = 44
      Width = 528
      Height = 260
    end
    object lbNumbered: TdxContainerListBox [1]
      Left = 10000
      Top = 10000
      Width = 528
      Height = 260
    end
    object lbOutlineNumbered: TdxContainerListBox [2]
      Left = 10000
      Top = 10000
      Width = 528
      Height = 260
    end
    object rbRestartNumbering: TcxRadioButton [3]
      Left = 10
      Top = 328
      Width = 127
      Height = 17
      Caption = 'Restart numbering'
      TabOrder = 3
      Transparent = True
    end
    object rbContinuePreviousList: TcxRadioButton [4]
      Left = 143
      Top = 328
      Width = 136
      Height = 17
      Caption = 'Continue previous list'
      TabOrder = 4
      Transparent = True
    end
    object btnCustomize: TcxButton [5]
      Left = 306
      Top = 324
      Width = 90
      Height = 25
      Caption = 'Customize...'
      TabOrder = 5
      OnClick = btnCustomizeClick
    end
    object btnOk: TcxButton [6]
      Left = 402
      Top = 324
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 6
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton [7]
      Left = 483
      Top = 324
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 7
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    object lcgTabControl: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldTabbed
      ShowBorder = False
      Index = 0
    end
    object lcgBulleted: TdxLayoutGroup
      Parent = lcgTabControl
      CaptionOptions.Text = '&Bulleted'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = lcgBulleted
      CaptionOptions.Text = 'cxListBox1'
      CaptionOptions.Visible = False
      Control = lbBulleted
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgNumbered: TdxLayoutGroup
      Tag = 1
      Parent = lcgTabControl
      CaptionOptions.Text = '&Numbered'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = lcgNumbered
      CaptionOptions.Text = 'cxListBox1'
      CaptionOptions.Visible = False
      Control = lbNumbered
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcgOutlineNumbered: TdxLayoutGroup
      Tag = 2
      Parent = lcgTabControl
      CaptionOptions.Text = 'O&utline Numbered'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = lcgOutlineNumbered
      CaptionOptions.Visible = False
      Control = lbOutlineNumbered
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Group2: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      CaptionOptions.Visible = False
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group3: TdxLayoutGroup
      Parent = dxLayoutControl1Group2
      AlignHorz = ahLeft
      CaptionOptions.AlignHorz = taCenter
      CaptionOptions.AlignVert = tavCenter
      CaptionOptions.Text = 'New Group'
      Offsets.Top = 4
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignVert = avCenter
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbRestartNumbering
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControl1Group3
      AlignVert = avCenter
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbContinuePreviousList
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group4: TdxLayoutGroup
      Parent = dxLayoutControl1Group2
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Offsets.Top = 4
      Control = btnCustomize
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Offsets.Top = 4
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group4
      CaptionOptions.Text = 'cxButton3'
      CaptionOptions.Visible = False
      Offsets.Top = 4
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
end

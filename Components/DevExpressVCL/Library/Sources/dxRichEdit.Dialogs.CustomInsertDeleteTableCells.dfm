inherited dxRichEditCustomInsertDeleteTableCellsDialogForm: TdxRichEditCustomInsertDeleteTableCellsDialogForm
  ClientHeight = 137
  ClientWidth = 244
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 244
    Height = 137
    object btnOk: TcxButton [0]
      Left = 78
      Top = 102
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 4
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton [1]
      Left = 159
      Top = 102
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 5
    end
    object rbCellOperationShiftLeft: TcxRadioButton [2]
      Left = 10
      Top = 10
      Width = 113
      Height = 17
      Checked = True
      TabOrder = 0
      TabStop = True
      OnDblClick = CellOperationDblClick
      GroupIndex = 1
      Transparent = True
    end
    object rbCellOperationShiftUp: TcxRadioButton [3]
      Tag = 1
      Left = 10
      Top = 33
      Width = 113
      Height = 17
      TabOrder = 1
      OnDblClick = CellOperationDblClick
      GroupIndex = 1
      Transparent = True
    end
    object rbCellOperationDeleteRow: TcxRadioButton [4]
      Tag = 2
      Left = 10
      Top = 56
      Width = 113
      Height = 17
      TabOrder = 2
      OnDblClick = CellOperationDblClick
      GroupIndex = 1
      Transparent = True
    end
    object rbCellOperationDeleteColumn: TcxRadioButton [5]
      Tag = 3
      Left = 10
      Top = 79
      Width = 113
      Height = 17
      TabOrder = 3
      OnDblClick = CellOperationDblClick
      GroupIndex = 1
      Transparent = True
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      CaptionOptions.Visible = False
      Index = -1
    end
    object lcMainGroup_Root: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item3: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton1'
      CaptionOptions.Visible = False
      Control = rbCellOperationShiftLeft
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton2'
      CaptionOptions.Visible = False
      Control = rbCellOperationShiftUp
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton3'
      CaptionOptions.Visible = False
      Control = rbCellOperationDeleteRow
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxRadioButton4'
      CaptionOptions.Visible = False
      Control = rbCellOperationDeleteColumn
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 3
    end
  end
end

inherited dxRichEditInsertTableForm: TdxRichEditInsertTableForm
  Caption = 'Insert Table'
  ClientHeight = 122
  ClientWidth = 262
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 262
    Height = 122
    object edtColumns: TcxSpinEdit [0]
      Left = 152
      Top = 30
      Properties.MaxValue = 63.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidationOptions = []
      TabOrder = 0
      Value = 1
      OnKeyPress = EditKeyPress
      Width = 100
    end
    object edtRows: TcxSpinEdit [1]
      Left = 152
      Top = 57
      Properties.MaxValue = 32767.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidationOptions = []
      TabOrder = 1
      Value = 1
      OnKeyPress = EditKeyPress
      Width = 100
    end
    object btnOk: TcxButton [2]
      Left = 96
      Top = 84
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 2
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton [3]
      Left = 177
      Top = 84
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 3
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    object liColumns: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Number of &columns:'
      Control = edtColumns
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object liRows: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Number of &rows:'
      Control = edtRows
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 2
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      LayoutDirection = ldHorizontal
      Index = 3
      AutoCreated = True
    end
    object lblTableSize: TdxLayoutSeparatorItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'Table Size'
      CaptionOptions.Visible = True
      Index = 0
    end
  end
end

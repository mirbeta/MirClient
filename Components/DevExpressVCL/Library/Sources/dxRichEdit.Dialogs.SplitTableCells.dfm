inherited dxRichEditSplitTableCellsDialogForm: TdxRichEditSplitTableCellsDialogForm
  Caption = 'Split Cells'
  ClientHeight = 126
  ClientWidth = 215
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 215
    Height = 126
    object btnOk: TcxButton [0]
      Left = 34
      Top = 91
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 3
      OnClick = btnOkClick
    end
    object btnCancel: TcxButton [1]
      Left = 115
      Top = 91
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object edtNumberOfColumns: TcxSpinEdit [2]
      Left = 110
      Top = 10
      Properties.ExceptionOnInvalidInput = True
      Properties.MaxValue = 1000.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidationOptions = [evoRaiseException, evoAllowLoseFocus]
      Properties.OnValidate = EditPropertiesValidate
      TabOrder = 0
      Value = 1
      OnKeyPress = EditKeyPress
      Width = 80
    end
    object edtNumberOfRows: TcxSpinEdit [3]
      Left = 110
      Top = 37
      Properties.ExceptionOnInvalidInput = True
      Properties.MaxValue = 1000.000000000000000000
      Properties.MinValue = 1.000000000000000000
      Properties.ValidationOptions = [evoRaiseException, evoAllowLoseFocus]
      Properties.OnValidate = EditPropertiesValidate
      TabOrder = 1
      Value = 1
      OnKeyPress = EditKeyPress
      Width = 80
    end
    object cbMergeBeforeSplit: TcxCheckBox [4]
      Left = 10
      Top = 64
      Caption = 'Merge cells before split'
      Style.HotTrack = False
      TabOrder = 2
      Transparent = True
      OnClick = cbMergeBeforeSplitClick
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
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
      Parent = dxLayoutControl1Group2
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControl1Group2
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Group2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object lciNumberOfColumns: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Number of &columns:'
      Control = edtNumberOfColumns
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciNumberOfRows: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'Number of &rows:'
      Control = edtNumberOfRows
      ControlOptions.AlignHorz = ahRight
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = lcMainGroup_Root
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbMergeBeforeSplit
      ControlOptions.ShowBorder = False
      Index = 2
    end
  end
end

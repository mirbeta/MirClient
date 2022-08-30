inherited dxRichEditCustomTableOptionsDialogForm: TdxRichEditCustomTableOptionsDialogForm
  ClientHeight = 211
  ClientWidth = 289
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 289
    Height = 211
    object edtTopMargin: TdxMeasurementUnitEdit [0]
      Left = 61
      Top = 30
      TabOrder = 0
      Width = 63
    end
    object edtLeftMargin: TdxMeasurementUnitEdit [1]
      Left = 216
      Top = 30
      TabOrder = 2
      Width = 63
    end
    object edtRightMargin: TdxMeasurementUnitEdit [2]
      Left = 216
      Top = 57
      TabOrder = 3
      Width = 63
    end
    object edtBottomMargin: TdxMeasurementUnitEdit [3]
      Left = 61
      Top = 57
      TabOrder = 1
      Width = 63
    end
    object btnOk: TcxButton [4]
      Left = 123
      Top = 176
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 4
    end
    object btnCancel: TcxButton [5]
      Left = 204
      Top = 176
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 5
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      Index = -1
    end
    object dxLayoutControlMainGroup: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignVert = avClient
      CaptionOptions.Text = 'New Group'
      ButtonOptions.Buttons = <>
      ShowBorder = False
      Index = 0
    end
    object dxLayoutControlMarginsGroup: TdxLayoutGroup
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lciTopMargin: TdxLayoutItem
      Parent = dxLayoutControlGroup2
      AlignHorz = ahClient
      CaptionOptions.Text = '&Top:'
      Control = edtTopMargin
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciLeftMargin: TdxLayoutItem
      Parent = dxLayoutControlGroup3
      AlignHorz = ahClient
      CaptionOptions.Text = '&Left:'
      Control = edtLeftMargin
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lciRightMargin: TdxLayoutItem
      Parent = dxLayoutControlGroup3
      CaptionOptions.Text = '&Right:'
      Control = edtRightMargin
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControlGroup3: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControlMarginsGroup
      AlignHorz = ahRight
      Index = 1
      AutoCreated = True
    end
    object lciBottomMargin: TdxLayoutItem
      Parent = dxLayoutControlGroup2
      CaptionOptions.Text = '&Bottom:'
      Control = edtBottomMargin
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControlGroup2: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControlMarginsGroup
      Index = 0
      AutoCreated = True
    end
    object dxLayoutControlGroup1: TdxLayoutAutoCreatedGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      LayoutDirection = ldHorizontal
      Index = 1
      AutoCreated = True
    end
    object dxLayoutControl1Item1: TdxLayoutItem
      Parent = dxLayoutControlGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnOk
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item2: TdxLayoutItem
      Parent = dxLayoutControlGroup1
      AlignHorz = ahRight
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblMargins: TdxLayoutSeparatorItem
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'lblMargins'
      CaptionOptions.Visible = True
      Index = 0
    end
  end
end

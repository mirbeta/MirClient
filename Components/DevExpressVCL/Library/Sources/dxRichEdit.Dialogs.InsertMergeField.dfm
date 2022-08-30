inherited dxRichEditInsertMergeFieldForm: TdxRichEditInsertMergeFieldForm
  AutoSize = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeable
  Caption = 'Insert Merge Field'
  ClientHeight = 315
  ClientWidth = 284
  Constraints.MinHeight = 300
  Constraints.MinWidth = 200
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 284
    Height = 315
    Align = alClient
    FocusOnClick = False
    AutoSize = False
    object btnInsert: TcxButton [0]
      Left = 108
      Top = 270
      Width = 75
      Height = 25
      Caption = '&Insert'
      Default = True
      TabOrder = 1
      OnClick = btnInsertClick
    end
    object btnCancel: TcxButton [1]
      Left = 189
      Top = 270
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Close'
      TabOrder = 2
      OnClick = btnCancelClick
    end
    object lbMergeFields: TcxListBox [2]
      Left = 10
      Top = 30
      Width = 254
      Height = 234
      ItemHeight = 13
      TabOrder = 0
      OnDblClick = lbMergeFieldsDblClick
    end
    inherited dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      CaptionOptions.Visible = False
      ItemIndex = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxButton2'
      CaptionOptions.Visible = False
      Control = btnInsert
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutGroup1
      AlignVert = avClient
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = btnCancel
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lblFields: TdxLayoutLabeledItem
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahClient
      CaptionOptions.Text = 'Fields:'
      Index = 0
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignVert = avClient
      CaptionOptions.Text = 'cxListBox1'
      CaptionOptions.Visible = False
      Control = lbMergeFields
      ControlOptions.OriginalHeight = 241
      ControlOptions.OriginalWidth = 277
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      AlignVert = avBottom
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 2
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

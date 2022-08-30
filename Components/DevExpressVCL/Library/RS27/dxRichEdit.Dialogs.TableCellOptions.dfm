inherited dxRichEditTableCellOptionsDialogForm: TdxRichEditTableCellOptionsDialogForm
  Caption = 'Cell Options'
  ClientHeight = 226
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Height = 226
    inherited edtTopMargin: TdxMeasurementUnitEdit
      Top = 57
      TabOrder = 1
    end
    inherited edtLeftMargin: TdxMeasurementUnitEdit
      Top = 57
      TabOrder = 3
    end
    inherited edtRightMargin: TdxMeasurementUnitEdit
      Top = 84
      TabOrder = 4
    end
    inherited edtBottomMargin: TdxMeasurementUnitEdit
      Top = 84
      TabOrder = 2
    end
    inherited btnOk: TcxButton
      Top = 185
      TabOrder = 7
    end
    inherited btnCancel: TcxButton
      Top = 185
      TabOrder = 8
    end
    object cbSameAsWholeTable: TcxCheckBox [6]
      Left = 18
      Top = 30
      Caption = '&Same as the whole table'
      Style.HotTrack = False
      TabOrder = 0
      Transparent = True
      Width = 209
    end
    object cbWrapText: TcxCheckBox [7]
      Left = 18
      Top = 131
      Caption = '&Wrap text'
      Style.HotTrack = False
      TabOrder = 5
      Transparent = True
      Width = 209
    end
    object cbFitText: TcxCheckBox [8]
      Left = 18
      Top = 158
      Caption = '&Fit text'
      Style.HotTrack = False
      TabOrder = 6
      Transparent = True
      Width = 209
    end
    inherited dxLayoutControlMarginsGroup: TdxLayoutGroup
      Index = 2
    end
    inherited lblMargins: TdxLayoutSeparatorItem
      CaptionOptions.Text = 'Cell margins'
    end
    object dxLayoutControl1Item4: TdxLayoutItem
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Offsets.Left = 8
      Control = cbSameAsWholeTable
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControlMainGroup
      AlignHorz = ahClient
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Offsets.Left = 8
      Control = cbWrapText
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 4
    end
    object dxLayoutControl1Item7: TdxLayoutItem
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'cxCheckBox2'
      CaptionOptions.Visible = False
      Offsets.Left = 8
      Control = cbFitText
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lblOptions: TdxLayoutSeparatorItem
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'Options'
      CaptionOptions.Visible = True
      Index = 3
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

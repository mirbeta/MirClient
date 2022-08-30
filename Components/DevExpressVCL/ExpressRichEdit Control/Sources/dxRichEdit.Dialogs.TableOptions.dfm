inherited dxRichEditTableOptionsDialogForm: TdxRichEditTableOptionsDialogForm
  Caption = 'Table Options'
  ClientHeight = 233
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Height = 233
    inherited edtLeftMargin: TdxMeasurementUnitEdit
      Left = 180
    end
    inherited edtRightMargin: TdxMeasurementUnitEdit
      Left = 180
    end
    inherited btnOk: TcxButton
      Left = 87
      Top = 178
      TabOrder = 7
    end
    inherited btnCancel: TcxButton
      Left = 168
      Top = 178
      TabOrder = 8
    end
    object cbAllowCellSpacing: TcxCheckBox [6]
      Left = 18
      Top = 104
      Caption = 'Allow &spacing between cells'
      Style.HotTrack = False
      TabOrder = 4
      Transparent = True
      Width = 156
    end
    object edtSpacingBetweenCells: TdxMeasurementUnitEdit [7]
      Left = 180
      Top = 104
      TabOrder = 5
      Width = 63
    end
    object cbResizeToFitContent: TcxCheckBox [8]
      Left = 18
      Top = 151
      Caption = 'Automatically resi&ze to fit contents'
      Style.HotTrack = False
      TabOrder = 6
      Transparent = True
      Width = 225
    end
    inherited lblMargins: TdxLayoutSeparatorItem
      CaptionOptions.Text = 'Default cell margins'
    end
    object dxLayoutControl1Group1: TdxLayoutGroup
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'New Group'
      Offsets.Left = 8
      ButtonOptions.Buttons = <>
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 3
    end
    object dxLayoutControl1Item5: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Control = cbAllowCellSpacing
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 156
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutControl1Item6: TdxLayoutItem
      Parent = dxLayoutControl1Group1
      AlignHorz = ahRight
      CaptionOptions.Text = 'dxMeasurementUnitEdit1'
      CaptionOptions.Visible = False
      Control = edtSpacingBetweenCells
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 63
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object dxLayoutControl1Item8: TdxLayoutItem
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'cxCheckBox1'
      CaptionOptions.Visible = False
      Offsets.Left = 8
      Control = cbResizeToFitContent
      ControlOptions.OriginalHeight = 21
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 5
    end
    object lblDefaultCellSpacing: TdxLayoutSeparatorItem
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'Default cell spacing'
      CaptionOptions.Visible = True
      Index = 2
    end
    object lblOptions: TdxLayoutSeparatorItem
      Parent = dxLayoutControlMainGroup
      CaptionOptions.Text = 'Options'
      CaptionOptions.Visible = True
      Index = 4
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

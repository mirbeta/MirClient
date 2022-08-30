inherited dxRichEditCustomNumberingListForm: TdxRichEditCustomNumberingListForm
  ClientHeight = 257
  ClientWidth = 383
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited dxLayoutControl1: TdxLayoutControl
    Width = 383
    Height = 257
    object btnOk: TcxButton [0]
      Left = 217
      Top = 222
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 4
    end
    object btnCancel: TcxButton [1]
      Left = 298
      Top = 222
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnFont: TcxButton [2]
      Left = 0
      Top = 0
      Width = 75
      Height = 25
      Caption = '&Font...'
      TabOrder = 0
      OnClick = btnFontClick
    end
    object edtAligned: TdxMeasurementUnitEdit [3]
      Left = 81
      Top = 2
      Properties.OnChange = edtAlignedIndentPropertiesChange
      TabOrder = 2
      Width = 71
    end
    object edtIndent: TdxMeasurementUnitEdit [4]
      Left = 158
      Top = 2
      TabOrder = 3
      Width = 71
    end
  end
  inherited dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList
    inherited dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel
      PixelsPerInch = 96
    end
  end
end

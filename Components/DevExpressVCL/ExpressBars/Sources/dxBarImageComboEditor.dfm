object dxBarImageComboEditorForm: TdxBarImageComboEditorForm
  Left = 219
  Top = 82
  BorderStyle = bsDialog
  Caption = 'ExpressBars ImageCombo Editor'
  ClientHeight = 286
  ClientWidth = 362
  Color = clBtnFace
  Position = poScreenCenter
  OnShow = FormShow
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object ButtonOk: TButton
    Left = 284
    Top = 230
    Width = 72
    Height = 22
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object ButtonCancel: TButton
    Left = 284
    Top = 258
    Width = 72
    Height = 22
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object ButtonAdd: TButton
    Left = 284
    Top = 6
    Width = 72
    Height = 22
    Caption = '&Add'
    TabOrder = 1
    OnClick = ButtonAddClick
  end
  object ButtonDelete: TButton
    Left = 284
    Top = 62
    Width = 72
    Height = 22
    Caption = '&Delete'
    TabOrder = 3
    OnClick = ButtonDeleteClick
  end
  object Grid: TStringGrid
    Left = 6
    Top = 6
    Width = 271
    Height = 273
    ColCount = 3
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goEditing, goThumbTracking]
    TabOrder = 0
    OnDrawCell = GridDrawCell
    OnGetEditText = GridGetEditText
    OnKeyDown = GridKeyDown
    OnSelectCell = GridSelectCell
    OnSetEditText = GridSetEditText
    ColWidths = (
      43
      63
      150)
  end
  object ButtonInsert: TButton
    Left = 284
    Top = 34
    Width = 72
    Height = 22
    Caption = '&Insert'
    TabOrder = 2
    OnClick = ButtonInsertClick
  end
end

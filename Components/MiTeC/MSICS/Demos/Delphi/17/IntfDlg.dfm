object dlgIntf: TdlgIntf
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Interface'
  ClientHeight = 428
  ClientWidth = 491
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    491
    428)
  PixelsPerInch = 96
  TextHeight = 13
  object bOK: TButton
    Left = 408
    Top = 395
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object List: TListView
    Left = 8
    Top = 8
    Width = 475
    Height = 381
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Property'
        Width = 150
      end
      item
        Caption = 'Value'
        Width = 300
      end>
    ColumnClick = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
  end
end

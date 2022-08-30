object IndexFieldsEditDlg: TIndexFieldsEditDlg
  Left = 250
  Top = 108
  BorderStyle = bsDialog
  Caption = 'EMF Field List Editor'
  ClientHeight = 255
  ClientWidth = 345
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SrcLabel: TLabel
    Left = 8
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'Source fields:'
  end
  object DstLabel: TLabel
    Left = 192
    Top = 8
    Width = 145
    Height = 16
    AutoSize = False
    Caption = 'Included fields:'
  end
  object IncludeBtn: TSpeedButton
    Left = 162
    Top = 24
    Width = 24
    Height = 24
    Caption = '>'
    OnClick = IncludeBtnClick
  end
  object IncAllBtn: TSpeedButton
    Left = 162
    Top = 56
    Width = 24
    Height = 24
    Caption = '>>'
    OnClick = IncAllBtnClick
  end
  object ExcludeBtn: TSpeedButton
    Left = 162
    Top = 88
    Width = 24
    Height = 24
    Caption = '<'
    Enabled = False
    OnClick = ExcludeBtnClick
  end
  object ExAllBtn: TSpeedButton
    Left = 162
    Top = 118
    Width = 24
    Height = 24
    Caption = '<<'
    Enabled = False
    OnClick = ExcAllBtnClick
  end
  object DownFldBtn: TSpeedButton
    Left = 162
    Top = 178
    Width = 24
    Height = 24
    Caption = #709
    Enabled = False
    OnClick = DownFldBtnClick
  end
  object UpFldBtn: TSpeedButton
    Left = 162
    Top = 148
    Width = 24
    Height = 24
    Caption = #708
    Enabled = False
    OnClick = UpFldBtnClick
  end
  object OKBtn: TButton
    Left = 181
    Top = 222
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object CancelBtn: TButton
    Left = 261
    Top = 222
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object SrcList: TListBox
    Left = 8
    Top = 24
    Width = 144
    Height = 185
    ItemHeight = 13
    MultiSelect = True
    Sorted = True
    TabOrder = 0
  end
  object DstList: TListBox
    Left = 193
    Top = 24
    Width = 144
    Height = 185
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
  end
end

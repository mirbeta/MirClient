object frmdxAddNewRegKey: TfrmdxAddNewRegKey
  Left = 195
  Top = 106
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Add a new registry key'
  ClientHeight = 84
  ClientWidth = 279
  Color = clBtnFace
  OldCreateOrder = True
  Position = poScreenCenter
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object lblName: TLabel
    Left = 2
    Top = 15
    Width = 37
    Height = 16
    Caption = 'Name'
  end
  object Edit: TEdit
    Left = 66
    Top = 10
    Width = 210
    Height = 24
    TabOrder = 0
    OnChange = EditChange
  end
  object bOk: TButton
    Left = 90
    Top = 49
    Width = 88
    Height = 27
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 187
    Top = 49
    Width = 89
    Height = 27
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end

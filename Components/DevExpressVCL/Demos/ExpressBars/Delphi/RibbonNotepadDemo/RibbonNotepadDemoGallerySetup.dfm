object ColorDialogSetupForm: TColorDialogSetupForm
  Left = 408
  Top = 291
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Color Dialog Setup'
  ClientHeight = 105
  ClientWidth = 209
  Color = clBtnFace
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 48
    Top = 72
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 128
    Top = 72
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object chkRemoveHorizontalItemPadding: TCheckBox
    Left = 16
    Top = 16
    Width = 185
    Height = 17
    Caption = 'Remove &Horizontal Item Padding'
    TabOrder = 2
  end
  object chkRemoveVerticalItemPadding: TCheckBox
    Left = 16
    Top = 40
    Width = 185
    Height = 17
    Caption = 'Remove &Vertical Item Padding'
    TabOrder = 3
  end
end

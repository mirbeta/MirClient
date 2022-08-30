object RzFrmPrefixSuffix: TRzFrmPrefixSuffix
  Left = 180
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Change Prefix/Suffix'
  ClientHeight = 59
  ClientWidth = 383
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LblPrefix: TRzLabel
    Left = 8
    Top = 32
    Width = 26
    Height = 13
    Caption = 'Prefix'
  end
  object OptPrefix: TRzRadioButton
    Left = 52
    Top = 8
    Width = 45
    Height = 15
    Caption = 'Prefix'
    Checked = True
    HotTrack = True
    TabOrder = 1
    TabStop = True
    OnClick = OptPrefixClick
  end
  object OptSuffix: TRzRadioButton
    Left = 128
    Top = 8
    Width = 45
    Height = 15
    Caption = 'Suffix'
    HotTrack = True
    TabOrder = 2
    OnClick = OptSuffixClick
  end
  object EdtPrefix: TRzEdit
    Left = 52
    Top = 28
    Width = 141
    Height = 21
    Text = ''
    FrameVisible = True
    TabOrder = 0
  end
  object BtnOK: TRzButton
    Left = 216
    Top = 24
    Default = True
    ModalResult = 1
    Caption = 'OK'
    Color = 15791348
    HotTrack = True
    TabOrder = 3
  end
  object BtnCancel: TRzButton
    Left = 300
    Top = 24
    Cancel = True
    ModalResult = 2
    Caption = 'Cancel'
    Color = 15791348
    HotTrack = True
    TabOrder = 4
  end
end

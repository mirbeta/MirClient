object RzCheckItemEditDlg: TRzCheckItemEditDlg
  Left = 226
  Top = 118
  BorderStyle = bsDialog
  Caption = 'Item'
  ClientHeight = 82
  ClientWidth = 427
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TRzLabel
    Left = 8
    Top = 15
    Width = 44
    Height = 13
    Caption = '&Caption'
    FocusControl = edtItem
  end
  object btnOK: TRzButton
    Left = 260
    Top = 48
    Default = True
    ModalResult = 1
    Caption = 'OK'
    Color = 15791348
    HotTrack = True
    TabOrder = 3
  end
  object btnCancel: TRzButton
    Left = 342
    Top = 48
    Cancel = True
    ModalResult = 2
    Caption = 'Cancel'
    Color = 15791348
    HotTrack = True
    TabOrder = 4
  end
  object edtItem: TRzMemo
    Left = 64
    Top = 12
    Width = 353
    Height = 21
    TabOrder = 0
    WantReturns = False
    WantTabs = True
    WordWrap = False
    FrameVisible = True
  end
  object optItem: TRzRadioButton
    Left = 64
    Top = 48
    Width = 46
    Height = 15
    Caption = '&Item'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object optGroup: TRzRadioButton
    Left = 148
    Top = 48
    Width = 54
    Height = 15
    Caption = '&Group'
    TabOrder = 2
  end
end

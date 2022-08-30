object cxMaskEditTextEditorDlg: TcxMaskEditTextEditorDlg
  Left = 327
  Top = 323
  BorderStyle = bsDialog
  Caption = 'Masked Text Editor'
  ClientHeight = 151
  ClientWidth = 466
  Color = clBtnFace
  KeyPreview = True
  OldCreateOrder = False
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  Position = poScreenCenter
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  AutoScroll = False
  object Label1: TLabel
    Left = 8
    Top = 24
    Width = 47
    Height = 13
    Caption = 'Input text:'
  end
  object Label2: TLabel
    Left = 8
    Top = 62
    Width = 49
    Height = 13
    Caption = 'Edit mask:'
  end
  object Label3: TLabel
    Left = 64
    Top = 62
    Width = 3
    Height = 13
  end
  object Bevel1: TBevel
    Left = 8
    Top = 96
    Width = 449
    Height = 2
  end
  object cxMaskEdit1: TcxMaskEdit
    Tag = 25
    Left = 64
    Top = 20
    Width = 391
    Height = 21
    Properties.IgnoreMaskBlank = True
    Properties.MaxLength = 0
    TabOrder = 0
  end
  object cxButton2: TButton
    Left = 352
    Top = 112
    Width = 105
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = cxButton2Click
  end
  object cxButton1: TButton
    Left = 240
    Top = 112
    Width = 105
    Height = 25
    Caption = 'OK'
    TabOrder = 2
    OnClick = cxButton1Click
  end
end

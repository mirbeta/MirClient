object cxCheckGroupStatesEditorDlg: TcxCheckGroupStatesEditorDlg
  Left = 260
  Top = 283
  BorderStyle = bsDialog
  Caption = 'cxCheckGroup - CheckStates editor'
  ClientHeight = 238
  ClientWidth = 436
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  object clbStates: TcxCheckListBox
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 320
    Height = 218
    Margins.Left = 10
    Margins.Top = 10
    Margins.Bottom = 10
    Align = alClient
    EditValueFormat = cvfCaptions
    Items = <>
    Style.LookAndFeel.Kind = lfUltraFlat
    Style.LookAndFeel.NativeStyle = True
    Style.TransparentBorder = False
    TabOrder = 0
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 336
    Top = 10
    Width = 90
    Height = 218
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TcxButton
      AlignWithMargins = True
      Left = 3
      Top = 0
      Width = 84
      Height = 25
      Margins.Top = 0
      Align = alTop
      Caption = 'OK'
      LookAndFeel.Kind = lfUltraFlat
      LookAndFeel.NativeStyle = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TcxButton
      AlignWithMargins = True
      Left = 3
      Top = 31
      Width = 84
      Height = 25
      Align = alTop
      Caption = 'Cancel'
      LookAndFeel.Kind = lfUltraFlat
      LookAndFeel.NativeStyle = True
      ModalResult = 2
      TabOrder = 1
    end
  end
end
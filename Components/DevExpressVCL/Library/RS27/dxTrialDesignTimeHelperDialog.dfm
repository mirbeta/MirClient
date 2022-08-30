object frmTrialDesignTimeHelperDialog: TfrmTrialDesignTimeHelperDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'DevExpress VCL Controls Trial Version'
  ClientHeight = 108
  ClientWidth = 389
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 72
    Width = 389
    Height = 36
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 389
      Height = 4
      Align = alTop
      Shape = bsTopLine
    end
    object btnOk: TButton
      Left = 155
      Top = 11
      Width = 80
      Height = 25
      Anchors = [akTop]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 389
    Height = 50
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object imWarning: TImage
      Left = 0
      Top = 0
      Width = 32
      Height = 50
      Align = alLeft
    end
    object lbDetails: TLabel
      Left = 40
      Top = 0
      Width = 349
      Height = 50
      Align = alClient
      AutoSize = False
      Transparent = True
      WordWrap = True
    end
    object Bevel2: TBevel
      Left = 32
      Top = 0
      Width = 8
      Height = 50
      Align = alLeft
      Shape = bsSpacer
    end
  end
  object cbCheckbox: TCheckBox
    Left = 40
    Top = 50
    Width = 349
    Height = 17
    Caption = 'cbCheckbox'
    TabOrder = 2
  end
end

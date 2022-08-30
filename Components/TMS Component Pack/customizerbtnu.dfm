object CusBtnForm: TCusBtnForm
  Left = 192
  Top = 107
  VertScrollBar.Visible = False
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'CusBtnForm'
  ClientHeight = 140
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object BtnOk: TButton
    Left = 49
    Top = 109
    Width = 75
    Height = 25
    Caption = '&OK'
    TabOrder = 0
    OnClick = OkBitBtnClick
  end
  object BtnCancel: TButton
    Left = 124
    Top = 109
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 0
    Width = 233
    Height = 102
    TabOrder = 2
    object LblBtnCaption: TLabel
      Left = 9
      Top = 17
      Width = 36
      Height = 13
      Caption = 'Caption'
      FocusControl = EdtBtnCaption
    end
    object LblBtnHint: TLabel
      Left = 26
      Top = 42
      Width = 19
      Height = 13
      Caption = 'Hint'
      FocusControl = EdtBtnHint
    end
    object LblGlyph: TLabel
      Left = 23
      Top = 67
      Width = 21
      Height = 13
      Caption = 'Icon'
      FocusControl = CmBxGlyph
    end
    object ChkBtnVis: TCheckBox
      Left = 129
      Top = 38
      Width = 99
      Height = 17
      Caption = 'Visible'
      TabOrder = 0
    end
    object ChkBtnShowCap: TCheckBox
      Left = 129
      Top = 17
      Width = 100
      Height = 17
      Caption = 'Show Captions'
      TabOrder = 1
    end
    object EdtBtnHint: TEdit
      Left = 50
      Top = 38
      Width = 73
      Height = 21
      TabOrder = 2
    end
    object EdtBtnCaption: TEdit
      Left = 50
      Top = 14
      Width = 73
      Height = 21
      TabOrder = 3
    end
    object CmBxGlyph: TComboBox
      Left = 50
      Top = 62
      Width = 74
      Height = 19
      Style = csOwnerDrawFixed
      ItemHeight = 13
      TabOrder = 4
      OnDrawItem = CmBxGlyphDrawItem
    end
  end
end

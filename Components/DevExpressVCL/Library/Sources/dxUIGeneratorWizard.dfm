object frmUIGenerator: TfrmUIGenerator
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 460
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TButton
    Left = 213
    Top = 424
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 294
    Top = 424
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object gbTarget: TGroupBox
    AlignWithMargins = True
    Left = 11
    Top = 11
    Width = 358
    Height = 57
    Align = alTop
    Caption = 'Target'
    TabOrder = 0
    object cbTarget: TComboBox
      AlignWithMargins = True
      Left = 10
      Top = 23
      Width = 338
      Height = 21
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Style = csDropDownList
      TabOrder = 0
    end
  end
  object gbActionList: TGroupBox
    AlignWithMargins = True
    Left = 11
    Top = 74
    Width = 358
    Height = 57
    Align = alTop
    Caption = 'Action List'
    TabOrder = 1
    object cbActionLists: TComboBox
      AlignWithMargins = True
      Left = 10
      Top = 23
      Width = 338
      Height = 21
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alTop
      Style = csDropDownList
      TabOrder = 0
    end
  end
  object gbBars: TGroupBox
    AlignWithMargins = True
    Left = 11
    Top = 137
    Width = 358
    Height = 279
    Margins.Bottom = 36
    Align = alClient
    Caption = 'Categories to Create'
    TabOrder = 2
    object clbCategories: TCheckListBox
      AlignWithMargins = True
      Left = 10
      Top = 21
      Width = 338
      Height = 248
      Margins.Left = 8
      Margins.Top = 6
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      ItemHeight = 13
      PopupMenu = pmCategories
      TabOrder = 0
    end
  end
  object pmCategories: TPopupMenu
    Left = 328
    Top = 168
    object miCheckSelected: TMenuItem
      Tag = 1
      Caption = 'Check Selected'
      OnClick = miCheckSelectedClick
    end
    object miUncheckSelected: TMenuItem
      Caption = 'Uncheck Selected'
      OnClick = miCheckSelectedClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miSelectAll: TMenuItem
      Caption = 'Select All'
      ShortCut = 16449
      OnClick = miSelectAllClick
    end
  end
end

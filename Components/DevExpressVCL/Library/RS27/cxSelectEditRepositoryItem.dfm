object cxSelectRepositoryItem: TcxSelectRepositoryItem
  Left = 403
  Top = 209
  BorderStyle = bsDialog
  Caption = 'Select EditRepositoryItem'
  ClientHeight = 392
  ClientWidth = 346
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  object Label1: TLabel
    AlignWithMargins = True
    Left = 10
    Top = 317
    Width = 326
    Height = 13
    Margins.Left = 10
    Margins.Right = 10
    Margins.Bottom = 0
    Align = alBottom
    Caption = 'Description:'
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 333
    Width = 326
    Height = 49
    Margins.Left = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alBottom
    BevelInner = bvLowered
    Color = clInfoBk
    TabOrder = 1
    object lbHint: TLabel
      Left = 4
      Top = 4
      Width = 321
      Height = 41
      AutoSize = False
      Caption = 'LongHint'
      WordWrap = True
    end
  end
  object pnlClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 326
    Height = 301
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lbItems: TListBox
      Left = 0
      Top = 0
      Width = 227
      Height = 301
      Align = alClient
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = lbItemsClick
      OnMouseDown = lbItemsMouseDown
    end
    object pnlBar: TPanel
      AlignWithMargins = True
      Left = 230
      Top = 0
      Width = 96
      Height = 298
      Margins.Top = 0
      Margins.Right = 0
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btCancel: TButton
        AlignWithMargins = True
        Left = 3
        Top = 31
        Width = 90
        Height = 25
        Align = alTop
        Cancel = True
        Caption = '&Cancel'
        ModalResult = 2
        TabOrder = 1
      end
      object btOk: TButton
        AlignWithMargins = True
        Left = 3
        Top = 0
        Width = 90
        Height = 25
        Margins.Top = 0
        Align = alTop
        Caption = '&Ok'
        Default = True
        Enabled = False
        ModalResult = 1
        TabOrder = 0
      end
    end
  end
end

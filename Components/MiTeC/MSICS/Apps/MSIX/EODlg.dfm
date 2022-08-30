object dlg_msi_EO: Tdlg_msi_EO
  Left = 702
  Top = 314
  Caption = 'System Overview Export'
  ClientHeight = 462
  ClientWidth = 555
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    555
    462)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 472
    Top = 429
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Panel: TPanel
    Left = 7
    Top = 3
    Width = 549
    Height = 420
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = ' '
    Color = clBlack
    TabOrder = 0
    object Memo: TMemo
      Left = 0
      Top = 47
      Width = 545
      Height = 369
      Align = alClient
      BorderStyle = bsNone
      Color = 1973790
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      WordWrap = False
    end
    object GroupHeader: TPanel
      Left = 0
      Top = 0
      Width = 545
      Height = 47
      Align = alTop
      BevelOuter = bvNone
      Color = 1973790
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      object Icon: TImage
        Left = 10
        Top = 6
        Width = 32
        Height = 32
        AutoSize = True
        Center = True
        Transparent = True
      end
      object lMachine: TLabel
        Left = 51
        Top = 15
        Width = 53
        Height = 13
        Caption = 'Overview'
        Color = 13882323
        Font.Charset = EASTEUROPE_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
        Transparent = True
        Layout = tlCenter
      end
    end
  end
end

object ExpTreeviewEditor: TExpTreeviewEditor
  Left = 355
  Top = 148
  Caption = 'ExplorerTreeviewEditor'
  ClientHeight = 406
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 112
    Width = 345
    Height = 257
    Caption = 'Items'
    TabOrder = 0
    object Treeview1: TTreeView
      Left = 8
      Top = 16
      Width = 241
      Height = 233
      Indent = 19
      TabOrder = 0
      OnChange = Treeview1Change
    end
    object Btn_NewItem: TButton
      Left = 256
      Top = 24
      Width = 75
      Height = 25
      Caption = 'NewItem'
      TabOrder = 1
      OnClick = Btn_NewItemClick
    end
    object Btn_NewSubItem: TButton
      Left = 256
      Top = 56
      Width = 75
      Height = 25
      Caption = 'NewSubItem'
      TabOrder = 2
      OnClick = Btn_NewSubItemClick
    end
    object Btn_Delete: TButton
      Left = 256
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Delete'
      TabOrder = 3
      OnClick = Btn_DeleteClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 97
    Caption = 'ItemProperties'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 27
      Width = 21
      Height = 13
      Caption = 'Text'
    end
    object Label2: TLabel
      Left = 8
      Top = 59
      Width = 55
      Height = 13
      Caption = 'ImageIndex'
    end
    object Image1: TImage
      Left = 184
      Top = 53
      Width = 57
      Height = 38
      Transparent = True
    end
    object Ed_Text: TEdit
      Left = 75
      Top = 19
      Width = 257
      Height = 21
      TabOrder = 0
      OnChange = Ed_TextChange
    end
    object Sp_ImageIndex: TSpinEdit
      Left = 75
      Top = 51
      Width = 102
      Height = 22
      MaxValue = -1
      MinValue = -1
      TabOrder = 1
      Value = -1
      OnChange = Sp_ImageIndexChange
    end
  end
  object Btn_Ok: TButton
    Left = 192
    Top = 375
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 2
    OnClick = Btn_OkClick
  end
  object Btn_Cancel: TButton
    Left = 272
    Top = 375
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end

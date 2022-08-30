object AddFields: TAddFields
  Left = 293
  Top = 103
  ActiveControl = FieldsList
  Caption = 'Add Fields'
  ClientHeight = 253
  ClientWidth = 248
  Color = clBtnFace
  Constraints.MinHeight = 279
  Constraints.MinWidth = 256
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    248
    253)
  PixelsPerInch = 96
  TextHeight = 13
  object OkBtn: TButton
    Left = 6
    Top = 219
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 86
    Top = 219
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 232
    Height = 197
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Available fields'
    TabOrder = 0
    DesignSize = (
      232
      197)
    object FieldsList: TListBox
      Left = 8
      Top = 16
      Width = 216
      Height = 173
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 0
    end
  end
  object HelpBtn: TButton
    Left = 166
    Top = 219
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end

object DefineField: TDefineField
  Left = 221
  Top = 137
  BorderStyle = bsDialog
  Caption = 'New Field'
  ClientHeight = 286
  ClientWidth = 502
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LookupGroup: TGroupBox
    Left = 8
    Top = 166
    Width = 488
    Height = 81
    Caption = 'Lookup definition'
    TabOrder = 2
    object DatasetLabel: TLabel
      Left = 248
      Top = 19
      Width = 42
      Height = 13
      Caption = 'D&ataset:'
      Enabled = False
      FocusControl = DatasetList
    end
    object KeyFieldsLabel: TLabel
      Left = 8
      Top = 19
      Width = 52
      Height = 13
      Caption = '&Key Fields:'
      Enabled = False
      FocusControl = KeyFieldsList
    end
    object LookupKeysLabel: TLabel
      Left = 8
      Top = 51
      Width = 64
      Height = 13
      Caption = 'Look&up Keys:'
      Enabled = False
      FocusControl = LookupKeysList
    end
    object ResultFieldLabel: TLabel
      Left = 248
      Top = 51
      Width = 59
      Height = 13
      Caption = '&Result Field:'
      Enabled = False
      FocusControl = ResultFieldList
    end
    object DatasetList: TComboBox
      Left = 342
      Top = 16
      Width = 136
      Height = 21
      Enabled = False
      TabOrder = 1
      OnChange = DatasetListChange
      OnDropDown = DatasetListDropDown
    end
    object KeyFieldsList: TComboBox
      Left = 104
      Top = 16
      Width = 136
      Height = 21
      Enabled = False
      TabOrder = 0
      OnDropDown = KeyFieldsListDropDown
    end
    object LookupKeysList: TComboBox
      Left = 104
      Top = 48
      Width = 136
      Height = 21
      Enabled = False
      TabOrder = 2
      OnDropDown = LookupKeysListDropDown
    end
    object ResultFieldList: TComboBox
      Left = 342
      Top = 48
      Width = 136
      Height = 21
      Enabled = False
      TabOrder = 3
      OnDropDown = ResultFieldListDropDown
    end
  end
  object OkBtn: TButton
    Left = 257
    Top = 253
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 338
    Top = 253
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object HelpBtn: TButton
    Left = 419
    Top = 253
    Width = 75
    Height = 25
    Caption = 'Help'
    TabOrder = 5
    OnClick = HelpBtnClick
  end
  object FieldGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 488
    Height = 81
    Caption = 'Field properties'
    TabOrder = 0
    object ComponentNameLabel: TLabel
      Left = 248
      Top = 19
      Width = 59
      Height = 13
      Caption = 'C&omponent:'
      FocusControl = ComponentNameEdit
    end
    object FieldNameLabel: TLabel
      Left = 8
      Top = 19
      Width = 31
      Height = 13
      Caption = '&Name:'
      FocusControl = FieldNameEdit
    end
    object FieldTypeLabel: TLabel
      Left = 8
      Top = 51
      Width = 28
      Height = 13
      Caption = '&Type:'
      FocusControl = FieldTypeList
    end
    object SizeEditLabel: TLabel
      Left = 248
      Top = 51
      Width = 23
      Height = 13
      Caption = '&Size:'
      FocusControl = SizeEdit
    end
    object ComponentNameEdit: TEdit
      Left = 342
      Top = 16
      Width = 136
      Height = 21
      TabOrder = 1
    end
    object FieldNameEdit: TEdit
      Left = 104
      Top = 16
      Width = 136
      Height = 21
      TabOrder = 0
      OnChange = FieldNameEditChange
      OnClick = FieldNameEditChange
    end
    object FieldTypeList: TComboBox
      Left = 104
      Top = 48
      Width = 136
      Height = 21
      TabOrder = 2
      OnChange = FieldTypeListChange
    end
    object SizeEdit: TEdit
      Left = 342
      Top = 48
      Width = 57
      Height = 21
      MaxLength = 5
      TabOrder = 3
    end
  end
  object FieldKind: TRadioGroup
    Left = 8
    Top = 92
    Width = 488
    Height = 68
    Caption = 'Field type'
    Columns = 3
    ItemIndex = 0
    Items.Strings = (
      '&Data'
      '&Calculated'
      '&Lookup')
    TabOrder = 1
    OnClick = FieldKindClick
  end
end

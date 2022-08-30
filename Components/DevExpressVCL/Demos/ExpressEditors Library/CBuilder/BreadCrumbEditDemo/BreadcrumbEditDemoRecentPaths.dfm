object dxBreadcrumbEditDemoRecentPathsForm: TdxBreadcrumbEditDemoRecentPathsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Recent Paths Editor'
  ClientHeight = 351
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bvSeparator: TdxBevel
    Left = 8
    Top = 304
    Width = 401
    Height = 8
  end
  object lvPaths: TcxListView
    Left = 8
    Top = 8
    Width = 401
    Height = 209
    Columns = <
      item
        Caption = 'Path'
        Width = 300
      end>
    SmallImages = dxBreadcrumbEditDemoForm.cxImageList1
    TabOrder = 0
    ViewStyle = vsReport
    OnSelectItem = lvPathsSelectItem
  end
  object btnCancel: TcxButton
    Left = 334
    Top = 318
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOk: TcxButton
    Left = 253
    Top = 318
    Width = 75
    Height = 25
    Caption = '&Ok'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object tePath: TcxTextEdit
    Left = 58
    Top = 246
    Properties.OnChange = tePathPropertiesChange
    TabOrder = 3
    Width = 351
  end
  object cbImage: TcxImageComboBox
    Left = 8
    Top = 246
    Properties.Images = dxBreadcrumbEditDemoForm.cxImageList1
    Properties.Items = <>
    TabOrder = 4
    Width = 44
  end
  object lbRecentPath: TcxLabel
    Left = 8
    Top = 223
    AutoSize = False
    Caption = 'Recent Path'
    Properties.LineOptions.Visible = True
    Transparent = True
    Height = 18
    Width = 401
  end
  object btnReplace: TcxButton
    Left = 334
    Top = 273
    Width = 75
    Height = 25
    Caption = '&Replace'
    Enabled = False
    TabOrder = 6
    OnClick = btnReplaceClick
  end
  object btnDelete: TcxButton
    Left = 253
    Top = 273
    Width = 75
    Height = 25
    Caption = '&Delete'
    Enabled = False
    TabOrder = 7
    OnClick = btnDeleteClick
  end
  object btnAdd: TcxButton
    Left = 172
    Top = 273
    Width = 75
    Height = 25
    Caption = '&Add'
    TabOrder = 8
    OnClick = btnAddClick
  end
end

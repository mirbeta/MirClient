object SummaryFooterDemoEditSummaryForm: TSummaryFooterDemoEditSummaryForm
  Left = 428
  Top = 215
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Insert/Delete Summaries'
  ClientHeight = 380
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 335
    Width = 265
    Height = 4
    Shape = bsTopLine
  end
  object gbSummaries: TcxGroupBox
    Left = 8
    Top = 8
    Caption = 'Summaries'
    ParentBackground = False
    ParentColor = False
    TabOrder = 0
    Height = 241
    Width = 265
    object lbSummaries: TcxListBox
      Left = 13
      Top = 21
      Width = 239
      Height = 205
      ItemHeight = 13
      Style.Color = 16247513
      TabOrder = 0
    end
  end
  object btnAdd: TcxButton
    Left = 8
    Top = 347
    Width = 83
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = btnAddClick
  end
  object btnDelete: TcxButton
    Left = 101
    Top = 347
    Width = 83
    Height = 25
    Caption = 'Delete'
    TabOrder = 2
    OnClick = btnDeleteClick
  end
  object btnExit: TcxButton
    Left = 190
    Top = 347
    Width = 83
    Height = 25
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 3
  end
  object cbCalculatedColumn: TcxComboBox
    Left = 139
    Top = 281
    Properties.DropDownListStyle = lsFixedList
    Properties.OnChange = cbCalculatedColumnPropertiesChange
    Style.Color = 16247513
    TabOrder = 4
    Width = 121
  end
  object cbFooterSummaryColumn: TcxComboBox
    Left = 139
    Top = 257
    Properties.DropDownListStyle = lsFixedList
    Properties.Revertable = True
    Properties.OnChange = cbFooterSummaryColumnPropertiesChange
    Style.Color = 16247513
    TabOrder = 5
    Width = 121
  end
  object cbSummaryKind: TcxImageComboBox
    Left = 139
    Top = 305
    Properties.DefaultDescription = 'None'
    Properties.Items = <>
    Style.Color = 16247513
    TabOrder = 6
    Width = 121
  end
  object Label1: TcxLabel
    Left = 18
    Top = 283
    Caption = '&Calculated column:'
    FocusControl = cbCalculatedColumn
    Transparent = True
  end
  object Label2: TcxLabel
    Left = 18
    Top = 259
    Caption = '&Footer summary column:'
    FocusControl = cbFooterSummaryColumn
    Transparent = True
  end
  object Label3: TcxLabel
    Left = 18
    Top = 306
    Caption = 'Summary &kind:'
    FocusControl = cbSummaryKind
    Transparent = True
  end
end

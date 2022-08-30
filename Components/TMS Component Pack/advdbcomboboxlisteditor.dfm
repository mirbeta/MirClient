object ComboListEditor: TComboListEditor
  Left = 0
  Top = 0
  Caption = 'AdvDBComboBox list editor'
  ClientHeight = 391
  ClientWidth = 563
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 350
    Width = 563
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      563
      41)
    object btn_Add: TButton
      Left = 8
      Top = 10
      Width = 65
      Height = 25
      Caption = 'Add Row'
      TabOrder = 0
      OnClick = btn_AddClick
    end
    object btn_Del: TButton
      Left = 74
      Top = 10
      Width = 65
      Height = 25
      Caption = 'Delete Row'
      TabOrder = 1
      OnClick = btn_DelClick
    end
    object bt_ClearList: TButton
      Left = 139
      Top = 10
      Width = 65
      Height = 25
      Caption = 'Clear'
      TabOrder = 2
      OnClick = bt_ClearListClick
    end
    object btn_GetStoredVal: TButton
      Left = 204
      Top = 10
      Width = 95
      Height = 25
      Hint = 'Retrieve values from data field'
      Caption = 'Get stored values'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = btn_GetStoredValClick
    end
    object btn_Ok: TButton
      Left = 413
      Top = 10
      Width = 70
      Height = 25
      Anchors = [akTop, akRight, akBottom]
      Caption = '&Ok'
      Default = True
      ModalResult = 1
      TabOrder = 4
      OnClick = btn_OkClick
    end
    object btn_Cancel: TButton
      Left = 483
      Top = 10
      Width = 70
      Height = 25
      Anchors = [akTop, akRight, akBottom]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 5
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 563
    Height = 350
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      563
      350)
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 200
      Height = 13
      Caption = 'List of displayed values and stored values'
    end
    object lbl_Images: TLabel
      Left = 448
      Top = 8
      Width = 79
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Available images'
    end
    object sg_ComboList: TStringGrid
      Left = 8
      Top = 24
      Width = 434
      Height = 315
      Anchors = [akLeft, akTop, akRight, akBottom]
      ColCount = 2
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goEditing]
      TabOrder = 0
      OnDragDrop = sg_ComboListDragDrop
      OnDragOver = sg_ComboListDragOver
      OnDrawCell = sg_ComboListDrawCell
      OnKeyDown = sg_ComboListKeyDown
    end
    object lb_ImageList: TListBox
      Left = 448
      Top = 24
      Width = 105
      Height = 315
      Style = lbOwnerDrawFixed
      Anchors = [akTop, akRight, akBottom]
      DragMode = dmAutomatic
      TabOrder = 1
      OnDrawItem = lb_ImageListDrawItem
    end
  end
end

object dxBreadcrumbEditEditorForm: TdxBreadcrumbEditEditorForm
  Left = 224
  Top = 210
  BorderIcons = [biSystemMenu]
  ClientHeight = 368
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 400
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  object pnlClient: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 364
    Height = 306
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Align = alClient
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 0
    object Panel2: TPanel
      Left = 232
      Top = 0
      Width = 132
      Height = 306
      Align = alRight
      BevelOuter = bvNone
      FullRepaint = False
      TabOrder = 1
      object btnNewItem: TcxButton
        AlignWithMargins = True
        Left = 6
        Top = 0
        Width = 126
        Height = 25
        Margins.Left = 6
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 6
        Align = alTop
        Caption = '&New Item'
        TabOrder = 0
        OnClick = btnNewItemClick
      end
      object btnDeleteItem: TcxButton
        AlignWithMargins = True
        Left = 6
        Top = 62
        Width = 126
        Height = 25
        Margins.Left = 6
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 6
        Align = alTop
        Caption = '&Delete'
        TabOrder = 2
        OnClick = btnDeleteItemClick
      end
      object btnNewSubItem: TcxButton
        AlignWithMargins = True
        Left = 6
        Top = 31
        Width = 126
        Height = 25
        Margins.Left = 6
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 6
        Align = alTop
        Caption = 'New &SubItem'
        TabOrder = 1
        OnClick = btnNewSubItemClick
      end
      object gbItemOptions: TcxGroupBox
        AlignWithMargins = True
        Left = 6
        Top = 96
        Margins.Left = 6
        Margins.Right = 0
        Align = alTop
        Caption = ' Item Options '
        TabOrder = 3
        Height = 150
        Width = 126
        object lbItemText: TcxLabel
          AlignWithMargins = True
          Left = 12
          Top = 21
          Margins.Left = 10
          Margins.Right = 10
          Margins.Bottom = 0
          Align = alTop
          Caption = '&Text:'
          FocusControl = teItemText
          Style.TransparentBorder = False
          Transparent = True
        end
        object teItemText: TcxTextEdit
          AlignWithMargins = True
          Left = 12
          Top = 37
          Margins.Left = 10
          Margins.Right = 10
          Align = alTop
          Properties.OnChange = teItemOptionsChange
          Style.TransparentBorder = False
          TabOrder = 1
          Width = 102
        end
        object lbItemImageIndex: TcxLabel
          AlignWithMargins = True
          Left = 12
          Top = 103
          Margins.Left = 10
          Margins.Right = 10
          Margins.Bottom = 0
          Align = alTop
          Caption = 'I&mage Index:'
          Style.TransparentBorder = False
          Transparent = True
        end
        object seItemImageIndex: TcxSpinEdit
          AlignWithMargins = True
          Left = 12
          Top = 119
          Margins.Left = 10
          Margins.Right = 10
          Align = alTop
          Properties.MinValue = -1.000000000000000000
          Properties.OnChange = teItemOptionsChange
          Style.TransparentBorder = False
          TabOrder = 5
          Value = -1
          Width = 102
        end
        object lbItemDisplayName: TcxLabel
          AlignWithMargins = True
          Left = 12
          Top = 62
          Margins.Left = 10
          Margins.Right = 10
          Margins.Bottom = 0
          Align = alTop
          Caption = 'Dis&play Text:'
          FocusControl = teItemDisplayName
          Style.TransparentBorder = False
          Transparent = True
        end
        object teItemDisplayName: TcxTextEdit
          AlignWithMargins = True
          Left = 12
          Top = 78
          Margins.Left = 10
          Margins.Right = 10
          Align = alTop
          Properties.OnChange = teItemOptionsChange
          Style.TransparentBorder = False
          TabOrder = 3
          Width = 102
        end
      end
    end
    object tvStruct: TcxTreeView
      Left = 0
      Top = 0
      Width = 232
      Height = 306
      Align = alClient
      DragMode = dmAutomatic
      Style.TransparentBorder = False
      TabOrder = 0
      OnDragDrop = tvStructDragDrop
      OnDragOver = tvStructDragOver
      HideSelection = False
      ReadOnly = True
      OnChange = tvStructChange
      OnDeletion = tvStructDeletion
    end
  end
  object pnlBottomBar: TPanel
    AlignWithMargins = True
    Left = 10
    Top = 322
    Width = 364
    Height = 36
    Margins.Left = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alBottom
    BevelOuter = bvNone
    FullRepaint = False
    TabOrder = 1
    object bvlLine: TdxBevel
      AlignWithMargins = True
      Left = 0
      Top = 0
      Width = 364
      Height = 6
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 5
      Align = alTop
      Shape = dxbsLineCenteredVert
    end
    object cxBtnCancel: TcxButton
      AlignWithMargins = True
      Left = 274
      Top = 11
      Width = 90
      Height = 25
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      Align = alRight
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object cxBtnOK: TcxButton
      AlignWithMargins = True
      Left = 178
      Top = 11
      Width = 90
      Height = 25
      Margins.Top = 0
      Margins.Bottom = 0
      Align = alRight
      Caption = '&OK'
      ModalResult = 1
      TabOrder = 1
    end
    object cbSaveSelection: TcxCheckBox
      Left = 0
      Top = 11
      Align = alLeft
      Caption = 'S&ave selection'
      State = cbsChecked
      TabOrder = 0
      Transparent = True
    end
  end
end

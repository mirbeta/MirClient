object cxVerticalGridEditor: TcxVerticalGridEditor
  Left = 404
  Top = 174
  Width = 282
  Height = 290
  Caption = 'VerticalGrid - rows editor'
  Color = clBtnFace
  Constraints.MinHeight = 290
  Constraints.MinWidth = 280
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  PopupMenu = PopupMenu
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 171
    Top = 0
    Width = 103
    Height = 256
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object btCategory: TcxButton
      Left = 5
      Top = 41
      Width = 89
      Height = 25
      Caption = 'Add category'
      TabOrder = 2
      OnClick = btCategoryClick
    end
    object btEditor: TcxButton
      Left = 5
      Top = 9
      Width = 89
      Height = 25
      Caption = 'Add editor'
      TabOrder = 0
      OnClick = btEditorClick
    end
    object btClose: TcxButton
      Left = 5
      Top = 226
      Width = 89
      Height = 25
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Close'
      TabOrder = 5
      OnClick = btCloseClick
    end
    object btMultiEditor: TcxButton
      Left = 5
      Top = 73
      Width = 89
      Height = 25
      Caption = 'Add multieditor'
      TabOrder = 1
      OnClick = btMultiEditorClick
    end
    object btDelete: TcxButton
      Left = 5
      Top = 105
      Width = 89
      Height = 25
      Caption = 'Delete'
      Enabled = False
      TabOrder = 3
      OnClick = btDeleteClick
    end
    object btClear: TcxButton
      Left = 5
      Top = 137
      Width = 89
      Height = 25
      Caption = 'Clear all'
      Enabled = False
      TabOrder = 4
      OnClick = btClearClick
    end
    object ShowVerticalGridLayoutEditor: TcxButton
      Left = 5
      Top = 168
      Width = 89
      Height = 25
      Caption = 'Layout Editor...'
      TabOrder = 6
      OnClick = ShowVerticalGridLayoutEditorClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 171
    Height = 256
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object lbRows: TListBox
      Left = 4
      Top = 4
      Width = 163
      Height = 248
      Align = alClient
      Color = clBtnFace
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbRowsClick
    end
  end
  object PopupMenu: TPopupMenu
    Left = 128
    Top = 16
    object miEditor: TMenuItem
      Caption = 'Add &editor'
      ShortCut = 45
      OnClick = miEditorClick
    end
    object miCategory: TMenuItem
      Caption = 'Add &category'
      OnClick = miCategoryClick
    end
    object miMultieditor: TMenuItem
      Caption = 'Add &multieditor'
      OnClick = miMultieditorClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miDelete: TMenuItem
      Caption = '&Delete row'
      Enabled = False
      ShortCut = 46
      OnClick = miDeleteClick
    end
    object miClearAll: TMenuItem
      Caption = 'C&lear all'
      Enabled = False
      OnClick = miClearAllClick
    end
  end
end

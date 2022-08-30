object FieldsEditor: TFieldsEditor
  Left = 196
  Top = 113
  ActiveControl = FieldListBox
  BorderStyle = bsSizeToolWin
  Caption = 'Form1.Table1'
  ClientHeight = 215
  ClientWidth = 197
  Color = clBtnFace
  ParentFont = True
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000010000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000FFFFFFFF07770000000000000000
    0000F44444FF0F0700000000000000000000FFFFFFFF0F770000000000000000
    0000F444444F000000000000000000000000FFFFFFFF07770000000000000000
    0000F4444FFF077700000000000000000000FFFFFFFF07770000000000000000
    0000F44444FF077700000000000000000000FFFFFFFF07770000000000000000
    0000F444444F000000000000000000000000FFFFFFFF07770000000000000000
    0000F44444FF0F0700000000000000088880FFFFFFFF0FF70000000000000008
    78F00000000000000000000000000008F8F77F7F7777F8000000000000000008
    78FFFF7FFFFFF8000000000000000008F8F77F7F777FF8000000000000000008
    78FFFF7FFFFFF8000999999000000008F8F77F7F7777F8000000000000000008
    78FFFF7FFFFFF800000000000000000888888888888888000000000000000008
    78F7777F7777780000000000000000088888888888888800000EA00000000000
    0000000000000000000AE0000000000000000000000000000EAEAEA000000000
    0000000000000000000AE000000000000000000000000000000EA00000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFE0007FFFE0007FFFE0007FFFE0007FFFE0007FFFE0007FFFE00
    07FFFE0007FFFE0007FFFE0007FFFE0007FFFE0007FFFE0007FFE00007FFE000
    07FFE0003FFFE0003FFFE000300FE000300FE000300FE0003FFFE0003FFFE000
    3C3FE0003C3FFFFFF00FFFFFF00FFFFFF00FFFFFFC3FFFFFFC3FFFFFFFFF}
  OldCreateOrder = True
  PopupMenu = LocalMenu
  PopupMode = pmExplicit
  ShowHint = True
  OnClick = SelectTable
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 175
    Width = 197
    Height = 2
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    Visible = False
    ExplicitTop = 172
    ExplicitWidth = 116
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 197
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 1
    ParentColor = True
    TabOrder = 0
    OnClick = SelectTable
    ExplicitWidth = 116
    object DBNavigator: TDBNavigator
      Left = 1
      Top = 1
      Width = 195
      Height = 18
      DataSource = DataSource
      VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast]
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 114
    end
  end
  object FieldListBox: TListBox
    Left = 0
    Top = 21
    Width = 197
    Height = 154
    Hint = 'Fields'
    Align = alClient
    DragMode = dmAutomatic
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
    OnClick = AListBoxClick
    OnDragDrop = FieldListBoxDragDrop
    OnDragOver = FieldListBoxDragOver
    OnKeyDown = AListBoxKeyDown
    OnKeyPress = AListBoxKeyPress
    OnStartDrag = FieldListBoxStartDrag
    ExplicitWidth = 116
  end
  object AggListBox: TListBox
    Left = 0
    Top = 177
    Width = 197
    Height = 38
    Hint = 'Aggregates'
    Align = alBottom
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 2
    Visible = False
    OnClick = AListBoxClick
    OnKeyDown = AListBoxKeyDown
    OnKeyPress = AListBoxKeyPress
    ExplicitWidth = 116
  end
  object DataSource: TDataSource
    Left = 10
    Top = 32
  end
  object LocalMenu: TPopupActionBar
    HelpContext = 30130
    OnPopup = LocalMenuPopup
    Left = 42
    Top = 32
    object AddItem: TMenuItem
      Caption = '&Add fields...'
      HelpContext = 30131
      ShortCut = 16449
      OnClick = AddItemClick
    end
    object NewItem: TMenuItem
      Caption = '&New field...'
      HelpContext = 30132
      ShortCut = 16462
      OnClick = NewItemClick
    end
    object Addallfields1: TMenuItem
      Caption = 'Add all &fields'
      ShortCut = 16454
      OnClick = AddAllFields
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object CutItem: TMenuItem
      Caption = 'Cu&t'
      HelpContext = 30133
      ShortCut = 16472
      OnClick = CutItemClick
    end
    object CopyItem: TMenuItem
      Caption = '&Copy'
      HelpContext = 30134
      ShortCut = 16451
      OnClick = CopyItemClick
    end
    object PasteItem: TMenuItem
      Caption = '&Paste'
      HelpContext = 30135
      ShortCut = 16470
      OnClick = PasteItemClick
    end
    object DeleteItem: TMenuItem
      Caption = '&Delete'
      HelpContext = 30136
      ShortCut = 46
      OnClick = DeleteItemClick
    end
    object SelectAllItem: TMenuItem
      Caption = 'Se&lect all'
      HelpContext = 30137
      ShortCut = 16460
      OnClick = SelectAllItemClick
    end
  end
end

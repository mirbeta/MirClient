object RzShellLocatorEditDlg: TRzShellLocatorEditDlg
  Left = 316
  Top = 149
  BorderStyle = bsDialog
  Caption = 'Shell Locator Editor'
  ClientHeight = 178
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ItemTxt: TRzLabel
    Left = 128
    Top = 104
    Width = 173
    Height = 21
    AutoSize = False
    Enabled = False
    Layout = tlCenter
    BorderOuter = fsStatus
  end
  object OkBtn: TRzButton
    Left = 226
    Top = 144
    Default = True
    ModalResult = 1
    Caption = 'OK'
    Color = 15791348
    HotTrack = True
    TabOrder = 4
  end
  object CancelBtn: TRzButton
    Left = 309
    Top = 144
    Cancel = True
    ModalResult = 2
    Caption = 'Cancel'
    Color = 15791348
    HotTrack = True
    TabOrder = 5
  end
  object UsePidlRdo: TRzRadioButton
    Left = 8
    Top = 108
    Width = 86
    Height = 15
    Caption = 'Use item id list'
    HotTrack = True
    TabOrder = 3
    OnClick = UsePidlRdoClick
  end
  object DontUseAnythingRdo: TRzRadioButton
    Left = 8
    Top = 12
    Width = 107
    Height = 15
    Caption = 'Don'#39't use anything'
    HotTrack = True
    TabOrder = 0
    OnClick = UsePidlRdoClick
  end
  object UsePathnameRdo: TRzRadioButton
    Left = 8
    Top = 76
    Width = 88
    Height = 15
    Caption = 'Use pathname'
    HotTrack = True
    TabOrder = 2
    OnClick = UsePidlRdoClick
  end
  object UseCSIDLRdo: TRzRadioButton
    Left = 8
    Top = 44
    Width = 72
    Height = 15
    Caption = 'Use CSIDL'
    HotTrack = True
    TabOrder = 1
    OnClick = UsePidlRdoClick
  end
  object ComboBox1: TRzComboBox
    Left = 128
    Top = 40
    Width = 253
    Height = 21
    Style = csDropDownList
    Ctl3D = False
    Enabled = False
    FlatButtons = True
    FrameVisible = True
    ItemHeight = 13
    ParentCtl3D = False
    TabOrder = 6
  end
  object PathNameEdt: TRzEdit
    Left = 128
    Top = 72
    Width = 253
    Height = 21
    Enabled = False
    FrameVisible = True
    TabOrder = 7
  end
  object BrowseBtn: TRzButton
    Left = 308
    Top = 102
    Caption = '&Browse...'
    Color = 15791348
    Enabled = False
    HotTrack = True
    TabOrder = 8
    OnClick = BrowseBtnClick
  end
  object RzSelectFolderDialog1: TRzSelectFolderDialog
    Title = 'Select an item'
    Options = [sfdoReadOnly]
    Left = 340
    Top = 4
  end
end

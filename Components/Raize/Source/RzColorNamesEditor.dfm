object RzColorNamesEditDlg: TRzColorNamesEditDlg
  Left = 213
  Top = 114
  BorderStyle = bsDialog
  Caption = 'Color Names Editor'
  ClientHeight = 302
  ClientWidth = 403
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object RzPanel1: TRzPanel
    Left = 8
    Top = 36
    Width = 385
    Height = 222
    BorderOuter = fsFlat
    TabOrder = 4
    object GrdColors: TStringGrid
      Left = 1
      Top = 1
      Width = 383
      Height = 220
      Align = alClient
      BorderStyle = bsNone
      ColCount = 3
      Ctl3D = False
      DefaultRowHeight = 16
      FixedCols = 0
      RowCount = 43
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goEditing, goThumbTracking]
      ParentCtl3D = False
      TabOrder = 0
      OnDrawCell = GrdColorsDrawCell
      OnSelectCell = GrdColorsSelectCell
      OnSetEditText = GrdColorsSetEditText
      ColWidths = (
        122
        60
        180)
      RowHeights = (
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16
        16)
    end
  end
  object BtnOK: TRzButton
    Left = 235
    Top = 268
    Default = True
    ModalResult = 1
    Caption = 'OK'
    Color = 15791348
    HotTrack = True
    TabOrder = 2
  end
  object BtnCancel: TRzButton
    Left = 318
    Top = 268
    Cancel = True
    ModalResult = 2
    Caption = 'Cancel'
    Color = 15791348
    HotTrack = True
    TabOrder = 3
  end
  object BtnLoad: TRzButton
    Left = 208
    Top = 8
    Caption = 'Load...'
    Color = 15791348
    HotTrack = True
    TabOrder = 0
    OnClick = BtnLoadClick
  end
  object BtnSave: TRzButton
    Left = 292
    Top = 8
    Caption = 'Save...'
    Color = 15791348
    HotTrack = True
    TabOrder = 1
    OnClick = BtnSaveClick
  end
  object DlgOpen: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Left = 36
    Top = 8
  end
  object DlgSave: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Left = 124
    Top = 8
  end
end

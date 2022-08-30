object RzComboBoxEditDlg: TRzComboBoxEditDlg
  Left = 250
  Top = 112
  Caption = '- ComboBox Editor'
  ClientHeight = 282
  ClientWidth = 379
  Color = clBtnFace
  Constraints.MinHeight = 320
  Constraints.MinWidth = 395
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PnlButtons: TRzPanel
    Left = 0
    Top = 246
    Width = 379
    Height = 36
    Align = alBottom
    BorderOuter = fsNone
    TabOrder = 0
    object btnLoad: TRzButton
      Left = 8
      Top = 4
      Caption = 'Load...'
      Color = 15791348
      HotTrack = True
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object btnClear: TRzButton
      Left = 95
      Top = 4
      Caption = 'Clear'
      Color = 15791348
      HotTrack = True
      TabOrder = 1
      OnClick = btnClearClick
    end
    object RzPanel1: TRzPanel
      Left = 213
      Top = 0
      Width = 166
      Height = 36
      Align = alRight
      BorderOuter = fsNone
      TabOrder = 2
      object btnOk: TRzButton
        Left = 0
        Top = 4
        Default = True
        ModalResult = 1
        Caption = 'OK'
        Color = 15791348
        HotTrack = True
        TabOrder = 0
      end
      object btnCancel: TRzButton
        Left = 82
        Top = 4
        Cancel = True
        ModalResult = 2
        Caption = 'Cancel'
        Color = 15791348
        HotTrack = True
        TabOrder = 1
      end
    end
  end
  object pnlClientArea: TRzPanel
    Left = 0
    Top = 0
    Width = 379
    Height = 246
    Align = alClient
    BorderOuter = fsNone
    BorderWidth = 4
    TabOrder = 1
    object grdItemsValues: TRzStringGrid
      Left = 4
      Top = 4
      Width = 371
      Height = 238
      Align = alClient
      ColCount = 3
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowMoving, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
      TabOrder = 0
      FrameVisible = True
      OnResize = grdItemsValuesResize
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 12
    Top = 24
  end
end

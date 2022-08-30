object RzDBRadioGroupEditDlg: TRzDBRadioGroupEditDlg
  Left = 330
  Top = 124
  Caption = '- DBRadioGroup Editor'
  ClientHeight = 295
  ClientWidth = 592
  Color = clBtnFace
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
  object pnlButtons: TRzPanel
    Left = 0
    Top = 262
    Width = 592
    Height = 33
    Align = alBottom
    BorderOuter = fsNone
    TabOrder = 1
    object btnLoad: TRzButton
      Left = 8
      Top = 0
      Caption = 'Load...'
      Color = 15791348
      HotTrack = True
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object btnClear: TRzButton
      Left = 91
      Top = 0
      Caption = 'Clear'
      Color = 15791348
      HotTrack = True
      TabOrder = 1
      OnClick = btnClearClick
    end
    object RzPanel1: TRzPanel
      Left = 426
      Top = 0
      Width = 166
      Height = 33
      Align = alRight
      BorderOuter = fsNone
      TabOrder = 2
      object btnOk: TRzButton
        Left = 0
        Top = 0
        Default = True
        ModalResult = 1
        Caption = 'OK'
        Color = 15791348
        HotTrack = True
        TabOrder = 0
        OnClick = btnOkClick
      end
      object btnCancel: TRzButton
        Left = 82
        Top = 0
        Cancel = True
        ModalResult = 2
        Caption = 'Cancel'
        Color = 15791348
        HotTrack = True
        TabOrder = 1
      end
    end
  end
  object PnlOptions: TRzPanel
    Left = 0
    Top = 0
    Width = 305
    Height = 262
    Align = alLeft
    BorderOuter = fsNone
    TabOrder = 0
    DesignSize = (
      305
      262)
    object Label1: TRzLabel
      Left = 8
      Top = 16
      Width = 44
      Height = 13
      Caption = 'Caption'
      ParentColor = False
    end
    object Label2: TRzLabel
      Left = 8
      Top = 48
      Width = 50
      Height = 13
      Caption = 'Columns'
      ParentColor = False
    end
    object edtCaption: TRzEdit
      Left = 76
      Top = 12
      Width = 216
      Height = 21
      Text = ''
      FrameVisible = True
      TabOrder = 0
      OnChange = edtCaptionChange
    end
    object trkColumns: TRzTrackBar
      Left = 68
      Top = 35
      Width = 233
      Min = 1
      Position = 1
      TickStyle = tkOwnerDraw
      OnChange = trkColumnsChange
      OnDrawTick = trkColumnsDrawTick
      TabOrder = 1
    end
    object grdItemsValues: TRzStringGrid
      Left = 10
      Top = 87
      Width = 282
      Height = 167
      Anchors = [akLeft, akTop, akBottom]
      ColCount = 3
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goRowMoving, goEditing, goTabs, goAlwaysShowEditor, goThumbTracking]
      TabOrder = 2
      OnClick = grdItemsValuesClick
      FrameVisible = True
      OnResize = grdItemsValuesResize
    end
  end
  object pnlPreview: TRzPanel
    Left = 305
    Top = 0
    Width = 287
    Height = 262
    Align = alClient
    BorderOuter = fsNone
    BorderWidth = 8
    Constraints.MinHeight = 150
    Constraints.MinWidth = 100
    TabOrder = 2
    object grpPreview: TRzDBRadioGroup
      Left = 8
      Top = 8
      Width = 271
      Height = 246
      Align = alClient
      Caption = ''
      TabOrder = 0
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text Files|*.txt|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 180
    Top = 236
  end
end

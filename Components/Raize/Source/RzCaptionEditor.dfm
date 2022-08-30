object RzCaptionEditDlg: TRzCaptionEditDlg
  Left = 236
  Top = 115
  Caption = 'Caption/Hint Editor'
  ClientHeight = 212
  ClientWidth = 334
  Color = clBtnFace
  Constraints.MinHeight = 250
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PnlEditor: TRzPanel
    Left = 0
    Top = 0
    Width = 334
    Height = 176
    Align = alClient
    BorderOuter = fsNone
    BorderWidth = 8
    TabOrder = 0
    ExplicitWidth = 342
    ExplicitHeight = 180
    object EdtCaption: TRzMemo
      Left = 8
      Top = 8
      Width = 326
      Height = 164
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      OnKeyPress = EdtCaptionKeyPress
      FrameVisible = True
    end
  end
  object RzDialogButtons1: TRzDialogButtons
    Left = 0
    Top = 176
    Width = 334
    ButtonColor = 15791348
    HotTrack = True
    TabOrder = 1
    ExplicitTop = 180
    ExplicitWidth = 342
  end
end

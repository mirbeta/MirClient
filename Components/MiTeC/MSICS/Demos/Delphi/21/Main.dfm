object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'CPU Usage'
  ClientHeight = 71
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lWarn: TLabel
    Left = 0
    Top = 0
    Width = 334
    Height = 71
    Align = alClient
    Alignment = taCenter
    Caption = 'Pdh initialization failed'
    Layout = tlCenter
    Visible = False
    ExplicitWidth = 105
    ExplicitHeight = 13
  end
end

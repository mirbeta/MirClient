object GridModeDemoTerminateForm: TGridModeDemoTerminateForm
  Left = 345
  Top = 248
  Cursor = crHourGlass
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'GridModeDemoTerminateForm'
  ClientHeight = 120
  ClientWidth = 334
  Color = 12937777
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 334
    Height = 120
    Align = alClient
    BevelWidth = 2
    ParentBackground = False
    ParentColor = True
    TabOrder = 0
    object lbDesc: TLabel
      Left = 13
      Top = 24
      Width = 307
      Height = 20
      Alignment = taCenter
      AutoSize = False
      Caption = 'Deleting previously inserted records...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 111
      Top = 72
      Width = 107
      Height = 20
      Caption = 'Please wait...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end

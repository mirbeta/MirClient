object ChatForm: TChatForm
  Left = 249
  Top = 261
  Width = 467
  Height = 396
  Caption = 'RTC Chat'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mainPanel: TPanel
    Left = 0
    Top = 270
    Width = 451
    Height = 90
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 2
    TabOrder = 0
    object Panel3: TPanel
      Left = 376
      Top = 2
      Width = 73
      Height = 86
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object btnBuzz: TSpeedButton
        Left = 5
        Top = 4
        Width = 64
        Height = 25
        Hint = 'Buzz!'
        Caption = 'BUZZ!'
        Flat = True
        ParentShowHint = False
        ShowHint = True
        OnClick = btnBuzzClick
      end
      object btnSend: TBitBtn
        Left = 4
        Top = 36
        Width = 65
        Height = 41
        Caption = 'SEND'
        Enabled = False
        TabOrder = 0
        TabStop = False
        OnClick = btnSendClick
      end
    end
    object eEnter: TMemo
      Left = 2
      Top = 2
      Width = 374
      Height = 86
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = eEnterChange
      OnKeyDown = eEnterKeyDown
      OnKeyPress = eEnterKeyPress
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 270
    Align = alClient
    BorderWidth = 2
    Color = 14408667
    TabOrder = 1
    object rtfChat: TRichEdit
      Left = 3
      Top = 3
      Width = 445
      Height = 264
      TabStop = False
      Align = alClient
      Color = 13565181
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end

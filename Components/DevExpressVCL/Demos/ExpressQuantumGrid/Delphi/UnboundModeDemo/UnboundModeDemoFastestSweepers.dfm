object UnboundModeDemoFastestSweepersForm: TUnboundModeDemoFastestSweepersForm
  Left = 328
  Top = 282
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Fastest Mine Sweepers'
  ClientHeight = 156
  ClientWidth = 249
  Color = 15451300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbBeginner: TLabel
    Left = 25
    Top = 22
    Width = 42
    Height = 13
    Caption = 'Beginner'
  end
  object lbIntermediate: TLabel
    Left = 25
    Top = 46
    Width = 58
    Height = 13
    Caption = 'Intermediate'
  end
  object lbExpert: TLabel
    Left = 25
    Top = 70
    Width = 30
    Height = 13
    Caption = 'Expert'
  end
  object lbExpertTime: TLabel
    Left = 105
    Top = 70
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object lbIntermediateTime: TLabel
    Left = 105
    Top = 46
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object lbBeginnerTime: TLabel
    Left = 105
    Top = 22
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object ibExpertName: TLabel
    Left = 177
    Top = 70
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object lbIntermediateName: TLabel
    Left = 177
    Top = 46
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object lbBeginnerName: TLabel
    Left = 177
    Top = 22
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object bntOK: TcxButton
    Left = 144
    Top = 104
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object btnResetScores: TcxButton
    Left = 24
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Reset Scores'
    TabOrder = 1
    OnClick = btnResetScoresClick
  end
end

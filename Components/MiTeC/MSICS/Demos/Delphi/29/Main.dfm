object wnd_gai_Main: Twnd_gai_Main
  Left = 345
  Top = 362
  Caption = 'Get Machine Info'
  ClientHeight = 657
  ClientWidth = 665
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ScreenSnap = True
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  DesignSize = (
    665
    657)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 57
    Top = 73
    Width = 78
    Height = 13
    Alignment = taRightJustify
    Caption = 'MAC addresses:'
  end
  object Label2: TLabel
    Left = 50
    Top = 17
    Width = 85
    Height = 13
    Alignment = taRightJustify
    Caption = 'Machine name:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Icon: TImage
    Left = 10
    Top = 6
    Width = 32
    Height = 32
    AutoSize = True
    Center = True
  end
  object Label3: TLabel
    Left = 46
    Top = 227
    Width = 89
    Height = 13
    Alignment = taRightJustify
    Caption = 'Operating system:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 39
    Top = 47
    Width = 96
    Height = 13
    Alignment = taRightJustify
    Caption = 'Domain/Workgroup:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object lCopy: TLabel
    Left = 8
    Top = 636
    Width = 170
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Copyright '#169' 2003-2006 Michal Mutl'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGray
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label6: TLabel
    Left = 69
    Top = 148
    Width = 66
    Height = 13
    Alignment = taRightJustify
    Caption = 'IP addresses:'
  end
  object Label7: TLabel
    Left = 12
    Top = 425
    Width = 88
    Height = 13
    Alignment = taRightJustify
    Caption = 'Shared resources:'
  end
  object Label8: TLabel
    Left = 72
    Top = 257
    Width = 63
    Height = 13
    Alignment = taRightJustify
    Caption = 'Logged user:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label5: TLabel
    Left = 47
    Top = 285
    Width = 88
    Height = 13
    Alignment = taRightJustify
    Caption = 'Machine datetime:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label9: TLabel
    Left = 81
    Top = 346
    Width = 54
    Height = 13
    Alignment = taRightJustify
    Caption = 'Type flags:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label10: TLabel
    Left = 78
    Top = 316
    Width = 57
    Height = 13
    Alignment = taRightJustify
    Caption = 'Description:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object MACMemo: TMemo
    Left = 141
    Top = 73
    Width = 508
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object eMachine: TEdit
    Left = 141
    Top = 13
    Width = 508
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 480
    Top = 620
    Width = 171
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Caption = 'Get Machine Info'
    Default = True
    TabOrder = 10
    OnClick = cmGet
  end
  object eOS: TEdit
    Left = 141
    Top = 223
    Width = 508
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    TabOrder = 4
  end
  object eGroup: TEdit
    Left = 141
    Top = 43
    Width = 508
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    TabOrder = 1
  end
  object IPMemo: TMemo
    Left = 141
    Top = 149
    Width = 508
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ShareMemo: TMemo
    Left = 14
    Top = 444
    Width = 638
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 9
    WordWrap = False
  end
  object eUser: TEdit
    Left = 141
    Top = 253
    Width = 508
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    TabOrder = 5
  end
  object eDT: TEdit
    Left = 141
    Top = 281
    Width = 508
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    TabOrder = 6
  end
  object eDesc: TEdit
    Left = 141
    Top = 312
    Width = 508
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = True
    ParentFont = False
    ReadOnly = True
    TabOrder = 7
  end
  object FlagMemo: TMemo
    Left = 141
    Top = 343
    Width = 508
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 8
  end
end

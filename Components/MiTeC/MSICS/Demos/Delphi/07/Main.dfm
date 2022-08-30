object Form1: TForm1
  Left = 318
  Top = 277
  Caption = 'Form1'
  ClientHeight = 562
  ClientWidth = 628
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    628
    562)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 628
    Height = 562
    Align = alClient
    Alignment = taCenter
    Caption = 'Collecting data...please wait'
    Layout = tlCenter
    ExplicitWidth = 137
    ExplicitHeight = 13
  end
  object Label2: TLabel
    Left = 16
    Top = 19
    Width = 43
    Height = 13
    Alignment = taRightJustify
    Caption = 'Machine:'
  end
  object Label3: TLabel
    Left = 199
    Top = 19
    Width = 52
    Height = 13
    Alignment = taRightJustify
    Caption = 'Username:'
  end
  object Label4: TLabel
    Left = 402
    Top = 19
    Width = 50
    Height = 13
    Alignment = taRightJustify
    Caption = 'Password:'
  end
  object Button1: TButton
    Left = 545
    Top = 529
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Tree: TTreeView
    Left = 8
    Top = 55
    Width = 612
    Height = 465
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderWidth = 5
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    RowSelect = True
    ShowButtons = False
    ShowHint = True
    ShowLines = False
    TabOrder = 1
    OnCustomDrawItem = TreeCustomDrawItem
  end
  object Button2: TButton
    Left = 8
    Top = 529
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Anchors = [akLeft, akBottom]
    Caption = 'Refresh'
    Default = True
    TabOrder = 2
    OnClick = cmRefresh
  end
  object eMachine: TEdit
    Left = 65
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object eUser: TEdit
    Left = 257
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object ePwd: TEdit
    Left = 458
    Top = 16
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 5
  end
end

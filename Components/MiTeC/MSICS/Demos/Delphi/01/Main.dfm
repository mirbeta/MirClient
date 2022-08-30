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
  OnShow = FormShow
  DesignSize = (
    628
    562)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 628
    Height = 522
    Align = alClient
    Alignment = taCenter
    Caption = 'Collecting data...please wait'
    Layout = tlCenter
    ExplicitWidth = 137
    ExplicitHeight = 13
  end
  object Tree: TTreeView
    Left = 8
    Top = 8
    Width = 612
    Height = 512
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    ParentShowHint = False
    ReadOnly = True
    RowSelect = True
    ShowButtons = False
    ShowHint = True
    ShowLines = False
    TabOrder = 0
    OnCustomDrawItem = TreeCustomDrawItem
  end
  object ButtonPanel: TPanel
    Left = 0
    Top = 522
    Width = 628
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      628
      40)
    object Button1: TButton
      Left = 545
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Close'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 6
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Refresh'
      Default = True
      TabOrder = 1
      OnClick = cmRefresh
    end
    object bSave: TButton
      Left = 89
      Top = 6
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Save...'
      TabOrder = 2
      OnClick = bSaveClick
    end
    object bLoad: TButton
      Left = 170
      Top = 6
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load...'
      TabOrder = 3
      OnClick = bLoadClick
    end
  end
  object od: TOpenDialog
    Filter = 'MiTeC System Information files|*.sif|All files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 316
    Top = 276
  end
  object sd: TSaveDialog
    Filter = 'MiTeC System Information files|*.sif|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 394
    Top = 276
  end
  object SI: TMiTeC_SystemInfo
    Left = 215
    Top = 276
  end
end

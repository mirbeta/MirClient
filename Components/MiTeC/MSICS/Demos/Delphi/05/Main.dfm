object wnd_dv_Main: Twnd_dv_Main
  Left = 295
  Top = 356
  AlphaBlend = True
  Caption = 'Storage Devices'
  ClientHeight = 573
  ClientWidth = 910
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 910
    Height = 532
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 10
    Caption = ' '
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 378
      Top = 10
      Height = 512
      ExplicitLeft = 443
      ExplicitTop = 20
    end
    object List: TListView
      Left = 381
      Top = 10
      Width = 519
      Height = 512
      Align = alClient
      Columns = <
        item
          Caption = 'Property'
          Width = 150
        end
        item
          Caption = 'Value'
          Width = 340
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object Tree: TTreeView
      Left = 10
      Top = 10
      Width = 368
      Height = 512
      Align = alLeft
      BorderWidth = 5
      HideSelection = False
      Images = ilSystem
      Indent = 19
      ReadOnly = True
      TabOrder = 1
      OnCustomDrawItem = TreeCustomDrawItem
      OnDeletion = TreeDeletion
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 532
    Width = 910
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    Caption = ' '
    TabOrder = 1
    DesignSize = (
      910
      41)
    object bClose: TButton
      Left = 823
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Close'
      Default = True
      TabOrder = 0
      OnClick = bCloseClick
    end
    object bRefresh: TButton
      Left = 11
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = cmRefresh
    end
    object bSave: TButton
      Left = 91
      Top = 8
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Caption = 'Save...'
      TabOrder = 2
      OnClick = bSaveClick
    end
    object cbxAuto: TCheckBox
      Left = 179
      Top = 14
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Caption = 'Auto Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
  end
  object sd: TSaveDialog
    Filter = 'MiTeC System Information files|*.sif|All files|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 278
    Top = 204
  end
  object ilSystem: TImageList
    ColorDepth = cd32Bit
    Left = 243
    Top = 423
  end
end

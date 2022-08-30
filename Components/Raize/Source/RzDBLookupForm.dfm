object RzDBLookupForm: TRzDBLookupForm
  Left = 227
  Top = 118
  Caption = 'Caption Goes Here'
  ClientHeight = 275
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PnlPrompt: TPanel
    Left = 0
    Top = 0
    Width = 419
    Height = 47
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object LblPrompt: TLabel
      Left = 4
      Top = 4
      Width = 52
      Height = 13
      Caption = 'Search for '
    end
    object EdtSearch: TRzEdit
      Left = 4
      Top = 21
      Width = 213
      Height = 21
      Text = ''
      TabOrder = 0
      OnChange = EdtSearchChange
      OnKeyDown = EdtSearchKeyDown
      OnKeyPress = EdtSearchKeyPress
    end
  end
  object PnlNavigator: TPanel
    Left = 0
    Top = 47
    Width = 419
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object NavLookup: TRzDBNavigator
      Left = 4
      Top = 0
      Width = 240
      Height = 25
      DataSource = SrcLookup
      BorderOuter = fsNone
      ParentShowHint = False
      ShowHint = False
      TabOrder = 0
    end
  end
  object PnlLookup: TRzPanel
    Left = 0
    Top = 73
    Width = 419
    Height = 166
    Align = alClient
    BorderOuter = fsNone
    BorderWidth = 4
    TabOrder = 2
    object GrdLookup: TRzDBGrid
      Left = 4
      Top = 4
      Width = 411
      Height = 158
      Align = alClient
      Ctl3D = True
      DataSource = SrcLookup
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit]
      ParentCtl3D = False
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      OnDblClick = GrdLookupDblClick
    end
  end
  object PnlButtons: TRzDialogButtons
    Left = 0
    Top = 239
    Width = 419
    ButtonColor = 15791348
    CaptionHelp = 'Help'
    HotTrack = True
    OnClickHelp = PnlButtonsClickHelp
    TabOrder = 3
  end
  object SrcLookup: TDataSource
    Left = 392
    Top = 4
  end
end

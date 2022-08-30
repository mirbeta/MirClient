object RzLookupForm: TRzLookupForm
  Left = 209
  Top = 123
  Caption = 'Enter your own caption'
  ClientHeight = 230
  ClientWidth = 259
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PnlSelections: TPanel
    Left = 0
    Top = 47
    Width = 259
    Height = 147
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    ExplicitWidth = 267
    ExplicitHeight = 151
    object LstSelections: TRzListBox
      Left = 4
      Top = 4
      Width = 259
      Height = 143
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object PnlPrompt: TPanel
    Left = 0
    Top = 0
    Width = 259
    Height = 21
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'Enter your selection prompt'
    TabOrder = 0
    ExplicitWidth = 267
  end
  object PnlSearch: TPanel
    Left = 0
    Top = 21
    Width = 259
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    ExplicitWidth = 267
    object EdtSearch: TRzEdit
      Left = 4
      Top = 4
      Width = 259
      Height = 21
      Text = ''
      AutoSelect = False
      TabOrder = 0
      OnChange = EdtSearchChange
      OnKeyDown = EdtSearchKeyDown
    end
  end
  object PnlButtons: TRzDialogButtons
    Left = 0
    Top = 194
    Width = 259
    CaptionHelp = 'Help'
    HotTrack = True
    OnClickHelp = PnlButtonsClickHelp
    TabOrder = 3
    ExplicitTop = 198
    ExplicitWidth = 267
  end
end

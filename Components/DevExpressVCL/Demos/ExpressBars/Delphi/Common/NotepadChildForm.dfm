object frmNotepadChild: TfrmNotepadChild
  Left = 0
  Top = 0
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object bvSpacer4: TBevel
    Left = 629
    Top = 6
    Width = 6
    Height = 288
    Align = alRight
    Shape = bsSpacer
  end
  object Bevel1: TBevel
    Left = 0
    Top = 6
    Width = 6
    Height = 288
    Align = alLeft
    Shape = bsSpacer
  end
  object Bevel2: TBevel
    Left = 0
    Top = 0
    Width = 635
    Height = 6
    Align = alTop
    Shape = bsSpacer
  end
  object Bevel3: TBevel
    Left = 0
    Top = 294
    Width = 635
    Height = 6
    Align = alBottom
    Shape = bsSpacer
  end
  object Editor: TcxRichEdit
    Left = 6
    Top = 6
    Align = alClient
    Properties.HideSelection = False
    Properties.ScrollBars = ssVertical
    Properties.OnChange = EditorPropertiesChange
    Properties.OnSelectionChange = EditorPropertiesSelectionChange
    Style.Color = clWindow
    Style.TextColor = clWindowText
    TabOrder = 0
    Height = 288
    Width = 623
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'RTF'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist]
    Left = 8
    Top = 8
  end
end

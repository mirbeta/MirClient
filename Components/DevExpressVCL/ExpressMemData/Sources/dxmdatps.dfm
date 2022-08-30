object frmdxMemDataPersistent: TfrmdxMemDataPersistent
  Left = 403
  Top = 251
  AutoScroll = False
  Caption = 'ExpressMemData Persistent Editor...'
  ClientHeight = 305
  ClientWidth = 525
  Color = clBtnFace
  OldCreateOrder = False
  Position = poScreenCenter
  Font.Height = -11
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottom: TPanel
    Left = 0
    Top = 272
    Width = 525
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnClear: TButton
      Left = 11
      Top = 5
      Width = 76
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Clear'
      TabOrder = 0
      OnClick = btnClearClick
    end
    object btnLoad: TButton
      Left = 182
      Top = 5
      Width = 77
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Load...'
      TabOrder = 1
      OnClick = btnLoadClick
    end
    object btnSave: TButton
      Left = 268
      Top = 5
      Width = 77
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&Save...'
      TabOrder = 2
      OnClick = btnSaveClick
    end
    object btnOK: TButton
      Left = 353
      Top = 5
      Width = 77
      Height = 23
      Anchors = [akTop, akRight]
      Caption = '&OK'
      ModalResult = 1
      TabOrder = 3
    end
    object btnCancel: TButton
      Left = 438
      Top = 5
      Width = 77
      Height = 23
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 4
    end
    object btnGetFieldsFromFile: TButton
      Left = 96
      Top = 5
      Width = 77
      Height = 23
      Anchors = [akTop, akRight]
      Caption = 'Get &Fields...'
      TabOrder = 5
      OnClick = btnGetFieldsFromFileClick
    end
  end
  object DBGrid: TDBGrid
    Left = 0
    Top = 0
    Width = 525
    Height = 272
    Align = alClient
    DataSource = DataSource
    Options = [dgEditing, dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgMultiSelect]
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object DataSource: TDataSource
    DataSet = InternalMemData
    Left = 88
    Top = 72
  end
  object InternalMemData: TdxMemData
    Indexes = <>
    SortOptions = []
    Left = 48
    Top = 48
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.dat'
    Filter = 'MemData Files|*.dat|All Files|*.*'
    Left = 224
    Top = 176
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.dat'
    Filter = 'MemData Files|*.dat|All Files|*.*'
    Left = 248
    Top = 168
  end
end

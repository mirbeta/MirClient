object fmAddDictionary: TfmAddDictionary
  Left = 312
  Top = 264
  AutoScroll = False
  BorderStyle = bsDialog
  Caption = 'Add Dictionary'
  ClientHeight = 290
  ClientWidth = 443
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 116
    Width = 39
    Height = 13
    Caption = 'Affix file:'
    Transparent = True
  end
  object Label2: TLabel
    Left = 8
    Top = 148
    Width = 66
    Height = 13
    Caption = 'Dictionary file:'
    Transparent = True
  end
  object Label3: TLabel
    Left = 8
    Top = 180
    Width = 51
    Height = 13
    Caption = 'Language:'
    Transparent = True
  end
  object Label4: TLabel
    Left = 8
    Top = 212
    Width = 55
    Height = 13
    Caption = 'Code page:'
    Transparent = True
    Visible = False
  end
  object Bevel1: TBevel
    Left = 8
    Top = 240
    Width = 425
    Height = 9
    Shape = bsTopLine
  end
  object Label5: TLabel
    Left = 56
    Top = 65
    Width = 312
    Height = 13
    Caption = 
      'You can download free Hunspell dictionaries at the following URL' +
      ':'
    Transparent = True
  end
  object rgDictionatyType: TcxRadioGroup
    Left = 8
    Top = 8
    Caption = ' Choose a dictionary type '
    Properties.Columns = 3
    Properties.Items = <
      item
        Caption = 'Hunspell (recommended)'
      end
      item
        Caption = 'Open Office'
        Tag = 1
      end
      item
        Caption = 'ISpell'
        Tag = 2
      end>
    Properties.OnChange = rgDictionatyTypePropertiesChange
    ItemIndex = 0
    TabOrder = 0
    Height = 49
    Width = 425
  end
  object hlLink: TcxHyperLinkEdit
    Left = 80
    Top = 80
    Properties.ReadOnly = True
    Properties.SingleClick = True
    Style.BorderStyle = ebsNone
    Style.Color = clBtnFace
    Style.TransparentBorder = False
    TabOrder = 1
    Text = 'http://wiki.services.openoffice.org/wiki/Dictionaries'
    Width = 257
  end
  object btnAdd: TcxButton
    Left = 216
    Top = 256
    Width = 97
    Height = 25
    Caption = 'Add'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object btnCancel: TcxButton
    Left = 336
    Top = 256
    Width = 97
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object beAffFile: TcxButtonEdit
    Left = 96
    Top = 112
    Properties.Buttons = <
      item
        Default = True
        Kind = bkEllipsis
      end>
    Properties.OnButtonClick = beAffFilePropertiesButtonClick
    Properties.OnChange = beAffFilePropertiesChange
    TabOrder = 4
    Width = 337
  end
  object beDicFile: TcxButtonEdit
    Tag = 1
    Left = 96
    Top = 144
    Properties.Buttons = <
      item
        Default = True
        Kind = bkEllipsis
      end>
    Properties.OnButtonClick = beDicFilePropertiesButtonClick
    Properties.OnChange = beAffFilePropertiesChange
    TabOrder = 5
    Width = 337
  end
  object cbLang: TcxComboBox
    Left = 96
    Top = 176
    Properties.DropDownListStyle = lsEditFixedList
    Properties.ImmediatePost = True
    TabOrder = 6
    Width = 337
  end
  object cbCodePage: TcxComboBox
    Left = 96
    Top = 208
    Properties.DropDownListStyle = lsEditFixedList
    Properties.ImmediatePost = True
    TabOrder = 7
    Visible = False
    Width = 337
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 16
    Top = 88
  end
end

object SPLCheckConfFrm: TSPLCheckConfFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Spell Check configuration'
  ClientHeight = 472
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 369
    Height = 442
    ActivePage = TabSheet3
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheet3: TTabSheet
      Caption = 'General'
      ImageIndex = 2
      object Label2: TLabel
        Left = 16
        Top = 13
        Width = 298
        Height = 13
        Caption = 'Single Data File (Including Dictionary,Configuation,Ignore List)'
      end
      object Button3: TButton
        Left = 16
        Top = 32
        Width = 105
        Height = 25
        Caption = 'Load'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Button4: TButton
        Left = 144
        Top = 32
        Width = 105
        Height = 25
        Caption = 'Save'
        TabOrder = 1
        OnClick = Button4Click
      end
      object GroupBox1: TGroupBox
        Left = 3
        Top = 63
        Width = 353
        Height = 150
        Caption = 'Ignore List'
        TabOrder = 2
        object edIgnoreList: TMemo
          Left = 13
          Top = 37
          Width = 161
          Height = 97
          TabOrder = 0
        end
        object Button1: TButton
          Left = 216
          Top = 76
          Width = 89
          Height = 25
          Caption = 'Save'
          TabOrder = 2
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 216
          Top = 35
          Width = 89
          Height = 25
          Caption = 'Load'
          TabOrder = 1
          OnClick = Button2Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 3
        Top = 219
        Width = 355
        Height = 141
        Caption = 'Database and configuration'
        TabOrder = 3
        object Label3: TLabel
          Left = 16
          Top = 23
          Width = 48
          Height = 13
          Caption = 'Dictionary'
        end
        object Label4: TLabel
          Left = 16
          Top = 79
          Width = 65
          Height = 13
          Caption = 'Configuration'
        end
        object Button5: TButton
          Left = 16
          Top = 42
          Width = 105
          Height = 25
          Caption = 'Load'
          TabOrder = 0
          OnClick = Button5Click
        end
        object Button6: TButton
          Left = 144
          Top = 42
          Width = 105
          Height = 25
          Caption = 'Save'
          TabOrder = 1
          OnClick = Button6Click
        end
        object Button7: TButton
          Left = 144
          Top = 98
          Width = 105
          Height = 25
          Caption = 'Save'
          TabOrder = 3
          OnClick = Button7Click
        end
        object Button8: TButton
          Left = 16
          Top = 98
          Width = 105
          Height = 25
          Caption = 'Load'
          TabOrder = 2
          OnClick = Button8Click
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Languages'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object lsLanguages: TListView
        Left = 3
        Top = 3
        Width = 355
        Height = 86
        Checkboxes = True
        Columns = <
          item
            Caption = 'Language'
            Width = 80
          end
          item
            Caption = 'Description'
            MaxWidth = 200
            Width = 200
          end>
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = lsLanguagesClick
        OnSelectItem = lsLanguagesSelectItem
        OnItemChecked = lsLanguagesItemChecked
      end
      object Button15: TButton
        Left = 3
        Top = 95
        Width = 75
        Height = 25
        Caption = 'Move up'
        TabOrder = 3
        OnClick = Button15Click
      end
      object Button16: TButton
        Left = 84
        Top = 94
        Width = 75
        Height = 25
        Caption = 'Move down'
        TabOrder = 4
        OnClick = Button16Click
      end
      object Button17: TButton
        Left = 202
        Top = 95
        Width = 75
        Height = 25
        Hint = 'Add a new language'
        Caption = 'Add'
        TabOrder = 5
        OnClick = Button17Click
      end
      object Button18: TButton
        Left = 283
        Top = 95
        Width = 75
        Height = 25
        Hint = 'Remove selected language'
        Caption = 'Remove'
        TabOrder = 6
        OnClick = Button18Click
      end
      object GroupBox4: TGroupBox
        Left = 2
        Top = 123
        Width = 356
        Height = 99
        Caption = 'General'
        TabOrder = 1
        object Label7: TLabel
          Left = 19
          Top = 24
          Width = 47
          Height = 13
          Caption = 'Language'
        end
        object Label6: TLabel
          Left = 19
          Top = 72
          Width = 53
          Height = 13
          Caption = 'Description'
        end
        object Label1: TLabel
          Left = 19
          Top = 45
          Width = 53
          Height = 13
          Caption = 'Sounds like'
        end
        object Label5: TLabel
          Left = 202
          Top = 45
          Width = 37
          Height = 13
          Caption = 'Process'
        end
        object edLangauge: TComboBox
          Left = 100
          Top = 21
          Width = 145
          Height = 21
          Enabled = False
          TabOrder = 0
          OnExit = edLangaugeExit
          Items.Strings = (
            'Custom'
            'Afrikaans'
            'Albanian'
            'Amharic'
            'Arabic'
            'Armenian'
            'Assamese'
            'AzeriCyrillic'
            'AzeriLatin'
            'Basque'
            'Belarusian'
            'Bengali'
            'Bosnian'
            'Bulgarian'
            'Burmese'
            'Catalan'
            'ChineseSimplified'
            'Croatian'
            'Czech'
            'Danish'
            'Divehi'
            'Dutch'
            'Edo'
            'Edo'
            'English'
            'Estonian'
            'Faroese'
            'Persian'
            'Filipino'
            'Finnish'
            'French'
            'Frisian'
            'FYRO'
            'Gaelic'
            'Galician'
            'Georgian'
            'German'
            'Greek'
            'Guarani'
            'Gujarati'
            'Hebrew'
            'HID'
            'Hindi'
            'Hungarian'
            'Hungarian'
            'Igbo'
            'Indonesian'
            'Italian'
            'Japanese'
            'Kannada'
            'Kashmiri'
            'Kazakh'
            'Khmer'
            'Konkani'
            'Korean'
            'Kyrgyz'
            'Lao'
            'Latin'
            'Latvian'
            'Lithuanian'
            'Malay'
            'Malayalam'
            'Maltese'
            'Maori'
            'Marathi'
            'MongolianStandard'
            'Mongolian'
            'Nepali'
            'Norwegian'
            'Oriya'
            'Polish'
            'Portuguese'
            'Punjabi'
            'Raeto_Romance'
            'Romanian'
            'Russian'
            'Sami'
            'Sanskrit'
            'Serbian'
            'Sesotho'
            'Setsuana'
            'Sindhi'
            'Sinhala'
            'Slovak'
            'Slovenian'
            'Somali'
            'Sorbian'
            'Spanish'
            'Swahili'
            'Swedish'
            'Syriac'
            'Tajik'
            'Tamil'
            'Tatar'
            'Telugu'
            'Thai'
            'Tibetan'
            'Tsonga'
            'Turkish'
            'Turkmen'
            'Ukrainian'
            'Unicode'
            'Urdu'
            'Uzbek'
            'Venda'
            'Vietnamese'
            'Welsh'
            'Xhos'
            'Yiddish'
            'Zulu')
        end
        object edDescription: TEdit
          Left = 78
          Top = 69
          Width = 251
          Height = 21
          Enabled = False
          TabOrder = 3
          OnExit = edDescriptionExit
        end
        object edSoundex: TComboBox
          Left = 100
          Top = 42
          Width = 93
          Height = 21
          Enabled = False
          TabOrder = 1
        end
        object edProcess: TComboBox
          Left = 251
          Top = 42
          Width = 93
          Height = 21
          Style = csDropDownList
          Enabled = False
          TabOrder = 2
          Items.Strings = (
            'Standard'
            'German'
            'French'
            'Spanish'
            'Italian'
            'Dutch')
        end
      end
      object GroupBox3: TGroupBox
        Left = 3
        Top = 228
        Width = 358
        Height = 181
        Caption = 'Words'
        TabOrder = 2
        object Label9: TLabel
          Left = 11
          Top = 139
          Width = 46
          Height = 13
          Caption = 'Affix File:'
        end
        object edWords: TMemo
          Left = 11
          Top = 24
          Width = 161
          Height = 79
          Enabled = False
          TabOrder = 0
        end
        object btSaveWords: TButton
          Left = 192
          Top = 53
          Width = 89
          Height = 25
          Caption = 'Save'
          Enabled = False
          TabOrder = 2
          OnClick = btSaveWordsClick
        end
        object btLoadWords: TButton
          Left = 192
          Top = 22
          Width = 89
          Height = 25
          Caption = 'Load'
          Enabled = False
          TabOrder = 1
          OnClick = btLoadWordsClick
        end
        object edSourceFile: TEdit
          Left = 11
          Top = 116
          Width = 209
          Height = 21
          TabOrder = 3
        end
        object btSourceFile: TButton
          Left = 226
          Top = 114
          Width = 79
          Height = 25
          Caption = 'Source File'
          Enabled = False
          TabOrder = 4
          OnClick = btSourceFileClick
        end
        object edAffix: TEdit
          Left = 11
          Top = 154
          Width = 209
          Height = 21
          TabOrder = 5
        end
        object LbAffix: TButton
          Left = 226
          Top = 150
          Width = 79
          Height = 25
          Caption = 'Source File'
          Enabled = False
          TabOrder = 6
          OnClick = LbAffixClick
        end
      end
    end
  end
  object Panel1: TPanel
    Left = -4
    Top = 433
    Width = 373
    Height = 41
    BevelOuter = bvNone
    TabOrder = 1
    OnClick = Panel1Click
    object edRefresh: TButton
      Left = 9
      Top = 7
      Width = 105
      Height = 25
      Caption = 'Refresh Dictionary'
      TabOrder = 0
      OnClick = edRefreshClick
    end
    object edSave: TButton
      Left = 120
      Top = 7
      Width = 72
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = edSaveClick
    end
    object Button14: TButton
      Left = 289
      Top = 6
      Width = 72
      Height = 25
      Caption = 'OK'
      TabOrder = 2
      OnClick = Button14Click
    end
    object edRetrieve: TButton
      Left = 211
      Top = 7
      Width = 72
      Height = 25
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = edRetrieveClick
    end
  end
  object OpenDialog: TOpenDialog
    Left = 152
    Top = 288
  end
  object SpellCheckerOpen: TOpenDialog
    Filter = 'Spell Checker Config File|*.SPLCFG'
    Left = 272
    Top = 264
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Filter = 
      'Text Files|*.txt|Dictionary Files|*.dic|ISPELL Dictionary Files|' +
      '*.lat'
    Left = 112
    Top = 192
  end
  object OPENSPlx: TOpenDialog
    Filter = 'TMS SpellChec Source|*.splx'
    Left = 56
    Top = 80
  end
  object DictOpen: TOpenDialog
    Filter = 'TMS Dictionary Source|*.SPL'
    Left = 40
    Top = 288
  end
  object SAVESPlx: TSaveDialog
    Filter = 'TMS SpellCheck Source|*.SPLX'
    Left = 328
    Top = 272
  end
  object SaveTextFileDialog1: TSaveTextFileDialog
    Left = 120
    Top = 272
  end
  object SaveDict: TSaveDialog
    Filter = 'TMS Dictionary Source|*.SPL'
    Left = 296
    Top = 320
  end
  object SaveDialog1: TSaveDialog
    Left = 136
    Top = 112
  end
  object SaveConfig: TSaveDialog
    Filter = 'Spell Checker Config File|*.SPLCFG'
    Left = 312
    Top = 72
  end
  object OpenDialog1: TOpenDialog
    Left = 176
    Top = 232
  end
  object OpenAffix: TOpenDialog
    Filter = 'Spell Checker Affix File|*.aff'
    Left = 232
    Top = 328
  end
end

inherited EditorsMaskDemoMainForm: TEditorsMaskDemoMainForm
  Left = 433
  Top = 106
  Caption = 'ExpressEditors MaskDemo'
  ClientHeight = 546
  ClientWidth = 792
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 792
    Height = 32
    Caption = 
      'Practice using masked data-aware editors for reading and writing' +
      '.'#10'Experiment by changing the Options above. Click '#39'About this de' +
      'mo'#39' for more information.'
  end
  object Panel1: TPanel [1]
    Left = 0
    Top = 32
    Width = 189
    Height = 514
    Align = alLeft
    TabOrder = 0
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 187
      Height = 32
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Departments'
      Color = 4707838
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
    end
    object DBGrid1: TDBGrid
      Left = 1
      Top = 33
      Width = 187
      Height = 480
      Align = alClient
      DataSource = EditorsMaskDemoMainDM.DataSourceDEPARTMENTS
      TabOrder = 1
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
      Columns = <
        item
          Expanded = False
          FieldName = 'NAME'
          Visible = True
        end>
    end
  end
  object Panel2: TPanel [2]
    Left = 189
    Top = 32
    Width = 603
    Height = 514
    Align = alClient
    Color = 15451300
    TabOrder = 1
    object lbFirstName: TLabel
      Left = 28
      Top = 56
      Width = 51
      Height = 13
      Caption = 'First name:'
      Transparent = True
    end
    object lbMiddleName: TLabel
      Left = 16
      Top = 84
      Width = 63
      Height = 13
      Caption = 'Middle name:'
      Transparent = True
    end
    object lbLastName: TLabel
      Left = 28
      Top = 112
      Width = 52
      Height = 13
      Caption = 'Last name:'
      Transparent = True
    end
    object lbCountry: TLabel
      Left = 40
      Top = 188
      Width = 39
      Height = 13
      Caption = 'Country:'
      Transparent = True
    end
    object lbPostalCode: TLabel
      Left = 20
      Top = 160
      Width = 59
      Height = 13
      Caption = 'Postal code:'
      Transparent = True
    end
    object lbCity: TLabel
      Left = 60
      Top = 216
      Width = 20
      Height = 13
      Caption = 'City:'
      Transparent = True
    end
    object lbAddress: TLabel
      Left = 44
      Top = 244
      Width = 35
      Height = 13
      Caption = 'Adress:'
      Transparent = True
    end
    object lbPhone: TLabel
      Left = 44
      Top = 292
      Width = 34
      Height = 13
      Caption = 'Phone:'
      Transparent = True
    end
    object lbFax: TLabel
      Left = 56
      Top = 320
      Width = 20
      Height = 13
      Caption = 'Fax:'
      Transparent = True
    end
    object lbEmail: TLabel
      Left = 48
      Top = 396
      Width = 31
      Height = 13
      Caption = 'E-mail:'
      Transparent = True
    end
    object lbHomePage: TLabel
      Left = 20
      Top = 368
      Width = 58
      Height = 13
      Caption = 'Home page:'
      Transparent = True
    end
    object lbInfoPhone: TLabel
      Left = 212
      Top = 292
      Width = 105
      Height = 13
      Caption = 'Delphi Standard Mask'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoFax: TLabel
      Left = 212
      Top = 320
      Width = 229
      Height = 13
      Caption = 'Regular Expression with Auto Complete Function'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoHomePage: TLabel
      Left = 344
      Top = 368
      Width = 229
      Height = 13
      Caption = 'Regular Expression with Auto Complete Function'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoEmail: TLabel
      Left = 344
      Top = 396
      Width = 91
      Height = 13
      Caption = 'Regular Expression'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoPostalCode: TLabel
      Left = 212
      Top = 160
      Width = 229
      Height = 13
      Caption = 'Regular Expression with Auto Complete Function'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoFirstName: TLabel
      Left = 212
      Top = 56
      Width = 42
      Height = 13
      Caption = 'No mask'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoMiddleName: TLabel
      Left = 212
      Top = 84
      Width = 42
      Height = 13
      Caption = 'No mask'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoLastName: TLabel
      Left = 212
      Top = 112
      Width = 42
      Height = 13
      Caption = 'No mask'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoCountry: TLabel
      Left = 212
      Top = 188
      Width = 42
      Height = 13
      Caption = 'No mask'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoCity: TLabel
      Left = 212
      Top = 216
      Width = 42
      Height = 13
      Caption = 'No mask'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object lbInfoAddress: TLabel
      Left = 344
      Top = 244
      Width = 42
      Height = 13
      Caption = 'No mask'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 12937777
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Transparent = True
    end
    object Panel5: TPanel
      Left = 1
      Top = 1
      Width = 601
      Height = 33
      Align = alTop
      BevelInner = bvLowered
      BevelOuter = bvLowered
      Caption = 'Users'
      Color = 4707838
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
    end
    object edtFirstName: TcxDBTextEdit
      Left = 84
      Top = 52
      DataBinding.DataField = 'FNAME'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Properties.ReadOnly = False
      Style.Color = 16247513
      TabOrder = 1
      Width = 121
    end
    object edtMiddleName: TcxDBTextEdit
      Left = 84
      Top = 80
      DataBinding.DataField = 'MNAME'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Properties.ReadOnly = False
      Style.Color = 16247513
      TabOrder = 2
      Width = 121
    end
    object edtLastName: TcxDBTextEdit
      Left = 84
      Top = 108
      DataBinding.DataField = 'LNAME'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Style.Color = 16247513
      TabOrder = 3
      Width = 121
    end
    object edtCountry: TcxDBTextEdit
      Left = 84
      Top = 184
      DataBinding.DataField = 'COUNTRY'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Style.Color = 16247513
      TabOrder = 5
      Width = 121
    end
    object edtCity: TcxDBTextEdit
      Left = 84
      Top = 212
      DataBinding.DataField = 'CITY'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Style.Color = 16247513
      TabOrder = 6
      Width = 121
    end
    object edtAddress: TcxDBTextEdit
      Left = 84
      Top = 240
      DataBinding.DataField = 'ADDRESS'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Style.Color = 16247513
      TabOrder = 7
      Width = 253
    end
    object DBNavigator1: TcxDBNavigator
      Left = 48
      Top = 440
      Width = 272
      Height = 25
      DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      InfoPanel.Font.Charset = DEFAULT_CHARSET
      InfoPanel.Font.Color = 536870912
      InfoPanel.Font.Height = -11
      InfoPanel.Font.Name = 'MS Sans Serif'
      InfoPanel.Font.Style = []
      TabOrder = 12
    end
    object edtPostalCode: TcxDBButtonEdit
      Left = 84
      Top = 156
      DataBinding.DataField = 'POSTALCODE'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
          Visible = False
        end>
      Properties.MaskKind = emkRegExprEx
      Properties.EditMask = '\d\d\d\d\d? | \w\w\w'#39' '#39'\w\w\w'
      Properties.MaxLength = 0
      Properties.OnButtonClick = edtPostalCodePropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 4
      Width = 121
    end
    object edtPhone: TcxDBButtonEdit
      Left = 84
      Top = 288
      DataBinding.DataField = 'PHONE'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
          Visible = False
        end>
      Properties.EditMask = '!\(999\) 000-0000;1;_'
      Properties.MaxLength = 0
      Properties.OnButtonClick = cxDBButtonEdit1PropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 8
      Width = 121
    end
    object edtFax: TcxDBButtonEdit
      Left = 84
      Top = 316
      DataBinding.DataField = 'FAX'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
          Visible = False
        end>
      Properties.MaskKind = emkRegExprEx
      Properties.EditMask = '(\(\d\d\d\)'#39#39' '#39#39')?\d\d\d-\d\d\d\d'
      Properties.MaxLength = 0
      Properties.OnButtonClick = cxDBButtonEdit2PropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 9
      Width = 121
    end
    object edtHomePage: TcxDBButtonEdit
      Left = 84
      Top = 364
      DataBinding.DataField = 'HOMEPAGE'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
          Visible = False
        end>
      Properties.MaskKind = emkRegExprEx
      Properties.EditMask = 'http\:\/\/(\w+(\.\w+)*@)?\w+\.\w+(\.\w+)*(/(\w+(/\w+)*/?)?)?'
      Properties.MaxLength = 0
      Properties.OnButtonClick = cxDBButtonEdit3PropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 10
      Width = 253
    end
    object edtEmail: TcxDBButtonEdit
      Left = 84
      Top = 392
      DataBinding.DataField = 'EMAIL'
      DataBinding.DataSource = EditorsMaskDemoMainDM.DataSourceUSERS
      Properties.Buttons = <
        item
          Default = True
          Kind = bkEllipsis
          Visible = False
        end>
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\w+@\w+\.\w+(\.\w+)*'
      Properties.MaxLength = 0
      Properties.OnButtonClick = cxDBButtonEdit4PropertiesButtonClick
      Style.Color = 16247513
      TabOrder = 11
      Width = 253
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object miDefaultMaskSettings: TMenuItem
        Caption = 'Default &Mask Settings'
        Hint = 'Sets the default mask set'
        OnClick = miDefaultMaskSettingsClick
      end
      object miShowMaskButtons: TMenuItem
        Caption = 'Show Mask &Buttons'
		AutoCheck = True
        Hint = 'Shows the edit mask buttons'
        OnClick = miShowMaskButtonsClick
      end
    end
  end
end

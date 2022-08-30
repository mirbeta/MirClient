object fmCV: TfmCV
  Left = 318
  Top = 95
  AutoScroll = False
  Caption = 'SpellCheckerSimpleDemo - Curriculum Vitae Form'
  ClientHeight = 626
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbPersonal: TcxGroupBox
    Left = 0
    Top = 97
    Align = alTop
    Caption = 'Personal Information'
    TabOrder = 0
    Height = 150
    Width = 729
    object cxLabel8: TcxLabel
      Left = 65
      Top = 117
      Caption = 'Family Status:'
    end
    object cxTextEdit5: TcxTextEdit
      Left = 146
      Top = 117
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 5
      Text = 'Maried, 2 kids'
      Width = 569
    end
    object cxLabel13: TcxLabel
      Left = 101
      Top = 85
      Caption = 'Phone:'
    end
    object cxMaskEdit3: TcxMaskEdit
      Left = 146
      Top = 85
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '(\(\d\d\d\))? \d(\d\d?)? - \d\d - \d\d'
      Properties.MaxLength = 0
      TabOrder = 2
      Text = '111-11-11'
      Width = 99
    end
    object cxLabel14: TcxLabel
      Left = 286
      Top = 85
      Caption = 'Birth Date:'
    end
    object cxDateEdit2: TcxDateEdit
      Left = 345
      Top = 85
      EditValue = 29875d
      TabOrder = 3
      Width = 98
    end
    object cxMaskEdit4: TcxMaskEdit
      Left = 527
      Top = 85
      Properties.MaskKind = emkRegExpr
      Properties.EditMask = '\w+@\w+\.\w+(\.\w+)*'
      Properties.MaxLength = 0
      TabOrder = 4
      Text = 'davolio@mail.com'
      Width = 189
    end
    object cxLabel15: TcxLabel
      Left = 481
      Top = 85
      Caption = 'E-mail:'
    end
    object cxTextEdit6: TcxTextEdit
      Left = 146
      Top = 52
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'USA, Woshington, Seattle, 507-20th Ave. E. Apt. 2A'
      Width = 569
    end
    object edtName: TcxTextEdit
      Left = 146
      Top = 20
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Nancy Davolio'
      Width = 569
    end
    object cxLabel16: TcxLabel
      Left = 98
      Top = 20
      Caption = 'Name:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'MS Sans Serif'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
    end
    object cxLabel17: TcxLabel
      Left = 91
      Top = 52
      Caption = 'Address:'
    end
  end
  object gbProfessional: TcxGroupBox
    Left = 0
    Top = 247
    Align = alClient
    Caption = 'Professional Information'
    TabOrder = 1
    Height = 337
    Width = 729
    object cxLabel6: TcxLabel
      Left = 77
      Top = 20
      Caption = 'Objective:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'MS Sans Serif'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
    end
    object cxTextEdit3: TcxTextEdit
      Left = 146
      Top = 20
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'Sales Reprseentative'
      Width = 569
    end
    object cxLabel7: TcxLabel
      Left = 75
      Top = 52
      Caption = 'Education:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'MS Sans Serif'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
    end
    object cxMemo1: TcxMemo
      Left = 146
      Top = 146
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        'North-West Trading Company 2002 - Presant'
        'Bulk Sales Manager'
          'Developed comprehensive marketing stratgy and aggressivelypursue' +
          'd new business while actively managing existing accounts.'
          'Developed proposals and negotiated sales.'
          '- Increased sales 35% in 2 years'
          '- Introduced first catalog for bulk sales - researched, designed' +
          ', and launchd')
      TabOrder = 2
      Height = 81
      Width = 569
    end
    object cxLabel10: TcxLabel
      Left = 9
      Top = 146
      Caption = 'Experience and Skills:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'MS Sans Serif'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
    end
    object cxLabel11: TcxLabel
      Left = 75
      Top = 241
      Caption = 'Interests:'
      ParentFont = False
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'MS Sans Serif'
      Style.Font.Style = [fsBold]
      Style.IsFontAssigned = True
    end
    object cxRichEdit2: TcxRichEdit
      Left = 146
      Top = 52
      Anchors = [akLeft, akTop, akRight]
      Lines.Strings = (
        
          'Education includes a BA in psyhology from Colorado State Univerc' +
          'ity in in 1970.  She also completed "The Art of the '
        'Cold Call."  Nancy is a member of Toastmasters Internationa.')
      TabOrder = 1
      Height = 81
      Width = 569
    end
    object cxRichEdit1: TcxRichEdit
      Left = 146
      Top = 241
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'Sprot, music, boxing, swiming')
      TabOrder = 3
      Height = 81
      Width = 569
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 584
    Width = 729
    Height = 42
    Align = alBottom
    TabOrder = 2
    object btnCheckSpelling: TcxButton
      Left = 16
      Top = 8
      Width = 122
      Height = 25
      Action = aCheckSpelling
      TabOrder = 0
      CanBeFocused = False
    end
    object cxButton1: TcxButton
      Left = 636
      Top = 8
      Width = 81
      Height = 25
      Action = actExit
      Anchors = [akRight]
      TabOrder = 1
    end
  end
  object cxGroupBox1: TcxGroupBox
    Left = 0
    Top = 0
    Align = alTop
    Caption = 'About'
    TabOrder = 3
    Height = 97
    Width = 729
    object cxLabel1: TcxLabel
      Left = 2
      Top = 18
      Align = alClient
      AutoSize = False
      Caption = 
        'This demo illustrates how to force the TdxSpellChecker component' +
        ' to check text within several controls placed on a control conta' +
        'iner. For this, the TdxSpellChecker.CheckContainer method is inv' +
        'oked, and each control is checked according to its tab order. Pre' +
        'ss F7 or click the "Check Spelling..." button, and the spell che' +
        'cker starts checking the text within all the controls onto the f' +
        'orm.'#10'Note that the spell checker automatically checks entered wo' +
        'rds while you are editing the text in the control, if the Check ' +
        'As You Type checking mode is active. In this mode, use the conte' +
        'xt menu to make suggested corrections, or display the Spelling d' +
        'ialog.'
      ParentFont = False
      Properties.WordWrap = True
      Style.Font.Charset = DEFAULT_CHARSET
      Style.Font.Color = clWindowText
      Style.Font.Height = -11
      Style.Font.Name = 'Tahoma'
      Style.Font.Style = []
      Style.IsFontAssigned = True
      Transparent = True
      Height = 77
      Width = 725
    end
  end
  object dxSpellChecker1: TdxSpellChecker
    AutoCorrectOptions.Active = True
    AutoLoadDictionaries = True
    CheckAsYouTypeOptions.Active = True
    DictionaryItems = <
      item
        DictionaryTypeClassName = 'TdxHunspellDictionary'
        DictionaryType.DictionaryPath = '..\..\Data\en_US.dic'
        DictionaryType.GrammarPath = '..\..\Data\en_US.aff'
        DictionaryType.Language = 1033
      end
      item
        DictionaryTypeClassName = 'TdxUserSpellCheckerDictionary'
        DictionaryType.DictionaryPath = '..\..\Data\User.dic'
      end>
    UseThreadedLoad = True
    OnCheckAsYouTypeStart = dxSpellChecker1CheckAsYouTypeStart
    OnCheckControlInContainer = dxSpellChecker1CheckControlInContainer
    Left = 16
    Top = 64
  end
  object cxLookAndFeelController1: TcxLookAndFeelController
    Left = 48
    Top = 64
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 24
    object File1: TMenuItem
      Caption = '&File'
      object CheckSpelling1: TMenuItem
        Action = aCheckSpelling
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object Options1: TMenuItem
      Caption = '&Options'
      object dfsd1: TMenuItem
        Caption = 'Spelling Form Type'
        object Outlook1: TMenuItem
          Action = aOutlookSpellType
          AutoCheck = True
        end
        object Word1: TMenuItem
          Action = aWordSpellType
          AutoCheck = True
        end
      end
      object Spelling1: TMenuItem
        Caption = 'Spelling'
        object CheckFromCursorPos1: TMenuItem
          Action = aCheckFromCursorPos
          AutoCheck = True
        end
        object CheckSelectedTextFirst1: TMenuItem
          Action = aCheckSelectedTextFirst
          AutoCheck = True
        end
        object IgnoreEmails1: TMenuItem
          Action = aIgnoreEmails
          AutoCheck = True
        end
        object IgnoreMixedCaseWords1: TMenuItem
          Action = aIgnoreMixedCaseWords
          AutoCheck = True
        end
        object IgnoreRepeatedWords1: TMenuItem
          Action = aIgnoreRepeatedWords
          AutoCheck = True
        end
        object IgnoreUppercaseWords1: TMenuItem
          Action = aIgnoreUpperCaseWords
          AutoCheck = True
        end
        object IgnoreURLs1: TMenuItem
          Action = aIgnoreURLs
          AutoCheck = True
        end
        object IgnoreWordsWithNumbers1: TMenuItem
          Action = aIgnoreWordsWithNumbers
          AutoCheck = True
        end
      end
      object aCAYTActive1: TMenuItem
        Action = aCAYTActive
        AutoCheck = True
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object DeveloperExpressProducts1: TMenuItem
        Action = actProducts
      end
      object DeveloperExpressDownloads1: TMenuItem
        Action = actDownloads
      end
      object DeveloperExpressontheWeb1: TMenuItem
        Action = actDXOnTheWeb
      end
      object DevExpressSupportCenter1: TMenuItem
        Action = actSupport
      end
    end
  end
  object alMain: TActionList
    Left = 48
    Top = 24
    object actDownloads: TAction
      Tag = 2
      Category = 'Help'
      Caption = 'Developer Express &Downloads'
      Hint = 
        'Launches the web-page with the list of available downloads of De' +
        'veloper Express products'
      ImageIndex = 1
      OnExecute = actDXOnTheWebExecute
    end
    object actSupport: TAction
      Tag = 4
      Category = 'Help'
      Caption = 'Develooer Express Support &Center'
      Hint = 'Launches the web-page with the Developer Express Support Center'
      ImageIndex = 1
      OnExecute = actDXOnTheWebExecute
    end
    object actDXOnTheWeb: TAction
      Category = 'Help'
      Caption = 'Developer Express on the &Web'
      Hint = 'Launches the official web-site of Developer Express'
      ImageIndex = 0
      OnExecute = actDXOnTheWebExecute
    end
    object actProducts: TAction
      Tag = 3
      Category = 'Help'
      Caption = 'Developer Express &Products'
      Hint = 
        'Launches the web-page with the list of Developer Express product' +
        's'
      ImageIndex = 1
      OnExecute = actDXOnTheWebExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Press to quit the demo-program'
      ShortCut = 32856
      OnExecute = actExitExecute
    end
    object aOutlookSpellType: TAction
      Category = 'SpellingFormType'
      AutoCheck = True
      Caption = 'Outlook'
      Checked = True
      GroupIndex = 3
      OnExecute = aOutlookSpellTypeExecute
    end
    object aWordSpellType: TAction
      Tag = 1
      Category = 'SpellingFormType'
      AutoCheck = True
      Caption = 'Word'
      GroupIndex = 3
      OnExecute = aOutlookSpellTypeExecute
    end
    object aCheckFromCursorPos: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Check From Cursor Pos'
      OnExecute = aCheckFromCursorPosExecute
    end
    object aCheckSelectedTextFirst: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Check Selected Text First'
      Checked = True
      OnExecute = aCheckSelectedTextFirstExecute
    end
    object aIgnoreEmails: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Ignore Emails'
      Checked = True
      OnExecute = aIgnoreEmailsExecute
    end
    object aIgnoreMixedCaseWords: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Ignore Mixed Case Words'
      Checked = True
      OnExecute = aIgnoreMixedCaseWordsExecute
    end
    object aCAYTActive: TAction
      Category = 'CheckAsYouType'
      AutoCheck = True
      Caption = 'Check As You Type'
      Checked = True
      OnExecute = aCAYTActiveExecute
    end
    object aIgnoreRepeatedWords: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Ignore Repeated Words'
      OnExecute = aIgnoreRepeatedWordsExecute
    end
    object aIgnoreUpperCaseWords: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Ignore Upper Case Words'
      Checked = True
      OnExecute = aIgnoreUpperCaseWordsExecute
    end
    object aIgnoreURLs: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Ignore URLs'
      Checked = True
      OnExecute = aIgnoreURLsExecute
    end
    object aIgnoreWordsWithNumbers: TAction
      Category = 'SpellingOptions'
      AutoCheck = True
      Caption = 'Ignore Words With Numbers'
      Checked = True
      OnExecute = aIgnoreWordsWithNumbersExecute
    end
    object aCheckSpelling: TAction
      Category = 'File'
      Caption = 'Check Spelling...'
      ShortCut = 118
      OnExecute = aCheckSpellingExecute
    end
  end
end

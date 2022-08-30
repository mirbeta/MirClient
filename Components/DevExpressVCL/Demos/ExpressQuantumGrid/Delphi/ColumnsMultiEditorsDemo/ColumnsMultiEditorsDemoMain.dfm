inherited ColumnsMultiEditorsDemoMainForm: TColumnsMultiEditorsDemoMainForm
  Left = 338
  Top = 207
  Caption = 'ExpressQuantumGrid ColumnsMultiEditors Demo'
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Height = 32
    Caption = 
      'This demo shows how several editors can be used in a single colu' +
      'mn. Click '#39'About this demo'#39' for more information . '
  end
  object Grid: TcxGrid [1]
    Left = 0
    Top = 32
    Width = 623
    Height = 344
    Align = alClient
    TabOrder = 0
    object tvSkills: TcxGridTableView
      NavigatorButtons.ConfirmDelete = False
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsData.Deleting = False
      OptionsData.Inserting = False
      OptionsView.ColumnAutoWidth = True
      OptionsView.HeaderAutoHeight = True
      object clnName: TcxGridColumn
        Caption = 'Name'
        Visible = False
        GroupIndex = 0
        Options.Editing = False
        Options.Focusing = False
        SortIndex = 0
        SortOrder = soAscending
      end
      object clnSkill: TcxGridColumn
        Caption = 'Skill'
        Options.Editing = False
        Options.Focusing = False
        Width = 50
      end
      object clnGrade: TcxGridColumn
        Caption = 'Grade'
        OnGetProperties = clnGradeGetProperties
        Options.Filtering = False
        Options.Grouping = False
        Options.Sorting = False
        Width = 50
      end
    end
    object lvSkills: TcxGridLevel
      GridView = tvSkills
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miEditButtons: TMenuItem
        Caption = '&Edit Buttons'
        object miEditButtonsNever: TMenuItem
          AutoCheck = True
          Caption = '&Never'
          Checked = True
          Hint = 'The editor buttons are displayed only for the focused column'
          RadioItem = True
          OnClick = miEditButtonsNeverClick
        end
        object miEditButtonsForFocusedRecord: TMenuItem
          AutoCheck = True
          Caption = 'For &Focused Record'
          Hint = 'The editor buttons are visible for the focused record'
          RadioItem = True
          OnClick = miEditButtonsFocusedRecordClick
        end
        object miEditButtonsAlways: TMenuItem
          AutoCheck = True
          Caption = '&Always'
          Hint = 'The editor buttons are always visible within the current view'
          RadioItem = True
          OnClick = miEditButtonsAlwaysClick
        end
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
  end
  object EditRepository: TcxEditRepository
    Left = 56
    Top = 16
    object ImageComboLanguages: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'Assembler'
          ImageIndex = 0
          Value = 0
        end
        item
          Description = 'Delphi'
          ImageIndex = 1
          Value = 1
        end
        item
          Description = 'Visual Basic'
          ImageIndex = 2
          Value = 2
        end
        item
          Description = 'C++'
          ImageIndex = 3
          Value = 3
        end
        item
          Description = 'Java'
          ImageIndex = 4
          Value = 4
        end>
    end
    object ImageComboCommunication: TcxEditRepositoryImageComboBoxItem
      Properties.Items = <
        item
          Description = 'Bad'
          ImageIndex = 0
          Value = 0
        end
        item
          Description = 'Average'
          ImageIndex = 1
          Value = 1
        end
        item
          Description = 'Good'
          ImageIndex = 2
          Value = 2
        end
        item
          Description = 'Excellent'
          ImageIndex = 3
          Value = 3
        end>
    end
    object SpinItemYears: TcxEditRepositorySpinItem
      Properties.MaxValue = 30.000000000000000000
      Properties.MinValue = 1.000000000000000000
    end
    object DateItemStartWorkFrom: TcxEditRepositoryDateItem
      Properties.OnGetDayOfWeekState = DateItemStartWorkFromPropertiesGetDayOfWeekState
    end
  end
end

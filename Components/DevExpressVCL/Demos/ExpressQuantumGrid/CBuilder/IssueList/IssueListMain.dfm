inherited IssueListMainForm: TIssueListMainForm
  Left = 271
  Top = 121
  Caption = 'Issue List'
  ClientHeight = 585
  ClientWidth = 864
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 864
    Visible = False
  end
  inherited sbMain: TStatusBar
    Top = 566
    Width = 864
  end
  inherited mmMain: TMainMenu
    Images = dmMain.ilMain
    object miView: TMenuItem [1]
      Caption = '&View'
      object miShowDescription: TMenuItem
        Action = actShowDescription
        AutoCheck = True
      end
      object miGridPictures: TMenuItem
        Action = actShowPictures
        AutoCheck = True
      end
      object miShowDepentOnData: TMenuItem
        Action = actShowDependsOnData
        AutoCheck = True
      end
      object miEditorsShadow: TMenuItem
        Action = actEditorsShadow
        AutoCheck = True
      end
      object miSeparator1: TMenuItem
        Caption = '-'
      end
      object miGoProjects: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = '&Projects'
        Checked = True
        Hint = 'Press to show information on projects'
        RadioItem = True
        OnClick = actGoProjectExecute
      end
      object miGoProjectItems: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Project &Items'
        Hint = 'Press to show information on issues list'
        RadioItem = True
        OnClick = actGoProjectExecute
      end
      object miGoDepartments: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = '&Departments'
        Hint = 'Press to show the list of departments'
        RadioItem = True
        OnClick = actGoProjectExecute
      end
      object miGoTeams: TMenuItem
        Tag = 4
        AutoCheck = True
        Caption = 'Projects &Teams'
        Hint = 'Press to show information on the users involved in a project'
        RadioItem = True
        OnClick = actGoProjectExecute
      end
      object miGoUsers: TMenuItem
        Tag = 5
        AutoCheck = True
        Caption = '&Users'
        Hint = 'Press to show the user list'
        RadioItem = True
        OnClick = actGoProjectExecute
      end
      object miGoSchedule: TMenuItem
        Tag = 6
        AutoCheck = True
        Caption = 'Schedule'
        Hint = 'Press to show the schedule of the users involved in a project'
        RadioItem = True
        OnClick = actGoProjectExecute
      end
    end
    object miGridOptions: TMenuItem [2]
      Caption = 'Grid&Options'
      object miGridActions: TMenuItem
        Caption = '&Actions'
        object miFullExpand: TMenuItem
          Action = actFullExpand
        end
        object miFullCollapse: TMenuItem
          Action = actFullCollapse
        end
        object miSeparator2: TMenuItem
          Caption = '-'
        end
        object miColumnCustomization: TMenuItem
          Action = actColumnsCustomize
        end
        object miBestFitallcolumns: TMenuItem
          Action = actBestFit
        end
      end
      object miGridView: TMenuItem
        Caption = '&View'
        object miGrouping: TMenuItem
          Action = actGrouping
          AutoCheck = True
        end
        object miacIndicator: TMenuItem
          Action = acIndicator
          AutoCheck = True
        end
        object miHeaders: TMenuItem
          Action = actHeader
          AutoCheck = True
        end
        object miSummaryFooter: TMenuItem
          Action = actSummaryFooter
          AutoCheck = True
        end
        object miShowGrid: TMenuItem
          Action = actGridLines
          AutoCheck = True
        end
        object miSeparator3: TMenuItem
          Caption = '-'
        end
        object mitAutoWidth: TMenuItem
          Action = actAutoWidth
          AutoCheck = True
        end
        object miAlwaysDisplayButtons: TMenuItem
          Action = actShowEditButtons
          AutoCheck = True
        end
        object miInvertSelected: TMenuItem
          Action = actInvertSelected
          AutoCheck = True
        end
        object actNewItemRow1: TMenuItem
          Action = actNewItemRow
          AutoCheck = True
        end
      end
      object miSeparator8: TMenuItem
        Caption = '-'
      end
      object actSelectStyleSheet1: TMenuItem
        Action = actSelectStyleSheet
        Caption = 'Predefined StyleSheets...'
        Hint = 
          'Press to select the predefined style sheet to change the grid'#39's ' +
          'display'
      end
    end
  end
  inherited StyleRepository: TcxStyleRepository
    inherited GridTableViewStyleSheetDevExpress: TcxGridTableViewStyleSheet
      BuiltIn = True
    end
    inherited GridCardViewStyleSheetDevExpress: TcxGridCardViewStyleSheet
      BuiltIn = True
    end
  end
  object alMain: TActionList
    Images = dmMain.ilMain
    Left = 40
    Top = 48
    object actShowDescription: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show &Description'
      Checked = True
      Hint = 
        'Press to display/hide the description panel with the currently d' +
        'isplayed information'
      OnExecute = actShowDescriptionExecute
    end
    object actColumnsCustomize: TAction
      Category = 'Actions'
      Caption = 'RunTime &Column Customization'
      Hint = 'Press to customize columns'
      ImageIndex = 24
      OnExecute = actColumnsCustomizeExecute
    end
    object actShowPictures: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Show &Pictures In A Grid'
      Hint = 
        'Press to activate/deactivate the display of images within grid c' +
        'ells'
      OnExecute = actShowPicturesExecute
    end
    object actGrouping: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Show &GroupBox'
      Checked = True
      Hint = 
        'Dispaly the group panel containing column headers used for group' +
        'ing'
      ImageIndex = 5
      OnExecute = actGroupingExecute
      OnUpdate = actGroupingUpdate
    end
    object acIndicator: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Show &Indicator'
      Hint = 'Display the grid view indicator specifying the record state'
      ImageIndex = 6
      OnExecute = acIndicatorExecute
      OnUpdate = acIndicatorUpdate
    end
    object actSummaryFooter: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Show Summary &Footer'
      Hint = 'Display the summary footer at the bottom of the grid view'
      ImageIndex = 7
      OnExecute = actSummaryFooterExecute
      OnUpdate = actSummaryFooterUpdate
    end
    object actHeader: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Show &Header'
      Hint = 'Display header for all grid columns'
      ImageIndex = 8
      OnExecute = actHeaderExecute
      OnUpdate = actHeaderUpdate
    end
    object actAutoWidth: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Columns Auto&Width'
      Checked = True
      Hint = 
        'Automatically sets the width of columns to the width of a grid v' +
        'iew'
      ImageIndex = 19
      OnExecute = actAutoWidthExecute
      OnUpdate = actAutoWidthUpdate
    end
    object actInvertSelected: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'In&vert Selected'
      Checked = True
      Hint = 'Display the grid view indicator specifying the record state'
      ImageIndex = 23
      OnExecute = actInvertSelectedExecute
      OnUpdate = actInvertSelectedUpdate
    end
    object actFullExpand: TAction
      Category = 'Actions'
      Caption = 'Full &Expand'
      Hint = 'Press to expand all records'
      ImageIndex = 27
      OnExecute = actFullExpandExecute
    end
    object actFullCollapse: TAction
      Category = 'Actions'
      Caption = 'Full &Collapse'
      Hint = 'Press to collapse all records'
      ImageIndex = 26
      OnExecute = actFullCollapseExecute
    end
    object actBestFit: TAction
      Category = 'Actions'
      Caption = 'Best Fit (all columns)'
      Hint = 'Changes the width of columns to best fit inside a grid view'
      OnExecute = actBestFitExecute
    end
    object actShowEditButtons: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Show Edit &Buttons'
      Hint = 'Displays/hides editor buttons for the current record'
      ImageIndex = 22
      OnExecute = actShowEditButtonsExecute
      OnUpdate = actShowEditButtonsUpdate
    end
    object actGridLines: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Show Grid &Lines'
      Checked = True
      Hint = 'Displays/hides horizontal and vertical grid lines'
      ImageIndex = 20
      OnExecute = actGridLinesExecute
      OnUpdate = actGridLinesUpdate
    end
    object actAutoPreview: TAction
      Category = 'GridOptions'
      Caption = 'Auto&Preview'
      Checked = True
      Hint = 'Display the preview section for each row'
      ImageIndex = 21
      OnExecute = actAutoPreviewExecute
      OnUpdate = actAutoPreviewUpdate
    end
    object actShowDependsOnData: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Depends On Data'
      Checked = True
      Hint = 'Press to activate/deactivate drawing depending on data'
      OnExecute = actShowDependsOnDataExecute
    end
    object actNewItemRow: TAction
      Category = 'GridOptions'
      AutoCheck = True
      Caption = 'Show New Item Row'
      Hint = 'Press to display an empty row to enter new values'
      OnExecute = actNewItemRowExecute
      OnUpdate = actNewItemRowUpdate
    end
    object actSelectStyleSheet: TAction
      Category = 'GridOptions'
      Caption = 'Select Predefined StyleSheet...'
      OnExecute = actSelectStyleSheetExecute
    end
    object actEditorsShadow: TAction
      Category = 'Style'
      AutoCheck = True
      Caption = '&Editors Shadow'
      Checked = True
      Hint = 
        'Setting this option activates/deactivates the shadow for stand-a' +
        'lone editors'
      OnExecute = actEditorsShadowExecute
    end
  end
end

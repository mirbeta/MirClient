inherited SimpleTreeDemoMainForm: TSimpleTreeDemoMainForm
  Left = 269
  Top = 131
  Caption = 'ExpressQuantumTreeList SimpleTreeDemo'
  ClientWidth = 578
  OldCreateOrder = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lscrip: TLabel
    Width = 578
    Caption = 
      'Experiment by changing the Options above and also see'#10#13'Help/Abou' +
      't for other things to try.'
  end
  inherited sbMain: TStatusBar
    Width = 578
  end
  object cxDBTreeList: TcxDBTreeList [2]
    Left = 0
    Top = 32
    Width = 578
    Height = 361
    Align = alClient
    Bands = <
      item
        Caption.Text = 'Band + 1'
        Width = 600
      end>
    DataController.DataSource = SimpleTreeDemoDataDM.dsDepartments
    DataController.ParentField = 'PARENTID'
    DataController.KeyField = 'ID'
    OptionsData.Inserting = True
    OptionsView.GridLineColor = 14916958
    OptionsView.GridLines = tlglBoth
    OptionsView.Indicator = True
    PopupMenu = mnuNodeOptions
    PopupMenus.ColumnHeaderMenu.UseBuiltInMenu = True
    Preview.Column = cxDBTreeListNAME
    Preview.Place = tlppTop
    Preview.Visible = True
    RootValue = -1
    Styles.StyleSheet = SimpleTreeDemoDataDM.TreeListStyleSheetDevExpress
    TabOrder = 1
    OnInitInsertingRecord = cxDBTreeListInitInsertingRecord
    object cxDBTreeListID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'ID'
      Position.ColIndex = 6
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListPARENTID: TcxDBTreeListColumn
      Visible = False
      DataBinding.FieldName = 'PARENTID'
      Position.ColIndex = 7
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListNAME: TcxDBTreeListColumn
      DataBinding.FieldName = 'NAME'
      Width = 183
      Position.ColIndex = 5
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListBUDGET: TcxDBTreeListColumn
      DataBinding.FieldName = 'BUDGET'
      Width = 120
      Position.ColIndex = 0
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListPHONE: TcxDBTreeListColumn
      DataBinding.FieldName = 'PHONE'
      Width = 120
      Position.ColIndex = 1
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListFAX: TcxDBTreeListColumn
      DataBinding.FieldName = 'FAX'
      Width = 120
      Position.ColIndex = 2
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListEMAIL: TcxDBTreeListColumn
      DataBinding.FieldName = 'EMAIL'
      Width = 120
      Position.ColIndex = 3
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
    object cxDBTreeListVACANCY: TcxDBTreeListColumn
      DataBinding.FieldName = 'VACANCY'
      Width = 120
      Position.ColIndex = 4
      Position.RowIndex = 0
      Position.BandIndex = 0
      Summary.FooterSummaryItems = <>
      Summary.GroupFooterSummaryItems = <>
    end
  end
  inherited mmMain: TMainMenu
    inherited miOptions: TMenuItem
      object miFullCollapse: TMenuItem [0]
        Caption = 'Full &Collapse'
        Hint = 'Collapses all nodes within a tree list control'
        OnClick = miFullCollapseClick
      end
      object miFullExpand: TMenuItem [1]
        Caption = 'Full &Expand'
        Hint = 'Expands all nodes within a tree list control'
        OnClick = miFullExpandClick
      end
      object N1: TMenuItem [2]
        Caption = '-'
      end
      object miColumnCustomization: TMenuItem [3]
        Caption = 'Column &Customization ...'
        Hint = 'Shows the column customization form'
        OnClick = miColumnCustomizationClick
      end
      object miPreview: TMenuItem [4]
        Caption = 'Show &Preview Row'
        Checked = True
        Hint = 'Displays the department name as a preview row'
        OnClick = miPreviewClick
      end
      object miOptionsView: TMenuItem [5]
        Caption = 'Options &View'
        object miShowRoot: TMenuItem
          Caption = '&Root'
          Checked = True
          Hint = 
            'Displays the '#39'+'#39' button to the left of the first node as a root ' +
            'of all nodes.'
          OnClick = miShowRootClick
        end
        object miHeaders: TMenuItem
          Caption = '&Headers'
          Checked = True
          Hint = 'Displays headers of a tree list control'
          OnClick = miHeadersClick
        end
        object miIndicator: TMenuItem
          Caption = '&Indicator'
          Checked = True
          Hint = 'Shows indicators on the left side of tree list nodes'
          OnClick = miIndicatorClick
        end
        object miButtons: TMenuItem
          Caption = '&Buttons'
          Checked = True
          Hint = 'Displays buttons to the left of each node with children'
          OnClick = miButtonsClick
        end
      end
      object miSeparator1: TMenuItem [6]
        Caption = '-'
      end
    end
  end
  object mnuNodeOptions: TPopupMenu
    OnPopup = mnuNodeOptionsPopup
    Left = 88
    Top = 8
    object miNodeDelete: TMenuItem
      Caption = '&Delete '
      Hint = 'Deletes selected node'
      OnClick = miNodeDeleteClick
    end
    object miNodeAdd: TMenuItem
      Caption = '&Add '
      Hint = 'Adds sibling of selected node'
      OnClick = miNodeAddClick
    end
    object miNodeAddChild: TMenuItem
      Caption = 'Add &Child'
      Hint = 'Adds child to selceted node'
      OnClick = miNodeAddChildClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miExpand: TMenuItem
      Caption = '&Expand recursively'
      Hint = 'Expands selected node and all its childs'
      OnClick = miExpandClick
    end
    object miCollapse: TMenuItem
      Caption = '&Collapse recursively'
      Hint = 'Collapses selected node and all its childs'
      OnClick = miCollapseClick
    end
  end
end

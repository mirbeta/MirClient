inherited ColumnsShareDemoMainForm: TColumnsShareDemoMainForm
  Left = 22
  Top = 77
  Caption = 'ExpressQuantumGrid Columns Share Demo'
  ClientHeight = 600
  ClientWidth = 977
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 977
    Caption = 
      'This demo shows how several columns use a single lookup editor' +
      '. Click '#39'About this demo'#39' for more information.'
  end
  object Grid: TcxGrid [1]
    Left = 0
    Top = 16
    Width = 977
    Height = 584
    Align = alClient
    TabOrder = 0
    RootLevelOptions.DetailTabsPosition = dtpTop
    object tvProjects: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = ColumnsShareDemoMainDM.dsProjects
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object tvProjectsNAME: TcxGridDBColumn
        Caption = 'Name'
        DataBinding.FieldName = 'NAME'
      end
      object tvProjectsMANAGERID: TcxGridDBColumn
        Caption = 'Manager'
        DataBinding.FieldName = 'MANAGERID'
        RepositoryItem = eriLookupComboUsers
      end
    end
    object tvItems: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = ColumnsShareDemoMainDM.dsItems
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CellAutoHeight = True
      OptionsView.Indicator = True
      Preview.Column = tvItemsDESCRIPTION
      Preview.Visible = True
      object tvItemsNAME: TcxGridDBColumn
        Caption = 'Name'
        DataBinding.FieldName = 'NAME'
      end
      object tvItemsTYPE: TcxGridDBColumn
        Caption = 'Type'
        DataBinding.FieldName = 'TYPE'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = ColumnsShareDemoMainDM.imStat
        Properties.Items = <
          item
            Description = 'Bug'
            ImageIndex = 2
            Value = False
          end
          item
            Description = 'Request'
            ImageIndex = 3
            Value = True
          end>
        Properties.LargeImages = ColumnsShareDemoMainDM.imStat
        MinWidth = 50
      end
      object tvItemsPROJECTID: TcxGridDBColumn
        Caption = 'Project'
        DataBinding.FieldName = 'PROJECTID'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = ColumnsShareDemoMainDM.dsProjects
        SortIndex = 0
        SortOrder = soAscending
      end
      object tvItemsPRIORITY: TcxGridDBColumn
        Caption = 'Priority'
        DataBinding.FieldName = 'PRIORITY'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Alignment.Horz = taLeftJustify
        Properties.Images = ColumnsShareDemoMainDM.imStat
        Properties.Items = <
          item
            Description = 'Low'
            ImageIndex = 0
            Value = 1
          end
          item
            Description = 'Normal'
            Value = 2
          end
          item
            Description = 'High'
            ImageIndex = 1
            Value = 3
          end>
        MinWidth = 50
      end
      object tvItemsSTATUS: TcxGridDBColumn
        Caption = 'Status'
        DataBinding.FieldName = 'STATUS'
        PropertiesClassName = 'TcxImageComboBoxProperties'
        Properties.Images = ColumnsShareDemoMainDM.imStat
        Properties.Items = <
          item
            Description = 'New'
            ImageIndex = 4
            Value = 1
          end
          item
            Description = 'Postponed'
            ImageIndex = 5
            Value = 2
          end
          item
            Description = 'Fixed'
            ImageIndex = 6
            Value = 3
          end
          item
            Description = 'Rejected'
            ImageIndex = 7
            Value = 4
          end>
      end
      object tvItemsCREATORID: TcxGridDBColumn
        Caption = 'Creator'
        DataBinding.FieldName = 'CREATORID'
        RepositoryItem = eriLookupComboUsers
        Width = 101
      end
      object tvItemsCREATEDDATE: TcxGridDBColumn
        Caption = 'Create Date'
        DataBinding.FieldName = 'CREATEDDATE'
      end
      object tvItemsOWNERID: TcxGridDBColumn
        Caption = 'Owner'
        DataBinding.FieldName = 'OWNERID'
        RepositoryItem = eriLookupComboUsers
      end
      object tvItemsLASTMODIFIEDDATE: TcxGridDBColumn
        Caption = 'Last Modified Date'
        DataBinding.FieldName = 'LASTMODIFIEDDATE'
      end
      object tvItemsFIXEDDATE: TcxGridDBColumn
        Caption = 'Fixed Date'
        DataBinding.FieldName = 'FIXEDDATE'
      end
      object tvItemsDESCRIPTION: TcxGridDBColumn
        Caption = 'Description'
        DataBinding.FieldName = 'DESCRIPTION'
      end
    end
    object cvUsers: TcxGridDBCardView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = ColumnsShareDemoMainDM.dsUsers
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.CardIndent = 7
      OptionsView.SeparatorColor = 12937777
      object cvUsersFNAME: TcxGridDBCardViewRow
        Caption = 'First Name'
        DataBinding.FieldName = 'FNAME'
        Position.BeginsLayer = True
      end
      object cvUsersMNAME: TcxGridDBCardViewRow
        Caption = 'Midle Name'
        DataBinding.FieldName = 'MNAME'
        Position.BeginsLayer = True
      end
      object cvUsersLNAME: TcxGridDBCardViewRow
        Caption = 'Last Name'
        DataBinding.FieldName = 'LNAME'
        Position.BeginsLayer = True
      end
      object cvUsersDepartment: TcxGridDBCardViewRow
        DataBinding.FieldName = 'Department'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'NAME'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Position.BeginsLayer = True
      end
      object cvUsersCOUNTRY: TcxGridDBCardViewRow
        Caption = 'Country'
        DataBinding.FieldName = 'COUNTRY'
        Position.BeginsLayer = True
      end
      object cvUsersPOSTALCODE: TcxGridDBCardViewRow
        Caption = 'PostalCode'
        DataBinding.FieldName = 'POSTALCODE'
        Position.BeginsLayer = True
      end
      object cvUsersCITY: TcxGridDBCardViewRow
        Caption = 'City'
        DataBinding.FieldName = 'CITY'
        Position.BeginsLayer = True
      end
      object cvUsersADDRESS: TcxGridDBCardViewRow
        Caption = 'Address'
        DataBinding.FieldName = 'ADDRESS'
        Position.BeginsLayer = True
      end
      object cvUsersPHONE: TcxGridDBCardViewRow
        Caption = 'Phone'
        DataBinding.FieldName = 'PHONE'
        PropertiesClassName = 'TcxMaskEditProperties'
        Properties.MaskKind = emkRegExprEx
        Properties.EditMask = '(\(\d\d\d\))? \d(\d\d?)? - (\d\d\d? - \d\d|\d\d\d\d)'
        Position.BeginsLayer = True
      end
      object cvUsersFAX: TcxGridDBCardViewRow
        Caption = 'Fax'
        DataBinding.FieldName = 'FAX'
        PropertiesClassName = 'TcxMaskEditProperties'
        Properties.MaskKind = emkRegExprEx
        Properties.EditMask = '(\(\d\d\d\))? \d(\d\d?)? - (\d\d - \d\d | \d\d\d\d)'
        Position.BeginsLayer = True
      end
      object cvUsersEMAIL: TcxGridDBCardViewRow
        Caption = 'EMail'
        DataBinding.FieldName = 'EMAIL'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Position.BeginsLayer = True
      end
      object cvUsersHOMEPAGE: TcxGridDBCardViewRow
        Caption = 'HomePage'
        DataBinding.FieldName = 'HOMEPAGE'
        PropertiesClassName = 'TcxHyperLinkEditProperties'
        Position.BeginsLayer = True
      end
    end
    object tvTeam: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = ColumnsShareDemoMainDM.dsProjectTeam
      DataController.DetailKeyFieldNames = 'PROJECTID'
      DataController.KeyFieldNames = 'ID'
      DataController.MasterKeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsView.ColumnAutoWidth = True
      OptionsView.Indicator = True
      object tvTeamUSERID: TcxGridDBColumn
        Caption = 'Name'
        DataBinding.FieldName = 'USERID'
        RepositoryItem = eriLookupComboUsers
      end
      object tvTeamFUNCTION: TcxGridDBColumn
        Caption = 'Function'
        DataBinding.FieldName = 'FUNCTION'
      end
    end
    object lvItems: TcxGridLevel
      Caption = 'Items'
      GridView = tvItems
    end
    object lvProjects: TcxGridLevel
      Caption = 'Projects'
      GridView = tvProjects
      object lvTeam: TcxGridLevel
        GridView = tvTeam
      end
    end
    object lvUsers: TcxGridLevel
      Caption = 'Users'
      GridView = cvUsers
    end
  end
  inherited mmMain: TMainMenu
    object miOptions: TMenuItem [1]
      Caption = '&Options'
      object miPersonEditor: TMenuItem
        Caption = 'Persons Editor'
        object miLookupEditor: TMenuItem
          AutoCheck = True
          Caption = 'Lookup ComboBox Editor'
          Checked = True
          GroupIndex = 1
          RadioItem = True
          OnClick = miLookUpEditorClick
        end
        object miExtLookUpEditor: TMenuItem
          AutoCheck = True
          Caption = 'ExtLookup ComboBox Editor'
          GroupIndex = 1
          RadioItem = True
          OnClick = miExtLookUpEditorClick
        end
      end
      object miCustomizePersonsLookupCombobox: TMenuItem
        Caption = '&Customize Persons Lookup Combobox ...'
        Hint = 'These changes will affect all Persons lookup combo boxes'
        OnClick = miCustomizePersonsLookupComboboxClick
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
  object cxEditRepository: TcxEditRepository
    Left = 536
    Top = 16
    object erExtLookupComboBoxItem: TcxEditRepositoryExtLookupComboBoxItem
      Properties.DropDownSizeable = True
      Properties.View = cxGridViewRepositoryDBTableView
      Properties.KeyFieldNames = 'ID'
      Properties.ListFieldItem = cxGridViewRepositoryDBTableViewUserName
    end
    object eriLookupComboUsers: TcxEditRepositoryLookupComboBoxItem
      Properties.DropDownAutoSize = True
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'UserName'
        end>
      Properties.ListOptions.GridLines = glNone
      Properties.ListOptions.ShowHeader = False
      Properties.ListSource = ColumnsShareDemoMainDM.dsUsers
    end
  end
  object cxGridPopupMenu1: TcxGridPopupMenu
    PopupMenus = <>
    Left = 352
    Top = 16
  end
  object cxGridViewRepository: TcxGridViewRepository
    Left = 384
    Top = 16
    object cxGridViewRepositoryDBTableView: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = ColumnsShareDemoMainDM.dsUsers
      DataController.KeyFieldNames = 'ID'
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      object cxGridViewRepositoryDBTableViewID: TcxGridDBColumn
        DataBinding.FieldName = 'ID'
        Width = 51
      end
      object cxGridViewRepositoryDBTableViewUserName: TcxGridDBColumn
        DataBinding.FieldName = 'UserName'
      end
      object cxGridViewRepositoryDBTableViewDepartment: TcxGridDBColumn
        DataBinding.FieldName = 'Department'
      end
      object cxGridViewRepositoryDBTableViewCOUNTRY: TcxGridDBColumn
        Caption = 'Country'
        DataBinding.FieldName = 'COUNTRY'
        Width = 69
      end
      object cxGridViewRepositoryDBTableViewPOSTALCODE: TcxGridDBColumn
        Caption = 'Postal Code'
        DataBinding.FieldName = 'POSTALCODE'
        Width = 92
      end
      object cxGridViewRepositoryDBTableViewCITY: TcxGridDBColumn
        Caption = 'City'
        DataBinding.FieldName = 'CITY'
      end
      object cxGridViewRepositoryDBTableViewADDRESS: TcxGridDBColumn
        Caption = 'Address'
        DataBinding.FieldName = 'ADDRESS'
        Width = 158
      end
      object cxGridViewRepositoryDBTableViewPHONE: TcxGridDBColumn
        Caption = 'Phone'
        DataBinding.FieldName = 'PHONE'
        Width = 95
      end
      object cxGridViewRepositoryDBTableViewFAX: TcxGridDBColumn
        Caption = 'Fax'
        DataBinding.FieldName = 'FAX'
        Width = 103
      end
      object cxGridViewRepositoryDBTableViewEMAIL: TcxGridDBColumn
        Caption = 'Email'
        DataBinding.FieldName = 'EMAIL'
        Width = 190
      end
      object cxGridViewRepositoryDBTableViewHOMEPAGE: TcxGridDBColumn
        Caption = 'Homepage'
        DataBinding.FieldName = 'HOMEPAGE'
        Width = 201
      end
    end
  end
end

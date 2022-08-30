inherited EditorsLookupDemoMainForm: TEditorsLookupDemoMainForm
  Left = 269
  Top = 185
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ExpressEditors LookupDemo'
  ClientHeight = 395
  ClientWidth = 546
  Color = 15451300
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 546
    Height = 48
    Caption = 
      'Practice using lookup data-aware editors for reading and writing' +
      '.'#10#13'See also the Storage menu to provide runtime store/restore de' +
      'mo settings.'#10#13'Click '#39'About this demo'#39' for more information.'
  end
  object pnlEditors: TPanel [1]
    Left = 0
    Top = 48
    Width = 546
    Height = 347
    Align = alClient
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 0
    object gbIssue: TcxGroupBox
      Left = 8
      Top = 72
      Caption = 'Issue'
      TabOrder = 1
      Height = 209
      Width = 329
      object Label1: TcxLabel
        Left = 8
        Top = 26
        Caption = 'Name:'
        Transparent = True
      end
      object Label2: TcxLabel
        Left = 9
        Top = 69
        Caption = 'Description:'
        Transparent = True
      end
      object Label9: TcxLabel
        Left = 8
        Top = 48
        Caption = 'Issue Creator:'
        Transparent = True
      end
      object edName: TcxDBTextEdit
        Left = 80
        Top = 20
        DataBinding.DataField = 'NAME'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 0
        Width = 241
      end
      object meDescription: TcxDBMemo
        Left = 8
        Top = 88
        DataBinding.DataField = 'DESCRIPTION'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Properties.ScrollBars = ssVertical
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 2
        Height = 109
        Width = 313
      end
      object lcbCreator: TcxDBLookupComboBox
        Left = 80
        Top = 43
        DataBinding.DataField = 'CREATORID'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Properties.DropDownListStyle = lsEditList
        Properties.ImmediateDropDownWhenKeyPressed = False
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'UserName'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = EditorsLookupDemoDataDM.dsUsers
        Properties.MaxLength = 50
        Properties.OnNewLookupDisplayText = lcbCreatorNewLookupDisplayText
        Style.Color = 16247513
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 1
        Width = 241
      end
    end
    object gbInfo: TcxGroupBox
      Left = 344
      Top = 72
      Caption = 'Info'
      TabOrder = 2
      Height = 101
      Width = 193
      object Label3: TcxLabel
        Left = 8
        Top = 26
        Caption = 'Priority:'
        Transparent = True
      end
      object Label4: TcxLabel
        Left = 8
        Top = 49
        Caption = 'Created:'
        Transparent = True
      end
      object cbPriority: TcxDBImageComboBox
        Left = 64
        Top = 20
        DataBinding.DataField = 'PRIORITY'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Properties.Images = EditorsLookupDemoDataDM.imStat
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
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 0
        Width = 121
      end
      object deCreateDate: TcxDBDateEdit
        Left = 64
        Top = 43
        DataBinding.DataField = 'CREATEDDATE'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 1
        Width = 121
      end
      object chbRequest: TcxDBCheckBox
        Left = 8
        Top = 66
        Caption = 'Request'
        DataBinding.DataField = 'TYPE'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        TabOrder = 2
        Transparent = True
        Width = 121
      end
    end
    object gbStatus: TcxGroupBox
      Left = 344
      Top = 179
      Caption = 'Status'
      TabOrder = 3
      Height = 102
      Width = 193
      object Label5: TcxLabel
        Left = 8
        Top = 27
        Caption = 'Status:'
        Transparent = True
      end
      object Label6: TcxLabel
        Left = 8
        Top = 50
        Caption = 'Modified:'
        Transparent = True
      end
      object Label7: TcxLabel
        Left = 8
        Top = 74
        Caption = 'Fixed:'
        Transparent = True
      end
      object cbStatus: TcxDBImageComboBox
        Left = 64
        Top = 21
        DataBinding.DataField = 'STATUS'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Properties.Images = EditorsLookupDemoDataDM.imStat
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
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 0
        Width = 121
      end
      object deLastModifiedDate: TcxDBDateEdit
        Left = 64
        Top = 45
        DataBinding.DataField = 'LASTMODIFIEDDATE'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 1
        Width = 121
      end
      object deFixedDate: TcxDBDateEdit
        Left = 64
        Top = 69
        DataBinding.DataField = 'FIXEDDATE'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 2
        Width = 121
      end
    end
    object DBNavigator: TcxDBNavigator
      Left = 8
      Top = 288
      Width = 272
      Height = 25
      Buttons.PageSize = 10
      DataSource = EditorsLookupDemoDataDM.dsItems
      InfoPanel.Font.Charset = DEFAULT_CHARSET
      InfoPanel.Font.Color = 536870912
      InfoPanel.Font.Height = -11
      InfoPanel.Font.Name = 'MS Sans Serif'
      InfoPanel.Font.Style = []
      TabOrder = 4
    end
    object gbProject: TcxGroupBox
      Left = 8
      Top = 6
      Caption = 'Primary Info'
      TabOrder = 0
      Height = 60
      Width = 529
      object Label8: TcxLabel
        Left = 9
        Top = 27
        Caption = 'Project:'
        Transparent = True
      end
      object Label10: TcxLabel
        Left = 272
        Top = 27
        Caption = 'Issue Owner:'
        Transparent = True
      end
      object lcbProject: TcxDBLookupComboBox
        Left = 49
        Top = 24
        DataBinding.DataField = 'PROJECTID'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Properties.DropDownAutoSize = True
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            Width = 200
            FieldName = 'NAME'
          end
          item
            Caption = 'Manager'
            Width = 100
            FieldName = 'Manager'
          end>
        Properties.ListSource = EditorsLookupDemoDataDM.dsProjects
        Style.Color = 16247513
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 0
        Width = 217
      end
      object lcbOwner: TcxDBLookupComboBox
        Left = 336
        Top = 24
        DataBinding.DataField = 'OWNERID'
        DataBinding.DataSource = EditorsLookupDemoDataDM.dsItems
        Properties.DropDownListStyle = lsEditList
        Properties.ImmediateDropDownWhenKeyPressed = False
        Properties.KeyFieldNames = 'ID'
        Properties.ListColumns = <
          item
            FieldName = 'UserName'
          end>
        Properties.ListOptions.GridLines = glNone
        Properties.ListOptions.ShowHeader = False
        Properties.ListSource = EditorsLookupDemoDataDM.dsUsers
        Properties.MaxLength = 50
        Properties.OnNewLookupDisplayText = lcbCreatorNewLookupDisplayText
        Style.Color = 16247513
        Style.StyleController = EditorsLookupDemoDataDM.StyleController
        TabOrder = 1
        Width = 185
      end
    end
  end
  inherited mmMain: TMainMenu
    Left = 16
    object miOptions: TMenuItem [1]
      Caption = 'Options'
      object LookupOptions1: TMenuItem
        Caption = 'Lookup &Options'
        object miEditMode: TMenuItem
          Tag = 3
          Caption = '&Edit mode'
          Checked = True
          GroupIndex = 2
          RadioItem = True
          OnClick = ChangeLookupModeClick
        end
        object miPickMode: TMenuItem
          Tag = 1
          Caption = '&Pick mode'
          GroupIndex = 2
          Hint = 'Pick mode with incremental filtering'
          RadioItem = True
          OnClick = ChangeLookupModeClick
        end
        object miStandardMode: TMenuItem
          Tag = 2
          Caption = '&Standard mode'
          GroupIndex = 2
          Hint = 'Standard lookup mode'
          RadioItem = True
          OnClick = ChangeLookupModeClick
        end
      end
    end
    object miStorage: TMenuItem [2]
      Caption = '&Storage'
      object miStorageActive: TMenuItem
        AutoCheck = True
        Caption = 'Save the settings before closing the demo '
        Checked = True
        Hint = 
          'Determines whether the settings are stored before closing the de' +
          'mo'
        OnClick = miStorageActiveClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object miStorageType: TMenuItem
        Caption = 'Storage &Location'
        object miIniStoreType: TMenuItem
          Caption = '&Ini file'
          Checked = True
          Hint = 'Setting this option changes the current storage to an INI file'
          RadioItem = True
          OnClick = StorageTypeClick
        end
        object miRegistryStoreType: TMenuItem
          Tag = 1
          Caption = '&Registry'
          Hint = 'Setting this option changes the current storage to the registry'
          RadioItem = True
          OnClick = StorageTypeClick
        end
        object miMemoryStoreType: TMenuItem
          Tag = 2
          Caption = '&Memory'
          Hint = 'Setting this option changes the current storage to memory'
          RadioItem = True
          OnClick = StorageTypeClick
        end
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miStore: TMenuItem
        Caption = '&Store'
        Hint = 'Saves the demo settings to the current storage'
        OnClick = miStoreClick
      end
      object miRestore: TMenuItem
        Caption = '&Restore'
        Hint = 'Restores the demo settings from the current storage'
        OnClick = miRestoreClick
      end
    end
  end
  object cxPropertiesStore: TcxPropertiesStore
    Components = <
      item
        Component = lcbCreator
        Properties.Strings = (
          'Properties.DropDownListStyle'
          'Properties.ImmediateDropDown')
      end
      item
        Component = lcbOwner
        Properties.Strings = (
          'Properties.DropDownListStyle'
          'Properties.ImmediateDropDown')
      end
      item
        Component = lcbProject
        Properties.Strings = (
          'Properties.DropDownListStyle')
      end
      item
        Component = miEditMode
        Properties.Strings = (
          'Checked')
      end
      item
        Component = miPickMode
        Properties.Strings = (
          'Checked')
      end
      item
        Component = miStandardMode
        Properties.Strings = (
          'Checked')
      end>
    StorageName = 'cxPropertiesStore.ini'
    Left = 364
    Top = 8
  end
  object cxStorageActiveStore: TcxPropertiesStore
    Components = <
      item
        Component = cxPropertiesStore
        Properties.Strings = (
          'Active'
          'StorageName'
          'StorageType')
      end
      item
        Component = miIniStoreType
        Properties.Strings = (
          'Checked')
      end
      item
        Component = miMemoryStoreType
        Properties.Strings = (
          'Checked')
      end
      item
        Component = miRegistryStoreType
        Properties.Strings = (
          'Checked')
      end
      item
        Component = miStorageActive
        Properties.Strings = (
          'Checked')
      end>
    StorageName = 'cxStorageActiveStore.INI'
    Left = 400
    Top = 8
  end
end

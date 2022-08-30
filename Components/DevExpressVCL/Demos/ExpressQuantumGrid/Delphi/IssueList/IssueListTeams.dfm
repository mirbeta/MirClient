inherited frmTeams: TfrmTeams
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbCaption: TcxLabel
    Style.IsFontAssigned = True
  end
  inherited plTop: TPanel
    object Label9: TcxLabel
      Left = 6
      Top = 88
      Caption = 'Function'
      Transparent = True
    end
    object Label5: TcxLabel
      Left = 6
      Top = 8
      Caption = 'Project'
      Transparent = True
    end
    object Label1: TcxLabel
      Left = 6
      Top = 48
      Caption = 'User'
      Transparent = True
    end
    object cxDBTextEdit1: TcxDBTextEdit
      Left = 8
      Top = 104
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'FUNCTION'
      DataBinding.DataSource = dmMain.dsTeam
      Style.StyleController = dmMain.edstcMain
      TabOrder = 0
      Width = 209
    end
    object cxDBLookupComboBox3: TcxDBLookupComboBox
      Left = 8
      Top = 24
      RepositoryItem = dmMain.edrepProjectName
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'PROJECTID'
      DataBinding.DataSource = dmMain.dsTeam
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'NAME'
        end>
      Properties.ListSource = dmMain.dsDepartments
      Style.StyleController = dmMain.edstcMain
      TabOrder = 1
      Width = 209
    end
    object cxDBLookupComboBox1: TcxDBLookupComboBox
      Left = 8
      Top = 64
      RepositoryItem = dmMain.edrepUserLookup
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'USERID'
      DataBinding.DataSource = dmMain.dsTeam
      Properties.DropDownListStyle = lsFixedList
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'FullName'
        end
        item
          FieldName = 'DepartmentName'
        end>
      Properties.ListSource = dmMain.dsUsers
      Style.StyleController = dmMain.edstcMain
      TabOrder = 2
      Width = 209
    end
  end
end

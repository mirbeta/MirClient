inherited frmProjects: TfrmProjects
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbCaption: TcxLabel
    Style.IsFontAssigned = True
  end
  inherited plTop: TPanel
    object Label5: TcxLabel
      Left = 6
      Top = 48
      Caption = 'Manager'
      Transparent = True
    end
    object Label1: TcxLabel
      Left = 6
      Top = 8
      Caption = 'Name'
      Transparent = True
    end
    object cxDBTextEdit1: TcxDBTextEdit
      Left = 8
      Top = 24
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'NAME'
      DataBinding.DataSource = dmMain.dsProjects
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 0
      Width = 211
    end
    object cxDBLookupComboBox3: TcxDBLookupComboBox
      Left = 8
      Top = 64
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'MANAGERID'
      DataBinding.DataSource = dmMain.dsProjects
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
      Properties.ReadOnly = False
      Style.StyleController = dmMain.edstcMain
      TabOrder = 1
      Width = 211
    end
  end
end

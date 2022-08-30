inherited frmUsers: TfrmUsers
  ClientHeight = 312
  ClientWidth = 212
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbCaption: TcxLabel
    Caption = 'lbCaption + Users'
    Style.IsFontAssigned = True
    Width = 212
  end
  inherited plTop: TPanel
    Width = 212
    Height = 252
    object Label9: TcxLabel
      Left = 6
      Top = 0
      Caption = 'Forename'
      Transparent = True
    end
    object Label1: TcxLabel
      Left = 6
      Top = 40
      Caption = 'Middlename'
      Transparent = True
    end
    object Label2: TcxLabel
      Left = 6
      Top = 80
      Caption = 'Surname'
      Transparent = True
    end
    object Label3: TcxLabel
      Left = 6
      Top = 160
      Caption = 'Phone'
      Transparent = True
    end
    object Label4: TcxLabel
      Left = 6
      Top = 200
      Caption = 'E-mail'
      Transparent = True
    end
    object Label5: TcxLabel
      Left = 6
      Top = 120
      Caption = 'Department'
      Transparent = True
    end
    object cxDBTextEdit1: TcxDBTextEdit
      Left = 8
      Top = 16
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'FNAME'
      DataBinding.DataSource = dmMain.dsUsers
      Style.StyleController = dmMain.edstcMain
      TabOrder = 0
      Width = 196
    end
    object cxDBTextEdit2: TcxDBTextEdit
      Left = 8
      Top = 56
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'MNAME'
      DataBinding.DataSource = dmMain.dsUsers
      Style.StyleController = dmMain.edstcMain
      TabOrder = 1
      Width = 196
    end
    object cxDBTextEdit3: TcxDBTextEdit
      Left = 8
      Top = 96
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'LNAME'
      DataBinding.DataSource = dmMain.dsUsers
      Style.StyleController = dmMain.edstcMain
      TabOrder = 2
      Width = 196
    end
    object cxDBMaskEdit1: TcxDBMaskEdit
      Left = 8
      Top = 176
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'PHONE'
      DataBinding.DataSource = dmMain.dsUsers
      Style.StyleController = dmMain.edstcMain
      TabOrder = 4
      Width = 196
    end
    object cxDBLookupComboBox3: TcxDBLookupComboBox
      Left = 8
      Top = 136
      RepositoryItem = dmMain.edrepDepartmentName
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'DEPARTMENTID'
      DataBinding.DataSource = dmMain.dsUsers
      Properties.KeyFieldNames = 'ID'
      Properties.ListColumns = <
        item
          FieldName = 'NAME'
        end>
      Properties.ListSource = dmMain.dsDepartment
      Style.StyleController = dmMain.edstcMain
      TabOrder = 3
      Width = 196
    end
    object cxDBTextEdit4: TcxDBTextEdit
      Left = 8
      Top = 216
      Anchors = [akLeft, akTop, akRight]
      DataBinding.DataField = 'EMAIL'
      DataBinding.DataSource = dmMain.dsUsers
      Style.StyleController = dmMain.edstcMain
      TabOrder = 5
      Width = 196
    end
  end
  inherited cxNavigator: TcxNavigator
    Width = 204
  end
end

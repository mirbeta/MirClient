inherited frmItems: TfrmItems
  ClientHeight = 498
  ClientWidth = 279
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbCaption: TcxLabel
    Style.IsFontAssigned = True
    Width = 279
  end
  inherited plTop: TPanel
    Width = 279
    Height = 438
    object PageControl: TcxPageControl
      Left = 0
      Top = 0
      Width = 279
      Height = 438
      Align = alClient
      TabOrder = 0
      Properties.ActivePage = tsGeneral
      Properties.MultiLine = True
      Properties.Rotate = True
      Properties.TabPosition = tpLeft
      ClientRectBottom = 438
      ClientRectLeft = 68
      ClientRectRight = 279
      ClientRectTop = 0
      object tsGeneral: TcxTabSheet
        Caption = 'General'
        ImageIndex = 2
        object cxDBDateEdit1: TcxDBDateEdit
          Left = 8
          Top = 384
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'LASTMODIFIEDDATE'
          DataBinding.DataSource = dmMain.dsItems
          Style.StyleController = dmMain.edstcMain
          TabOrder = 0
          Width = 198
        end
        object cxDBDateEdit2: TcxDBDateEdit
          Left = 8
          Top = 344
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'CREATEDDATE'
          DataBinding.DataSource = dmMain.dsItems
          Style.StyleController = dmMain.edstcMain
          TabOrder = 1
          Width = 198
        end
        object cxDBDateEdit3: TcxDBDateEdit
          Left = 8
          Top = 264
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'FIXEDDATE'
          DataBinding.DataSource = dmMain.dsItems
          Style.StyleController = dmMain.edstcMain
          TabOrder = 2
          Width = 198
        end
        object cxDBImageComboBox1: TcxDBImageComboBox
          Left = 8
          Top = 184
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'PRIORITY'
          DataBinding.DataSource = dmMain.dsItems
          Properties.Images = dmMain.imStat
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
          Style.StyleController = dmMain.edstcMain
          TabOrder = 3
          Width = 198
        end
        object cxDBImageComboBox2: TcxDBImageComboBox
          Left = 8
          Top = 144
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'STATUS'
          DataBinding.DataSource = dmMain.dsItems
          Properties.Images = dmMain.imStat
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
          Style.StyleController = dmMain.edstcMain
          TabOrder = 4
          Width = 198
        end
        object cxDBImageComboBox3: TcxDBImageComboBox
          Left = 8
          Top = 104
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'TYPE'
          DataBinding.DataSource = dmMain.dsItems
          Properties.Images = dmMain.imStat
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
          Style.StyleController = dmMain.edstcMain
          TabOrder = 5
          Width = 198
        end
        object cxDBLookupComboBox1: TcxDBLookupComboBox
          Left = 8
          Top = 224
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'OWNERID'
          DataBinding.DataSource = dmMain.dsItems
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
          TabOrder = 6
          Width = 198
        end
        object cxDBLookupComboBox2: TcxDBLookupComboBox
          Left = 8
          Top = 304
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'CREATORID'
          DataBinding.DataSource = dmMain.dsItems
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
          TabOrder = 7
          Width = 198
        end
        object cxDBLookupComboBox3: TcxDBLookupComboBox
          Left = 8
          Top = 64
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'PROJECTID'
          DataBinding.DataSource = dmMain.dsItems
          Properties.KeyFieldNames = 'ID'
          Properties.ListColumns = <
            item
              FieldName = 'NAME'
            end>
          Properties.ListSource = dmMain.dsProjects
          Style.StyleController = dmMain.edstcMain
          TabOrder = 8
          Width = 198
        end
        object cxDBTextEdit1: TcxDBTextEdit
          Left = 8
          Top = 24
          Anchors = [akLeft, akTop, akRight]
          DataBinding.DataField = 'NAME'
          DataBinding.DataSource = dmMain.dsItems
          Style.StyleController = dmMain.edstcMain
          TabOrder = 9
          Width = 198
        end
        object Label1: TcxLabel
          Left = 7
          Top = 288
          Caption = 'Creator'
          ParentFont = False
          Transparent = True
        end
        object Label10: TcxLabel
          Left = 7
          Top = 47
          Caption = 'Project'
          Transparent = True
        end
        object Label11: TcxLabel
          Left = 7
          Top = 7
          Caption = 'Subject'
          Transparent = True
        end
        object Label2: TcxLabel
          Left = 7
          Top = 248
          Caption = 'Fixed Date'
          ParentFont = False
          Transparent = True
        end
        object Label3: TcxLabel
          Left = 7
          Top = 207
          Caption = 'Owner'
          Transparent = True
        end
        object Label4: TcxLabel
          Left = 7
          Top = 368
          Caption = 'Last Modified Date'
          ParentFont = False
          Transparent = True
        end
        object Label5: TcxLabel
          Left = 7
          Top = 328
          Caption = 'Created Date'
          ParentFont = False
          Transparent = True
        end
        object Label6: TcxLabel
          Left = 7
          Top = 127
          Caption = 'Status'
          Transparent = True
        end
        object Label7: TcxLabel
          Left = 7
          Top = 167
          Caption = 'Priority'
          Transparent = True
        end
        object Label8: TcxLabel
          Left = 7
          Top = 87
          Caption = 'Type'
          Transparent = True
        end
      end
      object tsDescription: TcxTabSheet
        Caption = 'Description'
        object cxDBMemo1: TcxDBMemo
          Left = 0
          Top = 0
          Align = alClient
          DataBinding.DataField = 'DESCRIPTION'
          DataBinding.DataSource = dmMain.dsItems
          Properties.MaxLength = 0
          Properties.ReadOnly = False
          Properties.ScrollBars = ssVertical
          Style.StyleController = dmMain.edstcMain
          TabOrder = 0
          Height = 438
          Width = 211
        end
      end
    end
  end
  inherited cxNavigator: TcxNavigator
    Width = 276
  end
end

inherited frmEditorsDemoMain: TfrmEditorsDemoMain
  PixelsPerInch = 96
  TextHeight = 13
  inherited lcMain: TdxLayoutControl
    DragDropMode = ddmMultiChoice
    OptionsImage.Images = cxImageList1
    object DBEdit1: TDBEdit [0]
      Left = 123
      Top = 32
      Width = 123
      Height = 17
      BorderStyle = bsNone
      DataField = 'PurchaseDate'
      DataSource = dmDemo.dsOrders
      TabOrder = 0
    end
    object DBEdit2: TDBEdit [1]
      Left = 323
      Top = 32
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'Orders_Time'
      DataSource = dmDemo.dsOrders
      TabOrder = 2
    end
    object DBEdit4: TDBEdit [2]
      Left = 562
      Top = 59
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'PaymentAmount'
      DataSource = dmDemo.dsOrders
      TabOrder = 4
    end
    object DBEdit5: TDBEdit [3]
      Left = 123
      Top = 173
      Width = 161
      Height = 17
      BorderStyle = bsNone
      DataField = 'FirstName'
      DataSource = dmDemo.dsOrders
      TabOrder = 6
    end
    object DBEdit6: TDBEdit [4]
      Left = 123
      Top = 200
      Width = 161
      Height = 17
      BorderStyle = bsNone
      DataField = 'LastName'
      DataSource = dmDemo.dsOrders
      TabOrder = 7
    end
    object DBEdit7: TDBEdit [5]
      Left = 123
      Top = 146
      Width = 161
      Height = 17
      BorderStyle = bsNone
      DataField = 'Prefix'
      DataSource = dmDemo.dsOrders
      TabOrder = 5
    end
    object DBEdit8: TDBEdit [6]
      Left = 123
      Top = 227
      Width = 161
      Height = 17
      BorderStyle = bsNone
      DataField = 'Spouse'
      DataSource = dmDemo.dsOrders
      TabOrder = 8
    end
    object DBEdit9: TDBEdit [7]
      Left = 586
      Top = 173
      Width = 155
      Height = 17
      BorderStyle = bsNone
      DataField = 'Title'
      DataSource = dmDemo.dsOrders
      TabOrder = 13
    end
    object DBCheckBox1: TDBCheckBox [8]
      Left = 10000
      Top = 10000
      Width = 136
      Height = 17
      Caption = 'Customer'
      DataField = 'Customer'
      DataSource = dmDemo.dsOrders
      TabOrder = 32
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      Visible = False
    end
    object DBEdit10: TDBEdit [9]
      Left = 586
      Top = 146
      Width = 155
      Height = 17
      BorderStyle = bsNone
      DataField = 'Company'
      DataSource = dmDemo.dsOrders
      TabOrder = 12
    end
    object DBEdit11: TDBEdit [10]
      Left = 586
      Top = 200
      Width = 155
      Height = 17
      BorderStyle = bsNone
      DataField = 'Occupation'
      DataSource = dmDemo.dsOrders
      TabOrder = 14
    end
    object DBEdit12: TDBEdit [11]
      Left = 353
      Top = 146
      Width = 152
      Height = 17
      BorderStyle = bsNone
      DataField = 'State'
      DataSource = dmDemo.dsOrders
      TabOrder = 9
    end
    object DBEdit13: TDBEdit [12]
      Left = 353
      Top = 173
      Width = 152
      Height = 17
      BorderStyle = bsNone
      DataField = 'City'
      DataSource = dmDemo.dsOrders
      TabOrder = 10
    end
    object DBEdit14: TDBEdit [13]
      Left = 353
      Top = 200
      Width = 152
      Height = 17
      BorderStyle = bsNone
      DataField = 'ZipCode'
      DataSource = dmDemo.dsOrders
      TabOrder = 11
    end
    object DBEdit15: TDBEdit [14]
      Left = 123
      Top = 274
      Width = 618
      Height = 17
      BorderStyle = bsNone
      DataField = 'Address'
      DataSource = dmDemo.dsOrders
      TabOrder = 15
    end
    object DBEdit16: TDBEdit [15]
      Left = 123
      Top = 301
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'HomePhone'
      DataSource = dmDemo.dsOrders
      TabOrder = 16
    end
    object DBEdit17: TDBEdit [16]
      Left = 313
      Top = 301
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'FaxPhone'
      DataSource = dmDemo.dsOrders
      TabOrder = 17
    end
    object DBEdit18: TDBEdit [17]
      Left = 477
      Top = 301
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'Email'
      DataSource = dmDemo.dsOrders
      TabOrder = 18
    end
    object DBEdit19: TDBEdit [18]
      Left = 97
      Top = 394
      Width = 630
      Height = 17
      BorderStyle = bsNone
      DataField = 'Trademark'
      DataSource = dmDemo.dsOrders
      TabOrder = 19
    end
    object DBEdit20: TDBEdit [19]
      Left = 97
      Top = 421
      Width = 630
      Height = 17
      BorderStyle = bsNone
      DataField = 'Model'
      DataSource = dmDemo.dsOrders
      TabOrder = 20
    end
    object DBEdit21: TDBEdit [20]
      Left = 97
      Top = 448
      Width = 630
      Height = 17
      BorderStyle = bsNone
      DataField = 'Hyperlink'
      DataSource = dmDemo.dsOrders
      TabOrder = 21
    end
    object DBEdit22: TDBEdit [21]
      Left = 10000
      Top = 10000
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'Price'
      DataSource = dmDemo.dsOrders
      TabOrder = 30
      Visible = False
    end
    object DBEdit23: TDBEdit [22]
      Left = 10000
      Top = 10000
      Width = 50
      Height = 17
      BorderStyle = bsNone
      DataField = 'HP'
      DataSource = dmDemo.dsOrders
      TabOrder = 22
      Visible = False
    end
    object DBEdit24: TDBEdit [23]
      Left = 10000
      Top = 10000
      Width = 47
      Height = 17
      BorderStyle = bsNone
      DataField = 'Liter'
      DataSource = dmDemo.dsOrders
      TabOrder = 23
      Visible = False
    end
    object DBEdit25: TDBEdit [24]
      Left = 10000
      Top = 10000
      Width = 47
      Height = 17
      BorderStyle = bsNone
      DataField = 'Cyl'
      DataSource = dmDemo.dsOrders
      TabOrder = 24
      Visible = False
    end
    object DBEdit26: TDBEdit [25]
      Left = 10000
      Top = 10000
      Width = 87
      Height = 17
      BorderStyle = bsNone
      DataField = 'MPG_City'
      DataSource = dmDemo.dsOrders
      TabOrder = 35
      Visible = False
    end
    object DBEdit27: TDBEdit [26]
      Left = 10000
      Top = 10000
      Width = 90
      Height = 17
      BorderStyle = bsNone
      DataField = 'MPG_Highway'
      DataSource = dmDemo.dsOrders
      TabOrder = 29
      Visible = False
    end
    object DBCheckBox2: TDBCheckBox [27]
      Left = 10000
      Top = 10000
      Width = 215
      Height = 17
      Caption = 'Automatic Transmission'
      DataField = 'TransmissAutomatic'
      DataSource = dmDemo.dsOrders
      TabOrder = 25
      ValueChecked = 'True'
      ValueUnchecked = 'False'
      Visible = False
    end
    object DBEdit28: TDBEdit [28]
      Left = 10000
      Top = 10000
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'TransmissSpeedCount'
      DataSource = dmDemo.dsOrders
      TabOrder = 26
      Visible = False
    end
    object DBMemo1: TDBMemo [29]
      Left = 10000
      Top = 10000
      Width = 649
      Height = 175
      BorderStyle = bsNone
      DataField = 'Cars_Description'
      DataSource = dmDemo.dsOrders
      ScrollBars = ssBoth
      TabOrder = 27
      Visible = False
    end
    object DBNavigator1: TDBNavigator [30]
      Left = 10
      Top = 603
      Width = 200
      Height = 25
      DataSource = dmDemo.dsOrders
      TabOrder = 28
    end
    object DBComboBox2: TDBComboBox [31]
      Left = 121
      Top = 57
      Width = 127
      Height = 21
      DataField = 'PaymentType'
      DataSource = dmDemo.dsOrders
      Items.Strings = (
        'AmEx'
        'Cach'
        'Master'
        'Visa')
      TabOrder = 1
    end
    object DBEdit29: TDBEdit [32]
      Left = 323
      Top = 59
      Width = 121
      Height = 17
      BorderStyle = bsNone
      DataField = 'Quantity'
      DataSource = dmDemo.dsOrders
      TabOrder = 3
    end
    inherited lcMainGroup_Root1: TdxLayoutGroup
      AllowQuickCustomize = True
      Index = -1
    end
    object lcMainGroup1: TdxLayoutGroup
      Parent = lcMainGroup_Root1
      CaptionOptions.Text = 'Order'
      ButtonOptions.Buttons = <>
      ButtonOptions.ShowExpandButton = True
      LayoutDirection = ldHorizontal
      Index = 0
    end
    object lcMainGroup17: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lcMainItem1: TdxLayoutItem
      Parent = lcMainGroup17
      CaptionOptions.ImageIndex = 0
      CaptionOptions.Text = 'Purchase Date:'
      Control = DBEdit1
      Index = 0
    end
    object lcMainItem34: TdxLayoutItem
      Parent = lcMainGroup17
      CaptionOptions.ImageIndex = 1
      CaptionOptions.Text = 'Payment Type:'
      Control = DBComboBox2
      ControlOptions.ShowBorder = False
      Index = 1
    end
    object lcMainGroup16: TdxLayoutGroup
      Parent = lcMainGroup1
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 1
    end
    object lcMainItem2: TdxLayoutItem
      Parent = lcMainGroup16
      CaptionOptions.ImageIndex = 2
      CaptionOptions.Text = 'Time:'
      Control = DBEdit2
      Index = 0
    end
    object lcMainItem4: TdxLayoutItem
      Parent = lcMainGroup16
      CaptionOptions.ImageIndex = 3
      CaptionOptions.Text = 'Quantity:'
      Control = DBEdit29
      Index = 1
    end
    object lcMainItem5: TdxLayoutItem
      Parent = lcMainGroup1
      AlignVert = avBottom
      CaptionOptions.ImageIndex = 4
      CaptionOptions.Text = 'Payment Amount:'
      Control = DBEdit4
      Index = 2
    end
    object lcMainGroup2: TdxLayoutGroup
      Parent = lcMainGroup_Root1
      CaptionOptions.Text = 'Customer'
      ButtonOptions.Buttons = <
        item
          Glyph.Data = {
            3E020000424D3E0200000000000036000000280000000D0000000D0000000100
            1800000000000802000000000000000000000000000000000000FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FF00FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            FF00FFFF00FFFF00FF00FF00FFFF00FF0000FF0000FFFF00FFFF00FFFF00FFFF
            00FFFF00FF0000FF0000FFFF00FFFF00FF00FF00FFFF00FF0000FF0000FF0000
            FFFF00FFFF00FFFF00FF0000FF0000FF0000FFFF00FFFF00FF00FF00FFFF00FF
            FF00FF0000FF0000FF0000FFFF00FF0000FF0000FF0000FFFF00FFFF00FFFF00
            FF00FF00FFFF00FFFF00FFFF00FF0000FF0000FF0000FF0000FF0000FFFF00FF
            FF00FFFF00FFFF00FF00FF00FFFF00FFFF00FFFF00FFFF00FF0000FF0000FF00
            00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FFFF00FFFF00FF0000
            FF0000FF0000FF0000FF0000FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
            FF00FF0000FF0000FF0000FFFF00FF0000FF0000FF0000FFFF00FFFF00FFFF00
            FF00FF00FFFF00FF0000FF0000FF0000FFFF00FFFF00FFFF00FF0000FF0000FF
            0000FFFF00FFFF00FF00FF00FFFF00FF0000FF0000FFFF00FFFF00FFFF00FFFF
            00FFFF00FF0000FF0000FFFF00FFFF00FF00FF00FFFF00FFFF00FFFF00FFFF00
            FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00FF00FFFF00FF
            FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
            FF00}
          OnClick = lcMainGroup2Button0Click
        end>
      ButtonOptions.ShowExpandButton = True
      Index = 1
    end
    object lcMainGroup7: TdxLayoutGroup
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcMainGroup10: TdxLayoutGroup
      Parent = lcMainGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 0
    end
    object lcMainItem8: TdxLayoutItem
      Parent = lcMainGroup10
      CaptionOptions.Text = 'Prefix:'
      Control = DBEdit7
      Index = 0
    end
    object lcMainItem6: TdxLayoutItem
      Parent = lcMainGroup10
      CaptionOptions.Text = 'First Name:'
      Control = DBEdit5
      Index = 1
    end
    object lcMainItem7: TdxLayoutItem
      Parent = lcMainGroup10
      CaptionOptions.Text = 'Last Name:'
      Control = DBEdit6
      Index = 2
    end
    object lcMainItem9: TdxLayoutItem
      Parent = lcMainGroup10
      CaptionOptions.Text = 'Spouse:'
      Control = DBEdit8
      Index = 3
    end
    object lcMainSplitterItem1: TdxLayoutSplitterItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Splitter'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      AllowCloseOnClick = True
      Index = 1
    end
    object lcMainGroup9: TdxLayoutGroup
      Parent = lcMainGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 2
    end
    object lcMainItem14: TdxLayoutItem
      Parent = lcMainGroup9
      CaptionOptions.Text = 'State:'
      Control = DBEdit12
      Index = 0
    end
    object lcMainItem15: TdxLayoutItem
      Parent = lcMainGroup9
      CaptionOptions.Text = 'City:'
      Control = DBEdit13
      Index = 1
    end
    object lcMainItem16: TdxLayoutItem
      Parent = lcMainGroup9
      CaptionOptions.Text = 'Zip Code:'
      Control = DBEdit14
      Index = 2
    end
    object lcMainSplitterItem2: TdxLayoutSplitterItem
      Parent = lcMainGroup7
      CaptionOptions.Text = 'Splitter'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      AllowCloseOnClick = True
      Index = 3
    end
    object lcMainGroup8: TdxLayoutGroup
      Parent = lcMainGroup7
      AlignHorz = ahClient
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      ShowBorder = False
      Index = 4
    end
    object lcMainItem12: TdxLayoutItem
      Parent = lcMainGroup8
      CaptionOptions.Text = 'Company:'
      Control = DBEdit10
      Index = 0
    end
    object lcMainItem10: TdxLayoutItem
      Parent = lcMainGroup8
      CaptionOptions.Text = 'Title:'
      Control = DBEdit9
      Index = 1
    end
    object lcMainItem13: TdxLayoutItem
      Parent = lcMainGroup8
      CaptionOptions.Text = 'Occupation:'
      Control = DBEdit11
      Index = 2
    end
    object lcMainSeparatorItem1: TdxLayoutSeparatorItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Contact Info'
      CaptionOptions.Visible = True
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 2
    end
    object lcMainItem17: TdxLayoutItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Address:'
      Control = DBEdit15
      Index = 3
    end
    object lcMainGroup13: TdxLayoutGroup
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 4
    end
    object lcMainItem18: TdxLayoutItem
      Parent = lcMainGroup13
      CaptionOptions.Text = 'Home Phone:'
      Control = DBEdit16
      Index = 0
    end
    object lcMainItem19: TdxLayoutItem
      Parent = lcMainGroup13
      CaptionOptions.Text = 'Fax Phone:'
      Control = DBEdit17
      Index = 1
    end
    object lcMainItem20: TdxLayoutItem
      Parent = lcMainGroup13
      CaptionOptions.Text = 'Email:'
      Control = DBEdit18
      Index = 2
    end
    object lcMainGroup3: TdxLayoutGroup
      Parent = lcMainGroup_Root1
      CaptionOptions.Text = 'Car Info'
      ButtonOptions.Buttons = <>
      ButtonOptions.ShowExpandButton = True
      LayoutDirection = ldTabbed
      Index = 2
    end
    object lcMainGroup4: TdxLayoutGroup
      Parent = lcMainGroup3
      CaptionOptions.Text = 'Model'
      ButtonOptions.Buttons = <>
      Index = 0
    end
    object lcMainItem21: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Text = 'Trademark:'
      Control = DBEdit19
      Index = 0
    end
    object lcMainItem22: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Text = 'Model:'
      Control = DBEdit20
      Index = 1
    end
    object lcMainItem23: TdxLayoutItem
      Parent = lcMainGroup4
      CaptionOptions.Text = 'Hyperlink:'
      Control = DBEdit21
      Index = 2
    end
    object lcMainGroup5: TdxLayoutGroup
      Parent = lcMainGroup3
      CaptionOptions.Text = 'Engine'
      ButtonOptions.Buttons = <>
      Index = 1
    end
    object lcMainGroup14: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 0
    end
    object lcMainItem25: TdxLayoutItem
      Parent = lcMainGroup14
      CaptionOptions.Text = 'HP:'
      Control = DBEdit23
      Index = 0
    end
    object lcMainItem26: TdxLayoutItem
      Parent = lcMainGroup14
      CaptionOptions.Text = 'Liter:'
      Control = DBEdit24
      Index = 1
    end
    object lcMainItem27: TdxLayoutItem
      Parent = lcMainGroup14
      CaptionOptions.Text = 'Cyl:'
      Control = DBEdit25
      Index = 2
    end
    object lcMainGroup15: TdxLayoutGroup
      Parent = lcMainGroup5
      CaptionOptions.Text = 'Hidden Group'
      ButtonOptions.Buttons = <>
      Hidden = True
      LayoutDirection = ldHorizontal
      ShowBorder = False
      Index = 1
    end
    object lcMainItem30: TdxLayoutItem
      Parent = lcMainGroup15
      CaptionOptions.Text = 'Transmiss Automatic'
      CaptionOptions.Visible = False
      Control = DBCheckBox2
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object lcMainItem31: TdxLayoutItem
      Parent = lcMainGroup15
      CaptionOptions.Text = 'Speed Count:'
      Control = DBEdit28
      Index = 1
    end
    object lcMainGroup6: TdxLayoutGroup
      Parent = lcMainGroup3
      CaptionOptions.Text = 'Description'
      ButtonOptions.Buttons = <>
      Index = 2
    end
    object lcMainItem32: TdxLayoutItem
      Parent = lcMainGroup6
      CaptionOptions.Text = 'Description:'
      Control = DBMemo1
      Index = 0
    end
    object lcMainItem33: TdxLayoutItem
      Parent = lcMainGroup_Root1
      AlignHorz = ahLeft
      AlignVert = avTop
      CaptionOptions.Text = 'Navigator:'
      CaptionOptions.Visible = False
      Control = DBNavigator1
      ControlOptions.AutoColor = True
      ControlOptions.MinWidth = 200
      ControlOptions.ShowBorder = False
      Index = 3
    end
    object lcMainItem28: TdxLayoutItem
      CaptionOptions.Text = 'MPG_City:'
      Control = DBEdit26
      Index = -1
    end
    object lcMainItem11: TdxLayoutItem
      CaptionOptions.Text = 'Customer'
      CaptionOptions.Visible = False
      Control = DBCheckBox1
      ControlOptions.AutoColor = True
      ControlOptions.ShowBorder = False
      Index = -1
    end
    object lcMainItem24: TdxLayoutItem
      CaptionOptions.ImageIndex = 5
      CaptionOptions.Text = 'Price:'
      Control = DBEdit22
      Index = -1
    end
    object lcMainItem29: TdxLayoutItem
      CaptionOptions.Text = 'MPG_Highway'
      Control = DBEdit27
      Index = -1
    end
    object lcMainSeparatorItem2: TdxLayoutSeparatorItem
      Parent = lcMainGroup2
      CaptionOptions.Text = 'Personal Info'
      CaptionOptions.Visible = True
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      Index = 0
    end
    object dxLayoutSplitterItem1: TdxLayoutSplitterItem
      Parent = lcMainGroup_Root1
      CaptionOptions.Text = 'Splitter'
      SizeOptions.AssignedValues = [sovSizableHorz, sovSizableVert]
      SizeOptions.SizableHorz = False
      SizeOptions.SizableVert = False
      AllowCloseOnClick = True
      Index = 1
    end
  end
  object cxImageList1: TcxImageList
    FormatVersion = 1
    DesignInfo = 524752
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000000000000000000000000000000000000006E6E6EFF6F6F
          6FFF6E6E6EFF6D6D6DFF6D6D6DFF6C6C6CFF6B6B6BFF6A6A6AFF6A6A6AFF6969
          69FF696969FF686868FF686868FF676767FF666666FF656565FF767676FFF7F7
          F7FFF6F6F6FFC7C7C7FFF6F6F6FFF5F5F5FFC6C6C6FFF4F4F4FFF3F3F3FFC5C5
          C5FFF2F2F2FFF1F1F1FFC4C4C4FFF1F1F1FFF0F0F0FF6B6B6BFF7B7B7BFFCACA
          CAFFCACACAFFCACACAFFC9C9C9FFC9C9C9FFC9C9C9FFC9C9C9FFC8C8C8FFC8C8
          C8FFC8C8C8FFC8C8C8FFC7C7C7FFC7C7C7FFC7C7C7FF707070FF828282FFF9F9
          F9FFF9F9F9FFCDCDCDFFF8F8F8FFF8F8F8FFCCCCCCFFF7F7F7FFF7F7F7FFCBCB
          CBFFF6F6F6FFF6F6F6FFCACACAFFF5F5F5FFF4F4F4FF757575FF898989FFD0D0
          D0FFD0D0D0FFD0D0D0FFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCECECEFFCECE
          CEFFCECECEFFCECECEFFCDCDCDFFCDCDCDFFCDCDCDFF7B7B7BFF919191FFFCFC
          FCFFFBFBFBFFD3D3D3FFFBFBFBFFFAFAFAFFD2D2D2FFFAFAFAFFF9F9F9FFD1D1
          D1FFF9F9F9FFF8F8F8FFD0D0D0FFF8F8F8FFF7F7F7FF828282FF999999FFD6D6
          D6FFD6D6D6FFD6D6D6FFD6D6D6FFD6D6D6FFD5D5D5FFD5D5D5FFD5D5D5FF3434
          BCFF3131B9FF2E2EB5FF2B2BB0FFD3D3D3FFD3D3D3FF898989FFA3A3A3FFFDFD
          FDFFFDFDFDFFDADADAFFFDFDFDFFFDFDFDFFD9D9D9FFFCFCFCFFFCFCFCFF4747
          C8FF9999DEFF9797DCFF3E3EBCFFFAFAFAFFFAFAFAFF919191FFACACACFFDEDE
          DEFFDDDDDDFFDDDDDDFFDDDDDDFFDCDCDCFFDCDCDCFFDCDCDCFFDCDCDCFF5B5B
          D1FF5959CEFF5555CCFF5252C8FFDADADAFFDADADAFF999999FFB4B4B4FFFFFF
          FFFFFFFFFFFFE0E0E0FFFFFFFFFFFFFFFFFFDFDFDFFFFEFEFEFFFDFDFDFFDEDE
          DEFFFDFDFDFFFDFDFDFFDEDEDEFFFCFCFCFFFCFCFCFFA2A2A2FFA25013FFA250
          13FFA25013FFA25013FFA24F13FFA24F13FFA25013FFA24F13FFA25013FFA24F
          14FFA24F13FFA25013FFA25014FFA25014FFA25014FFA25013FFAC5B1FFFFACE
          8FFFFACC8CFFF9CA8AFFF9C888FFF8C786FFF8C684FFF7C482FFF7C180FFF6C0
          7EFFF6BE7CFFF6BD7AFFF5BB79FFF5BA77FFF5B976FFAC5C1FFFBF7335FFFFEE
          D0FFFFECCFFFFFEBCEFFFFEBCEFFFFEBCDFFFFEACDFFFEEACCFFFEE9CBFFFDE8
          CAFFFDE7C9FFFDE7C9FFFDE7C8FFFCE6C7FFFCE4C7FFBF7335FFD99255FFD992
          55FFD89355FFD99354FFD99355FFD99355FFD99355FFD99355FFD99355FFD992
          55FFD99255FFD99355FFD99354FFD99355FFD99255FFD99355FF000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          00002A2A2A2ACFCDCBD28B5737FF8B5736FF905C3BFF945F3DFF966140FF9862
          41FF996241FF986140FF97603FFF975F3EFF965E3DFF00000000000000002C2C
          2C2CDADADADAFCFAF8FF8A5838FFE9D7C5FFEDDDCCFFF0E1D3FFF1E3D6FFF2E5
          D8FFF2E6DAFFF3E7DCFFF3E7DCFFE2D7CCED98603FFF0000000000000000CECB
          C8D2FBF8F6FFF9F5F0FF8A5838FFEFE0D1FFF1E4D7FFDED3D7FF8A84CAFF3B3A
          B8FF3433B3FF7E7AC2FFDCD3D9FFE3D9CFED9A6442FF0000000000000000A97B
          58FFA77956FFA67855FF865434FFF2E6DAFFF4E8DEFF9C95D7FF9690D4FFD4CC
          DDFFD2CBDBFF847FC9FF807CC4FFE4DAD1ED9C6644FF0000000000000000AC80
          5EFFEFE0D2FFF2E6D9FFF3E8DEFFF5EAE0FFF5EDE4FF756FD9FFDBD4E3FF9A95
          D8FF938ED4FFD3CCDEFF3C3ABAFFE5DCD4ED9D6947FF0000000000000000AF85
          62FFF3E8DDFFF4EBE1FFF5EDE4FFCFB59EFFC5A68CFF7E74D2FFB59A99FF8D7B
          B0FF897AB3FFD8D1E2FF4D4AC5FFE6DED7ED9F6C49FF0000000000000000B28A
          68FFF6ECE5FFF7EFE8FFF8F1EBFFF9F2EDFFF9F3EEFFBFB8E9FFB9B3E8FFE2DB
          EAFFDED9E9FFA49EDEFF9C97DAFFE6E0D9EDA06E4CFF0000000000000000B58E
          6EFFF8F1EBFFF9F3EFFFFAF5F0FFFAF6F1FFFAF5F0FFF0EBEEFFC2BCEAFF948E
          E6FF8983E3FFAFAAE4FFE8E3EAFFE7E2DCEDA2714EFF0000000000000000B993
          72FFFAF6F2FFFBF7F3FFFBF7F3FFFBF6F3FFFAF6F2FFFAF6F2FFFAF6F1FFFAF5
          F1FFFAF5F0FFFAF5F0FFFAF4EFFFE7E3DEEDA37450FF0000000000000000BC98
          77FFFCF9F6FFA27656FFA17454FFA07352FF9E7151FF9C6F4EFF9C6D4DFFFBF7
          F3FFFBF7F3FFFBF7F2FFFAF6F2FFE8E5E0EDA67854FF0000000000000000BF9D
          7DFFFDFBF8FFFDFAF8FFFDFAF8FFFCFAF7FFFCFAF7FFFCF9F6FFFCF9F6FFFCF9
          F6FFFCF8F5FFFCF8F5FFFBF8F4FFE9E6E3EDA97C58FF0000000000000000C2A0
          81FFFDFCFAFFBF9C81FFBD9A7CFFBB9779FFB89376FFFDFBF9FFFDFBF8FFFDFA
          F8FFFCFAF7FFFCFAF7FFFCFAF7FFEAE7E5EDAC805DFF0000000000000000C5A5
          86FFFEFDFCFFFEFDFBFFFEFCFBFFFEFCFBFFFDFCFAFFFDFCFAFFFDFBFAFFFDFB
          F9FFFDFBF9FFFDFBF9FFFDFAF8FFEBE8E6EDAF8562FF0000000000000000C7A8
          8BFFFEFEFDFFD5BDA5FFD4BBA4FFD3B9A1FFD1B79EFFCFB49BFFCEB299FFFEFD
          FBFFFEFCFBFFFEFCFBFFFDFCFAFFEBEAE8EDB18967FF0000000000000000CAAC
          8FFFFFFFFFFFFFFFFEFFFFFFFEFFFFFEFEFFFFFEFEFFFFFEFEFFFEFEFDFFFEFE
          FDFFFEFDFDFFFEFDFDFFFEFDFCFFECEBEAEDB58E6CFF0000000000000000CCAF
          93FFCBAD90FFC9AA8EFFC8A98BFFC6A689FFC4A486FFC3A283FFC1A081FFC09E
          7EFFBE9B7BFFBD9979FFBB9776FFB99473FFB89271FF00000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000B0A
          0A241A19195A00000001010101090303031504040420050505290505052A0505
          0524030303190101010B000000020D0D0C5A0505052400000000000000001B1A
          1A5A494747FF302E2EB41817176B282727AD343231DF353434F7302F2FF82525
          25E21B1B1BB7101010771C1C1CB6252525FF0C0C0C5A00000000000000000000
          0003323030B4464343FF5B5858FE8A8786FFADAAA8FFC5C2BEFFC0BDBAFF9E9C
          99FF716E6CFF393938FE252525FF1B1B1BB80101010500000000000000010202
          020F28272795737170FEB9B7B6FFCFC6BAFFBEB19EFFB7A68EFFB9AA91FFBFB3
          A2FFC4BCB1FF979491FF4A4949FF171717AB0303031B00000002010101051413
          13505A5757E7C1BFBFFFC9BEAEFFBFAB8BFFDECAA8FFF5DBB2FFF6DCB0FFE2CA
          A3FFCAB797FFC2B8AAFF979491FF363635F10E0E0E6B0101010B0101010A2C2A
          2A9A9A9998FFD7CEC0FFBAA37FFFEDE2CFFFFFE9C5FFFFE8C2FFFFE7BEFFFFE5
          BAFFF0D6ACFFC9B696FFC4BCB1FF716E6DFF1B1B1BB7030303190202020E3F3D
          3DCFCBC9C9FFBBAA91FFD9C39FFFFFFDF9FFFFF0D9FFFFEBCCFFFFEAC7FFFFE7
          C1FFFFE5BAFFE1CAA2FFBEB3A0FF9F9C99FF262525E2050505240202020F4C49
          49F1EFEFEFFFA48E6BFFF3E0C1FFFFFDFAFFFFF8ECFF535353FF464646FF3B3A
          3AFF2F2F30FFF6DCB0FFB7A790FFC1BEBAFF302F2FF80505052A0202020D4C49
          49F1F1F0F0FFA28A67FFF3E2C6FFFFF9F1FFFFFDFBFF605F60FFFFEED5FFFFEC
          CCFFFFE8C2FFF5DCB2FFB5A48BFFC6C3C0FF353434F705050529010101093E3C
          3CCBCECDCDFFBAA88EFFD6C2A3FFFFF5E7FFFFFDFAFF6B6B6BFFFFF7ECFFFFEF
          D8FFFFE9C5FFDDC8A6FFBDAF9AFFAEABA9FF343232DF04040420010101062D2B
          2B9A9D9C9CFFDAD0C2FFB29A76FFEADAC1FFFFF5E7FF767676FFFFFDFAFFFFFD
          F9FFEDE1CEFFBDA887FFCEC6B9FF8A8887FF292828B503030315000000022725
          259F686666FFC9C8C8FFCBBEAAFFB29B76FFD6C2A3FFF3E1C6FFF3DFC1FFD8C2
          9EFFB8A07CFFC9BDABFFBAB8B7FF5F5C5CFF202020AF01010109000000003533
          33D6939191FF7D7C7CFFC9C8C8FFDAD0C2FFBAA88EFFA28B66FFA28C68FFBAA9
          8FFFD7CEC0FFC2C1C1FF787575FF807F7FFF242423D800000001000000003B39
          39D5CCCBCBFFADACACFF797777FF9D9C9CFFCECDCDFFF1F0F0FFF0F0F0FFCCCB
          CBFF9B9A99FF726F6FFF9A9999FFC1C1C1FF2A2929D600000000000000002725
          2580848382FFCDCCCCFFADACACFF5E5C5CFF424141E54C4949F14C4949F14744
          44E7636161FFA6A5A5FFC9C8C8FF81807FFF1D1C1C8000000000000000000808
          081A272525803B3A39D5353534D51D1C1C82060606230202020D0202020F0A09
          092628272785434141D6424040D5252424800706061A00000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          000000000000000000000000000100000004695E53DF2F2B2664000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000001595149DF2724206D08080841534B43E56D6257F1020202080000
          0000000000000000000000000000000000000000000000000000000000000000
          0001010101074F4843E33D3A37FA3E3E3EFF2E2E2EF3383531F10505052A0101
          010B5B5044DE29241E6300000000000000000000000000000000000000000000
          000217171759515151FD848585FFBFBFBFFF656566FF484849FF1F1F1FE90B0B
          0B68544B40DE615549F101010104000000000000000000000000000000000000
          0003363636AD8F8F90FFB3B4B4FFD0D0D0FF777777FF717071FF616161FF3333
          33FF1C1B1ACC4D453BE00202020E000000020000000000000000000000000404
          040B5C5B5BF6B0B1B1FFB3B4B4FFE5E5E5FF7C7B7CFF7A7A7BFF747475FF6E6D
          6EFF4D4C4DFF202020F108080851010101070000000000000000000000001F1F
          1F4B7B7C7CFFB3B4B4FFCCCDCDFFFDFDFDFF8E8D8EFF7C7B7CFF7C7B7CFF7876
          78FF717172FF4C4C4DFF141314AB0101010D0000000000000000000000004342
          439699999AFFE5E5E5FFD9D8D8FF959594FFADADACFFDBDADAFF7D7C7DFF7C7B
          7CFF7A797AFF6B6B6CFF202121E7020202110000000100000000000000005454
          54B2B1B1B1E5A2A1A1FE8F8F8FFF919090FF919090FF959494FFEEEDEDFFB4B4
          B4FF7B7A7BFF777677FF373737FF080808400000000300000000000000001212
          122447474796707070F47E7E7EFF8F8E8EFF959494FF959494FFF8F8F7FFF8F8
          F8FFF8F8F8FF878787FE565555FF171717A90101010600000000000000000000
          0000000000000707070F3232326F5E5E5EDB747474FF878686FFF5F5F5FFF5F5
          F5FFD4D4D4FF989898FF4B4C4BFF2A2A29ED0000000400000000000000000000
          0000000000000000000000000000000000001F1F1F484B4B4BBA797979FF6E6F
          6EFF414141CF2323237B090909390D0D0DE20000000400000000000000000000
          00000000000000000000000000000000000000000000000000001212123D1616
          16B7000000020000000000000000101017B403030C2F00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000F0F
          0FA00505123A05053B66060664B7050680F30101234700000000000000000000
          0000000000000000000000000000000000000000000000000000060627391111
          7EC7121297F00A0A64A803032E510000060C0000000000000000000000000000
          00000000000000000000000000000000000000000000000000000303131B0707
          324B000002030000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000100000009000000160000001000000004000000000000
          000000000000000000030000000B0000000B0000000400000000000000000000
          000000000001000000090F0804485C3119ED2B160B9500000019000000030000
          0000000000020305091F213B63D01A3153B40103041900000003000000000000
          000100000007110A0541724427F0A96A3DFF844D29FF2B160B97000000190000
          00030000000514243C86152741930F1D3177182F4CA80000000A000000000000
          0005130B063B7A4D2CEEB17040FFB06F3EFFAD6C3DFF824D29FF2B170B960000
          00180000000A203D65D4080E183F03050924254675EF0000000E00000004130C
          073383532FECBD845AFFBE906FFFC09679FFAA6F45FFA9693BFF834D29FF2B16
          0B9501020323264777FA0001010E0204071B274676F20000000D150D072D8C5C
          35EAC67F4AFFC79B7CFFFCFBFAFFEFE7E1FFEDE4DDFFB3825EFFAB6C3BFF834D
          29FF291A119C203F6BF20000000C09121D461F3A60C800000008825330DDD396
          67FFD08F5CFFD5B8A3FFF0E7E2FF9F6035FFE5D8CFFFC9AA93FFA96A3AFFAC6D
          3DFF7D4B2EFF22324DDE0000001D182D4CA0101E326E000000043A261564B178
          4BFDD69869FFD0A788FFEDE0D7FFAE7045FFFCFAF9FFB68A69FFD7BFAEFFA768
          3AFFAC6C3DFF824C29FF2A1C19A124446FE50102030F00000001000000013A26
          1664AF774AFDD49767FFC78756FFCDA589FFF0E7E2FF9A5D33FFF7F4F1FFD0B4
          A0FFB06E3DFFAD6D3DFF4F5161FF2C2D38BA0000001000000001000000000000
          000139261563B0784BFDD99A6BFFC89168FFE8DBD1FFEFE7E1FFFDFCFCFFC296
          7AFFB47B52FF957A6DFF606476FF6A3A1EFF0603022700000001000000000000
          0000000000013A261562B0774AFDD59768FFC89369FFCA9E7EFFC49471FFAB76
          51FF815234FF5F6880FFB78662FF6F3E20FF0603022700000001000000000000
          000000000000000000003A261562B0774AFDD99B6CFFD4915EFFCC834BFF8D57
          31FF281910677F5439FFC18252FF744223FF0603022300000001000000000000
          00000000000000000000000000003A261561B0784BFDDD9E6EFFDB9762FFB271
          40FF8E5730FFA8693BFFC27C46FF794726FF0503021900000001000000000000
          0000000000000000000000000000000000003A261560B0784BFDDD9E6EFFDE9F
          6FFFDB9E6EFFD69A6AFF9F6942FF2E1B0F6B0000000500000000000000000000
          000000000000000000000000000000000000000000003A251560946036FC935E
          35FF8E5A32FF88542EFC321F1166000000040000000100000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000010000000100000001000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000503
          020C3A1D1988793C1ADA934811ED7F3E16E046221A9D09040517010305070537
          6882075DB0D50263C4E70056B3D8073A6E8C01070D11000000000603020B6A38
          1DBFC4680FFFD16700FFD77A1AFFD6740EFFC96102FF6B3921D3145191C02D97
          F1FF3B9BF3FF2E91F0FF4A9CF1FF0373E6FF0756A5CC01070E1249260F7BD17D
          2BFFDF882DFFFCF4ECFFFDF9F4FFF8E6D4FFE2852BFF776957FF279FFBFF45A9
          F7FFEFF2F4FFEFF2F3FFE9EEF3FF4097F0FF006EE6FF073D74949E5A21DBE89D
          4FFFFDF7F0FFE5AD74FFE09E5BFFDB8E3FFFC8690BFF4B8BBDFF45B7FFFF30A1
          F5FF79C0F8FFE5EDF3FF007AECFF1783ECFF0074EBFF0661BEE3BF7530F0F2B7
          79FFFFFEFCFFFCF5EEFFFBF3EAFFE29A50FFBD640DFF4AA1E2FF52C0FFFF49B2
          F8FFEEF2F4FFF2F3F4FFD9E7F2FF0E83EDFF097DECFF0669CCEFAF6826DAF5B7
          78FFFEFAF4FFE8AA6AFFE59E55FFE49E56FFD7883CFF60A2D4FF5FCEFFFF4CB9
          FDFF9DD6FCFFEAF0F4FF2A9AF2FF2391F0FF1688F0FF0C65BFE35B300677F0B0
          70FFF9C791FFFEFBF7FFFEF8F2FFFAE9D8FFF4BB89FF7A7F78FF4DB5E1FF59BF
          FAFF7FD1FFFFEAF0F4FFE6EFF3FF4CAAF6FF1D8DEEFF09407696070300099757
          18BDF3B779FFFBC48AFFFFCA9AFFA3975EFF2A7210FF0C860BFF048A17FF127E
          30FF3996A3FF6FC9FFFF6DC4FDFF3AA4F3FF1363AED301080F14000000000A05
          000D6A360A88BB6F36D8857936FA008711FF04B41CFF6FD87AFFC8ECC4FF0AB3
          12FF008802FF2A868BFC3089DDDD114A86990109121700000000000000000000
          0000000000000B0B00240A761DF223C54BFFB6EABDFFEEF4E6FFEEF4E5FFA5E4
          ABFF13BC20FF017708F70311122F000000000000000000000000000000000000
          0000000000000027055A23B252FF3CC96BFF59CD76FF4AC862FFBEE6BBFFF3F6
          EAFF35C342FF01A30FFF0121006A000000000000000000000000000000000000
          00000000000006400B7A2EBE63FF29C265FF41C76BFFF0F5E9FFF5F7ECFF79D6
          86FF08B317FF07AD17FF0231058B000000000000000000000000000000000000
          0000000000000431075525B755FF3DC976FFF3F6EDFF98E1B4FFC3E9C7FF19B9
          3AFF12BA29FF09A318FF02210366000000000000000000000000000000000000
          000000000000010D0215169B2FE839C773FFBAECCFFFEDF4E8FFF0F5EAFFACE7
          BEFF1FC245FF097215F10008001B000000000000000000000000000000000000
          000000000000000000000430074C19AB3BF63FC773FF91E1B2FFD3EFD9FF3EC7
          70FF138E2EFA011C025900000000000000000000000000000000000000000000
          0000000000000000000000000000032005330A7118AD0E972BE70E912AE80860
          15B20218033B0000000000000000000000000000000000000000}
      end>
  end
end

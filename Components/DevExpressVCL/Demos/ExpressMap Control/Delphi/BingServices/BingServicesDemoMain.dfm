inherited BingServicesDemoMainForm: TBingServicesDemoMainForm
  Caption = 'Bing Services'
  ClientHeight = 455
  ClientWidth = 914
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inherited cxGroupBox2: TcxGroupBox
    Height = 455
    Width = 657
    inherited dxMapControl1: TdxMapControl
      Width = 650
      Height = 451
      CenterPoint.Longitude = -118.255629000000000000
      CenterPoint.Latitude = 34.158506000000000000
      OptionsBehavior.MapItemSelectMode = mismNone
      PopupMenu = PopupMenu1
      ZoomLevel = 14.000000000000000000
      OnMouseDown = dxMapControl1MouseDown
      OnMouseUp = dxMapControl1MouseUp
      object dxMapControl1ImageTileLayer1: TdxMapImageTileLayer
        ProviderClassName = 'TdxMapControlBingMapImageryDataProvider'
      end
      object dxMapControl1ItemLayer1: TdxMapItemLayer
        ProjectionClassName = 'TdxMapControlSphericalMercatorProjection'
        ItemStyle.AssignedValues = [mcsvBorderWidth, mcsvBorderColor]
        ItemStyle.BorderColor = -1627389697
        ItemStyle.BorderWidth = 4
        ItemStyleHot.AssignedValues = [mcsvBorderWidth, mcsvBorderColor]
        ItemStyleHot.BorderColor = -8355712
        ItemStyleHot.BorderWidth = 4
        ItemStyleSelected.AssignedValues = [mcsvBorderWidth]
        ItemStyleSelected.BorderWidth = 2
        object miManeuverPoint: TdxMapDot
          Style.AssignedValues = [mcsvColor, mcsvBorderColor]
          Style.BorderColor = -1627389952
          Style.Color = -1
          Visible = False
          Size = 2
        end
        object miNewPointPointer: TdxMapDot
          Style.AssignedValues = [mcsvColor, mcsvBorderWidth, mcsvBorderColor]
          Style.BorderColor = -1
          Style.BorderWidth = 1
          Style.Color = -65536
          Visible = False
          Size = 4
        end
      end
      object dxMapControl1BingMapGeoCodingDataProvider1: TdxMapControlBingMapGeoCodingDataProvider
        OnResponse = dxMapControl1BingMapGeoCodingDataProvider1Response
      end
      object dxMapControl1BingMapReverseGeoCodingDataProvider1: TdxMapControlBingMapReverseGeoCodingDataProvider
        OnResponse = dxMapControl1BingMapReverseGeoCodingDataProvider1Response
      end
      object dxMapControl1BingMapRouteDataProvider1: TdxMapControlBingMapRouteDataProvider
        OnResponse = dxMapControl1BingMapRouteDataProvider1Response
        MaxSolutions = 3
      end
      object dxMapControl1BingMapMajorRoadRouteDataProvider1: TdxMapControlBingMapMajorRoadRouteDataProvider
        OnResponse = dxMapControl1BingMapMajorRoadRouteDataProvider1Response
      end
    end
    object cxImage1: TcxImage
      Left = 453
      Top = 9
      TabStop = False
      Anchors = [akTop, akRight]
      AutoSize = True
      Properties.Center = False
      Properties.FitMode = ifmNormal
      Properties.GraphicClassName = 'TdxSmartImage'
      Properties.ShowFocusRect = False
      Style.BorderStyle = ebsNone
      StyleFocused.BorderStyle = ebsNone
      StyleHot.BorderStyle = ebsNone
      TabOrder = 3
      Transparent = True
      Height = 43
      Width = 182
    end
    object cxTextEdit1: TcxTextEdit
      Left = 465
      Top = 20
      Anchors = [akTop, akRight]
      Properties.OnChange = cxTextEdit1PropertiesChange
      Style.BorderStyle = ebsUltraFlat
      TabOrder = 1
      TextHint = 'Enter search location'
      Width = 161
    end
    object cxSplitter1: TcxSplitter
      Left = 652
      Top = 2
      Width = 3
      Height = 451
      AlignSplitter = salRight
      AutoSnap = True
      MinSize = 240
      Control = cxGroupBox1
    end
  end
  object cxGroupBox1: TcxGroupBox [1]
    Left = 657
    Top = 0
    Align = alRight
    PanelStyle.Active = True
    TabOrder = 1
    Height = 455
    Width = 257
    object cxListBox1: TcxListBox
      Left = 6
      Top = 47
      Width = 243
      Height = 370
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = cxListBox1Click
    end
    object cxButton1: TcxButton
      Left = 26
      Top = 423
      Width = 111
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Show all route points'
      TabOrder = 1
      OnClick = cxButton1Click
    end
    object cxButton2: TcxButton
      Left = 143
      Top = 423
      Width = 106
      Height = 25
      Action = actClear
      Anchors = [akRight, akBottom]
      TabOrder = 2
    end
    object cxComboBox1: TcxComboBox
      Left = 6
      Top = 20
      Anchors = [akLeft, akTop, akRight]
      Properties.DropDownListStyle = lsFixedList
      Properties.OnEditValueChanged = cxComboBox1PropertiesEditValueChanged
      TabOrder = 3
      Width = 243
    end
  end
  inherited cxLookAndFeelController1: TcxLookAndFeelController
    Left = 560
    Top = 160
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 504
    Top = 240
    object Setstartpoint1: TMenuItem
      Action = actAddStartPoint
    end
    object Setasstartpoint1: TMenuItem
      Action = actSetAsStartPoint
    end
    object Changestartpoint1: TMenuItem
      Action = actChangeStartPoint
    end
    object Addroutepoint1: TMenuItem
      Action = actAddEndPoint
    end
    object Setasroutepoint1: TMenuItem
      Action = actSetAsEndPoint
    end
    object Showroutefrommajorroads1: TMenuItem
      Action = actRouteFromMajorRoads
    end
    object Deletepoint1: TMenuItem
      Action = actDeletePoint
    end
  end
  object ActionList1: TActionList
    Left = 600
    Top = 240
    object actAddStartPoint: TAction
      Caption = 'Set start point'
      OnExecute = actAddStartPointExecute
    end
    object actAddEndPoint: TAction
      Caption = 'Add end point'
      OnExecute = actAddEndPointExecute
    end
    object actDeletePoint: TAction
      Caption = 'Delete point'
      OnExecute = actDeletePointExecute
    end
    object actChangeStartPoint: TAction
      Caption = 'Change start point'
      OnExecute = actChangeStartPointExecute
    end
    object actClear: TAction
      Caption = 'Clear route points'
      OnExecute = actClearExecute
    end
    object actSetAsStartPoint: TAction
      Caption = 'Set as start point'
      OnExecute = actSetAsStartPointExecute
    end
    object actSetAsEndPoint: TAction
      Caption = 'Set as end point'
      OnExecute = actSetAsEndPointExecute
    end
    object actRouteFromMajorRoads: TAction
      Caption = 'Show route from major roads'
      OnExecute = actRouteFromMajorRoadsExecute
    end
  end
  object dxScreenTipRepository1: TdxScreenTipRepository
    Left = 784
    Top = 216
    object dxScreenTipRepository1ScreenTip1: TdxScreenTip
      Header.Text = 'dxScreenTipRepository1ScreenTip1'
      UseHintAsHeader = True
    end
  end
  object cxHintStyleController1: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    Left = 776
    Top = 288
  end
end

inherited dxRangeControlDemoForm: TdxRangeControlDemoForm
  Caption = 'ExpressEditors RangeControl Demo'
  ClientHeight = 421
  ClientWidth = 734
  Constraints.MinHeight = 479
  Constraints.MinWidth = 750
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  inherited lbDescription: TLabel
    Width = 734
    Height = 0
    Align = alNone
    AutoSize = False
    Visible = False
  end
  object cxGroupBox5: TcxGroupBox [1]
    Left = 0
    Top = 0
    Align = alClient
    PanelStyle.Active = True
    Style.BorderStyle = ebsNone
    TabOrder = 0
    Transparent = True
    Height = 421
    Width = 734
    object cxGroupBox1: TcxGroupBox
      Left = 10
      Top = 13
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Numeric client'
      TabOrder = 0
      Height = 131
      Width = 714
      object rcNumericClient: TdxRangeControl
        Left = 16
        Top = 24
        Width = 420
        Height = 90
        Anchors = [akLeft, akTop, akRight, akBottom]
        ClientPropertiesClassName = 'TdxRangeControlNumericClientProperties'
        ClientProperties.MaxValue = 10
        ClientProperties.MinValue = 0
        ClientProperties.ScaleInterval = 1
        SelectedRangeMaxValue = 0
        SelectedRangeMinValue = 0
        TabOrder = 0
        VisibleRangeMaxScaleFactor = 10.000000000000000000
        VisibleRangeMaxValue = 10.000000000000000000
        VisibleRangeMinValue = 0.000000000000000000
        OnDrawContent = rcNumericClientDrawContent
      end
      object cxGroupBox6: TcxGroupBox
        Left = 442
        Top = 15
        Anchors = [akTop, akRight]
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        TabOrder = 1
        Transparent = True
        Height = 110
        Width = 270
        object rgNumberClientContentType: TcxRadioGroup
          Left = 3
          Top = 9
          Alignment = alCenterCenter
          Properties.Columns = 2
          Properties.Items = <
            item
              Caption = 'Line'
            end
            item
              Caption = 'Discrete'
            end>
          Properties.OnEditValueChanged = rgNumberClientContentTypePropertiesEditValueChanged
          ItemIndex = 1
          Style.BorderStyle = ebsNone
          TabOrder = 0
          Transparent = True
          Height = 22
          Width = 252
        end
        object tbNumericClient: TcxTrackBar
          Left = 88
          Top = 35
          Position = 1
          Properties.AutoSize = False
          Properties.Max = 4
          Properties.Min = 1
          Properties.ThumbStep = cxtsJump
          Properties.OnChange = tbNumericClientPropertiesChange
          TabOrder = 1
          Transparent = True
          Height = 31
          Width = 167
        end
        object cxLabel1: TcxLabel
          Left = 11
          Top = 39
          Caption = 'Scale interval:'
          Transparent = True
        end
      end
    end
    object cxGroupBox2: TcxGroupBox
      Left = 10
      Top = 147
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Date client'
      TabOrder = 1
      Height = 131
      Width = 714
      object rcDateTimeClient: TdxRangeControl
        Left = 16
        Top = 24
        Width = 420
        Height = 90
        Anchors = [akLeft, akTop, akRight, akBottom]
        ClientPropertiesClassName = 'TdxRangeControlDateTimeClientProperties'
        ClientProperties.MaxValue = 42706d
        ClientProperties.MinValue = 42702d
        ClientProperties.ScaleInterval = 1
        ClientProperties.Scales.Day.Active = True
        SelectedRangeMaxValue = 42702d
        SelectedRangeMinValue = 42702d
        TabOrder = 0
        VisibleRangeMaxScaleFactor = 10.000000000000000000
        VisibleRangeMaxValue = 42706d
        VisibleRangeMinValue = 42702d
        OnDrawContent = rcDateTimeClientDrawContent
      end
      object cxGroupBox7: TcxGroupBox
        Left = 442
        Top = 15
        Anchors = [akTop, akRight]
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        TabOrder = 1
        Transparent = True
        Height = 110
        Width = 270
        object rgDateTimeClientContentType: TcxRadioGroup
          Left = 3
          Top = 9
          Alignment = alCenterCenter
          Properties.Columns = 2
          Properties.Items = <
            item
              Caption = 'Line'
            end
            item
              Caption = 'Area'
            end>
          Properties.OnEditValueChanged = rgDateTimeClientContentTypePropertiesEditValueChanged
          ItemIndex = 1
          Style.BorderStyle = ebsNone
          TabOrder = 0
          Transparent = True
          Height = 22
          Width = 252
        end
        object tbDateTimeClient: TcxTrackBar
          Left = 88
          Top = 35
          Position = 1
          Properties.AutoSize = False
          Properties.Max = 4
          Properties.Min = 1
          Properties.ThumbStep = cxtsJump
          Properties.OnChange = tbDateTimeClientPropertiesChange
          TabOrder = 1
          Transparent = True
          Height = 31
          Width = 167
        end
        object cxLabel2: TcxLabel
          Left = 11
          Top = 39
          Caption = 'Scale interval:'
          Transparent = True
        end
      end
    end
    object cxGroupBox3: TcxGroupBox
      Left = 10
      Top = 281
      Anchors = [akLeft, akTop, akRight]
      Caption = 'DateHeader client'
      TabOrder = 2
      Height = 131
      Width = 714
      object rcDateTimeHeaderClient: TdxRangeControl
        Left = 16
        Top = 24
        Width = 420
        Height = 90
        Anchors = [akLeft, akTop, akRight, akBottom]
        ClientPropertiesClassName = 'TdxRangeControlDateTimeHeaderClientProperties'
        ClientProperties.MaxValue = 42706d
        ClientProperties.MinValue = 42702d
        ClientProperties.Scales.Day.Active = True
        ClientProperties.Scales.Day.Visible = True
        ClientProperties.Scales.Week.Visible = True
        ClientProperties.Scales.Year.Visible = True
        SelectedRangeMaxValue = 42703d
        SelectedRangeMinValue = 42702d
        TabOrder = 0
        VisibleRangeMaxScaleFactor = 10.000000000000000000
        VisibleRangeMaxValue = 42706d
        VisibleRangeMinValue = 42702d
        OnDrawContent = rcDateTimeHeaderClientDrawContent
      end
      object cxGroupBox4: TcxGroupBox
        Left = 442
        Top = 15
        Anchors = [akTop, akRight]
        PanelStyle.Active = True
        Style.BorderStyle = ebsNone
        TabOrder = 1
        Transparent = True
        Height = 110
        Width = 270
        object chbAutoFormatScaleCaptions: TcxCheckBox
          Left = 11
          Top = 9
          Caption = 'Auto format scale captions'
          Properties.OnEditValueChanged = chbAutoFormatScaleCaptionsPropertiesEditValueChanged
          State = cbsChecked
          TabOrder = 0
          Transparent = True
        end
        object cxCheckComboBox1: TcxCheckComboBox
          Left = 88
          Top = 36
          Properties.Items = <
            item
              Description = 'Day'
            end
            item
              Description = 'Week'
            end
            item
              Description = 'Month'
            end
            item
              Description = 'Quarter'
            end
            item
              Description = 'Year'
            end>
          Properties.OnEditValueChanged = cxCheckComboBox1PropertiesEditValueChanged
          EditValue = 19
          TabOrder = 1
          Width = 161
        end
        object cxLabel3: TcxLabel
          Left = 11
          Top = 38
          Caption = 'Scales:'
          Transparent = True
        end
      end
    end
  end
  inherited mmMain: TMainMenu
    object Options1: TMenuItem [1]
      Caption = '&Options'
      object Animation1: TMenuItem
        AutoCheck = True
        Caption = '&Animation'
        Checked = True
        OnClick = Animation1Click
      end
      object ShowRuler1: TMenuItem
        AutoCheck = True
        Caption = '&Show Ruler'
        Checked = True
        OnClick = ShowRuler1Click
      end
      object ShowZoomscrollbar1: TMenuItem
        AutoCheck = True
        Caption = 'Show &Zoom&&Scroll Bar'
        Checked = True
        OnClick = ShowZoomscrollbar1Click
      end
    end
  end
end

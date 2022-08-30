object WizardControlDemoSetupForm: TWizardControlDemoSetupForm
  Left = 317
  Top = 126
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Demo settings'
  ClientHeight = 350
  ClientWidth = 346
  Color = clBtnFace
  Constraints.MinHeight = 271
  Constraints.MinWidth = 297
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cbSkinForm: TcxCheckBox
    Left = 7
    Top = 322
    Anchors = [akLeft, akBottom]
    Caption = 'Skinned forms'
    Properties.Alignment = taLeftJustify
    Style.TransparentBorder = False
    TabOrder = 1
    Transparent = True
    OnClick = cbSkinFormClick
    Width = 194
  end
  object rgTransitionEffect: TcxRadioGroup
    Left = 207
    Top = 24
    Anchors = [akTop, akRight]
    Caption = ' Page animation effect '
    Properties.Items = <
      item
        Caption = 'None'
      end
      item
        Caption = 'Fade'
      end
      item
        Caption = 'Slide'
      end>
    ItemIndex = 1
    TabOrder = 2
    Transparent = True
    Height = 97
    Width = 131
  end
  object btnStartDemo: TcxButton
    Left = 207
    Top = 319
    Width = 131
    Height = 24
    Anchors = [akRight, akBottom]
    Caption = 'Start demo'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object rgViewStyle: TcxRadioGroup
    Left = 207
    Top = 132
    Anchors = [akTop, akRight]
    Caption = ' View style '
    Properties.Items = <
      item
        Caption = 'Aero'
      end
      item
        Caption = 'Wizard 97'
      end>
    ItemIndex = 0
    TabOrder = 4
    Transparent = True
    Height = 76
    Width = 131
  end
  object lbChooseSkin: TcxLabel
    Left = 6
    Top = 6
    Caption = 'Select look && feel:'
    Transparent = True
  end
  object tvSkins: TcxTreeView
    Left = 8
    Top = 24
    Width = 193
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    ParentFont = False
    TabOrder = 0
    HideSelection = False
    ReadOnly = True
    ShowRoot = False
    OnChange = tvSkinsChange
    OnCustomDrawItem = tvSkinsCustomDrawItem
  end
end

{=======================================================================================================================
  ComboBoxesFrame Unit

  Raize Components - Demo Program Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.
=======================================================================================================================}

{$I RCDemo.inc}

unit ComboBoxesFrame;

interface

uses
  Forms,
  RzPanel,
  RzRadGrp,
  RzCmboBx,
  Controls,
  StdCtrls,
  RzLabel,
  RzRadChk,
  RzButton,
  Classes,
  ExtCtrls,
  RzCommon,
  RzBorder, ImgList, RzStatus;

type
  TFmeComboBoxes = class(TFrame)
    grpTRzComboBox: TRzGroupBox;
    cbxStandard: TRzComboBox;
    optDropDown: TRzRadioButton;
    optDropDownList: TRzRadioButton;
    grpTRzMRUComboBox: TRzGroupBox;
    grpTRzColorComboBox: TRzGroupBox;
    grpTRzFontComboBox: TRzGroupBox;
    cbxColors: TRzColorComboBox;
    cbxFonts: TRzFontComboBox;
    cbxMRU: TRzMRUComboBox;
    OptShowSysColors: TRzCheckBox;
    OptShowColorNames: TRzCheckBox;
    OptShowDefault: TRzCheckBox;
    OptShowCustom: TRzCheckBox;
    GrpFontDevice: TRzRadioGroup;
    GrpFontType: TRzRadioGroup;
    GrpShowStyle: TRzRadioGroup;
    RzLabel1: TRzLabel;
    chkAllowEdit: TRzCheckBox;
    RzRegIniFile1: TRzRegIniFile;
    pnlHeader: TRzPanel;
    RzBorder1: TRzBorder;
    RzBorder2: TRzBorder;
    RzBorder3: TRzBorder;
    RzBorder4: TRzBorder;
    pnlPreview: TRzPanel;
    RzGroupBox1: TRzGroupBox;
    RzBorder5: TRzBorder;
    ImageList1: TImageList;
    RzGroupBox2: TRzGroupBox;
    RzBorder6: TRzBorder;
    cbxStates: TRzComboBox;
    cbxImages: TRzImageComboBox;
    chkAutoComplete: TRzCheckBox;
    RzLabel2: TRzLabel;
    stsStateAbbr: TRzStatusPane;
    RzLabel3: TRzLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ComboStyleChange(Sender: TObject);
    procedure OptShowSysColorsClick(Sender: TObject);
    procedure OptShowColorNamesClick(Sender: TObject);
    procedure OptShowDefaultClick(Sender: TObject);
    procedure OptShowCustomClick(Sender: TObject);
    procedure GrpFontDeviceClick(Sender: TObject);
    procedure GrpFontTypeClick(Sender: TObject);
    procedure GrpShowStyleClick(Sender: TObject);
    procedure cbxFontsChange(Sender: TObject);
    procedure cbxColorsChange(Sender: TObject);
    procedure chkAllowEditClick(Sender: TObject);
    procedure chkAutoCompleteClick(Sender: TObject);
    procedure cbxStatesChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Init;
    procedure UpdateVisualStyle( VS: TRzVisualStyle; GCS: TRzGradientColorStyle );
  end;


implementation

{$R *.dfm}

uses
  MainForm;

const
  RC_SettingsKey = 'Software\Raize\Raize Components\2.5';


procedure TFmeComboBoxes.Init;
begin
//  ParentBackground := False;

  cbxMRU.MruPath := RC_SettingsKey;

  cbxImages.AddItem( 'Embarcadero Products', 0, 0 );
  cbxImages.AddItem( 'Developer Tools', 9, 1 );
  cbxImages.AddItem( 'Konopka Signature VCL Controls', 7, 2 );
  cbxImages.AddItem( 'CodeSite', 3, 2 );
  cbxImages.AddItem( 'Consumer Products', 10, 1 );
  cbxImages.AddItem( 'BabyType', 2, 2 );
  cbxImages.AddItem( 'ScratchPad', 8, 2 );
  cbxImages.ItemIndex := 2;

  cbxStates.Value := 'IL';
  stsStateAbbr.Caption := 'IL';
end;


procedure TFmeComboBoxes.UpdateVisualStyle( VS: TRzVisualStyle;
                                            GCS: TRzGradientColorStyle );
begin
  pnlHeader.VisualStyle := VS;
  pnlHeader.GradientColorStyle := GCS;
end;


procedure TFmeComboBoxes.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FrmMain.RestoreMainNotes;
  Action := caFree;
end;

procedure TFmeComboBoxes.ComboStyleChange(Sender: TObject);
begin
  if OptDropDown.Checked then
    cbxStandard.Style := csDropDown
  else
    cbxStandard.Style := csDropDownList;
  ChkAllowEdit.Enabled := cbxStandard.Style = csDropDown;
  cbxStandard.ItemIndex := -1;
  cbxStandard.ClearSearchString;
end;

procedure TFmeComboBoxes.OptShowSysColorsClick(Sender: TObject);
begin
  cbxColors.ShowSysColors := OptShowSysColors.Checked;
end;

procedure TFmeComboBoxes.OptShowColorNamesClick(Sender: TObject);
begin
  cbxColors.ShowColorNames := OptShowColorNames.Checked;
end;

procedure TFmeComboBoxes.OptShowDefaultClick(Sender: TObject);
begin
  cbxColors.ShowDefaultColor := OptShowDefault.Checked;
end;

procedure TFmeComboBoxes.OptShowCustomClick(Sender: TObject);
begin
  cbxColors.ShowCustomColor := OptShowCustom.Checked;
end;

procedure TFmeComboBoxes.GrpFontDeviceClick(Sender: TObject);
begin
  cbxFonts.FontDevice := TRzFontDevice( GrpFontDevice.ItemIndex );
end;

procedure TFmeComboBoxes.GrpFontTypeClick(Sender: TObject);
begin
  cbxFonts.FontType := TRzFontType( GrpFontType.ItemIndex );
end;

procedure TFmeComboBoxes.GrpShowStyleClick(Sender: TObject);
begin
  cbxFonts.ShowStyle := TRzShowStyle( GrpShowStyle.ItemIndex );
  if cbxFonts.ShowStyle = ssFontNameAndSample then
    cbxFonts.DropDownWidth := 300
  else
    cbxFonts.DropDownWidth := cbxFonts.Width;
end;

procedure TFmeComboBoxes.cbxFontsChange(Sender: TObject);
begin
  if cbxFonts.SelectedFont <> nil then
    PnlPreview.Font.Name := cbxFonts.SelectedFont.Name;
end;

procedure TFmeComboBoxes.cbxColorsChange(Sender: TObject);
begin
  pnlPreview.Color := cbxColors.SelectedColor;
end;


procedure TFmeComboBoxes.chkAllowEditClick(Sender: TObject);
begin
  cbxStandard.AllowEdit := chkAllowEdit.Checked;
end;

procedure TFmeComboBoxes.chkAutoCompleteClick(Sender: TObject);
begin
  cbxStandard.AutoComplete := chkAutoComplete.Checked;
end;

procedure TFmeComboBoxes.cbxStatesChange(Sender: TObject);
begin
  stsStateAbbr.Caption := cbxStates.Value;
end;

end.


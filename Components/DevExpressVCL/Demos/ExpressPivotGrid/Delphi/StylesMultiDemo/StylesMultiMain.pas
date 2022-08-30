unit StylesMultiMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DemoBasicMain, cxControls, cxCustomPivotGrid, cxDBPivotGrid,
  cxLookAndFeels, Menus, StdCtrls, DemoBasicDM, cxLookAndFeelPainters,
  ExtCtrls, cxListBox, cxGroupBox, cxRadioGroup, cxButtons, cxContainer,
  cxEdit, cxStyles, cxClasses, cxStyleSheetEditor, cxPivotGridStyleSheetsPreview,
  cxGraphics, cxCustomData;

type
  TfrmStylesMulti = class(TfrmDemoBasicMain)
    DBPivotGrid: TcxDBPivotGrid;
    pnlLeft: TPanel;
    gbUserDefined: TcxGroupBox;
    cbUserStyleSheets: TComboBox;
    btnLoad: TcxButton;
    btnSave: TcxButton;
    btnEdit: TcxButton;
    RadioGroup: TcxRadioGroup;
    gbPredefined: TcxGroupBox;
    lbPredefinedStyleSheets: TcxListBox;
    pnlCurrentStyleSheet: TPanel;
    Splitter: TSplitter;
    PivotGridPurchaseDate: TcxDBPivotGridField;
    PivotGridPaymentType: TcxDBPivotGridField;
    PivotGridQuantity: TcxDBPivotGridField;
    PivotGridCarName: TcxDBPivotGridField;
    PivotGridUnitPrice: TcxDBPivotGridField;
    PivotGridCompanyName: TcxDBPivotGridField;
    PivotGridPaymentAmount: TcxDBPivotGridField;
    srPredefined: TcxStyleRepository;
    ClassicBackground: TcxStyle;
    ClassicContent: TcxStyle;
    ClassicHeader: TcxStyle;
    ClassicInactive: TcxStyle;
    ClassicSelection: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle13: TcxStyle;
    cxStyle14: TcxStyle;
    cxStyle17: TcxStyle;
    cxStyle21: TcxStyle;
    cxStyle24: TcxStyle;
    cxStyle25: TcxStyle;
    cxStyle28: TcxStyle;
    cxStyle32: TcxStyle;
    cxStyle35: TcxStyle;
    cxStyle36: TcxStyle;
    cxStyle39: TcxStyle;
    cxStyle43: TcxStyle;
    cxStyle46: TcxStyle;
    cxStyle47: TcxStyle;
    cxStyle50: TcxStyle;
    cxStyle54: TcxStyle;
    cxStyle57: TcxStyle;
    cxStyle58: TcxStyle;
    cxStyle61: TcxStyle;
    cxStyle65: TcxStyle;
    cxStyle68: TcxStyle;
    cxStyle69: TcxStyle;
    cxStyle72: TcxStyle;
    cxStyle76: TcxStyle;
    cxStyle79: TcxStyle;
    cxStyle80: TcxStyle;
    cxStyle83: TcxStyle;
    cxStyle87: TcxStyle;
    cxStyle90: TcxStyle;
    cxStyle91: TcxStyle;
    cxStyle94: TcxStyle;
    cxStyle98: TcxStyle;
    cxStyle101: TcxStyle;
    cxStyle102: TcxStyle;
    cxStyle105: TcxStyle;
    cxStyle109: TcxStyle;
    cxStyle112: TcxStyle;
    cxStyle113: TcxStyle;
    cxStyle116: TcxStyle;
    cxStyle120: TcxStyle;
    cxStyle123: TcxStyle;
    cxStyle124: TcxStyle;
    cxStyle127: TcxStyle;
    cxStyle131: TcxStyle;
    cxStyle134: TcxStyle;
    cxStyle135: TcxStyle;
    cxStyle138: TcxStyle;
    cxStyle142: TcxStyle;
    cxStyle145: TcxStyle;
    cxStyle146: TcxStyle;
    cxStyle149: TcxStyle;
    cxStyle153: TcxStyle;
    cxStyle156: TcxStyle;
    cxStyle157: TcxStyle;
    cxStyle160: TcxStyle;
    cxStyle164: TcxStyle;
    cxStyle167: TcxStyle;
    cxStyle168: TcxStyle;
    cxStyle171: TcxStyle;
    cxStyle175: TcxStyle;
    cxStyle178: TcxStyle;
    cxStyle179: TcxStyle;
    cxStyle182: TcxStyle;
    cxStyle186: TcxStyle;
    cxStyle189: TcxStyle;
    cxStyle190: TcxStyle;
    cxStyle193: TcxStyle;
    cxStyle197: TcxStyle;
    cxStyle200: TcxStyle;
    cxStyle201: TcxStyle;
    cxStyle204: TcxStyle;
    cxStyle208: TcxStyle;
    cxStyle211: TcxStyle;
    cxStyle212: TcxStyle;
    cxStyle215: TcxStyle;
    cxStyle219: TcxStyle;
    cxStyle222: TcxStyle;
    cxStyle223: TcxStyle;
    cxStyle226: TcxStyle;
    cxStyle230: TcxStyle;
    cxStyle233: TcxStyle;
    cxStyle234: TcxStyle;
    cxStyle237: TcxStyle;
    cxStyle241: TcxStyle;
    cxStyle244: TcxStyle;
    cxStyle245: TcxStyle;
    cxStyle248: TcxStyle;
    cxStyle252: TcxStyle;
    cxStyle255: TcxStyle;
    cxStyle256: TcxStyle;
    cxStyle259: TcxStyle;
    cxStyle263: TcxStyle;
    cxStyle266: TcxStyle;
    cxStyle267: TcxStyle;
    cxStyle270: TcxStyle;
    cxStyle274: TcxStyle;
    cxStyle277: TcxStyle;
    cxStyle278: TcxStyle;
    cxStyle281: TcxStyle;
    cxStyle285: TcxStyle;
    cxStyle286: TcxStyle;
    cxStyle288: TcxStyle;
    cxStyle289: TcxStyle;
    cxStyle292: TcxStyle;
    cxStyle296: TcxStyle;
    cxStyle297: TcxStyle;
    cxStyle299: TcxStyle;
    cxStyle300: TcxStyle;
    cxStyle303: TcxStyle;
    cxStyle307: TcxStyle;
    cxStyle308: TcxStyle;
    cxStyle310: TcxStyle;
    cxStyle311: TcxStyle;
    cxStyle314: TcxStyle;
    cxStyle318: TcxStyle;
    cxStyle319: TcxStyle;
    cxStyle321: TcxStyle;
    cxStyle322: TcxStyle;
    cxStyle325: TcxStyle;
    cxStyle329: TcxStyle;
    cxStyle330: TcxStyle;
    cxStyle332: TcxStyle;
    cxStyle333: TcxStyle;
    cxStyle336: TcxStyle;
    cxStyle340: TcxStyle;
    cxStyle341: TcxStyle;
    cxStyle343: TcxStyle;
    cxStyle344: TcxStyle;
    cxStyle347: TcxStyle;
    cxStyle351: TcxStyle;
    cxStyle354: TcxStyle;
    cxStyle355: TcxStyle;
    cxStyle358: TcxStyle;
    cxStyle362: TcxStyle;
    PivotGridStyleSheetDevExpress: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat1: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat2: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat3: TcxPivotGridStyleSheet;
    PivotGridStyleSheetUserFormat4: TcxPivotGridStyleSheet;
    PivotGridStyleSheetBrick: TcxPivotGridStyleSheet;
    PivotGridStyleSheetDesert: TcxPivotGridStyleSheet;
    PivotGridStyleSheetEggplant: TcxPivotGridStyleSheet;
    PivotGridStyleSheetLilac: TcxPivotGridStyleSheet;
    PivotGridStyleSheetMaple: TcxPivotGridStyleSheet;
    PivotGridStyleSheetMarinehighcolor: TcxPivotGridStyleSheet;
    PivotGridStyleSheetPlumhighcolor: TcxPivotGridStyleSheet;
    PivotGridStyleSheetPumpkinlarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRainyDay: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRedWhiteandBlueVGA: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRose: TcxPivotGridStyleSheet;
    PivotGridStyleSheetRoselarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetSlate: TcxPivotGridStyleSheet;
    PivotGridStyleSheetSpruce: TcxPivotGridStyleSheet;
    PivotGridStyleSheetStormVGA: TcxPivotGridStyleSheet;
    PivotGridStyleSheetTealVGA: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWheat: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsClassic: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsClassiclarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsStandard: TcxPivotGridStyleSheet;
    PivotGridStyleSheetWindowsStandardlarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast1: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast1large: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast2: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrast2large: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastBlack: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastBlacklarge: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastWhite: TcxPivotGridStyleSheet;
    PivotGridStyleSheetHighContrastWhitelarge: TcxPivotGridStyleSheet;
    OpenDialog: TOpenDialog;
    SaveDialog1: TSaveDialog;
    srUserDefined: TcxStyleRepository;
    cxStyle378: TcxStyle;
    cxStyle379: TcxStyle;
    cxStyle380: TcxStyle;
    cxStyle381: TcxStyle;
    cxStyle382: TcxStyle;
    cxStyle383: TcxStyle;
    cxStyle384: TcxStyle;
    cxStyle385: TcxStyle;
    cxStyle386: TcxStyle;
    cxStyle387: TcxStyle;
    cxStyle388: TcxStyle;
    cxStyle389: TcxStyle;
    cxStyle390: TcxStyle;
    cxStyle391: TcxStyle;
    cxStyle392: TcxStyle;
    cxStyle393: TcxStyle;
    cxStyle394: TcxStyle;
    cxStyle395: TcxStyle;
    cxStyle396: TcxStyle;
    cxStyle397: TcxStyle;
    cxStyle398: TcxStyle;
    cxStyle399: TcxStyle;
    cxStyle400: TcxStyle;
    cxStyle401: TcxStyle;
    cxStyle402: TcxStyle;
    cxStyle403: TcxStyle;
    cxStyle404: TcxStyle;
    cxStyle405: TcxStyle;
    cxPivotGridStyleSheet1: TcxPivotGridStyleSheet;
    cxPivotGridStyleSheet2: TcxPivotGridStyleSheet;
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure RadioGroupClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure cbUserStyleSheetsChange(Sender: TObject);
    procedure lbPredefinedStyleSheetsClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  protected
    procedure ChangeVisibility(AType: Integer);
    procedure ClearUserDefinedStyleSheets;
    procedure CreatePredefinedStyleSheetsList;
    procedure CreateUserStyleSheetsList;
    function GetCurrentStyleSheet: TcxPivotGridStyleSheet;
    function GetDefaultLookAndFeelKind: TcxLookAndFeelKind; override;
    procedure LoadUserDefinedStyleSheets(AFileName: TFileName);
    procedure SaveUserDefinedStyleSheets(AFileName: TFileName);
    procedure SetPredefinedStyleSheets;
    procedure SetUserDefinedStyleSheets;
    procedure UpdateGridStyleSheets(const AStyleSheet: TcxPivotGridStyleSheet);
    function GetPivotGrid: TcxCustomPivotGrid; override;
  public
    { Public declarations }
  end;

var
  frmStylesMulti: TfrmStylesMulti;

implementation

{$R *.dfm}


const
  cNone = 0;
  cPredefined = 1;
  cUserDefined = 2;

function TfrmStylesMulti.GetPivotGrid: TcxCustomPivotGrid;
begin
  Result := DBPivotGrid;
end;

procedure TfrmStylesMulti.CreateUserStyleSheetsList;
var
  I: Integer;
begin
  cbUserStyleSheets.Clear;
  with srUserDefined do
  begin
    for I := 0 to StyleSheetCount - 1 do
      cbUserStyleSheets.Items.AddObject(StyleSheets[I].Caption, StyleSheets[I]);
  end;
  cbUserStyleSheets.ItemIndex := 0;
end;

procedure TfrmStylesMulti.CreatePredefinedStyleSheetsList;
var
  I: Integer;
begin
  lbPredefinedStyleSheets.Clear;
  with srPredefined do
  begin
    for I := 0 to StyleSheetCount - 1 do
      lbPredefinedStyleSheets.Items.AddObject(StyleSheets[I].Caption, StyleSheets[I]);
  end; 
  lbPredefinedStyleSheets.ItemIndex := 0;
end;

procedure TfrmStylesMulti.UpdateGridStyleSheets(const AStyleSheet: TcxPivotGridStyleSheet);
begin
  if GetCurrentStyleSheet = AStyleSheet then
    Exit;
  PivotGrid.Styles.StyleSheet := AStyleSheet;
  if AStyleSheet <> nil then
    pnlCurrentStyleSheet.Caption := AStyleSheet.Caption
  else
    pnlCurrentStyleSheet.Caption := '';
end;

procedure TfrmStylesMulti.SetUserDefinedStyleSheets;
begin
  with cbUserStyleSheets do
    if Items.Count > 0 then
      UpdateGridStyleSheets(TcxPivotGridStyleSheet(Items.Objects[ItemIndex]));
end;

procedure TfrmStylesMulti.SetPredefinedStyleSheets;
begin
  with lbPredefinedStyleSheets do
    if Items.Count > 0 then
      UpdateGridStyleSheets(TcxPivotGridStyleSheet(Items.Objects[ItemIndex]));
end;

procedure TfrmStylesMulti.ChangeVisibility(AType: Integer);
begin
  cbUserStyleSheets.Enabled := AType = cUserDefined;
  gbUserDefined.Enabled := AType = cUserDefined;
  btnEdit.Enabled := AType = cUserDefined;
  btnLoad.Enabled := AType = cUserDefined;
  btnSave.Enabled := AType = cUserDefined;

  lbPredefinedStyleSheets.Enabled := AType = cPredefined;
end;

function TfrmStylesMulti.GetCurrentStyleSheet: TcxPivotGridStyleSheet;
begin
  Result := TcxPivotGridStyleSheet(PivotGrid.Styles.StyleSheet);
end;

function TfrmStylesMulti.GetDefaultLookAndFeelKind: TcxLookAndFeelKind;
begin
  Result := lfUltraFlat;
end;

procedure TfrmStylesMulti.LoadUserDefinedStyleSheets(AFileName: TFileName);
begin
  UpdateGridStyleSheets(nil);
  ClearUserDefinedStyleSheets;

  LoadStyleSheetsFromIniFile(AFileName, srUserDefined, TcxPivotGridStyleSheet);

  CreateUserStyleSheetsList;
  SetUserDefinedStyleSheets;
end;

procedure TfrmStylesMulti.SaveUserDefinedStyleSheets(AFileName: TFileName);
var
  I: Integer;
  AList: TList;
begin
  AList := TList.Create;
  try
    for I := 0 to srUserDefined.StyleSheetCount - 1 do
      AList.Add(srUserDefined.StyleSheets[I]);
    SaveStyleSheetsToIniFile(AFileName, AList);
  finally
    AList.Free;
  end;
end;

procedure TfrmStylesMulti.ClearUserDefinedStyleSheets;
begin
  with srUserDefined do
  begin
    Clear;
    ClearStyleSheets;
  end;
end;

procedure TfrmStylesMulti.FormActivate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(Application.ExeName);
  SaveDialog1.InitialDir := OpenDialog.InitialDir;
  PivotGridCarName.Area := faRow;
end;


procedure TfrmStylesMulti.cbUserStyleSheetsChange(
  Sender: TObject);
begin
  SetUserDefinedStyleSheets;
end;

procedure TfrmStylesMulti.lbPredefinedStyleSheetsClick(
  Sender: TObject);
begin
  SetPredefinedStyleSheets;
end;

procedure TfrmStylesMulti.btnEditClick(
  Sender: TObject);
begin
  with cbUserStyleSheets do
    ShowcxStyleSheetEditor(TcxPivotGridStyleSheet(Items.Objects[ItemIndex]), nil);
end;

procedure TfrmStylesMulti.btnSaveClick(Sender: TObject);
begin
  with SaveDialog1 do
    if Execute then
      SaveUserDefinedStyleSheets(FileName);
end;

procedure TfrmStylesMulti.btnLoadClick(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
      LoadUserDefinedStyleSheets(FileName);
end;

procedure TfrmStylesMulti.RadioGroupClick(Sender: TObject);
begin
  case TcxRadioGroup(Sender).ItemIndex of
    cNone:
      UpdateGridStyleSheets(nil);
    cPredefined:
      SetPredefinedStyleSheets;
    cUserDefined:
      SetUserDefinedStyleSheets;
  end;
  ChangeVisibility(TcxRadioGroup(Sender).ItemIndex);
end;

procedure TfrmStylesMulti.FormCreate(Sender: TObject);
begin
  SetDefaultLookAndFeel;
  miTouchMode.Checked := cxIsTouchModeEnabled;
  CreateUserStyleSheetsList;
  CreatePredefinedStyleSheetsList;
  SetPredefinedStyleSheets;
end;

end.

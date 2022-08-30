unit StylesMultiDemoMain;

interface

uses
  Windows, Messages, Forms, SysUtils, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxEdit, DB, cxDBData, Dialogs,Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, ExtCtrls, ShellAPI, cxButtons, cxData, cxClasses, cxControls,
  ComCtrls, cxLookAndFeelPainters, cxContainer, cxRadioGroup, cxListBox,
  DemoBasicMain, cxLookAndFeels, cxMaskEdit, cxMemo, cxCurrencyEdit,
  cxCheckBox, cxDBLookupComboBox, cxInplaceContainer, cxTextEdit, cxCalc,
  cxVGrid, cxDBVGrid;

type
  TcxStyleRepositoryType = (shtNone, shtPredefined, shtUserDefined);

  TStylesMultiDemoMainForm = class(TDemoBasicMainForm)
    pnlLeft: TPanel;
    Splitter: TSplitter;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    gbUserDefined: TGroupBox;
    gbPredefined: TGroupBox;
    btnLoad: TcxButton;
    btnSave: TcxButton;
    btnEdit: TcxButton;
    pnlCurrentStyleSheet: TPanel;
    cxDBVerticalGrid: TcxDBVerticalGrid;
    cxDBVerticalGridOrderInfo: TcxCategoryRow;
    cxDBVerticalGridPurchaseDate: TcxDBEditorRow;
    cxDBVerticalGridTime: TcxDBEditorRow;
    cxDBVerticalGridPaymentType: TcxDBEditorRow;
    cxDBVerticalGridPaymentAmount: TcxDBEditorRow;
    cxDBVerticalGridQuantity: TcxDBEditorRow;
    cxDBVerticalGridCustomerInfo: TcxCategoryRow;
    cxDBVerticalGridCommonCustomerInfo: TcxCategoryRow;
    cxDBVerticalGridFirstName: TcxDBEditorRow;
    cxDBVerticalGridLastName: TcxDBEditorRow;
    cxDBVerticalGridSpouse: TcxDBEditorRow;
    cxDBVerticalGridPrefix: TcxDBEditorRow;
    cxDBVerticalGridTitle: TcxDBEditorRow;
    cxDBVerticalGridCustomerContacts: TcxCategoryRow;
    cxDBVerticalGridPhonesAndFaxes: TcxCategoryRow;
    cxDBVerticalGridFaxPhone: TcxDBEditorRow;
    cxDBVerticalGridHomePhone: TcxDBEditorRow;
    cxDBVerticalGridCategoryAddress: TcxCategoryRow;
    cxDBVerticalGridState: TcxDBEditorRow;
    cxDBVerticalGridCity: TcxDBEditorRow;
    cxDBVerticalGridAddress: TcxDBEditorRow;
    cxDBVerticalGridZipCode: TcxDBEditorRow;
    cxDBVerticalGridEmail: TcxDBEditorRow;
    cxDBVerticalGridOccupation: TcxDBEditorRow;
    cxDBVerticalGridCustomer: TcxDBEditorRow;
    cxDBVerticalGridCompany: TcxDBEditorRow;
    actEditStyleSheet: TAction;
    actLoadFromFile: TAction;
    actSaveToFile: TAction;
    vgStyleSheets: TcxVerticalGrid;
    vgStyleSheetsPredefinedStyleSheets: TcxCategoryRow;
    vgStyleSheetsUserDefinedStyleSheets: TcxCategoryRow;
    vgStyleSheetsNone: TcxCategoryRow;
    procedure FormCreate(Sender: TObject);
    procedure actSaveToFileExecute(Sender: TObject);
    procedure actLoadFromFileExecute(Sender: TObject);
    procedure actEditStyleSheetExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure OnGetDisplayText(
      Sender: TcxCustomEditorRowProperties; ARecord: Integer;
      var AText: String);
    procedure vgStyleSheetsItemChanged(Sender: TObject;
      AOldRow: TcxCustomRow; AOldCellIndex: Integer);
    procedure actEditAndSaveStyleSheetUpdate(Sender: TObject);
    procedure vgStyleSheetsStylesGetCategoryStyle(Sender: TObject;
      ARow: TcxCustomRow; var AStyle: TcxStyle);
    procedure vgStyleSheetsDrawRowHeader(Sender: TObject;
      ACanvas: TcxCanvas; APainter: TcxvgPainter;
      AHeaderViewInfo: TcxCustomRowHeaderInfo; var Done: Boolean);
  private
    function GetCurrentStyleSheet: TcxVerticalGridStyleSheet;
    procedure CreateStyleSheetsList(AStyleRepositoryType: TcxStyleRepositoryType);
    procedure UpdateGridStyleSheets(const AStyleSheet: TcxVerticalGridStyleSheet);
    procedure ClearUserDefinedStyleSheets;
    procedure LoadUserDefinedStyleSheets(AFileName: TFileName);
    procedure SaveUserDefinedStyleSheets(AFileName: TFileName);
    procedure SelectFistChild(AStyleRepositoryType: TcxStyleRepositoryType);
  end;

var
  StylesMultiDemoMainForm: TStylesMultiDemoMainForm;

implementation

uses
  StylesMultiDemoData, cxStyleSheetEditor, cxVGridStyleSheetPreview;

type
  TcxCustomRowAccess = class(TcxCustomRow);

{$R *.dfm}

procedure TStylesMultiDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  with StylesMultiDemoDataDM do
  begin
    CreateStyleSheetsList(shtUserDefined);
    CreateStyleSheetsList(shtPredefined);
  end;
  SelectFistChild(shtUserDefined);

  cxDBVerticalGrid.FullExpand;
end;

procedure TStylesMultiDemoMainForm.UpdateGridStyleSheets(const AStyleSheet: TcxVerticalGridStyleSheet);
begin
  if GetCurrentStyleSheet = AStyleSheet then Exit;
  cxDBVerticalGrid.Styles.StyleSheet := AStyleSheet;
  if AStyleSheet <> nil then
    pnlCurrentStyleSheet.Caption := AStyleSheet.Caption
  else
    pnlCurrentStyleSheet.Caption := 'None';
  cxDBVerticalGrid.Update;
end;

procedure TStylesMultiDemoMainForm.actSaveToFileExecute(Sender: TObject);
begin
  with SaveDialog do
    if Execute then
      SaveUserDefinedStyleSheets(FileName);
end;

procedure TStylesMultiDemoMainForm.actLoadFromFileExecute(Sender: TObject);
begin
  with OpenDialog do
    if Execute then
    begin
      LoadUserDefinedStyleSheets(FileName);
      SelectFistChild(shtUserDefined);
    end;
end;

procedure TStylesMultiDemoMainForm.actEditStyleSheetExecute(
  Sender: TObject);
begin
  ShowcxStyleSheetEditor(GetCurrentStyleSheet, nil);
end;

function TStylesMultiDemoMainForm.GetCurrentStyleSheet: TcxVerticalGridStyleSheet;
begin
  Result := TcxVerticalGridStyleSheet(cxDBVerticalGrid.Styles.StyleSheet);
end;

procedure TStylesMultiDemoMainForm.LoadUserDefinedStyleSheets(AFileName: TFileName);
begin
  UpdateGridStyleSheets(nil);
  ClearUserDefinedStyleSheets;

  LoadStyleSheetsFromIniFile(AFileName, StylesMultiDemoDataDM.strepUserDefined,
    TcxVerticalGridStyleSheet);

  CreateStyleSheetsList(shtUserDefined);
end;

procedure TStylesMultiDemoMainForm.SaveUserDefinedStyleSheets(AFileName: TFileName);
var
  AList: TList;
begin
  AList := TList.Create;
  try
    PopulateStyleSheetsList(AList);
    SaveStyleSheetsToIniFile(AFileName, AList);
   finally
      AList.Free;
    end;
end;

procedure TStylesMultiDemoMainForm.ClearUserDefinedStyleSheets;
begin
  with StylesMultiDemoDataDM.strepUserDefined do
  begin
    Clear;
    ClearStyleSheets;
  end;
end;

procedure TStylesMultiDemoMainForm.FormActivate(Sender: TObject);
begin
  OpenDialog.InitialDir := ExtractFileDir(Application.ExeName);
  SaveDialog.InitialDir := OpenDialog.InitialDir;
end;

procedure TStylesMultiDemoMainForm.CreateStyleSheetsList(
  AStyleRepositoryType: TcxStyleRepositoryType);
var
  I: Integer;
  AStyleRepository: TcxStyleRepository;
  Category, Row: TcxCategoryRow;
begin
  if AStyleRepositoryType = shtNone then
    Exit;
  with StylesMultiDemoDataDM do
    if AStyleRepositoryType = shtUserDefined then
    begin
      AStyleRepository := strepUserDefined;
      Category := vgStyleSheetsUserDefinedStyleSheets;
    end
    else
    begin
      AStyleRepository := strepPredefined;
      Category := vgStyleSheetsPredefinedStyleSheets;
    end;

  with AStyleRepository do
  begin
    TcxCustomRowAccess(Category).RemoveAll;
    for I := 0 to StyleSheetCount - 1 do
    begin
      Row := TcxCategoryRow(vgStyleSheets.AddChild(Category, TcxCategoryRow));
      Row.Properties.Caption := StyleSheets[I].Caption;
      Row.Options.TabStop := False;
      Row.Styles.Header := StylesMultiDemoDataDM.cxVerticalGridStyleSheetDevExpress.Styles.Content;
      Row.Tag := Integer(StyleSheets[I]);
    end;
  end;
end;
procedure TStylesMultiDemoMainForm.SelectFistChild(
  AStyleRepositoryType: TcxStyleRepositoryType);
begin
  case AStyleRepositoryType of
    shtNone: vgStyleSheets.FocusedRow := vgStyleSheetsNone;
    shtPredefined: vgStyleSheets.FocusedRow := vgStyleSheetsPredefinedStyleSheets.Rows[0];
    shtUserDefined: vgStyleSheets.FocusedRow := vgStyleSheetsUserDefinedStyleSheets.Rows[0];
   end;
end;

procedure TStylesMultiDemoMainForm.OnGetDisplayText(
  Sender: TcxCustomEditorRowProperties; ARecord: Integer;
  var AText: String);
begin
  AText := Sender.Caption;
end;

procedure TStylesMultiDemoMainForm.vgStyleSheetsItemChanged(
  Sender: TObject; AOldRow: TcxCustomRow; AOldCellIndex: Integer);
begin
  if (vgStyleSheets.FocusedRow <> nil) and (vgStyleSheets.FocusedRow.Tag <> -1) then
  begin
    UpdateGridStyleSheets(TcxVerticalGridStyleSheet(vgStyleSheets.FocusedRow.Tag));
    vgStyleSheets.Update;
  end;
end;

procedure TStylesMultiDemoMainForm.actEditAndSaveStyleSheetUpdate(
  Sender: TObject);
begin
  TCustomAction(Sender).Enabled := (vgStyleSheets.FocusedRow.Parent <> nil) and
    (vgStyleSheets.FocusedRow.Parent = vgStyleSheetsUserDefinedStyleSheets);
end;

procedure TStylesMultiDemoMainForm.vgStyleSheetsStylesGetCategoryStyle(
  Sender: TObject; ARow: TcxCustomRow; var AStyle: TcxStyle);
begin
   if (vgStyleSheets.FocusedRow = ARow) then
    AStyle := StylesMultiDemoDataDM.cxVerticalGridStyleSheetDevExpress.Styles.Selection;
end;

procedure TStylesMultiDemoMainForm.vgStyleSheetsDrawRowHeader(
  Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter;
  AHeaderViewInfo: TcxCustomRowHeaderInfo; var Done: Boolean);
begin
  AHeaderViewInfo.FocusRect := Rect(0, 0, 0, 0);
end;

end.

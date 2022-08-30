unit StylesSimpleDemoStylesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxTextEdit, cxInplaceContainer, cxControls,
  cxButtonEdit, cxClasses, StdCtrls, cxMRUEdit, cxMaskEdit,
  cxDBLookupComboBox, cxCurrencyEdit, cxMemo, cxCheckBox,
  cxLookAndFeelPainters, cxButtons, cxGraphics, cxCustomData, cxVGrid,
  cxEdit, cxEditRepositoryItems, ActnList;

type
  TStyles = (sBackground, sCategory, sContent, sHeader, sInactive, sIncSearch, sSelection);

  TStylesSimpleDemoStylesDialogForm = class(TForm)
    btnRestore: TcxButton;
    cxVerticalGrid: TcxVerticalGrid;
    cxEditRepository: TcxEditRepository;
    cxEditRepositoryMRUItem: TcxEditRepositoryMRUItem;
    ActionList1: TActionList;
    cxVerticalGridBackground: TcxEditorRow;
    cxVerticalGridCategory: TcxEditorRow;
    cxVerticalGridContent: TcxEditorRow;
    cxVerticalGridHeader: TcxEditorRow;
    cxVerticalGridInactive: TcxEditorRow;
    cxVerticalGridIncSearch: TcxEditorRow;
    cxVerticalGridSelection: TcxEditorRow;
    cxVerticalGridCaption: TcxMultiEditorRow;
    cxStyleRepository1: TcxStyleRepository;
    cxVerticalGridStyleSheetDevExpress: TcxVerticalGridStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    procedure FormCreate(Sender: TObject);
    procedure OnButtonClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure OnEditValueChanged(Sender: TObject);
    procedure cxVerticalGridStylesGetContentStyle(Sender: TObject;
      AEditProp: TcxCustomEditorRowProperties; AFocused: Boolean;
      ARecordIndex: Integer; var AStyle: TcxStyle);
    procedure cxVerticalGridStylesGetHeaderStyle(Sender: TObject;
      ARow: TcxCustomRow; var AStyle: TcxStyle);
  private
    FRestoreDefaults: TNotifyEvent;
    function GetSelectedStyle: TcxStyle;
    procedure RefreshBinding;
    function GetCurrentStyle(AStyleID: TStyles): TcxStyle;
    procedure SetCurrentStyle(const AStyle: TcxStyle; AStyleID: TStyles);
  public
    property RestoreDefaults: TNotifyEvent read FRestoreDefaults write FRestoreDefaults;
  end;

var
  StylesSimpleDemoStylesDialogForm: TStylesSimpleDemoStylesDialogForm;

implementation

uses StylesSimpleDemoData, StylesSimpleDemoEdit, StylesSimpleDemoMain;

{$R *.dfm}

function ChangeStyleBinding(ACallback: TNotifyEvent): Boolean;
begin
  with TStylesSimpleDemoStylesDialogForm.Create(Application) do
  try
    RestoreDefaults := ACallback;
    Result := ShowModal = mrOK;
  finally
    Free;
  end;
end;

procedure TStylesSimpleDemoStylesDialogForm.FormCreate(Sender: TObject);
var
  i: integer;
  AStyle: TcxStyle;
begin
  OnClose := StylesSimpleDemoMainForm.StylesFormClosed;
  with cxEditRepositoryMRUItem.Properties.LookupItems, StylesSimpleDemoDataDM do
  begin
    Clear;
    for i := 0 to StyleRepository.Count - 1  do
    begin
       AStyle := TcxStyle(StyleRepository[i]);
       AddObject(AStyle.Name, AStyle);
    end;
  end;
  RefreshBinding;
end;

procedure TStylesSimpleDemoStylesDialogForm.OnButtonClick(
  Sender: TObject);
begin
  ChangeStyle(GetSelectedStyle);
end;

procedure TStylesSimpleDemoStylesDialogForm.btnRestoreClick(
  Sender: TObject);
begin
  if Assigned(FRestoreDefaults) then FRestoreDefaults(Sender);
  RefreshBinding;
end;

procedure TStylesSimpleDemoStylesDialogForm.RefreshBinding;
var
  I: Integer;
  AStyle: TcxStyle;
begin
  for i:=1 to cxVerticalGrid.Rows.Count - 1 do
  begin
    AStyle := GetCurrentStyle(TStyles(i-1));
    if AStyle <> nil then
      TcxEditorRow(cxVerticalGrid.Rows[i]).Properties.Value :=
        AStyle.Name;
  end;
end;

function TStylesSimpleDemoStylesDialogForm.GetSelectedStyle: TcxStyle;
var
  ItemIndex: Integer;
begin
  ItemIndex := cxEditRepositoryMRUItem.Properties.LookupItems.
    IndexOf(TcxEditorRow(cxVerticalGrid.FocusedRow).Properties.Value);
  Result := TcxStyle(cxEditRepositoryMRUItem.Properties.LookupItems.Objects[ItemIndex]);
end;

function TStylesSimpleDemoStylesDialogForm.GetCurrentStyle(AStyleID: TStyles): TcxStyle;
begin
  Result := nil;
  with StylesSimpleDemoMainForm do
  case AStyleID of
    sBackground:
      Result := cxDBVerticalGrid.Styles.Background;
    sCategory:
      Result := cxDBVerticalGrid.Styles.Category;
    sHeader:
      Result := cxDBVerticalGrid.Styles.Header;
    sContent:
      Result := cxDBVerticalGrid.Styles.Content;
    sInactive:
      Result := cxDBVerticalGrid.Styles.Inactive;
    sIncSearch:
      Result := cxDBVerticalGrid.Styles.IncSearch;
    sSelection:
       Result := cxDBVerticalGrid.Styles.Selection;
  end;
end;

procedure TStylesSimpleDemoStylesDialogForm.SetCurrentStyle(
  const AStyle: TcxStyle; AStyleID: TStyles);
begin
  with StylesSimpleDemoMainForm do
    case AStyleID of
      sBackground:
        cxDBVerticalGrid.Styles.Background := AStyle;
      sCategory:
        cxDBVerticalGrid.Styles.Category := AStyle;
      sHeader:
        cxDBVerticalGrid.Styles.Header := AStyle;
      sContent:
        cxDBVerticalGrid.Styles.Content := AStyle;
      sInactive:
        cxDBVerticalGrid.Styles.Inactive := AStyle;
      sIncSearch:
        cxDBVerticalGrid.Styles.IncSearch := AStyle;
      sSelection:
        cxDBVerticalGrid.Styles.Selection := AStyle;
    end;
end;

procedure TStylesSimpleDemoStylesDialogForm.OnEditValueChanged(
  Sender: TObject);
var
  ItemIndex: Integer;
  AStyle: TcxStyle;
begin
  ItemIndex := cxEditRepositoryMRUItem.Properties.LookupItems.IndexOf(TcxCustomMRUEdit(Sender).EditValue);
  AStyle := TcxStyle(cxEditRepositoryMRUItem.Properties.LookupItems.Objects[ItemIndex]);
  SetCurrentStyle(AStyle, TStyles(cxVerticalGrid.FocusedRow.VisibleIndex - 1));
end;

procedure TStylesSimpleDemoStylesDialogForm.cxVerticalGridStylesGetContentStyle(
  Sender: TObject; AEditProp: TcxCustomEditorRowProperties;
  AFocused: Boolean; ARecordIndex: Integer; var AStyle: TcxStyle);
begin
  if TcxEditorRow(AEditProp.Row).VisibleIndex = 0 then
    AStyle := cxVerticalGridStyleSheetDevExpress.Styles.Category
  else
    AStyle := cxVerticalGridStyleSheetDevExpress.Styles.Content;
end;

procedure TStylesSimpleDemoStylesDialogForm.cxVerticalGridStylesGetHeaderStyle(
  Sender: TObject; ARow: TcxCustomRow; var AStyle: TcxStyle);
begin
  if ARow.VisibleIndex = 0 then
    AStyle := cxVerticalGridStyleSheetDevExpress.Styles.Category
  else
    AStyle := cxVerticalGridStyleSheetDevExpress.Styles.Header;
end;

end.

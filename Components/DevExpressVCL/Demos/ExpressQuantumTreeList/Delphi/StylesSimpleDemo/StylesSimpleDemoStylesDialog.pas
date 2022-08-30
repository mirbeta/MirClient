unit StylesSimpleDemoStylesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxTL, cxTextEdit, cxInplaceContainer, cxControls,
  cxButtonEdit, cxClasses, StdCtrls, cxMRUEdit, cxMaskEdit,
  cxDBLookupComboBox, cxCurrencyEdit, cxMemo, cxCheckBox,
  cxLookAndFeelPainters, cxButtons, cxGraphics, cxCustomData, Menus;

type
  TStyles = (sBackground, sBandbackground, sBandContent, sBandHeader, sColumnFooter,
  sColumnHeader, sContent, sContentEven,  sContentOdd, sFooter, sInactive, sIncSearch,
  sIndicator, sPreview, sSelection);

  TStylesSimpleDemoStylesDialogForm = class(TForm)
    cxTreeList: TcxTreeList;
    tlcStyle: TcxTreeListColumn;
    tlcStyleNames: TcxTreeListColumn;
    lscrip: TLabel;
    cxStyleRepository1: TcxStyleRepository;
    TreeListStyleSheetDevExpress: TcxTreeListStyleSheet;
    cxStyle1: TcxStyle;
    cxStyle2: TcxStyle;
    cxStyle3: TcxStyle;
    cxStyle4: TcxStyle;
    cxStyle5: TcxStyle;
    cxStyle6: TcxStyle;
    cxStyle7: TcxStyle;
    cxStyle8: TcxStyle;
    cxStyle9: TcxStyle;
    cxStyle10: TcxStyle;
    cxStyle11: TcxStyle;
    cxStyle12: TcxStyle;
    cxStyle13: TcxStyle;
    btnRestore: TcxButton;
    procedure FormCreate(Sender: TObject);
    procedure tlcStyleNamesPropertiesButtonClick(Sender: TObject);
    procedure btnRestoreClick(Sender: TObject);
    procedure tlcStyleNamesPropertiesEditValueChanged(Sender: TObject);
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
  with TcxMRUEditProperties(tlcStyleNames.Properties).LookupItems, StylesSimpleDemoDataDM do
  begin
    Clear;
    for i := 0 to StyleRepository.Count -1  do
    begin
       AStyle := TcxStyle(StyleRepository[i]);
       AddObject(AStyle.Name, AStyle);
    end;
  end;
{ remove/add the closing brace on this line to disable/enable the following code}

  RefreshBinding;

//}
end;

procedure TStylesSimpleDemoStylesDialogForm.tlcStyleNamesPropertiesButtonClick(
  Sender: TObject);
var
  AStyle: TcxStyle;
begin
  AStyle := GetSelectedStyle;
  if AStyle <> nil then
    ChangeStyle(AStyle);
end;

procedure TStylesSimpleDemoStylesDialogForm.btnRestoreClick(
  Sender: TObject);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  if Assigned(FRestoreDefaults) then FRestoreDefaults(Sender);
  RefreshBinding;

//}
end;

procedure TStylesSimpleDemoStylesDialogForm.RefreshBinding;
var
  I: Integer;
  AStyle: TcxStyle;
begin
  for i:=0 to cxTreeList.Root.Count - 1 do
  begin
    AStyle := GetCurrentStyle(TStyles(i));
    if AStyle <> nil then
      cxTreeList.Root.Items[i].Values[1] := AStyle.Name
    else
      cxTreeList.Root.Items[i].Values[1] := '';
  end;
end;

function TStylesSimpleDemoStylesDialogForm.GetSelectedStyle: TcxStyle;
var
  ItemIndex: Integer;
begin
  Result := nil;
  ItemIndex := TcxMRUEditProperties(tlcStyleNames.Properties).LookupItems.
    IndexOf(tlcStyleNames.Values[cxTreeList.FocusedNode]);
  if ItemIndex <> -1 then
    Result := TcxStyle(TcxMRUEditProperties(tlcStyleNames.Properties).LookupItems.Objects[ItemIndex]);
end;

function TStylesSimpleDemoStylesDialogForm.GetCurrentStyle(AStyleID: TStyles): TcxStyle;
begin
  Result := nil;
  with StylesSimpleDemoMainForm do
  case AStyleID of
    sBackground:
      Result := cxDBTreeList.Styles.Background;
    sBandbackground:
      Result := cxDBTreeList.Styles.BandBackground;
    sBandContent:
      Result := cxDBTreeList.Styles.BandContent;
    sBandHeader:
      Result := cxDBTreeList.Styles.BandHeader;
    sColumnFooter:
      Result := cxDBTreeList.Styles.ColumnFooter;
    sColumnHeader:
      Result := cxDBTreeList.Styles.ColumnHeader;
    sContent:
      Result := cxDBTreeList.Styles.Content;
    sContentEven:
      Result := cxDBTreeList.Styles.ContentEven;
    sContentOdd:
      Result := cxDBTreeList.Styles.ContentOdd;
    sFooter:
      Result := cxDBTreeList.Styles.Footer;
    sInactive:
      Result := cxDBTreeList.Styles.Indicator;
    sIncSearch:
      Result := cxDBTreeList.Styles.IncSearch;
    sIndicator:
      Result := cxDBTreeList.Styles.Indicator;
    sPreview:
      Result := cxDBTreeList.Styles.Preview;
    sSelection:
       Result := cxDBTreeList.Styles.Selection;
  end;
end;

procedure TStylesSimpleDemoStylesDialogForm.SetCurrentStyle(
  const AStyle: TcxStyle; AStyleID: TStyles);
begin
  with StylesSimpleDemoMainForm do
    case AStyleID of
      sBackground:
        cxDBTreeList.Styles.Background := AStyle;
      sBandbackground:
        cxDBTreeList.Styles.BandBackground := AStyle;
      sBandContent:
        cxDBTreeList.Styles.BandContent := AStyle;
      sBandHeader:
        cxDBTreeList.Styles.BandHeader := AStyle;
      sColumnFooter:
        cxDBTreeList.Styles.ColumnFooter := AStyle;
      sColumnHeader:
        cxDBTreeList.Styles.ColumnHeader := AStyle;
      sContent:
        cxDBTreeList.Styles.Content := AStyle;
      sContentEven:
        cxDBTreeList.Styles.ContentEven := AStyle;
      sContentOdd:
        cxDBTreeList.Styles.ContentOdd := AStyle;
      sFooter:
        cxDBTreeList.Styles.Footer := AStyle;
      sInactive:
        cxDBTreeList.Styles.Inactive := AStyle;
      sIncSearch:
        cxDBTreeList.Styles.IncSearch := AStyle;
      sIndicator:
        cxDBTreeList.Styles.Indicator := AStyle;
      sPreview:
        cxDBTreeList.Styles.Preview := AStyle;
      sSelection:
        cxDBTreeList.Styles.Selection := AStyle;
    end;
end;

procedure TStylesSimpleDemoStylesDialogForm.tlcStyleNamesPropertiesEditValueChanged(
  Sender: TObject);
var
  ItemIndex: Integer;
  AStyle: TcxStyle;
begin
  ItemIndex := TcxMRUEditProperties(tlcStyleNames.Properties).
    LookupItems.IndexOf(TcxCustomMRUEdit(Sender).EditValue);
  AStyle := TcxStyle(TcxMRUEditProperties(tlcStyleNames.Properties).LookupItems.Objects[ItemIndex]);
  SetCurrentStyle(AStyle, TStyles(cxTreeList.FocusedNode.Index));
end;

end.

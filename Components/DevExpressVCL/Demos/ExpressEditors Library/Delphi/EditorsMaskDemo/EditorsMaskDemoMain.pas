unit EditorsMaskDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, cxControls,
  cxMaskEdit, StdCtrls, ExtCtrls, cxContainer, cxEdit, cxTextEdit, cxDropDownEdit,
  cxDBEdit, cxStyles, Menus, ActnList, ImgList, ComCtrls, cxLookAndFeels,
  cxButtonEdit, DBCtrls, EditorsMaskDemoData, cxCustomData, cxGraphics, cxFilter,
  cxData, DB, cxDBData, cxClasses, cxEditMaskEditor, cxNavigator, cxDBNavigator,
  cxDataStorage, BaseForm, cxLookAndFeelPainters, Grids, DBGrids, DemoUtils;

type
  TEditorsMaskDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    Panel1: TPanel;
    Panel3: TPanel;
    miShowMaskButtons: TMenuItem;
    miDefaultMaskSettings: TMenuItem;
    Panel2: TPanel;
    lbFirstName: TLabel;
    lbMiddleName: TLabel;
    lbLastName: TLabel;
    lbCountry: TLabel;
    lbPostalCode: TLabel;
    lbCity: TLabel;
    lbAddress: TLabel;
    lbPhone: TLabel;
    lbFax: TLabel;
    lbEmail: TLabel;
    lbHomePage: TLabel;
    lbInfoPhone: TLabel;
    lbInfoFax: TLabel;
    lbInfoHomePage: TLabel;
    lbInfoEmail: TLabel;
    lbInfoPostalCode: TLabel;
    lbInfoFirstName: TLabel;
    lbInfoMiddleName: TLabel;
    lbInfoLastName: TLabel;
    lbInfoCountry: TLabel;
    lbInfoCity: TLabel;
    lbInfoAddress: TLabel;
    Panel5: TPanel;
    edtFirstName: TcxDBTextEdit;
    edtMiddleName: TcxDBTextEdit;
    edtLastName: TcxDBTextEdit;
    edtCountry: TcxDBTextEdit;
    edtCity: TcxDBTextEdit;
    edtAddress: TcxDBTextEdit;
    DBNavigator1: TcxDBNavigator;
    edtPostalCode: TcxDBButtonEdit;
    edtPhone: TcxDBButtonEdit;
    edtFax: TcxDBButtonEdit;
    edtHomePage: TcxDBButtonEdit;
    edtEmail: TcxDBButtonEdit;
    DBGrid1: TDBGrid;
    procedure cxDBButtonEdit1PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxDBButtonEdit2PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxDBButtonEdit3PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure cxDBButtonEdit4PropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure edtPostalCodePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure miDefaultMaskSettingsClick(Sender: TObject);
    procedure miShowMaskButtonsClick(Sender: TObject);
  private
    procedure ChangeLabel(ALabel: TLabel; AProperties: TcxCustomMaskEditProperties);
    procedure CreateGrid;
    function GetMaskKindLabel(AMaskKind: TcxEditMaskKind): string;
    procedure ShowEditMaskDialog(AProperties: TcxCustomEditProperties);
  public
    procedure AfterConstruction; override;
  end;

var
  EditorsMaskDemoMainForm: TEditorsMaskDemoMainForm;

implementation

uses
{$IFDEF EXPRESSGRID}
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGridLevel,
  cxGridCustomView, cxGrid, cxGridCardView, cxGridDBCardView,
{$ENDIF}
  AboutDemoForm;

{$R *.dfm}

const
  cxDefaultPostalCodeMaskKind: TcxEditMaskKind = emkRegExprEx;
  cxDefaultPhoneMaskKind: TcxEditMaskKind = emkStandard;
  cxDefaultFaxMaskKind: TcxEditMaskKind = emkRegExprEx;
  cxDefaultHomePageMaskKind: TcxEditMaskKind = emkRegExprEx;
  cxDefaultEmailMaskKind: TcxEditMaskKind = emkRegExpr;
  cxDefaultPostalCodeEditMask: string = '\d\d\d\d\d? | \w\w\w'' ''\w\w\w';
  cxDefaultPhoneEditMask: string = '!\(999\) 000-0000;1;_';
  cxDefaultFaxEditMask: string = '(\(\d\d\d\)'' '')?\d\d\d-\d\d\d\d';
  cxDefaultHomePageEditMask: string = 'http\:\/\/(\w+(\.\w+)*@)?\w+\.\w+(\.\w+)*(/(\w+(/\w+)*/?)?)?';
  cxDefaultEmailEditMask: string = '\w+@\w+\.\w+(\.\w+)*';

type
  TcxCustomMaskEditPropertiesAccess = class(TcxCustomMaskEditProperties);

procedure TEditorsMaskDemoMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  CreateGrid;
end;

procedure TEditorsMaskDemoMainForm.ChangeLabel(ALabel: TLabel; AProperties: TcxCustomMaskEditProperties);
var
  AText: string;
begin
  if TcxCustomMaskEditPropertiesAccess(AProperties).EmptyMask(AProperties.EditMask) then
    AText := 'No mask'
  else
    AText := GetMaskKindLabel(AProperties.MaskKind);
  ALabel.Caption := AText;
end;

procedure TEditorsMaskDemoMainForm.CreateGrid;
{$IFDEF EXPRESSGRID}

  procedure InitializeGridViewOptions(AView: TcxGridDBTableView);
  begin
    AView.OptionsData.Deleting := False;
    AView.OptionsData.Editing := False;
    AView.OptionsData.Inserting := False;
    AView.OptionsSelection.InvertSelect := False;
    AView.OptionsView.ColumnAutoWidth := True;
    AView.OptionsView.GroupByBox := False;
    AView.Styles.Background := EditorsMaskDemoMainDM.stBlueSky;
    AView.Styles.Content := EditorsMaskDemoMainDM.stBlueLight;
    AView.Styles.Inactive := EditorsMaskDemoMainDM.stBlueSky;
    AView.Styles.Selection := EditorsMaskDemoMainDM.stBlueBright;
    AView.Styles.Header := EditorsMaskDemoMainDM.stBlueDark;
  end;

var
  AGrid: TcxGrid;
  AGridDBColumn: TcxGridDBColumn;
  AGridDBTableView: TcxGridDBTableView;
  I: Integer;
{$ENDIF}
begin
{$IFDEF EXPRESSGRID}
  AGrid := TcxGrid.Create(Self);
  AGrid.Parent := DBGrid1.Parent;
  AGrid.Align := DBGrid1.Align;
  AGridDBTableView := AGrid.CreateView(TcxGridDBTableView) as TcxGridDBTableView;
  AGrid.Levels.Add.GridView := AGridDBTableView;
  AGridDBTableView.DataController.DataSource := DBGrid1.DataSource;
  for I := 0 to DBGrid1.Columns.Count - 1 do
  begin
    AGridDBColumn := AGridDBTableView.CreateColumn as TcxGridDBColumn;
    AGridDBColumn.DataBinding.FieldName := DBGrid1.Columns[I].FieldName;
  end;
  InitializeGridViewOptions(AGridDBTableView);
  DBGrid1.Free;
{$ENDIF}
end;

function TEditorsMaskDemoMainForm.GetMaskKindLabel(AMaskKind: TcxEditMaskKind): string;
begin
  case AMaskKind of
    emkStandard:
      Result := 'Delphi Standard Mask';
    emkRegExpr:
      Result := 'Regular Expression';
    emkRegExprEx:
      Result := 'Regular Expression with Auto Complete Function';
    else
      Result := 'Unknown';
  end;
end;

procedure TEditorsMaskDemoMainForm.ShowEditMaskDialog(AProperties: TcxCustomEditProperties);
var
  ADialog: TcxEditMaskEditorDlg;
begin
  if AProperties.InheritsFrom(TcxCustomMaskEditProperties) then
  begin
    ADialog := TcxEditMaskEditorDlg.Create(nil);
    try
      ADialog.MaskEditProperties := AProperties as TcxCustomMaskEditProperties;
      ADialog.ShowModal;
    finally
      ADialog.Free;
    end;
  end;
end;

procedure TEditorsMaskDemoMainForm.edtPostalCodePropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  ShowEditMaskDialog((Sender as TcxDBButtonEdit).Properties);
  ChangeLabel(lbInfoPostalCode, (Sender as TcxDBButtonEdit).Properties as TcxCustomMaskEditProperties);
end;

procedure TEditorsMaskDemoMainForm.cxDBButtonEdit1PropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  ShowEditMaskDialog((Sender as TcxDBButtonEdit).Properties);
  ChangeLabel(lbInfoPhone, (Sender as TcxDBButtonEdit).Properties as TcxCustomMaskEditProperties);
end;

procedure TEditorsMaskDemoMainForm.cxDBButtonEdit2PropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  ShowEditMaskDialog((Sender as TcxDBButtonEdit).Properties);
  ChangeLabel(lbInfoFax, (Sender as TcxDBButtonEdit).Properties as TcxCustomMaskEditProperties);
end;

procedure TEditorsMaskDemoMainForm.cxDBButtonEdit3PropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  ShowEditMaskDialog((Sender as TcxDBButtonEdit).Properties);
  ChangeLabel(lbInfoHomePage, (Sender as TcxDBButtonEdit).Properties as TcxCustomMaskEditProperties);
end;

procedure TEditorsMaskDemoMainForm.cxDBButtonEdit4PropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
begin
  ShowEditMaskDialog((Sender as TcxDBButtonEdit).Properties);
  ChangeLabel(lbInfoEmail, (Sender as TcxDBButtonEdit).Properties as TcxCustomMaskEditProperties);
end;

procedure TEditorsMaskDemoMainForm.miShowMaskButtonsClick(Sender: TObject);
var
  AButtonVisible: Boolean;
begin
  AButtonVisible := dxDemoIsMenuChecked(Sender);
  edtPostalCode.Properties.Buttons.Items[0].Visible := AButtonVisible;
  edtPhone.Properties.Buttons.Items[0].Visible := AButtonVisible;
  edtFax.Properties.Buttons.Items[0].Visible := AButtonVisible;
  edtHomePage.Properties.Buttons.Items[0].Visible := AButtonVisible;
  edtEmail.Properties.Buttons.Items[0].Visible := AButtonVisible;
end;

procedure TEditorsMaskDemoMainForm.miDefaultMaskSettingsClick(Sender: TObject);
begin
  edtPostalCode.Properties.MaskKind := cxDefaultPostalCodeMaskKind;
  edtPostalCode.Properties.EditMask := cxDefaultPostalCodeEditMask;
  ChangeLabel(lbInfoPostalCode, edtPostalCode.Properties);
  edtPhone.Properties.MaskKind := cxDefaultPhoneMaskKind;
  edtPhone.Properties.EditMask := cxDefaultPhoneEditMask;
  ChangeLabel(lbInfoPhone, edtPhone.Properties);
  edtFax.Properties.MaskKind := cxDefaultFaxMaskKind;
  edtFax.Properties.EditMask := cxDefaultFaxEditMask;
  ChangeLabel(lbInfoFax, edtFax.Properties);
  edtHomePage.Properties.MaskKind := cxDefaultHomePageMaskKind;
  edtHomePage.Properties.EditMask := cxDefaultHomePageEditMask;
  ChangeLabel(lbInfoHomePage, edtHomePage.Properties);
  edtEmail.Properties.MaskKind := cxDefaultEmailMaskKind;
  edtEmail.Properties.EditMask := cxDefaultEmailEditMask;
  ChangeLabel(lbInfoEmail, edtEmail.Properties);
end;

end.


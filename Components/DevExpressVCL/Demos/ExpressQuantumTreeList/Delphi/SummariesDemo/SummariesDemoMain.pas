unit SummariesDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookAndFeels, ActnList, ImgList, Menus, ComCtrls,
  StdCtrls, DemoBasicMain, cxContainer, cxEdit, cxTextEdit, cxStyles, cxTL,
  cxInplaceContainer, cxTLData, cxDBTL, cxMaskEdit, cxCalendar,
  cxCurrencyEdit, cxDBLookupComboBox, cxDBEditRepository,
  cxEditRepositoryItems, cxImage, cxMemo,
  cxLookAndFeelPainters, cxButtons, cxDropDownEdit, cxTimeEdit,
  cxImageComboBox, cxCalc, cxSpinEdit, cxCustomData, Grids, DBGrids,
  cxGraphics, cxCheckBox, ExtCtrls, cxLabel;

type
  TSummariesDemoMainForm = class(TDemoBasicMainForm)
    tlDepartments: TcxDBTreeList;
    clName: TcxDBTreeListColumn;
    clBudget: TcxDBTreeListColumn;
    clPhone: TcxDBTreeListColumn;
    clFax: TcxDBTreeListColumn;
    clEmail: TcxDBTreeListColumn;
    clVacancy: TcxDBTreeListColumn;
    lblSummary: TLabel;
    ilUser: TcxImageList;
    procedure tlDepartmentsAfterSummary(Sender: TObject);
    procedure tlDepartmentsSummary(ASender: TcxCustomTreeList;
      const Arguments: TcxTreeListSummaryEventArguments;
      var OutArguments: TcxTreeListSummaryEventOutArguments);
    procedure clNameTcxTreeListColumnSummaryGroupFooterSummaryItems0GetText(
      Sender: TcxTreeListSummaryItem; const AValue: Variant;
      var AText: String);
    procedure tlDepartmentsPopupMenusFooterMenuClick(
      Sender: TcxCustomTreeList; AItem: TObject; var AHandled: Boolean);
    procedure tlDepartmentsPopupMenusFooterMenuPopup(
      Sender: TcxCustomTreeList; AContextMenu: TcxTreeListPopupMenu;
      var AHandled: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    FAlternateCounting: Boolean;
    FCheckBudget: Boolean;
    FCheckVacancies: Boolean;
  end;

var
  SummariesDemoMainForm: TSummariesDemoMainForm;

implementation

uses
  SummariesDemoData, DB, cxDBData;

{$R *.dfm}

procedure TSummariesDemoMainForm.tlDepartmentsAfterSummary(Sender: TObject);
var
  ASummaryItem: TcxTreeListSummaryItem;
  ANode: TcxTreeListNode;
  ASummaryText, AAboutChildDepartmentInclusionText: string;
begin
  ANode := tlDepartments.FindNodeByText('Top Management', clName);
  if ANode = nil then Exit;
  ASummaryItem := clName.Summary.GroupFooterSummaryItems.GetItemByKind(skCount);
  if ASummaryItem = nil then Exit;
  ASummaryText := ANode.FooterSummaryTexts[ASummaryItem.AbsoluteIndex];
  if ASummaryItem.AllNodes then
    AAboutChildDepartmentInclusionText := 'included'
  else
    AAboutChildDepartmentInclusionText := 'excluded';

  lblSummary.Caption := Format(
    'The ''Top Management'' department has %s departments (child departments are %s), matching the specified criteria',
    [ASummaryText, AAboutChildDepartmentInclusionText]);
end;

procedure TSummariesDemoMainForm.tlDepartmentsSummary(
  ASender: TcxCustomTreeList;
  const Arguments: TcxTreeListSummaryEventArguments;
  var OutArguments: TcxTreeListSummaryEventOutArguments);
begin
// If a department doesn't match the specified criteria, exclude its
// values from use in automatic summary calculations
  OutArguments.Done := FCheckBudget and (Arguments.Node.Values[clBudget.ItemIndex] <= 100000) or
    FCheckVacancies and not Arguments.Node.Values[clVacancy.ItemIndex];
end;

procedure TSummariesDemoMainForm.clNameTcxTreeListColumnSummaryGroupFooterSummaryItems0GetText(
  Sender: TcxTreeListSummaryItem; const AValue: Variant;
  var AText: String);
begin
  if FAlternateCounting then
    if (AValue < 40) then
      AText := 'Less than 40'
    else
      AText := 'Equal to or Greater than 40';
end;

procedure TSummariesDemoMainForm.tlDepartmentsPopupMenusFooterMenuClick(
  Sender: TcxCustomTreeList; AItem: TObject; var AHandled: Boolean);
begin
  case TComponent(AItem).Tag of
    tlcmUser: FCheckBudget := not FCheckBudget;
    tlcmUser + 1: FCheckVacancies := not FCheckVacancies;
    tlcmUser + 2: FAlternateCounting := not FAlternateCounting;
  end;
  tlDepartments.Summary.Recalculate;
end;

procedure TSummariesDemoMainForm.tlDepartmentsPopupMenusFooterMenuPopup(
  Sender: TcxCustomTreeList; AContextMenu: TcxTreeListPopupMenu;
  var AHandled: Boolean);
begin
  AContextMenu.CreateMenuItem(AContextMenu.Root, 'Budget exceeds 100000', tlcmUser, True,
    tlmitChecked, FCheckBudget, -1, True);
  AContextMenu.CreateMenuItem(AContextMenu.Root, 'Department has vacancies', tlcmUser + 1, True,
    tlmitChecked, FCheckVacancies, 0);
  AContextMenu.CreateMenuItem(AContextMenu.Root, 'Alternate department tally method', tlcmUser + 2, True,
    tlmitChecked, FAlternateCounting);
end;

procedure TSummariesDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  tlDepartments.PopupMenus.FooterMenu.UserImages := ilUser;
  tlDepartments.PopupMenus.GroupFooterMenu.UserImages := ilUser;
end;

end.

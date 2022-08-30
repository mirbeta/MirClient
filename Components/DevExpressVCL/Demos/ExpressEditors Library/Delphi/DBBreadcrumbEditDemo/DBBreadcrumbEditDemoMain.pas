unit DBBreadcrumbEditDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Forms, ComCtrls, ShlObj, cxShellCommon, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, Menus, cxClasses, cxContainer,
  cxEdit, ImgList, Controls, Classes, ActnList, dxBreadcrumbEdit, dxShellBreadcrumbEdit,
  Buttons, cxShellTreeView, cxSplitter, cxShellControls, cxShellListView,
  StdCtrls, ExtCtrls, dxGDIPlusClasses, ShellAPI, BaseForm, cxTreeView,
  cxGroupBox, cxLabel, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxButtons,
  cxCheckBox, dxDBBreadcrumbEdit, DB, dxmdaset, cxDBEdit, 
  cxHyperLinkEdit, cxNavigator, cxDBNavigator, cxCurrencyEdit;

type
  TdxDBBreadcrumbEditDemoForm = class(TfmBaseForm)
    gbMain: TcxGroupBox;
    bvSpacerLeft: TBevel;
    bvSpacerBottom: TBevel;
    bvSpacerTop: TBevel;
    bvSpacerRight: TBevel;
    mdDepartments: TdxMemData;
    dsDepartments: TDataSource;
    beNavigation: TdxDBBreadcrumbEdit;
    lbPhone: TcxLabel;
    meFax: TcxDBMaskEdit;
    lbFax: TcxLabel;
    mePhone: TcxDBMaskEdit;
    gbDepartment: TcxGroupBox;
    bvSpacer: TBevel;
    mdDepartmentsID: TAutoIncField;
    mdDepartmentsPARENTID: TIntegerField;
    mdDepartmentsNAME: TStringField;
    mdDepartmentsBUDGET: TFloatField;
    mdDepartmentsPHONE: TStringField;
    mdDepartmentsFAX: TStringField;
    mdDepartmentsEMAIL: TStringField;
    mdDepartmentsVACANCY: TBooleanField;
    mdDepartmentsMANAGERID: TIntegerField;
    cbVacancy: TcxDBCheckBox;
    lbEmail: TcxLabel;
    hleEmail: TcxDBHyperLinkEdit;
    lbBudget: TcxLabel;
    ceBudget: TcxDBCurrencyEdit;
    cxDBNavigator1: TcxDBNavigator;
    procedure beNavigationPathSelected(Sender: TObject);
  end;

var
  dxDBBreadcrumbEditDemoForm: TdxDBBreadcrumbEditDemoForm;

implementation

{$R *.dfm}

{ TdxDBBreadcrumbEditDemoForm }

procedure TdxDBBreadcrumbEditDemoForm.beNavigationPathSelected(Sender: TObject);
begin
  if beNavigation.Selected <> nil then
    gbDepartment.Caption := beNavigation.Selected.Name
  else
    gbDepartment.Caption := '';
end;

end.

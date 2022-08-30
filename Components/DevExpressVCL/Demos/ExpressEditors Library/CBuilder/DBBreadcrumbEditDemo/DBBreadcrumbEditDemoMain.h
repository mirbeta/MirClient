//---------------------------------------------------------------------------

#ifndef DBDBBreadcrumbEditDemoMainH
#define DBBreadcrumbEditDemoMainH
//---------------------------------------------------------------------------
#include <Windows.hpp>
#include <Messages.hpp>
#include <SysUtils.hpp> 
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ShlObj.hpp>
#include <ShellAPI.hpp>
#include <Menus.hpp>
#include <Buttons.hpp>
#include <Menus.hpp>
#include <ImgList.hpp>
#include <Controls.hpp>
#include <Classes.hpp>
#include <ActnList.hpp>
#include <Buttons.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <DB.hpp>

#include "cxGraphics.hpp"
#include "cxControls.hpp"
#include "cxLookAndFeels.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "dxBreadcrumbEdit.hpp"
#include "cxTreeView.hpp"
#include "cxSplitter.hpp"
#include "dxGDIPlusClasses.hpp"
#include "cxButtons.hpp"
#include "cxListBox.hpp"
#include "BaseForm.h"
#include "cxCheckBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxDBBreadcrumbEdit.hpp"
#include "dxmdaset.hpp"
#include "cxDBEdit.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxNavigator.hpp"
#include "cxDBNavigator.hpp"
#include "cxCurrencyEdit.hpp"

//---------------------------------------------------------------------------
class TdxDBBreadcrumbEditDemoForm : public TfmBaseForm
{
__published:  // IDE-managed Components
    TcxGroupBox *gbMain;
    TBevel *bvSpacerLeft;
    TBevel *bvSpacerBottom;
    TBevel *bvSpacerTop;
    TBevel *bvSpacerRight;
    TdxMemData *mdDepartments;
    TDataSource *dsDepartments;
    TdxDBBreadcrumbEdit *beNavigation;
    TcxLabel *lbPhone;
    TcxDBMaskEdit *meFax;
    TcxLabel *lbFax;
    TcxDBMaskEdit *mePhone;
    TcxGroupBox *gbDepartment;
    TBevel *bvSpacer;
    TAutoIncField *mdDepartmentsID;
    TIntegerField *mdDepartmentsPARENTID;
    TStringField *mdDepartmentsNAME;
    TFloatField *mdDepartmentsBUDGET;
    TStringField *mdDepartmentsPHONE;
    TStringField *mdDepartmentsFAX;
    TStringField *mdDepartmentsEMAIL;
    TBooleanField *mdDepartmentsVACANCY;
    TIntegerField *mdDepartmentsMANAGERID;
    TcxDBCheckBox *cbVacancy;
    TcxLabel *lbEmail;
    TcxDBHyperLinkEdit *hleEmail;
    TcxLabel *lbBudget;
    TcxDBCurrencyEdit *ceBudget;
    TcxDBNavigator *cxDBNavigator1;
    void __fastcall beNavigationPathSelected(TObject *Sender);
private:
public:   // User declarations
	__fastcall TdxDBBreadcrumbEditDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxDBBreadcrumbEditDemoForm *dxDBBreadcrumbEditDemoForm;
//---------------------------------------------------------------------------
#endif


//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "DBBreadcrumbEditDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxGraphics"
#pragma link "cxControls" 
#pragma link "cxLookAndFeels"
#pragma link "cxLookAndFeelPainters" 
#pragma link "cxClasses" 
#pragma link "cxContainer"
#pragma link "cxEdit" 
#pragma link "dxBreadcrumbEdit" 
#pragma link "cxTreeView" 
#pragma link "cxSplitter" 
#pragma link "dxGDIPlusClasses" 
#pragma link "cxButtons" 
#pragma link "cxListBox"
#pragma link "BaseForm"
#pragma link "cxCheckBox"
#pragma link "cxDropDownEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "dxDBBreadcrumbEdit"
#pragma link "dxmdaset"
#pragma link "cxDBEdit"
#pragma link "cxHyperLinkEdit"
#pragma link "cxNavigator"
#pragma link "cxDBNavigator"
#pragma link "cxCurrencyEdit"
#pragma resource "*.dfm"
TdxDBBreadcrumbEditDemoForm *dxDBBreadcrumbEditDemoForm;
//---------------------------------------------------------------------------
__fastcall TdxDBBreadcrumbEditDemoForm::TdxDBBreadcrumbEditDemoForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------  
void __fastcall TdxDBBreadcrumbEditDemoForm::beNavigationPathSelected(TObject *Sender)
{  
	if (beNavigation->Selected)
		gbDepartment->Caption = beNavigation->Selected->Name;
	else
		gbDepartment->Caption = "";
};
//---------------------------------------------------------------------------

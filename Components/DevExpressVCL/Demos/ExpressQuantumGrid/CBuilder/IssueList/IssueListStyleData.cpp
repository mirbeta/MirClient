//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "IssueListStyleData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma resource "*.dfm"

TdmStyles *dmStyles;
//---------------------------------------------------------------------------
__fastcall TdmStyles::TdmStyles(TComponent* Owner)
        : TDataModule(Owner)
{
}
//---------------------------------------------------------------------------

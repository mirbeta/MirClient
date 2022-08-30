//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "SelectStorageUnit.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TSelectStorage *SelectStorage;
//---------------------------------------------------------------------------
__fastcall TSelectStorage::TSelectStorage(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "BreadcrumbEditDemoRecentPaths.h"
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
#pragma link "dxBevel"
#pragma link "cxImageComboBox"
#pragma link "cxLabel"
#pragma link "cxListView"
#pragma link "cxDropDownEdit"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TdxBreadcrumbEditDemoRecentPathsForm *dxBreadcrumbEditDemoRecentPathsForm;
//---------------------------------------------------------------------------
__fastcall TdxBreadcrumbEditDemoRecentPathsForm::TdxBreadcrumbEditDemoRecentPathsForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------  
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::btnAddClick(TObject *Sender)
{
	TListItem *AItem = lvPaths->Items->Add();
	AItem->Caption = tePath->Text;
	AItem->ImageIndex = cbImage->ItemIndex;
	lvPaths->Selected = AItem;
};
//---------------------------------------------------------------------------  
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::btnDeleteClick(TObject *Sender)
{
	lvPaths->DeleteSelected();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::btnReplaceClick(TObject *Sender)
{
	lvPaths->Selected->ImageIndex = cbImage->ItemIndex;
	lvPaths->Selected->Caption = tePath->Text;
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::FormCreate(TObject *Sender)
{
	InitializeImageComboBox();
	UpdateControlsState();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::InitializeImageComboBox()
{
	TcxImageComboBoxItem *AItem;
	
	cbImage->Properties->Items->BeginUpdate();
	try
	{
		cbImage->Properties->Items->Clear();
		for (int I = 0; I < cbImage->Properties->Images->Count; I++)
		{
			AItem = cbImage->Properties->Items->Add();
			AItem->ImageIndex = I;
			AItem->Value = I;
		};
	}
	__finally
	{
		cbImage->Properties->Items->EndUpdate();
	};
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::LoadPaths(TdxBreadcrumbEditRecentPaths *APaths)
{
	TListItem *AListItem;
	TdxBreadcrumbEditRecentPath *APathItem;
  
	lvPaths->Items->BeginUpdate();
	try
	{
		lvPaths->Items->Clear();
		for (int I = 0; I < APaths->Count; I++)
		{
			APathItem = APaths->Items[I];
			AListItem = lvPaths->Items->Add();
			AListItem->Caption = APathItem->Path;
			AListItem->ImageIndex = APathItem->ImageIndex;
		}
	}
	__finally
	{
		lvPaths->Items->EndUpdate();
	};
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::SavePaths(TdxBreadcrumbEditRecentPaths *APaths)
{
	APaths->BeginUpdate();
	try
	{
		APaths->Clear();
		for (int I = 0; I < lvPaths->Items->Count; I++)
		{
			TListItem *AItem = lvPaths->Items->Item[I];
			APaths->Add(AItem->Caption, AItem->ImageIndex);
		}
	}
	__finally
	{
		APaths->EndUpdate();
	};
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::tePathPropertiesChange(TObject *Sender)
{
	UpdateControlsState();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::lvPathsSelectItem(
	TObject *Sender, TListItem *Item, Boolean Selected)
{
	if (lvPaths->Selected)
	{
		cbImage->ItemIndex = lvPaths->Selected->ImageIndex;
		tePath->Text = lvPaths->Selected->Caption;
	}
	else
	{
		cbImage->ItemIndex = -1;
		tePath->Text = "";
	};
	UpdateControlsState();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoRecentPathsForm::UpdateControlsState()
{
	btnAdd->Enabled = tePath->Text != "";
	btnDelete->Enabled = lvPaths->Selected;
	btnReplace->Enabled = btnAdd->Enabled && (lvPaths->Selected);
};
//---------------------------------------------------------------------------

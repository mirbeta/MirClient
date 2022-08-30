//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "BreadcrumbEditDemoMain.h"
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
#pragma link "cxCheckBox"
#pragma link "cxDropDownEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma resource "*.dfm"
TdxBreadcrumbEditDemoForm *dxBreadcrumbEditDemoForm;
//---------------------------------------------------------------------------
__fastcall TdxBreadcrumbEditDemoForm::TdxBreadcrumbEditDemoForm(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------  
void __fastcall TdxBreadcrumbEditDemoForm::EnableControls(TWinControl *AContainer, TcxCheckBox *ASender)
{  
	for (int I = 0; I < AContainer->ControlCount; I++)
	{
		TControl *AControl = AContainer->Controls[I];
		if (AControl != ASender)
			AControl->Enabled = ASender->Checked;
		if (dynamic_cast<TWinControl*>(AControl))
			EnableControls((TWinControl *)AControl, ASender);
	};
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::beNavigationPathSelected(TObject *Sender)
{
  	tvTree->Selected = (TTreeNode *)beNavigation->Selected->Data;
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::beNavigationPopulateChildren(
  TObject *Sender, TdxBreadcrumbEditNode *ANode)
{
	TTreeNode *ATreeNode = (TTreeNode *)ANode->Data;
	for (int I = 0; I < ATreeNode->Count; I++)
		SynchronizeNodes(ANode->AddChild(), ATreeNode->Item[I]);
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::btnEditRecentPathClick(TObject *Sender)
{
	TdxBreadcrumbEditDemoRecentPathsForm *ADialog = new TdxBreadcrumbEditDemoRecentPathsForm(NULL);
	try
	{
		ADialog->LoadPaths(beNavigation->Properties->PathEditor->RecentPaths);
		if (ADialog->ShowModal() == mrOk)
			ADialog->SavePaths(beNavigation->Properties->PathEditor->RecentPaths);
	}
	__finally
	{
		ADialog->Free();
	};
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::btnProgressStartClick(TObject *Sender)
{
	btnProgressStop->Enabled = True;
	btnProgressStart->Enabled = False;
	tmProgress->Enabled = True;
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::btnProgressStopClick(TObject *Sender)
{
	tmProgress->Enabled = False;
	btnProgressStop->Enabled = False;
	btnProgressStart->Enabled = True;
	beNavigation->Properties->ProgressBar->Position = 0;
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::PathEditorOptionsChanged(TObject *Sender)
{
	EnableControls(gbPathEditorOptions, cbpeEnabled);
	if (!FInitializingControls)
	{
		beNavigation->Properties->PathEditor->AutoComplete = cbpeAutoComplete->Checked;
		beNavigation->Properties->PathEditor->Enabled = cbpeEnabled->Checked;
		beNavigation->Properties->PathEditor->ReadOnly = cbpeReadOnly->Checked;
		beNavigation->Properties->PathEditor->RecentPathsAutoPopulate = cbpeRecentsAutoPopulate->Checked;
	};
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::ProgressBarPropertiesChange(TObject *Sender)
{
	beNavigation->Properties->ProgressBar->CancelEffect =
		(TdxBreadcrumbEditProgressBarCancelEffect)cbCancelEffect->ItemIndex;
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::InitializeControls()
{
	FInitializingControls = True;
	try
	{
		cbCancelEffect->ItemIndex = (int)beNavigation->Properties->ProgressBar->CancelEffect;
		cbpeAutoComplete->Checked = beNavigation->Properties->PathEditor->AutoComplete;
		cbpeEnabled->Checked = beNavigation->Properties->PathEditor->Enabled;
		cbpeReadOnly->Checked = beNavigation->Properties->PathEditor->ReadOnly;
		cbpeRecentsAutoPopulate->Checked = beNavigation->Properties->PathEditor->RecentPathsAutoPopulate;
	}
	__finally
	{
		FInitializingControls = False;
	};
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::FormCreate(TObject *Sender)
{
	InitializeControls();
	tvTree->FullExpand();
	SynchronizeNodes(beNavigation->Root, tvTree->Items->Item[0]);
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::SynchronizeNodes(TdxBreadcrumbEditNode *ANode, TTreeNode *ATreeNode)
{
	ANode->BeginUpdate();
	try
	{
		ANode->ImageIndex = ATreeNode->ImageIndex;
		ANode->HasChildren = ATreeNode->HasChildren;
		ANode->Name = ATreeNode->Text;
		ANode->Data = ATreeNode;
	}
	__finally
	{
		ANode->EndUpdate();
	};
};
//---------------------------------------------------------------------------
String TdxBreadcrumbEditDemoForm::GetPath(TTreeNode *ATreeNode)
{
	String Result = "";
	if (ATreeNode->Parent)
		Result = GetPath(ATreeNode->Parent);
	return Result + ATreeNode->Text + PathDelim;
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::tmProgressTimer(TObject *Sender)
{
	beNavigation->Properties->ProgressBar->Position = beNavigation->Properties->ProgressBar->Position + 1;
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::tvTreeChange(TObject *Sender, TTreeNode *Node)
{
	beNavigation->SelectedPath = GetPath(Node);
};
//---------------------------------------------------------------------------

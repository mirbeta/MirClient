//---------------------------------------------------------------------------

#include <vcl.h>
#include "shellapi.hpp"
#pragma hdrstop

#include "ShellBreadcrumbEditDemoMain.h"
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
#pragma link "dxShellBreadcrumbEdit"
#pragma link "cxShellTreeView" 
#pragma link "cxSplitter" 
#pragma link "cxShellControls" 
#pragma link "cxShellListView"
#pragma link "dxGDIPlusClasses" 
#pragma link "cxButtons" 
#pragma link "cxListBox"
#pragma link "cxShellCommon"
#pragma resource "*.dfm"
TdxBreadcrumbEditDemoForm *dxBreadcrumbEditDemoForm;
//---------------------------------------------------------------------------
class TcxInnerShellListViewAccess: public TcxInnerShellListView
{
	public:
    	 TcxShellListViewProducer* Producer() { return ItemProducer; };
};
//---------------------------------------------------------------------------
__fastcall TdxBreadcrumbEditDemoForm::TdxBreadcrumbEditDemoForm(TComponent* Owner)
  : TForm(Owner)
{
}
//---------------------------------------------------------------------------  
void TdxBreadcrumbEditDemoForm::InitializeLookAndFeel()
{
	bceAddressBar->Properties->Borders << bBottom;
	pnlAddressBarContainer->BorderWidth = 0;
    pnlAddressBarContainer->DoubleBuffered = true;
    lvFiles->InnerListView->RowSelect = true;
	bvTopSpacer->Visible = false;
};
//---------------------------------------------------------------------------
void TdxBreadcrumbEditDemoForm::InitializeShellLargeImages()
{
	TSHFileInfoA AFileInfo;

    ShellLargeImages = new TImageList(this);
  	ShellLargeImages->ShareImages = true;
	ShellLargeImages->Handle = SHGetFileInfo("", 0,
	  &AFileInfo, sizeof(AFileInfo), SHGFI_SYSICONINDEX | SHGFI_LARGEICON);
	ImageList_SetBkColor((_IMAGELIST *)ShellLargeImages->Handle, CLR_NONE);
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::lvFilesCurrentFolderChanged(TcxCustomShellListView *Sender)
{
	UpdateSelectionInfo();
}
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::lvFilesSelectItem(TObject *Sender, TListItem *Item, Boolean Selected)
{
  	UpdateSelectionInfo();
}
//---------------------------------------------------------------------------
void TdxBreadcrumbEditDemoForm::SetupIconsView(int AThumbnailSize)
{
    lvFiles->ViewStyle = vsIcon;
    lvFiles->ThumbnailOptions->BeginUpdate();
	try
	{
		  lvFiles->ThumbnailOptions->ShowThumbnails = AThumbnailSize > 0;
		  if (lvFiles->ThumbnailOptions->ShowThumbnails)
		  {
				lvFiles->ThumbnailOptions->Height = AThumbnailSize;
				lvFiles->ThumbnailOptions->Width  = AThumbnailSize;
		  };
	}
	__finally
	{
      	  lvFiles->ThumbnailOptions->EndUpdate();
    };
}
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::miViewDetailsClick(TObject *Sender)
{
	switch (((TComponent*)Sender)->Tag)
	{
		case 0:
			{
				SetupIconsView(256);
				break;
			}
		case 1:
			{
				SetupIconsView(96);
				break;
			}
		case 2:
			{
				SetupIconsView(0);
				break;
			}
		case 3:
			{
				lvFiles->ViewStyle = vsSmallIcon;
				break;
			}
		case 4:
			{
				lvFiles->ViewStyle = vsList;
				break;
			}
		case 5:
			{
				lvFiles->ViewStyle = vsReport;
				break;
			}
	}
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::FormCreate(TObject *Sender)
{
	InitializeLookAndFeel();
	InitializeShellLargeImages();
	UpdateCaption();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::FormShow(TObject *Sender)
{
  	pnlAddressBarContainer->Realign();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::acAboutExecute(TObject *Sender)
{
	ShowAboutDemoForm();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::acBrowseParentExecute(TObject *Sender)
{
	bceAddressBar->BrowseParent();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::acRefreshExecute(TObject *Sender)
{
	bceAddressBar->UpdateContent();
	tvFolders->UpdateContent();
	lvFiles->UpdateContent();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::bceAddressBarPathSelected(TObject *Sender)
{
	UpdateCaption();
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::pbSelectedItemIconPaint(TObject *Sender)
{
  	ShellLargeImages->Draw(pbSelectedItemIcon->Canvas, 0, 0, pbSelectedItemIcon->Tag);
};
//---------------------------------------------------------------------------
void __fastcall TdxBreadcrumbEditDemoForm::sbViewClick(TObject *Sender)
{
	TPoint P = sbView->ClientToScreen(Point(0, sbView->Height));
  	pmView->Popup(P.x, P.y);
};
//---------------------------------------------------------------------------
void TdxBreadcrumbEditDemoForm::UpdateCaption()
{
	if (bceAddressBar->Selected)
	{
		TVarRec vr[] = {(AnsiString)bceAddressBar->Selected->Name};
		AnsiString S = "DevExpress Explorer - %s";
		Caption = Format(S, vr, 1);
	}
  	else
    	Caption = "DevExpress Explorer";
};
//---------------------------------------------------------------------------
void TdxBreadcrumbEditDemoForm::GetItemInfo(PItemIDList APidl, TSHFileInfo &AFileInfo, Boolean ADisposePidl)
{
	ZeroMemory(&AFileInfo, sizeof(TSHFileInfo));
    cxShellGetThreadSafeFileInfo(PChar(APidl), 0, AFileInfo, sizeof(AFileInfo),
      SHGFI_PIDL | SHGFI_DISPLAYNAME | SHGFI_TYPENAME | SHGFI_SYSICONINDEX);
	if (ADisposePidl)
      DisposePidl(APidl);
}
//---------------------------------------------------------------------------
void TdxBreadcrumbEditDemoForm::GetItemInfoByItemIndex(int AIndex, TSHFileInfo &AFileInfo)
{
	TcxShellListViewProducer *AProducer = ((TcxInnerShellListViewAccess *)lvFiles->InnerListView)->Producer();
	AProducer->LockRead();
	try
	{
		TcxShellItemInfo *Info = (TcxShellItemInfo *)(AProducer->Items->Items[lvFiles->InnerListView->Selected->Index]);
		GetItemInfo(Info->FullPIDL, AFileInfo);
	}
	__finally
	{
	  	AProducer->UnlockRead();
	};
}
//---------------------------------------------------------------------------
void TdxBreadcrumbEditDemoForm::UpdateSelectionInfo()
{
	TSHFileInfo AFileInfo;
	if (!lvFiles->InnerListView->Selected)
	{
    	GetItemInfo(lvFiles->AbsolutePIDL, AFileInfo, true);
	    lbName->Caption = AFileInfo.szDisplayName;
		pbSelectedItemIcon->Tag = AFileInfo.iIcon;

		TVarRec V[] = {lvFiles->InnerListView->Items->Count};
		AnsiString S = "%d items";
		lbInfo->Caption = Format(S, V, 1);
	}
	else
	{
		pbSelectedItemIcon->Tag = lvFiles->InnerListView->Selected->ImageIndex;
		if (lvFiles->InnerListView->SelCount > 1)
		{
			TVarRec V[] = {lvFiles->InnerListView->SelCount};
			AnsiString S = "%d items selected";
			lbName->Caption = Format(S, V, 1);
			lbInfo->Caption = "";
		}
		else
		{
		    GetItemInfoByItemIndex(lvFiles->InnerListView->Selected->Index, AFileInfo);
			lbName->Caption = AFileInfo.szDisplayName;
			lbInfo->Caption = AFileInfo.szTypeName;
		}
	}
  	pbSelectedItemIcon->Invalidate();
};

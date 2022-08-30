//---------------------------------------------------------------------------

#include <vcl.h>
#include <Windows.h>
#pragma hdrstop

#include "uPDFViewerDemo.h"
#include "uExportToFileDialog.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxBarEditItem"
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxBar"
#pragma link "dxRibbon"
#pragma link "dxRibbonForm"
#pragma link "dxRibbonBackstageView"
#pragma link "dxRibbonSkins"
#pragma link "dxRibbonStatusBar"
#pragma link "dxStatusBar"
#pragma link "cxContainer"
#pragma link "cxEdit"
#pragma link "cxLabel"
#pragma link "cxShellBrowserDialog"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "cxTrackBar"
#pragma link "dxActions"
#pragma link "dxBarExtItems"
#pragma link "dxCustomPreview"
#pragma link "dxPDFViewer"
#pragma link "dxPDFViewerActions"
#pragma link "dxRibbonCustomizationForm"
#pragma link "dxRibbonGallery"
#pragma link "dxSkinChooserGallery"
#pragma link "dxZoomTrackBar"
#pragma link "dxPSActions"
#pragma link "dxPSdxPDFViewerLnk"
#pragma link "cxDrawTextUtils"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSCompsProvider"
#pragma link "dxPSCore"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwAdv"
#pragma link "dxPSPrVwRibbon"
#pragma link "dxPSPrVwStd"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "dxBarBuiltInMenu"
#pragma link "dxPDFDocument"
#pragma link "dxPrinting"
#pragma link "cxImageList"
#pragma link "dxPDFBase"
#pragma link "dxPDFDocumentViewer"
#pragma link "dxPDFRecognizedObject"
#pragma link "dxPDFText"
#pragma resource "*.dfm"
TfrmPDFViewer *frmPDFViewer;
//---------------------------------------------------------------------------
__fastcall TfrmPDFViewer::TfrmPDFViewer(TComponent* Owner)
	: TdxRibbonForm(Owner)
{
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::InitializeLookAndFeel()
{
  cxLookAndFeelController1->NativeStyle = False;
  cxLookAndFeelController1->SkinName = "DevExpressStyle";
  dxSkinChooserGalleryItem1->SelectedSkinName = RootLookAndFeel()->Painter->LookAndFeelName();
  dxRibbon1->ColorSchemeName = cxLookAndFeelController1->SkinName;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPDFViewer::FormCreate(TObject *Sender)
{
  DisableAero = True;
  InitializeLookAndFeel();
  dxPDFViewer1->LoadFromFile("..\\..\\Data\\DevAVProducts.pdf");
  dxPDFViewer1ZoomFactorChanged(NULL);
  Caption = "VCL PDF Viewer Demo";
}
//---------------------------------------------------------------------------
void __fastcall TfrmPDFViewer::dxPDFViewer1ZoomFactorChanged(TObject *Sender)
{
  bteActualZoom->EditValue = dxPDFViewer1->OptionsZoom->ZoomFactor;
  tbZoom->Position = dxPDFViewer1->OptionsZoom->ZoomFactor;
}
//---------------------------------------------------------------------------
bool TfrmPDFViewer::IsLocked()
{
  return FLockCount != 0;
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::BeginUpdate()
{
  FLockCount++;
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::CancelUpdate()
{
  FLockCount--;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPDFViewer::dxPDFViewer1SelectedPageChanged(TObject *Sender, int APageIndex)
{
  BeginUpdate();
  try
  {
	bseActivePage->EditValue = dxPDFViewer1->CurrentPageIndex + 1;
	sseActivePage->EditValue = Min(Max(bseActivePage->EditValue, 1), dxPDFViewer1->PageCount);
	sbPageCount->Caption = " of " + IntToStr(dxPDFViewer1->PageCount);
  }
  __finally
  {
	CancelUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPDFViewer::bseActivePageChange(TObject *Sender)
{
  UpdateActivePage(bseActivePage->EditValue);
}
//---------------------------------------------------------------------------
void __fastcall TfrmPDFViewer::sseActivePageChange(TObject *Sender)
{
  UpdateActivePage(sseActivePage->EditValue);
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::UpdateActivePage(int AIndex)
{
  if (!IsLocked())
	dxPDFViewer1->CurrentPageIndex = AIndex - 1;
}
//---------------------------------------------------------------------------
void __fastcall TfrmPDFViewer::tbZoomPropertiesChange(TObject *Sender)
{
  biStatusBarCurrentZoom->Caption = IntToStr(tbZoom->Position) + "%";
  dxPDFViewer1->OptionsZoom->ZoomFactor = tbZoom->Position;
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::UpdateActivePageEditor(TcxBarEditItem *ASpinEdit)
{
  ASpinEdit->Enabled = true;
  TcxSpinEditProperties* AProperties = (TcxSpinEditProperties*)(ASpinEdit->Properties);
  AProperties->MaxValue = dxPDFViewer1->PageCount;
  AProperties->MinValue = 1;
  ASpinEdit->EditValue = Max(dxPDFViewer1->CurrentPageIndex, 1);
}
//---------------------------------------------------------------------------

void __fastcall TfrmPDFViewer::dxPDFViewer1DocumentLoaded(TdxPDFDocument *ASender,
		  const TdxPDFDocumentLoadInfo &AInfo)
{
  dxPDFViewer1->BeginUpdate();
  try
  {
	dxPDFViewer1->OptionsZoom->ZoomFactor = 100;
    dxPDFViewer1->ClearViewStateHistory();
  }
  __finally
  {
	dxPDFViewer1->EndUpdate();
  }
  dxBarButtonExportToPNG->Enabled = true;
  dxBarButtonExportToTIFF->Enabled = true;
  bteActualZoom->Enabled = true;
  tbZoom->Enabled = true;
  UpdateActivePageEditor(bseActivePage);
  UpdateActivePageEditor(sseActivePage);
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::AfterExport()
{
  FProgressDialog->Free();
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::BeforeExport()
{
  FProgressDialog = new TfrmProgress(this);
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::ShowExportToPNGDialog()
{
  String AFileName;
  TfrmExportToBitmaps* AExportDialog = new TfrmExportToBitmaps(this);
  bool ANeedOpenAfterExport;
  try
  {
	if (AExportDialog->ShowModal() == mrOk)
	{
	  ANeedOpenAfterExport = AExportDialog->cbOpenAfterExport->Checked;
	  if (sbdSelectFolderDialog->Execute())
	  {
		BeforeExport();
		try
		{
		  FProgressDialog->Show();
		  AFileName = sbdSelectFolderDialog->Path;
		  dxPDFDocumentExportToPNG(dxPDFViewer1->Document, AFileName, AExportDialog->teFilePrefix->EditValue, AExportDialog->sePageZoom->EditValue / 100, FProgressDialog, dxPDFViewer1->RotationAngle);
		}
		__finally
		{
		  AfterExport();
		}
	  }
	  if (ANeedOpenAfterExport)
		dxShellExecute(AFileName);
	}
  }
  __finally
  {
	AExportDialog->Free();
  }
}
//---------------------------------------------------------------------------
void TfrmPDFViewer::ShowExportToTIFFDialog()
{
  String AFileName;
  TfrmExportToFileDialog* AExportDialog = new TfrmExportToFileDialog(this);
  bool ANeedOpenAfterExport;
  try
  {
	if (AExportDialog->ShowModal() == mrOk)
	{
	  ANeedOpenAfterExport = AExportDialog->cbOpenAfterExport->Checked;
	  TSaveDialog* ASaveDialog = new TSaveDialog(NULL);
	  ASaveDialog->Filter = "TIFF - Tag Image File Format (*.tiff)|*.tiff;";
	  String AFileName;
	  try
	  {
		if (ASaveDialog->Execute())
		  AFileName = ASaveDialog->FileName + ".tiff";
		else
		  AFileName = "";
	  }
	  __finally
	  {
		ASaveDialog->Free();
	  }
	  BeforeExport();
	  try
	  {
		  FProgressDialog->Show();
		  dxPDFDocumentExportToTIFF(dxPDFViewer1->Document, AFileName, AExportDialog->sePageZoom->EditValue / 100, FProgressDialog, dxPDFViewer1->RotationAngle);
	  }
	  __finally
	  {
		AfterExport();
	  }
	  if (ANeedOpenAfterExport)
		dxShellExecute(AFileName);

	}
  }
  __finally
  {
	AExportDialog->Free();
  }
}
//---------------------------------------------------------------------------
void __fastcall TfrmPDFViewer::dxBarButtonExportToTIFFClick(TObject *Sender)
{
	ShowExportToTIFFDialog();
}
//---------------------------------------------------------------------------

void __fastcall TfrmPDFViewer::dxBarButtonExportToPNGClick(TObject *Sender)
{
	ShowExportToPNGDialog();
}
//---------------------------------------------------------------------------

void __fastcall TfrmPDFViewer::dxBarButtonAboutClick(TObject *Sender)
{
	ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall TfrmPDFViewer::dxPDFViewer1CustomDrawPreRenderPage(TObject *Sender,
          TcxCanvas *ACanvas, const TdxPDFPreRenderPageInfo &APageInfo, bool &ADone)

{
   ADone = APageInfo.Thumbnail == NULL;
   if (ADone) {
	 ACanvas->Font->Size = cxRectWidth(APageInfo.Bounds) / 20;
	 cxDrawText(ACanvas, "Page rendering...", APageInfo.Bounds, DT_CENTER || DT_VCENTER || DT_SINGLELINE,
	   dxPDFViewer1->LookAndFeel->Painter->PrintPreviewBackgroundTextColor() );
   }
}
//---------------------------------------------------------------------------


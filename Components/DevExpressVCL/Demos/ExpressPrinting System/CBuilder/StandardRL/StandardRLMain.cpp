//---------------------------------------------------------------------------
#define STRICT
#include <vcl.h>
#pragma hdrstop

#include "StandardRLMain.h"
#include <stdlib.h>
#include <shellAPI.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxPSChLbxLnk"
#pragma link "dxPSCore"
#pragma link "dxPSGrLnks"
#pragma link "dxPSLbxLnk"
#pragma link "dxPSStdGrLnk"
#pragma link "dxPSRELnk"
#pragma link "dxPSTVLnk"
#pragma link "dxBkgnd"
#pragma link "dxPrnDev"
#pragma link "dxPrnPg"
#pragma link "dxPSBaseGridLnk"
#pragma link "dxPSCompsProvider"
#pragma link "dxPSEdgePatterns"
#pragma link "dxPSEngn"
#pragma link "dxPSFillPatterns"
#pragma link "dxPSGlbl"
#pragma link "dxPSUtl"
#pragma link "dxWrap"
#pragma link "cxDrawTextUtils"
#pragma link "dxPScxEditorProducers"
#pragma link "dxPScxExtEditorProducers"
#pragma link "dxPScxPageControlProducer"
#pragma link "dxPSPDFExport"
#pragma link "dxPSPDFExportCore"
#pragma link "dxPSPrVwStd"
#pragma link "DemoBasicMain"
#pragma link "cxGraphics"
#pragma resource "*.dfm"
TStandardRLMainForm *StandardRLMainForm;
//---------------------------------------------------------------------------
__fastcall TStandardRLMainForm::TStandardRLMainForm(TComponent* Owner)
    : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::DrawGridDrawCell(TObject *Sender, int Col,
      int Row, TRect &Rect, TGridDrawState State)
{
  if (State.Contains(gdSelected))
    ilFlags->DrawingStyle = dsSelected;
  else
    ilFlags->DrawingStyle = Imglist::dsNormal;
  DrawFlag(Col, Row, Rect, DrawGrid->Canvas);
}
//---------------------------------------------------------------------------
int __stdcall EnumFontFamProc(const LOGFONT *LogFont,const TEXTMETRIC *TextMetric, unsigned long FontType, long Data)
{
  TStrings *S = (TStrings *)Data;
  switch (FontType)
  {
   case TRUETYPE_FONTTYPE:
     S->AddObject(LogFont->lfFaceName, StandardRLMainForm->TtfBitmap);
   break;
   case RASTER_FONTTYPE:
     S->AddObject(LogFont->lfFaceName, StandardRLMainForm->SysBitmap);
   break;
   case DEVICE_FONTTYPE:
     S->AddObject(LogFont->lfFaceName, StandardRLMainForm->DevBitmap);
   break;
  }

  return 1;
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::BuildDriverList()
{
  DWORD dwDriveList = GetLogicalDrives ();
  const AnsiString slash = ":\\";

    for (char i = 0; i < 32; i++) {
        if (dwDriveList & (1 << i))
        {
           char d = (char)(65 + i);
           AnsiString Drive = d;
           Drive+=slash;
           cbDrives->Items->Add(Drive);
           if (i == 2)
             cbDrives->ItemIndex = cbDrives->Items->Count - 1;
        }
    }
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::FormCreate(TObject *Sender)
{
  TDemoBasicMainForm::FormCreate(Sender);
  HDC DC;
  int i, j;

  TtfBitmap = new Graphics::TBitmap();
  ilFontImages->GetBitmap(0,TtfBitmap);
  DevBitmap = new Graphics::TBitmap();
  ilFontImages->GetBitmap(2,DevBitmap);
  SysBitmap = new Graphics::TBitmap();
  ilFontImages->GetBitmap(1,SysBitmap);
  DC = GetDC(0);
  try
  {
    EnumFontFamilies(DC, NULL, (FONTENUMPROCA)EnumFontFamProc, long (FontsList->Items));
  }
  __finally
  {
    ReleaseDC(0, DC);
  }
  for (i = 1; i <= 10; i++)
  {
    StringGrid->Cells[0][i] = IntToStr(i);
    for (j = 1; j <= 10; j++)
    {
      StringGrid->Cells[j][0] = IntToStr(j);
      StringGrid->Cells[j][i] = IntToStr(i*j);
    }
  }
  Randomize();
  for (i = 0; i<CountryCodeList->Items->Count;i++)
    CountryCodeList->Checked[i] = (Boolean)random(2);
  try
  {
    Editor->Lines->LoadFromFile(ExtractFilePath(Application->ExeName)+"overview.rtf");
  }
  catch(...){};
  BuildDriverList();
  cbDrivesChange(cbDrives);

  dxComponentPrinter->CurrentLink = dxComponentPrinter->ReportLink[PageControl->ActivePage->PageIndex];

}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::FormDestroy(TObject *Sender)
{
  if (TtfBitmap != NULL) TtfBitmap->Free();
  if (DevBitmap != NULL) DevBitmap->Free();
  if (SysBitmap != NULL) SysBitmap->Free();
}
//---------------------------------------------------------------------------

void __fastcall TStandardRLMainForm::PageControlChange(TObject *Sender)
{
  dxComponentPrinter->CurrentLink = dxComponentPrinter->ReportLink[PageControl->ActivePage->PageIndex];
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::FontsListDrawItem(TWinControl *Control,
	  int Index, TRect &Rect, TOwnerDrawState State)
{
  int Offset;
  Graphics::TBitmap *Bitmap = (Graphics::TBitmap *)(FontsList->Items->Objects[Index]);
  FontsList->Canvas->FillRect(Rect);
  Offset = 2;
  if (Bitmap != NULL)
  {
    FontsList->Canvas->Draw(Rect.Left+2,Rect.Top,Bitmap);
    Offset = Bitmap->Width + 6;
  }
  FontsList->Canvas->Font->Name = FontsList->Items->Strings[Index];
  FontsList->Canvas->TextOut(Rect.Left+Offset, Rect.Top, FontsList->Items->Strings[Index]);
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::DrawFlag(int Row, int Col, TRect &Rect, TCanvas *ACanvas)
{
  Graphics::TBitmap *Bitmap = new Graphics::TBitmap();

  ACanvas->Pen->Color = clWhite;
  ACanvas->MoveTo(Rect.Left,Rect.Bottom);
  ACanvas->LineTo(Rect.Right,Rect.Bottom);
  ACanvas->LineTo(Rect.Right,Rect.Top-1);
  ACanvas->Pen->Color = clBtnShadow;
  ACanvas->MoveTo(DrawGrid->Canvas->PenPos.x - 1,DrawGrid->Canvas->PenPos.y + 1);
  ACanvas->LineTo(Rect.Left,Rect.Top);
  ACanvas->LineTo(Rect.Left,Rect.Bottom);
  ACanvas->Pen->Color = clBtnFace;
  ACanvas->MoveTo(Rect.Left + 1,Rect.Bottom - 1);
  ACanvas->LineTo(Rect.Right - 1,Rect.Bottom - 1);
  ACanvas->LineTo(Rect.Right - 1,Rect.Top);
  ACanvas->MoveTo(ACanvas->PenPos.x - 1,ACanvas->PenPos.y + 1);
  ACanvas->Pen->Color = clBlack;
  ACanvas->LineTo(Rect.Left + 1,Rect.Top + 1);
  ACanvas->LineTo(Rect.Left + 1,Rect.Bottom - 1);
  ilFlags->GetBitmap(Col+Row*7,Bitmap);
  if (Bitmap != NULL)
  {
    ACanvas->StretchDraw(Bounds(Rect.Left+2, Rect.Top+2, Rect.Right-Rect.Left-4, Rect.Bottom-Rect.Top-4),Bitmap);
    Bitmap->Free();
  }
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::dxComponentPrinterLink2CustomDrawCell(
      TBasedxReportLink *Sender, int ACol, int ARow, TCanvas *ACanvas,
      TRect &ABoundsRect, TRect &AClientRect)
{
  ilFlags->DrawingStyle = Imglist::dsNormal;
  DrawFlag(ACol, ARow, AClientRect, ACanvas);
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::BuildTree(String APath, TTreeNode *AItem)
{
    TSearchRec SearchRec, Dummy;
    TTreeNode* Node;

    int Found = FindFirst(APath + "*.*", faAnyFile, SearchRec);
    try
    {
      while (Found == 0)
      {
        if ((SearchRec.Name != ".") && (SearchRec.Name != ".."))
        {
          if (AItem != NULL)
            Node = TreeView->Items->AddChild(AItem, SearchRec.Name);
          else
            Node = TreeView->Items->Add(NULL, SearchRec.Name);
		  Node->StateIndex = 3;
          if ((SearchRec.Attr & faDirectory) != 0)
          {
            Node->HasChildren = (FindFirst(APath + SearchRec.Name + "\\*.*", faAnyFile, Dummy) == 0);
			Node->StateIndex = 1;
          }
        };
        Found = FindNext(SearchRec);
      }
    }
    __finally
    {
      FindClose(SearchRec);
    };
}
//---------------------------------------------------------------------------

void __fastcall TStandardRLMainForm::Button1Click(TObject *Sender)
{
  TreeView->Items->BeginUpdate();
  TreeView->FullExpand();
  TreeView->Items->EndUpdate();
}
//---------------------------------------------------------------------------

void __fastcall TStandardRLMainForm::Button2Click(TObject *Sender)
{
  TreeView->FullCollapse();
}
//---------------------------------------------------------------------------

void __fastcall TStandardRLMainForm::TreeViewExpanded(TObject *Sender,
      TTreeNode *Node)
{
  Node->StateIndex = 2;
}
//---------------------------------------------------------------------------

void __fastcall TStandardRLMainForm::TreeViewCollapsed(TObject *Sender,
      TTreeNode *Node)
{
  Node->StateIndex = 1;
}
//---------------------------------------------------------------------------

void __fastcall TStandardRLMainForm::cbDrivesChange(TObject *Sender)
{
  TreeView->Cursor = crHourGlass;
  TreeView->Items->BeginUpdate();
  TreeView->Items->Clear();
  BuildTree(cbDrives->Text, NULL);
  TreeView->Items->EndUpdate();
  TreeView->Cursor = crDefault;
}
//---------------------------------------------------------------------------
String __fastcall TStandardRLMainForm::GetNodeFullPath(TTreeNode *Node)
{
  String Result;

  while(Node != NULL)
  {
    if (Node->HasChildren)
      Result = "\\" + Result;
    Result = Node->Text + Result;
    Node = Node->Parent;
   }
    Result = cbDrives->Text + Result;
    return Result;
}
//---------------------------------------------------------------------------
void __fastcall TStandardRLMainForm::TreeViewExpanding(TObject *Sender,
      TTreeNode *Node, bool &AllowExpansion)
{
  if(Node->HasChildren && (Node->Count == 0))
    BuildTree(GetNodeFullPath(Node), Node);
}
//---------------------------------------------------------------------------

